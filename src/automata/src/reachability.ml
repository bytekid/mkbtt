(* Copyright 2008 Martin Korp, Christian Sternagel, Harald Zankl
 * GNU Lesser General Public License
 *
 * This file is part of TTT2.
 * 
 * TTT2 is free software: you can redistribute it and/or modify it under
 * the terms of the GNU Lesser General Public License as published by the
 * Free Software Foundation, either version 3 of the License, or (at your
 * option) any later version.
 * 
 * TTT2 is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public
 * License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public
 * License along with TTT2. If not, see <http://www.gnu.org/licenses/>.
 *)

(*** OPENS ********************************************************************)
open Prelude;;

(*** MODULES ******************************************************************)
module Make (R : REWRITING)
            (C : CATEGORIZATION with type 'a m = 'a R.Monad.t) = struct
 (*** MODULES *****************************************************************)
 module A = Automaton.Make (R) (C);;
 module Ass = Util.Map.Partial (R.Term);;
 module Fun = R.Function;;
 module I = Initial.Make (R) (C);;
 module Lhs = Lhs.Make (R);;
 module M = Monad.Make (R);;
 module P = Path.Make (R) (C);;
 module Rep = Util.Map.Partial (State);;
 module Rule = R.Rule;;
 module Pos = R.Position;;
 module S = Substitution.Make (R);;
 module T = Transducer.Make (R) (C);;
 module Term = Term.Make (R);;
 module Trs = R.Trs;;
 module Var = R.Variable;;

 (*** OPENS *******************************************************************)
 open Util;;

 (*** TYPES *******************************************************************)
 type 'a m = 'a M.t;;
 type automaton = A.t;;
 type term = R.Term.t = Var of Var.t | Fun of Fun.t * term list;;
 type trs = Trs.t;;

 (*** FUNCTIONS ***************************************************************)
 let (>>=) = M.(>>=);;

 let analyse ass trs a =
  let find_ran p = catch Not_found (A.D.find_ran p) [p] a.A.dependency in
  let find_dom ps =
   if List.length ps = 1 then List.hd ps else A.D.find_dom ps a.A.dependency
  in
  A.fold_trans (fun l r m ->
   let f = Option.the (Lhs.root l) and ps = Lhs.args l in
   let q = Option.the (Rhs.agent r) in
   Trs.fold (fun r m ->
    let l = Rule.lhs r and r = Rule.rhs r in
    let g = Option.fold id f (R.Term.root l) and us = R.Term.args l in
    let compare ui pi = R.Term.is_var ui || List.mem (Ass.find ui ass) pi in
    let check us = List.for_all2 compare us <.> List.map find_ran in
    if f = g && (try check us ps with Invalid_argument _ -> false) then
     let s = match l with R.Term.Var x -> S.add x q S.empty | _ -> S.empty in
     let add s ui pi = match ui with Var x -> S.add x pi s | _ -> s in
     let r = S.apply_term (List.foldl2 add s us ps) (Term.of_term r) in
     let add p m = M.lift (List.rev_append (find_ran p)) m in
     A.paths r a >>= List.foldl (fun m -> flip add m <.> snd) m
    else m) (M.return (find_ran q)) trs >>= fun qs ->
    let q' = find_dom (List.unique_hash qs) in
    if q = q' then m else M.lift (List.cons (l,q')) m) a (M.return [])
 ;;

 let initial fs trs a =
  let ts = List.flat_map R.Term.args (Trs.lhs trs) in
  let ts = List.unique_hash (List.filter R.Term.is_fun ts) in
  let fs = List.union fs (Trs.funs trs) and ass = Ass.empty in
  let add ass t q = M.return (Ass.add t q ass) in
  M.foldl (fun ass t -> M.fresh_state >>= add ass t) ass ts >>= fun ass ->
  let us = List.map Term.of_term ts in
  let find u = M.return [Ass.find (Term.to_term u) ass] and cp = P.fresh_max in
  I.instances find cp [] fs us >>= A.combine a >>= fun a ->
  let gs = List.diff fs (A.funs a) and add c g = M.liftm (C.add g c) in
  M.foldl add a.A.categorization gs >>= fun c ->
  M.lift (pair ass) (A.det ~c:true {a with A.categorization = c})
 ;;

 let restrict a =
  let p = A.D.find_dom [] a.A.dependency in
  A.fold (fun l r m -> match Rhs.remove p r with
   | None -> m >>= A.remove l
   | Some r -> m >>= A.replace l r) a (M.return a)
 ;;

 let predecessors fs trs a =
  if Trs.is_growing trs then
   (* transform TRS *)
   M.fresh_var >>= fun x ->
   let rec transform d = function
    | Var y -> if d > 1 then Var x else Var y
    | Fun (f,ts) -> Fun (f,List.map (transform (d+1)) ts)
   in
   let apply = Rule.apply (transform 0) id in
   let trs = Trs.project (uncurry Rule.of_terms <.> apply) trs in
   initial fs trs a >>= fun (ass,a) ->
   (* compute automaton *)
   let rec predecessors a =
    let solve = M.foldl (fun a (l,q') -> A.replace l (Rhs.singleton q') a) in
    analyse ass trs a >>= fun ts ->
    if ts = [] then M.return a else solve a ts >>= predecessors
   in
   predecessors a >>= A.reduce >>= (M.lift A.set_quasi_det <.> restrict)
  else M.fail "illegal TRS"
 ;;

 let successors fs trs a =
  let check l r = List.intersect (R.Term.vars l) (R.Term.vars r) = [] in
  if Trs.for_all (Rule.uncurry check) trs then
   let add r (ls,rs) = (Rule.lhs r :: ls,Rule.rhs r :: rs) in
   let (ls,rs) = Trs.fold add ([],[]) trs in
   let assl = Ass.empty and assr = Ass.empty in
   let add t q ass = Ass.add t (q :: catch Not_found (Ass.find t) [] ass) ass in
   let modify (assl,assr) l r q = (add l q assl,add r q assr) in
   let update ass l r = M.lift (modify ass l r) M.fresh_state in
   M.foldl2 update (assl,assr) ls rs >>= fun (assl,assr) ->
   let find ass t = M.return (Ass.find (Term.to_term t) ass) in
   let cp = P.fresh_max and fs = List.union fs (Trs.funs trs) in
   I.instances (find assl) cp [] fs (List.map Term.of_term ls) >>= fun b ->
   I.instances (find assr) cp [] fs (List.map Term.of_term rs) >>= fun c ->
   T.transitive_closure (T.make b c) >>= T.successors a
  else M.fail "illegal TRS"
 ;;
end
