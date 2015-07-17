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

(*** MODULES ******************************************************************)
module Fun = Function;;
module M = Monad;;
module Var = Variable;;

(*** EXCEPTIONS ***************************************************************)
exception Not_semi_unifiable;;
exception Not_unifiable;;
exception Not_matchable;;

(*** OPENS ********************************************************************)
open Prelude;;
open Util;;

(*** MODULES ******************************************************************)
module Make (L : LABEL) = struct
 (*** MODULES *****************************************************************)
 module M = M.Make (L);;
 module S = Substitution.Make (L);;
 module Sig = Signature.Make (L);;
 module Term = Term.Make (L);;
 
 (*** TYPES *******************************************************************)
 type 'a m = 'a M.t;;
 type substitution = S.t;;
 type term = Term.t = Var of Var.t | Fun of Fun.t * term list;;
 
 (*** EXCEPTIONS **************************************************************)
 exception Not_semi_unifiable = Not_semi_unifiable;;
 exception Not_unifiable = Not_unifiable;;
 exception Not_matchable = Not_matchable;;
 
 (*** FUNCTIONS ***************************************************************)
 let (>>=) = M.(>>=);;
 
 (* Renaming *)
 let renaming =
  let rec renaming s = function
   | Var x ->
    if S.mem x s then M.return s
    else M.fresh_var >>= fun y -> M.return (S.add x (Var y) s)
   | Fun (f,ts) -> M.foldl renaming s ts
  in
  renaming S.empty
 ;;
 
 (* Unification *)
 (* TODO *)
 (* let semi_unify_problem p = raise Not_semi_unifiable;; *)
 
 (* t semi_unify u v = semi_unify_problem [(u,v)];; *)
 
 let unify_problem =
  let rec unify s = function
   | [] -> s
   | (u,v) :: p when u = v -> unify s p
   | (Var x as u,v) :: p | (v,(Var x as u)) :: p ->
    if not (Term.is_subterm u v) && S.apply x v s = v then
     let t = S.singleton x v and apply = S.apply_term in
     unify (S.compose apply s t) (List.rev_map (Pair.map (apply t)) p)
    else raise Not_unifiable
   | (Fun (f,us),Fun (g,vs)) :: p ->
    if f = g then unify s (List.rev_append (List.rev_zip us vs) p)
    else raise Not_unifiable
  in
  unify S.empty
 ;;
 
 let unify u v = unify_problem [(u,v)];;
 
 (*
 let are_semi_unifiable u =
  catch Not_semi_unifiable (const true <.> semi_unify u) false
 ;;
 *)
 
 let are_unifiable u = catch Not_unifiable (const true <.> unify u) false;;
 let is_variant u = catch Not_unifiable (S.is_renaming <.> unify u) false;;
 
 (* Matching *)
 (* general matching *)
 let match_problem =
  let rec solve s = function
   | [] -> s
   | (u,Var x) :: p ->
    (try solve (S.add x u s) p with S.Inconsistent -> raise Not_matchable)
   | (Fun (f,us),Fun (g,vs)) :: p ->
    if f = g then solve s (List.rev_append (List.rev_zip us vs) p)
    else raise Not_matchable
   | _ -> raise Not_matchable
  in
  solve S.empty
 ;;
 
 let match_term u v = match_problem [(u,v)];;
 let matches u = catch Not_matchable (const true <.> match_term u) false;;
 let subsumes = flip matches;;
 let contains u v = List.exists (subsumes v) (Term.subterms u);;
 
 (* ground matching *)
 let rec simplify = function
  | [] -> Some []
  | (Var _,_) :: ps -> simplify ps
  | (Fun (f,us),Fun (g,vs)) :: ps ->
   if f = g then simplify (List.combine us vs @ ps) else None
  | (t,Var x) :: ps -> Option.map (List.cons (t,x)) (simplify ps)
 ;;
 
 let rec merge = function
  | Var _,u | u,Var _ -> Some u
  | Fun (f,us),Fun (g,vs) ->
   if f = g then
    let ws =
     List.foldr2 (fun u v ws -> Option.fold (fun ws ->
      Option.map (flip List.cons ws) (merge (u,v))) None ws) (Some []) us vs
    in
    Option.map (fun ws -> Fun (f,ws)) ws
   else None
 ;;
 
 let rec extend x u = function
  | [] -> Some []
  | (v,y) :: ps ->
   if x = y then Option.fold (flip (extend x) ps) None (merge (u,v))
   else Option.map (List.cons (v,y)) (extend x u ps)
 ;;
 
 let rec solve = function
  | [] -> true
  | (u,x) :: ps -> Option.fold solve false (extend x u ps)
 ;;
 
 let ground_matches u v = Option.fold solve false (simplify [(u,v)]);;
end
