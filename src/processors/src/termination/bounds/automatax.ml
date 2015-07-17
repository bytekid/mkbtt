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
open Util;;
open Rewritingx;;

(*** MODULES ******************************************************************)
module Automata = Automata.Make (Rewritingx) (Categorization);;
module Monad = Automata.Monad;;
module Parser = Automata.Parser;;
module Rhs = Automata.Rhs;;
module Status = Automata.Status;;
module Substitution = Automata.Substitution;;
module Term = Automata.Term;;

module State = struct
 (*** INCLUDES ****************************************************************)
 include Automata.State;;

 (*** MODULES *****************************************************************)
 module F = Format;;

 (*** FUNCTIONS ***************************************************************)
 (* Printers *)
 let fprintfx fmt p = F.fprintf fmt "@{<state>%a@}" fprintf p;;
 let to_stringx = F.flush_str_formatter <.> fprintfx F.str_formatter;;
end

module Lhs = struct
 (*** INCLUDES ****************************************************************)
 include Automata.Lhs;;

 (*** MODULES *****************************************************************)
 module F = Format;;
 module M = Monad;;
 module Monad = Rewritingx.Monad;;

 (*** FUNCTIONS ***************************************************************)
 let (>>=) = M.(>>=);;

 (* Printers *)
 let fprintfx fmt = function
  | State p -> M.return (F.fprintf fmt "@{<state>%a@}" State.fprintfx p)
  | Fun (f,ps) ->
   F.fprintf fmt "@{<funapp>"; M.liftm (Monad.fprintfx_fun fmt f) >>= fun _ ->
   M.return (F.fprintf fmt "@{<arg>%a@}@}" (List.fprintf State.fprintfx "") ps)
 ;;

 let to_stringx t =
  fprintfm F.str_formatter t >>= (M.return <.> F.flush_str_formatter)
 ;;
end

module Automaton = struct
 (*** INCLUDES ****************************************************************)
 include Automata.Automaton;;

 (*** MODULES *****************************************************************)
 module F = Format;;
 module Fun = Function;;
 module M = Monad;;
 module Monad = Rewritingx.Monad;;

 (*** FUNCTIONS ***************************************************************)
 let (>>=) = M.(>>=);;

 (* Raise Consistency *)
 let explicit qc l r a = match l with
  | Lhs.State _ -> failwith "lhs is state"
  | Lhs.Fun (f,ps) ->
   M.liftm (Monad.get_height f) >>= fun h -> category f a >>= fun fs ->
   M.foldl (fun ((h'',r''),a) f' ->
    M.liftm (Monad.get_height f') >>= fun h' ->
    if h' < h then
     foldf_trans f' (fun l' r' m ->
      let l'' = Lhs.Fun (f,Lhs.args l') and ps = Rhs.states r' in
      m >>= fun (r'',a) -> (if qc then remove l' a else M.return a) >>= fun a ->
      if l'' = l then M.return (Rhs.extend ps r'',a) else
       M.ite (mem l'' a) (extend l'' ps) (add l'' r') a >>=
       (M.return <.> pair r'')) a (M.return (r'',a)) >>= fun (r'',a) ->
     M.return ((h'',r''),a)
    else if h' > h then
     let l'' = Lhs.Fun (f',ps) in
     M.ite (mem l'' a) (extend l'' (Rhs.states r)) (add l'' r) a >>=
     (M.return <.> pair (max h' h'',r''))
    else M.return ((h'',r''),a)) ((h,r),a) fs >>= fun ((h'',r''),a) ->
   if qc && h'' > h then M.return a else
    M.ite (mem l a) (extend l (Rhs.states r'')) (add l r'') a
 ;;

 let implicit qc l r a = match l with
  | Lhs.State _ -> failwith "lhs is state"
  | Lhs.Fun (f,ps) ->
   M.liftm (Monad.get_height f) >>= fun h -> category f a >>= fun fs ->
   M.foldl (fun ((h'',r''),a) f' ->
    M.liftm (Monad.get_height f') >>= fun h' ->
    if h' < h then
     let l'' = Lhs.Fun (f',ps) in
     M.lift (Option.fold Rhs.states []) (find l'' a) >>= fun ps ->
     (if qc && ps <> [] then remove l'' a else M.return a) >>= fun a ->
     M.return ((h'',Rhs.extend ps r''),a)
    else if h' > h then
     let l'' = Lhs.Fun (f',ps) in mem l'' a >>= fun c ->
     if c then M.lift (pair (max h' h'',r'')) (extend l'' (Rhs.states r) a)
     else M.return ((h'',r''),a)
    else M.return ((h'',r''),a)) ((h,r),a) fs >>= fun ((h'',r''),a) ->
   if qc && h'' > h then M.return a else
    M.ite (mem l a) (extend l (Rhs.states r'')) (add l r'') a
 ;;

 let raise_cons qc f a =
  let clear a = fold_trans (fun l _ m -> m >>= remove l) a (M.return a) in
  fold_trans (fun l r m -> m >>= (if Lhs.is_state l then add else f qc) l r)
   a (clear (copy a))
 ;;

 let explicit ?(qc = false) = raise_cons qc explicit;;
 let implicit ?(qc = false) = raise_cons qc implicit;;

 (* quasi-deterministic and raise consistent *)
 let rec complete ?(qc = false) ?(rc = implicit) a =
  let n = size a in rc ~qc:qc a >>= quasi_det >>= fun a ->
  if n = size a then M.return a else complete ~qc:qc ~rc:rc a
 ;;

 (* Miscellaneous *)
 let bound a =
  fold_trans (fun l _ m ->
   let f = Option.the (Lhs.root l) in
   M.liftm (Monad.get_height f) >>= (flip M.lift m <.> max)) a (M.return 0)
 ;;

 (* Printers *)
 let fprintfx_trans fmt l r =
  F.fprintf fmt "@{<transition>@{<lhs>"; Lhs.fprintfx fmt l >>= fun _ ->
  M.return (F.fprintf fmt "@}@{<rhs>%a@}@}" State.fprintfx r)
 ;;

 let fprintfx fmt a =
  F.fprintf fmt "@{<treeAutomaton>@{<finalStates>";
  F.fprintf fmt "%a@}" (List.fprintf State.fprintfx "") (finals ~i:true a);
  let ts = fold (fun l -> Rhs.fold (List.cons <.> pair l)) a [] in
  M.iter (uncurry (fprintfx_trans fmt)) ts >>= fun _ ->
  M.return (F.fprintf fmt "@}")
 ;;
 
 let to_stringx g =
  fprintfx F.str_formatter g >>= (M.return <.> F.flush_str_formatter)
 ;;
end

module Path = struct
 (*** INCLUDES ****************************************************************)
 include Automata.Path;;

 (*** MODULES *****************************************************************)
 module A = Automaton;;
 module M = Monad;;
 module Monad = Rewritingx.Monad;;

 (*** FUNCTIONS ***************************************************************)
 let (>>=) = M.(>>=);;

 let fresh_funs ?(c = false) =
  let fresh t a = match t with
   | Lhs.Fun (f,[]) ->
    A.find ~c:c t a >>= Option.fold (M.return <.> Rhs.max) M.fresh_state
   | _ -> M.fresh_state
  in
  fresh_reuse ~f:fresh
 ;;

 let fresh_reuse ?(c = false) =
  let fresh t a = match t with
   | Lhs.Fun (f,ts) ->
    if c then
     let make_fun = flip Lhs.make_fun ts in
     let set_height = M.liftm <.> flip Monad.set_height 0 in
     M.liftm (Monad.drop_height f) >>= (M.lift make_fun <.> set_height) >>=
     flip (A.find ~c:c) a >>= Option.fold (M.return <.> Rhs.max) M.fresh_state
    else A.find ~c:c t a >>= Option.fold (M.return <.> Rhs.max) M.fresh_state
   | _ -> M.fresh_state
  in
  fresh_reuse ~f:fresh
 ;;
end

module Initial = struct
 (*** INCLUDES ****************************************************************)
 include Automata.Initial;;

 (*** MODULES *****************************************************************)
 module A = Automaton;;
 module F = Format;;
 module M = Monad;;
 module Monad = Rewritingx.Monad;;
 module T = Rewritingx.Term;;

 (*** FUNCTIONS ***************************************************************)
 let (>>) = M.(>>);;
 let (>>=) = M.(>>=);;

 let transform f t =
  M.liftm (Monad.set_height f 0) >>= fun f ->
  let rec transform = function
   | T.Var _ -> M.return (Term.Fun (f,[]))
   | T.Fun (f,rs) ->
    M.liftm (Monad.set_height f 0) >>= fun f ->
    M.lift (Term.make_fun f) (M.map transform rs)
  in
  transform t
 ;;

 let extend fs =
  if fs <> [] then M.return fs else
   M.fresh_fun >>= fun f -> M.add_ari f 0 >>
   M.liftm (Monad.set_height f 0) >>= (M.return <.> List.singleton)
 ;;

 let specific f trs =
  let cp = Path.fresh_funs in
  let fresh = const (M.lift List.singleton M.fresh_state) in
  M.map (transform f) (Trs.rhs trs) >>= (specific fresh cp <.> List.unique_hash)
 ;;

 let custom fs = if fs = [] then individual else custom fs;;

 let individual s w =
  M.liftm (Trs.label_height 0 (Trs.union s w)) >>= fun trs ->
  M.filter (M.liftm <.> Monad.is_dp) (Trs.def_symbols trs) >>= fun dps ->
  let mem fs gs f = List.mem f fs || List.mem f gs in
  let cs = Trs.left_con_symbols trs and rfs = Trs.right_funs trs in
  let cs = List.filter (not <.> mem dps rfs) cs in
  let fs = List.filter (not <.> mem dps cs) (Trs.funs trs) in
  if cs <> [] || fs <> [] then custom dps cs fs
  else extend cs >>= flip (custom dps) []
 ;;
  
 let split s w =
  let trs = Trs.union s w in
  M.filter (M.liftm <.> Monad.is_dp) (Trs.def_symbols trs) >>= fun dps ->
  let fs = List.filter (not <.> flip List.mem dps) (Trs.funs trs) in
  let n = float_of_int (List.length fs) in
  let size f = M.find_ari f >>= fun m -> M.return (n ** float_of_int m) in
  M.foldl (fun i f -> M.lift ((+.) i) (size f)) 0.0 fs >>= fun n ->
  if n > 750.0 then individual s w else
   M.liftm (Trs.label_height 0 trs) >>= fun trs ->
   M.filter (M.liftm <.> Monad.is_dp) (Trs.def_symbols trs) >>= fun dps ->
   let fs = List.filter (not <.> flip List.mem dps) (Trs.funs trs) in
   extend fs >>= flip (custom dps) []
 ;;

 let instances s w =
  M.liftm (Trs.label_height 0 s) >>= fun s ->
  M.liftm (Trs.label_height 0 w) >>= fun w ->
  M.filter (M.liftm <.> Monad.is_dp) (Trs.def_symbols s) >>= fun dps ->
  let rs = List.map Term.of_term (List.unique_hash (Trs.rhs s)) in
  let fs = List.filter (not <.> flip List.mem dps) (Trs.funs s) in
  let fs = List.unique_hash (List.rev_append fs (Trs.funs w)) in
  let fresh = const (M.lift List.singleton M.fresh_state) in
  let cp = Path.fresh_funs and n = float_of_int (List.length fs) in
  let size f = M.find_ari f >>= fun m -> M.return (n ** float_of_int m) in
  M.foldl (fun i f -> M.lift ((+.) i) (size f)) 0.0 fs >>= fun n ->
  (if n <= 750.0 then extend fs >>= fun fs -> instances fresh cp fs [] rs else
   let cs = Trs.left_con_symbols s and rfs = Trs.right_funs s in
   let cs = List.filter (not <.> flip List.mem rfs) cs in
   let fs = List.filter (not <.> flip List.mem cs) fs in
   if cs <> [] || fs <> [] then instances fresh cp cs fs rs
   else extend cs >>= fun cs -> instances fresh cp cs [] rs) >>= fun a ->
  if Trs.is_duplicating s || Trs.is_duplicating w then A.det a >>= A.reduce
  else M.return a
 ;;

 let constructor s w =
  M.liftm (Trs.label_height 0 (Trs.union s w)) >>= fun trs ->
  let ds = Trs.def_symbols trs and cs = Trs.con_symbols trs in
  let n = float_of_int (List.length cs) in
  let size f = M.find_ari f >>= fun m -> M.return (n ** float_of_int m) in
  M.foldl (fun i f -> M.lift ((+.) i) (size f)) 0.0 cs >>= fun n ->
  if n <= 750.0 then extend cs >>= flip (custom ds) [] else
   let rfs = Trs.right_funs s in
   let add (cs,fs) f = if List.mem f rfs then (cs,f::fs) else (f::cs,fs) in
   let (cs,fs) = List.foldl add ([],[]) cs in
   if cs <> [] || fs <> [] then custom ds cs fs
   else extend cs >>= flip (custom ds) []
 ;;
end
