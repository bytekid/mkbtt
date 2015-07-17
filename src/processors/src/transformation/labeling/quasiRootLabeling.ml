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

(*** MODULES (part 1) *********************************************************)
module C    = Complexity;;
module F    = Format;;
module Fun  = Function;;
module M    = Monad;;
module P    = Problem;;
module Var  = Variable;;
module Ctxt = Context;;
module G    = Util.Graph.Make(Fun);;
module S    = Signature;;

(*** TYPES ********************************************************************)
type intp = (Fun.t * int)list;;
type t = P.t * intp * P.t;;

type flags = {
 help : bool ref;
 max  : int ref;
};;

(*** GLOBALS ******************************************************************)
let code     = "qlab";;
let name     = "Quasi Root-Labeling Processor";;
let comment  = "Applies quasi root-labeling."
let keywords = ["quasi";"self";"root";"labeling";"labelling";"termination"];;

let flags = {
  help = ref false;
  max  = ref(-1);
};;

let spec =
 let spec = [
  ("--help",Arg.Set flags.help,"Prints information about flags.");
  ("-help",Arg.Set flags.help,"Prints information about flags.");
  ("-h",Arg.Set flags.help,"Prints information about flags.");
  ("--max",Arg.Set_int flags.max,"Maximum number of different labels used.");
  ("-n",Arg.Set_int flags.max,"Maximum number of different labels used.");
 ] in
 Arg.alignx 80 spec
;;

let help = (comment,keywords,List.map Triple.drop_snd spec);;

(*** FUNCTIONS ****************************************************************)
let (>>=) = Monad.(>>=);;

let init _ = 
  flags.help := false;
  flags.max  := -1;
;;

(* Destructors *)
let get_ip(p,_,_) = p;;
let get_op(_,_,p) = p;;
let get_intp(_,i,_) = i;;

let assignments vs n =
  List.map (List.combine vs) (List.combinations (List.length vs) (List.range 0 n))
;;

let interpret intp ass = function
  | Term.Var x    -> List.assoc x ass
  | Term.Fun(f,_) -> List.assoc f intp
;;

(******************************************************************************)
(**)let rec label_term intp ass = function
(**)  | Term.Var _ as x     -> M.return x
(**)  | Term.Fun(f,ts)      ->
(**)    let label = List.rev_map (interpret intp ass) ts in
(**)    M.set_qlab f label             >>= fun f' ->
(**)    M.map (label_term intp ass) ts >>= fun ts' ->
(**)    M.return(Term.Fun(f',ts'))
(**);;
(**)
(**)let label_rule lab n rule =
(**)  let (l,r) = Rule.to_terms rule in
(**)  M.map (fun ass ->
(**)    lab ass l >>= fun lhs ->
(**)    lab ass r >>= fun rhs ->
(**)    M.return(Rule.of_terms lhs rhs)
(**)  ) (assignments (Rule.vars rule) n)
(**);;
(**)
(**)(* quasi root labeling of a TRS [trs] *)
(**)let ql lab n trs =
(**)  M.flat_map (label_rule lab n) (Trs.to_list trs);;
(******************************************************************************)

let fresh_vars n = M.replicate n
  (M.fresh_var >>= fun x -> M.return(Term.Var x));;

let decreasing_rules fs n = M.flat_map (fun f ->
  M.find_ari f >>= fun a ->
  fresh_vars a >>= fun xs ->
  let vals  = List.combinations a (List.range 0 n) in
  let pairs = List.zip (List.tl vals) vals in
  M.map (fun(i,j) ->
    M.set_qlab f (List.rev i) >>= fun fi ->
    M.set_qlab f (List.rev j) >>= fun fj ->
    M.return(Rule.of_terms (Term.Fun(fi,xs)) (Term.Fun(fj,xs)))) pairs
) fs;;

let make_interpretation fs trs =
  let graph = Trs.fold (fun rule graph ->
    match Pair.map Term.root (Rule.to_terms rule) with
      | (Some f,Some g) -> G.add_edge (f,g) graph
      | (Some f,None)   -> List.fold_right (G.add_edge <.> pair f) fs graph
      | _               -> graph
  ) (List.foldr G.add_node G.empty fs) trs
  in
  let rel = List.collapse ~n:(!(flags.max)) (List.rev(G.sccs graph)) in
  (List.flat_mapi (fun i -> List.map (flip pair i)) rel,List.length rel)
;;

let solve fs p = 
  let configurate s = F.printf "%s@\n%!" s; flags.help := true in
  (try init(); Arg.parsex code spec fs with Arg.Bad s -> configurate s);
  if !(flags.help) then (Arg.usage spec ("Options for "^code^":"); exit 0);
  if P.is_sp p then (
    let trs      = P.get_trs p in
    let fs       = Trs.funs trs in
    let (intp,n) = make_interpretation fs trs in
    if n = 1 then M.return(None) else (
      ql (label_term intp) n trs >>= fun trs' ->
      decreasing_rules fs n      >>= fun decr ->
      M.return(Some(p,
        intp, 
        P.set_trs (Trs.of_list(trs'@decr)) p)
      )
    )
  ) else if P.is_dp p then (
    let (s,w)    = P.get_sw p in
    let fs_r     = Trs.roots s in
    let fs       = List.union (Trs.funs w) (List.diff (Trs.funs s) fs_r) in
    let (intp,n) = make_interpretation fs w in
    if n = 1 then M.return(None) else (
      ql (label_term intp) n s >>= fun s' ->
      ql (label_term intp) n w >>= fun w' ->
      decreasing_rules fs_r n  >>= fun decr ->
      decreasing_rules fs n    >>= fun decr' ->
      M.return(Some(p,
        List.map (flip pair 0) fs_r @ intp,
        P.set_dg P.Complete (
          P.set_sw (Trs.of_list(s'@decr)) (Trs.of_list(w'@decr')) p
        ))
      )
    )
  ) else failwith(F.sprintf "%s: no quasi root-labeling for relative \
                             termination" name)
;;

(* Complexity Bounds *)
let complexity c _ = C.mul c C.constant;;

(* Compare Functions *)
let equal p q = P.equal (get_ip p) (get_ip q);;

(* Printers *)
let fprintf_intp fmt = M.iter (fun(f,i) ->
  M.get >>= fun s ->
  F.fprintf fmt "[";
  let s = S.fprintf_fun fmt f s in
  F.fprintf fmt "] = %i@\n" i;
  M.set s >>= fun _ ->
  M.return()
);;

let fprintf fs fmt p =
  F.fprintf fmt "@[<1>%s:@\n" name; P.fprintfm fmt (get_op p) >>= fun _ ->
  F.fprintf fmt "@]@\n@\n@[<1>Interpretation:@\n";
  fprintf_intp fmt (get_intp p)       >>= fun _ ->
  F.fprintf fmt "@\n"; List.hd fs fmt >>= fun _ ->
  M.return(F.fprintf fmt "@]")
;;

let fprintfx fs fmt p =
  let (tag,tag') = if P.is_sp(get_op p) then ("semlab","flatContextClosure")
                                        else ("semlabProc","flatContextClosureProc")
  in
  F.fprintf fmt "@{<%s>@{<model><semantic/>@}" tag;
  P.fprintfx fmt (get_op p) >>= fun _ ->
  List.hd fs fmt            >>= fun _ ->
  M.return(F.fprintf fmt "@}")
;;
