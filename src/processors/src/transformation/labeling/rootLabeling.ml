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

(*** TYPES ********************************************************************)
type t = P.t * (Fun.t option * Ctxt.t list * P.t)option * P.t;;

type flags = {
 fresh : bool ref;
 help  : bool ref;
};;

(*** GLOBALS ******************************************************************)
let code     = "rlab";;
let name     = "Root-Labeling Processor";;
let comment  = "Applies root-labeling."
let keywords = ["self";"root";"labeling";"labelling";"termination"];;

let flags = {
  fresh = ref false;
  help  = ref false;
};;

let spec =
 let spec = [
  ("--help",Arg.Set flags.help,"Prints information about flags.");
  ("-help",Arg.Set flags.help,"Prints information about flags.");
  ("-h",Arg.Set flags.help,"Prints information about flags.");
  ("-fresh",Arg.Set flags.fresh,"Keep P and R separate by \
    introducing a fresh function symbol");
 ] in
 Arg.alignx 80 spec
;;

let help = (comment,keywords,List.map Triple.drop_snd spec);;

(*** FUNCTIONS ****************************************************************)
let (>>=) = Monad.(>>=);;

let init _ = 
  flags.help  := false;
  flags.fresh := false;
;;

(* Destructors *)
let get_ip(p,_,_) = p;;
let get_op(_,_,p) = p;;
let get_fcc(_,fcc,_) = fcc;;

let assignments vs fs =
  List.map (List.combine vs) (List.combinations (List.length vs) fs)
;;

let interpret ass = function
  | Term.Var x    -> List.assoc x ass
  | Term.Fun(f,_) -> f
;;

let fresh_vars n =
  M.replicate n (M.fresh_var >>= (M.return <.> Term.make_var));;

let fresh_fun n =
  M.fresh_fun   >>= fun f ->
  M.add_ari f n >>= fun _ ->
  M.return f
;;

(* NOTE: if fs does only consist of constants, then flat_contexts fs will
   be empty! *)
let flat_contexts fs =
  (* generate a context f(x1,...,[],...,xN),
     where the xI's are fresh and [] is at position i *)
  let fci f n i =
    fresh_vars i       >>= fun ts1 ->
    fresh_vars (n-i-1) >>= fun ts2 ->
    M.return(Ctxt.More(f,ts1,Ctxt.Hole,ts2))
  in
  M.flat_map (fun f ->
    M.find_ari f >>= fun a ->
    if a > 0 then M.replicatei a (fci f a)
             else M.return [] (* constants do not give rise to flat contexts *)
  ) fs
;;

(******************************************************************************)
(**)let rec root_label_term ass = function
(**)  | Term.Var _ as x     -> M.return x
(**)  | Term.Fun(f,ts)      ->
(**)    let label = List.rev_map (interpret ass) ts in
(**)    M.set_rlab f label             >>= fun f' ->
(**)    M.map (root_label_term ass) ts >>= fun ts' ->
(**)    M.return(Term.Fun(f',ts'))
(**);;
(**)
(**)let root_label_rule lab fs rule =
(**)  let (l,r) = Rule.to_terms rule in
(**)  M.map (fun ass ->
(**)    lab ass l >>= fun lhs ->
(**)    lab ass r >>= fun rhs ->
(**)    M.return(Rule.of_terms lhs rhs)
(**)  ) (assignments (Rule.vars rule) fs)
(**);;
(**)
(**)(* root labeling of a TRS [trs] *)
(**)let rl lab fs trs =
(**)  M.flat_map (root_label_rule lab fs) (Trs.to_list trs);;
(******************************************************************************)

let root_altering rule =
  Term.is_var(Rule.rhs rule)
    || Term.root(Rule.lhs rule) <> Term.root(Rule.rhs rule)
;;

(******************************************************************************)
(**)(* closure under flat contexts of TRS [trs] w.r.t. the set of
(**)   function symbols [fs] *)
(**)let _FC trs fcs = M.return(match fcs with
(**)  | []  -> trs
(**)  | fcs -> Trs.of_list(Trs.flat_map (fun r ->
(**)    if root_altering r
(**)      then List.map (fun c -> Rule.project (flip Ctxt.apply c) r) fcs
(**)      else [r]
(**)  ) trs)
(**));;
(**)
(**)let _FC1 f fcs s w =
(**)  let block f =
(**)    Rule.project (function
(**)      | Term.Fun(g,ts) -> Term.Fun(g,List.map (fun t -> Term.Fun(f,[t])) ts)
(**)      | _ -> failwith "RootLabeling: lhs or rhs is a variable!"
(**)    )
(**)  in
(**)  let s = Trs.of_list(Trs.map (block f) s) in
(**)  _FC w fcs   >>= fun w ->
(**)  M.return(s,w)
(**);;
(**)
(**)let _FC2 fcs_r fcs s w =
(**)  let ra = Trs.filter root_altering w in
(**)  _FC ra fcs_r       >>= fun w' ->
(**)  _FC w fcs          >>= fun w  ->
(**)  M.return(Trs.union s w',w)
(**);;
(******************************************************************************)

let solve fs p = 
  let configurate s = F.printf "%s@\n%!" s; flags.help := true in
  (try init(); Arg.parsex code spec fs with Arg.Bad s -> configurate s);
  if !(flags.help) then (Arg.usage spec ("Options for "^code^":"); exit 0);
  if P.is_sp p then (
    if !(flags.fresh)
    then failwith(F.sprintf "%s: -fresh only works for DP problems" name)
    else
    let trs = P.get_trs p in
    let fs = Trs.funs trs in
    flat_contexts fs           >>= fun fcs ->
    _FC trs fcs                >>= fun trs' ->
    rl root_label_term fs trs' >>= fun trs'' ->
    M.return(Some(p,(if trs = trs'
      then None
      else Some(None,fcs,P.set_trs trs' p)),
      P.set_trs (Trs.of_list trs'') p)
    )
  ) else if P.is_rp p || P.is_cp p then (
    if !(flags.fresh)
    then failwith(F.sprintf "%s: -fresh only works for DP problems" name)
    else
    let s = P.get_strict p and w = P.get_weak p in
    let trs = Trs.union s w in
    let fs = Trs.funs trs in
    flat_contexts fs         >>= fun fcs ->
    _FC s fcs                >>= fun s' ->
    _FC w fcs                >>= fun w' ->
    rl root_label_term fs s' >>= fun s'' ->
    rl root_label_term fs w' >>= fun w'' ->
    M.return(Some(p,(if Trs.equal s s' && Trs.equal w w' 
      then None
      else Some(None,fcs,P.set_sw s' w' p)),
      P.set_sw (Trs.of_list s'') (Trs.of_list w'') p)
    )
  ) else if P.is_dp p then (
    if !(flags.fresh) then (
      let (s,w) = P.get_sw p in
      let fs_r  = Trs.roots s in
      let fs    = List.union (Trs.funs w) (List.diff (Trs.funs s) fs_r) in
      fresh_fun 1              >>= fun f ->
      flat_contexts (f::fs)    >>= fun fcs ->
      _FC1 f fcs s w           >>= fun(s',w') ->
      rl root_label_term (f::fs) s' >>= fun s'' ->
      rl root_label_term (f::fs) w' >>= fun w'' ->
      M.return(Some(p,(if w = w'
        then None
        else Some(Some f,fcs,P.set_sw s' w' p)),
        (* IMPORTANT! *)
        P.set_dg P.Complete (P.set_sw (Trs.of_list s'') (Trs.of_list w'') p))
      )
    ) else (
      let (s,w) = P.get_sw p in
      let fs_r = Trs.roots s in
      let fs   = List.union (Trs.funs w) (List.diff (Trs.funs s) fs_r) in
      flat_contexts fs_r       >>= fun fcs_r ->
      flat_contexts fs         >>= fun fcs ->
      _FC2 fcs_r fcs s w       >>= fun(s',w') ->
      rl root_label_term fs s' >>= fun s'' ->
      rl root_label_term fs w' >>= fun w'' ->
      M.return(Some(p,(if s = s' && w = w'
        then None
        else Some(None,fcs_r@fcs,P.set_sw s' w' p)),
        (* IMPORTANT! *)
        P.set_dg P.Complete (P.set_sw (Trs.of_list s'') (Trs.of_list w'') p))
      )
    )
  ) else
    failwith(F.sprintf "%s: no root-labeling for this type of problem" name)
;;

(* Complexity Bounds *)
let complexity c _ = C.mul c C.constant;;

(* Compare Functions *)
let equal p q = P.equal (get_ip p) (get_ip q);;

(* Printers *)
let fprintf fs fmt p =
  F.fprintf fmt "@[<1>%s:@\n" name; P.fprintfm fmt (get_op p) >>= fun _ ->
  F.fprintf fmt "@\n"; List.hd fs fmt >>= fun _ -> M.return (F.fprintf fmt "@]")
;;

let fprintfx_contexts fmt = M.iter (Ctxt.fprintfx fmt);;

let fprintfx fs fmt p =
  let (tag,tag') = if P.is_sp(get_op p) then ("semlab","flatContextClosure")
                                        else ("semlabProc","flatContextClosureProc")
  in
  (match get_fcc p with
    | Some(Some f,fcs,fcc) ->
      F.fprintf fmt "@{<%s>" tag';
      F.fprintf fmt "@{<freshSymbol>";
      M.fprintfx_fun fmt f >>= fun _ -> 
      F.fprintf fmt "@}";
      F.fprintf fmt "@{<flatContexts>";
      fprintfx_contexts fmt fcs >>= fun _ ->
      F.fprintf fmt "@}";
      P.fprintfx fmt fcc
    | Some(None,fcs,fcc) ->
      F.fprintf fmt "@{<%s>" tag';
      F.fprintf fmt "@{<flatContexts>";
      fprintfx_contexts fmt fcs >>= fun _ ->
      F.fprintf fmt "@}";
      P.fprintfx fmt fcc
    | None -> M.return()
  ) >>= fun _ ->
  F.fprintf fmt "@{<%s>@{<model><rootLabeling/>@}" tag;
  P.fprintfx fmt (get_op p) >>= fun _ ->
  List.hd fs fmt            >>= fun _ ->
  F.fprintf fmt "@}";
  M.return(match get_fcc p with
    | Some _ -> F.fprintf fmt "@}"
    | None   -> ())
;;
