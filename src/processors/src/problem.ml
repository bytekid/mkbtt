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
module F = Format;;
module M = Monad;;

(*** OPENS ********************************************************************)
open Util;;
open Rewritingx;;

(*** TYPES ********************************************************************)
type graph = Complete | Partial of Graph.t;;
type language = All | Constructor;;
type strategy = Full | Innermost | Outermost;;

type dp = {dps : Trs.t; trs : Trs.t; dg : graph};;
type standard = Trs.t;;
type relative = {strict : Trs.t; weak : Trs.t};;

type confluence = {orig: Trs.t; cstrict: Trs.t; cweak: Trs.t; cds: Diagram.t list};;

(* SW *)
type equational = {eqs: Trs.t; etrs: Trs.t}
type equational_dp = {
 eqssharp: Trs.t; 
 edps: Trs.t; 
 dpeqs: Trs.t; 
 edptrs: Trs.t; 
 srules: Trs.t;
 edg: graph
}

type problem =
 | Cp of relative
 | Dp of dp
 | Standard of standard
 | Relative of relative
 | Confluence of confluence
(* SW *)
 | EqStandard of equational
 | EqDp of equational_dp
;;

type t = {l : language; s : strategy; p : problem}

(*** FUNCTIONS ****************************************************************)
let (>>=) = M.(>>=);;

(* Constructors *)
let create_cp s w = Cp {strict = s; weak = w};;
let create_dp dps trs dg = Dp {dps = dps; trs = trs; dg = dg};;
let create_sp trs = Standard trs;;
let create_rp s w = Relative {strict = s; weak = w};;
let create_crp trs s w cds = 
 Confluence {orig = trs; cstrict = s; cweak = w; cds = cds;};;
(*SW *)
let create_edp de dp e trs s g = 
 EqDp {eqssharp=de; edps=dp; dpeqs=e; edptrs=trs; srules=s; edg=g}
;;
let create_ep e trs = EqStandard {eqs=e; etrs=trs};;


let make_cp l strategy s w = {l = l; s = strategy; p = create_cp s w};;
let make_sp l strategy trs = {l = l; s = strategy; p = create_sp trs};;
let make_rp l strategy s w = {l = l; s = strategy; p = create_rp s w};;
let make_crp l strategy trs s w cds = 
 {l = l; s = strategy; p = create_crp trs s w cds;}
;;
let make_dp l strategy dps trs dg =
 {l = l; s = strategy; p = create_dp dps trs dg}
;;
(* SW *)
let make_edp l strategy de dp e trs s g = 
 {l = l; s = strategy; p = create_edp de dp e trs s g}
;;
let make_ep l strategy e trs =
 {l = l; s = strategy; p = create_ep e trs}
;;

let set_trs trs p = match p with
 | {p = Standard _} -> {p with p = Standard trs}
 | {p = Dp p'} -> {p with p = Dp {p' with trs = trs}}
 | {p = Confluence p'} -> {p with p = Confluence {p' with orig = trs}}
 | {p = EqDp p'} -> {p with p = EqDp {p' with edptrs = trs}}
 | {p = EqStandard p'} -> {p with p = EqStandard {p' with etrs = trs}}
 | _ -> failwith "not a standard termination or DP problem"
;;

let set_dg dg p = match p with
 | {p = Dp p'} -> {p with p = Dp {p' with dg = dg}}
 | {p = EqDp p'} -> {p with p = EqDp {p' with edg = dg}}
 | _ -> failwith "not a dp problem"
;;

let set_dps dps p = match p with
 | {p = Dp p'} -> {p with p = Dp {p' with dps = dps}}
 | {p = EqDp p'} -> {p with p = EqDp {p' with edps = dps}}
 | _ -> failwith "not a dp or equational problem"
;;

let set_strict s p = match p with
 | {p = Cp p'} -> {p with p = Cp {p' with strict = s}}
 | {p = Relative p'} -> {p with p = Relative {p' with strict = s}}
 | _ -> failwith "not a relative termination or CP problem"
;;

let set_weak w p = match p with
 | {p = Cp p'} -> {p with p = Cp {p' with weak = w}}
 | {p = Relative p'} -> {p with p = Relative {p' with weak = w}}
 | _ -> failwith "not a relative termination or CP problem"
;;

let get_eqs = function
 | {p = EqDp p} -> Trs.union p.eqssharp p.dpeqs
 | {p = EqStandard p} -> p.eqs
 | _ -> failwith "not an equational problem"
;;

let get_eqssharp = function
 | {p = EqDp p} -> p.eqssharp
 | _ -> failwith "not an equational DP problem"
;;

let set_sw s w p = match p with
 | {p = Cp _} -> {p with p = Cp {strict = s; weak = w}}
 | {p = Dp p'} -> {p with p = Dp {p' with dps = s; trs = w}}
 | {p = Standard _} ->
  if Trs.is_empty w then {p with p = Standard s}
  else failwith "not a standard termination problem"
 | {p = Relative _} -> {p with p = Relative {strict = s; weak = w}}
 | {p = Confluence p'} -> {p with p = Confluence {p' with cstrict = s; cweak = w}}
 | {p = EqDp p'} ->
  let dp = Trs.of_list (List.diff (Trs.to_list s) (Trs.to_list p'.srules)) in
  {p with p = EqDp {p' with edps=dp}}
 | {p = EqStandard p'} ->
  {p with p = EqStandard {p' with etrs=s}}
;;

let set_cds cds p = match p with
 | {p = Confluence p'} -> {p with p = Confluence {p' with cds = cds}}
 | _ -> p
;;

(* SW *)
let set_srules s p = match p with
 | {p = EqDp p'} -> {p with p = EqDp {p' with srules = s}}
 | _ -> p
;;

let set_eqs s p = match p with
 | {p = EqDp p'} -> {p with p = EqDp {p' with dpeqs = s}}
 | {p = EqStandard p'} -> {p with p = EqStandard {p' with eqs = s}}
 | _ -> p
;;

let set_deqs s p = match p with
 | {p = EqDp p'} -> {p with p = EqDp {p' with eqssharp = s}}
 | _ -> p
;;

let get_cds = function
 | {p = Confluence p} -> p.cds
 | _ -> failwith "not a confluence problem"
;;

let set_language l p = {p with l = l};;
let set_strategy s p = {p with s = s};;
let adapt p' p = {p with p = p'};;

let minimize_dg dps = function
 | Complete -> Complete
 | Partial g -> Partial (Graph.filter_nodes (flip Trs.mem dps) g)
;;

let minimize p = match p with
 | {p = Dp p'} -> {p with p = Dp {p' with dg = minimize_dg p'.dps p'.dg}}
 | {p = EqDp q} -> {p with p = EqDp {q with edg = minimize_dg q.edps q.edg}}
 | p -> p
;;

(* Access Functions *)
let get_trs = function
 | {p = Standard p} -> p
 | {p = Dp p} -> p.trs
 | {p = Confluence p} -> p.orig
 | {p = EqDp p} -> p.edptrs
 | {p = EqStandard p} -> p.etrs
 | _ -> failwith "not a standard termination or DP problem"

let get_dg = function
 | {p = Dp p} -> p.dg
 | {p = EqDp p} -> p.edg
 | _ -> failwith "not a DP problem"
;;

let get_dps = function
 | {p = Dp p} -> p.dps
 | {p = EqDp p} -> p.edps
 | _ -> failwith "not a DP problem"
;;

let get_strict = function
 | {p = Cp p} | {p = Relative p} -> p.strict
 | _ -> failwith "not a relative termination or CP problem"
;;

let get_weak = function
 | {p = Cp p} | {p = Relative p} -> p.weak
 | _ -> failwith "not a relative termination or CP problem"
;;


let get_sw = function
 | {p = Dp p} -> (p.dps,p.trs)
 | {p = Standard p} -> (p,Trs.empty)
 | {p = Cp p} | {p = Relative p} -> (p.strict,p.weak)
 | {p = Confluence p} | {p = Confluence p} -> (p.cstrict,p.cweak)
 | {p = EqStandard p} -> (p.etrs, p.eqs)
 | {p = EqDp p} as p' -> 
  (p.edps, Trs.union (Trs.union p.edptrs (get_eqs p')) p.srules)
;;

let get_eqs = function
 | {p = EqStandard p} -> p.eqs
 | {p = EqDp p} -> p.dpeqs
 | _ -> failwith "not an equational problem"
;;

let get_srules = function
 | {p = EqDp p} -> p.srules
 | {p = EqStandard p} ->  Trs.empty
 | _ -> failwith "not an equational problem"
;;

let get_language p = p.l;;
let get_strategy p = p.s;;

(* Properties *)
let is_cp = function {p = Cp _} -> true | _ -> false;;
let is_dp = function {p = Dp _} -> true | _ -> false;;
let is_sp = function {p = Standard _} -> true | _ -> false;;
let is_rp = function {p = Relative _} -> true | _ -> false;;
let is_crp = function {p = Confluence _} -> true | _ -> false;;
let is_edp = function {p = EqDp _} -> true | _ -> false;;
let is_ep = function {p = EqStandard _} -> true | _ -> false;;
let is_ft = function {s = Full} -> true | _ -> false;;
let is_it = function {s = Innermost} -> true | _ -> false;;
let is_ot = function {s = Outermost} -> true | _ -> false;;
let is_al = function {l = All} -> true | _ -> false;;
let is_cl = function {l = Constructor} -> true | _ -> false;;

let is_empty_dg = function
 | Complete -> false
 | Partial g -> Graph.is_empty g
;;

let is_empty = function
 | {p = Dp p} -> Trs.is_empty p.dps || is_empty_dg p.dg
 | {p = Standard p} -> Trs.is_empty p
 | {p = Cp p} | {p = Relative p} -> Trs.is_empty p.strict
 | {p = Confluence p} -> 
  Trs.is_empty p.orig || (Trs.is_empty p.cstrict && p.cds = [])
 | {p = EqStandard p} -> Trs.is_empty p.etrs
 | {p = EqDp p} -> Trs.is_empty p.edps || is_empty_dg p.edg
;;

let for_all f = function
 | {p = Dp p} -> f p.dps && f p.trs
 | {p = Standard p} -> f p
 | {p = Cp p} | {p = Relative p} -> f p.strict && f p.weak
;;

let for_allm f = function
 | {p = Dp p} -> M.ite (f p.dps) f (M.return <.> const false) p.trs
 | {p = Standard p} -> f p
 | {p = Cp p} | {p = Relative p} ->
   M.ite (f p.strict) f (M.return <.> const false) p.weak
;;

let exists f = not <.> for_all (not <.> f);;
let existsm f = M.lift not <.> for_allm (M.lift not <.> f);;

(* Compare Functions *)
let equal_dg g g' = match (g,g') with
 | Complete, Complete -> true
 | Partial g, Partial g' -> Graph.equal g g'
 | _, _ -> false
;;

let equal p q = match (p,q) with
 | {p = Dp p}, {p = Dp q} ->
  Trs.equal p.dps q.dps && Trs.equal p.trs q.trs && equal_dg p.dg q.dg
 | {p = Standard p}, {p = Standard q} -> Trs.equal p q
 | {p = Cp p}, {p = Cp q} | {p = Relative p}, {p = Relative q} ->
  Trs.equal p.strict q.strict && Trs.equal p.weak q.weak
 | {p = EqStandard p}, {p = EqStandard q} ->
  Trs.equal p.eqs q.eqs && Trs.equal p.etrs q.etrs
 | {p = EqDp p}, {p = EqDp q} ->
  Trs.equal p.eqssharp q.eqssharp && Trs.equal p.edps q.edps && 
  Trs.equal p.dpeqs q.dpeqs && Trs.equal p.edptrs q.edptrs &&
  Trs.equal p.srules q.srules && equal_dg p.edg q.edg
 | _, _ -> false
;;

(* Printers *)
(* standard printers *)
let fprintf_dg fmt = function
 | Complete -> F.fprintf fmt "@[graph: fully connected@]"
 | Partial g -> F.fprintf fmt "@[<1>graph:@\n%a@]" Graph.fprintf g
;;

let fprintf ?(g = false) fmt = function
 | {p = Dp p} ->
  F.fprintf fmt "@[@[<1>DPs:@\n%a@]@\n@[<1>TRS:@\n%a@]"
   Trs.fprintf p.dps Trs.fprintf p.trs;
  if g then F.fprintf fmt "@\n%a@]" fprintf_dg p.dg
  else F.fprintf fmt "@]"
 | {p = Standard p} -> Trs.fprintf fmt p
 | {p = EqStandard p} -> F.fprintf fmt "@[@[<1>Equations:@\n%a@]@\n@[<1>TRS:@\n%a@]"
   Trs.fprintf p.eqs Trs.fprintf p.etrs
 | {p = EqDp p} -> F.fprintf fmt "@[@[<1>Equations#:@\n%a@]@\n@[<1>DPs:@\n%a@]@\n@[<1>Equations:@\n%a@]@\n@[<1>TRS:@\n%a@]@\n@[<1>S:@\n%a@]"
   Trs.fprintf p.eqssharp Trs.fprintf p.edps
   Trs.fprintf p.dpeqs Trs.fprintf p.edptrs Trs.fprintf p.srules;
   if g then F.fprintf fmt "@\n%a@]" fprintf_dg p.edg else F.fprintf fmt "@]"
 | {p = Cp p} | {p = Relative p} ->
  F.fprintf fmt "strict:@\n%a@\nweak:@\n%a" Trs.fprintf p.strict
   Trs.fprintf p.weak
;;

let to_string ?(g = false) =
 F.flush_str_formatter <.> fprintf ~g:g F.str_formatter
;;

(* monadic printers *)
let fprintfm_dg fmt = function
 | Complete -> M.return (F.fprintf fmt "@[graph: fully connected@]")
 | Partial g ->
  F.fprintf fmt "@[<1>graph:@\n";
  (if Graph.size_edges g > 1000 then M.return (F.fprintf fmt "...")
  else Graph.fprintfm fmt g) >>= fun _ ->
  M.return (F.fprintf fmt "@]")
;;

let fprintfm ?(g = true) fmt = function
 | {p = Dp p} ->
  F.fprintf fmt "@[@[<1>DPs:@\n"; Trs.fprintfm fmt p.dps >>= fun _ ->
  F.fprintf fmt "@]@\n@[<1>TRS:@\n"; Trs.fprintfm fmt p.trs >>= fun _ ->
  if g then (
   F.fprintf fmt "@]@\n"; fprintfm_dg fmt p.dg >>= fun _ ->
   M.return (F.fprintf fmt "@]")
  ) else M.return (F.fprintf fmt "@]@]")
 | {p = Standard p} -> Trs.fprintfm fmt p
 | {p = Cp p} | {p = Relative p} ->
  F.fprintf fmt "@[@[<1>strict:@\n"; Trs.fprintfm fmt p.strict >>= fun _ ->
  F.fprintf fmt "@]@\n@[<1>weak:@\n"; Trs.fprintfm fmt p.weak >>= fun _ ->
  M.return (F.fprintf fmt "@]@]")
 | {p = Confluence p} ->
  F.fprintf fmt "@[@[<1>trs:@\n"; Trs.fprintfm fmt p.orig >>= fun _ ->
  F.fprintf fmt "@]@\n@[<1>strict:@\n"; Trs.fprintfm fmt p.cstrict >>= fun _ ->
  F.fprintf fmt "@]@\n@[<1>weak:@\n"; Trs.fprintfm fmt p.cweak >>= fun _ ->
  F.fprintf fmt "@]@\n@[<1>diagrams: (%d)" (List.length p.cds);  
   M.iter (fun d -> F.fprintf fmt "@\n"; Diagram.fprintfm fmt d) p.cds >>= fun _ ->
  F.fprintf fmt "@]@]";
  M.return ();
 | {p = EqStandard p} ->
  F.fprintf fmt "@[@[<1>Equations:@\n"; Trs.fprintfm fmt p.eqs >>= fun _ ->
  F.fprintf fmt "@]@\n@[<1>TRS:@\n"; Trs.fprintfm fmt p.etrs >>= fun _ ->
  M.return (F.fprintf fmt "@]@]")
 | {p = EqDp p} ->
  F.fprintf fmt "@[@[<1>Equations#:@\n"; Trs.fprintfm fmt p.eqssharp >>= fun _ ->
  F.fprintf fmt "@]@\n@[<1>DPs:@\n"; Trs.fprintfm fmt p.edps >>= fun _ ->
  F.fprintf fmt "@]@\n@[<1>Equations:@\n"; Trs.fprintfm fmt p.dpeqs >>= fun _ ->
  F.fprintf fmt "@]@\n@[<1>TRS:@\n"; Trs.fprintfm fmt p.edptrs >>= fun _ ->
  F.fprintf fmt "@]@\n@[<1>S:@\n"; Trs.fprintfm fmt p.srules >>= fun _ ->
  F.fprintf fmt "@]@]";
  M.return ();

;;

let to_stringm ?(g = false) p =
 fprintfm ~g:g F.str_formatter p >>= (M.return <.> F.flush_str_formatter)
;;

(* XML printers *)
let fprintfx_dg fmt = function
 | Complete -> M.return (F.fprintf fmt "@{<graph><fullyConnected/>}")
 | Partial g ->
  F.fprintf fmt "@{<graph>"; Graph.fprintfx fmt g >>= fun _ ->
  M.return (F.fprintf fmt "@}")
;;

let fprintfx ?(g = false) fmt = function
 | {p = Dp p} ->
  F.fprintf fmt "@{<dps>";
  Trs.fprintfx fmt p.dps >>= fun _ ->
  F.fprintf fmt "@}@{<trs>";
  Trs.fprintfx fmt p.trs >>= fun _ ->
  if g then (F.fprintf fmt "@}"; fprintfx_dg fmt p.dg)
  else M.return(F.fprintf fmt "@}")
 | {p = Standard p} ->
  F.fprintf fmt "@{<trs>";
  Trs.fprintfx fmt p >>= fun _ ->
  M.return (F.fprintf fmt "@}")
 | {p = Cp p} | {p = Relative p} ->
  F.fprintf fmt "@{<strict>";
  Trs.fprintfx fmt p.strict >>= fun _ ->
  F.fprintf fmt "@}@{<weak>";
  Trs.fprintfx fmt p.weak   >>= fun _ ->
  M.return(F.fprintf fmt "@}")
 | {p = Confluence p}  ->
  failwith "TODO: write xml printer"
;;

let to_stringx ?(g = false) p =
 fprintfx F.str_formatter p >>= (M.return <.> F.flush_str_formatter)
;;
