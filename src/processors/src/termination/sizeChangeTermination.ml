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
module C = Complexity;;
module F = Format;;
module M = Monad;;
module P = Problem;;
module Std = Pervasives;;


module G = struct
(*** EXCEPTIONS ***************************************************************)
exception SCP_not_applicable;;

(*** TYPES ********************************************************************)
(* Two arguments relate either strictly (true) or weakly (false) *)
(* (rule1,rule2,[(1,true,2);(2,fale,3)])
  means that there is a chain from the lhs of rule1
  to the rhs of rule2 such that there is a strict decrease
  from argument 1 to argument 2 and a weak decrease
  from argument 2 to argument 3.
  A decrease is measured w.r.t. the subterm relation.
*)
type t = (Rule.t * Rule.t * (int*bool*int)list);;

(*** FUNCTIONS ****************************************************************)
let (>>=) = M.(>>=);;

let edges s t = 
  let ss = List.mapi Pair.make (Term.args s) in
  let ts = List.mapi Pair.make (Term.args t) in
  if ss = [] || ts = [] then raise SCP_not_applicable else (
    List.foldl (fun es ((i,si),(j,tj)) ->
      if Term.is_subterm tj si then (i,not(Term.equal si tj),j)::es
                               else es
    ) [] (List.product ss ts)
  )
;;

let make rule =
  (rule,rule,let (lhs,rhs) = Rule.to_terms rule in edges lhs rhs)
;;

let compare (s,t,es) (u,v,fs) =
  let i = Rule.compare s u in
  if i = 0 then (
    let j = Rule.compare t v in
    if j = 0 then (
      if List.equal es fs then 0
                          else Std.compare es fs
    ) else j
  ) else i
;;

let equal g h = compare g h = 0;;

(* check if idempotent graphs allow for size-change termination *)
let okay comp ((_,_,es) as g) = match comp g g with
  | None   -> true
  | Some h ->
    if equal g h then List.exists (fun(i,b,j) -> b && i = j) es
                 else true
;;

let initial comp dps = Trs.fold (fun rule gs ->
  let g = make rule in
  if okay comp g then g::gs
                 else raise SCP_not_applicable
) [] dps
;;

(********************************************************)
(**) let merge ss ts = List.foldl (fun es (i,b,j) ->
(**)   List.foldl (fun es (k,c,l) ->
(**)     if j = k then (i,b||c,l)::es else es) es ts
(**) ) [] ss;;
(**) 
(**) (* Composition of size-change graphs w.r.t. an
(**) edge function (i.e., a function checking whether
(**) two graphs are possible due to some approximation
(**) of the DG. *)
(**) let comp conn (source,rule1,es1)
(**)               (rule2,target,es2) =
(**)   if conn(rule1,rule2) then (
(**)     Some(source,target,merge es1 es2)
(**)   ) else None
(**) ;;
(********************************************************)

(* Printers *)
let fprintfm_edges fmt es =
  let es = List.sort (fun (i,_,_) (j,_,_) -> Std.compare i j) es in
  M.iteri (fun n (i,b,j) -> M.return(
    if n > 0 then F.fprintf fmt "@\n";
    F.fprintf fmt "%2i %-2s %2i" i (if b then ">" else ">=") j)) es
;;

let fprintf_edges fmt es =
  let es = List.sort (fun (i,_,_) (j,_,_) -> Std.compare i j) es in
  List.iteri (fun n (i,b,j) -> (
    if n > 0 then F.fprintf fmt "@\n";
    F.fprintf fmt "%2i %-2s %2i" i (if b then ">" else ">=") j)) es
;;

let fprintfm_initial fmt (rule,_,es) =
  F.fprintf fmt "@[<1>The DP: "; Rule.fprintfm fmt rule     >>= fun _ ->
  F.fprintf fmt " has the edges:@\n"; fprintfm_edges fmt es >>= fun _ ->
  M.return(F.fprintf fmt "@]")
;;

let fprintf fmt (s,t,es) =
  F.fprintf fmt "start: %a\nend: %a\nedges:\n%a\n%!"
    Rule.fprintf s
    Rule.fprintf t
    fprintf_edges es
;;

let fprintfx_edges fmt es = M.iter (fun(i,b,j) ->
  M.return(F.fprintf fmt "@{<edge>@{<position>%i@}@{<strict>%B@}@{<position>%i@}@}" (i+1) b (j+1))
) es;;

let fprintfx_initial fmt (rule,_,es) =
  F.fprintf fmt "@{<sizeChangeGraph>";
  Rule.fprintfx fmt rule >>= fun _ ->
  fprintfx_edges fmt es  >>= fun _ ->
  M.return(F.fprintf fmt "@}")
;;

(* *)

(* compute the transitive closure of a given set of size-change
graphs. *)
let trancl comp bs =
  let rec merge n old = function
    | b::bs -> (match comp n b with
      | None    -> merge n old bs
      | Some nb -> if okay comp nb then (
        if List.mem ~c:compare nb old then merge n old bs
                                        else nb :: merge n old bs
      ) else raise SCP_not_applicable
    )
    | []    -> []
  in
  let rec trancl olds = function
    | n::news -> trancl (n::olds) (merge n (n::news@olds) bs @ news)
    | []      -> olds
  in
  trancl [] bs
;;

(* solve method *)
let check conn dps = try
  let comp = comp conn in
  let bs   = initial comp dps in
  let _    = trancl comp bs in
  Some bs
with SCP_not_applicable -> None
;;
end

(*** TYPES ********************************************************************)
type flags = {help : bool ref};;
type t = P.t * G.t list * P.t;;

(*** GLOBALS ******************************************************************)
let code = "sct";;
let name = "Size-Change Termination Processor";;
let comment = "Applies the size-change termination processor to a DP problem.";;
let keywords = [
  "size-change"; "size change"; "termination";
  "size-change principle"; "size change principle";
  "size-change termination"; "size change termination";
];;
let flags = {help = ref false};;

let spec =
 let spec = [
  ("--help",Arg.Set flags.help,"Prints information about flags.");
  ("-help",Arg.Set flags.help,"Prints information about flags.");
  ("-h",Arg.Set flags.help,"Prints information about flags.");]
 in
 Arg.alignx 80 spec
;;

let help = (comment,keywords,List.map Triple.drop_snd spec);;

(*** FUNCTIONS ****************************************************************)
let (>>=) = M.(>>=);;
let init _ = flags.help := false;;

(* Destructors *)
let get_ip = Triple.fst;;
let get_op = Triple.thd;;
let get_graphs = Triple.snd;;

(* Processor *)
let solve fs p =
  let configurate s = F.printf "%s@\n%!" s; flags.help := true in
  (try init (); Arg.parsex code spec fs with Arg.Bad s -> configurate s);
  if !(flags.help) then (Arg.usage spec ("Options for "^code^":"); exit 0);
  if P.is_dp p then (
    let mem = match P.get_dg p with
      | P.Complete   -> const true
      | P.Partial dg -> flip Graph.mem_edge dg
    in
    let dps = P.get_dps p in
    match G.check mem dps with
      | None    -> None
      | Some bs -> Some(p,bs,P.set_dps Trs.empty p)
  ) else None
;;

(* Complexity Bounds *)
let complexity c _ = C.mul c C.other;;

(* Compare Functions *)
let equal p q = P.equal (get_ip p) (get_ip q);;

(* Printers *)
let fprintfm_initials fmt = M.iteri (fun i ->
  if i > 0 then F.fprintf fmt "@\n";
  G.fprintfm_initial fmt)
;;

let fprintf fs fmt p =
  F.fprintf fmt "@[<1>%s:@\n" name; P.fprintfm fmt (get_op p) >>= fun _ ->
  F.fprintf fmt "@\n"; fprintfm_initials fmt (get_graphs p)   >>= fun _ ->
  F.fprintf fmt "@\n"; List.hd fs fmt                         >>= fun _ ->
  M.return(F.fprintf fmt "@]")
;;

let fprintfx_initials fmt = M.iter (G.fprintfx_initial fmt);; 

let fprintfx fs fmt p =
  F.fprintf fmt "@{<sizeChangeCriterion><subtermCriterion/>";
  fprintfx_initials fmt (get_graphs p) >>= fun _ ->
  M.return(F.fprintf fmt "@}")
;;
