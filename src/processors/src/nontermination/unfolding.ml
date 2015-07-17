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
module F   = Format;;
module LL  = LazyList;;
module Pos = Position;;
module Sub = Substitution;;
module Sig = Signature;;

module St = struct
  type t = {
    sign : Sig.t;
    size : int;
  };;

  let make size sign = { sign = sign; size = size; };;
  let get_sig s = s.sign;;
  let set_sig state s = { state with sign = s };;
  let get_size s = s.size;;
end

module S = Stream.Make(St);;

(*** TYPES ********************************************************************)
type flags = {
  help : bool ref;
  bwd : bool ref;
  fwd : bool ref;
  size : int ref;
  vpos : bool ref;
};;

type t = Problem.t * Loop.t;;

(*** GLOBALS ******************************************************************)
let code = "unfold";;
let name = "Unfolding Processor";;
let comment = "This processor searches for loops by using unfoldings.";;
let keywords = ["unfolding";"loop";"nontermination"];;

let flags = {
  help = ref false;
  bwd = ref false;
  fwd = ref false;
  size = ref max_int;
  vpos = ref false;
};;

let spec =
  let spec = [
    ("-bwd",Arg.Set flags.bwd,"Use backward unfoldings to find loops.");
    ("-fwd",Arg.Set flags.fwd,"Use forward unfoldings to find loops.");
    ("--help",Arg.Set flags.help,"Prints information about flags.");
    ("-help",Arg.Set flags.help,"Prints information about flags.");
    ("-h",Arg.Set flags.help,"Prints information about flags.");
    ("-size",Arg.Set_int flags.size,"Maximal size of terms considered.");
    ("-vpos",Arg.Set flags.vpos,"Rewrite also at variable positions.")]
  in
  Arg.alignx 80 spec
;;

let help = (comment,keywords,List.map Triple.drop_snd spec);;

(*** EXCEPTIONS ***************************************************************)
exception Too_large;;

(*** FUNCTIONS ****************************************************************)
let init _ = 
  flags.help := false;
  flags.bwd := false;
  flags.fwd := false;
  flags.size := max_int;
  flags.vpos := false;
;;

(* Destructors *)
let make = Pair.make;;
let get_loop = snd;;
let get_ip = fst;;

(* Processor *)
(* compute a renaming for a given term (and handle the signature explicitely *)
let renaming s t =
 let rec renaming (sub,s) = function
  | Term.Var x -> if Sub.mem x sub then (sub,s) else (
    let(y,s) = Sig.fresh_var s in
    (Sub.add x (Term.Var y) sub,s)
  )
  | Term.Fun(f,ts) -> List.foldl renaming (sub,s) ts
 in
 renaming (Sub.empty,s) t
;;

(* rename a rule *)
let rename s rule =
  let (ren,s) = renaming s (Rule.lhs rule) in
  (Rule.map (Sub.apply_term ren) rule,s)
;;

let unify p st rule state =
  let (s,t) = Rule.to_terms st in
  let size = St.get_size state in
  if Term.size s > size || Term.size t > size then 
    (F.printf "too large@\n%!";
    raise Too_large) else (
    let ((l',r'),sign) = rename (St.get_sig state) rule in
    let mgu = Elogic.unify (Term.subterm p t) l' in
    (Rule.of_terms (Sub.apply_term mgu s)
                   (Sub.apply_term mgu (Term.replace p r' t)),
     sign)
);;

(******************************************************************************)
(**) (* check whether [rule] is applicable at [p] in [t].
(**)    if this is the case [rs] is extended appropriately. *)
(**) let contractum p (st,rs) rule = (fun state -> try
(**)   let (rule',sign) = unify p st rule state in
(**)   Some((rule',rs@[(rule,p)]),St.set_sig state sign)
(**) with Elogic.Not_unifiable | Too_large -> None
(**) );;
(**) 
(**) let reducts_pos trs strs p = S.of_values (contractum p strs) trs;;
(**) 
(**) let reducts trs pos strs = S.of_streams (reducts_pos trs strs) pos;;
(**) 
(**) let forward trs pos rules = S.flat_map (fun((st,_) as strs) ->
(**)   reducts trs (pos(Rule.rhs st)) strs
(**) ) rules;;
(******************************************************************************)

(******************************************************************************)
(**) let extractum p (st,rs) rule = (fun state -> try
(**)   let (rule',sign) = unify p (Rule.invert st) (Rule.invert rule) state in
(**)   Some((Rule.invert rule',(rule,p)::rs),St.set_sig state sign)
(**) with Elogic.Not_unifiable | Too_large -> None
(**) );;
(**) 
(**) let ancestors_pos trs strs p = S.of_values (extractum p strs) trs;;
(**) 
(**) let ancestors trs pos strs = S.of_streams (ancestors_pos trs strs) pos;;
(**) 
(**) let backward trs pos rules = S.flat_map (fun((st,_) as strs) ->
(**)   ancestors trs (pos(Rule.lhs st)) strs
(**) ) rules;;
(******************************************************************************)

let interleave trs pos rules =
  S.merge (forward trs pos rules) (backward trs pos rules);;

let initial trs = S.of_values (fun rule state ->
  Some((rule,[(rule,Pos.root)]),state)
) trs
;;

let check(rule,rs) =
  let (l,r) = Rule.to_terms rule in
  S.of_values (fun u state -> try
    ignore(Elogic.match_term u l);
    Some((l,rs),state)
  with Elogic.Not_matchable -> try
    let mgu = Elogic.unify l u in
    Some((Sub.apply_term mgu l,rs),state)
  with Elogic.Not_unifiable -> None
  ) (Term.subterms r)
;;

let loops state start trs step pos =
  let start  = Trs.to_list start in
  let trs    = Trs.to_list trs in
  let ini    = initial start in
  let stream = S.concat(S.iterate (fun x s ->
    (S.sieve (fun r0 r1 ->
      not(Rule.subsumes (fst r1) (fst r0))) (step trs pos x),s)
  ) ini) in
  let stream = S.flat_map check stream in
  S.eval stream state
;;

(* wrap into state monad *)
module M = Rewritingx.Monad;;
let (>>=) = M.(>>=);;
let (>>)  = M.(>>);;

(* select relative loops for relative problems *)
let select p ((t,rs),_) = 
 if Problem.is_rp p then 
  let rs = Trs.of_list (List.map fst rs) in
  not (Trs.is_empty (Trs.intersect rs (Problem.get_strict p)))
 else
  true
;;

let solve fs p =
  let configurate s = F.printf "%s@\n%!" s; flags.help := true in
  (try init (); Arg.parsex code spec fs with Arg.Bad s -> configurate s);
  if !(flags.help) then (Arg.usage spec ("Options for "^code^":"); exit 0);
  if Problem.is_sp p || Problem.is_dp p || Problem.is_rp p then
    let (s,w) = Problem.get_sw p in
    let sw = Trs.union s w in
    let pos = if !(flags.vpos) then Term.pos else Term.funs_pos in
    let step = match (!(flags.fwd),!(flags.bwd)) with
      | (true,false)  -> forward
      | (false,true)  -> backward
      | (true,true)   -> interleave
      | (false,false) -> if not(Trs.is_duplicating sw) then forward
        else if not(Trs.is_duplicating(Trs.invert sw)) then backward
                                                       else interleave
    in
    let size = !(flags.size) in
    M.get >>= fun sign ->
    try
     let s = if Problem.is_dp p then s else sw in
     let st = St.make size sign in
     let loops = loops st s sw step pos in
     (* let ((t,rs),st) = LL.nth 0 loops in *)
     let ((t,rs),st) = LL.find (select p) loops in
     let loop = LL.nth 0 (Loop.of_term_rules_positions t rs) in
     M.set(St.get_sig st) >> M.return(Some(make p loop))
    with Failure e -> M.fail e
  else M.return(None)
;;

(* Compare Functions *)
let equal p q = Problem.equal (get_ip p) (get_ip q);;

(* Printers *)
let fprintf fs fmt p =
  F.fprintf fmt "@[<1>%s:@\n" name; Loop.fprintf fmt (get_loop p) >>= fun _ ->
  F.fprintf fmt "@\n"; List.hd fs fmt >>= fun _ -> M.return (F.fprintf fmt "@]")
;;

let fprintfx fs fmt p =
  Loop.fprintfx fmt (get_loop p) (get_ip p) >>= fun _ -> List.hd fs fmt
;;
