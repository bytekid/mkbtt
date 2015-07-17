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
open Logic.Operators;;

(*** MODULES ******************************************************************)
module C = Complexity;;
module F = Format;;
module Fun = Function;;
module Number = Logic.Number;;
module P = Problem;;
module Var = Variable;;

(*** TYPES ********************************************************************)
type flags = {
  help : bool ref;
  p : bool ref;
  strict : bool ref;
  weak : bool ref;
 };;
type t = P.t * (Rule.t * int) list * P.t;;
let make p rl q = (p,rl,q);;

(*** GLOBALS ******************************************************************)
let code = "rule_labeling";;
let name = "Rule Labeling Processor";;
let keywords = ["rule labeling";"decreasing diagrams";"confluence"];;
let comment = "Implements rule labeling for decreasing diagrams.";;
let flags = {
  help = ref false;
  p    = ref false;
  strict = ref false;
  weak = ref false;
};;

let spec =
 let spec = [
  ("--help",Arg.Set flags.help,"Prints information about flags.");
  ("-help",Arg.Set flags.help,"Prints information about flags.");
  ("-h",Arg.Set flags.help,"Prints information about flags.");
  ("-p",Arg.Set flags.p,"Print encoding in SMT format and fail.");
  ("-strict",Arg.Set flags.strict,"strict decreasingness.");
  ("-weak",Arg.Set flags.weak,"weak decreasingness.");
  ] in
 Arg.alignx 80 spec
;;

let help = (comment,keywords,List.map Triple.drop_snd spec);;

let (>>=) = Monad.(>>=);;
(* Destructors *)
let get_ip = Triple.fst;;
let get_op = Triple.thd;;
let get_rl = Triple.snd;;

(* Complexity Bounds *)
let complexity c _ = C.mul c C.constant;;

(* Compare Functions *)
let equal p q =
 P.equal (get_ip p) (get_ip q) && P.equal (get_op p) (get_op q)
;;

(* Printers *)
let fprintf_rl fmt rl =
 Monad.iter (fun (rule,lab) -> 
  Format.fprintf fmt "@\n"; 
  Rule.fprintfm fmt rule >>= fun _ ->
  Format.fprintf fmt ": %d" lab;
  Monad.return ();) 
  rl
;;

let fprintf fs fmt p =
 F.fprintf fmt "@[<1>%s:@\n" name; P.fprintfm fmt (get_op p) >>= fun _ ->
 F.fprintf fmt "@\n@[<1>rule labeling:";
 fprintf_rl fmt (get_rl p) >>= fun _ ->
 F.fprintf fmt "@]@\n"; List.hd fs fmt >>= fun _ -> Monad.return (F.fprintf fmt "@]")
;;

let fprintfx fs fmt p =
 F.fprintf fmt "@{<ruleLabeling>"; P.fprintfx fmt (get_op p) >>= fun _ ->
 List.hd fs fmt >>= fun _ -> Monad.return (F.fprintf fmt "@}")
;;

(*** MODULES (part 2) *********************************************************)
type context = {
 arith              : Logic.arith;
 rtbl               : (Rule.t,Logic.a) Hashtbl.t;
};;

module Statex = struct type t = context end;;
module M = Util.Monad.Transformer.State (Statex) (Logic.Monad);;
open M;;

(*** FUNCTIONS ****************************************************************)
let (>>=) = M.(>>=);;
let (>>) = M.(>>);;
let init _ = 
 flags.help   := false;
 flags.strict := false;
 flags.weak   := false;
;;

let cache_m tbl f k = 
 if Hashtbl.mem tbl k then return (Hashtbl.find tbl k)
 else (f k >>= fun v -> (Hashtbl.add tbl k v; return v))
;;

(*Utilities*)
let big_add xs = List.foldl (<+>) Logic.zero xs;;
(* functions lifted from Logic into M *)
let fresh_arith = get >>= fun s -> liftm (Logic.fresh_arith s.arith);;
let fresh_arith_spec arith = liftm (Logic.fresh_arith arith);;
let fresh_bool = get >>= fun s -> liftm Logic.fresh_bool;;
let ($&$) = lift2 (<&>);;
let ($|$) = lift2 (<|>);;
let ($->$) = lift2 (<->>);;
let ($<$) = lift2 (<<>);;
let ($>$) = lift2 (<>>);;
let ($>=$) = lift2 (<>=>);;
let ($<=$) = lift2 (<<=>);;
let ($=$) = lift2 (<=>);;
let ($+$) = lift2 (<<>);;
let eval_a a ass = a >>= fun a -> liftm (Logic.eval_a a ass);;
let eval_p p ass = p >>= fun p -> liftm (Logic.eval_p p ass);;
let map_op op f ls = sequence (List.map f ls) >>= (return <.> op);;
let mapi_op op f ls = sequence (List.mapi f ls) >>= (return <.> op);;
let gen_op op f n = sequence (List.gen f n) >>= (return <.> op);;
let map_and f = map_op Logic.big_and f;;
let map_add f = map_op big_add f;;
let mapi_and f = mapi_op Logic.big_and f;;
let gen_and f = gen_op Logic.big_and f;;
let gen_add f = gen_op big_add f;;
let map_or f = map_op Logic.big_or f;;
let mapi_or f = mapi_op Logic.big_or f;;
let gen_or f = gen_op Logic.big_or f;;

(* Encodings *)
let var_rule rule = 
 get >>= fun c ->
 cache_m c.rtbl (fun _ -> fresh_arith) rule

let var_step s = var_rule (Rewrite.step_get_rule s);;

let eq alpha s = alpha $=$ var_step s;;
let gt alpha s = alpha $>$ var_step s;;
let gt2 alpha beta s = gt alpha s $|$ gt beta s;;

let join_strict alpha beta j = 
 let (ls,rs) = j in
 map_and (gt2 alpha beta) (ls@rs)
;;

let seq_std_i alpha beta ss i _ = 
 let (ts,us) = List.split_at i ss in
 map_and (gt alpha) ts $&$
 (if us = [] then M.return Logic.top else
  (eq beta (List.hd us) $|$ gt2 alpha beta (List.hd us)) $&$
  map_and (gt2 alpha beta) (List.tl us)
 )
;;

let seq_std alpha beta ss = 
 if ss = [] then M.return Logic.top else
 mapi_or (seq_std_i alpha beta ss) ss
;;


let join_std alpha beta j =
 let (ls,rs) = j in
 seq_std alpha beta ls $&$
 seq_std beta alpha rs
;;

let join_weak d = 
 failwith "TODO";
;;


let encode_d e d = 
 let (l,r) = Diagram.peak d in
 let js = Diagram.joins d in
 map_or (e (var_step l) (var_step r)) js
;;


let encode cds = 
 let e = 
  if !(flags.strict) then join_strict
  else if !(flags.weak) then join_weak 
  else join_std 
 in
 map_and (encode_d e) cds;
;;

let context p = 
 {
 arith = Logic.nat (List.length (Trs.to_list (P.get_trs p)));
 rtbl = Hashtbl.create 512;
 }
;;

let decode ass p =
 if !(flags.strict) then M.return (P.set_cds [] p) 
 else if !(flags.weak) then failwith "TODO: implement"
 else M.return (P.set_cds [] p)
;;

let decode_rule ass rule = 
 eval_a (var_rule rule) ass >>= fun lab ->
 M.return (rule,Logic.Number.to_int lab)
;;

let decode_rl ass p = 
 let trs = P.get_trs p in
 M.map (decode_rule ass) (Trs.to_list trs)
;;
 
let solve p =
 if P.is_crp p then 
 let c = context p in
 Monad.return (Logic.run ~obits:(-1) (
  M.run c (encode (P.get_cds p) >>= fun phi ->
  if !(flags.p) then (
   Format.fprintf Format.std_formatter "@[%a@]@\n" 
    (fun ppt -> Logic.fprintf_smt ppt) phi;
   return None
  ) else
  M.liftm (Logic.solve ~solver:Logic.MiniSat phi) >>= function
   | None -> 
    M.return None
   | Some ass ->
    decode ass p >>= fun p' -> 
    decode_rl ass p >>= fun rl ->
    M.return (Some (make p rl p'))
  )))
 else Monad.return None
;;

let solve fs p = 
 let configure s = F.printf "%s@\n%!" s; flags.help := true in
 (try init (); Arg.parsex code spec fs with Arg.Bad s -> configure s);
 if !(flags.help) then (Arg.usage spec ("Options for "^code^":"); exit 0);
 if !(flags.strict) && !(flags.weak) then 
  (Arg.usage spec (code^": -strict and -weak are not allowed"));
 solve p 
;;

