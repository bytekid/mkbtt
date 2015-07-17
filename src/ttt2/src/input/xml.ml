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
open Processors;;
open Rewritingx;;
open Parser.Xml;;

(*** MODULES ******************************************************************)
module P = Problem;;

(*** TYPES ********************************************************************)
type language = All | Constructor;;
type strategy = Full | Innermost | Outermost;;

(*** FUNCTIONS ****************************************************************)
let rules = "rules" <?> node(
  Trs.xml                              >>= fun trs ->
  option("relrules" <?> node(Trs.xml)) >>= fun rel ->
  return(trs,rel)
);;

let strategy =
  "strategy" <?> string >>= function
    | "FULL" -> return Full
    | "INNERMOST" -> return Innermost
    | "OUTERMOST" -> return Outermost 
    | s -> fail("strategy "^s^" not supported")
;;

(*
let startterm =
  option("startterm" <?> string >>= function
    | "full" -> return All
    | "constructor-based" -> return Constructor
    | s -> fail("startterm "^s^" not supported"))
;;
*)

(*
let startterm = "startterm" <?> node (
  leaf >>= function
    | ("full", [], None) -> return (Some All)
    | ("constructor-based", [], None) -> return (Some Constructor)
    | _ -> return None
);;
*)

(*
let startterm = "startterm" <?> node ( choice [
  ("full" <?> node empty >>= fun _ -> return (Some All));
  ("constructor-based" <?> node empty >>= fun _ -> return (Some Constructor));
  fail "startterm not supported"
]);;
*)

let startterm = option("startterm" <?> node ( choice [
  ("full" <?> node empty >>= fun _ -> return All);
  ("constructor-based" <?> node empty >>= fun _ -> return Constructor);
  fail "startterm not supported"
]));;
  
let status = option("status" <?> (<*>));;
let meta = option("metainformation" <?> (<*>));;
let theory = option("theory" <?> string);;

let funcsym = "funcsym" <?> node(
  "name"  <?> string >>= fun _ ->
  "arity" <?> int    >>= fun _ ->
  theory 
);;

let signature =
  "signature" <?> node(many funcsym) (*>>= fun ts ->
  if List.exists Option.is_some ts
    then fail("theory annotations are not supported")
    else return()*)
;;

let trs = "trs" <?> node(
  rules                             >>= fun r ->
  signature                         >>
  option("comment" <?> (<*>))       >>
  option("conditiontype" <?> (<*>)) >>= function
    | Some _ -> fail "<conditiontype> not supported"
    | None   -> return r
);;

let problem = "problem" <?> node(
  trs       >>= fun(s,w) ->
  strategy  >>= fun strategy ->
  startterm >>= fun startterm ->
  status    >>
  meta      >>
  let language = function All -> P.All | Constructor -> P.Constructor in
  let l = Option.fold language P.All startterm in
  let strategy = match strategy with
    | Full -> P.Full
    | Innermost -> P.Innermost
    | Outermost -> P.Outermost
  in
  return (Option.fold (P.make_rp l strategy s) (P.make_sp l strategy s) w)
);;

let of_channel chin =
  let module XP = Parsec.Xml.MakeParser(Parsec.StringParser) in
  match XP.parse(Parsec.StringInput.of_channel chin) with
    | Left e     -> Pervasives.failwith(Parsec.Error.to_string e)
    | Right(_,x) ->
      let m t = get_state >>= (return <.> Pair.make t) in
      let m = problem >>= m in
      match run m (Signature.empty 100) x with
        | Left e -> Pervasives.failwith e
        | Right x -> x
;;
