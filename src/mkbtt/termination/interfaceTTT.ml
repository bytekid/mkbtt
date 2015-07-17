(* Copyright 2010 Sarah Winkler
 * GNU Lesser General Public License
 *
 * This file is part of MKBtt.
 * 
 * MKBtt is free software: you can redistribute it and/or modify it under
 * the terms of the GNU Lesser General Public License as published by the
 * Free Software Foundation, either version 3 of the License, or (at your
 * option) any later version.
 * 
 * MKBtt is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public
 * License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public
 * License along with MKBtt. If not, see <http://www.gnu.org/licenses/>.
 *)

(** Termination checks with TTT2
@author Sarah Winkler
@since  2008/02/19
*)

(** Calls TTT2 for termination checks *)

(*** OPENS ********************************************************************)
open Util;;
(*** SUBMODULES **********************************************************)
module SLexer = Ttt2.Strategy.Lexer;;
module SParser = Ttt2.Strategy.Parser;;
module TTT2 = U.MyTTT2;;
module Status = Ttt2.Strategy.Status;;
module Problem = U.MyProblem;;
module PProblem = Processors.Problem;;
module Trs = U.Trs;;
module RM = U.Monad;;
module St = Statistics;;

module Monad = World.Monad;;

(*** OPENS (2) ***********************************************************)
open World;;


(*** FUNCTIONS ***********************************************************)
let (>>=) = Monad.(>>=)
let (>>) = Monad.(>>)
let return = Monad.return
let liftm = Monad.liftm

let log2 s = World.log 2 s

(* updates statistics for one result *)
let update_stats (a, d) tmo =
 St.inc_n_termination_calls >>
 if a then St.inc_n_termination_yes
 else if d=tmo then St.inc_n_termination_timeouts
 else return ()
;;

let update_stats1 (a, d) =
 World.get_options >>= fun o ->
 let tmo = Completion.ttimeout o in
 update_stats (a,d) tmo >>
 St.add_t_termination d
;;

(* updates statistics for two results *)
let update_stats2 a b d =
 World.get_options >>= fun o ->
 let tmo = Completion.ttimeout o in
 update_stats (a,d) tmo >>
 update_stats (b,d) tmo >>
 St.add_t_termination d
;;

let run_ttt2 problem syntax =
 let (>>=) = RM.(>>=) in
 let run_ttt2 = 
  RM.get >>= fun s -> 
  let (_, _, s) = TTT2.run problem s syntax in
  RM.return (s = Status.terminating) 
 in
 liftm run_ttt2
;;

let ttt2_proof problem syntax =
 let (>>=) = RM.(>>=) in
 let run_ttt2 =
  RM.get >>= fun s ->
  let (p, _, s) = TTT2.run problem s syntax in
  RM.return (s, p)
 in
 liftm run_ttt2
;;

let problem trs = Problem.make_sp PProblem.All PProblem.Full trs

let query run =
 World.get_options >>= fun options ->
 let tmo = Completion.ttimeout options in
 let strategy = Completion.termination_strategy options in
 let strategy_timed = strategy^"["^(string_of_float tmo)^"]" in
 (* Format.printf "Termination strategy %s \n%!" strategy; *)
 let s = Lexing.from_string strategy_timed in
 let syntax = SParser.strategy SLexer.token s in
 run syntax
;;

let check trs =
 World.get_options >>= fun o ->
 World.M.Trsx.to_stringm trs >>= fun trsm ->
 log2 ("Check termination of "^trsm^"\n") >>
 let t_start = Unix.gettimeofday () in
 query (run_ttt2 (problem trs)) >>= fun r ->
 log2 ("result: "^(if r then "yes" else "no")) >>
 let t = Unix.gettimeofday () -. t_start in
 update_stats1 (r, t) >>= fun _ -> 
 (*Format.printf "Status is %s\n%!" (if r then "true" else "false");*)
 return r
;;

let prove_with trs s =
 World.get_options >>= fun o ->
 let run = (ttt2_proof (problem trs)) in
 let strategy_timed = s^"[30]" in
 let s = Lexing.from_string strategy_timed in
 let syntax = SParser.strategy SLexer.token s in
 run syntax
;;
(* ---------------- PARALLEL TERMINATION CHECKING ---------------- *)

let ask_ttt_twice run0 run1 = 
 let ask r = let (_, _, s) = r () in s = Status.terminating in 
 let run0 () = ask run0 in
 let run1 () = ask run1 in
 match Process.run_parallel [run0; run1] () with
 | [Some a0; Some a1] -> (a0, a1)
 | _ -> raise (Completion.GiveUp "No two answers")
;;

let run_ttt2_twice problem0 problem1 syntax =
 let (>>=) = RM.(>>=) in
 let run_ttt2 =
  RM.get >>= fun s ->
  let run0 () = TTT2.run problem0 s syntax in
  let run1 () = TTT2.run problem1 s syntax in
  RM.return (ask_ttt_twice run0 run1)
 in
 liftm run_ttt2
;;

(* Runs in parallel the two termination checks run0 and run1. If one
   of these runs results in an error, false is returned by
   default. At most 2 processes are running at the same time. The
   status_report function ist set to ignore. *)
let check_parallel trs0 trs1 =
 let t_start = Unix.gettimeofday () in
 World.get_options >>= fun o ->
 World.M.Trsx.to_stringm trs0 >>= fun trs0m ->
 World.M.Trsx.to_stringm trs1 >>= fun trs1m ->
 log2 ("Check termination of "^trs0m^"\nand\n"^trs1m^" \n") >>
 let p0, p1 = problem trs0, problem trs1 in
 query (run_ttt2_twice p0 p1) >>= fun (r0, r1) ->
 log2 ("result: "^(if r0 then "yes" else "no")^", "^(if r1 then "yes" else "no")) >>
 let d = (Unix.gettimeofday ()) -. t_start in
 update_stats2 r0 r1 d >>
 return (r0, r1)
;;




