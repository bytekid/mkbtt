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

(* Writing a wrapper for a processor:
 * There are two reasons why you have to write a wrapper for your processor.
 * On the on hand it must be guaranteed that each processor has the same
 * signature because otherwise they cannot be combined by the strategy. On
 * the other hand it is necessary to define the status of your processor and
 * its proof objects.
 * Writing a wrapper for your processor is quite easy. If the method [solve]
 * of your processor has a monadic return type you have to lift your processor
 * to the monad [M] by using the function [M.liftr]. Otherwise you can call
 * the method [solve] directly. After that change the current state according
 * to your processor proof object. Because each processor takes a single
 * problem as input and returns a list of new problems, the returned data must
 * have type [('a * S.t) list]. To lift results of this type to the monad [M]
 * use the function [M.result]. Finally you have to set the status of the new
 * termination problem. That means you have to specify if it is terminating,
 * nontermianting or open. If your processor failed you have to indicate that
 * by setting the status to fail. Note that you can automatically generate a
 * wrapper for your processor by using the function [apply] or [applym] if
 * your processor has a monadic return type.
 *
 * Register a new processor:
 * After you have written a wrapper for your processor, you have to register
 * it by editing the list [processor] below. Each entry has to be a tripple
 * consisting of the shortcut of the processor, the wrapper, and the help
 * messages. Important: To increase the readability of the wrapper file, the
 * name of the newly added wrapper should be identical to the shortcut you
 * have chosen for your method and registered by editing the list [processor].
 *)

(*** MODULES ******************************************************************)
module M = Monad;;
module P = Processor;;
module R = Processors.Rewritingx.Monad;;
module S = State;;

(*** OPENS ********************************************************************)
open Util;;
open Processors;;
open Rewritingx;;
open Nontermination;;

(*** FUNCTIONS ****************************************************************)
let (>>=) = M.(>>=);;
let finished p = (Proof.make p [Proof.finished],Status.nonterminating);;
let unfinished p = (Proof.make p [Proof.unfinished],Status.unfinished);;
let failed = (Proof.unfinished,Status.fail);;

let return s proof status =
 let s = S.update_status status s in
 let s = S.set_proof (Proof.append proof (S.get_proof s)) s in
 M.result [((),s)]
;;

let apply solve make =
 M.get >>= fun s ->
 let r = solve (S.get_problem s) in
 let (proof,status) = option (finished <.> make) failed r in
 return s proof status
;;

let applym solve make =
 M.get >>= fun s -> M.liftr (solve (S.get_problem s)) >>= fun r ->
 let (proof,status) = option (finished <.> make) failed r in
 return s proof status
;;

(* fresh variable processor *)
let var fs _ = apply (Variables.solve fs) (fun r -> P.P_var r);;

(* containment processor *)
let con fs _ = apply (Contained.solve fs) (fun r -> P.P_con r);;

(* SAT loop processor *)
let loop fs _ = applym (LoopSat.solve fs) (fun r -> P.P_loop r);;

(* unfolding processor *)
let unfold fs _ = applym (Unfolding.solve fs) (fun r -> P.P_unfold r);;

(*** GLOBALS ******************************************************************)
let processors = [
 (Contained.code,con,Contained.help);
 (LoopSat.code,loop,LoopSat.help);
 (Unfolding.code,unfold,Unfolding.help);
 (Variables.code,var,Variables.help);
];;
