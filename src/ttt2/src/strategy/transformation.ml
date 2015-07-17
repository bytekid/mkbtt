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
open Transformation;;

(*** MODULES (part 2) *********************************************************)
module RLab = RootLabeling;;
module QLab = QuasiRootLabeling;;
module Ti   = TypeIntroduction;;

(*** FUNCTIONS ****************************************************************)
let (>>=) = M.(>>=);;
let finished p = (Proof.make p [Proof.finished],Status.terminating);;
let unfinished p = (Proof.make p [Proof.unfinished],Status.unfinished);;
let failed = (Proof.unfinished,Status.fail);;

let return s p proof status =
 let s = S.update_status status (S.set_problem p s) in
 let s = S.set_proof (Proof.append proof (S.get_proof s)) s in
 M.result [((),s)]
;;

(* HZ copied methods from module Termination
let apply solve get_problem make =
 M.get >>= fun s ->
 let r = solve (S.get_problem s) in
 let (proof,status) = option (unfinished <.> make) failed r in
 return s (Option.fold get_problem (S.get_problem s) r) proof status
;;

let applym solve get_problem make =
 M.get >>= fun s -> M.liftr (solve (S.get_problem s)) >>= fun r ->
 let (proof,status) = option (unfinished <.> make) failed r in
 return s (Option.fold get_problem (S.get_problem s) r) proof status
;;
*)

let apply solve get_problem make =
 M.get >>= fun s ->
 let r = solve (S.get_problem s) in
 let is_empty r = Problem.is_empty (get_problem r) in
 let extract p r = if is_empty r then finished p else unfinished p in
 let (proof,status) = option (fun r -> extract (make r) r) failed r in
 return s (Option.fold get_problem (S.get_problem s) r) proof status
;;

let applym solve get_problem make =
 M.get >>= fun s -> M.liftr (solve (S.get_problem s)) >>= fun r ->
 let is_empty r = Problem.is_empty (get_problem r) in
 let extract p r = if is_empty r then finished p else unfinished p in
 let (proof,status) = option (fun r -> extract (make r) r) failed r in
 return s (Option.fold get_problem (S.get_problem s) r) proof status
;;

(* complexity transformation *)
let cp fs _ = apply (Cp.solve fs) Cp.get_op (fun r -> P.P_cp r);;

(* linearization *)
let linear fs _ =
 applym (Linear.solve fs) Linear.get_op (fun r -> P.P_linear r)
;;

(* quasi root-labeling transformation *)
let qlab fs _ = applym (QLab.solve fs) QLab.get_op (fun r -> P.P_qlab r);;

(* reflect transformation *)
let ref fs _ =
 apply (Reflect.solve fs) Reflect.get_op (fun r -> P.P_ref r)
;;

(* reverse transformation *)
let rev fs _ =
 apply (Reverse.solve fs) Reverse.get_op (fun r -> P.P_rev r)
;;

(* root-labeling transformation *)
let rlab fs _ = applym (RLab.solve fs) RLab.get_op (fun r -> P.P_rlab r);;

(* relative termination transformation *)
let rt fs _ = apply (Rt.solve fs) Rt.get_op (fun r -> P.P_rt r);;

(* dpify transformation *)
let dpify fs _ = applym (Dpify.solve fs) Dpify.get_op (fun r -> P.P_dpify r);;

(* dup transformation *)
let dup fs _ = applym (Dup.solve fs) Dup.get_op (fun r -> P.P_dup r);;

(* star transformation *)
let star fs _ = applym (Star.solve fs) Star.get_op (fun r -> P.P_star r);;

(* split transformation *)
let split fs _ =
 M.get >>= fun s ->
 let r = Split.solve fs (S.get_problem s) in
 let (proof,status) = option (fun r -> unfinished (P.P_split r)) failed r in
 let s = S.update_status status s in
 let s = S.set_proof (Proof.append proof (S.get_proof s)) s in
 M.result (match Option.map Split.get_ops r with
  | None -> [((),s)]
  | Some ps -> List.map (Pair.make () <.> flip S.set_problem s) ps)
;;

(* standard termination transformation *)
let st fs _ = apply (St.solve fs) St.get_op (fun r -> P.P_st r);;

(* type introduction transformation *)
let ti fs _ =
 M.get >>= fun s ->
 M.liftr (Ti.solve fs (S.get_problem s)) >>= fun r ->
 let (proof,status) = option (fun r -> unfinished (P.P_ti r)) failed r in
 let s = S.update_status status s in
 let s = S.set_proof (Proof.append proof (S.get_proof s)) s in
 M.result (match Option.map Ti.get_ops r with
  | None -> [((),s)]
  | Some ps -> List.map (Pair.make () <.> flip S.set_problem s) ps)
;;

(* AC DP unlabelling processor *)
let udpac fs _ = applym (Udpac.solve fs) Udpac.get_op (fun r -> P.P_udpac r);;

(* uncurrying processor *)
let uncurry fs _ =
 applym (Uncurry.solve fs) Uncurry.get_op (fun r -> P.P_uncurry r)
;;

(* extended uncurrying processor *)
let uncurryx fs _ =
 applym (Uncurryx.solve fs) Uncurryx.get_op (fun r -> P.P_uncurryx r)
;;

(* usable rules processor *)
let ur fs _ = applym (Ur.solve fs) Ur.get_op (fun r -> P.P_ur r);;

(*** GLOBALS ******************************************************************)
let processors = [
 (Cp.code,cp,Cp.help);
 (Dpify.code,dpify,Dpify.help);
 (Dup.code,dup,Dup.help);
 (Linear.code,linear,Linear.help);
 (QLab.code,qlab,QLab.help);
 (Reflect.code,ref,Reflect.help);
 (Reverse.code,rev,Reverse.help);
 (RLab.code,rlab,RLab.help);
 (Rt.code,rt,Rt.help);
 (Split.code,split,Split.help);
 (St.code,st,St.help);
 (Star.code,star,Star.help);
 (Ti.code,ti,Ti.help);
 (Udpac.code,udpac,Udpac.help);
 (Uncurry.code,uncurry,Uncurry.help);
 (Uncurryx.code,uncurryx,Uncurryx.help);
 (Ur.code,ur,Ur.help);
];;
