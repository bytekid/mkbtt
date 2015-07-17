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

(** Critical pair criteria
@author Sarah Winkler
@since  2009/11/06
*)

(** Provides functions to switch between critical pair criteria *)

(*** OPENS (1) ***********************************************************)
open Util;;

(*** SUBMODULES **********************************************************)
module C = Completion;;
module St = Statistics;;
module W  = World;;
module Monad = W.Monad;;
module CP = CompletionProcessx;;

(*** OPENS (2) ***********************************************************)
open Monad;;

(*** FUNCTIONS ***********************************************************)
let dont_filter _ _ pcset = return pcset

(* filters all processes out of pcset for which the critical pair given
   by r1\sigma <- l1\sigma -> l1\sigma[r2\sigma]_p would be redundant,
   case distinction on criterion in options
  overlap contains instantiated rule 1, position and rule 2.
  If the system is a SRS, both BCP and PCP can only yield results if
   a) the substitution is not minimal, 
   b) not a CP, or
   c) or the used rule is not reduced
  Not entirely sure bout this, but experiments confirm.
*)
let filter_nonredundant overlap (u,v) pcset =
 let tstart = Unix.gettimeofday () in
 W.get_options >>= fun o ->
 let filter_nonredundant =
  match (C.cpc o) with
(*   | C.Connected (*-> Connected.filter_nonredundant *) *)
   | C.SReducibility -> SReduce.filter_nonredundant
   | C.Prime -> Prime.filter_nonredundant
   | C.Mixed -> Mixed.filter_nonredundant 
   | _ -> dont_filter
 in
 filter_nonredundant overlap (u,v) pcset >>= fun result ->
 let red = CP.diff pcset result in (* processes for which CP is redundant *)
 St.add_cp_for_processes red >>
 let diff = (Unix.gettimeofday ()) -. tstart in
 St.add_t_cpc diff >>
 return result
;;
