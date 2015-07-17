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

(*** INCLUDES *****************************************************************)
include Filename;;

(*** MODULE TYPES *************************************************************)
module type FILENAME = sig
 val current_dir_name : string
 val parent_dir_name : string
 val concat : string -> string -> string
 val is_relative : string -> bool
 val is_implicit : string -> bool
 val check_suffix : string -> string -> bool
 val chop_suffix : string -> string -> string
 val chop_extension : string -> string
 val basename : string -> string
 val dirname : string -> string
 val temp_file : string -> string -> string
 val open_temp_file : ?mode:open_flag list -> string -> string
  -> string * out_channel
 val temp_dir_name : string
 val quote : string -> string
end

(*** FUNCTIONS ****************************************************************)
let temp_file p s = temp_file p s;;
let open_temp_file ?(mode = [Open_text]) p s = open_temp_file ~mode:mode p s;;
let clean p = Str.global_replace (Str.regexp "//+") "/" p;;
let expand p = Str.replace_first (Str.regexp "~") (Sys.getenv "HOME") p;;
let extension p = 
 let r = String.rindex p '.' in
 String.sub p (r+1) (String.length p - r -1)
;;
