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

(*** INCLUDES *****************************************************************)
include Prelude.Signature;;

(*** MODULES ******************************************************************)
module F = Format;;

(*** FUNCTIONS ****************************************************************)
(* Constructors and Destructors *)
let update p update f s =
 let (f,l) = search_lab p f s in replace_lab f (update l) s
;;

(* Properties *)
let is_curry = is_lab Label.is_curry;;
let is_dp = is_lab Label.is_dp;;
let is_height = is_lab Label.is_height;;
let is_rlab = is_lab Label.is_rlab;;
let is_int = is_lab Label.is_int;;
let is_slab = is_lab Label.is_slab;;
let is_theory tl = is_lab (Label.is_theory tl);;
let is_some_theory = is_lab Label.is_some_theory;;

(* Search Functions *)
let get_curry f = Label.get_curry <.> pick_lab Label.is_curry f;;
let get_height f = Label.get_height <.> pick_lab Label.is_height f;;
let get_int f = Label.get_int <.> pick_lab Label.is_int f;;
let get_int_list f = Label.get_int_list <.> pick_lab Label.is_int f;;
let get_rlab f = Label.get_rlab <.> pick_lab Label.is_rlab f;;
let get_slab f = Label.get_slab <.> pick_lab Label.is_slab f;;
let get_theory f = Label.get_theory <.> pick_lab Label.is_some_theory f;;

(* Modifiers *)
let add_rlab f = update Label.is_rlab (Label.add_rlab f);;
let incr_curry = update Label.is_curry Label.incr_curry;;
let modify_curry f n = update Label.is_curry (const (Label.make_curry n)) f;;
let modify_height f n = update Label.is_height (const (Label.make_height n)) f;;
let modify_int f n = update Label.is_int (const (Label.make_int_list n)) f;;
let modify_rlab f fs = update Label.is_rlab (const (Label.make_rlab fs)) f;;
let modify_slab f n = update Label.is_slab (const (Label.make_slab n)) f;;
let modify_theory f th = 
 update Label.is_some_theory (const (Label.make_theory th)) f
;;
let set_curry ?(arity = ~-1) f = label ~arity:arity f <.> Label.make_curry;;
let set_dp = flip label Label.make_dp;;
let set_height f = label f <.> Label.make_height;;
let set_rlab f = label f <.> Label.make_rlab;;
let set_qlab f = label f <.> Label.make_qlab;;
let set_slab f = label f <.> Label.make_slab;;
let set_theory f = label f <.> Label.make_theory;;
let drop_curry f s = unlabel (drop_lab Label.is_curry f s) s;;
let drop_dp f s = unlabel (drop_lab Label.is_dp f s) s;;
let drop_height f s = unlabel (drop_lab Label.is_height f s) s;;
let drop_rlab f s = unlabel (drop_lab Label.is_rlab f s) s;;
let drop_slab f s = unlabel (drop_lab Label.is_slab f s) s;;
let drop_theory f s = unlabel (drop_lab Label.is_some_theory f s) s;;
let add_int ?(arity = ~-1) f i s = 
 let l = get_int_list f s in
 modify_int f l s;
 (f,s)
;;


let drop_int f s = 
 let li = get_int_list f s in
 modify_int f (List.tl li) s;
 f;;

(* Printers *)
let fprintfx_var s fmt x signature =
 let (n,s) = create_var_name x (set_signature signature s) in
 Format.fprintf fmt "%s" n; to_signature s
;;

let fprintfx_labs f g fmt ls s =
  List.foldl (flip (Label.fprintfx f g fmt)) s ls
;;

let rec fprintfx_fun s fmt f signature =
 let s = set_signature signature s in
 let (n,s) = create_fun_name f s and ls = labs f s in
 F.fprintf fmt "@{<name>%s@}" n;
 let f = fprintfx_fun s and g = fprintfx_var s in
 let signature = fprintfx_labs f g fmt ls (to_signature s) in
 signature
;;

let fprintfx_fun fmt f s =
 set_signature (fprintfx_fun s fmt f (to_signature s)) s
;;

let fprintfx_var fmt x s =
 set_signature (fprintfx_var s fmt x (to_signature s)) s
;;

let print_with_internal_buffer pf x s =
  let buff = Buffer.create 64 in
  let bfmt = F.formatter_of_buffer buff in
  F.pp_set_tags bfmt true;
  let s    = pf bfmt x s in
  F.fprintf bfmt "@?";
  let res  = Buffer.contents buff in
  Buffer.reset buff;
  (res,s)
;;

let to_stringx_fun f s = print_with_internal_buffer fprintfx_fun f s;;
let to_stringx_var x s = print_with_internal_buffer fprintfx_var x s;;
