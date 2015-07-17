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
open Prelude;;
open Util;;

(*** MODULES ******************************************************************)
module Fun = Function;;
module Var = Variable;;

module Make (L : LABEL) = struct
 (*** MODULES *****************************************************************)
 module Lab = struct
  type t = Fun.t * L.t;;
  
  let make f l = (f,l);;
  let get_fs = fst;;
  let get_lab = snd;;
 
  let compare s t =
   let c = compare (get_lab s) (get_lab t) in
   if c = 0 then compare (get_fs s) (get_fs t) else c
  ;;
 
  let copy t = make (get_fs t) (L.copy (get_lab t));;
  let hash t = Hashtbl.hash (get_fs t, L.hash (get_lab t));;
 
  let fprintf fmt s =
   Format.fprintf fmt "@[f=%a, l=%a@]" Fun.fprintf (get_fs s)
    L.fprintf (get_lab s)
  ;;
 end
 
 module A = Index.Make (Fun) (Int);;
 module F = L.F;;
 module I = Index.Isomorphic (Fun) (Lab);;
 module V = L.V;;
 
 (*** TYPES *******************************************************************)
 type label = L.t;;
 type signature = L.signature;;
 type t = {fun_sig : F.t; fun_ari : A.t; fun_lab : I.t; var_sig : V.t};;
 
 (*** FUNCTIONS ***************************************************************)
 (* Search Functions *)
 let find_fun n s = F.find_key n s.fun_sig;;
 let find_var n s = V.find_key n s.var_sig;;
 let find_var_name x s = V.find_elt x s.var_sig;;
 let find_lab f s = Lab.get_lab (I.find_elt f s.fun_lab);;
 let unlabel f s = Lab.get_fs (I.find_elt f s.fun_lab);;
 
 let rec find_ari f s =
  try A.find f s.fun_ari with Not_found -> find_ari (unlabel f s) s
 ;;

 let labs f s =
  let rec labs ls f i =
   try let l = I.find_elt f i in labs (Lab.get_lab l :: ls) (Lab.get_fs l) i
   with Not_found -> ls
  in
  labs [] f s.fun_lab
 ;;

 let rec search_lab p f s =
  let l = find_lab f s in if p l then (f,l) else search_lab p (unlabel f s) s
 ;;

 let pick_lab p f = snd <.> search_lab p f;;
 let drop_lab p f = fst <.> search_lab p f;;
 let rec origin f s = try origin (unlabel f s) s with Not_found -> f;;
 let find_fun_name f s = F.find_elt (origin f s) s.fun_sig;;

 (* Scan Functions *)
 let mem_ari f = catch Not_found (const true <.> find_ari f) false;;
 let mem_fun f = catch Not_found (const true <.> find_fun_name f) false;;
 let mem_fun_name n s = F.mem_elt n s.fun_sig;;
 let mem_var x s = V.mem_key x s.var_sig;;
 let mem_var_name n s = V.mem_elt n s.var_sig;;
 let is_ari n f = catch Not_found ((=) n <.> find_ari f) false;;
 let is_defined_fun f s = mem_fun f s && mem_ari f s;;
 let is_defined_var = mem_var;;
 let is_lab p f = catch Not_found (const true <.> pick_lab p f) false;;
 let is_labeled f s = I.mem_key f s.fun_lab;;
 let is_unlabeled f = not <.> is_labeled f;;

 (* Constructors and Destructors *)
 let update_ari update f a s = {s with fun_ari = update f a s.fun_ari};;
 let update_fun update f n s = {s with fun_sig = update f n s.fun_sig};;
 let update_var update x n s = {s with var_sig = update x n s.var_sig};;
 let add_ari = update_ari A.add;;
 let var_names s = V.elements s.var_sig;;

 let add_fun_name f n s =
  if is_unlabeled f s then update_fun F.add f n s else s
 ;;

 let add_fun f a n = add_ari f a <.> add_fun_name f n;;
 let add_var = update_var V.add;;
 let replace_ari = update_ari A.replace;;

 let replace_fun_name f n s =
  if is_unlabeled f s then update_fun F.replace f n s else s
 ;;

 let replace_fun f a n = replace_ari f a <.> replace_fun_name f n;;
 let replace_var = update_var V.replace;;

 let empty n = {
  fun_sig = F.empty n;
  fun_ari = A.empty;
  fun_lab = I.empty n;
  var_sig = V.empty n;
 };;

 (* Fresh Symbols *)
 let fresh_fun s = let (f,fs) = F.fresh s.fun_sig in (f,{s with fun_sig = fs});;
 
 let create_fun a n s =
  try
   let f = find_fun n s in
   if find_ari f s = a then (f,s) else failwith "incorrect arity"
  with Not_found -> let (f,s) = fresh_fun s in (f,add_fun f a n s)
 ;;
 
 let fresh_var s =
  let (x,var_sig) = V.fresh s.var_sig in (x,{s with var_sig = var_sig})
 ;;
 
 let create_var n s =
  try (find_var n s,s)
  with Not_found -> let (x,s) = fresh_var s in (x,add_var x n s)
 ;;

 let register to_int mem add p x s =
  let rec register i =
   let n = Format.sprintf "%s%i" p i in
   if mem n s then register (i + 1) else (n,add x n s)
  in
  register (to_int x)
 ;;

 let create_var_name x s =
  try (find_var_name x s,s)
  with Not_found -> register Var.to_int mem_var_name add_var "x" x s
 ;;

 let create_fun_name f s =
  let f = origin f s in
  try (find_fun_name f s,s)
  with Not_found -> register Fun.to_int mem_fun_name add_fun_name "f" f s
 ;;

 (* Constructors and Destructors *)
 let update_lab update f l s = {s with fun_lab = update f l s.fun_lab};;

 let label ?(arity = ~-1) f l s =
  try (I.find_key (Lab.make f l) s.fun_lab,s) with Not_found ->
  let (g,s) = fresh_fun s in
  let s = if arity = ~-1 then s else add_ari g arity s in
  (g,update_lab I.add g (Lab.make f l) s)
 ;;

 let replace_lab f l s =
  try update_lab I.replace f (Lab.make (unlabel f s) l) s
  with Not_found -> failwith "unlabeled function symbol"
 ;;

 (* Miscellaneous *)
 let copy s = {
  fun_sig = F.copy s.fun_sig;
  fun_ari = A.copy s.fun_ari;
  fun_lab = I.copy s.fun_lab;
  var_sig = V.copy s.var_sig
 };;

 let set_signature (fs,vs) s = {s with fun_sig = fs; var_sig = vs};;
 let to_signature s = (s.fun_sig,s.var_sig);;

 (* Printers *)
 let fprintf fmt s =
  Format.fprintf fmt
   "@[Functions:@[<1>@\n%a@]@\nArities:@[<1>@\n%a@]@\nLabels:@[<1>@\n%a@]@]\
    @\nVariables:@[<1>@\n%a@]"
   F.fprintf s.fun_sig A.fprintf s.fun_ari I.fprintf s.fun_lab
   V.fprintf s.var_sig
 ;;

 let to_string = Format.flush_str_formatter <.> fprintf Format.str_formatter;;

 let fprintf_var s fmt x signature =
  let (n,s) = create_var_name x (set_signature signature s) in
  Format.fprintf fmt "@[%s@]" n; to_signature s
 ;;

 let fprintf_labs f g fmt ls s =
  let f = L.fprintfs f g fmt in
  let rec fprintf_labs s = function
   | []    -> s
   | [l]   -> f l s
   | l::ls -> let s = f l s in Format.fprintf fmt ","; fprintf_labs s ls
  in
  Format.fprintf fmt "@["; let s = fprintf_labs s ls in
  Format.fprintf fmt "@]"; s
 ;;

 let rec fprintf_fun s fmt f signature =
  let s = set_signature signature s in
  let (n,s) = create_fun_name f s and ls = labs f s in
  let n = String.xml_entities_decode n in
  let m = List.length ls and signature = to_signature s in
  Format.fprintf fmt "@[%s" n; if m > 1 then Format.fprintf fmt "{";
  let f = fprintf_fun s and g = fprintf_var s in
  let signature = fprintf_labs f g fmt ls signature in
  if m > 1 then Format.fprintf fmt "}"; Format.fprintf fmt "@]"; signature
 ;;

 let fprintf_fun fmt f s =
  set_signature (fprintf_fun s fmt f (to_signature s)) s
 ;;

 let fprintf_var fmt x s =
  set_signature (fprintf_var s fmt x (to_signature s)) s
 ;;

 let to_string_fun f s =
  let s = fprintf_fun Format.str_formatter f s in
  (Format.flush_str_formatter (),s)
 ;;
 
 let to_string_var x s =
  let s = fprintf_var Format.str_formatter x s in
  (Format.flush_str_formatter (),s)
 ;;
end
