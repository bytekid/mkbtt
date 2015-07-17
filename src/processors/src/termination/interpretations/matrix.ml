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

(*** MODULES ******************************************************************)
module type COEFFICIENT = sig
 type t
 val add : t -> t -> t
 val fprintf : Format.formatter -> t -> unit
 val fprintfx : Format.formatter -> t -> unit
 val is_one : t -> bool
 val is_zero : t -> bool
 val mul : t -> t -> t
 val one : t
 val to_string : t -> string
 val zero : t
end

module type MATRIX = sig
 type t
 type elt
 val add : t -> t -> t
 val columns : t -> int
 val column_vectors : t -> t list
 val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
 val forall : (elt -> bool) -> t -> bool
 val fprintf_intp : Format.formatter -> (string * int * t list * t)
  -> unit
 val fprintfx_intp : int -> Format.formatter -> (string * int * t list * t)
  -> unit
 val fprintf_term : Format.formatter -> (string * (t list * string list) * t)
  -> unit
 val fprintf_rule : Format.formatter -> (string * (t list * string list * t) *
     string * (t list * string list * t) * string) -> unit
 val get : int -> int -> t -> elt
 val set : int -> int -> t -> elt -> t
 val identity : int -> t
 val is_identity : t -> bool
 val is_zero : t -> bool
 val makeij : int -> int -> (int -> int -> elt) -> t
 val make : int -> int -> elt -> t
 val map2list : (elt -> elt -> 'a) -> t -> t -> 'a list
 val map2 : (elt -> elt -> elt) -> t -> t -> t
 val mul : t -> t -> t
 val of_list : int -> int -> elt list -> t
 val of_vector_list : t list -> t
 val one : int -> int -> t
 val rows : t -> int
 val scale : elt -> t -> t
 val fprintf : Format.formatter -> t -> unit
 val to_list : t -> elt list
 val to_string : t -> string
 val transpose : t -> t
 val unit_vector : int -> int -> t
 val vector_of_list : int -> (elt * int) list -> t
 val zero : int -> int -> t
end

module Make (Elt : COEFFICIENT) : (MATRIX with type elt = Elt.t) = struct
 (*** TYPES *******************************************************************)
 type elt = Elt.t;;
 type t = elt array array;;

 (*** FUNCTIONS ***************************************************************)
 let make i j e = Array.make_matrix i j e;;

 let makeij i j f =
  let m = make i j Elt.zero in
  for i = 0 to (i - 1) do
   for j = 0 to (j - 1) do
    m.(i).(j) <- f i j
   done
  done;
  m;;

 let zero i j = make i j Elt.zero;;

 let one i j = make i j Elt.one;;
 
 let rows m = Array.length m;;
 
 let columns m = Array.length m.(0);;

 let of_list m n ls = makeij m n (fun i j -> List.nth ls (i*n + j));;

 let get i j m = m.(i).(j);;

 let copy m0 =
  let r = rows m0 in
  let c = columns m0 in
   let res = zero r c in
   for i = 0 to (r - 1) do
    for j = 0 to (c - 1) do
      res.(i).(j) <- m0.(i).(j);
     done
   done;
   res
;;

let set i j m elt = let n = copy m in n.(i).(j) <- elt; n;;

 let identity d = 
  makeij d d (fun i j -> if i = j then Elt.one else Elt.zero);;

 let column_vector m j = 
  let r = rows m in
  let result = zero r 1 in
  if j >= (columns m) then failwith "column_vector: out of bounds";
  for i = 0 to (r - 1) do
   result.(i).(0) <- m.(i).(j)
  done;
  result;;

 let column_vectors m = List.gen (column_vector m) (columns m);;
 
 let unit_vector n j =
  let v = zero n 1 in
  v.(j).(0) <- Elt.one;
  v;;

 let vector_of_list n ls =
  let v = zero n 1 in
  List.iter (fun (a, i) -> v.(i).(0) <- a) ls;
  v;;

 let set_column j m c = 
  let r = rows m in
  if r <> rows c then failwith "Matrix.set_column";
  for i = 0 to (r - 1) do
   m.(i).(j) <- c.(i).(0)
  done;
  m;;

(* in case of trivial zero-solution only [vs] is empty *)
 let of_vector_list vs = 
  if List.length vs = 0 then failwith "of_vector_list: empty list";
  let t = zero (rows (List.hd vs)) (List.length vs) in
  List.fold_lefti set_column t vs;;
 
 let forall p m =
  let r = rows m in
  let c = columns m in
  let rec forall i j =
   if i = r then
    true
   else if j = c then
    forall (i + 1) 0
   else
    p (get i j m) && forall i (j + 1)
  in
  forall 0 0;;


 let row i m = List.gen (fun j -> m.(i).(j)) (Array.length m.(i));;
 
 let column j m = List.gen (fun i -> m.(i).(j)) (Array.length m);;

 let foldij f m a =
  let (r, c) = rows m, columns m in
  let rec fold f m a i j =
   if i = r then
    a
   else if j = c then
    fold f m a (i + 1) 0
   else
    f i j (get i j m) (fold f m a i (j + 1))
  in
  fold f m a 0 0;;

 let fold f m a = foldij (fun _ _ -> f) m a;;
 
 let is_zero m = forall Elt.is_zero m;;

 let is_identity m = 
  foldij (fun i j elt acc -> 
   (if i = j then Elt.is_one elt else Elt.is_zero elt) && acc)
   m
   (rows m = columns m);;

  let add m0 m1 =
   let r = rows m0 in
   let c = columns m0 in
   if c = columns m1 && r = rows m1 then
    let m = zero r c in
    for i = 0 to (r - 1) do
     for j = 0 to (c - 1) do
      m.(i).(j) <- Elt.add m0.(i).(j) m1.(i).(j)
     done
    done;
    m
   else
    failwith "add: dimensions do not fit";;
 
  let mul m0 m1 = 
   let d = columns m0 in
   if d = rows m1 then
    let r = rows m0 in
    let c = columns m1 in
    let m = zero r c in
    for i = 0 to (r - 1) do
     for j = 0 to (c - 1) do
      for k = 0 to (d - 1) do
       m.(i).(j) <- Elt.add m.(i).(j) (Elt.mul m0.(i).(k) m1.(k).(j))
       done
      done
    done;
    m
   else
    failwith "mul: dimensions do not fit";;

 let transpose m =
  let r = rows m in
  let c = columns m in
  let result = zero c r in
  for i = 0 to (r - 1) do
   for j = 0 to (c - 1) do
    result.(j).(i) <- m.(i).(j)
   done
  done;
  result;;

 let scale a m = 
  let r = rows m in
  let c = columns m in
  let res = zero r c in
  for i = 0 to (r - 1) do
   for j = 0 to (c - 1) do
    res.(i).(j) <- Elt.mul a m.(i).(j)
   done
  done;
  res;;

 let map2 f m0 m1 =
  let r = rows m0 in
  let c = columns m0 in
  if c = columns m1 && r = rows m1 then
   let res = zero r c in
   for i = 0 to (r - 1) do
    for j = 0 to (c - 1) do
      res.(i).(j) <- f m0.(i).(j) m1.(i).(j);
     done
   done;
   res
  else
   failwith "map2: dimensions do not fit";;

 let map2list f m0 m1 =
  let r = rows m0 in
  let c = columns m0 in
  if c = columns m1 && r = rows m1 then
   let is = List.range 0 r in
   let js = List.range 0 c in
   List.flat_map (fun i -> List.map (fun j -> f m0.(i).(j) m1.(i).(j)) js) is
  else
   failwith "map2list: dimensions do not fit";;

 let to_list m = map2list (fun a b -> a) m m;;

 let max_len_of_entry m =
  let max_len = ref 0 in
  for i = 0 to (rows m - 1) do
   for j = 0 to (columns m - 1) do
    let l = String.length (Elt.to_string m.(i).(j)) in
    if l > !max_len then max_len := l;
   done
  done;
  !max_len;;
 
 let align_entry e len = 
  let e = Elt.to_string e in
  let l = String.length e in
  let suffix = String.make (max (len - l) 0) ' ' in
  e ^ suffix;;

(*
 let to_string m =
  let r = rows m in
  let c = columns m in
  let max_len = max_len_of_entry m in
  let is = List.range 0 r in
  let js = List.range 0 c in
  List.join 
   (fun i -> 
    Format.sprintf 
     "[%s]" 
     (List.join (fun j -> align_entry m.(i).(j) max_len) " " js)
   ) 
   "\n" 
   is
 ;;
*)

(** s ... some text printed left
   p1 ... polynomial 1 
   op ... operator printed in middle
   p2 ... polynomial 2
   t ... some text printed right
*)
 let fprintf_complicated ppf (s,p1,op,p2,t) =
  let height = rows (Triple.thd p1) in
  let half = height / 2 in

  let fpf_delim ppf (arg,i,s) = 
   let delim =
    let d = Format.sprintf "%s%s" arg s in
    if i = half then d else String.make (String.length d) ' '
   in
   Format.fprintf ppf "%s" delim
  in

  let fpf_row i l ppf m =
   let c = columns m - 1 in
   for j = 0 to c do
    let d = if j < c then " " else "" in
    Format.fprintf ppf "%s%s" (align_entry (get i j m) l) d
   done
  in

  let fpf_const i ppf c =
   let l = max_len_of_entry c in
   (* Format.fprintf ppf "%s" (align_entry (get i 0 c) l) *)
   fpf_row i l ppf c
  in

  let fpf_row_all ppf (l,p,m,r) = 
   if is_identity m then () else Format.fprintf ppf "%s%a%s" l p m r
  in

  let (l,r) = 
   try
    let m = List.hd (Triple.fst p1) in
    if columns m = 1 then ("", "") else ("[", "]")
   with
    | Failure _ -> if height = 1 then ("", "") else ("[", "]")
  in

  let rec fpf_line ppf i const = function
   | [] -> 
    Format.fprintf ppf "%s%a%s" l (fpf_const i) const r
   | [(m, arg, len)] ->
    if is_zero const then
     Format.fprintf ppf "%a%a" fpf_row_all (l,fpf_row i len,m,r) fpf_delim
      (arg, i, "")
    else (
     Format.fprintf ppf "%a%a" fpf_row_all (l,fpf_row i len,m,r) fpf_delim
      (arg, i, " + ");
     fpf_line ppf i const [])
   | (m, arg, len) :: ms ->
    Format.fprintf ppf "%a%a" fpf_row_all (l,fpf_row i len,m,r) fpf_delim
     (arg, i, " + ");
    fpf_line ppf i const ms
  in

  let fpf_aux i t =
   if i = half then Format.fprintf ppf "%s" t
   else Format.fprintf ppf "%s" (String.make (String.length t) ' ')
  in
   
  let fpf_poly i (ms,xs,co) =
   let ms = List.combine ms xs in
   let ms = List.filter (fun (m, i) -> not (is_zero m)) ms in
   let ms = List.map (fun (m, arg) -> (m, arg, max_len_of_entry m)) ms in
   if ms = [] then Format.fprintf ppf "%s%a%s" l (fpf_const i) co r
   else fpf_line ppf i co ms
  in

  for i = 0 to height - 1 do 
   if i > 0 then Format.fprintf ppf "@\n";
   fpf_aux i s;
   fpf_poly i p1;
   fpf_aux i op;
   Option.fold (fpf_poly i) () p2;
   fpf_aux i t;
   done
 ;;

 let fprintf_term ppf (s,(ms,xs),const) =
  fprintf_complicated ppf (Format.sprintf "[%s] = " s,(ms,xs,const),"",None,"")
 ;;
 
 let fprintf_rule ppf (s,(ms,xs,const),op,(ms2,xs2,const2),t) =
  fprintf_complicated ppf (s,(ms,xs,const),op,Some(ms2,xs2,const2),t)
 ;;
 
 let fprintf_intp ppf (fs,a,ms,const) =
  let xs = List.gen (fun i -> Format.sprintf "x%d" i) a in
  let xxs = List.gen (fun i -> Format.sprintf "x%dx%d" (i/a) (i mod a)) a in
  let lp, rp = if xs = [] then ("", "") else ("(", ")") in
  let xlist = List.join id ", " xs in
  let xs = xs@xxs in
  let vars = List.take (List.length ms) xs in
  let s = Format.sprintf "[%s]%s%s%s = " fs lp xlist rp in
  fprintf_complicated ppf (s,(ms,vars,const),"",None,"")
 ;;

 let fprintf ppf m = 
 let p = ([],[],m) in
 Format.printf "@[<0>%a@]" fprintf_complicated
 ("",p,"",None,"");;

 let to_string m = 
  Format.fprintf Format.str_formatter "@[%a@]" fprintf m;
  Format.flush_str_formatter ()
 ;;


(*print polynomials*)
 let fprintfx_const fmt c = if rows c = 1 then (
  Format.fprintf fmt "@{<polynomial>@{<coefficient>%a@}@}" Elt.fprintfx (get 0 0 c)
 ) else (
  Format.fprintf fmt "<notSupported/>"
 );;

 let fprintfx_var a fmt i = 
  if a >= i then 
 Format.fprintf fmt "@{<polynomial>@{<variable>%i@}@}" i
 else
 let j = i - a in
 Format.fprintf fmt "@{<polynomial>@{<variable>%i@}@}\
                     @{<polynomial>@{<variable>%i@}@}"
 j j
 ;;

 let fprintfx_coeff a fmt i m = if rows m = 1 && columns m = 1 then (
  Format.fprintf fmt
    "@{<polynomial>@{<product>@{<polynomial>@{<coefficient>%a@}@}%a@}@}"
    Elt.fprintfx (get 0 0 m)
    (fprintfx_var a) (i+1)
 ) else (
  Format.fprintf fmt "<notSupported/>"
 );;

 let fprintfx_function fmt (a,ms,const) =
  Format.fprintf fmt "@{<polynomial>@{<sum>%a%a@}@}"
    (fun fmt -> List.iteri (fprintfx_coeff a fmt)) ms
    fprintfx_const const
 ;;

 let fprintfx_intp fmt (f,arity,ms,const) =
  Format.fprintf fmt "@{<interpret>%s@{<arity>%i@}%a@}"
   f
   arity
   fprintfx_function (arity,ms,const)
 ;;

(*print matrices*)
 let list_of_vec v = 
  let rec lov i acc = if i < 0 then acc else lov (i-1) (v.(i).(0)::acc) in
  lov (rows v - 1) []
 ;;

 let fprintfx_e fmt e = 
  Format.fprintf fmt "@{<coefficient>%a@}" Elt.fprintfx e;;

 let fprintfx_v fmt v =
  Format.fprintf fmt "@{<vector>%a@}" (List.fprintf fprintfx_e "") (list_of_vec v)
 ;;

 let fprintfx_m fmt m = 
  Format.fprintf fmt "@{<matrix>%a@}" (List.fprintf fprintfx_v "") (column_vectors m)
 ;;
 
 let fprintfx_const_m fmt c =
  Format.fprintf fmt "@{<polynomial>@{<coefficient>%a@}@}" fprintfx_m c
 ;;

 let fprintfx_coeff_m fmt i m = 
  Format.fprintf fmt
   "@{<polynomial>@{<product>@{<polynomial>@{<coefficient>%a@}@}@{<polynomial>@{<variable>%d@}@}@}@}"
   fprintfx_m m
   (i+1)
 ;;

 let fprintfx_function_m fmt (ms,const) =
  Format.fprintf fmt "@{<polynomial>@{<sum>%a%a@}@}"
    (fun fmt -> List.iteri (fprintfx_coeff_m fmt)) ms
    fprintfx_const_m const
 ;;

 let fprintfx_intp_m fmt (f,arity,ms,const) =
  Format.fprintf fmt "@{<interpret>%s@{<arity>%i@}%a@}"
   f
   arity
   fprintfx_function_m (ms,const)
 ;;

 let blow_up_m d m = 
 let r = zero d d in
  for i = 0 to (rows m - 1) do
   for j = 0 to (columns m - 1) do
    r.(i).(j) <- m.(i).(j)
   done
  done;
  r;;

 let blow_up ms const d =
  let cs = const::ms in
  let cs = List.map (blow_up_m d) cs in
  (List.tl cs, List.hd cs)
 ;;

 let fprintfx_intp dim fmt ((f,a,ms,const) as d) =
  if dim > 1 then
   let (ms,const) = blow_up ms const dim in fprintfx_intp_m fmt (f,a,ms,const)
  else fprintfx_intp fmt d
end

(*** TESTS ********************************************************************)
let test () =
 Format.printf "testing module Matrix ...\n";
 let module C = struct
  type t = int;;
  let zero = 0;;
  let one = 1;;
  let add = ( + );;
  let fprintf ppf i = Format.fprintf ppf "%s" (string_of_int i);;
  let fprintfx ppf i = Format.fprintf ppf "%s" (string_of_int i);;
  let mul = ( * );;
  let to_string = string_of_int;;
  let is_one = (=) 1;;
  let is_zero = (=) 0;;
 end
 in
 let module M = Make (C) in
 let m0 = M.identity 3 in
 Format.printf "m0 =\n%s\n" (M.to_string m0);
 let m1 = M.add m0 m0 in
 Format.printf "m1 = m0 + m0 =\n%s\n" (M.to_string m1);
 let m2 = M.mul m1 m1 in
 Format.printf "m2 = m1 * m1 =\n%s\n" (M.to_string m2);
 let m3 = M.makeij 3 3 ( + ) in
 Format.printf "m3 = \n%s\n" (M.to_string m3);
 let m4 = M.mul m3 m3 in
 Format.printf "m4 = m3 * m3 = \n%s\n" (M.to_string m4);
 let m5 = M.mul m4 m4 in
 Format.printf "m5 = m4 * m4 = \n%s\n" (M.to_string m5);
 let c0 = M.makeij 3 1 ( + ) in
 let m6 = M.zero 3 3 in
 let inter0 = ("f", 3, [m0; m6; m2], c0) in
 Format.printf "Interpretation\n";
 M.fprintf_intp Format.std_formatter inter0;
 Format.printf "@\n";
 let inter1 = ("c", 0, [], c0) in
 Format.printf "Interpretation\n";
 M.fprintf_intp Format.std_formatter inter1;
 Format.printf "@\n";
 let c1 = M.scale 3 c0 in
 Format.printf "Matrix c1: %s@\n" (M.to_string c1);

 (* tests for speedup *)
 let a = M.makeij 100 100 ( + ) in
 let t0 = Unix.gettimeofday () in
 let _ = M.mul a a in
 let t1 = Unix.gettimeofday () in
 Format.printf "Time for mul: %f\n" (t1 -. t0);
 let _ = M.mul a a in
 let t2 = Unix.gettimeofday () in
 Format.printf "Time for mul: %f\n" (t2 -. t1);
 ()
;;
