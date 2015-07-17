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

(*** MODULES ******************************************************************)
module H = Hashtbl;;
module P = Pair;;

module Set = struct
 type 'a t = Empty | Node of 'a t * 'a * 'a t * int;;
 
 let empty = Empty
 let height = function Empty -> 0 | Node (_,_,_,h) -> h;;
 
 let create l v r =
  let hl = height l and hr = height r in
  Node (l,v,r,(if hl >= hr then hl + 1 else hr + 1))
 ;;
 
 let bal l v r =
  let hl = height l and hr = height r in
  if hl > hr + 2 then match l with
   | Empty -> failwith "bal"
   | Node (ll,lv,lr,_) ->
    if height ll >= height lr then create ll lv (create lr v r)
    else match lr with
     | Empty -> failwith "bal"
     | Node (lrl,lrv,lrr,_)->
      create (create ll lv lrl) lrv (create lrr v r)
  else if hr > hl + 2 then match r with
   | Empty -> failwith "bal"
   | Node (rl,rv,rr,_) ->
    if height rr >= height rl then create (create l v rl) rv rr
    else match rl with
     | Empty -> failwith "bal"
     | Node (rll,rlv,rlr,_) ->
      create (create l v rll) rlv (create rlr rv rr)
  else Node (l,v,r,(if hl >= hr then hl + 1 else hr + 1))
 ;;

 let rec add ?(c = compare) x = function
  | Empty -> Node (Empty,x,Empty,1)
  | Node (l,v,r,_) as t ->
   let n = c x v in
   if n = 0 then t
   else if n < 0 then bal (add ~c:c x l) v r
   else bal l v (add ~c:c x r)
 ;;
 
 let rec mem ?(c = compare) x = function
  | Empty -> false
  | Node (l,v,r,_) ->
   let n = c x v in n = 0 || mem ~c:c x (if n < 0 then l else r)
 ;;
end

(*** INCLUDES *****************************************************************)
include List;;

(*** MODULE TYPES *************************************************************)
module type LIST = sig
 val append : 'a list -> 'a list -> 'a list
 val assq : 'a -> ('a * 'b) list -> 'b
 val combine : 'a list -> 'b list -> ('a * 'b) list
 val concat : 'a list list -> 'a list
 val exists : ('a -> bool) -> 'a list -> bool
 val exists2 : ('a -> 'b -> bool) -> 'a list -> 'b list -> bool
 val fast_sort : ('a -> 'a -> int) -> 'a list -> 'a list
 val filter : ('a -> bool) -> 'a list -> 'a list
 val find : ('a -> bool) -> 'a list -> 'a
 val find_all : ('a -> bool) -> 'a list -> 'a list
 val flatten : 'a list list -> 'a list
 val fold_left : ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a
 val fold_left2 : ('a -> 'b -> 'c -> 'a) -> 'a -> 'b list -> 'c list -> 'a
 val fold_right : ('a -> 'b -> 'b) -> 'a list -> 'b -> 'b
 val fold_right2 : ('a -> 'b -> 'c -> 'c) -> 'a list -> 'b list -> 'c -> 'c
 val for_all : ('a -> bool) -> 'a list -> bool
 val for_all2 : ('a -> 'b -> bool) -> 'a list -> 'b list -> bool
 val hd : 'a list -> 'a
 val iter : ('a -> unit) -> 'a list -> unit
 val iter2 : ('a -> 'b -> unit) -> 'a list -> 'b list -> unit
 val length : 'a list -> int
 val map : ('a -> 'b) -> 'a list -> 'b list
 val map2 : ('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list
 val mem_assq : 'a -> ('a * 'b) list -> bool
 val memq : 'a -> 'a list -> bool
 val merge : ('a -> 'a -> int) -> 'a list -> 'a list -> 'a list
 val nth : 'a list -> int -> 'a
 val partition : ('a -> bool) -> 'a list -> 'a list * 'a list
 val remove_assq : 'a -> ('a * 'b) list -> ('a * 'b) list
 val rev : 'a list -> 'a list
 val rev_append : 'a list -> 'a list -> 'a list
 val rev_map : ('a -> 'b) -> 'a list -> 'b list
 val rev_map2 : ('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list
 val sort : ('a -> 'a -> int) -> 'a list -> 'a list
 val split : ('a * 'b) list -> 'a list * 'b list
 val stable_sort : ('a -> 'a -> int) -> 'a list -> 'a list
 val tl : 'a list -> 'a list
end

(*** FUNCTIONS ****************************************************************)
let cons x xs = x :: xs;;

(* Iterators *)
let foldl = fold_left;;
let foldl1 f = (fun xs -> foldl f (hd xs) (tl xs)) <?> "empty list";;
let foldl2 = fold_left2;;
let foldli f d = snd <.> foldl (fun (i, r) x -> (i + 1, f i r x)) (0, d);;
let foldr f d xs = fold_right f xs d;;
let foldr1 f = (fun xs -> foldr f (hd xs) (tl xs)) <?> "empty list";;
let foldr2 f d xs ys = fold_right2 f xs ys d;;

let foldri f d xs =
 snd (foldr (fun x (i,r) -> (i-1,f i x r)) (length xs - 1,d) xs)
;;

let fold_left1 f = foldl1 f <?> "empty list";;
let fold_lefti = foldli;;
let fold_right1 = foldr1;;
let fold_right1 f = foldr1 f <?> "empty list";;
let fold_righti f = flip (foldri f);;
let flat_mapi f = rev <.> foldli (fun i xs x -> rev_append (f i x) xs) [];;
let flat_map f = flat_mapi (const f);;
let map_tl f = rev <.> rev_map f;;
let iteri f = foldli (drop f) ();;
let rev_mapi f = foldli (fun i xs x -> f i x :: xs) [];;
let mapi f = rev <.> rev_mapi f;;

let map_option f =
 rev <.> foldl (fun xs x -> Option.fold (flip cons xs) xs (f x)) []
;;

let map_range f n m =
 let rec map_range f n m xs =
  if n > m then xs else map_range f n (m - 1) (f m :: xs)
 in
 map_range f n (m - 1) []
;;

let gen f n = map_range f 0 n;;

(* Sublists *)
let rev_spani p xs =
 let rec spani i acc = function
  | [] -> (acc, [])
  | x :: xs as ys ->
   if p i x then spani (i + 1) (x :: acc) xs else (acc, ys)
 in
 spani 0 [] xs
;;

let spani p xs = let (hd,tl) = rev_spani p xs in (rev hd, tl);;
let rev_span p = rev_spani (const p);;
let span p = spani (const p);;
let rev_split_at n = rev_spani (drop ((>) n));;
let split_at n = spani (drop ((>) n));;
let rev_split_last n xs = rev_split_at (length xs - n) xs;;
let split_last n xs = split_at (length xs - n) xs;;
let drop n = snd <.> split_at n;;
let drop_while p = snd <.> spani (const p);;
let take n = fst <.> split_at n;;
let take_while p = fst <.> spani (const p);;

let rec group ?(c = compare) xs =
 let f (x,xs,ys) y = if c x y = 0 then (x,y::xs,ys) else (y,[y],xs::ys) in
 if xs = [] then []
 else let (_,xs,ys) = foldl f (hd xs,[],[]) xs in rev (xs :: ys)
;;

(* Constructors and Destructors *)
let insert p e xs = let (hd,tl) = rev_split_at p xs in rev_append hd (e::tl);;
let replace ?(c = compare) x y = map_tl (fun z -> if c x z = 0 then y else z);;
let rev_concat xs = foldl (flip rev_append) [] xs;;
let singleton x = [x];;
let remove_all ?(c = compare) x = filter (fun y -> c x y <> 0);;

let collapse ?(n = -1) xs = 
 let chunk_size = if n < 1 then 1 else (length xs + n - 1)/n in
 let rec chunks = function
  | [] -> []
  | xs ->
  let (chunk,rest) = split_at chunk_size xs in
  concat chunk :: chunks rest
 in  
 if chunk_size < 1 then [] else chunks xs
;;

let remove ?(c = compare) x xs =
 let rec remove acc = function
  | [] -> acc
  | y :: xs -> if c x y = 0 then rev_append xs acc else remove (y :: acc) xs
 in
 rev (remove [] xs)
;;

let rec remove_equal ?(c = compare) xs ys = match (xs, ys) with
 | x :: us, y :: vs -> if c x y = 0 then remove_equal ~c:c us vs else (xs, ys)
 | _, _ -> (xs, ys)
;;

(* Scan Functions *)
let for_alli p =
 let rec for_alli i = function
  | [] -> true
  | x :: xs -> if p i x then for_alli (i + 1) xs else false
 in
 for_alli 0
;;

let existsi p = not <.> for_alli (fun i x -> not (p i x));;
let mem ?(c = compare) x = exists (fun y -> c x y = 0);;

(* Search Functions *)
let filteri p =
 let f i x xs = if p i x then x :: xs else xs in
 rev <.> snd <.> foldl (fun (i, xs) x -> (i + 1, f i x xs)) (0, [])
;;

let findi p xs =
 let rec findi i = function
  | [] -> raise Not_found
  | x :: xs -> if p i x then x else findi (i + 1) xs
 in
 findi 0 xs
;;

let position p xs =
 let rec position i = function
  | [] -> raise Not_found
  | x :: xs -> if p x then i else position (i + 1) xs
 in
 position 0 xs
;;

(* Zip Functions *)
let rev_zip_with f xs ys =
 let rec rev_zip_with acc = function
 | (x :: xs, y :: ys) -> rev_zip_with (f x y :: acc) (xs, ys)
 | _ -> acc
 in
 rev_zip_with [] (xs, ys)
;;

let zip_with f xs = rev <.> rev_zip_with f xs;;
let rev_zip xs = rev_zip_with P.make xs;;
let zip xs = rev <.> rev_zip xs;;

let rev_unzip_with f xs =
 foldl (fun (xs,ys) x -> let (x,y) = f x in (x::xs,y::ys)) ([],[]) xs
;;

let unzip_with f xs = rev_unzip_with f (rev xs);;
let rev_unzip xs = rev_unzip_with id xs;;
let unzip xs = unzip_with id xs;;

(* Assoctiation Lists *)
let assoc ?(c = compare) x = snd <.> find (fun (y,_) -> c x y = 0);;
let mem_assoc ?(c = compare) x = exists (fun (y,_) -> c x y = 0);;

let rec remove_assoc ?(c = compare) x = function
 | [] -> []
 | (y, z) :: xs -> if c x y = 0 then xs else (y, z) :: remove_assoc ~c:c x xs
;;

(* Lists as Sets *)
let union ?(c = compare) xs ys =
 let (xs,ys) = foldl (fun (xs,ys) x -> (x::xs,remove_all x ys)) ([],ys) xs in
 rev (rev_append ys xs)
;;

let flat_times f d xs =
 let rec flat_times f i acc = function
  | [] -> if Int.even i then rev acc else acc
  | xs :: ys ->
   let xs = if Int.even i then rev xs else xs in
   let combine f ys = foldl (fun ys x -> f x :: ys) ys acc in
   flat_times f (i + 1) (foldl (fun ys x -> combine (flip f x) ys) [] xs) ys
 in
 flat_times f 0 [d] xs
;;

let flat_product f xs ys =
 rev_concat (rev_map (fun x -> rev_map (f x) ys) xs)
;;

let rev_times xs = flat_times (flip cons) [] xs;;
let times xs = map_tl rev (rev_times xs);;
let product xs = flat_product P.make xs;;
let square xs = product xs xs;;
let diff ?(c = compare) xs ys = filter (fun x -> not (mem ~c:c x ys)) xs;;
let is_subset ?(c = compare) xs ys = for_all (flip (mem ~c:c) ys) xs;;
let is_supset ?(c = compare) = flip (is_subset ~c:c);;
let equal ?(c = compare) xs ys = is_subset ~c:c xs ys && is_subset ~c:c ys xs;;
let intersect ?(c = compare) xs ys = filter (flip (mem ~c:c) ys) xs;;

let is_proper_subset ?(c = compare) xs ys =
 is_subset ~c:c xs ys && not (is_subset ~c:c ys xs)
;;

let unique ?(c = compare) xs =
 let mem = Set.mem ~c:c and add = Set.add ~c:c in
 let add (xs,s) x = if mem x s then (xs,s) else (x::xs,add x s) in
 rev (fst (foldl add ([],Set.empty) xs))
;;

let unique_hash xs =
 let t = H.create (length xs) in
 let add xs x = if H.mem t x then xs else (H.add t x 1; x::xs) in
 rev (foldl add [] xs)
;;

let powerset xs =
 let rec powerset ys = function
  | [] -> ys
  | x::xs -> powerset (rev_append (map (cons x) ys) ys) xs
 in
 powerset [[]] (rev xs)
;;

(* Printers *)
let fprintfi f d fmt xs =
 let fpf = Format.fprintf in
 let rec fprintfi i fmt = function
  | []    -> ()
  | [x]   -> fpf fmt "%a" (f i) x
  | x::xs -> fpf fmt "%a%a%a" (f i) x fpf d (fprintfi (i+1)) xs
 in
 fpf fmt "@[%a@]" (fprintfi 0) xs
;;

let fprintf f d fmt = fprintfi (const f) d fmt;;

let joini f d = 
 let f i fmt x = Format.fprintf fmt "%s" (f i x) in
 Format.flush_str_formatter <.> fprintfi f d Format.str_formatter
;;

let join f = joini (const f);;
let to_stringi = joini;;
let to_string = join;;

(* Properties *)
let is_empty xs = xs = [];;
let is_singleton = function [_] -> true | _ -> false;;
let is_prefix ?(c = compare) xs ys = fst (remove_equal ~c:c xs ys) = [];;
let is_suffix ?(c = compare) xs ys = is_prefix ~c:c (rev xs) (rev ys);;

let is_proper_prefix ?(c = compare) xs ys =
 let (xs,ys) = remove_equal ~c:c xs ys in
 xs = [] && ys <> []
;;

let is_proper_suffix ?(c = compare) xs ys =
 is_proper_prefix ~c:c (rev xs) (rev ys)
;;

(* Miscallenous *)
let copy = id;;
let replicate ?(c = id) n x = map_range (fun _ -> c x) 0 n;;
let range n m = map_range id n m;;

let rec extend ?(c = id) n xs =
 if n <= 0 then [] else
  let ys = map c (take n xs) in ys @ extend (n - length ys) xs
;;

let combinations i xs =
 let rec combinations i acc =
  if i < 1 then if Int.even i then rev acc else acc
  else
   let xs = if Int.even i then rev xs else xs in
   let combine y ys = foldl (fun ys x -> cons y x :: ys) ys acc in
   combinations (i - 1) (foldl (fun ys x -> combine x ys) [] xs)
 in
 combinations i [[]]
;;

let init xs =
 let rec init acc = function
  | [] -> failwith "empty list"
  | [_] -> rev acc
  | x :: xs -> init (x :: acc) xs
 in
 init [] xs
;;

let rec last = function
 | [] -> failwith "empty list"
 | [x] -> x
 | _ :: xs -> last xs
;;

let transpose xs =
 let rec transpose = function
  | (_ :: _) :: _ as xs -> map hd xs :: transpose (map tl xs)
  | xs -> if for_all ((=) []) xs then [] else failwith "different list lengths"
 in
 (transpose <?> "different list lengths") xs
;;

let count elt = foldl (fun acc li -> if li = elt then acc+1 else acc) 0;;
