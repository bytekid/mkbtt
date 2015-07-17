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

(*** TYPES ********************************************************************)
type 'a t   = 'a cell Lazy.t
and 'a cell = Nil | Cons of ('a * 'a t);;

(*** EXCEPTIONS ***************************************************************)
exception End_of_list;;

(*** FUNCTIONS ****************************************************************)
let fc = Lazy.force;;
let empty = lazy(Nil);;
let null xs = match fc xs with Nil -> true | _ -> false;;
let singleton x = lazy(Cons(x,empty));;

let hd xs = match fc xs with Cons(x,_) -> x
                           | Nil       -> failwith "empty"
;;

let tl xs = match fc xs with Cons(_,xs) -> xs
                           | Nil        -> failwith "empty"
;;

let rec find p xs = match fc xs with
 | Cons(x,xs) -> if p x then x else find p xs
 | Nil        -> raise Not_found
;;

let nth n ls =
 let rec nth i xs = match fc xs with
  | Cons(x,xs) -> if i = n then x else nth (i+1) xs
  | Nil        -> failwith "out of bounds"
 in nth 0 ls
;;

let rec take n xs = if n < 1 then empty else lazy(match fc xs with
  | Cons(x,xs) -> Cons(x,take (n-1) xs)
  | Nil        -> Nil
);;

(******************************************************************************)
(**) let rec take_list i xs = if i < 1 then [] else match fc xs with
(**)  | Cons(x,xs) -> x :: take_list (i-1) xs
(**)  | Nil        -> []
(**) ;;
(**) 
(**) let rec to_list_all xs = match fc xs with
(**)  | Cons(x,xs) -> x :: to_list_all xs
(**)  | Nil        -> []
(**) ;;
(**) 
(**) let to_list ?n xs = match n with None -> to_list_all xs
(**)                                | Some n -> take_list n xs
(**) ;;
(******************************************************************************)

let rec make i f = lazy(
 Cons(i,try make (f i) f with End_of_list -> lazy(Cons(i,empty)))
);;

let of_function f =
  let rec gen i = lazy(match f i with
    | Some x -> Cons(x,gen(i+1))
    | None   -> Nil
  ) in
  gen 0
;;

let of_string s = of_function(fun i ->
  if i < String.length s then Some(s.[i])
                         else None
);;

let of_channel ch = of_function(fun _ -> try
    Some(input_char ch)
  with End_of_file -> (close_in ch; None)
);;

let of_file file = of_channel(open_in file);;

let rec map f xs = lazy(match fc xs with
 | Cons(x,xs) -> Cons(f x,map f xs)
 | Nil        -> Nil
);;

let rec filter f xs = lazy(match fc xs with
 | Cons(x,xs) -> if f x then Cons(x,filter f xs)
                        else fc(filter f xs)
 | Nil        -> Nil
);;

let rec append xs ys = lazy(match fc xs with
  | Cons(x,xs) -> Cons(x,append xs ys)
  | Nil        -> fc ys
);;

let rec of_list xs = lazy(match xs with
 | x::xs -> Cons(x,of_list xs)
 | []    -> Nil
);;

let rec flatten xss = lazy(match fc xss with
 | Cons(xs,xss) -> fc(append xs (flatten xss))
 | Nil          -> Nil
);;

let concat = flatten;;

let mem ?(n = -1) e ls =
 let rec mem i xs = match fc xs with
  | Cons(x,xs) -> if i = n then false
                           else if x = e then true
                           else mem (i+1) xs
  | Nil -> false
 in
 mem 0 ls
;;

let init n f =
 let rec init i acc =
  if (i < 0) then acc
             else init (i-1) (lazy(Cons(f i,acc)))
 in
 if n < 0 then raise (Invalid_argument "init")
          else init (n-1) empty
;;

let rec map2 f xs ys = lazy(match (fc xs,fc ys) with
  | (Cons(x,xs),Cons(y,ys)) -> Cons(f x y,map2 f xs ys)
  | (Nil,Nil)               -> Nil
  | _                       -> raise(Invalid_argument "map2")
);;

let rec prefix xs = lazy(match fc xs with
 | Cons(x,xs) -> Cons(empty,map (fun ls -> lazy(Cons(x,ls))) (prefix xs))
 | Nil        -> Cons(empty,empty)
);;

let rec suffix xs = lazy(match fc xs with
 | Cons(_,xs) as ls -> Cons(lazy ls,suffix xs)
 | Nil              -> Cons(empty,empty)
);;

let interleave x ys =
 map2 
 (fun ls rs -> append (append ls (singleton x)) rs)
 (prefix ys)
 (suffix ys)
;;

let rec permutations xs = match fc xs with
 | Cons(x,xs) -> flatten(map (interleave x) (permutations xs))
 | Nil        -> singleton empty
;;

let rec zip xs ys = lazy(match (fc xs,fc ys) with
 | (Cons(x,xs),Cons(y,ys)) -> Cons((x,y),zip xs ys)
 | (Nil,_) | (_,Nil)       -> Nil
);;

let combinations len ls =
 let rec combinations i len ls =
  if i = len then singleton empty else (
   flatten (map
     (fun l -> map (fun ls' -> lazy(Cons(l,ls'))) (combinations (i+1) len ls))
     ls)
  )
 in
 combinations 0 len ls
;;

let rec iterate f x = lazy(Cons(x,iterate f (f x)));;

let foldr f d xs =
  let rec foldr xs = lazy(match fc xs with
    | Cons(x,xs) -> fc(f x (foldr xs))
    | Nil        -> fc(d)
  ) in
  foldr xs
;;

let fprintfi f d fmt xs =
 let fpf = Format.fprintf in
 let rec fprintfi i f d fmt xs = match fc xs with
  | Nil        -> ()
  | Cons(x,xs) -> match fc xs with
   | Nil -> fpf fmt "%a" (f i) x
   | _   -> fpf fmt "%a%a%a" (f i) x fpf d (fprintfi (i+1) f d) xs
 in
 fpf fmt "@[%a@]" (fprintfi 0 f d) xs
;;

let fprintf f d fmt = fprintfi (const f) d fmt;;
