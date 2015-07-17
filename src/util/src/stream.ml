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
module LL = LazyList;;

(*** MODULE TYPES *************************************************************)
module type STATE = sig type t end

module type SIGNATURE = sig
  type 'a t
  type state

  val gen : ('a -> state -> ('a * state)option) -> 'a -> 'a t
  val of_streams : ('a -> 'b t) -> 'a list -> 'b t
  val of_values : ('a -> state -> ('b * state)option) -> 'a list -> 'b t

  val append : 'a t -> 'a t -> 'a t
  val concat : 'a t t -> 'a t
  val flat_map : ('a -> 'b t) -> 'a t -> 'b t
  val merge : 'a t -> 'a t -> 'a t

  val filter : ('a -> bool) -> 'a t -> 'a t
  val sieve : ('a -> 'a -> bool) -> 'a t -> 'a t

  val eval : 'a t -> state -> ('a * state)LazyList.t

  val iterate : ('a -> state -> ('a * state)) -> 'a -> 'a t
end

(*** MODULES ******************************************************************)
module Make(State : STATE) : SIGNATURE with type state = State.t = struct
  type state = State.t;;

  (* A stream is a function taking a state on which the value
  of the first element may depend. The result consists of the
  first element together with a resulting state and the function
  for the next element. *)
  type 'a t = state -> 'a cell
  and 'a cell = Eos | Cell of ('a * state * 'a t);;

  let rec append xs ys = (fun s -> match xs s with
    | Cell(x,s,xs) -> Cell(x,s,append xs ys)
    | Eos          -> ys s
  );;

(******************************************************************************)
(**) let rec concat xss = (fun s -> match xss s with
(**)   | Cell(xs,_,xss) -> append xs (concat xss) s
(**)   | Eos            -> Eos
(**) );;
(**) 
(**) let rec map f xs = (fun s -> match xs s with
(**)   | Cell(x,s,xs) -> let (x,s) = f x s in Cell(x,s,map f xs)
(**)   | Eos          -> Eos
(**) );;
(**) 
(**) let flat_map f xs = concat(map (fun x s -> (f x,s)) xs);;
(******************************************************************************)

  let rec eval xs s = lazy(match xs s with
    | Cell(x,s,xs) -> LL.Cons((x,s),eval xs s)
    | Eos          -> LL.Nil
  );;

(******************************************************************************)
(**) let rec filter p xs = (fun s -> match xs s with
(**)   | Cell(x,s',xs) -> if p x then Cell(x,s',filter p xs)
(**)                             else filter p xs s
(**)   | Eos           -> Eos
(**) );;
(**)
(**) let rec sieve p xs = (fun s -> match xs s with
(**)   | Cell(x,s',xs) -> Cell(x,s',sieve p (filter (p x) xs))
(**)   | Eos           -> Eos
(**) );;
(******************************************************************************)

  let rec gen f x = (fun s -> match f x s with
    | Some(x,s) -> Cell(x,s,gen f x)
    | None      -> Eos
  );;

  let rec merge xs ys = (fun s -> match xs s with
    | Cell(x,s,xs) -> Cell(x,s,merge ys xs)
    | Eos          -> ys s
  );;

  let rec iterate f x = (fun s ->
    let (y,s') = f x s in
    Cell(x,s,(fun _ -> iterate f y s'))
  );;

  let rec of_values f xs = (fun s -> match xs with
    | x::xs -> (match f x s with Some(y,s) -> Cell(y,s,of_values f xs)
                               | None      -> of_values f xs s
    )
    | []    -> Eos
  );;

  let rec of_streams f xs = concat(of_values (fun x s -> Some(f x,s)) xs);;
end
