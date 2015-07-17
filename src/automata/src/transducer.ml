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
module Make (R : REWRITING)
            (C : CATEGORIZATION with type 'a m = 'a R.Monad.t) = struct
 (*** MODULES *****************************************************************)
 module A = Automaton.Make (R) (C);;
 module F = Format;;
 module Fun = R.Function;;
 module Lhs = Lhs.Make (R);;
 module M = Monad.Make (R);;

 (*** OPENS *******************************************************************)
 open Util;;

 (*** TYPES *******************************************************************)
 type 'a m = 'a M.t;;
 type automaton = A.t;;
 type lhs = Lhs.t = State of State.t | Fun of Fun.t * State.t list;;
 type t = A.t * A.t;;

 (*** FUNCTIONS ***************************************************************)
 let (>>=) = M.(>>=);;

 (* Iterators *)
 let apply = Pair.apply;;
 let fold f = Pair.fold f f;;
 let map = Pair.map;;
 let project = Pair.map;;
 let uncurry = Pair.uncurry;;
 
 (* Constructors and Destructors *)
 let make = pair;;
 let fst = fst;;
 let snd = snd;;
 let invert = Pair.flip;;

 let transitive_closure (a,b) =
  let rec closure (a,b) n m =
   A.analogies a b >>= M.foldl (fun (a,b) (p,q) ->
    A.update (State q) p a >>= fun a -> A.update (State p) q b >>= fun b ->
    M.return (a,b)) (a,b) >>= fun (a,b) ->
   let n' = A.size a and m' = A.size b in
   if n' = n && m' = m then M.return (a,b) else closure (a,b) n' m'
  in
  closure (a,b) (A.size a) (A.size b) >>= M.project A.reduce
 ;;

 (* Miscellaneous *)
 let copy = map A.copy;;
 let size = fold (+) 0 <.> map A.size;;

 let successors c (a,b) =
  A.analogies a c >>= M.foldl (fun c (p,q) -> A.update (State p) q c) c >>=
  flip A.combine b >>= A.reduce
 ;;

 (* Printers *)
 let fprintf fmt (a,b) =
  F.fprintf fmt "@[@[<1>first:@\n%a@]" A.fprintf a;
  F.fprintf fmt "@[<1>second:@\n%a@]@]" A.fprintf b
 ;;

 let to_string = F.flush_str_formatter <.> fprintf F.str_formatter;;

 let fprintfm fmt (a,b) =
  F.fprintf fmt "@[@[<1>first:@\n"; A.fprintfm fmt a >>= fun _ ->
  F.fprintf fmt "@]@[<1>second:@\n"; A.fprintfm fmt b >>= fun _ ->
  M.return (F.fprintf fmt "@]@]")
 ;;

 let to_stringm t =
  fprintfm F.str_formatter t >>= (M.return <.> F.flush_str_formatter)
 ;;
end
