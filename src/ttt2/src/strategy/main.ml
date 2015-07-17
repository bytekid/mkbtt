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

(* Adding a new processor:
 * If you want to add a new processor to TTT2 you have to do two things:
 * - Implement your processor and add the corresponding files to the library
 *   [Processors]. Detals about that (guidelines and other useful information)
 *   can be found in the file [processors.ml] contained in the library
 *   Processors.
 * - As soon as your processor is available trough the library [Processors],
 *   you have to specify some information of the processor in order to make
 *   it accessible for TTT2. This means, at first you have to specify the
 *   proof type as well as the functions used to print proof objects created
 *   by your processor. To do that, you have to edit the file
 *   [strategy/processor.ml]. After that you have to implement a wrapper for
 *   your processor. If your processor is used to prove termination, edit
 *   the file [strategy/termination.ml]. If it proves nontermination, edit
 *   the file [strategy/nontermination.ml]. If it is a transformation
 *   processor, edit the file [strategy/transformation.ml] and if it is a
 *   modifier, edit the file [strategy/modifier.ml].
 *
 * Adding a new predicate:
 * Adding a new predicate to TTT2 is a little bit simpler than adding a new
 * processor. You only have to implement a wrapper for your predicate by
 * editing the file [strategy/predicate.ml].
 *)

(*** MODULES ******************************************************************)
module M = Monad;;
module N = Nontermination;;
module O = Modifier;;
module P = Predicate;;
module R = Transformation;;
module S = Util.String;;
module T = Termination;;

(*** OPENS ********************************************************************)
open Util;;

(*** TYPES ********************************************************************)
type condition = Syntax.condition =
 | Atom of string * string list
 | Not of condition
 | And of condition * condition
 | Or of condition * condition
;;

type modifier = Syntax.modifier;;

type strategy = Syntax.t =
 | Strategy of string * string list
 (* combinators *)
 | Combine of strategy * strategy
 | Choose of strategy * strategy
 | Parallel of strategy * strategy
 | Condition of condition * strategy * strategy
 (* iterators *)
 | Optional of strategy
 | Iterate of strategy
 | Repeat of strategy
 | Replicate of int * strategy
 | Iterate_timed of float * strategy
 (* specifiers *)
 | Modify of strategy * modifier
 | Stop of strategy
 | Strict of strategy
 | Timed of float * strategy
;;

(*** GLOBALS ******************************************************************)
let modifiers = List.map Triple.drop_thd O.processors;;
let predicates = List.map Triple.drop_thd P.processors;;

let processors =
 List.map Triple.drop_thd (N.processors @ T.processors @ R.processors)
;;

let fprintf fs fmt ps =
 let extract (id,_,(c,_,fs)) (ps,xs) = ((id,c)::ps,fs::xs) in
 let (ps,xs) = List.foldr extract ([],[]) ps in
 if fs then
  let n = List.foldl (fun j -> max j <.> String.length <.> fst) 0 ps in
  List.iter2 (fun (id,c) fs ->
   Format.fprintf fmt "@[<%i>%s@\n@[<2>Flags:@\n%s@]@]" (n + 1)
    (S.itemizex ~n:2 80 [(String.shift_left (n - String.length id) id,c)])
    (S.itemizex ~n:(n + 5) 80 fs)) ps xs
 else Format.fprintf fmt "@[%s@]" (S.itemizex ~n:2 80 ps)
;;

let help ?(fs = false) _ =
 Format.printf "@[@[<2>Termination Processors:@\n%a@]@\n@\n\
  @[<2>Nontermination Processors:@\n%a@]@\n@\n\
  @[<2>Transformation Processors:@\n%a@]@\n@\n\
  @[<2>Modifiers:@\n%a@]@\n@\n@[<2>Predicates:@\n%a@]@\n@]%!"
  (fprintf fs) T.processors (fprintf fs) N.processors (fprintf fs) R.processors
  (fprintf fs) O.processors (fprintf fs) P.processors
;;

let locate ?(fs = false) e =
 let adapt ps = List.map (Triple.replace_snd ()) ps in
 let exists = List.exists (flip (Str.string_match e) 0) in
 let ps =
  List.filter (fun (id,_,(_,ks,_)) -> exists (id::ks))
   ((adapt T.processors) @ (adapt N.processors) @ (adapt R.processors) @
   (adapt O.processors) @ (adapt P.processors))
 in
 Format.printf "@[@[<2>Processors:@\n%a@]@\n@]" (fprintf fs) ps
;;

(*** FUNCTIONS ****************************************************************)
let (>>=) = M.(>>=);;
let find n ps = try List.assoc n ps with Not_found -> failwith "unknown";;

let rec condition = function
 | Atom (n,fs) ->
  (find n <?> Format.sprintf "unknown predicate: `%s'" n) predicates fs
 | Not c -> condition c >>= (M.return <.> not)
 | And (c,c') ->
  condition c >>= fun c -> if c then condition c' else M.return false
 | Or (c,c') ->
  condition c >>= fun c -> if c then M.return true else condition c'
;;

let rec strategy = function
 | Strategy (n,fs) ->
  (find n <?> Format.sprintf "unknown processor: `%s'" n) processors fs
 (* combinators *)
 | Combine (s,s') -> M.combine (strategy s) (strategy s')
 | Choose (s,s') -> M.choose (strategy s) (strategy s')
 | Parallel (s,s') -> M.parallel (strategy s) (strategy s')
 | Condition (c,s,s') -> M.condition (condition c) (strategy s) (strategy s')
 (* iterators *)
 | Optional s -> M.optional (strategy s)
 | Iterate s -> M.iterate (strategy s)
 | Repeat s -> M.repeat (strategy s)
 | Replicate (n,s) -> M.duplicate n (strategy s)
 | Iterate_timed (t,s) -> M.iterate_timed t (strategy s)
 (* specifiers *)
 | Modify (s,(n,fs)) ->
  let m = (find n <?> Format.sprintf "unknown modifier: `%s'" n) modifiers in
  M.alter (strategy s) (m fs)
 | Stop s -> M.stop (strategy s)
 | Strict s -> M.strict (strategy s)
 | Timed (t,s) -> M.timed t (strategy s)
;;

let run p s t =
 M.run (State.make p Proof.unfinished Status.unfinished) s (strategy t ())
;;
