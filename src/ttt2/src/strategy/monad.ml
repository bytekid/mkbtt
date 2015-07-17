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
open Processors;;

(*** MODULES ******************************************************************)
module Signature = Rewritingx.Signature;;
module P = Pair;;
module R = Rewritingx.Monad;;

module Configuration = struct
 (*** TYPES ******************************************************************)
 type t = {stop : bool};;

 (*** FUNCTIONS **************************************************************)
 (* Constructors *)
 let make stop = {stop = stop};;
 let clear_stop _ = {stop = false};;
 let set_stop _ = {stop = true};;

 (* Predicates *)
 let is_stop s = s.stop;;
end

(* state monad used to configurate the strategy (defines when computation
should be aborted) *)
module C = struct
 (*** MODULES *****************************************************************)
 module S = Configuration;;
 module M = Monad.Transformer.State (S) (R);;

 (*** INCLUDES ****************************************************************)
 include Monad.Make (M);;

 (*** TYPES *******************************************************************)
 type state = Signature.t * Configuration.t;;

 (*** FUNCTIONS ***************************************************************)
 (* Monadic Functions *)
 let liftm = M.liftm;;
 let get = M.get >>= fun c -> liftm R.get >>= fun s -> return (P.make s c);;
 let set s = liftm (R.set (fst s)) >> M.set (snd s);;
 let modify f = get >>= fun s -> let s = f s in set s >> return s;;
 let update f = get >>= fun s -> set (f s);;
 let with_state f m = m >>= fun x -> update f >> return x;;

 let run s m =
  let m = m >>= fun x -> get >>= (return <.> Util.pair x) in
  either failwith id (R.run (fst s) (M.run (snd s) m))
 ;;

 (* Access Functions *)
 let clear_stop = M.get >>= (M.set <.> S.clear_stop);;
 let set_stop = M.get >>= (M.set <.> S.set_stop);;

 (* Predicates *)
 let is_stop = M.get >>= (return <.> S.is_stop);;

 (* Counstructors *)
 let liftr m = liftm m;;
 let result r = liftm (R.return r);;
end

(* list monad used to store the problems *)
module L = struct
 (*** TYPES *******************************************************************)
 type state = Signature.t * Status.t;;

 (*** INCLUDES ****************************************************************)
 include Monad.Transformer.List (C);;

 (*** FUNCTIONS ***************************************************************)
 (* Monadic Functions *)
 let modify = liftm <.> C.modify;;
 let update = liftm <.> C.update;;
 let get = liftm C.get;;
 let set = liftm <.> C.set;;
 let with_state f = liftm <.> C.with_state f;;
 let run s = C.run s <.> run;;

 (* Counstructors *)
 let liftr m = liftm (C.liftr m);;
 let result = C.result;;
end

(* error monad used to differ between completed and incomplete proofs *)
module T = struct
 (*** INCLUDES ****************************************************************)
 include Monad.Transformer.Error (struct type t = State.t end) (L);;

 (*** FUNCTIONS ***************************************************************)
 (* Monadic Functions *)
 let modify = liftm <.> L.modify;;
 let update = liftm <.> L.update;;
 let get = liftm L.get;;
 let set = liftm <.> L.set;;
 let with_state f = liftm <.> L.with_state f;;
 let run s = L.run s <.> run;;

 (* Counstructors *)
 let liftr m = liftm (L.liftr m);;
 let result xs = liftm (L.result xs);;
end

(*** TYPES ********************************************************************)
type state = State.t;;
type 'a m = 'a T.t;;

(*** FUNCTIONS ****************************************************************)
(* Properties *)
let status xs =
 let collect f = flip (Status.collect <.> State.get_status <.> f) in
 let default f = State.get_status <.> f <.> List.hd in
 List.foldl (collect (either id snd)) (default (either id snd) xs) xs
;;

let is_fail xs = Status.is_fail (status xs);;
let is_success xs = Status.is_success (status xs);;
let is_complete xs = Status.is_complete (status xs);;
let is_unfinished xs = Status.is_unfinished (status xs);;

(*** INCLUDES *****************************************************************)
include Monad.Make (struct
 type 'a t = state -> ('a * state) m;;

 let check f g xs =
  let status = status xs in
  if Status.is_nonterminating status then g xs
  else if Status.is_fail status then C.ite C.is_stop g f xs
  else f xs
 ;;

 let employ f =
  let rec employ ys = function
   | [] -> C.return (List.rev ys)
   | Left s :: xs -> employ (Left s :: ys) xs
   | Right (x,s) :: xs ->
    let status = State.get_status s in
    if Status.is_terminating status || Status.is_fail status then
     employ (Left s :: ys) xs
    else
     let append = flip employ xs <.> flip List.rev_append ys in
     let return zs =
      C.(>>=) (T.(>>=) (C.return xs) (T.fail <.> snd))
      (C.return <.> List.rev <.> flip List.rev_append (List.rev_append zs ys))
     in
     C.(>>=) (f x s) (check append return)
  in
  employ []
 ;;

 let (>>=) m f = (fun s ->
   let return = flip T.(>>=) (T.fail <.> snd) <.> C.return in
   C.(>>=) (m s) (check (employ f) return))
 ;;
 
 let return x = (fun s -> T.return (x,s));;
end);;

(*** FUNCTIONS ****************************************************************)
 (* Monadic Functions *)
let liftm m = (fun s -> T.(>>=) m (flip return s));;
let modify f = (fun s -> let s = f s in T.return (s,s));;
let update f = (fun s -> T.return ((),f s));;
let get = (fun s -> modify id s);;
let set s = update (const s);;
let with_state f m = (fun s -> T.(>>=) (m s) (fun (x,s) -> return x (f s)));;

let execute s s' m =
 let (xs,s') = T.run s' (m s) in
 let collect f = flip (Status.collect <.> State.get_status <.> f) in
 let default f = State.get_status <.> f <.> List.hd in
 let status f xs = List.foldl (collect f) (default f xs) xs in
 (xs,s',status (either id snd) xs)
;;

let run s s' m =
 let filter = Proof.merge <.> List.map (State.get_proof <.> either id snd) in
 let error = Format.sprintf "error '%s' occurred" in
 let c = Configuration.make false in
 try Triple.apply filter fst id (execute s (s',c) m)
 with Failure s -> failwith(error s)
;;

(* Constructors *)
let liftr m = liftm (T.liftr m);;
let result r = const (T.result r);;

(* Combinators *)
(* s;s *)
let combine = (>=>);;

(* s | s *)
let choose f g x = (fun s ->
 C.(>>=) (f x s) (fun xs -> if is_fail xs then g x s else C.return xs))
;;

(* s || s *)
let parallel f g x = (fun s ->
 C.(>>=) C.get (fun s' ->
 let is_success = Status.is_success <.> Triple.thd in
 let f = execute s s' <.> f and g = execute s s' <.> g in
 match Process.run_parallel_until is_success [f;g] x with
  | None -> T.result [(x,State.set_status Status.fail s)]
  | Some (xs,s',_) -> C.(>>) (C.set s') (C.return xs)))
;;

(* if p then s else s *)
let condition = ite;;

(* {s}s *)
let alter f g x = get >>= fun s -> f x >>= g (State.get_problem s);;

(* Iterators *)
let reapply f xs = const (C.return xs) >>= f;;

(* s? *)
let optional f x = (fun s ->
 C.(>>=) (f x s) (fun xs ->
 if is_fail xs then T.result [(x,s)] else C.return xs))
;;

(* s* *)
let rec iterate f x = (fun s ->
 C.(>>=) (f x s) (fun xs ->
 if is_fail xs then T.result [(x,s)] else reapply (iterate f) xs s))
;;

(* s+ *)
let rec repeat f x = f x >>= (repeat f);;

(* sn* *)
let rec duplicate n f x = (fun s ->
 C.(>>=) (f x s) (fun xs ->
 if is_fail xs then T.result [(x,s)]
 else if n <= 1 then C.return xs
 else reapply (duplicate (n - 1) f) xs s))
;;

(* s[t]* *)
let iterate_timed t =
 let t = Unix.gettimeofday () +. t in
 let rec iterate_timed f x = (fun s ->
  C.(>>=) C.get (fun s' ->
  match fst (Process.run_timed (Process.Global t) (execute s s' <.> f) x) with
   | None -> T.result [(x,s)]
   | Some (xs,s',_) ->
    C.(>>) (C.set s')
     (if is_fail xs then T.result [(x,s)]
     else reapply (iterate_timed f) xs s)))
 in
 iterate_timed
;;

(* Specifiers *)
(* s% *)
let stop f x = (fun s ->
 C.(>>=) (C.(>>) C.set_stop (f x s)) (C.(>>) C.clear_stop <.> C.return))
;;

(* s! *)
let strict f x = (fun s ->
 C.(>>=) (f x s) (fun xs ->
 if is_complete xs then C.return xs
 else T.result [(x,State.set_status Status.fail s)]))
;;

(* s[t] *)
let timed t f x = (fun s ->
 C.(>>=) C.get (fun s' ->
 match fst (Process.run_timed (Process.Local t) (execute s s' <.> f) x) with
  | None -> T.result [(x,State.set_status Status.fail s)]
  | Some (xs,s',_) -> C.(>>) (C.set s') (C.return xs)))
;;
