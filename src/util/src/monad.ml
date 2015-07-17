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
open Either;;

(*** MODULES ******************************************************************)
module F = Format;;
module LL = LazyList;;

(*** MODULE TYPES *************************************************************)
module type ERROR = sig type t end
module type STATE = sig type t end

module type MINIMAL_MONAD = sig
 type 'a t

 val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
 val return : 'a -> 'a t
end

module type SIGNATURE = sig
 include MINIMAL_MONAD

 val (=<<) : ('a -> 'b t) -> 'a t -> 'b t
 val (>>) : 'a t -> 'b t -> 'b t
 val (<<) : 'b t -> 'a t -> 'b t
 val (>=>) : ('a -> 'b t) -> ('b -> 'c t) -> 'a -> 'c t
 val (<=<) : ('b -> 'c t) -> ('a -> 'b t) -> 'a -> 'c t
 val ap : ('a -> 'b) t -> 'a t -> 'b t
 val join : 'a t t -> 'a t
 val lift : ('a -> 'b) -> 'a t -> 'b t
 val lift2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
 val lift3 : ('a -> 'b -> 'c -> 'd) -> 'a t -> 'b t -> 'c t -> 'd t
 val exists : ('a -> bool t) -> 'a list -> bool t
 val filter : ('a -> bool t) -> 'a list -> 'a list t
 val flat_map : ('a -> 'b list t) -> 'a list -> 'b list t
 val flat_mapi : (int -> 'a -> 'b list t) -> 'a list -> 'b list t
 val foldl : ('a -> 'b -> 'a t) -> 'a -> 'b list -> 'a t
 val foldl1 : ('a -> 'a -> 'a t) -> 'a list -> 'a t
 val foldl2 : ('a -> 'b -> 'c -> 'a t) -> 'a -> 'b list -> 'c list -> 'a t
 val foldli : (int -> 'a -> 'b -> 'a t) -> 'a -> 'b list -> 'a t
 val foldr : ('a -> 'b -> 'b t) -> 'b -> 'a list -> 'b t
 val foldr1 : ('a -> 'a -> 'a t) -> 'a list -> 'a t
 val foldr2 : ('a -> 'b -> 'c -> 'c t) -> 'c -> 'a list -> 'b list -> 'c t
 val foldri : (int -> 'a -> 'b -> 'b t) -> 'b -> 'a list -> 'b t
 val for_all : ('a -> bool t) -> 'a list -> bool t
 val iter : ('a -> unit t) -> 'a list -> unit t
 val iteri : (int -> 'a -> unit t) -> 'a list -> unit t
 val map : ('a -> 'b t) -> 'a list -> 'b list t
 val mapi : (int -> 'a -> 'b t) -> 'a list -> 'b list t
 val replicate : int -> 'a t -> 'a list t
 val replicatei : int -> (int -> 'a t) -> 'a list t
 val rev_map : ('a -> 'b t) -> 'a list -> 'b list t
 val rev_mapi : (int -> 'a -> 'b t) -> 'a list -> 'b list t
 val sequence : 'a t list -> 'a list t
 val apply : ('a -> 'c t) -> ('b -> 'd t) -> 'a * 'b -> ('c * 'd) t
 val cross : ('a -> 'c t) * ('b -> 'd t) -> 'a * 'b -> ('c * 'd) t
 val fold : ('a -> 'c -> 'c t) -> ('b -> 'c -> 'c t) -> 'c -> 'a * 'b -> 'c t
 val pair : ('a -> 'b t) * ('a -> 'c t) -> 'a -> ('b * 'c) t
 val project : ('a -> 'b t) -> 'a * 'a -> ('b * 'b) t
 val uncurry : ('a -> 'b -> 'c t) -> 'a * 'b -> 'c t
 val ite : bool t -> ('a -> 'b t) -> ('a -> 'b t) -> 'a -> 'b t
 val fprintf : (F.formatter -> 'a -> unit t)
  -> (unit,F.formatter,unit) Pervasives.format -> F.formatter
  -> 'a list -> unit t
 val fprintfi : (int -> F.formatter -> 'a -> unit t) 
  -> (unit,F.formatter,unit) Pervasives.format -> F.formatter
  -> 'a list -> unit t
 val to_string : ('a -> string t)
  -> (unit,F.formatter,unit) Pervasives.format -> 'a list -> string t
 val to_stringi : (int -> 'a -> string t)
  -> (unit,F.formatter,unit) Pervasives.format -> 'a list -> string t
end

module type ERROR_MONAD = sig
 type error

 include SIGNATURE

 val ap_error : ((error,'a) either -> (error,'b) either) t -> 'a t -> 'b t
 val map_error : ((error,'a) either -> (error,'b) either) -> 'a t -> 'b t
 val catch : (error -> 'a t) -> 'a t -> 'a t
 val fail : error -> 'a t
 val failif : bool -> error -> unit t
 val run : 'a t -> (error,'a) either
end

module type ID_MONAD = sig
 include SIGNATURE

 val run : 'a t -> 'a
end

module type STATE_MONAD = sig
  type state
 
  include SIGNATURE
 
  val ap_state : ('a * state -> 'b * state) t -> 'a t -> 'b t
  val map_state : ('a * state -> 'b * state) -> 'a t -> 'b t
  val adopt : (state -> 'a * state) -> 'a t
  val get : state t
  val modify : (state -> state) -> state t
  val set : state -> unit t
  val update : (state -> state) -> unit t
  val with_state : (state -> state) -> 'a t -> 'a t
  val eval : state -> 'a t -> 'a * state
  val execute : state -> 'a t -> state
  val run : state -> 'a t -> 'a
end

(*** MODULES ******************************************************************)
module Make (M : MINIMAL_MONAD) = struct
 (*** INCLUDES ****************************************************************)
 include M;;

 (*** FUNCTIONS ***************************************************************)
 (* Miscellaneous *)
 let (=<<) f m = m >>= f;;
 let (>>) m n = m >>= const n;;
 let (<<) m n = n >> m;;
 let (>=>) f g x = f x >>= g;;
 let (<=<) g f = f >=> g;;
 let ap m n = m >>= fun f -> n >>= (return <.> f);;
 let join m = m >>= id;;
 let lift f = ap (return f);;
 let lift2 f m = ap (lift f m);;
 let lift3 f m n = ap (lift2 f m n);;

 (* List Functions *)
 let foldli f = Listx.foldli (fun i m x -> m >>= flip (f i) x) <.> return;;
 let foldl f = foldli (const f);;
 let foldri f = Listx.foldri (fun i x m -> m >>= (f i) x) <.> return;;
 let foldr f = foldri (const f);;

 let foldl1 f =
  (fun xs -> foldl f (Listx.hd xs) (Listx.tl xs)) <?> "empty list"
 ;;

 let foldr1 f =
  (fun xs -> foldr f (Listx.hd xs) (Listx.tl xs)) <?> "empty list"
 ;;

 let foldl2 f = Listx.foldl2 (fun m x y -> m >>= fun z -> f z x y) <.> return;;
 let foldr2 f = Listx.foldr2 (fun x y m -> m >>= f x y) <.> return;;

 let iteri f = foldli (drop f) ();;
 let iter f = iteri (const f);;

 let employ combine f xs =
  let f i xs x = f i x >>= (return <.> flip combine xs) in foldli f [] xs
 ;;

 let flat_mapi f xs = employ Listx.rev_append f xs >>= (return <.> Listx.rev);;
 let flat_map f = flat_mapi (const f);;
 let mapi f xs = employ Listx.cons f xs >>= (return <.> Listx.rev)
 let map f = mapi (const f);;
 let rev_mapi f = employ Listx.cons f;;
 let rev_map f = rev_mapi (const f);;
 let sequence ms = map id ms;;

 let replicatei i m =
  let rec replicatei j n =
   if j < i then
    m j >>= fun x -> n >>= (replicatei (j+1) <.> return <.> Listx.cons x)
   else n >>= (return <.> Listx.rev)
  in
  replicatei 0 (return [])
 ;;

 let replicate n = replicatei n <.> const;;

 let rec for_all p = function
  | [] -> return true
  | x :: xs -> p x >>= fun c -> if c then for_all p xs else return false
 ;;

 let exists p = lift not <.> for_all (lift not <.> p);;

 let filter p xs =
  let f xs x = p x >>= fun c -> if c then return (x :: xs) else return xs in
  foldl f [] xs >>= (return <.> Listx.rev)
 ;;

 (* Pair Functions *)
 let cross (f,g) (x,y) = f x >>= (flip lift (g y) <.> Pair.make);;
 let pair fs = cross fs <.> Pair.create;;
 let apply f = cross <.> Pair.make f;;
 let project f = apply f f;;
 let uncurry f (x,y) = f x y;;
 let fold f g d (x,y) = f x d >>= g y;;

 (* Boolean Functions *)
 let ite m f g x = m >>= fun c -> if c then f x else g x;;

 (* Printers *)
 let fprintfi f d fmt xs =
  let rec fprintfi i = function
   | []    -> return ()
   | [x]   -> f i fmt x
   | x::xs -> f i fmt x >>= fun _ -> F.fprintf fmt d; fprintfi (i+1) xs
  in
  F.fprintf fmt "@["; fprintfi 0 xs >>= fun _ -> return (F.fprintf fmt "@]")
 ;;
 
 let fprintf f = fprintfi (const f);;

 let to_stringi f d xs = 
  let f i fmt x = f i x >>= (return <.> F.fprintf fmt "%s") in
  fprintfi f d F.str_formatter xs >>= (return <.> F.flush_str_formatter)
 ;;

 let to_string f = to_stringi (const f);;
end

(*** MODULES ******************************************************************)
module Transformer = struct
 (*** MODULE TYPES ************************************************************)
 module type COMBINED_MONAD = sig
  type 'a m
 
  include SIGNATURE
 
  val liftm : 'a m -> 'a t
 end
 
 module type ERROR_MONAD = sig
  type error
 
  include COMBINED_MONAD
 
  val ap_error : ((error,'a) either m -> (error,'b) either m) t -> 'a t -> 'b t
  val map_error : ((error,'a) either m -> (error,'b) either m) -> 'a t -> 'b t
  val catch : (error -> 'a t) -> 'a t -> 'a t
  val fail : error -> 'a t
  val failif : bool -> error -> unit t
  val run : 'a t -> (error,'a) either m
 end
 
 module type LIST_MONAD = sig
  include COMBINED_MONAD
 
  val ap_list : ('a list m -> 'b list m) t -> 'a t -> 'b t
  val map_list : ('a list m -> 'b list m) -> 'a t -> 'b t
  val run : 'a t -> 'a list m
 end
 
 module type OPTION_MONAD = sig
  include COMBINED_MONAD
 
  val ap_option : ('a option m -> 'b option m) t -> 'a t -> 'b t
  val map_option : ('a option m -> 'b option m) -> 'a t -> 'b t
  val run : 'a t -> 'a option m
 end

 module type STATE_MONAD = sig
  type state
 
  include COMBINED_MONAD
 
  val ap_state : (('a * state) m -> ('b * state) m) t -> 'a t -> 'b t
  val map_state : (('a * state) m -> ('b * state) m) -> 'a t -> 'b t
  val adopt : (state -> 'a * state) -> 'a t
  val get : state t
  val modify : (state -> state) -> state t
  val set : state -> unit t
  val update : (state -> state) -> unit t
  val with_state : (state -> state) -> 'a t -> 'a t
  val eval : state -> 'a t -> ('a * state) m
  val execute : state -> 'a t -> state m
  val run : state -> 'a t -> 'a m
 end

 (*** MODULES *****************************************************************)
 module Error (E : ERROR) (M : MINIMAL_MONAD) = struct
  (*** MODULES ****************************************************************)
  module M = Make (M);;
 
  (*** TYPES ******************************************************************)
  type error = E.t;;
  type 'a m = 'a M.t;;

  (*** FUNCTIONS **************************************************************)
  let error e = Left e;;
  let result x = Right x;;
 
  (*** INCLUDES ***************************************************************)
  include Make (struct 
   type 'a t = (error,'a) either m;;

   let (>>=) m = M.(>>=) m <.> either (M.return <.> error);;
   let return x = M.return (result x);;
  end);;
 
  (*** FUNCTIONS **************************************************************)
  let liftm m = M.(>>=) m return;;

  (* Access Functions *)
  let ap_error m n = m >>= swap n;;
  let map_error f = ap_error (return f);;

  (* Error Handling *)
  let catch h m = M.(>>=) m (either h return);;
  let fail e = M.return (error e);;
  let failif b e = if b then fail e else return ();;
  
  (* Evaluation Functions *)
  let run = id;;
 end

 module List (M : MINIMAL_MONAD) = struct
  (*** TYPES ******************************************************************)
  type 'a m = 'a M.t;;
 
  (*** INCLUDES ***************************************************************)
  include Make (struct 
   type 'a t = 'a list m;;
  
   let (>>=) m f =
    let rev_append xs = M.return <.> Listx.rev_append xs in
    let f x xs = M.(>>=) (f x) (flip rev_append xs) in
    let g = Listx.foldl (fun m -> M.(>>=) m <.> f) (M.return []) in
    M.(>>=) (M.(>>=) m g) (M.return <.> Listx.rev)
   ;;
   let return x = M.return [x];;
  end);;
 
  (*** FUNCTIONS **************************************************************)
  let liftm m = M.(>>=) m return;;

  (* Access Functions *)
  let ap_list m n = m >>= swap n;;
  let map_list f = ap_list (return f);;

  (* Evaluation Functions *)
  let run = id;;
 end
 
 module Option (M : MINIMAL_MONAD) = struct
  (*** TYPES ******************************************************************)
  type 'a m = 'a M.t;;
 
  (*** INCLUDES ***************************************************************)
  include Make (struct 
   type 'a t = 'a option m;;
  
   let (>>=) m f = M.(>>=) m (Option.fold f (M.return None));;
   let return x = M.return (Some x);;
  end);;
 
  (*** FUNCTIONS **************************************************************)
  let liftm m = M.(>>=) m return;;

  (* Access Functions *)
  let ap_option m n = m >>= swap n;;
  let map_option f = ap_option (return f);;

  (* Evaluation Functions *)
  let run = id;;
 end

 module State (S : STATE) (M : MINIMAL_MONAD) = struct
  (*** TYPES ******************************************************************)
  type state = S.t;;
  type 'a m = 'a M.t;;
 
  (*** INCLUDES ***************************************************************)
  include Make (struct 
   type 'a t = state -> ('a * state) m;;
  
   let (>>=) m f = (fun s -> M.(>>=) (m s) (Pair.uncurry f));;
   let return x = (fun s -> M.return (x,s));;
  end);;
 
  (*** FUNCTIONS **************************************************************)
  let liftm m = (fun s -> M.(>>=) m (flip return s));;

  (* Access Functions *)
  let ap_state m n = m >>= fun f -> f <.> n;;
  let map_state f = ap_state (return f);;

  (* State Modifications *)
  let adopt f = (fun s -> let (x,s) = f s in M.return (x,s));;
  let modify f = adopt (Pair.create <.> f);;
  let update f = (fun s -> M.return ((),f s));;
  let get = (fun s -> modify id s);;
  let set s = update (const s);;
  let with_state f m = (fun s -> M.(>>=) (m s) (fun (x,s) -> return x (f s)));;

  (* Evaluation Functions *)
  let execute s m = M.(>>=) (m s) (M.return <.> snd);;
  let run s m = M.(>>=) (m s) (M.return <.> fst);;
  let eval s m = m s;;
 end
end

module Id = Make (struct 
 type 'a t = 'a;;

 let (>>=) = swap;;
 let return = id;;
end);;

module LazyList = Make (struct
 type 'a t = 'a LL.t;;

 let return x = lazy (LL.Cons (x,LL.empty));;
 let (>>=) xs f = LL.concat (LL.map f xs);;
end);;

module Error = Transformer.Error (Stringx) (Id);;
module List = Transformer.List (Id);;
module Option = Transformer.Option (Id);;
module State (S : STATE) = Transformer.State (S) (Id);;
