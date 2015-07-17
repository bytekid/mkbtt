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
module Make (R : REWRITING) = struct
 (*** MODULES *****************************************************************)
 module E = Util.Either;;
 module F = Format;;
 module Fun = R.Function;;
 module M = Monad.Make (R);;
 module P = Parser.Make (R);;
 module Sig = R.Signature;;
 module Term = R.Term;;
 module Var = R.Variable;;
 
 (*** OPENS *******************************************************************)
 open Util;;

 (*** TYPES *******************************************************************)
 type 'a m = 'a M.t;;
 type 'a p = 'a P.t;;
 type term = Term.t;;
 type t = Var of Var.t | State of State.t | Fun of Fun.t * t list;;
 
 (*** FUNCTIONS ***************************************************************)
 (* Parsers *)
 let (>>=) = P.(>>=);;
 let (>>) = P.(>>);;
 let (<?>) = P.(<?>);;
 let (<|>) = P.(<|>);;

 let rec parse vs ps =
  let id = (P.many1 (P.noneof " \t\n\r()\",") >>= function
   | ['-';'>'] | ['=';'='] | ['-';'>';'='] | ['-';'>';'<';'-'] -> P.fail
   | i -> P.return i) <?> "identifier"
  in
  let create_fun n a =
   P.get_state >>= fun (s,status) ->
   let (f,s) = Sig.create_fun a n s in P.set_state (s,status) >> P.return f
  in
  let create_var n =
   P.get_state >>= fun (s,status) ->
   let (x,s) = Sig.create_var n s in P.set_state (s,status) >> P.return x
  in
  let create_state n =
   P.get_state >>= fun (s,status) ->
   try
    let (p,status) = Status.create_state (int_of_string n) status in
    P.set_state (s,status) >> P.return p
   with Failure _ -> P.fail
  in
  P.lex id >>= fun i ->
  let n = String.of_char_list i in
  (P.lex (P.char '(') >>
   P.sep_by (parse vs ps) (P.lex (P.char ',')) >>= fun ts ->
   create_fun n (List.length ts) >>= fun f ->
   P.lex (P.char ')') >> P.return (Fun (f,ts)))
  <|>
  (if List.mem n vs then create_var n >>= fun x -> P.return (Var x)
  else if List.mem n ps then create_state n >>= fun p -> P.return (State p)
  else create_fun n 0 >>= fun f -> P.return (Fun (f,[])))
 ;;

 let of_string vs ps input =
  M.(>>=) M.get (fun s ->
  let m = parse vs ps >>= fun t -> P.get_state >>= (P.return <.> Pair.make t) in
  match P.run m s (Parsec.StringInput.of_string input) with
   | Left e -> M.fail (Parsec.Error.to_string e)
   | Right (t,s) -> M.(>>) (M.set s) (M.return t))
 ;;

 let (>>=) = M.(>>=);;

 (* Constructors and Destructors *)
 let make_fun f ts = Fun (f,ts);;
 let make_state p = State p;;
 let make_var x = Var x;;
 
 let rec of_term = function
  | Term.Var x -> Var x
  | Term.Fun (f,ts) -> Fun (f,List.map of_term ts)
 ;;
 
 let rec to_term = function
  | Var x -> Term.Var x
  | State _ -> failwith "state occured" 
  | Fun (f,ts) -> Term.Fun (f,List.map to_term ts)
 ;;

 (* Term Symbols *)
 let cons =
  let rec cons = function
   | Var _ | State _ -> []
   | Fun (f,ts) ->
    let cs = List.flat_map cons ts in if ts = [] then f::cs else cs
  in
  List.unique_hash <.> cons
 ;;

 let states =
  let rec states = function
   | Var _ -> []
   | State p -> [p]
   | Fun (f,ts) -> List.flat_map states ts
  in
  List.unique_hash <.> states
 ;;

 let symbols p f =
  let add s ss = if p s then f s :: ss else ss in
  let rec symbols = function
   | State _ -> []
   | Var x -> add (Left x) []
   | Fun (f,ts) -> add (Right f) (List.flat_map symbols ts)
  in
  List.unique_hash <.> symbols
 ;;

 let funs = symbols (E.either (const false) (const true)) E.right;;
 let vars = symbols (E.either (const true) (const false)) E.left;;
 let root = function Fun(f,_) -> Some f | _ -> None;;
 
 (* Properties *)
 let is_fun = function Fun _ -> true | _ -> false;;
 let is_state = function State _ -> true | _ -> false;;
 let is_var = function Var _ -> true | _ -> false;;

 let rec is_ground = function
  | Var _ -> false
  | State _ -> true
  | Fun (_,ts) -> List.for_all is_ground ts
 ;;

 (* Compare Functions *)
 let compare = compare;;
 let equal s t = compare s t = 0;;
 
 (* Miscellaneous *)
 let args = function Var _ | State _ -> [] | Fun (_,ts) -> ts;;
 
 let rec size = function
  | Var _ | State _ -> 1
  | Fun (f,ts) -> List.foldl (fun n ti -> n + size ti) 1 ts
 ;;

 let rec fun_size = function
  | Var _ | State _ -> 0
  | Fun (f,ts) -> List.foldl (fun n ti -> n + size ti) 1 ts
 ;;

 let rec var_size = function
  | Var _ -> 1 | State _ -> 0
  | Fun (f,ts) -> List.foldl (fun n ti -> n + size ti) 0 ts
 ;;

 let rec state_size = function
  | Var _ -> 0 | State _ -> 1
  | Fun (f,ts) -> List.foldl (fun n ti -> n + size ti) 0 ts
 ;;
 
 let min ts =
  let t = List.hd ts and ts = List.tl ts in
  let d = ([t],size t) in
  fst (List.foldl (fun (us,n) ti ->
   let m = size ti in
   if m < n then ([ti],m) else if m = n then (ti::us,n) else (us,n)) d ts)
 ;;
 
 let choose ts =
  let rec value = function
   | Var _ -> 0
   | State q -> State.to_int q
   | Fun (f,ts) -> List.foldl (fun n ti -> n + value ti) 0 ts
  in
  let us = min ts in
  let u = List.hd us in
  let min (ui,n) uj = let m = value uj in if m < n then (uj,m) else (ui,n) in
  fst (List.foldl min (u,value u) (List.tl us)) 
 ;;

 (* Printers *)
 let rec fprintf fmt = function
  | Var x -> Var.fprintf fmt x
  | State p -> State.fprintf fmt p
  | Fun (f,ts) ->
   F.fprintf fmt "@[%a@[(%a)@]@]" Fun.fprintf f (List.fprintf fprintf ",") ts
 ;;
 
 let to_string = F.flush_str_formatter <.> fprintf F.str_formatter;;

 let rec fprintfm fmt = function
  | Var x -> M.fprintf_var fmt x
  | State p -> M.return (State.fprintf fmt p)
  | Fun (f,ts) ->
   F.fprintf fmt "@["; M.fprintf_fun fmt f >>= fun _ ->
   (F.fprintf fmt "@[("; M.fprintf fprintfm "," fmt ts) >>= fun _ ->
   M.return (F.fprintf fmt ")@]@]")
 ;;

 let to_stringm t =
  fprintfm F.str_formatter t >>= (M.return <.> F.flush_str_formatter)
 ;;
end
