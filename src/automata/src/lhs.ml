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
 module F = Format;;
 module Fun = R.Function;;
 module M = Monad.Make (R);;
 module P = Parser.Make (R);;
 module S = State;;
 module Sig = R.Signature;;
 module T = Term.Make (R);;

 (*** OPENS *******************************************************************)
 open Util;;
 
 (*** TYPES *******************************************************************)
 type 'a m = 'a M.t;;
 type 'a p = 'a P.t;;
 type term = T.t;;
 type t = State of S.t | Fun of Fun.t * S.t list;;
 
 (*** FUNCTIONS ***************************************************************)
 (* Parsers *)
 let (>>=) = P.(>>=);;
 let (>>) = P.(>>);;
 let (<?>) = P.(<?>);;
 let (<|>) = P.(<|>);;

 let parse ps =
  let id = (P.many1 (P.noneof " \t\n\r()\",") >>= function
   | ['-';'>'] | ['=';'='] | ['-';'>';'='] | ['-';'>';'<';'-'] -> P.fail
   | i -> P.return i) <?> "identifier"
  in
  let create_fun n a =
   P.get_state >>= fun (s,status) ->
   let (f,s) = Sig.create_fun a n s in P.set_state (s,status) >> P.return f
  in
  let create_state n =
   P.get_state >>= fun (s,status) ->
   try
    let (p,status) = Status.create_state (int_of_string n) status in
    P.set_state (s,status) >> P.return p
   with Failure _ -> P.fail
  in
  let parse_state = P.lex id >>= (create_state <.> String.of_char_list) in
  P.lex id >>= fun i ->
  let n = String.of_char_list i in
  (P.lex (P.char '(') >>
   P.sep_by parse_state (P.lex (P.char ',')) >>= fun ps ->
   create_fun n (List.length ps) >>= fun f ->
   P.lex (P.char ')') >> P.return (Fun (f,ps)))
  <|>
  (if List.mem n ps then create_state n >>= fun p -> P.return (State p)
  else create_fun n 0 >>= fun f -> P.return (Fun (f,[])))
 ;;

 let of_string ps input =
  M.(>>=) M.get (fun s ->
  let m = parse ps >>= fun l -> P.get_state >>= (P.return <.> Pair.make l) in
  match P.run m s (Parsec.StringInput.of_string input) with
   | Left e -> M.fail (Parsec.Error.to_string e)
   | Right (l,s) -> M.(>>) (M.set s) (M.return l))
 ;;

 let (>>=) = M.(>>=);;

 (* Constructors and Destructors *)
 let make_state p = State p;;
 let make_fun f ps = Fun (f,ps);;
 
 let of_term = function
  | T.Var _ -> failwith "illegal term"
  | T.State q -> State q
  | T.Fun (f,ts) ->
   let map = function T.State q -> q | _ -> failwith "illegal term" in
   Fun (f,List.map map ts)
 ;;
 
 let to_term = function
  | State q -> T.State q
  | Fun (f,ps) -> T.Fun (f,List.map T.make_state ps)
 ;;
 
 (* Term Symbols *)
 let states = function State p -> [p] | Fun (_,ps) -> ps;;
 let root = function State _ -> None | Fun (f,_) -> Some f;;

 (* Properties *)
 let is_fun = function Fun _ -> true | _ -> false;;
 let is_state = function State _ -> true | _ -> false;;
 
 (* Miscellaneous *)
 let args = function State _ -> [] | Fun (_,ps) -> ps;;
 let hash = Hashtbl.hash;;
 
 (* Compare Functions *)
 let compare = compare;;
 let equal l l' = compare l l' = 0;;
 
 (* Printers *)
 let fprintf fmt = function
  | State p -> S.fprintf fmt p
  | Fun (f,ps) ->
   F.fprintf fmt "@[%a@[(%a)@]@]" Fun.fprintf f (List.fprintf S.fprintf ",") ps
 ;;
 
 let to_string = F.flush_str_formatter <.> fprintf F.str_formatter;;

 let fprintfm fmt = function
  | State p -> M.return (S.fprintf fmt p)
  | Fun (f,ps) ->
   F.fprintf fmt "@["; M.fprintf_fun fmt f >>= fun _ ->
   M.return (F.fprintf fmt "@[(%a)@]@]" (List.fprintf S.fprintf ",") ps)
 ;;

 let to_stringm t =
  fprintfm F.str_formatter t >>= (M.return <.> F.flush_str_formatter)
 ;;
end
