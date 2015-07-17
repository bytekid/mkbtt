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
module Perv = Pervasives;;
module Pf = Printf;;

module Pos = struct
 type t = { name : string option; col : int; row : int };;
 let initial name = { name = name; col = 0; row = 1 };;
 let to_string p = match p.name with
   | Some n -> Pf.sprintf "File \"%s\", line %i, character %i" n p.row p.col
   | None   -> Pf.sprintf "line %i, character %i" p.row p.col
 ;;
 let update_char p = function
   | '\n' -> { p with row = p.row + 1; col = 1 }
   | '\t' -> { p with col = p.col + 8 - ((p.col - 1) mod 8) }
   | _    -> { p with col = p.col + 1 }
 ;;
 let update_char_list p cs = List.foldl update_char p cs;;
 let col p = p.col;;
 let row p = p.row;;
 let file p = p.name;;
end

module Error = struct
 type msg = Expecting of string
          | Message of string
          | SysUnexpected of string
          | Unexpected of string
 ;;

 type t = Pos.t * msg list;;

 let position (p,_) = p

 let merge (p,ms) (_,ns) = (p,ms @ ns);;
 let unknown p = (p,[]);;
 let message msg p = (p,[Message msg]);;
 let sys_unexpected msg p = (p,[SysUnexpected msg]);;
 let unexpected msg p = (p,[Unexpected msg]);;
 let m2i = function
   | SysUnexpected _ -> 0
   | Unexpected _    -> 1
   | Expecting _     -> 2
   | Message _       -> 3
 ;;
 
 let m2s =
   function SysUnexpected s | Unexpected s | Expecting s | Message s -> s
 ;;
 
 let compare m n = Perv.compare (m2i m) (m2i n);;
 let equals m n = compare m n = 0;;
 
 let add_msg m (p,ms) = (p,m::ms);;
 let set_msg m (p,ms) = (p,m::List.filter (fun n -> not(equals m n)) ms);;
 let set_pos p (_,ms) = (p,ms);;
 
 let set_expecting e = function
  | []    -> set_msg (Expecting "") e
  | [msg] -> set_msg (Expecting msg) e
  | m::ms ->
   List.foldr (fun m -> add_msg (Expecting m)) (set_msg (Expecting m) e) ms
 ;;
 
 let is_unknown (_,ms) = ms = [];;
 
 let show_messages ms = if ms = [] then "unknown parse error" else (
   let (sysue,m1) = List.span (equals(SysUnexpected "")) ms in
   let (ue,m2)    = List.span (equals(Unexpected "")) m1 in
   let (e,msgs)   = List.span (equals(Expecting "")) m2 in
   let clean      = List.unique_hash <.> List.filter ((<>)"") in
   let comma_sep  = String.concat ", " <.> clean in
   let commas_or  = function [] -> "" | [x] -> x | xs -> (
     comma_sep(List.take (List.length xs - 1) xs) ^ " or " ^ List.last xs
   ) in
   let show_many pre = function [] -> "" | xs -> (
     ((fun xs -> pre^" "^commas_or xs) <.> clean <.> List.map m2s) xs
   ) in
   let se     = show_many "expecting" e in
   let sue    = show_many "unexpected" ue in
   let sm     = show_many "" msgs in
   let ssysue = if ue<>[] || sysue = [] then "" else (
     let fstmsg = m2s(List.hd sysue) in
     if fstmsg = "" then "unexpected end of input"
                    else "unexpected " ^ fstmsg
   ) in
   String.concat "\n" (clean[ssysue;sue;se;sm])
  );;
 
 let to_string(p,m) = Pos.to_string p^": "^show_messages(List.sort compare m);;
end

(*** MODULE TYPES *************************************************************)
module type TOKEN = sig
  type t
  val to_string : t -> string
end

module type INPUT = sig
  type t
  module Token : TOKEN
  val is_empty : t -> bool
  val next : t -> Token.t
  val of_list : Token.t list -> t
  val rest : t -> t
  val singleton : Token.t -> t
  val to_list : t -> Token.t list
  val to_string : t -> string
end

module type CHAR_INPUT = sig
  include INPUT with type Token.t = char
  val filename : t -> string option
  val of_channel : in_channel -> t
  val of_file : string -> t
  val of_string : string -> t
end

module type STATEFUL_PARSER = sig
  type token
  type input
  type state
 
  include Util.Monad.SIGNATURE
 
  val (<|>) : 'a t -> 'a t -> 'a t
  val (<?>) : 'a t -> string -> 'a t
  val any_token : token t
  val choice : 'a t list -> 'a t
  val count : int -> 'a t -> 'a list t
  val between : 'a t -> 'b t -> 'c t -> 'b t
  val eoi : unit t
  val fail : 'a t
  val failwith : string -> 'a t
  val get_position : Pos.t t
  val get_state : state t
  val lookahead : 'a t -> 'a t
  val many : 'a t -> 'a list t
  val many1 : 'a t -> 'a list t
  val many_till : 'a t -> 'e t -> 'a list t
  val option : 'a -> 'a t -> 'a t
  val optional : 'a t -> unit t
  val run : ?file:string -> 'a t -> state -> input -> (Error.t,'a)either
  val sep_by : 'a t -> 'b t -> 'a list t
  val sep_by1 : 'a t -> 'b t -> 'a list t
  val sep_end_by : 'a t -> 'b t -> 'a list t
  val set_position : Pos.t -> unit t
  val set_state : state -> unit t
  val skip_many : 'a t -> unit t
  val skip_many1 : 'a t -> unit t
  val tempt : 'a t -> 'a t
  val token : (token -> Pos.t) -> (token -> 'a option) -> 'a t
  val unexpected : string -> 'a t
  val update_state : (state -> state) -> state t
end

module type PARSER = sig
  include STATEFUL_PARSER with type state = unit
  val parse : 'a t -> input -> (Error.t,'a)Util.either
end

module type STATEFUL_CHAR_PARSER = sig
  include STATEFUL_PARSER with type token = char
  val alnum : char t
  val any : char t
  val char : char -> char t
  val digit : char t
  val letter : char t
  val lex : 'a t -> 'a t
  val lower : char t
  val nl : char t
  val noneof : string -> char t
  val notchar : char -> char t
  val oneof : string -> char t
  val quoted : char -> char -> char list t
  val sat : (char -> bool) -> char t
  val space : char t
  val spaces : unit t
  val string : string -> char list t
  val tab : char t
  val test : 'a t -> state -> string -> 'a
  val upper : char t
end

module type CHAR_PARSER = sig
  include STATEFUL_CHAR_PARSER with type state = unit
  val parse : 'a t -> input -> (Error.t,'a)Util.either
end

(*** MODULES ******************************************************************)
module MakeListInput(Tok : TOKEN) :
  INPUT with module Token = Tok and type t = Tok.t list = struct
  type t = Tok.t list;;
  module Token = Tok;;

  let is_empty = (=) [];;
  let next = List.hd;;
  let of_list = id;;
  let rest = List.tl;;
  let singleton t = [t];;
  let to_list = id;;
  let to_string = String.concat "" <.> List.map Tok.to_string;;
end

module MakeLazyListInput(Tok : TOKEN) = struct
  type t = Tok.t LazyList.t;;
  
  module Token = Tok;;

  let is_empty = LazyList.null;;
  let next = LazyList.hd;;
  let rest = LazyList.tl;;
  let of_list = LazyList.of_list;;
  let singleton t = lazy(LazyList.Cons(t,LazyList.empty));;
  let token_to_string = Tok.to_string;;
  let to_list t = LazyList.to_list t;;
  let to_string = String.concat "" <.> List.map Tok.to_string <.> to_list;;
end

module StringInput : CHAR_INPUT = struct
  type t = (string option * int * int * string);;

  module Token = struct
    type t = char;;
    let to_string = String.make 1;;
  end

  let filename(f,_,_,_) = f;;
  let is_empty(_,i,last,_) = i = last;;
  let next(_,i,_,s) = s.[i];;
  let of_channel c = let s = read_channel c in (None,0,String.length s,s);;
  let of_file name = let s = read_file name in (Some name,0,String.length s,s);;
  let of_list cs = (None,0,List.length cs,String.of_char_list cs);;
  let of_string s = (None,0,String.length s,s);;
  let rest(f,i,len,s) = (f,i+1,len,s);;
  let singleton t = (None,0,1,Token.to_string t);;
  let to_string(_,i,len,s) = String.sub s i (len-i);;
  let to_list = String.to_char_list <.> to_string;;
end

module CharListInput : CHAR_INPUT = struct
  module L = MakeListInput(struct
    type t = char;;
    let to_string = String.make 1;;
  end);;
  type t = (string option * L.t);;
  module Token = L.Token;;
  let to_string = L.to_string <.> snd;;
  let to_list = L.to_list <.> snd;;
  let singleton c = (None,L.singleton c);;
  let rest(f,d) = (f,L.rest d);;
  let of_list l = (None,L.of_list l);;
  let is_empty = L.is_empty <.> snd;;
  let next = L.next <.> snd;;
  let filename = fst;;
  let of_channel c = (None,String.to_char_list(read_channel c));;
  let of_file n = (Some n,String.to_char_list(read_file n));;
  let of_string s = (None,String.to_char_list s);;
end

module LazyCharListInput = struct
  module L =  MakeLazyListInput(struct
    type t = char;;
    let to_string = String.make 1;;
  end);;
  type t = (string option * L.t);;
  module Token = L.Token;;
  let to_string = L.to_string <.> snd;;
  let to_list = L.to_list <.> snd;;
  let singleton c = (None,L.singleton c);;
  let rest(f,d) = (f,L.rest d);;
  let of_list l = (None,L.of_list l);;
  let is_empty = L.is_empty <.> snd;;
  let next = L.next <.> snd;;
  let filename = fst;;
  let of_channel c = (None,LazyList.of_channel c);;
  let of_file n = (Some n,LazyList.of_file n);;
  let of_string s = (None,LazyList.of_string s);;
end

module MakeStatefulParser(Input : INPUT)(UserState : Monad.STATE) = struct
 (*** Parser definition *******************************************************)
 type token = Input.Token.t;;
 type input = Input.t;;
 type state = UserState.t;;

 (* The condition of the input token list is either pristine' if no token
 has been read, or consumed' if at least one token has been read. *)
 let pristine x = Left x;;
 let consumed x = Right x;;
 let error    x = Left x;;
 let result   x = Right x;;

 module Cond = struct
  type 'a t = ('a,'a)either;;
  let (>>=) m f = either f (consumed <.> Either.strip <.> f) m;;
  let return = pristine;;
 end

 module CondMonad = Monad.Make(Cond);;

 module State = struct
  type t = {
    input : Input.t; (* the remaining tokens *)
    pos   : Pos.t; (* the current position *)
    state : UserState.t; (* a user specified state *)
    error : Error.t (* error information *)
  };;

  let initial id s i = let p = Pos.initial id in
    {input = i; pos = p; state = s; error = Error.unknown p}
  ;;
 end

 module ErrorM = Monad.Transformer.Error(Error)(Cond);;
 module StateM = Monad.Transformer.State(State)(ErrorM);;
 include Monad.Make(StateM);;

 type 'a result = (Error.t,('a * State.t))either Cond.t;;

 let modify = StateM.modify;;
 let update = StateM.update;;

 let merge_errors e : 'a result -> 'a result =
  let f s = {s with State.error = Error.merge e s.State.error} in
  let g = Either.map (Error.merge e) (Pair.cross(id,f)) in
  Either.map g g
 ;;

 (*** run a parser ************************************************************)
 let run ?file p user toks =
  Either.map id fst (Either.strip(p(State.initial file user toks)));;
 
 (*** combinators *************************************************************)
 let (<|>) p q s = either
  (either (fun e -> merge_errors e (q s)) (pristine <.> result))
  consumed
  (p s)
 ;;

 let labels p msgs s = match p s with
  | Left(Left e) -> pristine(error(Error.set_expecting e msgs))
  | Left(Right(x,s)) -> pristine(
   if Error.is_unknown s.State.error then result(x,s) else (
    result(x,{ s with State.error = Error.set_expecting s.State.error msgs })
   )
  )
  | r -> r
 ;;

 let (<?>) p msg = labels p [msg];;

 (*** primitive parsers and combinators ***************************************)
 let tempt p s = either
  (pristine)
  (either
   (pristine <.> error <.> Error.set_pos s.State.pos) (consumed <.> result))
  (p s)
 ;;

 let failwith msg s = pristine(error(Error.message msg s.State.pos));;
 let unexpected msg s = pristine(error(Error.unexpected msg s.State.pos));;
 let fail s = pristine(error(Error.unknown s.State.pos));;

 let many_acc acc p s =
  let rec many_acc xs s = function
   | Left(Left e)  -> result(xs,{ s with State.error = e })
   | Left _        -> Perv.failwith "many' with parser that accepts empty input"
   | Right(Left e) -> error e
   | Right(Right(x,s')) -> many_acc (acc x xs) s' (p s')
  in match p s with
   | Left(Right _) -> Perv.failwith "many' with parser that accepts empty input"
   | Left(Left e)  -> pristine(result([],{ s with State.error = e }))
   | r             -> consumed(many_acc [] s r)
 ;;

 let many p = many_acc cons p >>= fun xs -> return(List.rev xs);;

 let skip_many p = many_acc (fun _ _ -> []) p >> return();;

 let token_prim_ex ns nextpos test = match ns with
  | None -> (fun s -> if Input.is_empty s.State.input then (
    pristine(error(Error.sys_unexpected "" s.State.pos))
  ) else (let c = Input.next s.State.input and cs = Input.rest s.State.input in
   match test c with
    | Some x -> let p = nextpos s.State.pos c cs in
                let s = { s with State.input = cs;
                                 State.pos   = p;
                                 State.error = Error.unknown p } in
                consumed(result(x,s))
    | None ->
     pristine(error(Error.sys_unexpected ("'"^Input.Token.to_string c^"'") s.State.pos))
   )
  )
  | Some ns -> (fun s -> if Input.is_empty s.State.input then (
   pristine(error(Error.sys_unexpected "" s.State.pos))
  ) else (let c = Input.next s.State.input and cs = Input.rest s.State.input in
   match test c with
    | Some x -> let p = nextpos s.State.pos c cs in
                let u = ns s.State.pos c cs s.State.state in
                let s = { State.input = cs;
                          State.pos   = p;
                          State.state = u;
                          State.error = Error.unknown p } in
                consumed(result(x,s))
    | None ->
     pristine(error(Error.sys_unexpected ("'"^Input.Token.to_string c^"'") s.State.pos))
   )
  )
 ;;

 let token_prim nextpos test = token_prim_ex None nextpos test;;

 let token tokpos test =
  let nextpos _ tok input =
    if Input.is_empty input then tokpos tok
                            else tokpos (Input.next input)
  in
  token_prim nextpos test
 ;;

 let tokens show nextpos toks s =
  let ok cs = let p = nextpos s.State.pos toks in
              let s = { s with State.input = cs;
                               State.pos   = p;
                               State.error = Error.unknown p } in
              result(toks,s)
  in
  let eof() = error(
   Error.set_expecting (Error.sys_unexpected "" s.State.pos) [show toks]
  ) in
  let exp c = error(
   Error.set_expecting (Error.sys_unexpected (show(Input.singleton c)) s.State.pos) [show toks]
  ) in
  let rec walk(xs,cs) = 
   if Input.is_empty xs then ok cs else
   if Input.is_empty cs then eof() else
    let c = Input.next cs in
    if Input.next xs = c then walk(Input.rest xs,Input.rest cs)
                         else exp c
  in
  let walk1(xs,cs) =
   if Input.is_empty xs then pristine(ok cs) else
   if Input.is_empty cs then pristine(eof()) else
    let c = Input.next cs in
    if Input.next xs = c then consumed(walk(Input.rest xs,Input.rest cs))
                         else pristine(exp c)
  in
  walk1(toks,s.State.input)
 ;;

 (*** Parser state manipulation ***********************************************)
 let modify_parser_state f = modify (fun s ->
  let s' = f s in { s' with State.error = Error.unknown s'.State.pos })
 ;;

 let update_parser_state f = update (fun s ->
  let s' = f s in { s' with State.error = Error.unknown s'.State.pos })
 ;;

 let get_parser_state = modify_parser_state id;;
 let set_parser_state s = update_parser_state (const s);;
 let get_position = get_parser_state >>= fun s -> return s.State.pos;;
 let get_input = get_parser_state >>= fun s -> return s.State.input;;
 let set_position p = update_parser_state (fun s -> { s with State.pos = p });;
 let set_input i = update_parser_state (fun s -> { s with State.input = i });;

 (*** User state manipulation *************************************************)
 let update_state f =
  modify_parser_state
   (fun s -> {s with State.state = f s.State.state}) >>= fun s ->
  return s.State.state
 ;;

 let get_state = update_state id;;
 let set_state s = update_state (const s) >> return();;

 (*** Combinators *************************************************************)
 let choice ps = List.foldr (<|>) fail ps;;
 let option d p = p <|> return d;;
 let optional p = (p >> return()) <|> return();;
 let between o p c = o >> p >>= fun x -> c >> return x;;
 let skip_many1 p = p >> skip_many p;;
 let many1 p = p >>= fun x -> many p >>= fun xs -> return(x::xs);;
 let sep_by1 p s = p >>= fun x -> many(s>>p) >>= fun xs -> return(x::xs);;
 let sep_by p s = sep_by1 p s <|> return [];;
 let rec sep_end_by1 p s = p >>= (fun x -> 
  (s >> sep_end_by p s >>= fun xs -> return(x::xs))
   <|> return [x]
 ) and sep_end_by p s = sep_end_by1 p s <|> return[];;
 let end_by1 p s = many1(p >>= fun x -> s >> return x);;
 let end_by p s = many(p >>= fun x -> s >> return x);;
 let count n p = if n < 1 then return [] else replicate n p;;
 let chainr1 p op =
  let rec scan p = p >>= fun x -> rest x and
  rest x = (op >>= fun f -> scan p >>= fun y -> return(f x y)) <|> return x in
  scan p
 ;;
 let chainr p op x = chainr1 p op <|> return x;;
 let chainl1 p op =
  let rec rest x = (op >>= fun f -> p >>= fun y -> rest(f x y)) <|> return x in
  p >>= fun x -> rest x
 ;;
 let chainl p op x = chainl1 p op <|> return x;;

 (*** Tricky Combinators ******************************************************)
 let any_token = token_prim (fun p _ _ -> p) Option.some;;
 let not_followed_by p = tempt(
  (p >>= fun c -> unexpected(Input.Token.to_string c)) <|> return()
 );;
 let eoi = not_followed_by any_token <?> "end of input";;
 let lookahead p =
  get_parser_state   >>= fun s ->
  p                  >>= fun x ->
  set_parser_state s >>
  return x
 ;;
 let many_till p e =
  let rec scan e =
   (lookahead e >> return[])
   <|> (p >>= fun x -> scan e >>= (return <.> cons x))
  in scan e
 ;;
end

module MakeParser(Input : INPUT) = struct
  include MakeStatefulParser(Input)(struct type t = unit end);;
  let parse p i = run p () i;;
end

module MakeStatefulCharParser(Input : CHAR_INPUT)(UserState : Monad.STATE) 
 : STATEFUL_CHAR_PARSER
 with type input = Input.t and type state = UserState.t = struct
 include MakeStatefulParser(Input)(UserState);;

 module C = Char;;

 (*** Primitive character parsers *********************************************)
 let sat p = token_prim
  (fun p c _ -> Pos.update_char p c)
  (fun c -> if p c then Some c else None)
 ;;
 
 let string s = (
  let toks = Input.of_list(String.to_char_list s) in
  tokens
    Input.to_string
    (fun p -> Pos.update_char_list p <.> Input.to_list)
    toks >>= fun s -> return(Input.to_list s)
 ) <?> (Pf.sprintf "'%s'" s);;

 (*** Character parsers *******************************************************)
 let any = sat(const true) <?> "any character";;
 let char c = sat ((=)c) <?> Pf.sprintf "'%c'" c;;
 let notchar c = sat ((<>)c) <?> (Pf.sprintf "not the character '%c'" c);;
 let oneof s = sat (String.contains s);;
 let noneof s = sat (not <.> String.contains s);;
 let space = sat C.is_space <?> "a space";;
 let spaces = skip_many space <?> "some white spaces"
 let nl = char '\n' <?> "a new-line";;
 let tab = char '\t' <?> "a tab";;
 let upper = sat C.is_upper <?> "an uppercase letter";;
 let lower = sat C.is_lower <?> "a lowercase letter";;
 let letter = sat C.is_alpha <?> "a letter";;
 let lex p = p >>= fun x -> spaces >> return x;;
 let alnum = sat C.is_alnum <?> "a letter or a digit";;
 let digit = sat C.is_digit <?> "a digit";;
 let quoted esc c =
   char c >> many_till ((char esc >> any) <|> notchar c) (char c);;

 let test p state input = match run p state (Input.of_string input) with
   | Left e  -> Perv.failwith(Error.to_string e)
   | Right r -> r
 ;;
end

module MakeCharParser(Input : CHAR_INPUT) = struct
  include MakeStatefulCharParser(Input)(struct type t = unit end);;
  let parse p i = run ?file:(Input.filename i) p () i;;
end

module MakeStringParser = MakeStatefulCharParser(StringInput);;
module MakeCharListParser = MakeStatefulCharParser(CharListInput);;
module MakeLazyCharListParser = MakeStatefulCharParser(LazyCharListInput);;
module StringParser = MakeCharParser(StringInput);;
module CharListParser = MakeCharParser(CharListInput);;
module LazyCharListParser = MakeCharParser(LazyCharListInput);;

module Xml = struct

  type attrs = (string * string)list;;
  type node  = {
    id : string;
    attrs : attrs;
    succs : node list;
    text : string option;
  };;
  type meta = (string * attrs)list;;
  type t    = (meta * node);;

  let mk_node id attrs succs text = {
    id = id; attrs = attrs; succs = succs; text = text;
  };;

  module MakeLexer(P : CHAR_PARSER) = struct
    open P;;

    module Token = struct
      type token = Close of string
                 | Comment
                 | Empty of (string * attrs)
                 | Meta of (string * attrs)
                 | Open of (string * attrs)
                 | Text of string
      ;;

      (* the position at which the token ends + the token *)
      type t = (Pos.t * token);;

      let to_string(_,t) = match t with
        | Meta(id,_)  -> id
	      | Text _      -> "text node"
	      | Empty(id,_) -> "empty tag <"^id^"/>"
	      | Open(id,_)  -> "opening tag <"^id^">"
	      | Close id    -> "closing tag </"^id^">"
	      | Comment     -> "comment"
      ;;
    end

    type input = P.input;;

    let is_id_letter c = Char.is_alpha c || c = '-' || c = ':';;

    let id_letter = P.sat is_id_letter;;

    let ident m = (
      m id_letter >>= fun id ->
      spaces      >>
      return id
    ) <?> "identifier"
    ;;

    let value = between
      (char '"')
      (many(notchar '"'))
      (char '"' <?> "end of attribute value: '\"'")
    ;;

    let attr =
      ident many1 >>= fun key ->
      char '='    >>
      spaces      >>
      value       >>= fun value ->
      spaces      >>
      return(String.of_char_list key,String.of_char_list value)
    ;;
      
    let attrs = many attr;;

    let tag =
      char '<'     >>
      any          >>= fun c ->
      if c = '/' then (
        ident many1  >>= fun id ->
	      char '>'     >>
	      get_position >>= fun e ->
	      return(e,Token.Close(String.of_char_list id))
      ) else if c = '?' then (
        ident many1  >>= fun id ->
	      attrs        >>= fun attrs ->
	      (char '?' >> char '>') <?> "end of meta tag: '?>'" >>
	      get_position >>= fun e ->
	      return(e,Token.Meta(String.of_char_list id,attrs))
      ) else if c = '!' then (
        string "--"                >>
	      many_till any (string"--") >>= fun _ ->
	      char '>'                   >>
	      get_position               >>= fun e ->
	      return(e,Token.Comment)
      ) else (
        ident many >>= fun cs ->
	      let id = c::cs in
	      attrs      >>= fun attrs ->
	      any        >>= fun c ->
	      if c = '/' then (
	        char '>'     >>
          get_position >>= fun e ->
	        return(e,Token.Empty(String.of_char_list id,attrs))
	      ) else (
	        get_position >>= fun e ->
	        if c = '>' then return(e,Token.Open(String.of_char_list id,attrs))
	                   else fail
	      )
      )
    ;;
    let tag = (tag >>= fun t -> spaces >> return t);;

    let text =
      let symb = noneof "<>" in
      get_position       >>= fun s ->
      many1(symb)        >>= fun cs ->
      get_position       >>= fun e ->
      return(e,Token.Text(String.of_char_list cs))
    ;;

    let lexer =
      spaces             >>
      many(tag <|> text) >>= fun r ->
      spaces             >>
      eoi                >>
      return r
    ;;

    let tokenize = parse lexer;;
  end

  module MakeParser(P : CHAR_PARSER) = struct
    module Lexer = MakeLexer(P);;
    module Tok   = Lexer.Token;;
    module Input = MakeListInput(Lexer.Token);;
    include MakeParser(Input);;

   let token p = token_prim (fun _ (pos,_) _ -> pos) (fun (_,c) -> p c);;

   let text = token(function Tok.Text txt -> Some txt | _ -> None);;
   let tago = token(function Tok.Open(id,attrs) -> Some(id,attrs) | _ -> None);;
   let tagc = token(function Tok.Close id -> Some id | _ -> None);;
   let tage = token(function Tok.Empty(id,attrs) -> Some(id,attrs) | _ -> None);;
   let meta = token(function Tok.Meta(id,attrs) -> Some(id,attrs) | _ -> None);;
   let cmmt = token(function Tok.Comment -> Some() | _ -> None);;

   let text = option None (text >>= fun r -> return(Some r));;

   let skip_comments = skip_many cmmt;;

   let rec node() = (
    (tage >>= fun(id,attrs) -> return(mk_node id attrs [] None))
    <|>
    (
      tago          >>= fun(id,attrs) ->
      skip_comments >>
      text          >>= fun txt ->      
      skip_comments >>
      many(node())  >>= fun chs ->
      skip_comments >>
      tagc          >>= fun id' ->
      if id = id' then return(mk_node id attrs chs txt)
                  else failwith("open tag <"^id^"> closed by tag </"^id'^">")
    )
   ) <?> "xml node";;

   let node = node() >>= fun r -> skip_comments >> return r;;

   let header = many(meta >>= fun m -> skip_comments >> return m) <?> "header";;

   let doc =
     skip_comments >>
     header        >>= fun header ->
     node          >>= fun doc ->
     eoi           >>
     return(header,doc)
   ;;

   let parse input = match Lexer.tokenize input with
     | Util.Right toks -> parse doc (Input.of_list toks)
     | Util.Left e     -> Perv.failwith(Error.to_string e)
   ;;

  end

  let rec fpf_attrs fmt = function
    | (k,v)::kvs -> Format.fprintf fmt " %s=\"%s\"%a" k v fpf_attrs kvs
    | _          -> ()
  ;;

  let fpf_text fmt = function
    | Some cs -> Format.fprintf fmt "%s" cs
    | None    -> ()
  ;;

  let rec fpf_nodes fmt = function
    | [n]   -> Format.fprintf fmt "@[%a@]" fprintf n
    | n::ns -> Format.fprintf fmt "%a%a" fpf_nodes [n] fpf_nodes ns
    | _     -> ()
  and fprintf fmt n =
    Format.fprintf fmt "@{<%s%a>%a%a@}"
      n.id fpf_attrs n.attrs fpf_text n.text fpf_nodes n.succs
  ;;

  let to_string n =
    let module F = Format in
    let module S = String in
    let buff = Buffer.create(1024) in
    let bfmt = F.formatter_of_buffer buff in
    F.pp_set_tags bfmt true;
    F.pp_set_formatter_tag_functions bfmt {
      F.mark_open_tag   = (fun s -> "<"^s^">");
      F.mark_close_tag  = (fun s ->
        "</"^(if S.contains s ' ' then S.sub s 0 (S.index s ' ')
                                  else s)^">"
      );
      F.print_open_tag  = const();
      F.print_close_tag = const();
    };
    F.fprintf bfmt "%a@?" fprintf n;
    Buffer.contents buff
  ;;
  
  module type TRANSFORMER = sig
    type 'a t
    type state

    val (<?) : 'a t -> string -> 'a t
    val (<?>) : string -> 'a t -> 'a t
    val (<*>) : unit t
    val any : 'a t -> (string * attrs * string option * 'a)t
    val bool : bool t
    val choice : 'a t list -> 'a t
    val empty : unit t
    val fail : string -> 'a t
    val int : int t
    val leaf : (string * attrs * string option)t
    val many : 'a t -> 'a list t
    val node : 'a t -> 'a t
    val option : 'a t -> 'a option t
    val string : string t

    val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
    val (>>) : 'a t -> 'b t -> 'b t
    val return : 'a -> 'a t
    
    val get_state : state t
    val set_state : state -> unit t

    (** {3 Applying a Transformation} *)
    val run : 'a t -> state -> node -> (string,'a)Util.either
  end

  module Transformer(State : Monad.STATE) = struct
    let to_s = to_string;;

    type s = {input : node list; state : State.t};;
    module S = struct
      type t = s;;
    end

    module E = Monad.Error;;
    module M = Monad.Transformer.State(S)(E);;
    type 'a t = 'a M.t;;
    type state = State.t;;

    let (>>=) = M.(>>=);;
    let (>>) = M.(>>);;
    let return = M.return;;
    let (|=) m f = m >>= (return <.> f);;
    let fail e = M.liftm(Monad.Error.fail e);;
    let catch h m s = Monad.Error.catch (flip h s) (m s);;
    let failif b e = M.liftm(Monad.Error.failif b e);;
    let (<?) m msg = catch (fun e -> fail(msg^"\n"^e)) m;;
    let err_empty() = fail "unexpected end of nodes";;
    let run t s node = E.run(M.run {input = [node];state = s} t);; 

    let empty = M.get >>= fun s -> match s.input with
      | []    -> return()
      | n::_  -> fail("unexpected node "^to_s n^"\nexpecting end of nodes")
    ;;

    let (!$) m = m >>= fun x -> empty >> return x;;

    let any args = (M.get >>= fun s -> match s.input with
      | n::ns -> (
        M.set{s with input = n.succs} >>
        (!$ args)                     >>= fun x ->
        M.get                         >>= fun s' ->
        M.set{s' with input = ns}     >>
        return(n.id,n.attrs,n.text,x)
      ) <? ("failed to transform arguments of\n"^to_s n)
      | _ -> err_empty()
    );;

    let (<*>) = M.get >>= fun s -> M.set{s with input = []} >> return();;

    let rec many m = catch(const(return[])) (m >>= fun x -> many m |= (List.cons x));;

    let node args = any args |= (fun(_,_,_,x) -> x);;

    let tag t m = (M.get >>= fun s -> match s.input with
      | n::_ -> if n.id = t then m else fail("unexpected node "^to_s n^"\nexpecting tag '"^t^"'")
      | _    -> err_empty()
    );;
    let (<?>) t m ns = tag t m ns;;

    let leaf = (any empty |= fun(i,a,t,_) -> (i,a,t)) <? "failed to transform leaf node";;

    let string = (leaf >>= function
      | (_,_,Some txt) -> return txt
      | (id,_,_)       -> fail("expecting text inside '"^id^"'")
    ) <? "failed to transform string";;

    let string2bool s =
      if s = "true" then return true else if s = "false" then return false else (
        fail("cannot convert '"^s^"' into Boolean")
      )
    ;;
    let bool = (string >>= string2bool) <? "failed to transform Boolean";;
    
    let string2int s = try
      return(int_of_string s)
    with Failure "int_of_string" -> fail("cannot convert '"^s^"' into int")
    ;;
    let int = (string >>= string2int) <? "failed to transform integer";;

    let rec choice = function
      | [m]   -> m <? "last choice failed"
      | m::ms -> catch (fun _ -> choice ms) m
      | _     -> fail "empty list of choices"
    ;;
    (*
    let rec choice ms s = match ms with
      | [m] -> (m <? "last choice failed") s
      | m :: ms -> catch (fun _ -> choice ms) m s
      | _ -> fail "empty list of choice" s
    ;;
    *)

    let option m = catch (const(return None)) (m |= Option.some);;

    (* State Manipulation *)
    let get_state = M.get |= function{state = s} -> s;;
    let set_state s = M.get >>= fun t -> M.set{t with state = s};
  end
end
