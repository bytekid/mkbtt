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

(** Combinator Parser Library
@author Christian Sternagel
@since  Thu Dec  4 08:00:14 CET 2008 *)

(** Provides Functors to create combinator parsers on arbitrary tokens,
using a custom state. *)

(*** MODULES ******************************************************************)
(** {2 Module Pos} *)

(** Positions
@author Christian Sternagel 
@since  Sat Nov 29 15:28:49 CET 2008 *)

(** This module defines parsing positions. *)
module Pos : sig
  type t
  val col  : t -> int
  val file : t -> string option
  val row  : t -> int
end

(** {2 Module Error} *)

(** Errors
@author Christian Sternagel 
@since  Sat Nov 29 15:28:49 CET 2008 *)

(** This modules defines parsing errors. *)
module Error : sig
  type t
  val to_string : t -> string
  val position  : t -> Pos.t
end

(*** MODULES ******************************************************************)
(** {2 Functor Make} *)

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
  (** 
  {3 Standard Monadic Functions for Parsers}
  {ul
  {- [return x] creates a parser that always succeeds and returns the
  result [x] without changing the state. This is useful to lift non-parser
  values into a parser.}
  {- [m >> n] first applies the parser [m], and if that was successful,
  the parser [n] is applied (ignoring the result of [m]).}
  }
  *)

  val (<|>) : 'a t -> 'a t -> 'a t
  (** [p <|> q] does first try the parser [p], if this succeeds or
  at least one token has been consumed by [p], then the combined
  parser behaves exactly like [p]; otherwise the parser [q] is applied.
  To get a more intuitive choice (with exponential complexity) use
  [tempt p <|> q]. Then [q] is always used, whenever [p] fails.
  (see {!val: Parsec.STATEFUL_PARSER.tempt}). *)
  val (<?>) : 'a t -> string -> 'a t
  (** [p <?> d] uses [d] as description of [p] in any error messages
  that may result from applications of [p]. *)
  val any_token : token t
  (** [any] accepts a single token. *)
  val choice : 'a t list -> 'a t
  (** [choice \[p1;...;pN\]] is a generalization of [(<|>)]. *)
  val count : int -> 'a t -> 'a list t
  (** [count n p] aplies the parser [p], [n] times. *)
  val between : 'a t -> 'b t -> 'c t -> 'b t
  (** [between o p c] accepts the same as [p] surrounded by [o] and [c]. *)
  val eoi : unit t
  (** [eoi] is only successful at the end of input. It can be used to
  guarantee that the whole input has been consumed. *)
  val fail : 'a t
  (** [fail] does fail on any input. *)
  val failwith : string -> 'a t
  (** Same as {!val: Parsec.STATEFUL_PARSER.fail} but also specifies an
  error message. *)
  val get_position : Pos.t t
  (** [get_position] returns the current source position as its result. *)
  val get_state : state t
  (** [get_state] returns the current state as result. *)
  val lookahead : 'a t -> 'a t
  (** [lookahead p] is the same as [p] except that no input is actually
  read, i.e., the internal state of the parser is the same as before
  applying [p]. *)
  val many : 'a t -> 'a list t
  (** [many p] applies [p] as often as possible and collects the
  results in a list. *)
  val many1 : 'a t -> 'a list t
  (** [many1 p] is the same as [many p] except that [p] has to
  accept at least once. *)
  val many_till : 'a t -> 'e t -> 'a list t
  (** [many_till p e] parses occurrences of [p] until [e] matches. *)
  val option : 'a -> 'a t -> 'a t
  (** [option d p] tries to apply [p], if succesful it returns its
  result, otherwise, the default value [d] is returned. *)
  val optional : 'a t -> unit t
  (** [optional p] applies [p] if possible and does nothing otherwise. *)
  val run : ?file:string -> 'a t -> state -> input -> (Error.t,'a)Util.either
  (** [run p s n i] applies the parser [p] to the input [i] using the initial
  state [s]. For error messages the file name [n] is used. *)
  val sep_by : 'a t -> 'b t -> 'a list t
  (** [sep_by p s] uses [s] as separator between [p] parses. The
  result is the list of results of [p] parses. *)
  val sep_by1 : 'a t -> 'b t -> 'a list t
  (** Same as [sep_by] but parses at least one list element. *)
  val sep_end_by : 'a t -> 'b t -> 'a list t
  (** Same as [sep_by] but optionally ended by a separator. *)
  val set_position : Pos.t -> unit t
  (** [set_position p] sets the current position to [p]. *)
  val set_state : state -> unit t
  (** [set_state s] sets the current state to [s]. *)
  val skip_many : 'a t -> unit t
  (** [skip_many p] skips as many occurences of [p] as possible. *)
  val skip_many1 : 'a t -> unit t
  (** Same as [skip_may p] but skips at least one occurrence of [p]. *)
  val tempt : 'a t -> 'a t
  (** [tempt p] behaves exactly like [p] on success, on failure, however,
  it pretends to not have read any input. This is a potential source of
  inefficiency, but makes writing parsers easier
  (see {!val: Parsec.STATEFUL_PARSER.(<|>)}). *)
  val token : (token -> Pos.t) -> (token -> 'a option) -> 'a t
  (** [token pos tok] results in a parser that uses [tok] to get the next
  token from the input and [pos] to update the internal position. *)
  val unexpected : string -> 'a t
  (** [unexpected msg] always fails with message [msg]. *)
  val update_state : (state -> state) -> state t
  (** [update_state f] returns the current state as its result and
  generates the next state by applying [f]. *)
end

module type PARSER = sig
  include STATEFUL_PARSER with type state = unit

  val parse : 'a t -> input -> (Error.t,'a)Util.either
end

(** Generate Parser
@author Christian Sternagel 
@since  Sat Nov 29 15:28:49 CET 2008 *)

module MakeStatefulParser(I : INPUT)(S : Util.Monad.STATE) :
  STATEFUL_PARSER with
  type token = I.Token.t and type input = I.t and type state = S.t

module MakeParser(I : INPUT) : PARSER
  with type token = I.Token.t and type input = I.t

(** {2 Functor Char} *)

module type STATEFUL_CHAR_PARSER = sig
  include STATEFUL_PARSER with type token = char

  val alnum : char t
  (** [alnum] accepts any alphanumeric letter
  (see {!val: Parsec.STATEFUL_CHAR_PARSER.letter} and
  {!val: Parsec.STATEFUL_CHAR_PARSER.digit}). *)
  val any : char t
  (** [any] accepts any character. *)
  val char : char -> char t
  (** [char c] is a parser accepting only the character [c]. *)
  val digit : char t
  (** [digit] accepts only digits (from ['0'] to ['9']). *)
  val letter : char t
  (** [letter] accepts only letters
  (see {!val: Parsec.STATEFUL_CHAR_PARSER.lower} and
  {!val: Parsec.STATEFUL_CHAR_PARSER.upper}). *)
  val lex : 'a t -> 'a t
  (** [lex p] accepts the same input as [p] and consumes trailing spaces. *)
  val lower : char t
  (** [lower] accepts only lowercase letters (from ['a'] to ['z']). *)
  val nl : char t
  (** [nl] accepts only a new-line character (['\n']). *)
  val noneof : string -> char t
  (** [noneof s] accepts any single character, except those contained in
  [s]. *)
  val notchar : char -> char t
  (** [notchar c] accepts any character except [c]. *)
  val oneof : string -> char t
  (** [oneof s] accepts only a character that is contained in [s]. *)
  val quoted : char -> char -> char list t
  (** [quoted esc c] reads everything between two [c] characters (where even
  a [c] character may occur if escaped by an [esc]. *)
  val sat : (char -> bool) -> char t
  (** [sat p] accepts any character satisfying [p]. *)
  val space : char t
  (** [space] accepts a single blank ([' ']). *)
  val spaces : unit t
  (** [spaces] accepts an arbitrary number of white-spaces
  ([' '], ['\t'], ['\n'], ['\r']). *)
  val string : string -> char list t
  (** [string s] accepts the given string and returns it. *)
  val tab : char t
  (** [tab] accepts a single tab (['\t']). *)
  val test : 'a t -> state -> string -> 'a
  (** [test p s i] runs the parser [p] with initial state [s] on input [i].
  @raise Failure [m] if parsing is not successful. *)
  val upper : char t
  (** [upper] accepts only uppercase letters (from ['A'] to ['Z']). *)
end

module type CHAR_PARSER = sig
  include STATEFUL_CHAR_PARSER with type state = unit
  
  val parse : 'a t -> input -> (Error.t,'a)Util.either
end

module StringInput : CHAR_INPUT
module CharListInput : CHAR_INPUT
module LazyCharListInput : CHAR_INPUT

(** Generate Character Parser
@author Christian Sternagel 
@since  Sat Nov 29 15:28:49 CET 2008 *)

module MakeStatefulCharParser(I : CHAR_INPUT)(S : Util.Monad.STATE) :
  STATEFUL_CHAR_PARSER with type state = S.t and type input = I.t

module MakeCharParser(I : CHAR_INPUT) : CHAR_PARSER
  with type input = I.t

module MakeStringParser(S : Util.Monad.STATE) :
  STATEFUL_CHAR_PARSER
  with type state = S.t and type input = StringInput.t

module MakeCharListParser(S : Util.Monad.STATE) :
  STATEFUL_CHAR_PARSER
  with type state = S.t and type input = CharListInput.t

module MakeLazyCharListParser(S : Util.Monad.STATE) :
  STATEFUL_CHAR_PARSER
  with type state = S.t and type input = LazyCharListInput.t

module StringParser : CHAR_PARSER with type input = StringInput.t
module CharListParser : CHAR_PARSER with type input = CharListInput.t
module LazyCharListParser : CHAR_PARSER with type input = LazyCharListInput.t

module Xml : sig
  type attrs = (string * string)list
  (** XML attributes are stored as a key-value lists. *)
  type node  = {
    id : string;
    attrs : attrs;
    succs : node list;
    text : string option;
  };;
  (** The type of XML nodes, consisting of a tag name [id],
  a list of attributes [attrs], a list of child nodes [succs],
  and contained text [text] (possibly empty). *)
  type meta = (string * attrs)list
  (** Meta information for an XML document. *)
  type t    = (meta * node)
  (** An XML document consists of a (possibly empty) header containing
  meta information together with a single document root. *)

  module MakeParser(P : CHAR_PARSER) : sig
    module Lexer : sig
      module Token : TOKEN
      type input
    
      val tokenize : input -> (Error.t,Token.t list)Util.either
    end
    val parse : P.input -> (Error.t,t)Util.either
  end

  module type TRANSFORMER = sig
    type 'a t
    (** The type of XML transformers. *)
    type state
    (** A user-supplied state that is maintained during the transformation. *)

    (** {3 Basic Transformers} *)
    val (<?) : 'a t -> string -> 'a t
    (** [t <? msg] accepts the same input as [t], but in case of an error it
    prepends [msg] to the error message of [t]. *)
    val (<?>) : string -> 'a t -> 'a t
    (** [tag <?> t] fixes the transformer [t] to only succeed on nodes having
    tag [tag]. *)
    val (<*>) : unit t
    (** [(<*>)] (don't forget the parenthesis) is a wild card transformation
    accepting anything and returning nothing. *)
    val any : 'a t -> (string * attrs * string option * 'a)t
    (** [any args] succeds on any node if [args] is able to parse it's children.
    The result consists of the node's tag, it's attributes, contained text
    (possibly empty), and the result of parsing it's child nodes. *)
    val bool : bool t
    (** [bool] only succeeds on leaf nodes containing the string ["true"] or
    the string ["false"] and returns the corresponding Boolean value. *)
    val choice : 'a t list -> 'a t
    (** [choice ts] tries to apply the elements of [ts] from left to right.
    The first transformation that succeeds, delivers the result. *)
    val empty : unit t
    (** [empty] does only succeed on an empty list of nodes. *)
    val fail : string -> 'a t
    (** [fail msg] fails with message [msg]. *)
    val int : int t
    (** [int] only succeeds on leaf nodes containing some integer and returns
    it's value. *)
    val leaf : (string * attrs * string option)t
    (** [leaf] only succeeds on leaf nodes (i.e., having no children).
    The result consists of the node's tag, it's attributes, and
    contained text (possibly empty). *)
    val many : 'a t -> 'a list t
    (** [many t] applies the transformation [t] as often as possible. *)
    val node : 'a t -> 'a t
    (** [node args] is similar to {!val: Parsec.Xml.TRANSFORMER.any}, but only
    returns the result of transforming the current node's children. *)
    val option : 'a t -> 'a option t
    (** [option t] accepts the same input as [t]. If [t] failes [None] is
    returned. *)
    val string : string t
    (** [string] extracts the text contents of a leaf node. *)

    (** {3 Monad Combinators} *)
    val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
    val (>>) : 'a t -> 'b t -> 'b t
    val return : 'a -> 'a t
    
    (** {3 State Manipulations} *)
    val get_state : state t
    val set_state : state -> unit t

    (** {3 Applying a Transformation} *)
    val run : 'a t -> state -> node -> (string,'a)Util.either
    (** [run t s xml] applies transformation [t] with initial state [s] to
    the XML node [xml]. On success, it returns [Right x], where [x] is the
    result of the transformation. On error, it returns [Left msg], where
    [msg] is an error message. *)
  end

  module Transformer(S : Util.Monad.STATE) : TRANSFORMER
    with type state = S.t
end
