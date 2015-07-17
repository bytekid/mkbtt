(* Copyright 2010 Sarah Winkler
 * GNU Lesser General Public License
 *
 * This file is part of MKBtt.
 * 
 * MKBtt is free software: you can redistribute it and/or modify it under
 * the terms of the GNU Lesser General Public License as published by the
 * Free Software Foundation, either version 3 of the License, or (at your
 * option) any later version.
 * 
 * MKBtt is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public
 * License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public
 * License along with MKBtt. If not, see <http://www.gnu.org/licenses/>.
 *)

 (*** MODULES *****************************************************************)
 module Function : Rewriting.FUNCTION
 module Variable : Rewriting.VARIABLE

module Label : sig
  (*** MODULES ****************************************************************)
  module F : Util.Index.ISOMORPHIC with
   type key = Function.t and type element = string
 
  module V : Util.Index.ISOMORPHIC with
   type key = Variable.t and type element = string

  (*** TYPES ******************************************************************)
  type signature = F.t * V.t
  type theory = A | C | AC
  type t
  
  (*** VALUES *****************************************************************)
  (** {3 Constructors and Destructors} *)

  val add_rlab : Function.t -> t -> t
  (** [add_rlab f l] adds [f] to the label [l] if [l] is a root label.
  @raise Failure "unknown label" if [l] is not a root label. *)
  val incr_curry : t -> t
  (** [incr_curry l] increments the value of [l] if [l] is a curry label.
  @raise Failure "unknown label" if [l] is not a curry label. *)
  val make_curry : int -> t
  (** [make_curry n] creates a curry label with value [n]. *)
  val make_dp : t
  (** [make_dp] creates a dp label. *)
  val make_height : int -> t
  (** [make_height n] creates a height label with value [n]. *)
  val make_rlab : Function.t list -> t
  (** [make_rlab fs] creates a root label with value [fs]. *)
  val make_slab : int -> t
  (** [make_slab n] creates a semantic label with value [n]. *)
  val make_theory : theory -> t
  (** [make_theory t] creates a theory label. *)

  (** {3 Search Functions} *)

  val get_curry : t -> int
  (** [get_curry l] returns the value of [l] if [l] is a curry label.
  @raise Failure "unknown label" if [l] is not a curry label. *)
  val get_height : t -> int
  (** [get_height l] returns the value of [l] if [l] is a height label.
  @raise Failure "unknown label" if [l] is not a height label. *)
  val get_rlab : t -> Function.t list
  (** [get_rlab l] returns the value of [l] if [l] is a root label.
  @raise Failure "unknown label" if [l] is not a root label. *)
  val get_slab : t -> int
  (** [get_slab l] returns the value of [l] if [l] is a semantic label.
  @raise Failure "unknown label" if [l] is not a semantic label. *)
  val get_theory : t -> theory
  (** [get_theory l] returns the theory label of [l].
  @raise Failure "unknown label" if [l] is not a semantic label. *)

  (** {3 Properties} *)

  val is_curry : t -> bool
  (** [is_curry l] checks if [l] is a curry label. *)
  val is_dp : t -> bool
  (** [is_dp l] checks if [l] is a dp label. *)
  val is_height : t -> bool
  (** [is_height l] checks if [l] is a height label. *)
  val is_rlab : t -> bool
  (** [is_rlab l] checks if [l] is a root label. *)
  val is_slab : t -> bool
  (** [is_slab l] checks if [l] is a semantic label. *)
  val is_theory : theory -> t -> bool
  (** [is_theory l] checks if [l] is a [theory] label. *)
  val is_some_theory : t -> bool
  (** [is_some_theory l] checks if [l] is a theory label. *)

  (** {3 Miscellaneous} *)

  val copy : t -> t
  (** [copy l] returns a copy of [l]. *)
  val hash : t -> int
  (** [hash l] returns the hash value of [l]. *)

  (** {3 Compare Functions} *)

  val compare : t -> t -> int
  (** [compare l l'] compares the labels [l] and [l']. This function defines a
  total ordering on labels. *)
  val equal : t -> t -> bool
  (** [equal l l'] checks if [l] and [l'] are equal. This function is
  equivalent to [compare l l' = 0]. *)

  (** {3 Printers} *)

  val fprintf : Format.formatter -> t -> unit
  (** [fprintf fmt l] prints [l] using the OCaml module Format. *)
  val fprintfs : (Format.formatter -> Function.t -> signature -> signature)
   -> (Format.formatter -> Variable.t -> signature -> signature)
   -> Format.formatter -> t -> signature -> signature
  (** [fprintfs f g fmt l s] prints [l] using the [OCaml] module [Format].
  In difference to {!val: Processors.Rewritingx.Label.fprintf}, this function
  uses the information stored in the signature [s] to print function symbols
  and variables. *)
  val fprintfx : (Format.formatter -> Function.t -> signature -> signature)
   -> (Format.formatter -> Variable.t -> signature -> signature)
   -> Format.formatter -> t -> signature -> signature
  (** [fprintfx f g fmt l s] prints [l] in XML using the [OCaml] module
  [Format]. In difference to {!val: Processors.Rewritingx.Label.fprintf},
  this function uses the information stored in the signature [s] to print
  function symbols and variables. *)
  val to_string : t -> string
  (** [to_string l] returns a string that represents [l]. *)
 end

 module Signature : sig
  (*** INCLUDES ***************************************************************)
  include Rewriting.STATEX with type label = Label.t

  (*** VALUES *****************************************************************)
  (** {3 Constructors and Destructors} *)
  
  val add_rlab : Function.t -> Function.t -> t -> t
  (** [add_rlab f g s] updates the outermost root label of [g] by adding the
  function [f] to the top of the given list. Note that as a side effect the
  old signature changes all over. 
  @raise Not_found if [g] has no root label. *)
  val drop_curry : Function.t -> t -> Function.t
  (** [drop_curry f s] unlabels [f] if [f] admits a curry label.
  @raise Not_found otherwise. *)
  val drop_dp : Function.t -> t -> Function.t
  (** [drop_dp f s] unlabels [f] if [f] is a DP symbol.
  @raise Not_found otherwise. *)
  val drop_height : Function.t -> t -> Function.t
  (** [drop_height f s] unlabels [f] if [f] admits a height label.
  @raise Not_found otherwise. *)
  val drop_rlab : Function.t -> t -> Function.t
  (** [drop_rlab f s] unlabels [f] if [f] admits a root label.
  @raise Not_found otherwise. *)
  val drop_slab : Function.t -> t -> Function.t
  (** [drop_slab f s] unlabels [f] if [f] admits a semantic label.
  @raise Not_found otherwise. *)
  val drop_theory : Function.t -> t -> Function.t
  (** [drop_theory f s] unlabels [f] if [f] admits a theory label.
  @raise Not_found otherwise. *)
  val incr_curry : Function.t -> t -> t
  (** [incr_curry f s] increments the outermost curry label of [f]. Note that
  as a side effect the old signature changes all over.
  @raise Not_found if [f] has no curry label. *)
  val modify_curry : Function.t -> int -> t -> t
  (** [modify_curry f n s] replaces the outermost curry label by a new curry
  label with value [n]. Note that as a side effect the old signature changes
  all over. 
  @raise Not_found if [f] has no curry label. *)
  val modify_height : Function.t -> int -> t -> t
  (** [modify_height f n s] replaces the outermost height label by a new height
  label with value [n]. Note that as a side effect the old signature changes
  all over. 
  @raise Not_found if [f] has no height label. *)
  val modify_rlab : Function.t -> Function.t list -> t -> t
  (** [modify_rlab f fs s] replaces the outermost root label by a new root
  label with value [fs]. Note that as a side effect the old signature changes
  all over. 
  @raise Not_found if [f] has no root label. *)
  val modify_slab : Function.t -> int -> t -> t
  (** [modify_slab f n s] replaces the outermost semantic label by a new
  semantic label with value [n]. Note that as a side effect the old signature
  changes all over. 
  @raise Not_found if [f] has no semantic label. *)
  val set_curry : ?arity:int -> Function.t -> int -> t -> Function.t * t
  (** [set_curry ~arity:a f n s] labels the function symbol [f] with the curry
  label [n]. Furthermore the arity of the labeled symbol is set to [a]. If
  [a] is not specified, the arity of the [f] is taken. Note that as a side
  effect the old signature changes all over. *)
  val set_dp : Function.t -> t -> Function.t * t
  (** [set_dp f s] labels the function symbol [f] as DP symbol. Note that as
  a side effect the old signature changes all over. *)
  val set_height : Function.t -> int -> t -> Function.t * t
  (** [set_height f n s] labels the function symbol [f] with height [n]. Note
  that as a side effect the old signature changes all over. *)
  val set_rlab : Function.t -> Function.t list -> t -> Function.t * t
  (** [set_rlab f fs s] labels the function symbol [f] with the root label
  [fs]. Note that as a side effect the old signature changes all over. *)
  val set_slab : Function.t -> int -> t -> Function.t * t
  (** [set_slab f n s] labels the function symbol [f] with the semantic label
  [n]. Note that as a side effect the old signature changes all over. *)
  val set_theory : Function.t -> Label.theory -> t -> Function.t * t
  (** [set_theory f t s] labels the function symbol [f] with the theory label
  [t]. Note that as a side effect the old signature changes all over. *)
  
  (** {3 Search Functions} *)

  val get_curry : Function.t -> t -> int
  (** [get_curry f s] returns the value of the first curry label of [f].
  @raise Not_found if [f] does not admit a curry label. *)
  val get_height : Function.t -> t -> int
  (** [get_height f s] returns the value of the first height label of [f].
  @raise Not_found if [f] does not admit a height label. *)
  val get_rlab : Function.t -> t -> Function.t list
  (** [get_rlab f s] returns the value of the first root label of [f].
  @raise Not_found if [f] does not admit a root label. *)
  val get_slab : Function.t -> t -> int
  (** [get_slab f s] returns the value of the first semantic label of [f].
  @raise Not_found if [f] does not admit a semantic label. *)
  
  (** {3 Properties} *)

  val is_curry : Function.t -> t -> bool
  (** [is_curry f s] checks if [f] admits a curry label. *)
  val is_dp : Function.t -> t -> bool
  (** [is_dp f s] checks if [f] is a DP symbol. *)
  val is_height : Function.t -> t -> bool
  (** [is_height f s] checks if [f] admits a height label. *)
  val is_rlab : Function.t -> t -> bool
  (** [is_rlab f s] checks if [f] admits a root label. *)
  val is_slab : Function.t -> t -> bool
  (** [is_slab f s] checks if [f] admits a semantic label. *)
  val is_theory : Label.theory -> Function.t -> t -> bool
  (** [is_theory tl f s] checks if [f] adheres to theory [tl]. *)
  val is_some_theory : Function.t -> t -> bool
  (** [is_some theory f s] checks if [f] adheres to some theory. *)

  (** {3 Printers} *)
 
  val fprintfx_fun : Format.formatter -> Function.t -> t -> t
  (** [fprintfx_fun fmt f s] prints [f] in XML using the [OCaml] module
  [Format]. This function uses the information stored in the signature
  [s] to print function symbols. If the name of a function symbol is not
  defined, a randomly generated name is printed. Note that as a side
  effect copies of [s] are changed too. Not tail-recursive. *)
  val fprintfx_var : Format.formatter -> Variable.t -> t -> t
  (** [fprintfx_var fmt x s] prints [x] in XML using the [OCaml] module
  [Format]. This function uses the information stored in the signature
  [s] to print variables. If the name of a variable is not defined, a
  randomly generated name is printed. Note that as a side effect copies
  of [s] are changed too. *)
  val to_stringx_fun : Function.t -> t -> string * t
  (** [to_stringx_fun f s] returns a formatted string that represents [f]
  in XML. This function uses the information stored in the signature [s] to
  print function symbols. If the name of a function symbol is not defined,
  a randomly generated name is printed. Note that as a side effect copies
  of [s] are changed too. Not tail-recursive. *)
  val to_stringx_var : Variable.t -> t -> string * t
  (** [to_stringx_var x s] returns a formatted string that represents [x]
  in XML. This function uses the information stored in the signature [s]
  to print variables. If the name of a variable is not defined, a randomly
  generated name is printed. Note that as a side effect copies of [s] are
  changed too. *)
 end

 module Monad : sig
  (*** INCLUDES ***************************************************************)
  include Rewriting.MONADX with
   type label = Label.t and type state = Signature.t

  (*** VALUES *****************************************************************)
  (** {3 Labeling Constructors and Destructors} *)
  
  (** {4 Generic Labelings } *)
  
  val add_int : ?arity:int -> Function.t -> int -> Function.t t
  (** [add_int ~arity:a i f] adds integer label [i] to [f] and sets
  its arity to [~arity]. Labels can be stacked. *)
  val drop_int : Function.t -> Function.t t
  (** [drop_int f] drops integer label of [f] from the top of the
  stack. Note that the arity of [f] is not changed by unlabeling.
  @raise No_int_label if [f] has no integer label *)
  val get_int : Function.t -> int t
  (** [get_int f] returns integer label of [f] on from the top of
  stack.
  @raise  No_int_label if [f] has no integer label. *)
  val is_int : Function.t -> bool t
  (** [is_int f] checks if [f] has an integer label*)

  (** {4 Specific Labelings } *)

  val add_rlab : Function.t -> Function.t -> unit t
  (** [add_rlab f g] is equivalent to
  {!val: Processors.Rewritingx.Signature.add_rlab} except that the signature is
  encapsulated in a monad. If [g] has no root label then the returned monad
  represents the error "not found". Note that as a side effect the underlying
  signature changes all over. *)
  val drop_curry : Function.t -> Function.t t
  (** [drop_curry f] is equivalent to
  {!val: Processors.Rewritingx.Signature.drop_curry} except that the result is
  encapsulated in a monad. If [f] is not labeled with a curry label, then the
  returned monad represents the error "not found". *)
  val drop_dp : Function.t -> Function.t t
  (** [drop_dp f] is equivalent to
  {!val: Processors.Rewritingx.Signature.drop_dp} except that the result is
  encapsulated in a monad. If [f] is not a DP symbol, then the returned monad
  represents the error "not found". *)
  val drop_height : Function.t -> Function.t t
  (** [drop_height f] is equivalent to
  {!val: Processors.Rewritingx.Signature.drop_height} except that the result
  is encapsulated in a monad. If [f] is not labeled with a height label, then
  the returned monad represents the error "not found". *)
  val drop_rlab : Function.t -> Function.t t
  (** [drop_rlab f] is equivalent to
  {!val: Processors.Rewritingx.Signature.drop_rlab} except that the result is
  encapsulated in a monad. If [f] is not labeled with a root label, then the
  returned monad represents the error "not found". *)
  val drop_slab : Function.t -> Function.t t
  (** [drop_slab f] is equivalent to
  {!val: Processors.Rewritingx.Signature.drop_slab} except that the result is
  encapsulated in a monad. If [f] is not labeled with a semantic label, then
  the returned monad represents the error "not found". *)
  val drop_theory : Function.t -> Function.t t
  (** [drop_theory f] is equivalent to
  {!val: Processors.Rewritingx.Signature.drop_theory} except that the result
  is encapsulated in a monad. If [f] is not labeled with a theory label, then
  the returned monad represents the error "not found". *)
  val incr_curry : Function.t -> unit t
  (** [incr_curry f] is equivalent to
  {!val: Processors.Rewritingx.Signature.incr_curry} except that the signature
  is encapsulated in a monad. If [f] has no curry label then the returned monad
  represents the error "not found". Note that as a side effect the underlying
  signature changes all over. *)
  val modify_curry : Function.t -> int -> unit t
  (** [modify_curry f n] is equivalent to
  {!val: Processors.Rewritingx.Signature.modify_curry} except that the signature
  is encapsulated in a monad. If [f] has no curry label then the returned monad
  represents the error "not found". Note that as a side effect the underlying
  signature changes all over. *)
  val modify_height : Function.t -> int -> unit t
  (** [modify_height f n] is equivalent to
  {!val: Processors.Rewritingx.Signature.modify_height} except that the
  signature is encapsulated in a monad. If [f] has no height label then the
  returned monad represents the error "not found". Note that as a side effect
  the underlying signature changes all over. *)
  val modify_rlab : Function.t -> Function.t list -> unit t
  (** [modify_rlab f fs] is equivalent to
  {!val: Processors.Rewritingx.Signature.modify_rlab} except that the signature
  is encapsulated in a monad. If [f] has no root label then the returned monad
  represents the error "not found". Note that as a side effect the underlying
  signature changes all over. *)
  val modify_slab : Function.t -> int -> unit t
  (** [modify_slab f n] is equivalent to
  {!val: Processors.Rewritingx.Signature.modify_slab} except that the
  signature is encapsulated in a monad. If [f] has no semantic label then
  the returned monad represents the error "not found". Note that as a side
  effect the underlying signature changes all over. *)
  val set_curry : ?arity:int -> Function.t -> int -> Function.t t
  (** [set_curry ~arity:a f n] is equivalent to
  {!val: Processors.Rewritingx.Signature.set_curry} except that the result is
  encapsulated in a monad. Note that as a side effect the underlying signature
  changes all over. *)
  val set_dp : Function.t -> Function.t t
  (** [set_dp f] is equivalent to
  {!val: Processors.Rewritingx.Signature.set_dp} except that the result is
  encapsulated in a monad. Note that as a side effect the underlying signature
  changes all over. *)
  val set_height : Function.t -> int -> Function.t t
  (** [set_height f n] is equivalent to
  {!val: Processors.Rewritingx.Signature.set_height} except that the result is
  encapsulated in a monad. Note that as a side effect the underlying signature
  changes all over. *)
  val set_rlab : Function.t -> Function.t list -> Function.t t
  (** [set_rlab f fs] is equivalent to
  {!val: Processors.Rewritingx.Signature.set_rlab} except that the result is
  encapsulated in a monad. Note that as a side effect the underlying signature
  changes all over. *)
  val set_slab : Function.t -> int -> Function.t t
  (** [set_slab f n] is equivalent to
  {!val: Processors.Rewritingx.Signature.set_slab} except that the result is
  encapsulated in a monad. Note that as a side effect the underlying signature
  changes all over. *)
  val set_theory : Function.t -> Label.theory -> Function.t t
  (** [set_theory f n] is equivalent to
  {!val: Processors.Rewritingx.Signature.set_theory} except that the result is
  encapsulated in a monad. Note that as a side effect the underlying signature
  changes all over. *)

  (** {3 Labeling Search Functions} *)

  val get_curry : Function.t -> int t
  (** [get_curry f] is equivalent to
  {!val: Processors.Rewritingx.Signature.get_curry} except that the result is
  encapsulated in a monad. If [f] is not labeled with a curry label, then the
  returned monad represents the error "not found". *)
  val get_height : Function.t -> int t
  (** [get_height f] is equivalent to
  {!val: Processors.Rewritingx.Signature.get_height} except that the result is
  encapsulated in a monad. If [f] is not labeled with a height label, then the
  returned monad represents the error "not found". *)
  val get_rlab : Function.t -> Function.t list t
  (** [get_rlab f] is equivalent to
  {!val: Processors.Rewritingx.Signature.get_rlab} except that the result is
  encapsulated in a monad. If [f] is not labeled with a root label, then the
  returned monad represents the error "not found". *)
  val get_slab : Function.t -> int t
  (** [get_slab f] is equivalent to
  {!val: Processors.Rewritingx.Signature.get_slab} except that the result is
  encapsulated in a monad. If [f] is not labeled with a semantic label, then
  the returned monad represents the error "not found". *)
  val get_theory : Function.t -> Label.theory t
  (** [get_theory f] is equivalent to
  {!val: Processors.Rewritingx.Signature.get_theory} except that the result is
  encapsulated in a monad. If [f] is not labeled with a theory label, then
  the returned monad represents the error "not found". *)


  (** {3 Labeling Properties} *)

  val is_curry : Function.t -> bool t
  (** [is_curry f] is equivalent to
  {!val: Processors.Rewritingx.Signature.is_curry} except that the result is
  encapsulated in a monad. *)
  val is_dp : Function.t -> bool t
  (** [is_curry f] is equivalent to
  {!val: Processors.Rewritingx.Signature.is_dp} except that the result is
  encapsulated in a monad. *)
  val is_height : Function.t -> bool t
  (** [is_curry f] is equivalent to
  {!val: Processors.Rewritingx.Signature.is_height} except that the result is
  encapsulated in a monad. *)
  val is_rlab : Function.t -> bool t
  (** [is_curry f] is equivalent to
  {!val: Processors.Rewritingx.Signature.is_rlab} except that the result is
  encapsulated in a monad. *)
  val is_slab : Function.t -> bool t
  (** [is_curry f] is equivalent to
  {!val: Processors.Rewritingx.Signature.is_slab} except that the result is
  encapsulated in a monad. *)
  val is_theory : Label.theory -> Function.t -> bool t
  (** [is_theory f] is equivalent to
  {!val: Processors.Rewritingx.Signature.is_theory} except that the result is
  encapsulated in a monad. *)
  val is_some_theory : Function.t -> bool t
  (** [is_some_theory f] is equivalent to
  {!val: Processors.Rewritingx.Signature.is_some_theory} except that the 
  result is encapsulated in a monad. *)

  (** {3 Printers} *)

  val fprintfx_fun : Format.formatter -> Function.t -> unit t
  (** [fprintfx_fun fmt f] is equivalent to
  {!val: Processors.Rewritingx.Signature.fprintfx_fun} except that the
  signature is encapsulated in a monad. Note that as a side effect the
  old signature is changed too. Not tail-recursive. *)
  val fprintfx_var : Format.formatter -> Variable.t -> unit t
  (** [fprintfx_var fmt x] is equivalent to
  {!val: Processors.Rewritingx.Signature.fprintfx_var} except that the
  signature is encapsulated in a monad. Note that as a side effect the
  old signature is changed too. *)
  val to_stringx_fun : Function.t -> string t
  (** [to_stringx_fun f] is equivalent to
  {!val: Processors.Rewritingx.Signature.to_stringx_fun} except that the
  signature is encapsulated in a monad. Note that as a side effect the
  old signature is changed too. Not tail-recursive. *)
  val to_stringx_var : Variable.t -> string t
  (** [to_stringx_var x] is equivalent to
  {!val: Processors.Rewritingx.Signature.to_stringx_var} except that the
  signature is encapsulated in a monad. Note that as a side effect the
  old signature is changed too. *)
 end

 module Position : sig
  (*** INCLUDES ***************************************************************)
  include Rewriting.POSITION

  (*** VALUES *****************************************************************)
  (** {3 Printers} *)

  val fprintfx : Format.formatter -> t -> unit
  (** [fprintfx fmt p] prints [p] in XML using the [OCaml] module [Format]. *)
  val to_stringx : t -> string
  (** [to_stringx p] returns a formatted string that represents [p] in XML. *)
 end

 module Parser : Rewriting.PARSER
  with type state = Signature.t and type input = Parsec.StringInput.t

 module Term : sig
  (*** INCLUDES ***************************************************************)
  include Rewriting.TERM with type 'a m = 'a Monad.t and type 'a p = 'a Parser.t
    and type 'a x = 'a Parser.Xml.t

  (*** VALUES *****************************************************************)
  (** {3 Labeling and Unlabeling Functions} *)

  val label_dp : t -> t m
  (** [label_dp t] labels the root symbol of [t] as dependency pair symbol. *)
  val label_height : int -> t -> t m
  (** [label_height h t] labels all function symbols of [t] with height [h]. *)
  val unlabel_dp : t -> t m
  (** [unlabel_dp t] checks if the root symbol of [t] is a dependency pair
  symbol. If that is the case, the original symbol is restored. Otherwise
  [t] is returned. *)
  val unlabel_height : t -> t m
  (** [unlabel_height t] unlabels all function symbol that have been labeled
  with a height label. Note that if [t] does not contains a function symbol
  with height label then [t] is returned. *)

  (** {3 Printers} *)

  val fprintfx : Format.formatter -> t -> unit m
  (** [fprintfx fmt t] prints [t] in XML using the [OCaml] module [Format].
  This function uses the information stored in the underlying signature to
  print function symbols and variables. If the name of a function symbol or
  variable is not defined, a randomly generated name is printed. Note that
  as a side effect copies of the underlying signature are changed too. This
  function is not tail-recursive. *)
  val to_stringx : t -> string m
  (** [to_stringx t] returns a formatted string that represents [t] in XML.
  This function uses the information stored in the underlying signature to
  print function symbols and variables. If the name of a function symbol or
  variable is not defined, a randomly generated name is printed. Note that
  as a side effect copies of the underlying signature are changed too. This
  function is not tail-recursive. *)
 end

 module Context : sig
  (*** INCLUDES ***************************************************************)
  include Rewriting.CONTEXT with type term = Term.t

  (*** VALUES *****************************************************************)
  (** {3 Printers} *)

  val fprintfx : Format.formatter -> t -> unit m
  (** [fprintfx fmt c] prints [c] in XML using the [OCaml] module [Format].
  This function uses the information stored in the underlying signature to
  print function symbols and variables. If the name of a function symbol or
  variable is not defined, a randomly generated name is printed. Note that
  as a side effect copies of the underlying signature are changed too. This
  function is not tail-recursive. *)
  val to_stringx : t -> string m
  (** [to_stringx c] returns a formatted string that represents [c] in XML.
  This function uses the information stored in the underlying signature to
  print function symbols and variables. If the name of a function symbol or
  variable is not defined, a randomly generated name is printed. Note that
  as a side effect copies of the underlying signature are changed too. This
  function is not tail-recursive. *)
 end
 
 module Substitution : sig
  (*** INCLUDES ***************************************************************)
  include Rewriting.SUBSTITUTION with
   type term = Term.t and type context = Context.t

  (*** VALUES *****************************************************************)
  (** {3 Printers} *)

  val fprintfx : Format.formatter -> t -> unit m
  (** [fprintfx fmt s] prints [s] in XML using the [OCaml] module [Format].
  This function uses the information stored in the underlying signature to
  print function symbols and variables. If the name of a function symbol or
  variable is not defined, a randomly generated name is printed. Note that
  as a side effect copies of the underlying signature are changed too. This
  function is not tail-recursive. *)
  val to_stringx : t -> string m
  (** [to_stringx s] returns a formatted string that represents [s] in XML.
  This function uses the information stored in the underlying signature to
  print function symbols and variables. If the name of a function symbol or
  variable is not defined, a randomly generated name is printed. Note that
  as a side effect copies of the underlying signature are changed too. This
  function is not tail-recursive. *)
 end
 
 module Elogic : Rewriting.ELOGIC with
  type 'a m = 'a Monad.t and type substitution = Substitution.t and
  type term = Term.t
 
 module Rule : sig
  (*** INCLUDES ***************************************************************)
  include Rewriting.RULE with
   type 'a m = 'a Monad.t and type 'a p = 'a Parser.t
    and type substitution = Substitution.t and type term = Term.t
    and type 'a x = 'a Parser.Xml.t

  (*** VALUES *****************************************************************)
  (** {3 Printers} *)

  val fprintfx : Format.formatter -> t -> unit m
  (** [fprintfx fmt r] prints [r] in XML using the [OCaml] module [Format].
  This function uses the information stored in the underlying signature to
  print function symbols and variables. If the name of a function symbol or
  variable is not defined, a randomly generated name is printed. Note that
  as a side effect copies of the underlying signature are changed too. This
  function is not tail-recursive. *)
  val to_stringx : t -> string m
  (** [to_stringx r] returns a formatted string that represents [r] in XML.
  This function uses the information stored in the underlying signature to
  print function symbols and variables. If the name of a function symbol or
  variable is not defined, a randomly generated name is printed. Note that
  as a side effect copies of the underlying signature are changed too. This
  function is not tail-recursive. *)
 end
 
 module Trs : sig
  (*** INCLUDES ***************************************************************)
  include Rewriting.TRS with
   type 'a m = 'a Monad.t and type 'a p = 'a Parser.t
    and type rule = Rule.t and type term = Term.t
    and type 'a x = 'a Parser.Xml.t

  (*** VALUES *****************************************************************)
  (** {3 Miscellaneous} *)

  val etcap : term -> t -> term m
  (** [etcap t r] modifies the term [t] such that it can be checked whether
  an instance of [t] rewrites to some term [u] with respect to the TRS [r],
  by using ground matching. *)
  val icap : term -> t -> term m
  (** [icap t r] modifies the term [t] such that it can be checked whether
  an instance of [t] rewrites to some term [u] with respect to the innermost
  rewrite relation induced by the TRS [r], by using unification. *)
  val is_strongly_nonoverlapping : t -> bool m
  (** [is_strongly_nonoverlapping r], checks if there are no overlaps after
  linearizing [r]. *)
  val linear : t -> t m
  (** [linear r] returns a complete linearization of [r]. I.e., all possible
  linearizations of left-hand sides are taken into consideration. *)
  val linearize : t -> t m
  (** [linearize r] linearizes [r]. In difference to
  {!val: Processors.Rewritingx.Trs.linear} only one linearization is
  computed. *)
  val recursors : t -> rule list
  (** [recursors r] returns all rewrite rules of [r] whose right-hand side
  contains a defined symbol. *)
  val sharp : t -> (Function.t * t) m
  (** [sharp r] transforms [r] into a rewrite system which can be used to
  compute the set of right-hand sides of forward closures via tree automata
  completion. *)
  val tcap : term -> t -> term m
  (** [tcap t r] modifies the term [t] such that it can be checked whether
  an instance of [t] rewrites to some term [u] with respect to the the TRS
  [r], by using unification. *)
  val extend : t -> t m
  (** [extend t] computes all AC extension rules of [r]. *)
  val srules : t -> t m
  (** [srules t] computes all S-rules corresponding to AC symbols occurring
  in the trs [t], required for AC-DPs. *)
  val theory : t -> t m
  (** [theory t] computes all AC-equations corresponding to AC symbols 
  occurring in the trs [t]. The resulting TRS is symmetric, i.e., inversed
  equations are included. *)

  (** {3 Printers} *)

  val fprintfx : Format.formatter -> t -> unit m
  (** [fprintfx fmt r] prints [r] in XML using the [OCaml] module [Format].
  This function uses the information stored in the underlying signature to
  print function symbols and variables. If the name of a function symbol or
  variable is not defined, a randomly generated name is printed. Note that
  as a side effect copies of the underlying signature are changed too. This
  function is not tail-recursive. *)
  val to_stringx : t -> string m
  (** [to_stringx r] returns a formatted string that represents [r] in XML.
  This function uses the information stored in the underlying signature to
  print function symbols and variables. If the name of a function symbol or
  variable is not defined, a randomly generated name is printed. Note that
  as a side effect copies of the underlying signature are changed too. This
  function is not tail-recursive. *)
 end

module MyTTT2: sig
 val run : 
   Processors.Problem.t -> 
   Signature.t -> 
   Ttt2.Strategy.Syntax.t -> 
   Ttt2.Strategy.Proof.t * Signature.t * Ttt2.Strategy.Status.t
end

module MyProblem : sig
 type t = Processors.Problem.t
 val make_sp : 
  Processors.Problem.language -> 
  Processors.Problem.strategy -> Trs.t -> 
  Processors.Problem.t
 val make_ep : 
  Processors.Problem.language -> 
  Processors.Problem.strategy -> 
  Trs.t -> Trs.t -> 
  Processors.Problem.t
 val get_trs : t -> Trs.t
end

module MyTrsSyntax : sig
 type t
 val to_problem_with : t -> Signature.t -> MyProblem.t * Signature.t            end

module MyTrsParser : sig
 type token = Ttt2.Input.TrsParser.token
 val trs : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> MyTrsSyntax.t
end

module MyTrsLexer : sig
 val token : Lexing.lexbuf -> MyTrsParser.token
end

