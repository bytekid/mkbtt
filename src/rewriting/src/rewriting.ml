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

(*** INCLUDES *****************************************************************)
include Rewritingx.Prelude;;

(*** OPEN *********************************************************************)
open Util;;

(*** MODULE TYPES *************************************************************)
module type FUNCTION = sig
 type t = int
 
 val copy : t -> t
 val hash : t -> int
 val next : t -> t
 val to_int : t -> int
 val zero : t
 val compare : t -> t -> int
 val equal : t -> t -> bool
 val fprintf : Format.formatter -> t -> unit
 val to_string : t -> string
end

module type VARIABLE = sig
 type t

 val copy : t -> t
 val hash : t -> int
 val next : t -> t
 val to_int : t -> int
 val zero : t
 val compare : t -> t -> int
 val equal : t -> t -> bool
 val fprintf : Format.formatter -> t -> unit
 val to_string : t -> string
end

module type STATE = sig
 type t

 val add_ari : Function.t -> int -> t -> t
 val add_fun : Function.t -> int -> string -> t -> t
 val add_fun_name : Function.t -> string -> t -> t
 val add_var : Variable.t -> string -> t -> t
 val empty : int -> t
 val replace_ari : Function.t -> int -> t -> t
 val replace_fun : Function.t -> int -> string -> t -> t
 val replace_fun_name : Function.t -> string -> t -> t
 val replace_var : Variable.t -> string -> t -> t
 val create_fun : int -> string -> t -> Function.t * t
 val create_fun_name : Function.t -> t -> string * t
 val create_var : string -> t -> Variable.t * t
 val create_var_name : Variable.t -> t -> string * t
 val fresh_fun : t -> Function.t * t
 val fresh_var : t -> Variable.t * t
 val find_ari : Function.t -> t -> int
 val find_fun : string -> t -> Function.t
 val find_fun_name : Function.t -> t -> string
 val find_var : string -> t -> Variable.t
 val find_var_name : Variable.t -> t -> string
 val var_names : t -> string list
 val is_ari : int -> Function.t -> t -> bool
 val is_defined_fun : Function.t -> t -> bool
 val is_defined_var : Variable.t -> t -> bool
 val mem_ari : Function.t -> t -> bool
 val mem_fun : Function.t -> t -> bool
 val mem_fun_name : string -> t -> bool
 val mem_var : Variable.t -> t -> bool
 val mem_var_name : string -> t -> bool
 val copy : t -> t
 val fprintf : Format.formatter -> t -> unit
 val fprintf_fun : Format.formatter -> Function.t -> t -> t
 val fprintf_var : Format.formatter -> Variable.t -> t -> t
 val to_string : t -> string
 val to_string_fun : Function.t -> t -> string * t
 val to_string_var : Variable.t -> t -> string * t
end

module type STATEX = sig
 type label
 type signature

 include STATE

 val label : ?arity:int -> Function.t -> label -> t -> Function.t * t
 val replace_lab : Function.t -> label -> t -> t
 val drop_lab : (label -> bool) -> Function.t -> t -> Function.t
 val find_lab : Function.t -> t -> label
 val labs : Function.t -> t -> label list
 val origin : Function.t -> t -> Function.t
 val pick_lab : (label -> bool) -> Function.t -> t -> label
 val search_lab : (label -> bool) -> Function.t -> t -> Function.t * label
 val unlabel : Function.t -> t -> Function.t
 val is_lab : (label -> bool) -> Function.t -> t -> bool
 val is_labeled : Function.t -> t -> bool
 val is_unlabeled : Function.t -> t -> bool
 val set_signature : signature -> t -> t
 val to_signature : t -> signature
end

module type MONAD = sig
 type error = string
 type state

 include Monad.SIGNATURE

 val ap_comb : ((error,'a * state) either -> (error,'b * state) either) t
  -> 'a t -> 'b t
 val map_comb : ((error,'a * state) either -> (error,'b * state) either)
  -> 'a t -> 'b t
 val adopt : (state -> 'a * state) -> 'a t
 val get : state t
 val modify : (state -> state) -> state t
 val set : state -> unit t
 val update : (state -> state) -> unit t
 val with_state : (state -> state) -> 'a t -> 'a t
 val catch : (error -> 'a t) -> 'a t -> 'a t
 val fail : error -> 'a t
 val eval : state -> 'a t -> (error,'a * state) either
 val execute : state -> 'a t -> (error,state) either
 val run : state -> 'a t -> (error,'a) either
 val add_ari : Function.t -> int -> unit t
 val add_fun : Function.t -> int -> string -> unit t
 val add_fun_name : Function.t -> string -> unit t
 val add_var : Variable.t -> string -> unit t
 val replace_ari : Function.t -> int -> unit t
 val replace_fun : Function.t -> int -> string -> unit t
 val replace_fun_name : Function.t -> string -> unit t
 val replace_var : Variable.t -> string -> unit t
 val create_fun : int -> string -> Function.t t
 val create_fun_name : Function.t -> string t
 val create_var : string -> Variable.t t
 val create_var_name : Variable.t -> string t
 val fresh_fun : Function.t t
 val fresh_var : Variable.t t
 val find_ari : Function.t -> int t
 val find_fun : string -> Function.t t
 val find_fun_name : Function.t -> string t
 val find_var : string -> Variable.t t
 val find_var_name : Variable.t -> string t
 val is_ari : int -> Function.t -> bool t
 val is_defined_fun : Function.t -> bool t
 val is_defined_var : Variable.t -> bool t
 val mem_ari : Function.t -> bool t
 val mem_fun : Function.t -> bool t
 val mem_fun_name : string -> bool t
 val mem_var : Variable.t -> bool t
 val mem_var_name : string -> bool t
 val fprintf_fun : Format.formatter -> Function.t -> unit t
 val fprintf_var : Format.formatter -> Variable.t -> unit t
 val to_string_fun : Function.t -> string t
 val to_string_var : Variable.t -> string t
end

module type MONADX = sig
 type label

 include MONAD

 val label : ?arity:int -> Function.t -> label -> Function.t t
 val replace_lab : Function.t -> label -> unit t
 val drop_lab : (label -> bool) -> Function.t -> Function.t t
 val find_lab : Function.t -> label t
 val labs : Function.t -> label list t
 val origin : Function.t -> Function.t t
 val pick_lab : (label -> bool) -> Function.t -> label t
 val search_lab : (label -> bool) -> Function.t -> (Function.t * label) t
 val unlabel : Function.t -> Function.t t
 val is_lab : (label -> bool) -> Function.t -> bool t
 val is_labeled : Function.t -> bool t
 val is_unlabeled : Function.t -> bool t
end

module type POSITION = sig
 type t

 val add_first : int -> t -> t
 val add_last : int -> t -> t
 val root : t
 val head : t -> int
 val init : t -> t
 val last : t -> int
 val make : int -> t
 val of_list : int list -> t
 val split_first : t -> int * t
 val split_last : t -> t * int
 val tail : t -> t
 val to_list : t -> int list
 val (||) : t -> t -> bool
 val (<=) : t -> t -> bool
 val (<) : t -> t -> bool
 val (>=) : t -> t -> bool
 val (>) : t -> t -> bool
 val is_root : t -> bool
 val are_parallel : t -> t -> bool
 val is_prefix : t -> t -> bool
 val is_proper_prefix : t -> t -> bool
 val is_proper_suffix : t -> t -> bool
 val is_suffix : t -> t -> bool
 val append : t -> t -> t
 val copy : t -> t
 val hash : t -> int
 val length : t -> int
 val compare : t -> t -> int
 val equal : t -> t -> bool
 val fprintf : Format.formatter -> t -> unit
 val to_string : t -> string
end

module type PARSER = sig
  include Parsec.STATEFUL_CHAR_PARSER
  module Xml : Parsec.Xml.TRANSFORMER with type state = state
end

module type TERM = sig
 type 'a m
 type 'a p
 type 'a x
 type state
 type t = Var of Variable.t | Fun of Function.t * t list

 val make_fun : Function.t -> t list -> t
 val make_var : Variable.t -> t
 val reflect : t -> t
 val replace : Position.t -> t -> t -> t
 val reverse : t -> t
 val subterm : Position.t -> t -> t
 val ren : ?p:(Position.t -> Variable.t -> bool) -> t -> t m
 val rename : t -> t m
 val cons : t -> Function.t list
 val funs : t -> Function.t list
 val root : t -> Function.t option
 val symbols : t -> (Variable.t,Function.t) either list
 val vars : t -> Variable.t list
 val is_build : Function.t list -> t -> bool
 val is_cons : t -> bool
 val is_dummy : t -> bool
 val is_embedded : t -> t -> bool
 val is_flat : t -> bool
 val is_fun : t -> bool
 val is_ground : t -> bool
 val is_linear : t -> bool
 val is_proper_subterm : t -> t -> bool
 val is_shallow : t -> bool
 val is_string : t -> bool
 val is_subterm : t -> t -> bool
 val is_var : t -> bool
 val mem_fun : Function.t -> t -> bool
 val mem_var : Variable.t -> t -> bool
 val fun_pos : Function.t -> t -> Position.t list
 val funs_pos : t -> Position.t list
 val pos : t -> Position.t list
 val subterm_pos : t -> t -> Position.t list
 val var_pos : Variable.t -> t -> Position.t list
 val vars_pos : t -> Position.t list
 val count_fun : Function.t -> t -> int
 val fold_funs : (Function.t -> 'a -> 'a) -> 'a -> t -> 'a
 val map : (Function.t -> Function.t) -> t -> t
 val args : t -> t list
 val copy : t -> t
 val depth : t -> int
 val fun_size : t -> int
 val hash : t -> int
 val proper_subterms : ?p:(t -> bool) -> t -> t list
 val size : t -> int
 val subterms : ?p:(t -> bool) -> t -> t list
 val var_size : t -> int
 val compare : t -> t -> int
 val equal : t -> t -> bool
 val of_string : string list -> string -> t m
 val of_xml_string : string -> t m
 val parse : string list -> t p
 val xml : t x
 val fprintf : Format.formatter -> t -> unit
 val fprintfm : Format.formatter -> t -> unit m
 val fprintfs : state -> Format.formatter -> t -> state
 val to_string : t -> string
 val to_stringm : t -> string m
 val to_strings : state -> t -> (state*string)
end

module type CONTEXT = sig
 type 'a m
 type term
 type t = Hole | More of Function.t * term list * t * term list

 val apply : term -> t -> term
 val compose : t -> t -> t
 val of_term : Position.t -> term -> t
 val subcontext : Position.t -> t -> t
 val is_empty : t -> bool
 val hash : t -> int
 val hole_pos : t -> Position.t
 val pos : t -> Position.t list
 val compare : t -> t -> int
 val equal : t -> t -> bool
 val fprintf : Format.formatter -> t -> unit
 val fprintfm : Format.formatter -> t -> unit m
 val to_string : t -> string
 val to_stringm : t -> string m
end

module type SUBSTITUTION = sig
 type 'a m
 type context
 type term

 include Replacement.SIGNATURE with
  type domain = Variable.t and type range = term

 val apply_term : t -> term -> term
 val apply_context : t -> context -> context
 val is_renaming : t -> bool
 val fprintfm : Format.formatter -> t -> unit m
 val to_stringm : t -> string m
end

module type ELOGIC = sig
 type 'a m
 type substitution
 type term

 exception Not_matchable
 exception Not_semi_unifiable
 exception Not_unifiable

 (* val are_semi_unifiable : term -> term -> bool *)
 val are_unifiable : term -> term -> bool
 val is_variant : term -> term -> bool
 (* val semi_unify : term -> term -> substitution * substitution *)
 (* val semi_unify_problem : (term * term) list -> substitution * substitution *)
 val unify : term -> term -> substitution
 val unify_problem : (term * term) list -> substitution
 val contains : term -> term -> bool
 val ground_matches : term -> term -> bool
 val matches : term -> term -> bool
 val match_problem : (term * term) list -> substitution
 val match_term : term -> term -> substitution
 val subsumes : term -> term -> bool
 val renaming : term -> substitution m
end

module type RULE = sig
 type 'a m
 type 'a p
 type 'a x
 type substitution
 type term
 type t

 val invert : t -> t
 val lhs : t -> term
 val of_terms : term -> term -> t
 val reflect : t -> t
 val reverse : t -> t
 val rhs : t -> term
 val to_terms : t -> term * term
 val rename : t -> t m
 val apply : (term -> 'a) -> (term -> 'b) -> t -> 'a * 'b
 val count_fun : Function.t -> t -> int
 val fold : (term -> 'a -> 'a) -> 'a -> t -> 'a
 val map : (term -> 'a) -> t -> 'a * 'a
 val project : (term -> term) -> t -> t
 val uncurry : (term -> term -> 'a) -> t -> 'a
 val cons : t -> Function.t list
 val funs : t -> Function.t list
 val left_cons : t -> Function.t list
 val left_funs : t -> Function.t list
 val left_symbols : t -> (Variable.t,Function.t) either list
 val left_vars : t -> Variable.t list
 val left_root : t -> Function.t
 val right_cons : t -> Function.t list
 val right_funs : t -> Function.t list
 val right_symbols : t -> (Variable.t,Function.t) either list
 val right_vars : t -> Variable.t list
 val right_root : t -> Function.t
 val roots : t -> Function.t list
 val symbols : t -> (Variable.t,Function.t) either list
 val vars : t -> Variable.t list
 val is_build : Function.t list -> t -> bool
 val is_collapsing : t -> bool
 val is_contained : t -> bool
 val is_dummy : t -> bool
 val is_duplicating : t -> bool
 val is_embedded : t -> bool
 val is_erasing : t -> bool
 val is_flat : t -> bool
 val is_ground : t -> bool
 val is_growing : t -> bool
 val is_left_build : Function.t list -> t -> bool
 val is_left_dummy : t -> bool
 val is_left_flat : t -> bool
 val is_left_ground : t -> bool
 val is_left_linear : t -> bool
 val is_left_shallow : t -> bool
 val is_left_string : t -> bool
 val is_linear : t -> bool
 val is_normal_form : term -> t -> bool
 val is_proper_embedded : t -> bool
 val is_redex : term -> t -> bool
 val is_rewrite_rule : t -> bool
 val is_right_build : Function.t list -> t -> bool
 val is_right_dummy : t -> bool
 val is_right_flat : t -> bool
 val is_right_ground : t -> bool
 val is_right_linear : t -> bool
 val is_right_shallow : t -> bool
 val is_right_string : t -> bool
 val is_shallow : t -> bool
 val is_size_preserving : t -> bool
 val is_string : t -> bool
 val is_variant : t -> t -> bool
 val matches : t -> t -> bool
 val subsumes : t -> t -> bool
 val are_overlapping : t -> t -> bool m
 val is_overlap : Position.t -> t -> t -> bool m
 val reducts : term -> t -> term list
 val rewrite : term -> Position.t -> t -> term
 val rewrites : term -> Position.t -> t -> term -> bool
 val redex_pos : term -> t -> Position.t list
 val apply_sub : substitution -> t -> t
 val copy : t -> t
 val depth : t -> int
 val hash : t -> int
 val overlaps : t -> t -> (t * Position.t * t) list m
 val compare : t -> t -> int
 val equal : t -> t -> bool
 val of_string : string list -> string -> t m
 val of_xml_string : string -> t m
 val parse : string list -> t p
 val xml : t x
 val fprintf : Format.formatter -> t -> unit
 val fprintfm : Format.formatter -> t -> unit m
 val to_string : t -> string
 val to_stringm : t -> string m
end

module type TRS = sig
 type 'a m
 type 'a p
 type 'a x
 type rule
 type term
 type t

 val add : rule -> t -> t
 val diff : t -> t -> t
 val empty : t
 val intersect : t -> t -> t
 val invert : t -> t
 val lhs : t -> term list
 val of_list : rule list -> t
 val partition : (rule -> bool) -> t -> t * t
 val reflect : t -> t
 val remove : rule -> t -> t
 val reverse : t -> t
 val rhs : t -> term list
 val singleton : rule -> t
 val to_list : t -> rule list
 val union : t -> t -> t
 val unique : t -> t
 val terms : t -> term list
 val rename : t -> t m
 val count_fun : Function.t -> t -> int
 val flat_map : (rule -> 'a list) -> t -> 'a list
 val fold : (rule -> 'a -> 'a) -> 'a -> t -> 'a
 val iter : (rule -> unit) -> t -> unit
 val map : (rule -> 'a) -> t -> 'a list
 val project : (rule -> rule) -> t -> t
 val exists : (rule -> bool) -> t -> bool
 val for_all : (rule -> bool) -> t -> bool
 val mem : rule -> t -> bool
 val choose : t -> rule
 val filter : (rule -> bool) -> t -> t
 val find : (rule -> bool) -> t -> rule
 val cons : t -> Function.t list
 val con_symbols : t -> Function.t list
 val def_symbols : t -> Function.t list
 val funs : t -> Function.t list
 val left_cons : t -> Function.t list
 val left_con_symbols : t -> Function.t list
 val left_funs : t -> Function.t list
 val left_roots : t -> Function.t list
 val left_symbols : t -> (Variable.t,Function.t) either list
 val left_vars : t -> Variable.t list
 val right_cons : t -> Function.t list
 val right_con_symbols : t -> Function.t list
 val right_funs : t -> Function.t list
 val right_roots : t -> Function.t list
 val right_symbols : t -> (Variable.t,Function.t) either list
 val right_vars : t -> Variable.t list
 val roots : t -> Function.t list
 val symbols : t -> (Variable.t,Function.t) either list
 val vars : t -> Variable.t list
 val is_build : Function.t list -> t -> bool
 val is_collapsing : t -> bool
 val is_constructor : t -> bool
 val is_dummy : t -> bool
 val is_duplicating : t -> bool
 val is_erasing : t -> bool
 val is_empty : t -> bool
 val is_flat : t -> bool
 val is_ground : t -> bool
 val is_growing : t -> bool
 val is_left_build : Function.t list -> t -> bool
 val is_left_constructor : t -> bool
 val is_left_dummy : t -> bool
 val is_left_flat : t -> bool
 val is_left_ground : t -> bool
 val is_left_linear : t -> bool
 val is_left_shallow : t -> bool
 val is_linear : t -> bool
 val is_normal_form : term -> t -> bool
 val is_proper_subset : t -> t -> bool
 val is_redex : term -> t -> bool
 val is_right_build : Function.t list -> t -> bool
 val is_right_constructor : t -> bool
 val is_right_dummy : t -> bool
 val is_right_flat : t -> bool
 val is_right_ground : t -> bool
 val is_right_linear : t -> bool
 val is_right_shallow : t -> bool
 val is_shallow : t -> bool
 val is_size_preserving : t -> bool
 val is_srs : t -> bool
 val is_subset : t -> t -> bool
 val is_trs : t -> bool
 val is_variant : t -> t -> bool
 val is_applicative : t -> bool m
 val is_overlapping : t -> bool m
 val is_overlay : t -> bool m
 val reducts : term -> t -> term list
 val rewrite : term -> Position.t -> t -> term list
 val rewrites : term -> Position.t -> t -> term -> bool
 val copy : t -> t
 val depth : t -> int
 val hash : t -> int
 val overlaps : t -> (rule * Position.t * rule) list m
 val size : t -> int
 val compare : t -> t -> int
 val equal : t -> t -> bool
 val equivalent : t -> t -> bool
 val of_string : string list -> string -> t m
 val of_xml_string : string -> t m
 val parse : string list -> t p
 val xml : t x
 val fprintf : Format.formatter -> t -> unit
 val fprintfm : Format.formatter -> t -> unit m
 val to_string : t -> string
 val to_stringm : t -> string m
end

module type REWRITE = sig
 type 'a m
 type term
 type rule
 type trs
 type position

 type step
 type t = Full 

 val step_make : term -> position -> rule -> step

 val step_get_term : step -> term
 val step_get_rule : step -> rule
 val step_get_pos : step -> position

 val step_get_lab : step -> int list
 val step_add_lab_left : step -> int -> step
 val step_add_lab_right: step -> int -> step

 (*
 val join_get_sequences : join -> sequence * sequence
 val join_to_join : sequence * sequence -> join
 val sequence_to_steps : sequence -> step list
 val sequence_of_steps : step list -> sequence
 *)

 val are_joinable : ?s:t -> ?n:int -> term -> term -> trs -> bool
 val reducts : ?s:t -> ?n:int -> term -> trs -> term list
(*  val rewrite : ?s:t -> ?n:int -> term -> trs -> term list *)
 val critical_pairs : trs -> (term * term) list m
 val is_wcr : ?s:t -> ?n:int -> trs -> bool m
 val min_joins : ?s:t -> term -> term -> trs -> (term list * term list) list
end

module type DIAGRAM = sig
 type 'a m
 type term
 type trs
 type step 

 type t

 val critical : trs -> t list m
 val peak : t -> step * step
 val joins : t -> (step list * step list) list
 val label : (int -> step -> int * step) -> t -> t
 val make : (step * step) -> (step list * step list) list -> t

 val is_decreasing : t -> bool

 val fprintfm : Format.formatter -> t -> unit m
end

module type SIGNATURE = sig
 type label
 type signature

 module Function : FUNCTION with type t = Function.t
 module Variable : VARIABLE with type t = Variable.t

 module Signature : STATEX with
  type label = label and type signature = signature

 module Monad : MONADX with type label = label and type state = Signature.t
 module Position : POSITION with type t = Position.t
 module Parser : PARSER
   with type state = Signature.t and type input = Parsec.StringInput.t
 module Term : TERM
   with type 'a m = 'a Monad.t and type 'a p = 'a Parser.t
   and type 'a x = 'a Parser.Xml.t and type state = Signature.t
 module Context : CONTEXT with type 'a m = 'a Monad.t and type term = Term.t

 module Substitution : SUBSTITUTION with
  type 'a m = 'a Monad.t and type term = Term.t and type context = Context.t

 module Elogic : ELOGIC with
  type 'a m = 'a Monad.t and type substitution = Substitution.t
  and type term = Term.t

 module Rule : RULE with
  type 'a m = 'a Monad.t and type 'a p = 'a Parser.t
  and type substitution = Substitution.t and type term = Term.t
  and type 'a x = 'a Parser.Xml.t

 module Trs : TRS with
  type 'a m = 'a Monad.t and type 'a p = 'a Parser.t
  and type rule = Rule.t and type term = Term.t
  and type 'a x = 'a Parser.Xml.t

 module Rewrite : REWRITE with
  type 'a m = 'a Monad.t and type term = Term.t and type trs = Trs.t and
  type rule = Rule.t and type position = Position.t

 module Diagram: DIAGRAM with 
  type 'a m = 'a Monad.t and type term = Term.t and type trs = Trs.t and
  type step = Rewrite.step
end

(*** MODULES ******************************************************************)
module Make (L : LABEL) = struct
 (*** TYPES *******************************************************************)
 type label = L.t;;
 type signature = L.signature;;

 (*** MODULES *****************************************************************)
 module Function = Rewritingx.Function;;
 module Variable = Rewritingx.Variable;;
 module Signature = Rewritingx.Signature.Make (L);;
 module Monad = Rewritingx.Monad.Make (L);;
 module Position = Rewritingx.Position;;
 module Parser = Rewritingx.Parser.Make (L);;
 module Term = Rewritingx.Term.Make (L);;
 module Context = Rewritingx.Context.Make (L);;
 module Substitution = Rewritingx.Substitution.Make (L);;
 module Elogic = Rewritingx.Elogic.Make (L);;
 module Rule = Rewritingx.Rule.Make (L);;
 module Trs = Rewritingx.Trs.Make (L);;
 module Rewrite = Rewritingx.Rewrite.Make (L);;
 module Diagram = Rewritingx.Diagram.Make (L);;
end

module Label = struct
 (*** MODULES *****************************************************************)
 module F = Index.Isomorphic (Rewritingx.Function) (String);;
 module V = Index.Isomorphic (Rewritingx.Variable) (String);;

 (*** TYPES *******************************************************************)
 type signature = F.t * V.t;;
 type t = unit;;

 (*** FUNCTIONS ***************************************************************)
 let compare _ _ = 0;;
 let copy _ = ();;
 let fprintf _ _ = ();;
 let fprintfs _ _ _ _ = id;;
 let hash _ = 0;;
end

(*** INCLUDES ******************************************************************)
include Make (Label);;
