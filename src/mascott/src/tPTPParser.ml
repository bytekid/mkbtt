type token =
  | LSYM of (string)
  | USYM of (string)
  | INPUT_CLAUSE
  | CNF
  | AXIOM
  | HYPOTHESIS
  | CONJECTURE
  | NEGCONJECTURE
  | EQUAL
  | EQUALSIGN
  | DISEQUALSIGN
  | LPAREN
  | RPAREN
  | LBRACKET
  | RBRACKET
  | PLUSPLUS
  | MINUSMINUS
  | COMMA
  | DOT
  | SLASH
  | QUOTE
  | INCLUDE
  | EOF

open Parsing;;
# 2 "tPTPParser.mly"
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

(* tptp parser *)

# 50 "tPTPParser.ml"
let yytransl_const = [|
  259 (* INPUT_CLAUSE *);
  260 (* CNF *);
  261 (* AXIOM *);
  262 (* HYPOTHESIS *);
  263 (* CONJECTURE *);
  264 (* NEGCONJECTURE *);
  265 (* EQUAL *);
  266 (* EQUALSIGN *);
  267 (* DISEQUALSIGN *);
  268 (* LPAREN *);
  269 (* RPAREN *);
  270 (* LBRACKET *);
  271 (* RBRACKET *);
  272 (* PLUSPLUS *);
  273 (* MINUSMINUS *);
  274 (* COMMA *);
  275 (* DOT *);
  276 (* SLASH *);
  277 (* QUOTE *);
  278 (* INCLUDE *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  257 (* LSYM *);
  258 (* USYM *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\002\000\004\000\003\000\005\000\005\000\
\006\000\006\000\006\000\006\000\007\000\007\000\007\000\007\000\
\008\000\008\000\008\000\009\000\009\000\000\000"

let yylen = "\002\000\
\001\000\002\000\002\000\001\000\009\000\011\000\001\000\001\000\
\001\000\001\000\001\000\001\000\007\000\007\000\003\000\003\000\
\004\000\001\000\001\000\001\000\003\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\008\000\007\000\000\000\004\000\022\000\001\000\
\000\000\000\000\000\000\000\000\002\000\003\000\000\000\000\000\
\000\000\000\000\000\000\000\000\009\000\010\000\011\000\012\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\019\000\
\000\000\000\000\000\000\000\000\005\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\015\000\016\000\000\000\017\000\000\000\000\000\006\000\021\000\
\000\000\000\000\000\000\000\000\013\000\014\000"

let yydgoto = "\002\000\
\007\000\008\000\009\000\010\000\011\000\025\000\035\000\044\000\
\045\000"

let yysindex = "\005\000\
\001\000\000\000\000\000\000\000\004\255\000\000\000\000\000\000\
\001\000\001\000\015\255\244\254\000\000\000\000\029\255\030\255\
\013\255\014\255\016\255\031\255\000\000\000\000\000\000\000\000\
\017\255\018\255\024\255\025\255\003\255\021\255\032\255\000\000\
\028\255\033\255\034\255\248\254\000\000\010\255\036\255\037\255\
\038\255\010\255\010\255\023\255\039\255\010\255\010\255\026\255\
\000\000\000\000\010\255\000\000\035\255\040\255\000\000\000\000\
\010\255\010\255\041\255\042\255\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\253\254\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\043\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\016\000\000\000\000\000\000\000\000\000\000\000\227\255\
\248\255"

let yytablesize = 279
let yytable = "\036\000\
\006\000\042\000\043\000\031\000\032\000\001\000\018\000\018\000\
\016\000\018\000\031\000\032\000\049\000\050\000\018\000\012\000\
\053\000\054\000\033\000\034\000\021\000\022\000\023\000\024\000\
\013\000\014\000\015\000\059\000\060\000\017\000\019\000\018\000\
\026\000\020\000\027\000\029\000\039\000\030\000\028\000\037\000\
\051\000\040\000\056\000\038\000\055\000\000\000\041\000\046\000\
\047\000\000\000\048\000\052\000\057\000\061\000\062\000\020\000\
\000\000\058\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\003\000\004\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\005\000"

let yycheck = "\029\000\
\000\000\010\001\011\001\001\001\002\001\001\000\010\001\011\001\
\021\001\013\001\001\001\002\001\042\000\043\000\018\001\012\001\
\046\000\047\000\016\001\017\001\005\001\006\001\007\001\008\001\
\009\000\010\000\012\001\057\000\058\000\001\001\018\001\002\001\
\002\001\020\001\018\001\012\001\009\001\013\001\021\001\019\001\
\018\001\009\001\051\000\012\001\019\001\255\255\013\001\012\001\
\012\001\255\255\013\001\013\001\018\001\013\001\013\001\013\001\
\255\255\018\001\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\003\001\004\001\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\022\001"

let yynames_const = "\
  INPUT_CLAUSE\000\
  CNF\000\
  AXIOM\000\
  HYPOTHESIS\000\
  CONJECTURE\000\
  NEGCONJECTURE\000\
  EQUAL\000\
  EQUALSIGN\000\
  DISEQUALSIGN\000\
  LPAREN\000\
  RPAREN\000\
  LBRACKET\000\
  RBRACKET\000\
  PLUSPLUS\000\
  MINUSMINUS\000\
  COMMA\000\
  DOT\000\
  SLASH\000\
  QUOTE\000\
  INCLUDE\000\
  EOF\000\
  "

let yynames_block = "\
  LSYM\000\
  USYM\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'elements) in
    Obj.repr(
# 53 "tPTPParser.mly"
         ( _1 )
# 239 "tPTPParser.ml"
               : TPTPFormat.element list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'one_clause) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'elements) in
    Obj.repr(
# 56 "tPTPParser.mly"
                      ( _1 :: _2 )
# 247 "tPTPParser.ml"
               : 'elements))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'include_axioms) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'elements) in
    Obj.repr(
# 57 "tPTPParser.mly"
                          ( _1 :: _2 )
# 255 "tPTPParser.ml"
               : 'elements))
; (fun __caml_parser_env ->
    Obj.repr(
# 58 "tPTPParser.mly"
      ( [] )
# 261 "tPTPParser.ml"
               : 'elements))
; (fun __caml_parser_env ->
    let _4 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _6 = (Parsing.peek_val __caml_parser_env 3 : string) in
    Obj.repr(
# 62 "tPTPParser.mly"
 (TPTPFormat.Include(_4, _6))
# 269 "tPTPParser.ml"
               : 'include_axioms))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 10 : 'annotated_formula) in
    let _3 = (Parsing.peek_val __caml_parser_env 8 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 6 : 'clause_status) in
    let _8 = (Parsing.peek_val __caml_parser_env 3 : 'formula) in
    Obj.repr(
# 66 "tPTPParser.mly"
( TPTPFormat.InputClause(_3, _5, _8) )
# 279 "tPTPParser.ml"
               : 'one_clause))
; (fun __caml_parser_env ->
    Obj.repr(
# 69 "tPTPParser.mly"
      ()
# 285 "tPTPParser.ml"
               : 'annotated_formula))
; (fun __caml_parser_env ->
    Obj.repr(
# 70 "tPTPParser.mly"
               ()
# 291 "tPTPParser.ml"
               : 'annotated_formula))
; (fun __caml_parser_env ->
    Obj.repr(
# 73 "tPTPParser.mly"
      (TPTPFormat.Axiom)
# 297 "tPTPParser.ml"
               : 'clause_status))
; (fun __caml_parser_env ->
    Obj.repr(
# 74 "tPTPParser.mly"
             (TPTPFormat.Hypothesis)
# 303 "tPTPParser.ml"
               : 'clause_status))
; (fun __caml_parser_env ->
    Obj.repr(
# 75 "tPTPParser.mly"
             (TPTPFormat.Conjecture)
# 309 "tPTPParser.ml"
               : 'clause_status))
; (fun __caml_parser_env ->
    Obj.repr(
# 76 "tPTPParser.mly"
                (TPTPFormat.NegatedConjecture)
# 315 "tPTPParser.ml"
               : 'clause_status))
; (fun __caml_parser_env ->
    let _4 = (Parsing.peek_val __caml_parser_env 3 : 'term) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'term) in
    Obj.repr(
# 79 "tPTPParser.mly"
                                             ( TPTPFormat.Equality(_4, _6) )
# 323 "tPTPParser.ml"
               : 'formula))
; (fun __caml_parser_env ->
    let _4 = (Parsing.peek_val __caml_parser_env 3 : 'term) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'term) in
    Obj.repr(
# 80 "tPTPParser.mly"
                                                 ( TPTPFormat.DisEquality(_4, _6) )
# 331 "tPTPParser.ml"
               : 'formula))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 81 "tPTPParser.mly"
                      ( TPTPFormat.Equality(_1, _3) )
# 339 "tPTPParser.ml"
               : 'formula))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 82 "tPTPParser.mly"
                         ( TPTPFormat.DisEquality(_1, _3) )
# 347 "tPTPParser.ml"
               : 'formula))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'term_list) in
    Obj.repr(
# 85 "tPTPParser.mly"
                               ( TPTPFormat.Fun(_1, _3)  )
# 355 "tPTPParser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 86 "tPTPParser.mly"
       ( TPTPFormat.Fun (_1,[]) )
# 362 "tPTPParser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 87 "tPTPParser.mly"
       ( TPTPFormat.Var _1 )
# 369 "tPTPParser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 92 "tPTPParser.mly"
        ( [_1] )
# 376 "tPTPParser.ml"
               : 'term_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'term_list) in
    Obj.repr(
# 93 "tPTPParser.mly"
                        ( _1 :: _3 )
# 384 "tPTPParser.ml"
               : 'term_list))
(* Entry start *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let start (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : TPTPFormat.element list)
