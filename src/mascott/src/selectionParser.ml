type token =
  | SIZE_MAX
  | SIZE_SUM
  | SUM
  | MIN
  | CP
  | DATA
  | ELABEL
  | FLOAT of (string)
  | RANDOM
  | COUNT
  | PLUS
  | MINUS
  | TIMESTAMP
  | E
  | R
  | C
  | LPAREN
  | RPAREN
  | COMMA
  | COLON
  | EOF

open Parsing;;
# 2 "selectionParser.mly"
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

(* parser for selection strategy *)
module SS = SelectionStrategy;;
# 48 "selectionParser.ml"
let yytransl_const = [|
  257 (* SIZE_MAX *);
  258 (* SIZE_SUM *);
  259 (* SUM *);
  260 (* MIN *);
  261 (* CP *);
  262 (* DATA *);
  263 (* ELABEL *);
  265 (* RANDOM *);
  266 (* COUNT *);
  267 (* PLUS *);
  268 (* MINUS *);
  269 (* TIMESTAMP *);
  270 (* E *);
  271 (* R *);
  272 (* C *);
  273 (* LPAREN *);
  274 (* RPAREN *);
  275 (* COMMA *);
  276 (* COLON *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  264 (* FLOAT *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\002\000\003\000\003\000\003\000\003\000\
\003\000\005\000\005\000\005\000\006\000\006\000\006\000\006\000\
\008\000\008\000\008\000\007\000\007\000\004\000\004\000\000\000"

let yylen = "\002\000\
\002\000\001\000\005\000\006\000\001\000\004\000\003\000\002\000\
\004\000\004\000\004\000\001\000\003\000\004\000\004\000\004\000\
\004\000\004\000\001\000\004\000\001\000\001\000\001\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\002\000\000\000\024\000\000\000\000\000\
\000\000\000\000\000\000\005\000\000\000\001\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\022\000\023\000\000\000\
\000\000\000\000\012\000\000\000\000\000\000\000\000\000\006\000\
\000\000\000\000\009\000\003\000\004\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\011\000\010\000\
\000\000\021\000\000\000\000\000\000\000\019\000\000\000\000\000\
\000\000\000\000\014\000\000\000\000\000\015\000\016\000\000\000\
\000\000\000\000\020\000\017\000\018\000"

let yydgoto = "\002\000\
\006\000\007\000\013\000\024\000\028\000\041\000\051\000\055\000"

let yysindex = "\004\000\
\254\254\000\000\250\254\000\000\011\255\000\000\031\000\254\254\
\022\255\023\255\011\255\000\000\246\254\000\000\013\255\036\255\
\000\255\030\255\011\255\254\254\254\254\000\000\000\000\024\255\
\026\255\027\255\000\000\028\255\030\255\029\255\031\255\000\000\
\020\255\020\255\000\000\000\000\000\000\033\255\034\255\035\255\
\009\255\014\255\019\255\016\255\016\255\020\255\000\000\000\000\
\037\255\000\000\038\255\040\255\041\255\000\000\042\255\043\255\
\044\255\036\255\000\000\036\255\019\255\000\000\000\000\045\255\
\046\255\047\255\000\000\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\048\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\049\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\051\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\248\255\253\255\226\255\000\000\224\255\240\255\003\000"

let yytablesize = 69
let yytable = "\015\000\
\019\000\042\000\025\000\026\000\001\000\003\000\004\000\018\000\
\020\000\027\000\008\000\030\000\031\000\057\000\005\000\029\000\
\009\000\010\000\052\000\046\000\053\000\049\000\011\000\012\000\
\046\000\054\000\047\000\064\000\050\000\065\000\014\000\048\000\
\021\000\038\000\039\000\040\000\022\000\023\000\016\000\017\000\
\019\000\032\000\033\000\034\000\066\000\035\000\036\000\056\000\
\037\000\043\000\044\000\045\000\000\000\058\000\046\000\059\000\
\060\000\061\000\000\000\062\000\063\000\000\000\067\000\068\000\
\069\000\000\000\008\000\007\000\013\000"

let yycheck = "\008\000\
\011\001\034\000\003\001\004\001\001\000\008\001\009\001\011\000\
\019\001\010\001\017\001\020\000\021\000\046\000\017\001\019\000\
\006\001\007\001\003\001\011\001\005\001\003\001\012\001\013\001\
\011\001\010\001\018\001\058\000\010\001\060\000\000\000\018\001\
\020\001\014\001\015\001\016\001\001\001\002\001\017\001\017\001\
\011\001\018\001\017\001\017\001\061\000\018\001\018\001\045\000\
\018\001\017\001\017\001\017\001\255\255\017\001\011\001\018\001\
\017\001\017\001\255\255\018\001\018\001\255\255\018\001\018\001\
\018\001\255\255\019\001\019\001\018\001"

let yynames_const = "\
  SIZE_MAX\000\
  SIZE_SUM\000\
  SUM\000\
  MIN\000\
  CP\000\
  DATA\000\
  ELABEL\000\
  RANDOM\000\
  COUNT\000\
  PLUS\000\
  MINUS\000\
  TIMESTAMP\000\
  E\000\
  R\000\
  C\000\
  LPAREN\000\
  RPAREN\000\
  COMMA\000\
  COLON\000\
  EOF\000\
  "

let yynames_block = "\
  FLOAT\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'strategy) in
    Obj.repr(
# 51 "selectionParser.mly"
              ( _1 )
# 182 "selectionParser.ml"
               : SelectionStrategy.t))
; (fun __caml_parser_env ->
    Obj.repr(
# 54 "selectionParser.mly"
          (SS.Random)
# 188 "selectionParser.ml"
               : 'strategy))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'node) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'strategy) in
    Obj.repr(
# 55 "selectionParser.mly"
                                     (SS.Property(_2, _4))
# 196 "selectionParser.ml"
               : 'strategy))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 : 'strategy) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'strategy) in
    Obj.repr(
# 56 "selectionParser.mly"
                                               (
  let f = float_of_string _1 in
  SS.Probability(f, _3, _5))
# 207 "selectionParser.ml"
               : 'strategy))
; (fun __caml_parser_env ->
    Obj.repr(
# 61 "selectionParser.mly"
             (SS.Age)
# 213 "selectionParser.ml"
               : 'node))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'term_pair) in
    Obj.repr(
# 62 "selectionParser.mly"
                                (SS.DataProperty _3)
# 220 "selectionParser.ml"
               : 'node))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'node) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'node) in
    Obj.repr(
# 63 "selectionParser.mly"
                  (SS.Plus(_1, _3))
# 228 "selectionParser.ml"
               : 'node))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'node) in
    Obj.repr(
# 64 "selectionParser.mly"
              (SS.Negative _2)
# 235 "selectionParser.ml"
               : 'node))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'process_set) in
    Obj.repr(
# 65 "selectionParser.mly"
                                    (SS.ELabel _3)
# 242 "selectionParser.ml"
               : 'node))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'process) in
    Obj.repr(
# 68 "selectionParser.mly"
                             (SS.PMin _3)
# 249 "selectionParser.ml"
               : 'process_set))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'process) in
    Obj.repr(
# 69 "selectionParser.mly"
                             (SS.PSum _3)
# 256 "selectionParser.ml"
               : 'process_set))
; (fun __caml_parser_env ->
    Obj.repr(
# 70 "selectionParser.mly"
         (SS.Count)
# 262 "selectionParser.ml"
               : 'process_set))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'process) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'process) in
    Obj.repr(
# 73 "selectionParser.mly"
                        (SS.PPlus(_1, _3))
# 270 "selectionParser.ml"
               : 'process))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'equations) in
    Obj.repr(
# 74 "selectionParser.mly"
                             (SS.EProjection _3)
# 277 "selectionParser.ml"
               : 'process))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'trs) in
    Obj.repr(
# 75 "selectionParser.mly"
                       (SS.RProjection _3)
# 284 "selectionParser.ml"
               : 'process))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'trs) in
    Obj.repr(
# 76 "selectionParser.mly"
                       (SS.CProjection _3)
# 291 "selectionParser.ml"
               : 'process))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'term_pair) in
    Obj.repr(
# 79 "selectionParser.mly"
                               (SS.TSum _3)
# 298 "selectionParser.ml"
               : 'trs))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'equations) in
    Obj.repr(
# 80 "selectionParser.mly"
                              (SS.CPs _3)
# 305 "selectionParser.ml"
               : 'trs))
; (fun __caml_parser_env ->
    Obj.repr(
# 81 "selectionParser.mly"
         (SS.Size)
# 311 "selectionParser.ml"
               : 'trs))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'term_pair) in
    Obj.repr(
# 84 "selectionParser.mly"
                               (SS.ESum _3)
# 318 "selectionParser.ml"
               : 'equations))
; (fun __caml_parser_env ->
    Obj.repr(
# 85 "selectionParser.mly"
         (SS.Cardinal)
# 324 "selectionParser.ml"
               : 'equations))
; (fun __caml_parser_env ->
    Obj.repr(
# 88 "selectionParser.mly"
            (SS.MaxSize)
# 330 "selectionParser.ml"
               : 'term_pair))
; (fun __caml_parser_env ->
    Obj.repr(
# 89 "selectionParser.mly"
            (SS.SumSize)
# 336 "selectionParser.ml"
               : 'term_pair))
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
   (Parsing.yyparse yytables 1 lexfun lexbuf : SelectionStrategy.t)
