# 20 "selectionLexer.mll"
 
 (* lexing selection strategies *)
 open SelectionParser

# 7 "selectionLexer.ml"
let __ocaml_lex_tables = {
  Lexing.lex_base = 
   "\000\000\233\255\234\255\235\255\236\255\237\255\238\255\239\255\
    \241\255\243\255\244\255\245\255\246\255\247\255\000\000\000\000\
    \000\000\000\000\000\000\001\000\002\000\000\000\001\000\000\000\
    \255\255\004\000\254\255\253\255\009\000\252\255\251\255\005\000\
    \005\000\250\255\249\255\016\000";
  Lexing.lex_backtrk = 
   "\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\022\000\013\000\
    \022\000\015\000\022\000\022\000\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\007\000";
  Lexing.lex_default = 
   "\001\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \000\000\255\255\000\000\000\000\255\255\000\000\000\000\255\255\
    \255\255\000\000\000\000\255\255";
  Lexing.lex_trans = 
   "\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \003\000\000\000\000\000\012\000\000\000\000\000\000\000\000\000\
    \007\000\006\000\009\000\011\000\005\000\010\000\035\000\000\000\
    \014\000\014\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\004\000\000\000\000\000\000\000\000\000\013\000\
    \035\000\035\000\035\000\035\000\035\000\035\000\035\000\035\000\
    \035\000\035\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\031\000\023\000\017\000\016\000\015\000\033\000\000\000\
    \000\000\028\000\000\000\000\000\034\000\018\000\022\000\027\000\
    \030\000\026\000\008\000\019\000\021\000\025\000\020\000\029\000\
    \024\000\032\000\000\000\000\000\000\000\000\000\000\000\000\000\
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
    \002\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000";
  Lexing.lex_check = 
   "\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \000\000\255\255\255\255\000\000\255\255\255\255\255\255\255\255\
    \000\000\000\000\000\000\000\000\000\000\000\000\014\000\255\255\
    \000\000\000\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\000\000\255\255\255\255\255\255\255\255\000\000\
    \035\000\035\000\035\000\035\000\035\000\035\000\035\000\035\000\
    \035\000\035\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\016\000\022\000\000\000\000\000\000\000\032\000\255\255\
    \255\255\018\000\255\255\255\255\015\000\000\000\019\000\020\000\
    \017\000\025\000\000\000\000\000\019\000\021\000\019\000\028\000\
    \023\000\031\000\255\255\255\255\255\255\255\255\255\255\255\255\
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
    \000\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255";
  Lexing.lex_base_code = 
   "";
  Lexing.lex_backtrk_code = 
   "";
  Lexing.lex_default_code = 
   "";
  Lexing.lex_trans_code = 
   "";
  Lexing.lex_check_code = 
   "";
  Lexing.lex_code = 
   "";
}

let rec token lexbuf =
    __ocaml_lex_token_rec lexbuf 0
and __ocaml_lex_token_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 25 "selectionLexer.mll"
           (SIZE_MAX)
# 120 "selectionLexer.ml"

  | 1 ->
# 26 "selectionLexer.mll"
           (SIZE_SUM)
# 125 "selectionLexer.ml"

  | 2 ->
# 27 "selectionLexer.mll"
          (SUM)
# 130 "selectionLexer.ml"

  | 3 ->
# 28 "selectionLexer.mll"
          (MIN)
# 135 "selectionLexer.ml"

  | 4 ->
# 29 "selectionLexer.mll"
         (CP)
# 140 "selectionLexer.ml"

  | 5 ->
# 30 "selectionLexer.mll"
           (DATA)
# 145 "selectionLexer.ml"

  | 6 ->
# 31 "selectionLexer.mll"
         (ELABEL)
# 150 "selectionLexer.ml"

  | 7 ->
# 32 "selectionLexer.mll"
                             (FLOAT(Lexing.lexeme lexbuf))
# 155 "selectionLexer.ml"

  | 8 ->
# 33 "selectionLexer.mll"
        (RANDOM)
# 160 "selectionLexer.ml"

  | 9 ->
# 34 "selectionLexer.mll"
        (COUNT)
# 165 "selectionLexer.ml"

  | 10 ->
# 35 "selectionLexer.mll"
        (PLUS)
# 170 "selectionLexer.ml"

  | 11 ->
# 36 "selectionLexer.mll"
        (MINUS)
# 175 "selectionLexer.ml"

  | 12 ->
# 37 "selectionLexer.mll"
        (TIMESTAMP)
# 180 "selectionLexer.ml"

  | 13 ->
# 38 "selectionLexer.mll"
        (E)
# 185 "selectionLexer.ml"

  | 14 ->
# 39 "selectionLexer.mll"
        (R)
# 190 "selectionLexer.ml"

  | 15 ->
# 40 "selectionLexer.mll"
        (C)
# 195 "selectionLexer.ml"

  | 16 ->
# 41 "selectionLexer.mll"
        (LPAREN)
# 200 "selectionLexer.ml"

  | 17 ->
# 42 "selectionLexer.mll"
        (RPAREN)
# 205 "selectionLexer.ml"

  | 18 ->
# 43 "selectionLexer.mll"
        (COMMA)
# 210 "selectionLexer.ml"

  | 19 ->
# 44 "selectionLexer.mll"
        (COLON)
# 215 "selectionLexer.ml"

  | 20 ->
# 45 "selectionLexer.mll"
          (token lexbuf)
# 220 "selectionLexer.ml"

  | 21 ->
# 46 "selectionLexer.mll"
          (EOF)
# 225 "selectionLexer.ml"

  | 22 ->
# 47 "selectionLexer.mll"
      (token lexbuf)
# 230 "selectionLexer.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf; __ocaml_lex_token_rec lexbuf __ocaml_lex_state

;;
