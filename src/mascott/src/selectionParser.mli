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

val start :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> SelectionStrategy.t
