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

val start :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> TPTPFormat.element list
