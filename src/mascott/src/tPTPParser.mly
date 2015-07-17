%{
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

%}
%token <string> LSYM
%token <string> USYM
%token INPUT_CLAUSE
%token CNF
%token AXIOM
%token HYPOTHESIS
%token CONJECTURE
%token NEGCONJECTURE
%token EQUAL
%token EQUALSIGN
%token DISEQUALSIGN
%token LPAREN
%token RPAREN
%token LBRACKET
%token RBRACKET
%token PLUSPLUS
%token MINUSMINUS
%token COMMA
%token DOT
%token SLASH
%token QUOTE
%token INCLUDE
%token EOF

%start start
%type <TPTPFormat.element list> start

%%
start: 
elements { $1 }

elements:
  one_clause elements { $1 :: $2 }
| include_axioms elements { $1 :: $2 }
| EOF { [] }

include_axioms:
INCLUDE LPAREN QUOTE USYM SLASH USYM QUOTE RPAREN DOT 
 {TPTPFormat.Include($4, $6)}

one_clause:
| annotated_formula LPAREN LSYM COMMA clause_status COMMA LPAREN formula RPAREN RPAREN DOT
{ TPTPFormat.InputClause($3, $5, $8) }

annotated_formula:
  CNF {}
| INPUT_CLAUSE {}

clause_status:
AXIOM {TPTPFormat.Axiom}
| HYPOTHESIS {TPTPFormat.Hypothesis}
| CONJECTURE {TPTPFormat.Conjecture}
| NEGCONJECTURE {TPTPFormat.NegatedConjecture}

formula:
PLUSPLUS EQUAL LPAREN term COMMA term RPAREN { TPTPFormat.Equality($4, $6) }
| MINUSMINUS EQUAL LPAREN term COMMA term RPAREN { TPTPFormat.DisEquality($4, $6) }
| term EQUALSIGN term { TPTPFormat.Equality($1, $3) }
| term DISEQUALSIGN term { TPTPFormat.DisEquality($1, $3) }

term:
| LSYM LPAREN term_list RPAREN { TPTPFormat.Fun($1, $3)  }
| LSYM { TPTPFormat.Fun ($1,[]) }
| USYM { TPTPFormat.Var $1 }
;


term_list:
   term { [$1] }
 | term COMMA term_list { $1 :: $3 }
;



