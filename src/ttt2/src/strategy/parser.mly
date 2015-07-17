/* Copyright 2008 Martin Korp, Christian Sternagel, Harald Zankl
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
 */

/*** HEADER *******************************************************************/
%{
(*** OPENS ********************************************************************)
open Syntax;;
%}

/*** DECLARATIONS *************************************************************/
%token AND
%token COLON
%token COMMA
%token ELSE
%token EOF
%token EXCLAMATION_MARK
%token GREATER
%token IF
%token LEFT_BRACE
%token LEFT_BRACKET
%token LEFT_PAREN
%token PERCENT
%token PIPE
%token PLUS
%token QUESTION_MARK
%token RIGHT_BRACE
%token RIGHT_BRACKET
%token RIGHT_PAREN
%token SEMICOLON
%token SMALLER
%token STAR
%token THEN
%token <float> FLOAT
%token <string> ID
%token <int> INT

/* lowest precedence */
%nonassoc ELSE IF THEN
%left PIPE
%left AND COLON SEMICOLON
%left EXCLAMATION_MARK PERCENT PLUS QUESTION_MARK STAR
%left COMMA
/* highest precedence */

%start strategy
%type <Syntax.t> strategy

/*** RULES ********************************************************************/
%%
strategy:
   ID args {Strategy ($1,$2)}
 /* specifiers */
 | strategy PERCENT {Stop $1}
 | strategy EXCLAMATION_MARK {Strict $1}
 | strategy LEFT_BRACKET INT RIGHT_BRACKET {Timed (float_of_int $3,$1)}
 | strategy LEFT_BRACKET FLOAT RIGHT_BRACKET {Timed ($3,$1)}
 | LEFT_BRACE strategy RIGHT_BRACE ID args {Modify ($2,($4,$5))}
 /* iterators */
 | strategy QUESTION_MARK {Optional $1}
 | strategy STAR {Iterate $1}
 | strategy PLUS {Repeat $1}
 | strategy INT STAR {Replicate ($2,$1)}
 | strategy LEFT_BRACKET INT RIGHT_BRACKET STAR
  {Iterate_timed (float_of_int $3,$1)}
 | strategy LEFT_BRACKET FLOAT RIGHT_BRACKET STAR {Iterate_timed ($3,$1)}
 /* combinators */
 | strategy SEMICOLON strategy {Combine ($1,$3)}
 | strategy PIPE strategy {Choose ($1,$3)}
 | strategy PIPE PIPE strategy {Parallel ($1,$4)}
 | IF condition THEN strategy ELSE strategy {Condition ($2,$4,$6)}
 /* miscellaneous */
 | LEFT_PAREN strategy RIGHT_PAREN {$2}
;

condition:
   ID args {Atom ($1,$2)}
 | EXCLAMATION_MARK condition {Not $2}
 | condition AND condition {And ($1,$3)}
 | condition PIPE condition {Or ($1,$3)}
 | LEFT_PAREN condition RIGHT_PAREN {$2}
;

args:
   {[]}
 | ID args {$1 :: $2}
 | INT args {string_of_int $1 :: $2}
 | FLOAT args {string_of_float $1 :: $2}
;

/*** TRAILER ******************************************************************/
%%
