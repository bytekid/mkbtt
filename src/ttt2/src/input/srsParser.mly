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
open SrsSyntax;;

(*** GLOBALS ******************************************************************)
let x = Var "x";;
%}

/*** DECLARATIONS *************************************************************/
%token ARROW
%token ARROW_EQUAL
%token COMMA
%token EOF
%token LEFTMOST
%token LEFT_PAREN
%token RIGHTMOST
%token RIGHT_PAREN
%token RULES
%token STRATEGY
%token <string> ID
%token <string> STRING

%start srs
%type <SrsSyntax.t> srs

/*** RULES ********************************************************************/
%%
srs:
   LEFT_PAREN RULES rules RIGHT_PAREN srs {add_rules $3 $5}
 | LEFT_PAREN STRATEGY strategy RIGHT_PAREN srs {set_strategy $3 $5}
 | LEFT_PAREN ID comments RIGHT_PAREN srs {$5}
 | EOF {empty}
;

comments:
   {}
 | LEFTMOST comments {}
 | RIGHTMOST comments {}
 | RULES comments {}
 | STRATEGY comments {}
 | LEFT_PAREN comments RIGHT_PAREN comments {}
 | COMMA comments {}
 | ARROW comments {}
 | ARROW_EQUAL comments {}
 | STRING comments {}
 | ID comments {}
;

rules:
   rule {[$1]}
 | rule COMMA {[$1]}
 | rule COMMA rules {$1 :: $3}
;

rule:
   term ARROW term {Strict ($1,$3)}
 | term ARROW {Strict ($1,x)}
 | term ARROW_EQUAL term {Weak ($1,$3)}
 | term ARROW_EQUAL {Weak ($1,x)}
;

term:
   ID {Fun ($1,[x])}
 | ID term {Fun ($1,[$2])}
;

strategy:
   LEFTMOST {Leftmost}
 | RIGHTMOST {Rightmost}
;

/*** TRAILER ******************************************************************/
%%
