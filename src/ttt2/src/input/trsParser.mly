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
open Util;;
open TrsSyntax;;

(*** GLOBALS ******************************************************************)
let vars = ref [];;
%}

/*** DECLARATIONS *************************************************************/
%token ARROW
%token ARROW_EQUAL
%token ARROW_LR
%token COMMA
%token CONTEXTSENSITIVE
%token EOF
%token EQUAL
%token EQUATIONS
%token INNERMOST
%token LEFT_PAREN
%token OUTERMOST
%token PIPE
%token RIGHT_PAREN
%token RULES
%token STRATEGY
%token THEORY
%token VAR
%token <string> ID
%token <string> STRING

%start trs
%type <TrsSyntax.t> trs

/*** RULES ********************************************************************/
%%
trs:
   LEFT_PAREN VAR variables RIGHT_PAREN trs {$5}
 | LEFT_PAREN THEORY theories RIGHT_PAREN trs {add_theories $3 $5}
 | LEFT_PAREN RULES rules RIGHT_PAREN trs {add_rules $3 $5}
 | LEFT_PAREN STRATEGY strategy RIGHT_PAREN trs {set_strategy $3 $5}
 | LEFT_PAREN ID comments RIGHT_PAREN trs {$5}
 | EOF {empty}
;

comments:
   {}
 | VAR comments {}
 | RULES comments {}
 | THEORY comments {}
 | STRATEGY comments {}
 | EQUATIONS comments {}
 | INNERMOST comments {}
 | OUTERMOST comments {}
 | CONTEXTSENSITIVE comments {}
 | LEFT_PAREN comments RIGHT_PAREN comments {}
 | COMMA comments {}
 | PIPE comments {}
 | ARROW comments {}
 | ARROW_EQUAL comments {}
 | ARROW_LR comments {}
 | EQUAL comments {}
 | STRING comments {}
 | ID comments {}
;

variables:
   {}
 | ID variables {vars := $1 :: !vars}
;

theories:
   {[]}
 | LEFT_PAREN theory RIGHT_PAREN theories {$2 :: $4}
;

theory:
   ID ids {Theory ($1,$2)}
 | EQUATIONS equations {Equations $2}
;

ids:
   {[]}
 | ID ids {$1 :: $2}
;

equations:
   {[]}
 | equation equations {$1 :: $2}
;

equation:
   term EQUAL term {($1,$3)}
;

rules:
   {[]}
 | rule rules {$1 :: $2}
;

rule:
   term ARROW term {Strict ($1,$3,[])}
 | term ARROW term PIPE conditions {Strict ($1,$3,$5)}
 | term ARROW_EQUAL term {Weak ($1,$3,[])}
 | term ARROW_EQUAL term PIPE conditions {Weak ($1,$3,$5)}
;

conditions:
   condition {[]}
 | condition COMMA conditions {$1 :: $3}
;

condition:
   term ARROW term {Strong ($1,$3)}
 | term ARROW_LR term {Low ($1,$3)}
;

term:
   ID {if List.mem $1 !vars then Var $1 else Fun ($1,[])}
 | ID LEFT_PAREN RIGHT_PAREN {Fun ($1,[])}
 | ID LEFT_PAREN terms RIGHT_PAREN {Fun ($1,$3)}
;

terms:
   term {[$1]}
 | term COMMA terms {$1 :: $3}
;

strategy:
   INNERMOST {Innermost}
 | OUTERMOST {Outermost}
 | CONTEXTSENSITIVE strategies {Contextsensitive $2}
;

strategies:
   {[]}
 | LEFT_PAREN ID integers RIGHT_PAREN strategies {($2,$3) :: $5}
;

integers:
   {[]}
 | ID integers {int_of_string $1 :: $2}
;

/*** TRAILER ******************************************************************/
%%
