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

(* parser for selection strategy *)
module SS = SelectionStrategy;;
%}
%token SIZE_MAX
%token SIZE_SUM
%token SUM
%token MIN
%token CP
%token DATA
%token ELABEL
%token <string> FLOAT
%token RANDOM
%token COUNT
%token PLUS
%token MINUS
%token TIMESTAMP
%token E
%token R
%token C
%token LPAREN
%token RPAREN
%token COMMA
%token COLON
%token EOF

%start start
%type <SelectionStrategy.t> start

%%
start:
 strategy EOF { $1 }

strategy:
   RANDOM {SS.Random}
 | LPAREN node COMMA strategy RPAREN {SS.Property($2, $4)}
 | FLOAT LPAREN strategy COLON strategy RPAREN {
  let f = float_of_string $1 in
  SS.Probability(f, $3, $5)}

node:
   TIMESTAMP {SS.Age}
 | DATA LPAREN term_pair RPAREN {SS.DataProperty $3}
 | node PLUS node {SS.Plus($1, $3)}
 | MINUS node {SS.Negative $2}
 | ELABEL LPAREN process_set RPAREN {SS.ELabel $3} 

process_set:
   MIN LPAREN process RPAREN {SS.PMin $3}
 | SUM LPAREN process RPAREN {SS.PSum $3}
 | COUNT {SS.Count}

process:
   process PLUS process {SS.PPlus($1, $3)}
 | E LPAREN equations RPAREN {SS.EProjection $3}
 | R LPAREN trs RPAREN {SS.RProjection $3}
 | C LPAREN trs RPAREN {SS.CProjection $3}

trs:
   SUM LPAREN term_pair RPAREN {SS.TSum $3}
 | CP LPAREN equations RPAREN {SS.CPs $3}
 | COUNT {SS.Size}

equations:
   SUM LPAREN term_pair RPAREN {SS.ESum $3}
 | COUNT {SS.Cardinal}

term_pair:
   SIZE_MAX {SS.MaxSize}
 | SIZE_SUM {SS.SumSize} 

