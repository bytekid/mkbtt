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
open ConfSyntax;;
%}

/*** DECLARATIONS *************************************************************/
%token BACKSLASH
%token EOF
%token EQUAL
%token NEWLINE
%token SHARP
%token <string> ID

%start conf
%type <ConfSyntax.t> conf

/*** RULES ********************************************************************/
%%
conf :
   EOF {empty}
 | NEWLINE conf {$2}
 | SHARP comment conf {$3}
 | ID EQUAL strategy conf {add_abbreviation ($1,$3) $4}
;

comment :
   {}
 | NEWLINE {}
 | BACKSLASH comment {}
 | EQUAL comment {}
 | ID comment {}
 | SHARP comment {}
;

strategy :
   {""}
 | BACKSLASH NEWLINE strategy {$3}
 | ID strategy {if $2 = "" then $1 else $1^" "^$2}
;

/*** TRAILER ******************************************************************/
%%
