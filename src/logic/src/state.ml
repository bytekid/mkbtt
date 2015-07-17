(* Copyright 2008 Martin Korp, Christian Sternagel, Harald Zankl
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
 *)

(*** FUNCTIONS ****************************************************************)
open Util;;

 type t = {
  id  : int;                          (* id for variables *)
  (* MiniSat use only *)
  tbl : (int, BinType.t) Hashtbl.t;   (* map id of variable to bin *)
  ta_tbl : (Formula.a, BinType.t) Hashtbl.t; (* chache transformation *)
  tp_tbl : (Formula.p, Formula.p) Hashtbl.t; (* chache transformation *)
  tt_tbl : ((Formula.p, Formula.p) Hashtbl.t*(Formula.p*int,unit)Hashtbl.t); (* tseiting transformation *)
  ea_tbl : (Formula.a, Number.t) Hashtbl.t; (* chache evaluation*)
  ep_tbl : (Formula.p, bool) Hashtbl.t;     (* chache evaluateion*)
  eay_tbl : (Formula.a, Number.t) Hashtbl.t; (* chache evaluation yices *)
  epy_tbl : (Formula.p, bool) Hashtbl.t;     (* chache evaluateion yices *)
  sc  : (Formula.p, bool) Hashtbl.t;     (* side conditions *)
  dbits : int;                           (* bits for after decimal point for intermediate results *)
  obits : int;                           (* bits for intermediate results *)
  (* MiniSat+ use only *)
  count : int ref; (* variable counter *)
  gen_tbl : (int*Formula.arith, (int*int) list) Hashtbl.t 
 };; 
 let incr_id t = {t with id = succ t.id};;
 let get_tbl t = t.tbl;;
 let init dbits obits = {
   id = 0;
   tbl = Hashtbl.create 512; 
   ta_tbl = Hashtbl.create 512; 
   tp_tbl = Hashtbl.create 512; 
   tt_tbl = (Hashtbl.create 512,Hashtbl.create 512); 
   ea_tbl = Hashtbl.create 512; 
   ep_tbl = Hashtbl.create 512; 
   eay_tbl = Hashtbl.create 512; 
   epy_tbl = Hashtbl.create 512; 
   sc = Hashtbl.create 512;
   dbits = dbits;
   obits = obits;
   count = ref 0;
   gen_tbl   = Hashtbl.create 512;
 };;
