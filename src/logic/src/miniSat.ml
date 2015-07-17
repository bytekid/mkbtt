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

(*** OPENS ********************************************************************)
open Monad;;
open Formula;;
open Util;;

(*** MODULES ******************************************************************)
module I = Interface;;
module Idx = Index.Standard (Int);;

(*** FUNCTIONS ****************************************************************)
(* monadic if then else *)
let mite mx ma mb = mx >>= fun x -> if x then ma else mb;;
(* lift sloppy *)
let slift2 f am bm = am >>= fun a -> bm >>= fun b -> f a b;;
let slift3 f am bm cm = am >>= fun a -> bm >>= fun b -> cm >>= fun c -> f a b c;;
(* return monadic value *)
let m x = return x;;


(********************** interaction with state ********************************)
let cache_bool tbl k = get >>= fun s -> 
 let s' = if not (Hashtbl.mem tbl k) then (
  Hashtbl.add tbl k (prop s.State.id);
  State.incr_id s) else s
 in
 set s' >> m (Hashtbl.find tbl k);
;;

(********************** Plaisted/Greenbaum transformation *********************)

(* Plaisted/Greenbaum transformation *)
(* associate every non-atomic subformula with a fresh variable *)
let tseitin_var (tbl, _) f = if is_literal f then m f else cache_bool tbl f;;

let w_not x         = Not x;;
let w_and x y       = And (x, y);;
let w_or x y        = Or (x, y);;
(*let w_implies x y = Implies (x, y);; *)
let w_iff x y       = Iff (x, y);;

(* cnf if [z] appears positively *)
let simp_p z = function
 | Not x             -> [[w_not x; w_not z]]
 | And (x, y)        -> [[x; w_not z]; [y; w_not z]]
 | Or  (x, y)        -> [[x; y; Not z]]
 (*| Implies  (x, y) -> [[Not x; y; Not z]] *)
 | Iff (x, y)        -> [[Not x; y; Not z]; [x; Not y; Not z]]
 | _ -> failwith "unknown operand occured"
;;

(* cnf if [z] appears negatively *)
let simp_n z = function
 | Not x             -> [[x; z]]
 | And (x, y)        -> [[w_not x; w_not y; z]]
 | Or  (x, y)        -> [[Not x; z]; [Not y; z]]
 (*| Implies  (x, y) -> [[x; z]; [Not y; z]] *)
 | Iff (x, y)        -> [[x; y; z]; [Not x; Not y; z]]
 | _ -> failwith "unknown operand occured"
;;

(* simplify according to polarity *)
let simp = function
 | 0 -> simp_p 
 | 1 -> simp_n
 | 2 -> fun z f -> List.concat [simp_p z f; simp_n z f]
 | _ -> failwith "value not allowed"
;;

(* alternate polarity *)
let alt = function
 | 0 -> 1
 | 1 -> 0
 | 2 -> 2
 | _ -> failwith "value not allowd"
;;

(* transform unary operators *)
let rec tseitin1 tbl acc p p1 f x z =
 tseitin_var tbl x >>= fun x' -> 
 tseitin_var tbl z >>= fun z' ->
 tseitin tbl (simp p z' (f x') @ acc) p1 x
 
(* transform binary operators *)
and tseitin2 tbl acc p p1 p2 f x y z =
 tseitin_var tbl x >>= fun x' -> 
 tseitin_var tbl y >>= fun y' ->
 tseitin_var tbl z >>= fun z' ->
 tseitin tbl (simp p z' (f x' y') @ acc) p1 x >>= fun acc ->
 tseitin tbl acc p2 y

(* transform formula to cnf *)
and tseitin ((_, t) as tbl) acc p z =
 if Hashtbl.mem t (z,p) then return acc
 else (
  Hashtbl.add t (z,p) ();
  if is_literal z then return acc
  else match z with
   | Not x            -> tseitin1 tbl acc p (alt p) w_not x z
   | And (x, y)       -> tseitin2 tbl acc p p p w_and x y z
   | Or (x, y)        -> tseitin2 tbl acc p p p w_or x y z
   (*| Implies (x, y) -> tseitin2 tbl acc p (alt p) p w_implies x y z *)
   | Iff (x, y)       -> tseitin2 tbl acc p 2 2 w_iff x y z
   | _ -> failwith ("illegal case: %s" ^ (to_string_p z))
);;

(*
let transform_cnf s f = 
 (*let p = 2 in (* tseitin *) *)
 let p = 0 in (* plaisted/greenbaum *)
 let tbl = (Hashtbl.create 1024, Hashtbl.create 1024) in
 List.unique_hash ([tseitin_var s tbl f] :: tseitin tbl [] p f)
;;
*)

let rec to_clause f = 
 if is_literal f then [f] else match f with
  | Or (x,y) -> to_clause x @ to_clause y
  | _         -> failwith "to_clause"
;;

let is_clause f = 
 try
  ignore (to_clause f);
  true
 with
  | Failure "to_clause" -> false
;;

let transform_cnf' acc tbl f = 
 (*let p = 2 in (* tseitin *) *)
 let p = 0 in (* plaisted/greenbaum *)
  if is_clause f then m (to_clause f::acc) else
 (*is_literal f      >>= fun il -> if il then f >>= fun f -> return [[f]] else *)
  tseitin_var tbl f   >>= fun f0 ->
  tseitin tbl acc p f >>= fun f1 ->
  return ([f0]::f1)
;;

let rec transform_cnf acc tbl = function
 | And (x,y) -> 
  transform_cnf acc tbl x >>= fun acc ->
  transform_cnf acc tbl y
 | f         -> transform_cnf' acc tbl f
;;

let transform_cnf f = get >>= fun c ->
 transform_cnf [] c.State.tt_tbl f >>= (return <.> List.unique_hash)
;;

(********************** transforming diophantine to propositional *************)
let cache tbl f k = 
 if Hashtbl.mem tbl k then m (Hashtbl.find tbl k)
 else (f k >>= fun v -> Hashtbl.add tbl k v; m v)
;;

let rec tp o fm = fm >>= function
 | Bot       -> m Bot
 | Top       -> m Top
 | P _ as p  -> m p
 | Not x     -> lift w_not (tpc o (m x))
 | And (x,y) -> lift2 w_and (tpc o (m x)) (tpc o (m y))
 | Or (x,y)  -> lift2 w_or (tpc o (m x)) (tpc o (m y))
 | Iff (x,y) -> lift2 w_iff (tpc o (m x)) (tpc o (m y))
 | Gt (x, y) -> slift2 BinNumber.gt (tac o (m x)) (tac o (m y))
 | Ge (x, y) -> slift2 BinNumber.ge (tac o (m x)) (tac o (m y))
 | Eq (x, y) -> slift2 BinNumber.eq (tac o (m x)) (tac o (m y))
 | Obits (n,x) -> tp n (m x)
and ta o fm = fm >>= function
 | A a         -> BinNumber.of_arith a
 | C i         -> m (BinNumber.of_number i)
 | Fresh a     -> ta o (return a) >>= BinNumber.use_fresh_bits
 | Add (a,b)   -> slift2 (BinNumber.add ~obits:o) (tac o (m a)) (tac o (m b))
 | Sub (a,b)   -> slift2 (BinNumber.sub ~obits:o) (tac o (m a)) (tac o (m b))
 | Mul (a,b)   -> slift2 (BinNumber.mul ~obits:o) (tac o (m a)) (tac o (m b))
 | Ite (x,a,b) -> slift3 BinNumber.ite (tpc o (m x)) (tac o (m a)) (tac o (m b))
 | Min (a,b)   -> slift2 BinNumber.min (tac o (m a)) (tac o (m b))
 | Max (a,b)   -> slift2 BinNumber.max (tac o (m a)) (tac o (m b))
and tac o fm = fm >>= fun f -> get >>= fun s -> cache s.State.ta_tbl (fun _ -> ta o fm) f
and tpc o fm = fm >>= fun f -> get >>= fun s -> cache s.State.tp_tbl (fun _ -> tp o fm) f
;; 

let tp fm = get_obits () >>= fun o -> tp o fm;;
let ta fm = get_obits () >>= fun o -> ta o fm;;

(********************** calling Minisat Interface begin ***********************)
exception Unsatisfiable;;
exception Literal_superfluent;;
exception Disjunct_superfluent;;

let index x idx = 
 ignore (Idx.index x idx);
 Idx.find_key x idx
;;

(* lit2int: maps literal to integers (DIMACS format)*)
let rec lit2int vars = function
 | Bot | Not Top -> raise Literal_superfluent
 | Top | Not Bot -> raise Disjunct_superfluent
 | P x           -> (index x vars) + 1
 | Not (Not l)   -> lit2int vars l 
 | Not l         -> let i = lit2int vars l in (-1) * i
 | _             -> (* non-literal formula in lit2int *) 
  raise (Match_failure ("PLog.lit2int", 0, 0))
;;

let to_dimacs clause vars = 
 let rec to_dimacs res vars = function
  | []    -> res
  | l::ls ->
   try to_dimacs (lit2int vars l::res) vars ls with
    | Literal_superfluent  -> to_dimacs res vars ls
    | Disjunct_superfluent -> [0] (* 0 = unused index *)
 in
 to_dimacs [] vars clause
;;

let send_clause s = function
 | []  -> raise Unsatisfiable
 | [0] -> () (* clause superfluent  *)
 | xs  -> I.add s (Array.of_list xs) (List.length xs)
;;

let solve_aux cnf =
 let idx = Idx.empty 512 in
 let s = I.get_solver () in
 (* let t0 = Sys.time () in *)
(* Format.printf "sending %d clauses to minisat: %f\n" (List.length cnf)
(Sys.time () -. t0); *)
 List.iter (fun c -> send_clause s (to_dimacs c idx)) cnf;
 let vars = Idx.elements idx in
 (* let t1 = Sys.time () in *)
 (* Format.printf "%d clauses (%d variables) sent : %f (start
 solving)\n%!" *)
  (* (List.length cnf) (List.length vars) (t1 -. t0); *)
 let answer = 
  let a = I.is_sat s in
   (* Format.printf "(pure) solving time: %f\n" (Sys.time () -. t1); *)
  if a = 1 then begin
   let assign = 
    List.fold_left
     (fun acc x -> 
      if I.val_of s (index x idx) = 1 then
       Assignment.add_p (P x) true acc
      else
       Assignment.add_p (P x) false acc)
     Assignment.empty
     vars
   in
   (*Format.printf "(extended) solving time: %f\n" (Sys.time () -. t1); *)
   Some assign
  end else
   None
 in
 ignore (I.delete_solver s);
 answer
;;
(********************** calling Minisat Interface end *******************)
let is_var = function
 | Formula.P _ 
 | Formula.Not (Formula.P _) -> true
 | _ -> false
;;

let solve f =
 (*Format.printf "Time before solve: %f@\n%!" (Sys.time ()); *)
 tp (return f) >>= fun f -> 
 (*Format.printf "Time : %f@\n%!" (Sys.time ()); *)
 get_side_constraint () >>= fun sc ->
 let phi = f <&> sc in
 if !(Formula.print_formula) then 
   (Format.printf "@[%a@]" Formula.fprintf_p (Formula.simplify_b phi);
   return None)
 else
 transform_cnf phi >>= fun cnf ->
 (*Format.printf "after cnf (%d clauses, %d variables): %f@\n%!"  *)
  (*(List.length cnf) (List.length (List.filter is_var (List.flatten
  cnf))) (Sys.time ()); *)
 return (
  try let r = solve_aux cnf in
  (*Format.printf "Time after solve: %f@\n%!" (Sys.time ()); *)
   r
  with | Unsatisfiable -> None)
;;

let rec ea a ass = match a with
 | A l         -> ta (return a) >>= (return <.> (flip BinNumber.eval ass))
 | C r         -> return r
 | Fresh a     -> eval_a a ass
 | Add (a,b)   -> lift2 Number.add (eval_a a ass) (eval_a b ass)
 | Sub (a,b)   -> lift2 Number.sub (eval_a a ass) (eval_a b ass)
 | Mul (a,b)   -> lift2 Number.mul (eval_a a ass) (eval_a b ass)
 | Ite (x,a,b) -> mite (eval_p x ass) (eval_a a ass) (eval_a b ass)
 | Min (a,b)   -> lift2 Number.min (eval_a a ass) (eval_a b ass)
 | Max (a,b)   -> lift2 Number.max (eval_a a ass) (eval_a b ass)
and ep f ass = match f with
 | Top       -> return true
 | Bot       -> return false
 | P _       -> return (try Assignment.find_p f ass with | Not_found -> false)
 | Not x     -> lift not (eval_p x ass)
 | And (x,y) -> lift2 (&&) (eval_p x ass) (eval_p y ass)
 | Or (x,y)  -> lift2 (||) (eval_p x ass) (eval_p y ass)
 | Iff (x,y) -> lift2 (=) (eval_p x ass) (eval_p y ass) 
 | Eq (a,b)  -> lift2 Number.eq (eval_a a ass) (eval_a b ass)
 | Gt (a,b)  -> lift2 Number.gt (eval_a a ass) (eval_a b ass)
 | Ge (a,b)  -> lift2 Number.ge (eval_a a ass) (eval_a b ass)
 | Obits(_,x) -> ep x ass
and eval_a a ass = get >>= fun s -> 
 cache s.State.ea_tbl (flip ea ass) a >>= fun va ->
 (* let _ = Format.printf "%a : %a@\n%!" Formula.fprintf_a a
 Number.fprintf va in *)
 return va
and eval_p p ass = get >>= fun s -> 
 cache s.State.ep_tbl (flip ep ass) p >>= fun vp ->
 (* let _ = Format.printf "%a : %b@\n%!" Formula.fprintf_p p vp in *)
 return vp
;;

