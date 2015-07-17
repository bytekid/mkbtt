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
module F = Formula;;
open Yinterface;;
open Monad;;
open Util;;

type ctx = {
 con : Yinterface.yices_context;
 vars : (int,Yinterface.yices_var_decl * string) Hashtbl.t;
 ta_tbl : (F.a, Yinterface.yices_expr) Hashtbl.t;
 tf_tbl : (F.p,Yinterface.yices_expr) Hashtbl.t;
};;

(* create and delete context *)
let init () = 
 yices_enable_type_checker (1);
 {
  con = yices_mk_context ();
  vars = Hashtbl.create 512;
  ta_tbl = Hashtbl.create 512;
  tf_tbl = Hashtbl.create 512;
}

let finalize ctx = 
 yices_del_context ctx.con;
 Hashtbl.clear ctx.vars;
;;

let decl t ctx x =
 let ty = yices_mk_type ctx.con t in
 if not (Hashtbl.mem ctx.vars x) then begin
  let xdecl = yices_mk_var_decl ctx.con (string_of_int x) ty in
  Hashtbl.add ctx.vars x (xdecl, t);
 end;
;;

let args x y = 
 let args = Array.make 2 x in
  args.(0) <- x;
  args.(1) <- y;
 args
;;

(* comparisons *)
let eq ctx a b = yices_mk_eq ctx.con a b;;
let ge ctx a b = yices_mk_ge ctx.con a b;;
let gt ctx a b = yices_mk_gt ctx.con a b;;

let ite ctx c t e = yices_mk_ite ctx.con c t e;;

(* boolean operators *)
let bot ctx = yices_mk_false ctx.con;;
let top ctx = yices_mk_true ctx.con;;
let conj ctx x y = yices_mk_and ctx.con (args x y) 2;;
let disj ctx x y = yices_mk_or ctx.con (args x y) 2;;
let neg ctx x = yices_mk_not ctx.con x;;
let impl c x y = disj c (neg c x) y;;
let iff c x y = conj c (impl c x y) (impl c y x);;

(* arithmetic *)
let add ctx x y = yices_mk_sum ctx.con (args x y) 2;;
let sub ctx x y = yices_mk_sub ctx.con (args x y) 2;;
let mul ctx x y = yices_mk_mul ctx.con (args x y) 2;;

let to_int n = (int_of_string (Number.to_string n))
let of_number ctx r = yices_mk_num ctx.con (to_int r)
;;

let var ?(neg=false) t ctx x = 
 if not (Hashtbl.mem ctx.vars x) then (
  decl t ctx x;
  if neg || t = "bool" then () else (
   let z = yices_mk_num ctx.con 0 in
   let v = yices_mk_var_from_decl ctx.con (fst (Hashtbl.find ctx.vars x)) in
   yices_assert ctx.con (ge ctx v z);
  );
  );
 yices_mk_var_from_decl ctx.con (fst (Hashtbl.find ctx.vars x))
;;

let var_p = var "bool";;

let var_a con x = 
 let t = 
  if (F.a_spec x).F.rat <> 1 || (F.a_spec x).F.real then "real" else
   "int" in
 var ~neg:(F.a_spec x).F.neg t con (F.a_id x)
;;

let value ctx m x = 
 let (decl, t) = Hashtbl.find ctx.vars x in
 match t with
  | "int" -> Number.of_int (helper_get_int_value m decl)
  | "bool" -> Number.of_int (helper_get_value m decl)
  | "real" -> (* rat is subsumed by real *)
   Number.of_rat (helper_get_num_value m decl)
                 (helper_get_dnum_value m decl)
  | _ -> failwith "unknown variable declaration"
;;

let cache tbl f k =
 if not (Hashtbl.mem tbl k) then Hashtbl.add tbl k (f k);
 Hashtbl.find tbl k
;;

let rec tf ctx = function
 | F.Top -> top ctx
 | F.Bot -> bot ctx
 | F.P i -> var_p ctx i
 | F.Not (x) -> neg ctx (tfc ctx x)
 | F.And (x, y) -> conj ctx (tfc ctx x) (tfc ctx y)
 | F.Or (x, y) -> disj ctx (tfc ctx x) (tfc ctx y)
 (*| F.Implies (x, y) -> impl ctx (tfc ctx x) (tfc ctx y) *)
 | F.Iff (x, y) -> iff ctx (tfc ctx x) (tfc ctx y)
 | F.Eq (a, b) -> eq ctx (tc ctx a) (tc ctx b)
 | F.Ge (a, b) -> ge ctx (tc ctx a) (tc ctx b)
 | F.Gt (a, b) -> gt ctx (tc ctx a) (tc ctx b)
and t ctx = function
 (*| F.App (f, args) -> app ctx f (List.map (t ctx) args) *)
 | F.A a -> var_a ctx a
 | F.C r -> of_number ctx r
 | F.Fresh a -> tc ctx a
 | F.Add (a, b) -> add ctx (tc ctx a) (tc ctx b)
 | F.Sub (a, b) -> sub ctx (tc ctx a) (tc ctx b)
 | F.Mul (a, b) -> mul ctx (tc ctx a) (tc ctx b) 
 (*| F.SMul (i, b) -> mul ctx (of_int ctx i) (t ctx b) *)
 (*| F.Ite (x, C 1, C 0) ->  *)
 | F.Ite (x,a,b) -> ite ctx (tfc ctx x) (tc ctx a) (tc ctx b)
 | F.Max (a,b) -> tc ctx (F.Ite (F.Gt (a,b),a,b))
 | F.Min (a,b) -> tc ctx (F.Ite (F.Gt (a,b),b,a))
and tfc ctx x = cache ctx.tf_tbl (tf ctx) x
and tc ctx x = cache ctx.ta_tbl (t ctx) x
;;

let solve f =
 let t0 = Unix.gettimeofday() in
 let t _ = (Unix.gettimeofday () -. t0) in
 (*Format.eprintf "Entering Yices section@\n%!"; *)
 let ctx = init () in
 (*Format.eprintf "Before transforming %f@\n%!" (t ()); *)
 ignore (yices_assert ctx.con (tfc ctx f));
 (*Format.eprintf "Before solving %f@\n%!" (t ()); *)
 let r = yices_check_aux ctx.con in
 (*Format.eprintf "After solving %f@\n%!" (t ()); *)
 let assign = 
  if (r = ~-1) then
   None
  else 
   let m = yices_get_model ctx.con in
   (*ignore (yices_display_model m); *)
   let assign = Hashtbl.fold 
    (fun k v -> Assignment.add_a (Formula.arith k) (value ctx m k))
    ctx.vars
    Assignment.empty
   in Some assign
 in
 finalize ctx;
 (*Format.eprintf "Leaving Yices sections %f@\n%!" (t ()); *)
 return assign
;;

(* monadic caching *)
let cache tbl f k = 
 if Hashtbl.mem tbl k then return (Hashtbl.find tbl k)
 else (f k >>= fun v -> Hashtbl.add tbl k v; return v)
;;
let mite mx ma mb = mx >>= fun x -> if x then ma else mb;;

let rec ea a ass = match a with
 | F.A l         -> return (try Assignment.find_a a ass with | Not_found -> Number.of_int 0)
 | F.C r         -> return r
 | F.Fresh a     -> eval_a a ass
 | F.Add (a,b)   -> lift2 Number.add (eval_a a ass) (eval_a b ass)
 | F.Sub (a,b)   -> lift2 Number.sub (eval_a a ass) (eval_a b ass)
 | F.Mul (a,b)   -> lift2 Number.mul (eval_a a ass) (eval_a b ass)
 | F.Ite (x,a,b) -> mite (eval_p x ass) (eval_a a ass) (eval_a b ass)
 | F.Min (a,b)   -> lift2 Number.min (eval_a a ass) (eval_a b ass)
 | F.Max (a,b)   -> lift2 Number.max (eval_a a ass) (eval_a b ass)
and ep f ass = match f with
 | F.Top       -> return true
 | F.Bot       -> return false
 | F.P x       -> 
  let v = try Assignment.find_a (Formula.arith x) ass with | Not_found -> Number.zero in
  if v = Number.zero then return false 
  else if v = Number.one then return true 
  else failwith "propositional variable not propositional"
 | F.Not x     -> lift not (eval_p x ass)
 | F.And (x,y) -> lift2 (&&) (eval_p x ass) (eval_p y ass)
 | F.Or (x,y)  -> lift2 (||) (eval_p x ass) (eval_p y ass)
 | F.Iff (x,y) -> lift2 (=) (eval_p x ass) (eval_p y ass) 
 | F.Eq (a,b)  -> lift2 Number.eq (eval_a a ass) (eval_a b ass)
 | F.Gt (a,b)  -> lift2 Number.gt (eval_a a ass) (eval_a b ass)
 | F.Ge (a,b)  -> lift2 Number.ge (eval_a a ass) (eval_a b ass)
and eval_a a ass = get >>= fun s -> cache s.State.eay_tbl (flip ea ass) a
and eval_p p ass = get >>= fun s -> cache s.State.epy_tbl (flip ep ass) p
;;
