
(*** OPENS ***************************************************************)
open Util;;

(*** SUBMODULES **********************************************************)
module Var = Rewriting.Variable;;
module Fun = Rewriting.Function;;
module M = U.Monad;;
module L = U.Label;;
module T = U.Term;;
module R = U.Rule;;
module Trs = U.Trs;;

(*** TYPES ***************************************************************)
type symbol_theory =
 | AC of Fun.t
 | ACU of (Fun.t * Fun.t) (* f, zero *)

type t = symbol_theory list

(*** OPENS (2) ***********************************************************)
open M;;

(*** FUNCTIONS ***********************************************************)
let ac f =
 let x,y,z = Var.zero, Var.next Var.zero, Var.next (Var.next Var.zero) in
 let s,t = T.Fun(f,[T.Var x; T.Var y]), T.Fun(f,[T.Var y; T.Var x]) in
 let r1 = Equation.of_terms s t in
 let s = T.Fun(f,[T.Fun(f,[T.Var x; T.Var y]); T.Var z]) in
 let t = T.Fun(f,[T.Var x; T.Fun(f,[T.Var y; T.Var z])]) in
 let r2 = Equation.of_terms s t in
 [r1; r2]
;;

let assoc f =
 let x,y,z = Var.zero, Var.next Var.zero, Var.next (Var.next Var.zero) in
 let s = T.Fun(f,[T.Fun(f,[T.Var x; T.Var y]); T.Var z]) in
 let t = T.Fun(f,[T.Var x; T.Fun(f,[T.Var y; T.Var z])]) in
 Equation.of_terms s t
;;

let comm f =
 let x,y = Var.zero, Var.next Var.zero in
 let s,t = T.Fun(f,[T.Var x; T.Var y]), T.Fun(f,[T.Var y; T.Var x]) in
 Equation.of_terms s t
;;

let c_variant ff rl =
 match Equation.terms rl with
  | (T.Fun(f,[T.Var x; T.Var y]), T.Fun(f',[T.Var y'; T.Var x'])) 
   when x=x' && y=y' && f=f' && x<>y && f=ff -> true
  | _ -> false
;;

let c_symbols rl =
 match Equation.terms rl with
  | (T.Fun(f,[T.Var x; T.Var y]), T.Fun(f',[T.Var y'; T.Var x'])) 
   when x=x' && y=y' && f=f' && x<>y -> [f]
  | _ -> []
;;

let a_variant ff rl =
 let a_var rl = match Equation.terms rl with
  | T.Fun(f,[T.Fun(f',[T.Var x; T.Var y]); T.Var z]),
    T.Fun(g,[T.Var x'; T.Fun(g',[T.Var y'; T.Var z'])]) 
    when x=x' && y=y' && z=z' && x<>y && x<>z && y<>z &&
         f=f' && f=g && g=g' && f=ff -> true
  | _ -> false
 in a_var rl || (a_var (Equation.invert rl))
;;

let a_symbols rl = 
 let a_sym rl = match Equation.terms rl with
  | T.Fun(f,[T.Fun(f',[T.Var x; T.Var y]); T.Var z]),
    T.Fun(g,[T.Var x'; T.Fun(g',[T.Var y'; T.Var z'])])
    when x=x' && y=y' && z=z' && x<>y && x<>z && y<>z &&
         f=f' && f=g && g=g' -> [f]
  | _ -> []
 in a_sym rl @ (a_sym (Equation.invert rl))
;;

let recognize_ac eqs =
 let sc = List.flat_map c_symbols eqs in
 let sa = List.flat_map a_symbols eqs in
 let ss = List.intersect sc sa in
 let ac_variant_for rl f = a_variant f rl || c_variant f rl in
 let no_ac_variant rl = not (List.exists (ac_variant_for rl) ss) in
 let eqs' = List.filter no_ac_variant eqs in
 eqs',ss
;; 

let acu = 
 fresh_var >>= fun x ->
 M.fresh_fun >>= M.create_fun_name >>= (M.create_fun 2) >>= fun f ->
 M.fresh_fun >>= M.create_fun_name >>= (M.create_fun 0) >>= fun z ->
 let r1 = R.of_terms (T.Fun(f,[T.Var x; T.Fun(z,[])])) (T.Var x) in
 let r2 = R.of_terms (T.Fun(f,[T.Fun(z,[]);T.Var x])) (T.Var x) in
 return (r1,r2)
;;

let acu_variant rl =
 match R.to_terms rl with
  | (T.Fun(f,[T.Var x; T.Fun(z,[])]), T.Var x') when x=x' -> true
  | (T.Fun(f,[T.Fun(z,[]); T.Var x]), T.Var x') when x=x' -> true
  | _ -> false
;;

let acu_symbols rl =
 match R.to_terms rl with
  | (T.Fun(f,[T.Var x; T.Fun(z,[])]), T.Var x') when x=x' -> (f,z)
  | (T.Fun(f,[T.Fun(z,[]); T.Var x]), T.Var x') when x=x' -> (f,z)
  | _ -> failwith "no acu rule"
;;

let recognize_acu trs =
 acu >>= fun (r1,r2) ->
 match
  (try Some (List.find acu_variant (Trs.to_list trs)) 
   with Not_found -> None)
 with
  | None -> return []
  | Some rl ->
   let f,z = acu_symbols rl in
   M.is_theory L.AC f >>= fun f_is_ac ->
   if f_is_ac then return [ACU(f,z)] else return []
;;

let is_acu_symbol f = function
 [] -> false
 | (ACU (f',_)) :: _ when f = f' -> true
 | _ -> false
;;

let unit_for f = function
 [] -> failwith "no unit exists" 
 | (ACU (f',z)) :: _ when f = f' -> z
 | _ -> failwith "unexpected pattern in unit_for"
;;
