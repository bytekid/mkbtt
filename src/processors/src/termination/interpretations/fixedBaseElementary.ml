(* Copyright 2013 Sarah Winkler, Harald Zankl
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
open Util;;
open Rewritingx;;
open Logic.Operators;;

(*** MODULES (part 1) *********************************************************)
module F = Format;;
module Fun = Function;;
module Var = Variable;;
module Number = Logic.Number;;
module M = Matrix.Make(Coefficient);;
module Mono = Monomial.Make (M) (String);;
module P = Polynomial.Make (Mono);;
module Sig = Signature;;
(*** TYPES ********************************************************************)
(* shape: p + (base^exp)q *)
type e = { p:   P.t;   (*linear coefficients*)
           base: M.t;  (*** mostly ignored ***)
           exp:  t;
           q:  P.t;    (*non-linear coefficients*)
         }
and  t = Left of P.t | Right of e;;

type context = {
 arith              : Logic.arith;
 ds                 : (Fun.t,int) Hashtbl.t;
 interpretations    : (Fun.t, t) Hashtbl.t;
 ht_con             : (Variable.t * t, Logic.p * Logic.p) Hashtbl.t;
 ht_geq             : (t * t, Logic.p * Logic.p) Hashtbl.t;
 ht_gt              : (t * t, Logic.p * Logic.p) Hashtbl.t;
 ht_eq              : (t * t, Logic.p * Logic.p) Hashtbl.t;
 ht_avars           : (M.t, M.t) Hashtbl.t;
 ht_pvars           : (Logic.p,Logic.p) Hashtbl.t;
 ht_zero            : (t, Logic.p * Logic.p) Hashtbl.t;
 gt_encodings       : (Rule.t,Logic.p) Hashtbl.t;
 geq_encodings      : (Rule.t,Logic.p) Hashtbl.t;
 out_deg            : int;
 p_constraints      : Logic.p;
 p_compatible       : Logic.p;
 state              : Sig.t;
 subterm_encodings  : (Term.t,t * t) Hashtbl.t;
};;


(*** GLOBALS ******************************************************************)
let cache = ref false;;
let t_compose = ref 0.0;;
let t_compare = ref 0.0;;
let t_desired = ref "";;
let t_add = ref 0.0;;
let t_pi = ref false;;
let t_pc = ref false;;
let t_pm = ref false;;
let t_pp = ref false;; 
let t_itp = ref "";; 
let t_fresh = ref false;; 
let t_pd = ref false;; 
let t_eval = ref false;; 
let t_ob = ref max_int;; 
let t_fb = ref max_int;; 
let t_carrier = ref 2;; 
let t_base = ref 2;; 

(*** MODULES (part 2) *********************************************************)
module Statex = struct type t = context end;;
module Made = struct
 include Util.Monad.Transformer.State (Statex) (Logic.Monad);;
(* functions lifted from Logic into Made *)
let fresh_arith = get >>= fun s -> liftm (Logic.fresh_arith s.arith);;
let fresh_arith_spec arith = liftm (Logic.fresh_arith arith);;
let fresh_bool = get >>= fun s -> liftm Logic.fresh_bool;;
let nm = lift (~!);;
let eval_a a ass = a >>= fun a -> liftm (Logic.eval_a a ass);;
let eval_p p ass = p >>= fun p -> liftm (Logic.eval_p p ass);;
let map_op op f ls = sequence (List.map f ls) >>= (return <.> op);;
let map_and f = map_op Logic.big_and f;;

module Operators = struct
let ($&$) = lift2 (<&>);;
let ($|$) = lift2 (<|>);;
let ($->$) = lift2 (<->>);;
let ($<->$) = lift2 (<<->>);;
let ($<$) = lift2 (<<>);;
let ($>$) = lift2 (<>>);;
let ($>=$) = lift2 (<>=>);;
let ($<=$) = lift2 (<<=>);;
let ($+$) = lift2 (<+>);;
let iteM = lift3 Logic.ite
end;;
end;;

open Made;;
open Made.Operators;;

(*** FUNCTIONS ****************************************************************)
(* state monad interaction *)
let arity f = get >>= fun c -> return (Sig.find_ari f c.state);;
let name f = get >>= fun c -> return (Sig.find_fun_name f c.state);;
let var_name f = get >>= fun c -> return (Sig.find_var_name f c.state);;

(* to store formulas and associate them with a fresh variable *)
let cache_m tbl f k = 
 if Hashtbl.mem tbl k then return (Hashtbl.find tbl k)
 else (f k >>= fun v -> (Hashtbl.add tbl k v; return v))
;;

let print_timing () =
     F.printf "time in compose: %f\n%!" !t_compose;
     F.printf "time in compare: %f\n%!" !t_compare;
     F.printf "time in add: %f\n%!" !t_add;
;;


(*useful functions for matrix*)
let coeff_of_int n = M.make 1 1 (Logic.constant (Number.of_int n));;
let matrix_zero = coeff_of_int 0;;
let matrix_one = coeff_of_int 1;;
let matrix_two = coeff_of_int 2;;
let fresh_coeff = fresh_arith >>= fun c -> return (M.make 1 1 c);;
let fresh_coeff_spec spec = fresh_arith_spec spec >>= fun c -> return (M.make 1 1 c);;

let (%+%) = M.add;;
let (%*%) = M.mul;;
let (%>=%) m0 m1 = Logic.big_and (M.map2list (<>=>) m0 m1);;
let (%>%) m0 m1 = Logic.big_and (M.map2list (<>>) m0 m1);;
let (%=%) m0 m1 = Logic.big_and (M.map2list (<=>) m0 m1);;

let is_zero_matrix m = m %=% matrix_zero;;
let greater_zero_matrix m = m %>% matrix_zero;;
let ito b = M.make 1 1 (b <?> Logic.one <:> Logic.zero);;
 
let max_matrix m0 m1 = M.map2 Logic.max m0 m1;;
let min_matrix m0 m1 = M.map2 Logic.min m0 m1;;


(*useful functions for poly*)
let base _ = coeff_of_int !t_base;;
let carrier _ = coeff_of_int !t_carrier;;

let poly_zero = P.make [Mono.make matrix_zero []];;
let poly_one = P.make [Mono.make matrix_one []];;
let ite_poly b p q = P.add (P.scale (ito b) p) (P.scale (ito (~!b)) q);;

let is_zero_poly p = Logic.big_and (List.map is_zero_matrix (P.coefficients p));;
let greater_zero_poly p = Logic.neg (is_zero_poly p);;
let greater_one_poly p = 
 greater_zero_poly (P.non_constant_part p) <|> (Mono.coefficient (P.constant_part p) %>% matrix_one);;

let fresh_poly xs =
 let vars = List.map List.singleton xs @ [[]] in (*[[]] for constant part*)
 Made.sequence (List.map (fun _ -> fresh_coeff) vars) >>= fun ps ->
 return (P.make (List.map2 Mono.make ps vars))
;;
let extend_vars p xs = 
 List.foldr (fun x -> P.add (P.make [Mono.make matrix_zero x])) p xs
;;

let same_variables p0 p1 =
 let v0,v1 = Pair.map P.variables (p0,p1) in
 let x0,x1 = List.diff v1 v0, List.diff v0 v1 in
 (extend_vars p0 x0, extend_vars p1 x1)
;;

let combine_mono f m0 m1 =
 if Mono.variables m0 = Mono.variables m1 then
  Mono.make (f (Mono.coefficient m0) (Mono.coefficient m1)) (Mono.variables m0) 
 else failwith "FBI: combine_mono: variables do not match"
;;

let combine_poly f p0 p1 =
 let (p0,p1) = same_variables p0 p1 in
 P.make (P.map2 (combine_mono f) p0 p1)
;;

let greater_equal_mono m0 m1 = Mono.coefficient m0 %>=% Mono.coefficient m1;;
let greater_mono m0 m1 = Mono.coefficient m0 %>% Mono.coefficient m1;;

(*lift carrier: x -> x+min_elt_of_carrier*)
let sigma x = P.make [Mono.make matrix_one [x];Mono.make (carrier ()) []];;

let geq_poly p q = (* taking carrier into account *)
 let (p,q) = same_variables p q in
 let (p,q) = Pair.map (P.subst sigma) (p,q) in
 Logic.big_and (P.map2 greater_equal_mono p q)
;;

let gt_poly p q = (* taking carrier into account *)
 let (p,q) = same_variables p q in
 let (p,q) = Pair.map (P.subst sigma) (p,q) in
 let (c0,c1) = Pair.map P.constant_part (p,q) in
 Logic.big_and (greater_mono c0 c1::P.map2 greater_equal_mono p q)
;; 


(*FBIs start here *)
let side_conditions () =
 let p v a = Format.printf "%a <-> %a\n%!" Logic.fprintf_p v Logic.fprintf_p a in
 let f ht =
  Hashtbl.fold (fun _ (v,a) phi -> ((*p v a;*) phi <&> (v <<->> a))
  ) ht Logic.top
 in
 let f' cmp ht = Hashtbl.fold (fun c v p -> p <&> (cmp v c)) ht Logic.top in
 let f'' cmp ht = Hashtbl.fold (fun _ (v,c) p -> p <&> (cmp v c)) ht Logic.top in
 get >>= fun c ->
 let sc = ((f c.ht_eq) <&> (f c.ht_geq) <&> (f c.ht_gt) <&> (f c.ht_con) 
 <&> (f c.ht_zero) <&> (f' (<<->>) c.ht_pvars) <&> (f' (%=%) c.ht_avars)
 <&> c.p_constraints <&> c.p_compatible) in
 return sc
;;

let add_sc phi = get >>= fun c ->
 set {c with p_constraints= (c.p_constraints <&> phi)}
;;

(*not side-effect free*)
let eval txt p = 
 if not !t_eval then (return p)
 else
  (return p) $&$ side_conditions () >>= fun phi ->
  add_sc p >>= fun _ ->
  let a = Logic.run ~obits:!t_ob (Logic.solve phi) in
  let b = match a with 
   | None -> false
   | Some _ -> true
  in Format.printf "%s: %b@\n@?" txt b;
  return p
;;

let eval_sc txt p = eval txt p >>= fun _ -> add_sc p >> return p;;

(*constructors *)
let one = Left poly_one;;
let make_o p base exp q = {p=p; base=base; exp=exp; q=q;};;
let make p base exp q = Right (make_o p base exp q);;
let make_var x = Left (P.make [Mono.make matrix_one [x];Mono.make matrix_zero []]);;
let make_exp exp = Some (make_o poly_zero (base ()) exp poly_one);;

(* destructors *)
let p o    = o.p;; 
(* let base o = o.base;; *)
let exp o  = o.exp;;
let q o    = o.q;;
let getx x p = try fst (List.find (fun (_,y) -> x = y) (P.split p)) with
 | Not_found -> matrix_zero
;;

let px x o = getx x (p o);;
let p0 o = px [] o;;
let qx x o = getx x (q o);;
let q0 o = px [] o;;

(* printers *)
let fprintf_p fmt p =
 let c0 = try Mono.coefficient (P.constant_part p) with | Not_found -> matrix_zero in
 let cs = P.coefficients (P.non_constant_part p) in
 F.fprintf fmt "%a" M.fprintf_intp ("",List.length cs,cs,c0)
;;
 
let z p = is_zero_poly p = Logic.top;;

let rec fprintf fmt = function
 | Left p -> fprintf_p fmt p
 | Right o -> 
  if (z (p o) && z (q o)) then F.fprintf fmt "0" else (
  if (z (p o)) then () else F.fprintf fmt "%a" fprintf_p (p o);
  if (z (p o) || z (q o)) then () else F.fprintf fmt " + ";
  if (z (q o)) then () else F.fprintf fmt "%a^(%a)(%a)" M.fprintf (base ()) fprintf (exp o) fprintf_p (q o)
)

(*for debugging *)
let rec fprintf_debug fmt = function
  | Left p -> fprintf_p fmt p
  | Right o -> 
     F.fprintf fmt "%a" P.fprintf (p o) ;
      F.fprintf fmt " + %a^(%a)(%a)" M.fprintf (base ()) fprintf_debug (exp o) P.fprintf (q o)
;;

let fprintf fmt = (if !t_pd then fprintf_debug else fprintf) fmt;;

let fprintf_m fmt m = M.fprintf fmt m;;

let fprintf_intp_p fmt (f,o) = F.fprintf fmt "@[[%s] = %a@]" f fprintf o;;

let fprintf_intp fmt fs = Monad.return (List.fprintf fprintf_intp_p "@\n" fmt fs);;

(* useful functions *)
let rec vars = function
 | Left p -> P.variables p
 | Right o -> P.variables (p o) @ P.variables (q o) @ vars (exp o)
;;

let vars = List.unique <.> vars;;

let rec depth = function Left _ -> 0 | Right o -> 1 + (depth o.exp) 

let rec subst sigma = function
 | Left p -> Left (P.subst sigma p)
 | Right o -> make (P.subst sigma (p o)) (base ()) (subst sigma (exp o)) (P.subst sigma (q o))
;;

let rec get_vars acc s n = 
 if n <= 0 then return acc
 else get_vars (("x"^s)::acc) (s^"1") (n-1)
;;

(*variables must be sorted; otherwise a different polynomial might be constructed*)
let get_vars n =  
 get_vars [] "" n >>= fun xs ->
 return (List.sort Pervasives.compare xs)
;;

(*test if [c] occurs in !t_desired*)
let desired c = String.contains !t_desired c;;

let rec heuristic n fs trs =
 if fs = [] then max 0 (n-2) (*to have degree 1 for constructors and add*)
 else
  let (cand,trs) = Trs.partition (fun r -> List.mem (Rule.left_root r) fs) trs in
  heuristic (n+1) (Trs.right_funs cand) trs
;;

let set_degree_f trs d f  = 
 get >>= fun c -> name f >>= fun fn -> 
 let d = if desired 'd' then min d (heuristic 0 [f] trs) else d in
  if !t_pd then F.printf "degree(%s)=%d@\n" fn d;
 Hashtbl.add c.ds f d;
 return ()
;;


(*for constructing interpretations *)
let make_poly cs vs = P.make (List.map2 Mono.make cs vs);;

let zero_poly xs =
 let zs = List.map (fun _ -> matrix_zero) xs in
 make_poly zs xs
;;

let makel ps xs = return (Left (make_poly ps xs));;

let makee ps xs f' qs ys = f' >>= fun f' ->
 return (make (make_poly ps xs) (base ()) f' (make_poly qs ys))
;;

let make_pow xs f' = f' >>= fun f' -> 
 let zs = List.map (fun _ -> matrix_zero) xs in
 return (make (make_poly zs xs) (base ()) f' (make_poly (List.tl zs@[matrix_one]) xs))
;;

(*get coefficients with concrete integer values or variables fixed to a value*)
let get_fixed_coeff () =
 if !t_fresh then
  let fix var value = var %=% (coeff_of_int value) in
  get >>= fun c ->
  let arith = (Logic.nat 2) in
  fresh_coeff_spec arith >>= fun a0 -> 
  fresh_coeff_spec arith >>= fun a1 -> 
  fresh_coeff_spec arith >>= fun a2 ->
  fresh_coeff_spec arith >>= fun a3 ->
  add_sc (fix a0 0 <&> fix a1 1 <&> fix a2 2 <&> fix a3 3) >>
  return [a0;a1;a2]
 else
  return (List.gen (fun i -> coeff_of_int i) 4) 
;;

let coeffs n = Made.sequence (List.gen (fun _ -> fresh_coeff) n)

let rec make_fresh d xs f = 
 if d <= 0 then 
  fresh_poly xs >>= fun p ->
  return (Left p)
 else (
  if desired '2' then
   fresh_poly xs >>= fun poly ->
   fresh_bool >>= fun b ->
   return (P.scale (ito b) poly, P.scale (ito (~!b)) poly) 
  else
   fresh_poly xs >>= fun p ->
   fresh_poly xs >>= fun q ->
   return (p,q)
  ) >>= fun (p,q) ->
  make_fresh (d-1) xs f >>= fun exp ->
  return (make p (base ()) exp q)
;;

let dummy make f = 
 if !t_itp = "" then make f else
 get >>= fun c ->
 name f >>= fun fn ->
 get_fixed_coeff () >>= fun [a0;a1;a2;a3] ->
 get_vars 3 >>= fun [x;y;z] -> 
 let v0 = [[]] and v1 = [[x];[]] and v2 = [[x];[y];[]] and v3 = [[x];[y];[z];[]] in 
 let c0 = matrix_zero in
 let c1 = matrix_one in
 let c2 = matrix_two in
 match !t_itp with
  | "mul" -> (match fn with
   | "0"    -> makel [a2] v0
   | "s"    -> makel [a1;a1] v1
   | "add"  -> makel [a2;a1;a0] v2
   (* | "mul"  -> makee [a0;a0;a0] v2 (makel [a1;a0;a0] v2) [a0;a1;a0] v2 *)
   | _ -> make f
  )
  | "mul2" -> (match fn with
   | "0"    -> makee [a0] v0 (makel [a0] v0) [a1] v0
   | "s"    -> makel [a1;a1] v1
   | "add"  -> makee [a2;a1;a0] v2 (makel [a0;a0;a0] v2) [a0;a0;a0] v2
   | "mul"  -> makee [a0;a1;a1] v2 (makel [a1;a1;a0] v2) [a0;a0;a1] v2
   | _ -> make f
  )
  | "lescanne" -> (match fn with
   | "0"    -> makel [a2] v0
   | "s"    -> makel [a1;a2] v1
   | "add"  -> makel [a2;a1;a1] v2
   | "mul"  -> makee [a0;a0;a0] v2 (makel [a1;a0;a0] v2) [a0;a1;a0] v2
   | "fact"  -> make_pow v1 (make_pow v1 (makel [a1;a0] v1))
  )
  | "lucas" -> (match fn with
   | "0"    -> makel [a2] v0
   | "s"    -> makel [a1;a1] v1
   | "add"  -> makel [a1;a2;a0] v2
   | "mul"  -> makee [a0;a0;a0] v2 (makel [a1;a0;a0] v2) [a0;a1;a0] v2
   | "fact"  ->  make_pow v1 (make_pow v1 (makel [a1;a0] v1))
  )
  | "lucas2" -> (match fn with
   | "0"    -> makel [a2] v0
   | "s"    -> makel [a1;a1] v1
   | "add"  -> makel [a1;a2;a0] v2
   | "mul"  -> makee [a0;a1;a1] v2 (makel [a1;a0;a0] v2) [a0;a1;a0] v2
   | "fact"  -> makee [a1;a1] v1 (makel [a1;a0] v1) [a1;a0] v1
  )
  | "lucas3" -> (match fn with
   | "0"    -> makel [a1] v0
   | "s"    -> makel [a1;a1] v1
   | "add"  -> makel [a1;a2;a0] v2
   | "mul"  -> makee [a0;a0;a0] v2 (makel [a1;a0;a0] v2) [a0;a1;a0] v2
   | "fact"  ->  make_pow v1 (make_pow v1 (makel [a1;a0] v1))
  )
  | "lucasn" -> (match fn with
   | "0"    -> coeffs 1 >>= fun cs -> makel cs v0
   | "s"    -> coeffs 2 >>= fun cs -> makel cs v1
   | "add"  -> coeffs 3 >>= fun cs -> makel cs v2
   | "mul"  -> coeffs 3 >>= fun cs -> coeffs 3 >>= fun ds -> coeffs 3 >>= fun es ->
               makee cs v2 (makel ds v2) es v2
   | "fact" -> coeffs 2 >>= fun cs -> coeffs 2 >>= fun es ->
               coeffs 2 >>= fun cs1 -> coeffs 2 >>= fun es1 ->
               coeffs 2 >>= fun cs2 -> coeffs 2 >>= fun es2 ->
               makee cs v1 (makee cs1 v1 (makel cs2 v1) es1 v1) es v1
  )
  | "lucasm" -> (match fn with
   | "0"    -> make_fresh 1 (List.concat v0) f
   | "s"    -> make_fresh 1 (List.concat v1) f
   | "add"  -> make_fresh 1 (List.concat v2) f 
   | "mul"  -> make_fresh 2 (List.concat v2) f
   | "fact" -> make_fresh 3 (List.concat v1) f
  )
  | "exp" -> (match fn with
   | "0"    -> makel [a2] v0
   | "s"    -> makel [a1;a2] v1
   | "add"  -> makel [a2;a1;a1] v2
   | "mul"  -> makee [a0;a0;a0] v2 (makel [a1;a0;a0] v2) [a0;a1;a0] v2
   | _ -> make_fresh 3 (List.concat v2) f
)
  | "toyama" -> (match fn with
   | "0" -> makel [a2] v0
   | "1" -> makee [a1] v0 (makee [a3] v0 (makel [a0] v0) [a1] v0) [a1] v0
   (* | "f" -> makee [a0;a1;a1;a0] v3 (makee [a0;a0;a0;a1] v3 (makel [a0;a0;a0;a0] v3) [a0;a0;a0;a2] v3 ) [a1;a0;a2;a0] v3 *)
   | "f" -> makee [a0;a1;a1;a0] v3 (makee [a0;a0;a0;a1] v3 (makel [a0;a0;a0;a0] v3) [a0;a0;a0;a2] v3 ) [a1;a0;a2;a0] v3
   | _ -> make_fresh 3 (List.concat v2) f
)
  | _ -> failwith (F.sprintf "interpretation '%s' not known" !t_itp)
;;

let make_interpretation f = get >>= fun c ->
 arity f >>= fun a ->
 name f >>= fun fn ->
 get_vars a >>= fun xs ->
 let d = Hashtbl.find c.ds f in
 dummy (make_fresh d xs) f >>= fun i ->
 if !t_pi then F.printf "@[%a@]@\n@?" fprintf_intp_p (fn,i);
 return i
;;

let interpretation f = get >>= fun c ->
 cache_m c.interpretations make_interpretation f
;;

(*desired constraints (to reduce search space) *)
let rec cx x = function
 | Left p -> [getx x p]
 | Right o -> px x o::qx x o::cx x (exp o)
;;

let rec at_most_one = function
 | [] -> Logic.top
 | x::xs -> (x <->> ~!(Logic.big_or xs)) <&> at_most_one xs
;;

let at_most_one_gt_zero cs = at_most_one (List.map greater_zero_matrix cs);;

let restrict_shape = function
  | Left p -> return Logic.top
  | (Right _) as o -> let xs = List.filter (fun x -> x <> []) (vars o) in
  return (Logic.big_and (List.map (fun x -> at_most_one_gt_zero (cx x o)) xs))
;;

let rec restrict_q = function
 | Left p -> return Logic.top
 | Right o -> return (at_most_one_gt_zero (P.coefficients (q o))) $&$ restrict_q (exp o)
;;

let rec is_zero = function
 | Left p -> is_zero_poly p
 | Right o -> is_zero_poly (p o) <&> is_zero_poly (q o) 
;;

let rec restrict_exp = function
 | Left p -> return Logic.top
 | Right o -> return (is_zero_poly (q o) <->> is_zero (exp o)) $&$ restrict_exp (exp o)
;;

let rec all_zero = function
 | Left p -> is_zero_poly p
 | Right o -> is_zero (Right o) <&> all_zero (exp o)
;;

let rec restrict_exp_all = function
 | Left p -> return Logic.top
 | Right o -> return (is_zero_poly (q o) <->> all_zero (exp o)) $&$ restrict_exp (exp o)
;;

let ifdesired n phi = if desired n then return phi else return Logic.top;;

let desired_f f =  
 interpretation f >>= fun o ->
 (*d uses heuristic for degree*)
 restrict_shape o >>= ifdesired '1' $&$
 (*2 uses flipped variables for p and q*)
 restrict_q o >>= ifdesired '3' $&$ 
 restrict_exp o >>= ifdesired '4' $&$
 restrict_exp_all o >>= ifdesired '5' $&$
 (*6 uses approximation for multiplication of polynomials*)
 return Logic.top
;;

(* encoding *)
(* upper bound for two FBIs *)
let rec omax f g =
 let mmax = combine_poly max_matrix in
 let omax = function
  | Left p , Left q -> Left (mmax p q)
  | Left g , (Right o) (*fallthrough*)
  | (Right o), Left g -> make (mmax g (p o)) (base ()) (exp o) (q o)
  | Right f, Right g -> make (mmax (p f) (p g)) (base f) (omax (exp f) (exp g)) (mmax (q f) (q g))
 in omax (f,g)
;;

(* lower bound for two FBIs *)
let rec omin f g =
 let mmin = combine_poly min_matrix in
 let omin = function
  | Left p, Left q -> Left (mmin p q)
  | Left g, Right o (*fallthrough*)
  | Right o, Left g-> make (mmin g (p o)) (base ()) (exp o) (q o)
  | Right f, Right g -> make (mmin (p f) (p g)) (base f) (omin (exp f) (exp g)) (mmin (q f) (q g)) 
 in omin (f,g)
;;

let dup x = x,x;;

let scale fi g = match g with
 | Left p -> Left (P.scale fi p)
 | Right g -> make (P.scale fi (p g)) (base ()) (exp g) (P.scale fi (q g))
;;

let rec ite b ff gg = 
 match (ff,gg) with
  | Left p, Left q-> Left (ite_poly b p q)
  | Left _, Right _ -> ite (~! b) gg ff
  | Right f, Left g ->  
   let p = ite_poly b (p f) g in
   let q = ite_poly b (q f) poly_zero in
   make p (base ()) (exp f) q
  | Right f, Right g ->
   let p = ite_poly b (p f) (p g) in
   let q = ite_poly b (q f) (q g) in
   make p (base ()) (ite b (exp f) (exp g)) q
;;

(* f + g *) 
let add ff gg =
 match ff, gg with
  | Left p, Left q -> return (dup (Left (P.add p q)))
  | Left g, Right o (*fallthrough*)
  | Right o,Left g -> return (dup (make (P.add g (p o)) (base ()) (exp o) (q o)))
  | Right f, Right g ->
   let p' = P.add (p f) (p g) in
   let q' = P.add (q f) (q g) in
   (* upper bound *)
   let uexp = ite (is_zero_poly (q f)) (exp g)
               (ite (is_zero_poly (q g)) (exp f) (omax (exp f) (exp g))) in 
   let u = make p' (base ()) uexp q' in
   (* lower bound *)
   let lexp = ite (is_zero_poly (q f)) (exp g)
               (ite (is_zero_poly (q g)) (exp f) (omin (exp f) (exp g))) in 
   let l = make p' (base ()) lexp q' in
   return (u,l)
;;

let add f g =
 let t = Unix.gettimeofday () in
 add f g >>= fun (u,l) ->
 t_add := !t_add +. (Unix.gettimeofday () -. t);
 return (u,l) 
;;


(* [mult expo o] = expo \cdot o*)
let mult expo = function
 | Left p -> return (dup (make (zero_poly (P.variables p)) (base ()) expo p))
 | Right f -> 
  add (exp f) expo >>= fun (expu,expl) ->
  (* upper *)
  let u = 
   (* SW: change if-then-else to ite encoding *)
   let z = make poly_zero (base ()) expo (p f) in
   let nz = make poly_zero (base ()) expu (P.add (p f) (q f)) in
   ite (is_zero_poly (q f)) z nz in 
  (*lower*)
  let l = 
   (* SW: change if-then-else to ite encoding *)
   let z = make poly_zero (base ()) expo (p f) in
   let nz = make poly_zero (base ()) expl (q f) in
   ite (is_zero_poly (q f)) z nz in
  let _ = if !t_pm then Format.printf "...expo %a@\n...mult %a@\n... multu %a@\n... multl %a@\n" 
    fprintf expo fprintf (Right f) fprintf u fprintf l in
  return (u,l)
;;

let rec big2 f = function
 | [xs] -> return (dup xs)
 | x::xs -> big2 f xs >>= fun (u,l) ->
  f x u >>= fun (u,_) ->
  f x l >>= fun (_,l) ->
  return (u,l)
;;

let compose_p_one op gs p = 
 let gis = List.map2 (fun gi fi -> scale fi gi) ([one]@gs) (P.coefficients p) in
 big2 op gis
;; 

let compose_p op gs p =
 compose_p_one op (List.map fst gs) p >>= fun (u,_) ->
 compose_p_one op (List.map snd gs) p >>= fun (_,l) ->
 return (u,l)
;;
 
let rec compose d gs = function
 | Left p -> compose_p add gs p 
 | Right f ->
   let _ = if !t_pp then F.printf "%sf: %a@\n" d fprintf (Right f) in
  compose_p add gs (p f) >>= fun (psu,psl) ->
  compose_p add gs (q f) >>= fun (qsu,qsl) ->
  compose (d^".") gs (exp f) >>= fun (expu,expl) ->
  (*upper*)
  mult expu qsu >>= fun (tmpu,_) -> 
  add psu tmpu >>= fun (u,_) -> 
   let _ = if !t_pp then List.iteri (fun i gi -> F.printf "%sgu%d: %a@\n" d i fprintf (fst gi)) gs in
   let _ = if !t_pp then F.printf "%spsu: %a@\n" d fprintf psu in
   let _ = if !t_pp then F.printf "%sqsu: %a@\n" d fprintf qsu in
   let _ = if !t_pp then F.printf "%sexpu: %a@\n" d fprintf expu in
   let _ = if !t_pp then F.printf "%stmpu: %a@\n" d fprintf tmpu in
   let _ = if !t_pp then F.printf "%su: %a@\n" d fprintf u in
  (*lower*)
  mult expl qsl >>= fun (_,tmpl) ->
  add psl tmpl >>= fun (_,l) ->
   let _ = if !t_pp then List.iteri (fun i gi -> F.printf "%sgl%d: %a@\n" d i fprintf (snd gi)) gs in
   let _ = if !t_pp then F.printf "%spsl: %a@\n" d fprintf psl in
   let _ = if !t_pp then F.printf "%sqsl: %a@\n" d fprintf qsl in
   let _ = if !t_pp then F.printf "%sexpl: %a@\n" d fprintf expl in
   let _ = if !t_pp then F.printf "%stmpl: %a@\n" d fprintf tmpl in
   let _ = if !t_pp then F.printf "%sl: %a@\n" d fprintf l in
  return (u,l)
;;

let compose gs f = compose "." gs f;;

let store_p c =
 if not !cache then return c
 else (get >>= fun s -> cache_m s.ht_pvars (fun _ -> fresh_bool) c)
;;

let store_a c =
 if not !cache then return c
 else (
  let spec = Logic.nat (Int.bit_max !t_ob) in
  get >>= fun s -> cache_m s.ht_avars (fun _ -> fresh_coeff_spec spec) c)
;;

let is_zero_q = function
 | Left p -> Logic.top
 | Right f -> Logic.big_and (List.map is_zero_matrix (P.coefficients (q f))) 
;;

let output_degree_conditions (u,l) =
 let rec conds d acc add = function
  | Left p -> return (Left p)
  | Right f -> if d <= 0 then (
    let sc = if add then Logic.big_or (is_zero_q (Right f)::acc) else Logic.top in
    add_sc sc >>
    return (Left (p f))
  ) else conds (d-1) (is_zero_q (Right f)::acc) add (exp f) >>= fun e -> 
  return (make (p f) (base ()) e (q f))
 in get >>= fun c ->
 conds c.out_deg [] true u >>= fun u ->
 conds c.out_deg [] false l >>= fun l ->
 return (u,l)
;;

let interpret_with_args os o =
 let t = Unix.gettimeofday () in
 compose os o >>= fun r ->
 t_compose := !t_compose +. (Unix.gettimeofday () -. t);
 output_degree_conditions r >>
 return r
;;

(* comparisons *)
let rec conx x = function
 | Left p -> getx x p %>% matrix_zero
 | Right o -> (px x o %>% matrix_zero) <|> 
             (qx x o %>% matrix_zero) <|> 
             (greater_zero_poly (q o) <&> conx x (exp o))
;;

(* f >= g *)
(*comparison of FBIs*)
let low f = ite_poly (is_zero_poly f) poly_one (P.scale (base ()) f);;

let lin_exp e = match exp e with 
 | Left p -> p
 | Right f -> p f
;;

let diff_poly p q = P.add p (P.scale (coeff_of_int ~-1) q);;

let mul_approx q p =
 let const_carrier = (fun _ -> P.make [Mono.make (carrier ()) []]) in
 let c = Mono.coefficient (P.constant_part (P.subst const_carrier q)) in
 let l = P.scale c (P.non_constant_part p) in
 let r = P.scale (Mono.coefficient (P.constant_part p)) q in
 ite_poly (is_zero_poly (P.non_constant_part q)) 
  (P.scale (Mono.coefficient (P.constant_part q)) p)
  (P.add l r)
;;

let mul_poly p q = (if desired '6' then mul_approx else P.mul) p q;;
let left2right p = 
 let pz = zero_poly (P.variables p) in
 (make p (base ()) (Left pz) pz)
;;

let rec geq f g = match f,g with
  | Left p , Left q -> geq_poly p q
  | Left g , Right f -> is_zero_poly (q f) <&> geq_poly g (p f)
  | Right f, Left g -> geq (Right f) (left2right g)
  | Right f, Right g ->
   let l1 = mul_poly (low (diff_poly (lin_exp f) (lin_exp g))) (q f) in
   let r1 = q g in
   let l2 = P.add (p f) (mul_poly (low (lin_exp g)) l1) in
   let r2 = P.add (p g) (mul_poly (low (lin_exp g)) r1) in
    (greater_zero_poly (q g) <->> geq (exp f) (exp g)) <&> (
     (greater_zero_poly (q f) <&> geq (scale (base ()) (exp f)) (Right g)) <|>
     (geq_poly (p f) (p g) <&> geq_poly (q f) (q g)) <|>
     (geq_poly l1 r1 <&> geq_poly l2 r2))
;;


let rec gt f g = match f,g with
  | Left p , Left q -> gt_poly p q
  | Left g , Right f -> is_zero_poly (q f) <&> (gt_poly g (p f))
  | Right f, Left g -> gt (Right f) (left2right g) 
  | Right f, Right g ->
   let l1 = mul_poly (low (diff_poly (lin_exp f) (lin_exp g))) (q f) in
   let r1 = q g in
   let l2 = P.add (p f) (mul_poly (low (lin_exp g)) l1) in
   let r2 = P.add (p g) (mul_poly (low (lin_exp g)) r1) in
   let pre = (greater_zero_poly (q g) <->> geq (exp f) (exp g)) in
   let x1 = (greater_zero_poly (q f) <&> gt (scale (base ()) (exp f)) (Right g)) in
   let x2 = (geq_poly (p f) (p g) <&> geq_poly (q f) (q g) <&>
      ((greater_zero_poly (q f) <&> gt (exp f) (exp g)) <|> gt_poly (p f) (p g) <|> (gt_poly (q f) (q g)))) in
   let x3 = 
 (* (gt_poly l1 r1 <&> geq_poly l2 r2) <|>  *)
     (geq_poly l1 r1 <&> gt_poly l2 r2) in
     let _ = (if !t_pd then 
(*
     let _ = F.printf "pre: %a@\n" Logic.fprintf_p pre in
     let _ = F.printf "x1: %a@\n" Logic.fprintf_p x1 in
     let _ = F.printf "x2: %a@\n" Logic.fprintf_p x2 in
     let _ = F.printf "x3: %a@\n" Logic.fprintf_p x3 in
*)
(*
     let _ = F.printf "%a > %a@\n" fprintf (Right f) fprintf (Right g) in
     let _ = F.printf "l1: %a@\n" fprintf_p l1 in
     let _ = F.printf "r1: %a@\n" fprintf_p r1 in
     let _ = F.printf "l2: %a@\n" fprintf_p l2 in
     let _ = F.printf "r2: %a@\n" fprintf_p r2 in
*)
     () else ()) in

   (pre <&> (x1 <|> x2 <|> x3))
;;

let store_poly p =
 map (fun (c,xs) -> 
  store_a c >>= fun d -> 
  return (Mono.make  d xs))
  ((P.split p)) >>= fun ms ->
 return (P.make ms)
;;
 
let rec store = function
 | Left f -> store_poly f >>= fun f -> return (Left f)
 | Right o ->
   store_poly (p o) >>= fun p ->
   store_poly (q o) >>= fun q ->
   store (exp o) >>= fun exp -> 
   return (make p (base ()) exp q)
;;

let geq f g = 
 store f >>= fun f ->
 store g >>= fun g ->
return (geq f g);;

let gt f g = 
 store f >>= fun f ->
 store g >>= fun g ->
return (gt f g);;

(*
let rec substitute_vars xs o = 
 let xs' = List.map List.singleton xs in
 let subp p = 
  let ps, pc = P.non_constant_part p, P.constant_part p in
  let ps' = P.make (List.map2 Mono.make (P.coefficients ps) xs') in
  P.add ps' (P.make [pc])
 in 
 let rec subst xs' = function
  | Left p -> Left (subp p)
  | Right f ->
   make (subp (p f)) (base ()) (subst xs' (exp f)) (subp (q f))
 in
 subst xs' o
;;
*)

(*other desired properties*)
let well_defined_f f = 
 interpretation f >>= fun o -> 
 (makel [carrier ()] [[]]) >>= fun min_val ->
 geq o min_val
;;

let monotone_f f = 
 arity f >>= fun a ->
 interpretation f >>= fun o -> match o with
  | Left p -> let ncp = P.non_constant_part p in
   return (Logic.big_and (P.map (fun m -> Mono.coefficient m %>% matrix_zero) ncp))
  | Right _ -> let xs = List.filter (fun xs -> xs <> []) (vars o) in
   return (Logic.big_and (List.map (fun x -> conx x o) xs))
;;

(* decoding *)
let eval_n a ass = a >>= fun m ->
 eval_a (return (M.get 0 0 m)) ass >>= fun c ->
 Made.return (M.make 1 1 (Logic.constant c))
;;

let eval_poly ass p = 
 Made.map (fun (a,x) -> 
  eval_n (return a) ass >>= fun c -> 
  return (c,x))
 (P.split p) >>= fun ms ->
 return (P.make (List.map (Util.uncurry Mono.make) ms))
;;

let rec eval_interpretation ass f = match f with
 | Left g -> eval_poly ass g >>= fun p -> return (Left p)
 | Right o ->
  eval_poly ass (p o) >>= fun p ->
  eval_n (return (base ())) ass >>= fun b ->
  eval_interpretation ass (exp o) >>= fun e ->
  eval_poly ass (q o) >>= fun q ->
  return (make p b e q)
;; 


(*
let test _ _ = 
  get >>= fun c -> 
  let f,s = Sig.create_fun 1 "f" c.state in
  let g,s = Sig.create_fun 1 "g" c.state in
  set {c with state=s} >>
  make_interpretation 1 f >>= fun f ->
  make_interpretation 1 g >>= fun g ->
  gt f g
*)

