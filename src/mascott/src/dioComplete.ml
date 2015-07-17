(*** OPENS (2) ***********************************************************)
open Util;;

(*** SUBMODULES **********************************************************)
module L = List;;

(*** FUNCTIONS ***********************************************************)
let ei i n = 
 let a = Array.make n 0 in
 Array.set a (i-1) 1;
 Array.to_list a
;;

let add xs ys = L.map (fun (x,y) -> x+y) (L.zip xs ys)

let add2 (a,b) (c,d) = (add a c, add b d)

let zero n = Array.to_list (Array.make n 0)

let p0 m n = L.map (fun i -> ei i m, zero n) (L.range 1 (m+1))

let sum = L.fold_left2 (fun s a b -> s + a*b) 0

(* the defect *)
let d xs ys (px,py) = (sum xs px) - (sum ys py)

(* component wise smaller or equal *)
let leq = L.fold_left2 (fun v a b -> v && (a <= b)) true

let minimal (px,py) m = 
 not (L.exists (fun (x,y) -> (leq x px) && (leq y py)) m)
;;

let rec complete d en em p m q m_all =
 if p = [] then L.unique m_all
 else
  let pn,pp = L.partition (fun p -> d p < 0) (L.filter (fun x -> d x <> 0) p) in
  let q' = 
   (L.flat_map (fun ej -> L.map (add2 ej) pn) en) @
   (L.flat_map (fun ei -> L.map (add2 ei) pp) em) 
  in
  let m' = L.filter (fun s -> d s = 0) q' in
  let p' = L.filter (fun p -> minimal p m_all) (L.diff q' m') in
  complete d en em p' m' q' (m' @ m_all)
;;

let basis aa bb =
 let m,n = L.length aa, L.length bb in
 let d = d aa bb in
 let em = List.map (fun i -> ei i m, zero n) (L.range 1 (m+1)) in
 let en = List.map (fun i -> zero m, ei i n) (L.range 1 (n+1)) in
 complete d em en (p0 m n) [] [] []
;;

let print_sols aa bb =
 List.iter
  (fun (l1, l2) -> 
   let s l = List.fold_left (fun s i -> s^", "^(string_of_int i)) "" l in
   Format.printf "%s - %s\n" (s l1) (s l2)) 
  (basis aa bb)
;;

let test () =
 Format.printf "x + 2y = 3z\n";
 print_sols [1;2] [3];
 Format.printf "3x + 5y = 3z\n";
 print_sols [3;5] [3];
 Format.printf "3x + 5y = z + 7w\n";
 print_sols [3;5] [1; 7];

;;

(* test () *)
