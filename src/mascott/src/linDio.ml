(* linear diophantine equation solver following approach
   presented in 
   M. Filgueiras, A.Tomas:
   Fast methods for solving linear diophantine equations (1993) 
*)

(******************************** PRELIMINARIES ********************************)
(* return ceil (a / b) for integers a and b *)
let ceil_div a b =
 int_of_float (ceil ((float_of_int a) /. (float_of_int b)))
;;

let rec gcd a = function
   0 -> a
 | b -> gcd b (a mod b)
;;

let gcd_list = List.fold_left gcd 0

let flip (a,b) = (b,a)

let rec range n m = if n > m then [] else n :: (range (n+1) m) 

let extgcd a b =
 let rec extgcd (x2,y2,r2,q2) (x1,y1,r1,q1) =
  if r1 = 0 then (x2,y2)
  else
   let q,r = r2/r1, r2 mod r1 in
   let x,y = x2 - x1*q, y2 - y1*q in
   extgcd (x1,y1,r1,q1) (x,y,r,q)
 in if a > b then extgcd (1,0,a,1) (0,1,b,0)
 else flip (extgcd (1,0,b,1) (0,1,a,0))
;;

let extgcd_list l = 
 let rec egcd = function
   | [] -> [],0
   | a::l -> 
    let coeffs,g = egcd l in
    let u,v = extgcd a g in
    u :: (List.map (fun z -> z*v) coeffs), gcd a g
 in fst (egcd l)
;;

let rem a b = 
 let c = a mod b in
 if c < 0 then c + b else c
;;

let (++) (a,b) (a',b') = (a+a',b+b');;
let (--) (a,b) (a',b') = (a-a',b-b');;

(********************* DIOPHANTINE EQUATION SOLVING ***************************)

exception No_solution;;

(* equation of shape (2) in paper:
   a*x = b*y + c*z + v where a,b,c \in N and v \in Z *)
module Homogeneous = struct
type hom_basic_eqn = {
 a: int;
 b: int;
 c: int
}

let eqn a b c = {a=a;b=b;c=c}

let a e = e.a;;
let b e = e.b;;
let c e = e.c;;

(* homogeneous case: v=0 *) 

let ymax e = let a,b = a e, b e in a / (gcd a b);;
let zmax e = let a,c = a e, c e in a / (gcd a c);;

(* compute minimal spacing associated to equation *)
let min_spacing e = 
 let a,b,c = a e, b e, c e in
 let mb = snd (extgcd a b) in
 let dy1 = rem (mb * c / (gcd_list [a;b;c])) (ymax e) in
 let dz1 = (gcd a b) / (gcd_list [a;b;c]) in
 (dy1,dz1)
;;


let solve e =
 let a,b,c = a e, b e, c e in
 Format.printf "solve %i*x = %i*y + %i*z\n" a b c;
 let ymax,zmax = ymax e,zmax e in
 let dy1,dz1 = min_spacing e in
 if dy1 = 0 then [0,zmax; ymax,0] 
 else
  let rec sols y z dy dz =
   if (dy <= 0) || (y = 0) then []
   else
    let s = List.map (fun i -> (y-i*dy,z+i*dz)) (range 1 ((y-1)/dy)) in
    s @ (sols (y mod dy) (z+(dz*(y-1)/dy)) (rem dy y) ((dy/y)*z+dz))
  in 
  let y,z = ymax-dy1,dz1 in 
  (0,zmax) :: (ymax,0) :: (y,z) :: (sols y z dy1 dz1)
;;

(* TESTS *)
let check_solutions e =
 let a,b,c = a e, b e, c e in
 let ss = solve e in
 let is_solution (y,z) = (b*y+c*z) mod a = 0 in
 let is_minimal (y,z) = 
  List.for_all (fun (y',z') -> ((y'=y)&&(z=y')) || (y>=y') || (z>=z')) ss
 in
 let is_complete =
  let n = max a (max b c) in (* bound on sum of coefficients with same sign *)
  let square l = 
   List.concat(List.map (fun x -> List.map (fun y -> (x,y)) l) l) 
  in
  let rec is_decomposable (y,z) =
   let ss' = List.filter (fun (y',z') -> (y'<=y) && (z'<=z)) ss in
   (y+z=0) || (List.exists (fun s -> is_decomposable ((y,z) -- s)) ss')
  in
  let candidates = square (range 0 n) in
  List.for_all (fun p -> not (is_solution p) || is_decomposable p) candidates
 in
 assert(List.for_all is_solution ss);
 assert(List.for_all is_minimal ss);
 assert is_complete;
 ss
;;

check_solutions (eqn 2 1 3);;
check_solutions (eqn 13 2 3);;
check_solutions (eqn 13 3 2);;
check_solutions (eqn 18 9 3);;
check_solutions (eqn 75 24 13);;
check_solutions (eqn 2 3 5);;
check_solutions (eqn 2 4 3);;
check_solutions (eqn 27 4 3);;
end


module Inhomogeneous = struct

module H = Homogeneous;;

type basic_eqn = {
 a: int;
 b: int;
 c: int;
 v: int
}

let heqn e = H.eqn (a e) (b e) (c e);;
let inherited f e = f (heqn e);;
let ymax = inherited H.ymax;;
let zmax = inherited H.zmax;;

let eqn a b c v = {a=a;b=b;c=c;v=v}

let a e = e.a;;
let b e = e.b;;
let c e = e.c;;
let v e = e.v;;

let ymax e = let a,b = a e, b e in a / (gcd a b);;
let zmax e = let a,c = a e, c e in a / (gcd a c);;
 

(* compute minimal spacing associated to equation *)
let min_spacing e = 
 let a,b,c,v = a e, b e, c e, v e in
 let mb = snd (extgcd a b) in
 let dy1 = rem (mb * c / (gcd_list [a;b;c])) (ymax e) in
 let dz1 = (gcd a b) / (gcd_list [a;b;c]) in
 (dy1,dz1)
;;

let starting_solution e =
 let a,b,c,v = a e, b e, c e, v e in
 let mc = List.nth (extgcd_list [a;b;c]) 2 in
 let _,dz1 = min_spacing e in
 let z0 = rem ((-1) * v * mc / (gcd_list [a;b;c])) dz1 in
 let mb = snd (extgcd a b) in
 let ymax = ymax e in
 let y0 =  rem (((-1) * v - z0 * c) * mb / (gcd a b)) ymax in
 let k = (max 0 ((-1)*b*y0 - c*z0 - v) / (b*ymax)) in
 let y0',z0' =  (y0,z0) ++ (k*ymax,0) in (* correction if v < 0 results in x<0*)
 (y0',z0')
;;

starting_solution (eqn 7 3 2 1);;
starting_solution (eqn 2 2 5 (-10));; (* difference beween y0 and y0' *)

let basic_spacings e =
 let a,b,c = a e, b e, c e in
 let ymax = ymax e in
 let dy1,dz1 = min_spacing e in
 let sp = H.solve (H.eqn ymax (ymax-1) dy1) in 
 let sp' = List.map (fun (dy,u) -> ((-1)*dy,u*dz1)) sp in
 let sp' = List.filter (fun (dy,_) -> dy <> 0) sp' in
 List.sort (fun (a,_) (b,_) -> compare a b) sp'
;;

let rec initial_spacing y = function
 | [] -> 0
 | (s,_) :: sp ->  if y+s >= 0 then s else initial_spacing y sp
;;

let is_minimal ss yz = 
 let leq (a,b) (c,d) = (a <= c) && (b <= d) in
 List.for_all (fun uv -> (uv == yz) || not (leq uv yz)) ss
;;

let solve_pos_v e =
  let a,b,c,v = a e, b e,c e, v e in
 if rem v (gcd_list[a;b;c]) <> 0 then raise No_solution
 else 
 let ymax,zmax = ymax e,zmax e in
 let y0,z0 = starting_solution e in
 let bs = basic_spacings e in
 let dymin = initial_spacing y0 bs in
 if dymin = 0 then  [y0,z0(*;0,zmax*)]
 else
  let rec sols ss (y,z) =
   if (y+dymin < 0) || (z >= zmax) then ss
   else
(*    let deltas = List.filter (fun (dy,_) -> (y+dy >= 0)) bs in
    let yz = List.rev_map ((++) (y,z)) deltas in (* only one delta? *)*)
    let yz' = (y,z) ++ (List.find (fun (dy,_) -> (y+dy >= 0)) bs) in
    sols (yz'::ss) yz'
    (*sols (yz @ ss) (List.hd yz)*)
  in 
  let ss = (sols [y0,z0(*;0,zmax*)] (y0,z0)) in
  List.filter (is_minimal ss) ss
;;

let print_sols l =
 let rec print = function
  | [] -> Format.printf "}\n";
  | (b,c) :: ss -> Format.printf "(%i, %i)  " b c; print ss
 in Format.printf "{%s" ""; print l
;;

let solve_neg_v e =
  let a,b,c,v = a e, b e,c e, v e in
 if rem v (gcd_list[a;b;c]) <> 0 then raise No_solution
 else 
 let ymax,zmax = ymax e,zmax e in
 let y0,z0 = starting_solution e in
 let bs = basic_spacings e in
 let dymin = initial_spacing y0 bs in
 if dymin = 0 then  [y0,z0(*;0,zmax*)]
 else
  let rec sols ss (y,z) =
   if (y+dymin < 0) || (z >= zmax) then ss
   else
    let deltas = List.filter (fun (dy,_) -> (y+dy >= 0)) bs in
    let yz = List.rev_map ((++) (y,z)) deltas in (* only one delta? *)
    sols (yz @ ss) (List.hd yz)
  in 
  let ss = (sols [y0,z0(*;0,zmax*)] (y0,z0)) in
  Format.printf "Solutions: "; print_sols ss;
  let is_ok (y,z) = (b*y+c*z+v) >= 0 in
  let ss = List.filter is_ok ss in
  Format.printf "ok: "; print_sols ss;
  let ss = List.filter (is_minimal ss) ss in
  Format.printf "minimal: "; print_sols ss;
  ss
;;

let solve e = 
 if (v e) < 0 then solve_neg_v e else if (v e) > 0 then solve_pos_v e 
 else H.solve (heqn e)
;;

let check_solutions e =
 let a,b,c,v = a e, b e, c e, v e in
 Format.printf "%ix = %iy+ %iz + %i\n" a b c v;
 let ss = solve e in
 let is_solution (y,z) = ((b*y+c*z+v) mod a = 0) && ((b*y+c*z+v) >= 0) in
 let is_contained yz = List.exists ((=) yz) ss in
 let is_complete =
  let n = max a (max b (max c v)) in (* bound on sum of coefficients with same sign *)
  let square l = 
   List.concat(List.map (fun x -> List.map (fun y -> (x,y)) l) l) 
  in
  let nonminimal (y,z) =
   let d = (not (is_minimal ss (y,z))) in 
   if not d then Format.printf "not covered: (%i,%i)\n" y z; 
   d 
  in
  let cs = square (range 0 n) in
  List.for_all (fun p -> (not (is_solution p)) ||  (is_contained p) || (nonminimal p)) cs
 in
 let check b = if b then "yes" else "no" in
 Format.printf "solutions correct: %s\n" (check (List.for_all is_solution ss));
 Format.printf "solutions minimal: %s\n" (check (List.for_all (is_minimal ss) ss));
 Format.printf "solutions complete: %s" (check is_complete)
;;

let e = eqn 3 2 5 1;;
basic_spacings e;;
solve_pos_v e;; 
check_solutions e;;

let e = eqn 4 2 3 1;;
starting_solution e;;
basic_spacings e;;
solve_pos_v e;;
check_solutions e;;

let e = eqn 5 7 2 1;;
starting_solution e;;
basic_spacings e;;
solve_pos_v e;;
check_solutions e;;

let e = eqn 7 13 2 5;;
starting_solution e;;
basic_spacings e;;
zmax e;;
solve_pos_v e;;
check_solutions e;;

(* more solutions ... *)
let e = eqn 5 1 1 3;;
solve_pos_v e;;
check_solutions e;;

let e = eqn 8 1 1 5;;
solve_pos_v e;;
check_solutions e;;

let e = eqn 16 2 2 6;;
solve_pos_v e;;
check_solutions e;; 

(* v = 0 *)
let e = eqn 16 2 2 0;;
solve_pos_v e;;
check_solutions e;;
 
let e = eqn 16 7 13 0 in check_solutions e;;


(* no solution *)
let e = eqn 2 2 2 5;;
solve_pos_v e;;

(* negative v *)
let e = eqn 5 2 2 (-6);;
solve e;;
check_solutions e;;

let e = eqn 7 1 1 (-7);;
solve e;;
check_solutions e;;

let e = eqn 3 2 2 (-7);;
solve e;;
check_solutions e;;

end


