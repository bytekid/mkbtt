(*** SUBMODULES **********************************************************)
module List = Util.List;;
module Var = Rewriting.Variable;;
module Sig = Rewriting.Signature;;
module L = Logic;;
module Op = L.Operators;;
module N = L.Number;;
module M = L.Monad;;
module P = Util.Pair;;

(*** OPENS (2) ***********************************************************)
open M;;

(*** TYPES ***************************************************************)
type linpoly = (int * Var.t) list
type t = linpoly * linpoly

let var_cache = Hashtbl.create 100

let (<.>) = Util.(<.>);;
let (<&>) = Op.(<&>);;
let (<=>) = Op.(<=>);;
let (<<=>) = Op.(<<=>);;
let (<>>) = Op.(<>>);;
let big_and l = L.big_and l;;
let big_or l = L.big_or l;;
let big_sum l = L.big_sum l;;
let var s v = L.cache_arith var_cache s v;;

(* equation constraint, s is spec defining natural number range *)
let polynomials s (xs,ys) =
 let poly zs = 
  let mono c v = var s v >>= fun x -> return (L.scale (N.of_int c) x) in
  let gs = List.group ~c:Var.compare zs in
  map (fun g -> mono (List.length g) (List.hd g)) gs >>=
  (return <.> big_sum)
 in
 project poly (xs,ys)
;;

(* return formula negating assignment *)
let negate = L.negate_assignment

(* call solver *)
let solve f = L.solve ~solver:L.Yices f

(* When computing a basis, an upper bound for all solution values is 
   given by the maximum of the abolute value of coefficients *)
let max_coeff zs =
 let lengths zs = List.map List.length (List.group ~c:Var.compare zs) in
 List.foldl max 0 (lengths zs)
;;

(*
let smaller_than_upper_bound s b vars =
 let b = L.constant (N.of_int b) in
 map (fun x -> var s x >>= fun x -> return (x <<=> b)) vars >>= 
 (return <.> big_and)
;;*)

(* ok, this was due to Huet (78). Lambert (87) noted that the sum of
 the elements in a basis vector corresponding to coefficients with the 
 same sign does not exceed the maximal coefficient. *)
let smaller_than_upper_bound s (xs,ys) =
 let sum zs b = 
  let b = L.constant (N.of_int b) in
  map (var s) zs >>= fun s -> return (big_sum s <<=> b)
 in
 sum (List.unique xs) (max_coeff ys) >>= fun xsum ->
 sum (List.unique ys) (max_coeff xs) >>= fun ysum ->
 return (xsum <&> ysum)
;;


let not_all_zero s vars =
 let z = L.constant (N.of_int 0) in
 map (fun x -> var s x >>= fun x -> return (x <>> z)) vars >>=
 (return <.> big_or)
;;

let to_ass s vars a = 
 let to_int v n = return (v,N.to_int n) in
 let val_of a v = var s v >>= Util.flip L.eval_a a >>= to_int v in
 M.map (val_of a) vars
;;

let all_solutions (xs,ys) = 
 let b = max (max_coeff xs) (max_coeff ys) in
 let spec = L.nat b in
 let vars = List.unique (xs@ys) in
 polynomials spec (xs,ys) >>= fun (px,py) ->
 smaller_than_upper_bound spec (xs,ys) >>= fun f ->
 not_all_zero spec vars >>= fun not_zero ->
 let rec all_solutions' f ass =
   (*L.fprintf_p Format.std_formatter f; Format.printf "\n%!";*)
  match L.run (solve f >>= function
   | None -> return None
   | Some a -> 
   (*Format.printf "ass: %a@\n" L.fprintf_assignment a;*)
    let phi = negate a <&> f in
    to_ass spec vars a >>= fun ax ->
    return (Some (phi,ax)))
  with
   | None -> ass
   | Some (phi,ax) -> all_solutions' phi (ax::ass)
 in
 return (all_solutions' (f <&> (px <=> py) <&> not_zero) [])
;;



(* Given  lists xs=[x1,x2,...,xn] and ys=[y1,y2,..,ym] find 
 all assignments such that x1+...+xn=y1+...+ym *)
let solve (xs,ys) = 
 Hashtbl.clear var_cache; 
 Logic.run (all_solutions (xs,ys))
;;

let minimize =
 let (>=) l l' = List.for_all (fun (a,b) -> a >= b) (List.zip l l') in
 let is_minimal a rest = not (List.exists ((>=) a) rest) in
 let rec filter hd = function
  | [] -> []
  | a :: tl -> 
   let res = filter (a::hd) tl in
   if is_minimal a (hd@tl) then a::res else res
 in filter []
;;
 

let test () =
 let sigma = Sig.empty 20 in
 let x,sigma = Sig.create_var "x" sigma in
 let y,sigma = Sig.create_var "y" sigma in
 let z,sigma = Sig.create_var "z" sigma in
 let u,sigma = Sig.create_var "u" sigma in
 let v,sigma = Sig.create_var "v" sigma in
 let w,sigma = Sig.create_var "w" sigma in
 let var_name x = Sig.find_var_name x sigma in
 let var_names = List.foldl (fun s p -> (var_name p)^" "^s) "" in
 let value (x,n) = (var_name x)^":"^(string_of_int n)^" " in
 let print_ass a = List.foldl (fun s p -> (value p)^s) "" a in
 let print = List.foldl (fun s p -> (print_ass p)^", "^s) "" in
 let check (xs,ys) =
  Format.printf "Solving %s=%s yields\n %s\n%!" 
  (var_names xs) (var_names ys) (print (solve (xs,ys)))
 in
 check ([x; z], [y]);
 check ([x], [y]);
 check ([x; z], [y;u]);
 check ([x;x;y;w],[u;u;z]);
 check ([x; z], [y;y]);
;;

(*test ();;*)





