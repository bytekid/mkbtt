(*** SUBMODULES **********************************************************)
module List = Util.List;;
module Option = Util.Option;;
module Pair = Util.Pair;;
module Var = Rewriting.Variable;;
module Sig = Rewriting.Signature;;
module L = Logic;;
module Op = L.Operators;;
module N = L.Number;;
module M = L.Monad;;
module P = Util.Pair;;
module T = U.Term;;

(*** OPENS (2) ***********************************************************)
open M;;

(*** TYPES ***************************************************************)
type linpoly = (int * Var.t) list
type t = linpoly * linpoly

(*** FUNCTIONS ***********************************************************)
let var_cache = Hashtbl.create 100;;
let spec = L.nat 1;;
let var v = L.cache_arith var_cache spec v;;

let (<.>) = Util.(<.>);;
let flip = Util.flip;;
let top = L.top;;
let (<&>) = Op.(<&>);;
let (<=>) = Op.(<=>);;
let (<>=>) = Op.(<>=>);;
let (<<=>) = Op.(<<=>);;
let (<+>) = Op.(<+>);;
let (<->) = Op.(<->);;
let (<>>) = Op.(<>>);;
let big_and l = L.big_and l;;
let big_or l = L.big_or l;;
let big_sum l = L.big_sum l;;
let one = L.constant (N.of_int 1);;
let zero = L.constant (N.of_int 0);;

(* --------------------- OBTAINING THE ASSIGNMENT ----------------------- *)
let to_ass m a =
 let m = Array.to_list (Array.map Array.to_list m) in
 let to_int n = return (N.to_int n) in
 let val_of ij = var ij >>= Util.flip L.eval_a a >>= to_int in
 let add_cell i j ass = function
  | None -> return ass
  | Some x -> val_of (i,j) >>= fun v ->
    return (if v=1 then x::ass else ass)
 in
 let add_row i ass r = foldli (add_cell i) ass r in
 foldli add_row [] m
;;

(* ----------------------------- SOLVING -------------------------------- *)
let solve f = L.solve ~solver:L.Yices f

let negate = L.negate_assignment

let all_solutions m f0 = 
 let rec all_solutions' i f ass =
  (*L.fprintf_p Format.std_formatter f; Format.printf "\n%!";*)
  match L.run (solve f >>= function
   | None -> return None
   | Some a ->
   (*Format.printf "ass: %a@\n" L.fprintf_assignment a;*)
    let phi = negate a <&> f in
    to_ass m a >>= fun ax ->
    return (Some (phi,ax)))
  with
   | None -> ass
   | Some (phi,ax) -> all_solutions' (i+1) phi (ax::ass)
 in all_solutions' 0 f0 []
;;

(* ----------------------- BUILDING THE FORMULA ------------------------- *)
let value m (i,j) = 
 match m.(i).(j) with
  | None  -> return None 
  | Some _ -> var (i,j) >>= fun x -> return (Some x)
;;

let option_sum os = 
 big_sum (List.flat_map (function Some x -> [x] | _ -> []) os)
;;

let single_one_each_col mx lower upper cs =
 let single_one_in_col c =
  let is = List.map (flip Pair.make c) (List.range lower upper) in
  map (value mx) is >>= fun s -> return (option_sum s <=> one)
 in
 map single_one_in_col cs >>= (return <.> big_and)
;;

let single_one_each_row mx lower upper rs = 
 let single_one_in_row r =
  let is = List.map (Pair.make r) (List.range lower upper) in
  map (value mx) is >>= fun s -> return (option_sum s <=> one)
 in
 map single_one_in_row rs >>= (return <.> big_and)
;;

let nonzero_each_col mx lower upper cs =
 let nonzero_col c = 
  let is = List.map (flip Pair.make c) (List.range lower upper) in
  map (value mx) is >>= fun s -> return (option_sum s <>> zero)
 in map nonzero_col cs >>= (return <.> big_and)
;;

let nonzero_each_row mx lower upper rs =
 let nonzero_row r =
  let is = List.map (Pair.make r) (List.range lower upper) in
  map (value mx) is >>= fun s -> return (option_sum s <>> zero)
 in map nonzero_row rs >>= (return <.> big_and)
;;

let pairs ss =
 let rec pairs = function
  | [] -> []
  | i :: is -> List.map (fun j -> if i<j then i,j else j,i) is @ (pairs is)
 in
 let ss = List.foldri (fun i x l -> (x,i)::l) [] ss in
 let sss = List.group ~c:(fun p q -> compare (fst p) (fst q)) ss in 
 let sss = List.map (List.map snd) sss in
 let sss = List.filter (fun l -> (List.length l) > 1) sss in
 List.flat_map pairs sss
;;

let the_value m (i,j) = 
 value m (i,j) >>= function
  | None -> return zero
  | Some x -> return x
;;

let col_compare op mx (j,j') =
 let rec diff i =
  if i < 0 then 
   return zero 
  else
   the_value mx (i,j) >>= fun vj ->
   the_value mx (i,j') >>= fun vj' ->
   diff (i-1) >>= fun d -> 
   return ((vj <-> vj') <+> (L.scale (N.of_int 2) d))
 in diff (Array.length mx - 1) >>= fun d -> return (op d zero)
;;

let row_compare op mx (i,i') =
 let rec diff j =
  if j < 0 then 
   return zero 
  else
   the_value mx (i,j) >>= fun vi ->
   the_value mx (i',j) >>= fun vi' ->
   diff (j-1) >>= fun d ->
   return ((vi <-> vi') <+> (L.scale (N.of_int 2) d))
 in diff (Array.length mx.(0) - 1) >>= fun d -> return (op d zero)
;;

let ordered_rows mx ss = 
 let vs,ss = List.partition T.is_var ss in
 (*map (row_compare (<=>) mx) (pairs vs) >>= fun eq ->*)
 map (row_compare (<>=>) mx) (pairs ss) >>= fun sm ->
 return (big_and ((*eq@*)sm))
;;

let ordered_cols mx ts = 
 let vt,ts = List.partition T.is_var ts in
 (*map (col_compare (<=>) mx) (pairs vt) >>= fun eq ->*)
 map (col_compare (<>=>) mx) (pairs ts) >>= fun sm ->
 return (big_and ((*eq@*)sm))
;;

let binary m =
 let add_cell i j f = function
   | None -> return f
   | Some x -> var (i,j) >>= fun v -> return (f <&> (v <<=> one))
 in
 let add_row i ass r = foldli (add_cell i) ass r in
 foldli add_row top (List.map Array.to_list (Array.to_list m))
;;

let find_borders ts =
 let constants, rest = List.partition T.is_cons ts in
 let vars,rest = List.partition T.is_var rest in
 let k = List.length constants in
 let m = List.length rest + k in
 let n = List.length ts in
 k,m,n
;;

let solve' (ss,ts) mx =
 let (k,m,n) = find_borders ss in
 let (k',m',n') = find_borders ts in
 (* C/V *)
 single_one_each_row mx m' n' (List.range 0 k) >>= fun phi0 ->
 (* V/C *)
 single_one_each_col mx m n (List.range 0 k') >>= fun phi1 ->
 (* T/(T+V) *)
 single_one_each_row mx k' n' (List.range k m) >>= fun phi2 ->
 (* (T+V)/T *)
 single_one_each_col mx k n (List.range k' m') >>= fun phi3 ->
 (* V/(C+T+V) *)
 nonzero_each_row mx 0 n' (List.range m n) >>= fun phi4 ->
 (* (C+T+V)/V *)
 nonzero_each_col mx 0 n (List.range m' n') >>= fun phi5 ->
 (* rows (or cols) headed by equal terms are ordered *)
 ordered_rows mx ss >>= fun phi6 ->
 ordered_cols mx ts >>= fun phi7 ->
 binary mx >>= fun phi8 ->
 let phi = big_and [phi0;phi1;phi2;phi3;phi4;phi5;phi6;phi7;phi8] in
 return (all_solutions mx phi)
;;

(* --------------------------- MAIN SOLVER ------------------------------ *)

(* Matrix mx has (List.length ss) rows and (List.length ts) cols, and is
 filled with variables *)
let solve mx ss ts =
 if (List.is_empty ss) || (List.is_empty ts) then 
  [] 
 else (
  assert ((Array.length mx = (List.length ss)) && (Array.length mx.(0) = (List.length ts)));
  Hashtbl.clear var_cache;
  Logic.run (solve' (ss,ts) mx))
;;

