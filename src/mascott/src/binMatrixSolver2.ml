(*** OPENS ***************************************************************)
open Util;;

(*** SUBMODULES **********************************************************)
module Var = Rewriting.Variable;;
module Fun = Rewriting.Function;;
module T = U.Term;;
module Sig = U.Signature;;
module M = U.Monad;;

open M;;

(*** TYPES ***************************************************************)
let term = function
 | Some x -> T.Var x
 | _ -> failwith "No Var in BS.term"
;;

let to_variable = function
 | Some x -> x
 | _ -> failwith "No Some in BS.var"
;;

let satisfy_matrix mx (k,m,n) (k',m',n') a =
 let block = List.product (List.range k m) (List.range k' m') in
 let conflict (i,j) = (mx.(i).(j) = None) && (a.(i).(j) = 1) in
 not (List.exists conflict block)
;;

(* upper row larger than lower row *)
let row_smaller i j m =
 let rec cmp k =
  if k = Array.length m.(0) then true
  else if m.(i).(k) > m.(j).(k) then true
  else if m.(i).(k) < m.(j).(k) then false
  else cmp (k+1)
 in cmp 0
;;

let row_equal i j m =
 let rec cmp k =
  if k = Array.length m.(0) then true
  else if m.(i).(k) <> m.(j).(k) then false
  else cmp (k+1)
 in cmp 0
;;

(* first col read top down larger than second *)
let col_smaller i j m =
 let rec cmp k =
  if k = Array.length m then true
  else if m.(k).(i) > m.(k).(j) then true
  else if m.(k).(i) < m.(k).(j) then false
  else cmp (k+1)
 in cmp 0
;;

let col_equal i j m =
 let rec cmp k =
  if k = Array.length m then true
  else if m.(k).(i) <> m.(k).(j) then false
  else cmp (k+1)
 in cmp 0
;;


let check_compare ss ts a =
 let pairs xs =
  let r = List.map_range (fun x -> x) 0 (List.length xs) in
  let ps = List.product r r in
  List.filter (fun (i,j) -> i < j && List.nth xs i = (List.nth xs j)) ps
 in
 let check cmp xs = List.for_all (fun (i, j) -> cmp i j a) (pairs xs) in
 (check row_smaller ss) && (check col_smaller ts)
;;


let print_ass =
 let print_ass a =
  let pstr (i,j) = "("^(string_of_int i)^","^(string_of_int j)^")" in
  let s = List.foldl (fun s p -> s^" "^(pstr p)) "" a in
  Format.printf " [%s]\n" s;
 in
 List.iter print_ass
;;

let print_matrix a = 
 let pr a = (Array.fold_left (fun s i -> s^" "^(string_of_int i)) "["  a)^"]" in
 let pm a = (Array.fold_left (fun s r -> s^" "^(pr r)) "["  a)^"]" in
 Format.printf " %s\n" (pm a)
;;

(* combine two assignments *)
let combine a a' = List.rev_map (Util.uncurry List.rev_append) (List.product a a')

let one_per_row r lower upper =
 List.map (fun i -> [r,i]) (List.range lower upper)
;;

let one_per_col c lower upper =
 List.map (fun i -> [i,c]) (List.range lower upper)
;;

let one_per_each_row rs lower upper =
 let one_per_row r = one_per_row r lower upper in
 List.fold_left (fun a r -> combine (one_per_row r) a) [[]] rs
;;

let one_per_each_col cs lower upper =
 let one_per_col c = one_per_col c lower upper in
 List.fold_left (fun a c -> combine (one_per_col c) a) [[]] cs
;;

let one_per_row_col (k,m,n) (k',m',n') =
 let cs' = List.range m' n' in
 (* one per row, at most one per column between k' and m' *)
 let rec t_tv cs = function
    [] -> [[]]
  | r :: rs -> 
   let add r c a = (r,c)::a in
   let rc c = List.rev_map (add r c) (t_tv (List.remove c cs) rs) in
   let rc' c = List.rev_map (add r c) (t_tv cs rs) in 
   List.concat (List.rev_append (List.rev_map rc cs) (List.rev_map rc' cs'))
 in 
 let a = t_tv (List.range k' m') (List.range k m) in
 let cs = List.range k' m' in
 let complete a =
  let cs' = List.filter (fun c -> List.for_all (fun (_,c') -> c' <> c) a) cs in
  combine [a] (one_per_each_col cs' m n)
 in List.concat (List.rev_map complete a)
;;


(* all assignments for matrix block between lr and (excluding) lu
 and columns lc and (excluding) lc *)
let all lr ur lc uc = 
 let rs = List.range lr ur in
 let cs = List.range lc uc in
 List.powerset (List.product rs cs)
;;

let find_borders ts =
 let constants, rest = List.partition T.is_cons ts in
 let vars,rest = List.partition T.is_var rest in
 let k = List.length constants in
 let m = List.length rest + k in
 let n = List.length ts in
 k,m,n
;;

let to_matrix (_,_,n) (_,_,n') a =
 let m = Array.create_matrix n n' 0 in
 List.iter (fun (i,j) -> m.(i).(j) <- 1) a;
 m
;;

let to_variables mx a =
 let add_cell i (vs,j) c = (if a.(i).(j) = 1 then c::vs else vs),j+1 in
 let add_row (vs,i) r = fst (Array.fold_left (add_cell i) (vs,0) r),i+1 in
 let vs = fst (Array.fold_left add_row ([],0) mx) in
 List.map to_variable vs
;;

(* * *)
let row_sum a i = Array.fold_left (+) 0 a.(i);;

let col_sum a j = Array.fold_left (fun s row -> s + row.(j)) 0 a;;

let single_one_per_row a i = (row_sum a i) = 1

let single_one_per_col a i = (col_sum a i) = 1

let nonzero_row a i = (row_sum a i) > 0

let nonzero_col a i = (col_sum a i) > 0

let check (k,m,n) (k',m',n') a =
 List.for_all (single_one_per_row a) (List.range k m) &&
 List.for_all (single_one_per_col a) (List.range k' m') &&
 List.for_all (nonzero_row a) (List.range m n) &&
 List.for_all (nonzero_col a) (List.range m' n')
;;

let assignments' (k,m,n) (k',m',n') =
 (* C/V *)
 let a = one_per_each_row (List.range 0 k) m' n' in
 (*Format.printf "assign c/V\n";
 print_ass a;*)
 (* V/C *)
 let a = combine a (one_per_each_col (List.range 0 k') m n) in
 (*Format.printf "assign v/c\n";
 print_ass a;*)
 (* (T+V)/T *)
 let a = combine a (one_per_row_col (k,m,n) (k',m',n')) in
 let a = combine a (all m n m' n') in
 let to_matrix = to_matrix (k,m,n) (k',m',n') in
 List.filter (check (k,m,n) (k',m',n')) (List.rev_map to_matrix a)
;;

let assignment_table = Hashtbl.create 1000

let assignments (k,m,n) (k',m',n') =
 try Hashtbl.find assignment_table ((k,m,n),(k',m',n'))
 with Not_found ->
  let ass = assignments' (k,m,n) (k',m',n') in
  Hashtbl.add assignment_table ((k,m,n),(k',m',n')) ass;
  ass
;;

(* * *)

(* Matrix mx has (List.length ss) rows and (List.length ts) cols *)
let solve mx ss ts =
 if (List.is_empty ss) || (List.is_empty ts) then [] else (
 assert ((Array.length mx = (List.length ss)) && (Array.length mx.(0) = (List.length ts)));
 let brows = find_borders ss in
 let bcols = find_borders ts in
 let a = assignments brows bcols in
 let check_matrix = satisfy_matrix mx brows bcols in
 let check_compare = check_compare ss ts in
 let a = List.filter (fun x -> (check_matrix x) && (check_compare x)) a in
 (*List.iter print_matrix a;*)
 List.rev_map (to_variables mx) a )
;;



(* TEST *)(*
let test () =
let sigma = Sig.empty 20 in
let x,sigma = Sig.create_var "x" sigma in
let y,sigma = Sig.create_var "y" sigma in
let z,sigma = Sig.create_var "z" sigma in
let u,sigma = Sig.create_var "u" sigma in
let v,sigma = Sig.create_var "v" sigma in
let w,sigma = Sig.create_var "w" sigma in
let x1,sigma = Sig.create_var "x1" sigma in
let x2,sigma = Sig.create_var "x2" sigma in
let x3,sigma = Sig.create_var "x3" sigma in
let x4,sigma = Sig.create_var "x4" sigma in
let x5,sigma = Sig.create_var "x5" sigma in
let x6,sigma = Sig.create_var "x6" sigma in
let x7,sigma = Sig.create_var "x7" sigma in
let x8,sigma = Sig.create_var "x8" sigma in
let x9,sigma = Sig.create_var "x9" sigma in
let x10,sigma = Sig.create_var "x10" sigma in
let x11,sigma = Sig.create_var "x11" sigma in
let x12,sigma = Sig.create_var "x12" sigma in
let vars = [x;y;z;u;v;w] in
let x,y,z,u,v,w = T.Var x, T.Var y, T.Var z, T.Var u, T.Var v, T.Var w in
let f,sigma = Sig.create_fun 2 "f" sigma in
let a,sigma = Sig.create_fun 0 "a" sigma in
let b,sigma = Sig.create_fun 0 "b" sigma in
let c,sigma = Sig.create_fun 0 "c" sigma in
let d,sigma = Sig.create_fun 0 "d" sigma in
let e,sigma = Sig.create_fun 0 "e" sigma in
let i,sigma = Sig.create_fun 1 "i" sigma in
let a_ = T.Fun(a, []) in
let b_ = T.Fun(b, []) in
let c_ = T.Fun(c, []) in
let d_ = T.Fun(d, []) in
let faa = T.Fun(f, [a_;a_]) in
let faaa = T.Fun(f, [a_;faa]) in
let fabc = T.Fun(f, [a_;T.Fun(f, [b_;c_])]) in
let fxx = T.Fun(f,[x;x]) in
let ix = T.Fun(i,[x]) in
let ifxx = T.Fun(i,[fxx]) in
let ifaa = T.Fun(i,[faa]) in
let ifabc = T.Fun(i,[fabc]) in
let ss = [b_;faa;y] in
let ts = [a_;fabc;x] in
let mx = let [x;y;z;u;v;w] = vars in 
 Array.map Array.of_list (Array.of_list 
 ([[None;None;Some x];[None;Some y;Some z];[Some u;Some v;Some w]]))
in
let to_str = M.find_var_name in
let print_listm = M.foldl (fun s v -> to_str v >>= fun vn -> return (s^" "^vn)) "\n" in
let run_solve mx ss ts =
 Either.right (M.run sigma (
 let a = solve mx ss ts in
 M.map print_listm a >>= fun l ->
 let s = List.foldl (^) "" l in
 Format.printf "There are %i assignments:\n%s\n%!" (List.length l) s;
 let cmp l l' = if List.equal l l' then 0 else compare l l' in
 let a' = BinMatrixSolver.solve mx ts ss in
 M.map print_listm a' >>= fun l ->
 let s = List.foldl (^) "" l in
 Format.printf "Old solver: %i assignments:\n%s\n%!" (List.length l) s;
 assert (List.equal ~c:cmp a a');
 return ())) 
in
let arr l = Array.map Array.of_list (Array.of_list l) in
run_solve (arr [[None]]) [a_] [b_];
run_solve mx ss ts;
let m = (arr [[None;Some x1];[Some x2;Some x3];[Some x4; Some x5]]) in
run_solve m [b_;y;z] [a_;x];
run_solve (arr [[Some x1]]) [x] [y];
let m = (arr [[Some x1; Some x2];
              [None;   Some x3]]) in
run_solve m [fabc;faaa] [fabc;fxx];
Format.printf "(ix,x) vs (ifxx,ifaa,ifabc)\n";
let m = (arr [[Some x1; Some x2; Some x3];
              [Some x4; Some x5; Some x6]]) in
run_solve m [ix;x] [ifxx; ifaa; ifabc];
Format.printf "(c,d,fabc,z) vs (a,b,faa,x,x)\n";
let m = (arr [[None;   None;   None;   Some x1; Some x2];
              [None;   None;   None;   Some x3; Some x4]; 
              [None;   None;   Some x5; Some x6; Some x7];
              [Some x8; Some x9; Some x10;Some x11;Some x12]]) in
run_solve m [c_;d_;fabc;z] [a_;b_;faa; x; x];
Format.printf "(c,d,fabc) vs (a,b,faa,x,x)\n";
let m = (arr [[None;   None;   None;   Some x1; Some x2];
              [None;   None;   None;   Some x3; Some x4];
              [None;   None;   Some x5; Some x6; Some x7]]) in
run_solve m [c_;d_;fabc] [a_;b_;faa; x; x];
Format.printf "(c,d,fabc,z) vs (a,b,faa,x)\n";
let m = (arr [[None;   None;   None;   Some x1];
              [None;   None;   None;   Some x3];
              [None;   None;   Some x5; Some x6];
              [Some x8; Some x9; Some x10;Some x11]]) in
run_solve m [c_;d_;fabc;z] [a_;b_;faa; x];
let m = (arr [[None; Some x1; Some x2];
              [None; Some x3; Some x4];
              [None; Some x5; Some x6];
              [Some x7;Some x8;Some x9]]) in
run_solve m [a_;a_;b_;ifaa] [ix; y; z];
;;  

test ();; *)
