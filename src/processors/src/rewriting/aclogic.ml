(*** SUBMODULES **********************************************************)
module Var = Prelude.Variable;;
module Fun = Prelude.Function;;
module M = Monad;;
module T = Prelude.Term;;
module Sub = Prelude.Substitution;;
module Elogic = Prelude.Elogic;;
module TSub = Util.Replacement.Make(T)(T);;
module L = Label;;
module Sig = Prelude.Signature;;
module A = Array;;

(*** OPENS ***************************************************************)
open Util;;

(*** EXCEPTIONS **********************************************************)
exception Repeated_variables

(*** FUNCTIONS ***********************************************************)
let (>>=) = M.(>>=);;
let (>>) = M.(>>);;
let return = M.return;;

let is_ac_symbol = M.is_theory L.AC

let rec lex = function
  | [],[] -> 0
  | [],_ -> -1
  |_, [] -> 1
  | x::xs,y::ys -> let c = my_compare x y in if c=0 then lex (xs,ys) else c
and my_compare t t' =
 match t, t' with
  | Term.Var x, Term.Var y -> Var.compare x y
  | Term.Fun _, Term.Var _ -> -1
  | Term.Var _, Term.Fun _ -> 1
  | Term.Fun(_,[]), Term.Fun(_,t::ts) -> -1
  | Term.Fun(_,t::ts), Term.Fun(_,[]) -> 1
  | Term.Fun(f,fs), Term.Fun(g,gs) when f=g -> lex (fs,gs)
  | Term.Fun(f,_), Term.Fun(g,_) -> Fun.compare f g
;;

let find_term_with f =
 let rooted_f = function Term.Fun (g,_) when f=g -> true | _ -> false in
 let rec find ys = function
  | [] -> None
  | x::xs when rooted_f x -> Some(x, ys@xs)
  | x::xs -> find (x::ys) xs
 in find []
;;

let rec flatten = function
 | Term.Fun(f, ts) ->
  is_ac_symbol f >>= fun f_is_ac -> (
  if f_is_ac then
   M.map flatten ts >>= fun ts' ->
   match find_term_with f ts' with
    | None -> return (Term.Fun(f, List.sort my_compare ts'))
    | Some (ti,ts'') -> flatten (Term.Fun(f, (ts''@(Term.args ti))))
  else
   M.map flatten ts >>= fun us -> return (Term.Fun(f,us)))
 | v -> return v
;;



(** BinMatrixSolver ******************************************************)
module BSolver = struct
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

let check_equal cmp m xs =
 let rec pairs = function
  | [] -> []
  | i :: is -> List.map (fun j -> if i<j then i,j else j,i) is @ (pairs is)
 in
 let xs = List.foldri (fun i x l -> (x,i)::l) [] xs in
 let xss = List.group ~c:(fun p q -> compare (fst p) (fst q)) xs in
 (*let xss = List.filter (T.is_var <.> fst <.> List.hd) xss in*)
 let xss = List.map (List.map snd) xss in
 let xss = List.filter (fun l -> (List.length l) > 1) xss in
 let pairs = List.foldl (fun res l -> pairs l :: res) [] xss in
 List.for_all (fun (i, j) -> cmp i j m) (List.concat pairs)
;;

let check_compare ss ts a =
 let vs,ss = List.partition T.is_var ss in
 let vt,ts = List.partition T.is_var ts in
 check_equal row_smaller a ss && (check_equal col_smaller a ts) 
 (*&& stimmt nicht, vgl f(x,x) vs f(a,a)
 check_equal row_equal a vs && (check_equal col_equal a vt)*)
;;


let print_ass =
 let print_ass a =
  let pstr (i,j) = "("^(string_of_int i)^","^(string_of_int j)^")" in
  let s = List.foldl (fun s p -> s^" "^(pstr p)) "" a in
  Format.printf " [%s]\n" s;
 in
 List.iter print_ass
;;


(* combine two assignments *)
let combine a a' = List.map (Util.uncurry (@)) (List.product a a')

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
 let a = combine a (one_per_each_row (List.range k m) k' n') in
 let a = combine a (all m n k' n') in
 let to_matrix = to_matrix (k,m,n) (k',m',n') in
 List.filter (check (k,m,n) (k',m',n')) (List.map to_matrix a)
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
 (*print_ass a;*)
 let check_matrix = satisfy_matrix mx brows bcols in
 let check_compare = check_compare ss ts in
 let a = List.filter (fun x -> (check_matrix x) && (check_compare x)) a in
 List.map (to_variables mx) a )
;;

end
(*************************************************************************)

let t_solve = ref 0.0

(* zero substitution list {{}} *)
let zero_list = [Sub.empty]

let fresh_var = 
 M.fresh_var >>= fun v ->
 M.create_var_name v >>
 return v
;;

let ac_root = function
 | T.Var _ -> return None
 | T.Fun(f,_) -> 
  is_ac_symbol f >>= fun b -> return (if b then Some f else None)
;;

let (<<) s t = my_compare s t < 0

let print_constraints cs =
 M.foldl
 (fun str (t,s) ->
  T.to_stringm s >>= fun s ->
  T.to_stringm t >>= fun t ->
  return (str^" "^s^"="^t^"   ")) "" cs >>= fun s ->
 (Format.printf "%s\n%!" s; return ())
;;

let print_sub sub = 
 let sub = Sub.to_list sub in
 let append s (x,t) =
  T.to_stringm t >>= fun t ->
  M.find_var_name x >>= fun x ->
  return (s^" "^x^"->"^t)
 in
 M.foldl append "" sub 
;;

let print_subs ss =
 let add s sub = print_sub sub >>= fun s' -> return (s^s'^"\n ") in
 M.foldl add " " ss >>= fun s ->
 Format.printf "%s%!" s; return ()
;;

let print_sub_option = function
 | None -> return (Format.printf "None\n%!")
 | Some subs -> print_subs subs
;;

let contains_zero_row m =
 let row r = Array.fold_left (fun b x -> b && (x=None)) true r in
 Array.fold_left (fun b r -> b || (row r)) false m
;;

(* removes common occurrences in both argument lists (like mutual 
   multiset difference *)
let remove_common_args xs ys = 
 let rec remove (sx,sy) = function
  | [], ys -> List.rev sx, List.rev sy @ ys
  | xs, [] -> List.rev sx @ xs, List.rev sy
  | x::xs,y::ys when x=y -> remove (sx,sy) (xs,ys)
  | x::xs,y::ys when x << y -> remove (x::sx,sy) (xs,y::ys)
  | x::xs,y::ys (*when y << x*) -> remove (sx,y::sy) (x::xs,ys)
 in remove ([],[]) (xs,ys)
;;

(*let remove_common_args xs ys = List.diff xs ys, List.diff ys xs*)

(* For s=f(s_1,...,s_m) and t=f(t_1,...,t_n) for AC-Symbol f, abstract s t
   returns a pair (f(x_1,...,x_m), f(y_1,...,y_n)) together with pairs
   (x_i,s_i) and (y_j,t_j) where x_i, y_j are fresh variables.*)
let abstract s t =
 let f, ss, ts = Option.the (T.root s), T.args s, T.args t in
 let var _ = fresh_var in
 M.map var ss >>= fun xs -> let xst = List.map T.make_var xs in
 M.map var ts >>= fun ys -> let yst = List.map T.make_var ys in
 let c0 = (T.Fun(f,xst), T.Fun(f,yst)) in
 return (c0, (List.zip xst ss) @ (List.zip yst ts),xs,ys)
;;

let sub_constraints theta es = 
 List.map (Pair.map (Sub.apply_term theta)) es
;;

(* From an assignment a for the variables in the matrix m, construct
   a substitution binding variables in xs and ys. *)
let substitution_for_assignment f m xs ys a =
 let row i = A.to_list m.(i) in
 let col j = A.fold_right (fun r l -> r.(j)::l) m [] in
 let pos = function None -> false | Some x -> List.mem x a in
 let term = function Some x -> T.Var x | _ -> failwith "No Var" in
 let collect = function [t] -> term t | ts -> T.Fun(f,List.map term ts) in
 let xbind j s xj = Sub.add xj (collect (List.filter pos (col j))) s in
 let ybind i s yi = Sub.add yi (collect (List.filter pos (row i))) s in
 let s = List.foldli xbind Sub.empty xs in
 List.foldli ybind s ys
;;

(* Checks whether in both unificands repeated variables occur - in
   this case Repeated_variables is raised as the algorithm does not
   terminate in such cases. *)
let both_contain_repeated_variables ss ts =
 let ss, ts = Pair.map (List.filter T.is_var) (ss, ts) in
 let groups l = List.group ~c:T.compare l in
 let unq l = List.for_all (fun g -> List.length g = 1) (groups l) in
  (not (unq ss)) && (not (unq ts))
;;

let replace_repeated_variable ss = 
 let rec replace x x' = function
  | [] -> failwith "Nothing to replace"
  | t::ts when T.equal t x -> x'::ts
  | t::ts -> t::(replace x x' ts)
 in
 let ss = List.filter T.is_var ss in
 let groups = List.group ~c:T.compare ss in
 let x = List.hd (List.find (fun g -> List.length g > 1) groups) in
 fresh_var >>= (return <.> T.make_var) >>= fun x' -> 
 let ss' = replace x x' ss in
 return ((x,x'),ss')
;;


(* Given terms s and t, return complete set of unifiers for s=_{AC} t.
   Calls unify' and simplifies returned set of bindings in that all
   bindings x -> t are removed where x does not occur in s or t. *)
let rec unify_me c (s, t) =
 (* restrict substitution to variables of interest *)
 let vars = List.union (T.vars s) (T.vars t) in
 let add x t s = if List.mem x vars then Sub.add x t s else s in
 let simplify s =  Sub.fold add s Sub.empty in
 (M.lift (Option.map (List.map simplify))) (unify (c+1) (s, t))
(* Given terms s and t, return complete set of unifiers for s=_{AC} t.
   When matching, s is ground.  *)
and unify c (s,t) =
 match s, t with
  | T.Var x, _ -> 
   if T.is_proper_subterm s t then return None (* fail *)
   else if s = t then return (Some zero_list)
   else return (Some [Sub.add x t Sub.empty])
  | _, T.Var y ->
   if T.is_proper_subterm t s then return None (* fail *)
   else return (Some [Sub.add y s Sub.empty])
  | T.Fun(a,[]),T.Fun(b,[]) -> (* both constants *) 
   if a = b then return (Some zero_list) else return None
  | T.Fun(f, _), T.Fun(g, _) when f <> g -> return None
  | T.Fun(f,ss), T.Fun(_, ts) -> ((* same root symbol *)
   is_ac_symbol f >>= fun f_is_ac ->
   if not f_is_ac then (* assume length ss = length ts *)
    unify_with_list c (List.zip ss ts) zero_list
   else (
    flatten s >>= fun s' -> flatten t >>= fun t' ->
    let ss', ts' = remove_common_args (T.args s') (T.args t') in
  if List.is_empty ss' && List.is_empty ts' then
   return (Some [Sub.empty])
  else
    (* if too many variables, switch to other implementation *)
    let vs, vt = Pair.map (List.filter T.is_var) (ss', ts') in
    let too_large = (List.length vs) * (List.length vt) > 6 in
    if both_contain_repeated_variables ss' ts' || too_large then
     (*raise Repeated_variables*) return (Some [Sub.empty])
    else
     abstract (T.Fun(f,ss')) (T.Fun(f,ts')) >>= fun (c0,cs,xs,ys) ->
     matrix_solve c f (c0 :: cs) xs ys ss' ts' >>= function
      | None -> return None
      | Some subs -> unify_with_list (c+1) cs subs) )
(* Given list (i.e. conjunction) of equations es and set of substitutions
   subs, take union of CSU(es\theta) over all \theta in subs, where CSU(X)
   is complete set of unifiers for X. *)
and unify_with_list c es subs =
 let add csus theta = 
  let compose s = Sub.compose Sub.apply_term theta s in
  let cs = sub_constraints theta es in
  unify_conjunction c cs >>= function
  | None -> return csus
  | Some z -> return ((List.map compose z) @ csus)
 in
 M.foldl add [] subs >>= function
 [] -> return None | subs -> return (Some subs)
(* Given a list (i.e. conjunction) of equations es, return complete set 
   of unifiers for es. *)
and unify_conjunction c cj =
 match cj with
 | [] -> return (Some zero_list) (*e.g. unify (fxy,fxy) *)
 | [e1] -> unify c e1
 | e1 :: es -> unify c e1 >>= function
   | None -> return None | Some ss -> unify_with_list c es ss
(* Given conjunction of f(X_1,...,X_m) = f(Y_1,...,Y_n) named c and 
   X_1=s_1, ...,X_m=s_m, Y_1=t_1, ...,Y_n=t_n named xs and ys with 
   distinct variables X_i,Y_j, matrix_solve finds complete set of 
   substitutions unifying f(X_1,...,X_m) and f(Y_1,...,Y_n). *)
and preprocess_matrix c m ss ts = 
 let coeff (i,yi) (j,xj) =
  match yi, xj with
   | T.Fun(a,[]), T.Fun(b,_) 
   | T.Fun(a,_), T.Fun(b,[])-> return None
   | T.Var _, _
   | _, T.Var _ -> fresh_var >>= fun z -> return (Some z)
   | _ -> ( (* two function symbols - check unifiability? *)
    (*unify c (yi,xj) >>= function 
    | None -> return BS.None 
    | Some sub ->*) fresh_var >>= fun z -> return (Some z))
 in
 let fill_cell i yi j m xj = 
  coeff (i,yi) (j,xj) >>= fun v -> m.(i).(j) <- v; return m 
 in 
 let fill_row i m yi = M.foldli (fill_cell i yi) m ss in
 M.foldli fill_row m ts

and matrix_solve c f cs xs ys ss ts =
 let m = A.make_matrix (List.length ys) (List.length xs) None in
 preprocess_matrix c m ss ts >>= fun m ->
 let ass =
  if (contains_zero_row m) then [] 
  else (
   let tstart = Unix.gettimeofday () in
   let a = BSolver.solve m ts ss in
   t_solve := !t_solve +. (Unix.gettimeofday () -. tstart);
   a) in
 if List.is_empty ass then return None else
 let subs = List.map (substitution_for_assignment f m xs ys) ass in
 return (Some subs)
;;

let unify ts = unify_me 0 ts 

let are_unifiable s t = 
 try
  unify (s,t) >>= (M.return <.> Option.is_some)
 with Repeated_variables -> M.return true
