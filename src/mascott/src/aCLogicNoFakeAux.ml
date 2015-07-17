(*** OPENS ***************************************************************)
open Util;;

(*** SUBMODULES **********************************************************)
module Var = Rewriting.Variable;;
module Fun = Rewriting.Function;;
module M = U.Monad;;
module T = U.Term;;
module Sub = U.Substitution;;
module Elogic = U.Elogic;;
module TSub = Replacement.Make(T)(T);;
module L = U.Label;;
module Sig = U.Signature;;
module A = Array;;

(*** OPENS (2) ***********************************************************)
open M;;

(*** EXCEPTIONS **********************************************************)
exception Repeated_variables

(*** FUNCTIONS ***********************************************************)

let (<<) s t = Termx.my_compare s t < 0

let fresh_var =
 fresh_var >>= fun v ->
 create_var_name v >>
 return v
;;

let print_constraints cs =
 foldl
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
  find_var_name x >>= fun x ->
  return (s^" "^x^"->"^t)
 in
 foldl append "" sub
;;

let print_subs ss =
 let add s sub = print_sub sub >>= fun s' -> return (s^s'^"\n ") in
 foldl add " " ss >>= fun s ->
 Format.printf "%s%!" s; return ()
;;

let print_sub_option = function
 | None -> return (Format.printf "None\n%!")
 | Some subs -> print_subs subs
;;

let list_to_string f l = (List.foldl (fun s x -> s^"; "^(f x)) "[" l)^"]";;
let list_to_stringn f l = (list_to_string f l)^"\n";;
let list_to_stringm f l = (foldl (fun s x -> f x >>= fun x -> return (s^"; "^x)) "[" l) >>= fun s -> return (s^"]");;

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

(* For s=f(s_1,...,s_m) and t=f(t_1,...,t_n) for AC-Symbol f, abstract s t
   returns a pair (f(x_1,...,x_m), f(y_1,...,y_n)) together with pairs
   (x_i,s_i) and (y_j,t_j) where x_i, y_j are fresh variables.*)
let abstract s t =
 let f, ss, ts = Option.the (T.root s), T.args s, T.args t in
 let var t (s,vs) = 
  match t with
   T.Var x -> return (s,x::vs) 
  | u -> fresh_var >>= fun x' -> return (Sub.add x' u s,x'::vs)
 in
 let sub = Sub.empty in
 foldr var (sub,[]) ss >>= fun (sub,xs) -> 
 let xst = List.map T.make_var xs in
 foldr var (sub,[]) ts >>= fun (sub,ys) -> 
 let yst = List.map T.make_var ys in
 return ((List.zip xst ss) @ (List.zip yst ts),xs,ys,sub)
;;

(* always fresh variables *)
let more_abstract s t =
 let f, ss, ts = Option.the (T.root s), T.args s, T.args t in
 let var t (s,vs) =
  (*match t with
   T.Var x -> return (s,x::vs)
  | u ->*) fresh_var >>= fun x' -> return (Sub.add x' t s,x'::vs)
 in
 let sub = Sub.empty in
 foldr var (sub,[]) ss >>= fun (sub,xs) ->
 let xst = List.map T.make_var xs in
 foldr var (sub,[]) ts >>= fun (sub,ys) ->
 let yst = List.map T.make_var ys in
 return ((List.zip xst ss) @ (List.zip yst ts),xs,ys,sub)
;;


let sub_constraints theta es =
 List.map (Pair.map (Sub.apply_term theta)) es
;;

(* fill matrix with T.Var vi lists according to solutions *)
let to_semigroup f ass =
 let ith v i = List.replicate i (T.Var v) in
 let sg a (acc, vs) =
  fresh_var >>= fun vi ->
  let a' = List.map (fun (x,i) -> ith vi i) a in
  return (a'::acc,vi::vs)
 in 
 foldr sg ([],[]) ass >>= fun (ass,vs) ->
 return (Array.of_list (List.map Array.of_list ass), vs)
;;

(* Construct all subsets of the assignment set b such that there is no zero
 column sum. Some constraints:
 - for a column corresponding to a non-variable term, the column sum is 1
 - delete rows where ones are at positions corresponding to non-unifiable
   terms (only implemented partially here) 
 where the two conditions are checked by check_row *)
let check_row terms b i =
 let a = List.nth b i in
 assert(List.length a = (List.length terms));
 let ts = List.zip_with (fun t (_,value) -> t,value) terms a in
 let compound_true (t,v) = (v > 0) && (not (T.is_var t)) in
 let ts = List.filter compound_true ts in
 (* no entries > 1 *)
 if List.for_all (fun (_,v) -> v=1) ts then
  (* no nonunifiable entries *)
  List.for_all (fun ((t,_),(u,_)) -> T.root t = (T.root u)) (List.square ts)
 else false
;;

let valid m terms subset =
 let col i = 
  let s = List.fold_left (fun s j -> s + m.(j).(i)) 0 subset in
  if not (T.is_var (List.nth terms i)) then s = 1 else s > 0
 in List.for_alli (fun i _ -> col i) terms
;;

let subsets (ss,ts) b =
 (* filter out equal variable *)
 let cmp t s = if T.is_var t && (T.is_var s) then T.compare t s else 1 in
 let terms = List.unique ~c:cmp (ss@ts) in (* same order as variables *)
 let indices = List.range 0 (List.length b) in
 let indices = List.filter (check_row terms b) indices in
 (* matrix now more convenient *)
 let b' = List.map (List.map snd) b in
 let m = Array.of_list (List.map Array.of_list b') in
 (* possible index subsets *)
 let ixsets = List.remove [] (List.powerset indices) in
 (* check *)
 List.filter (valid m terms) ixsets
;;

let fterm f = function
 | [] -> failwith "Nothing substituted for variable"
 | [x] -> x
 | args -> T.Fun(f,args)
;;

let sum_up subsets b' (xs,ys) f =
 let vars = List.unique (xs@ys) in
 let col_sum sub i = fterm f (List.flat_map (fun r -> b'.(r).(i)) sub) in
 let add_binding sub i s v = Sub.add v (col_sum sub i) s in
 let sub_for_subset s = List.foldli (add_binding s) Sub.empty vars in
 List.map sub_for_subset subsets
;;

let feterm f e = function
 | [] -> T.Fun(e,[]) (* neutral element *)
 | [x] -> x
 | args -> T.Fun(f,args)
;;

let sum b' (xs,ys) (f,e) =
 let vars = List.unique (xs@ys) in
 let indices = List.range 0 (Array.length b') in
 let col_sum ixs i = feterm f e (List.flat_map (fun r -> b'.(r).(i)) ixs) in
 let add_binding ixs i s v = Sub.add v (col_sum ixs i) s in
  let s = List.foldli (add_binding indices) Sub.empty vars in
 (*Format.printf "sub empty: %i\n%!" (if Sub.is_empty s then 1 else 0); *)
 s
;;

let unabstract sigma =
 let var = function T.Var x -> x | _ -> failwith "Not a variable" in
 let bind x t sol = 
  let v = Sub.find x sol in 
  Sub.add (var v) t (Sub.remove x sol)
 in
 let unabstract sigma sol = Sub.fold bind sigma sol in
 List.map (unabstract sigma)
;;

let make_idempotent sigma =
 let rec fixpower sigma sigman = 
  let sigman' = Sub.compose Sub.apply_term sigman sigma in
  if Sub.equal sigman' sigman then sigman
  else fixpower sigma sigman'
 in
 fixpower sigma sigma
;;

let make_all_idempotent = List.map make_idempotent

(* s = e or s = x or s = f(z_1, \ldots, z_n) for z_i \in Var *)
let acu_unify_fterm s t (f,e) =
 let et = T.Fun(e,[]) in
 let rec subs ys = function
  | [] -> []
  | z::zs -> 
   let all_e = List.map (fun t -> t,et) in
   ((z,t)::(all_e (ys@zs))) :: (subs (ys@[z]) zs)
 in match s with
  | T.Var x -> [[s,t]]
  | T.Fun(e',[]) when e' = e -> []
  | T.Fun(f',zs) when f' = f -> subs [] zs
  | _ -> failwith "unexpected term in acu_unify"
;;

let dio_solve xs ys = 
 let c s = List.map (fun g -> List.hd g, List.length g) (List.group s) in
 let xs', ys' = c xs, c ys in
 let bs = DioComplete.basis (List.map snd xs') (List.map snd ys') in
 let vars = List.map fst (xs'@ys') in
 List.map (fun (xb,yb) -> List.zip vars (xb@yb)) bs
;;

let str_assignments =
 let vn = find_var_name in 
 let value (x,n) = vn x >>= fun x -> return (x^":"^(string_of_int n)^" ") in
 let ass a = foldl (fun s p -> value p >>= fun v -> return (s^v)) "" a in
 foldl (fun s p -> ass p >>= fun a -> return (a^", "^s)) ""
;;

let str_vars vs =
 map find_var_name vs >>= fun ns ->
 return (List.foldl (fun s v -> s^", "^v) "" ns)
;;

let str_terms vs =
 map T.to_stringm vs >>= fun ns ->
 return (List.foldl (fun s v -> s^", "^v) "" ns)
;; 

let test () =
 let sigma = Sig.empty 20 in
 let x,sigma = Sig.create_var "x" sigma in
 let y,sigma = Sig.create_var "y" sigma in
 let z,sigma = Sig.create_var "z" sigma in
 let u,sigma = Sig.create_var "u" sigma in
 let v,sigma = Sig.create_var "v" sigma in
 let w,sigma = Sig.create_var "w" sigma in
 let x',y',z',u',v',w' = x,y,z,u,v,w in
 let x,y,z,u,v,w = T.Var x, T.Var y, T.Var z, T.Var u, T.Var v, T.Var w in
 let f,sigma = Sig.create_fun 2 "f" sigma in
 let f,sigma = Sig.set_theory f U.Label.AC sigma in
 let a,sigma = Sig.create_fun 0 "a" sigma in
 let b,sigma = Sig.create_fun 0 "b" sigma in
 let c,sigma = Sig.create_fun 0 "c" sigma in
 let a,b = T.Fun(a,[]), T.Fun(b,[]) in
 (* need fresh vars *)
 let add_var (s,vs) _ = let v,s = Sig.fresh_var s in s,v::vs in
 let fresh i = List.foldl add_var (sigma,[]) (List.replicate i 'a') in
 (* some tests *)
 (* 1 *)
 let ss,ts = [x;x;y;a],[b;b;z] in
 let ass = [[0;0;1;0;1];[0;1;0;0;1];[0;0;2;1;0];[0;1;1;1;0];
            [0;2;0;1;0];[1;0;0;0;2];[1;0;0;1;0]] in
 let sigma,vars = fresh 5 in
 let b1 = List.map (List.zip_with Pair.make vars) ass in
 let subs1 = subsets (ss,ts) b1 in
 let s = list_to_stringn (list_to_string string_of_int) subs1 in
 Format.printf "Subsets are %s\n%!" s;
 (* 2 *) (*
 let ss = subsets (ss,ts) [] in
 let s = list_to_string (list_to_string string_of_int) ss in
 Format.printf "Subsets are %s\n%!" s;
 (* 3 *)
 let ss,ts = [x],[y;z] in
 let ass = [[1;0;1];[1;1;0]] in
 let sigma,vars = fresh 3 in
 let b = List.map (List.zip_with Pair.make vars) ass in
 let subs = subsets (ss,ts) b in
 let s = list_to_string (list_to_string string_of_int) subs in
 Format.printf "Subsets are %s\n%!" s;*)
 (* check sum_up *)
 let b', vars = Either.right (M.run sigma (to_semigroup f b1)) in
 let bl = Array.to_list (Array.map Array.to_list b') in
 let fstr ts = Either.right (M.run sigma (T.to_stringm (T.Fun(f,ts)))) in
 let s = list_to_stringn (list_to_string fstr) bl in
 Format.printf "b' is %s\n%!" s;
 let xs,ys=[x';x';y';w'],[u';u';z'] in
 let subs = sum_up subs1 b' (xs,ys) f in
 Format.printf "Substitutions (%i):\n%!" (List.length subs);
 ignore (M.run sigma (print_subs subs));
 let theta = Sub.add u' b (Sub.add w' a Sub.empty) in
 let subs = unabstract theta subs in
 Format.printf "Substitutions unabstracted (%i):\n%!" (List.length subs);
 ignore (M.run sigma (print_subs subs));
 let subs = make_all_idempotent subs in
 Format.printf "Substitutions idempotent (%i):\n%!" (List.length subs);
 M.run sigma (print_subs subs);
;;

(*test ()*)









