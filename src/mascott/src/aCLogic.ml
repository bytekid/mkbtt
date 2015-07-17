(*** OPENS ***************************************************************)
open Util;;

(*** SUBMODULES **********************************************************)
module Var = Rewriting.Variable;;
module Fun = Rewriting.Function;;
module M = U.Monad;;
module T = U.Term;;
module Rule = U.Rule;;
module Sub = U.Substitution;;
module VSub = Replacement.Make(Var)(Var);;
module Elogic = U.Elogic;;
module TSub = Replacement.Make(T)(T);;
module L = U.Label;;
module Sig = U.Signature;;
module Tx = Termx;;
module A = Array;;
module BS = BinMatrixSolver2;;

module F = struct
 include Format
 let formatter_of_out_channel c =
  let f = formatter_of_out_channel c in
  pp_set_margin f 9999;
  f
end

(*** OPENS (2) ***********************************************************)
open M;;

(*** EXCEPTIONS **********************************************************)
exception Repeated_variables

(*** FUNCTIONS ***********************************************************)
let t_solve = ref 0.0;;
let t_unify = ref 0.0;;
let mcount = ref 0;;
let mrepcount = ref 0;;

let unifycache : (T.t * T.t, Sub.t list option) Hashtbl.t = Hashtbl.create 300

let mcache1 f k table =
 mcount := !mcount + 1;
 let x = try Some (Hashtbl.find table k) with Not_found -> None in
 match x with
  | None -> (
   let tstart = Unix.gettimeofday () in
   f k >>= fun v ->
   t_unify := !t_unify +. (Unix.gettimeofday () -. tstart);
   Hashtbl.add table k v;
   return v)
  | Some v -> (mrepcount := !mrepcount + 1; return v)
;;


(* zero substitution list {{}} *)
let zero_list = [Sub.empty]

let fresh_var = 
 fresh_var >>= fun v ->
 create_var_name v >>
 return v
;;

let is_ac_symbol = M.is_theory L.AC

let ac_root = function
 | T.Var _ -> return None
 | T.Fun(f,_) -> 
  is_ac_symbol f >>= fun b -> return (if b then Some f else None)
;;

let (<<) s t = Termx.my_compare s t < 0

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
 map var ss >>= fun xs -> let xst = List.map T.make_var xs in
 map var ts >>= fun ys -> let yst = List.map T.make_var ys in
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

(* the purpose of this function is to filter out susbtitutions which
 assign some variable x occurring in both ss and ts  different values - 
 this may lead to nontermination as in the unification of
 f(a,a,x) = f(b,b,x) 
let filter_valid_subs xs ss ys ts subs =
 let sx = List.zip_with Pair.make ss xs in
 let ty = List.zip_with Pair.make ts ys in
 let sx = List.filter (fun (si,_) -> List.mem si ts) sx in
 if List.is_empty sx then subs else
  let valid_sub s =
   let check xi yi = 
    let t = Sub.find xi s in
    let t' = Sub.find yi s in
    t=t' || (not(T.mem_var yi t) && not (T.mem_var xi t'))
   in
   let check_value_in (si,xi) = check xi (List.assoc si ty) in
   List.for_all check_value_in sx
  in List.filter valid_sub subs
;;*)

(* Given terms s and t, return complete set of unifiers for s=_{AC} t.
   Calls unify' and simplifies returned set of bindings in that all
   bindings x -> t are removed where x does not occur in s or t. *)
let rec unify_me c (s, t) =
 (* restrict substitution to variables of interest *)
 let vars = List.union (T.vars s) (T.vars t) in
 let add x t s = if List.mem x vars then Sub.add x t s else s in
 let simplify s =  Sub.fold add s Sub.empty in
 (lift (Option.map (List.map simplify))) (unify (c+1) (s, t))
(* Given terms s and t, return complete set of unifiers for s=_{AC} t.
   When matching, s is ground. c is just to count steps. *)
and unify c (s,t) =
 match s, t with
  | T.Var x, _ -> 
   if T.is_proper_subterm s t then return None (* occur check *)
   else if s = t then return (Some zero_list)
   else return (Some [Sub.add x t Sub.empty])
  | _, T.Var y ->
   if T.is_proper_subterm t s then return None (* occur check *)
   else return (Some [Sub.add y s Sub.empty])
  | T.Fun(a,[]),T.Fun(b,[]) -> (* both constants *) 
   if a = b then return (Some zero_list) else return None
  | T.Fun(f, _), T.Fun(g, _) when f <> g -> return None
  | T.Fun(f,ss), T.Fun(_, ts) -> ((* same root symbol *)
   is_ac_symbol f >>= fun f_is_ac ->
   if not f_is_ac then (* assume length ss = length ts *)
    unify_with_list c (List.zip ss ts) zero_list
   else (
    Termx.flatten s >>= fun s' -> Termx.flatten t >>= fun t' ->
    let ss', ts' = remove_common_args (T.args s') (T.args t') in
    if List.is_empty ss' && List.is_empty ts' then
     return (Some [Sub.empty])
    else
     (* if too many variables, switch to other implementation *)
     let vs, vt = Pair.map (List.filter T.is_var) (ss', ts') in
     let too_large = (List.length vs) * (List.length vt) > 6 in
     if both_contain_repeated_variables ss' ts' || too_large then
      (* switch implementation *)
      ACLogicNoFake.unify (T.Fun(f,ss'), T.Fun(f,ts'))
     else
      abstract (T.Fun(f,ss')) (T.Fun(f,ts')) >>= fun (c0,cs,xs,ys) ->
      matrix_solve c f (c0 :: cs) xs ys ss' ts' >>= function
       | None -> return None
       | Some subs -> unify_with_list (c+1) cs subs) 
    )
(* Given list (i.e. conjunction) of equations es+ set of substitutions
   subs, take union of CSU(es\theta) for all \theta in subs, 
   where CSU(X) is complete set of unifiers for X. *)
and unify_with_list c es subs =
 let add csus theta = 
  let compose s = Sub.compose Sub.apply_term theta s in
  let cs = sub_constraints theta es in
  unify_conjunction c cs >>= function
  | None -> return csus
  | Some z -> return (List.rev_append (List.map compose z)  csus)
 in
 (*Format.printf "Called unify list with initial constraints:\n%!";
 print_constraints es >>= fun _ ->
 Format.printf "(%i) and substitutions \n%!" c;
 print_subs subs >>= fun _ ->*)
 foldl add [] subs >>= fun subs ->
 (*Format.printf "(%i) Unify list with constraints:\n%!" c;
 print_constraints es >>= fun _ ->
 Format.printf "(%i) and substitutions \n%!" c;
 print_subs subs >>= fun _ ->
 Format.printf "(%i) yields %i results\n%!" c (List.length subs); 
 print_subs subs >>*)
 match subs with 
 [] -> return None | subs -> return (Some subs)

(* Given a list (i.e. conjunction) of equations es, return complete set 
   of unifiers for es. *)
and unify_conjunction c cj =
 (*Format.printf "Called unify conjunction %i elements\n" (List.length cj);*)
 match cj with
 | [] -> return (Some zero_list) (*e.g. unify (fxy,fxy) *)
 | [e1] -> unify (c+1) e1
 | e1 :: es -> unify (c+1) e1 >>= function
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
 let fill_row i m yi = foldli (fill_cell i yi) m ss in
 foldli fill_row m ts

and matrix_solve c f cs xs ys ss ts =
 let m = A.make_matrix (List.length ys) (List.length xs) None in
 preprocess_matrix c m ss ts >>= fun m ->
 let ass =
  if (contains_zero_row m) then [] 
  else (
   let tstart = Unix.gettimeofday () in
   let a = BS.solve m ts ss in
   t_solve := !t_solve +. (Unix.gettimeofday () -. tstart);
   a) in
 if List.is_empty ass then return None else
 let subs = List.map (substitution_for_assignment f m xs ys) ass in
 return (Some subs)
;;

 let fprintfi f d fmt xs =
  let rec fprintfi i = function
   | []    -> return ()
   | [x]   -> f i fmt x
   | x::xs -> f i fmt x >>= fun _ -> F.fprintf fmt d; fprintfi (i+1) xs
  in
  F.fprintf fmt ""; fprintfi 0 xs >>= fun _ -> return (F.fprintf fmt "")
 ;;

 let fprintfm' f = fprintfi (const f);;

 let rec fprintfm fmt = function
  | T.Var x -> M.fprintf_var fmt x
  | T.Fun (f,ts) ->
   F.fprintf fmt ""; M.fprintf_fun fmt f >>= fun _ ->
   (F.fprintf fmt "("; fprintfm' fprintfm "," fmt ts) >>= fun _ ->
   M.return (F.fprintf fmt ")")
 ;;


 let tto_stringm t =
  fprintfm F.str_formatter t >>= (M.return <.> F.flush_str_formatter)
 ;;

let unify_me_print c (s,t) =
 unify_me c (s,t) >>= fun subs -> (
 project tto_stringm (s,t) >>= fun (s', t') ->
 F.printf "%s = %s: " s' t';
 match subs with | None -> Format.printf "0\n"; return ()
 | Some s -> Format.printf "%i \n%!" (List.length s); return ()) >>
 return subs
;;

let unify_cache' ts = mcache1 (unify_me(*_print*) 0) ts unifycache

let unify_cache (s,t) = 
 let r, rho = Tx.normalize_rule (Rule.of_terms s t) in
(* project T.to_stringm (s,t) >>= fun (s', t') ->
 Format.printf "Unify %s, %s  - calls: %i, repetitions: %i, time: %fi\n%!"  
  s' t' (!mcount) (!mrepcount) (!t_unify); *)
 unify_cache' (Rule.to_terms r) >>= function
  | None -> return None
  | Some ss ->
   let e = Sub.empty in
   let ohr = Sub.fold (fun x (T.Var y) s -> Sub.add y (T.Var x) s) rho e in
   let add x t s = Sub.add (Tx.var (Sub.find x ohr)) (Sub.apply_term ohr t) s in
   let rename s =  Sub.fold add s e in 
   return (Some (List.map rename ss))
(*>>= fun subs -> (
 project T.to_stringm (s,t) >>= fun (s', t') ->
 Format.printf "%s = %s: " s' t';
 match subs with | None -> Format.printf "0\n"; return ()
 | Some s -> Format.printf "%i \n%!" (List.length s); return ()) >>
 return subs*)
;;

let unify =  unify_me 0

let rec tsub_apply tsub = function
 | T.Fun(c,[]) as ct -> TSub.apply ct ct tsub
 | T.Fun(f,ts) -> T.Fun(f,List.map (tsub_apply tsub) ts)
 | v -> v
;;

let reverse_skolemization rho =
 let tsub_add x t s = TSub.add t (T.Var x) s in
 Sub.fold tsub_add rho TSub.empty
;;

(* Returns a complete set of matching substitutions \sigma such that for 
   every \theta in \sigma t\theta=s. *)
let matches (s,t) =
 Termx.skolemization s >>= fun rho ->
 let s' = Sub.apply_term rho s in
 unify (s',t) >>= function
  | None -> return None
  | Some sigma ->
   let ohr = reverse_skolemization rho in
   return (Some (List.map (Sub.map (tsub_apply ohr)) sigma))
;;

(* TESTS *)
let test () =
 let sigma = Sig.empty 20 in
 let x,sigma = Sig.create_var "x" sigma in
 let y,sigma = Sig.create_var "y" sigma in
 let z,sigma = Sig.create_var "z" sigma in
 let u,sigma = Sig.create_var "u" sigma in
 let v,sigma = Sig.create_var "v" sigma in
 let w,sigma = Sig.create_var "w" sigma in
 let x,y,z,u,v,w = T.Var x, T.Var y, T.Var z, T.Var u, T.Var v, T.Var w in
 let f,sigma = Sig.create_fun 2 "f" sigma in
 let f,sigma = Sig.set_theory f U.Label.AC sigma in
 let h,sigma = Sig.create_fun 2 "h" sigma in
 let h,sigma = Sig.set_theory h U.Label.AC sigma in
 let a,sigma = Sig.create_fun 0 "a" sigma in
 let b,sigma = Sig.create_fun 0 "b" sigma in
 let c,sigma = Sig.create_fun 0 "c" sigma in
 let d,sigma = Sig.create_fun 0 "d" sigma in
 let e,sigma = Sig.create_fun 0 "e" sigma in
 let g,sigma = Sig.create_fun 1 "g" sigma in
 let a_ = T.Fun(a, []) in
 let b_ = T.Fun(b, []) in
 let c_ = T.Fun(c, []) in
 let d_ = T.Fun(d, []) in
 let e_ = T.Fun(e, []) in
 let faa = T.Fun(f, [a_;a_]) in
 let faaa = T.Fun(f, [a_;faa]) in
 let fabc = T.Fun(f, [a_;T.Fun(f, [b_;c_])]) in
 let fcab = T.Fun(f, [c_;T.Fun(f, [a_;b_])]) in
 let faby = T.Fun(f, [a_;T.Fun(f, [b_;y])]) in
 let fxy = T.Fun(f, [x;y]) in
 let fxx = T.Fun(f, [x;x]) in
 let fyy = T.Fun(f, [y;y]) in
 let fzz = T.Fun(f, [z;z]) in
 let fax = T.Fun(f, [a_;x]) in
 let gx = T.Fun(g, [x]) in
 let ga = T.Fun(g, [a_]) in
 let gb = T.Fun(g, [b_]) in
 let gu = T.Fun(g, [u]) in
 let fgax = T.Fun(f, [ga; x]) in
 let fgay = T.Fun(f, [ga; y]) in
 let gfaa = T.Fun(g,[faa]) in
 let gfax = T.Fun(g,[fax]) in
 let t2 = T.Fun(f, [T.Fun(h,[z;v]);T.Fun(h,[u;v]);w]) in
 let rec flist = function
   [s; t] -> T.Fun(f, [s; t]) 
  | s :: ts -> T.Fun(f, [s; flist ts]) 
  | _ -> failwith "Unexpected pattern"
 in
 let t_lc88_1 = flist [c_;c_;gu; x] in
 let t_lc88_2 = flist [b_;ga;y; z] in
 (* testing unification *)
 let run_unify (s,t) = Either.right (M.run sigma (
  Termx.flatten s >>= fun s ->
  Termx.flatten t >>= fun t ->
  unify (s,t))) in
 let print_subs (s,t) = let (s,n) = Either.right (M.run sigma (
  Termx.flatten s >>= fun s ->
  Termx.flatten t >>= fun t ->
  T.to_stringm s >>= fun ss ->
  T.to_stringm t >>= fun ts ->
  let m = "To unify "^ss^" and "^ts^", " in
  unify (s,t) >>= function 
    None -> return (m^"none", 0)
  | Some us -> 
   let m' = m^(string_of_int (List.length us))^" substitutions\n" in
   foldl 
    (fun s u -> print_sub u >>= fun us -> return (s^"\n or\n"^us)) 
    m' us >>= fun s -> return (s, List.length us)
  )) in Format.printf "%s\n" s; n in
 let assert_some ts = 
  assert(ignore (print_subs ts); Option.is_some (run_unify ts)) in
 let assert_more ts n =
  assert(Option.is_some (run_unify ts)); assert (print_subs ts = n) in
 let assert_none ts = assert(Option.is_none (run_unify ts)) in
 assert_some (x,x);
 assert_some (fxy,t2);
 assert_some (x,y);
 assert_some (x,a_);
 assert_some (a_, y);
 assert_some (a_,a_);
 assert_none (ga,gb);
 assert_some (gx,ga);
 assert_some (faa, y);
 assert_some (gx,gfaa);
 assert_none (a_, gx);
 assert_none (x, gx);
 assert_some (y, gx);
 assert_none (faa, faaa);
 assert_some (fcab, fabc);
 assert_some (faa, fax);
 assert_some (gfaa, gfax);
 assert_some (faaa, fax);
 assert_some (fabc, fax);
 assert_some (faaa, fxy);
 assert_some (fabc, fxy);
 assert_some (faby, fxy);
 assert_some (T.Fun(f,[u; gfaa]), T.Fun(f, [gfax; y]));
 assert_none (fgax, fax);
 assert_some (fgay, fax);
 assert_none (fgay, faaa);
 assert_some (fgay, fgay);
 assert_some (T.Fun(f, [T.Fun(g,[faa]); u]), faby);
 (* some examples from christian/lincoln 88 *)
 assert_some (t_lc88_1,t_lc88_2);
 assert_some (flist [w;x;gx],flist [y;u;gu]);
 assert_more (flist [x;a_;b_],flist [u;c_;d_;e_]) 2;
 assert_more (flist [x;a_;b_],flist [u;c_;c_;d_]) 2;
 assert_more (flist [x;a_;b_],flist [u;c_;c_;c_]) 2;
 assert_more (flist [x;a_;b_],flist [u;y;c_;d_]) 12;
 assert_more (flist [x;a_;b_],flist [u;y;c_;c_]) 12;
 assert_more (flist [x;a_;b_],flist [u;y;z;c_]) 30;
 assert_more (flist [x;a_;b_],flist [u;y;z;v]) 56;
 assert_more (flist [x;a_;a_],flist [u;c_;d_;e_]) 2;
 assert_more (flist [x;a_;a_],flist [u;c_;c_;d_]) 2;
 assert_more (flist [x;a_;a_],flist [u;c_;c_;c_]) 2;
 assert_more (flist [x;a_;a_],flist [u;y;c_;d_]) 8;
 assert_more (flist [x;a_;a_],flist [u;y;v;c_]) 18;
 assert_more (flist [x;a_;a_],flist [u;y;z;v]) 32;
 assert_more (flist [x;y;a_],flist [u;c_;d_;e_]) 28;
 assert_more (flist [x;y;a_],flist [u;c_;c_;d_]) 20;
 assert_more (flist [x;y;a_],flist [u;v;c_;d_]) 88;
 assert_more (flist [x;y;a_],flist [u;v;z;c_]) 204;
 assert_more (flist [x;y;z],flist [u;v;c_;d_]) 336;
 assert_more (flist [x;y;z],flist [u;v;w;c_]) 870;
 (* examples with repeated variables *)
 assert_some (T.Fun(f,[x]), T.Fun(f,[y]));
 assert_some (fxy,fzz);
 assert_some (fxx,fyy);
 assert_some (flist [x;x;gx], flist[u;u;gu]);
(* testing matching *)(*
 let test_skolem t =
  skolemization t >>= fun theta ->
  let ateht = reverse_skolemization theta in
  let t' = tsub_apply ateht (Sub.apply_term theta t) in
  assert (t=t'); 
  T.to_stringm (Sub.apply_term theta t) >>= fun s ->
  T.to_stringm t'>>= fun s' -> 
  return (s,s')
 in
 M.run sigma (test_skolem fxy);
 M.run sigma (test_skolem (flist [x;y;a_]));
let run_matches (s,t) = Either.right (M.run sigma (matches (s,t)));
let print_msubs (s,t) = let (s,n) = Either.right (M.run sigma (
 T.to_stringm s >>= fun ss ->
 T.to_stringm t >>= fun ts ->
 let m = "To match "^ss^" with "^ts^", " in
 matches (s,t) >>= function
   None -> return ("none", 0)
 | Some us ->
  let m' = m^(string_of_int (List.length us))^" substitutions\n" in
  foldl
   (fun s u -> Sub.to_stringm u >>= fun us -> return (s^"\n or\n"^us))
   m' us >>= fun s -> return (s, List.length us)
 )) in Format.printf "%s\n" s; n in
 let assert_msome ts =
  assert(Option.is_some (run_matches ts)); print_msubs ts in
 let assert_mmore ts n =
  assert(Option.is_some (run_matches ts)); assert (print_msubs ts = n) in
 let assert_mnone ts = assert(Option.is_none (run_matches ts)) in
 assert_msome (x,x);
 assert_msome (x,y);
 assert_mnone (x,a_);
 assert_msome (a_, y);
 assert_msome (a_,a_);
 assert_mnone (gx,ga);
 assert_msome (faa, y);
 assert_msome (gfaa,gx);
 assert_mnone (a_, gx);
 assert_msome (gx, x); (* ! - but with renamed rules no problem *)
 assert_mnone (y, gx);
 assert_mnone (faa, faaa);
 assert_msome (fcab, fabc);
 assert_msome (faa, fax);
 assert_msome (gfaa, gfax);
 assert_msome (faaa, fax);
 assert_msome (fabc, fax);
 assert_msome (faaa, fxy);
 assert_msome (fabc, fxy);
 assert_msome (faby, fxy);
 assert_msome (T.Fun(f,[u; gfaa]), T.Fun(f, [gfax; y]));
 assert_msome (flist[ga;u;w], fxy);
 assert_mnone (fgay, fax);
 assert_mnone (fgay, faaa);
 assert_msome (fgay, fgay);
 assert_msome (flist [a_;ga;b_;x], flist[a_;y;ga;b_]);*)
;;

(*test ()*)
