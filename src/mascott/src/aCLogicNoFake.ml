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
module A = ACLogicNoFakeAux;;

(*** OPENS (2) ***********************************************************)
open M;;

(*** EXCEPTIONS **********************************************************)
exception Repeated_variables

(*** FUNCTIONS ***********************************************************)
let t_solve = ref 0.0

(* zero substitution list {{}} *)
let zero_list = [Sub.empty]


let is_ac_symbol = M.is_theory L.AC

let ac_root = function
 | T.Var _ -> return None
 | T.Fun(f,_) -> 
  is_ac_symbol f >>= fun b -> return (if b then Some f else None)
;;


(* Given terms s and t, return complete set of unifiers for s=_{AC} t.
   Calls unify' and simplifies returned set of bindings in that all
   bindings x -> t are removed where x does not occur in s or t. *)
let rec unify_me c (s, t) =
 (*T.to_stringm s >>= fun rs ->
 T.to_stringm t  >>= fun ts ->
 Format.printf "\nNo Fake: Unification start %s with %s (%i)\n%!" ts rs c;*)
 (* restrict substitution to variables of interest *)
 let vars = List.union (T.vars s) (T.vars t) in
 let add x t s = if List.mem x vars then Sub.add x t s else s in
 let simplify s =  Sub.fold add s Sub.empty in
 (lift (Option.map (List.map simplify))) (unify (c+1) (s, t))
(* Given terms s and t, return complete set of unifiers for s=_{AC} t.
   When matching, s is ground.  *)
and unify c (s,t) =
 (*T.to_stringm s >>= fun rs ->
 T.to_stringm t  >>= fun ts ->
 Format.printf "\nUnify %s with %s (%i)\n%!" ts rs c;*)
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
    Termx.flatten s >>= fun s' -> Termx.flatten t >>= fun t' ->
    (* (1) remove common arguments *)
    let ss', ts' = A.remove_common_args (T.args s') (T.args t') in
    if List.is_empty ss' && List.is_empty ts' then
     return (Some [Sub.empty])
    else if List.is_empty ss' || List.is_empty ts' then
     return None
    else (
     (* (2) abstract non-variable terms *)
     (*Format.printf "Before abstract\n%!";*)
     A.abstract (T.Fun(f,ss')) (T.Fun(f,ts')) >>= fun (cs,xs,ys,sigma) ->
      (*project A.str_vars (xs,ys) >>= fun (xss,yss) ->
      Format.printf "Abstractions: %s and %s \n Constraint:%!" xss yss;
      A.print_constraints cs >>= fun _ ->*)
     (* (3) make polynomial *)
     (* (4) solve diophantine equations *)
     let b = A.dio_solve xs ys in
      (*A.str_assignments b >>= fun bs ->
      Format.printf "Assignments: %s\n%!" bs;*)
     (* (5) construct semigroup representation, fresh vars for rows *)
     A.to_semigroup f b >>= fun (b',vars) ->
     (*A.list_to_stringm (A.list_to_stringm (A.list_to_stringm T.to_stringm)) (Array.to_list (Array.map Array.to_list b')) >>= fun bs ->
     Format.printf "to semigroup yields %s\n%!" bs;*)
     (* (6) find usable subsets of rows in b *)
     let bsubsets = A.subsets (ss',ts') b in
     (*project A.str_terms (ss',ts') >>= fun (xss,yss) ->
      Format.printf "Term lists are %s and %s \n%!" xss yss;
     let s = A.list_to_stringn (A.list_to_string string_of_int) bsubsets in
     Format.printf "Subsets are %s\n%!" s;*)
     (* (7) construct semigroup unifiers *)
     let gamma = A.sum_up bsubsets b' (xs,ys) f in
     (*Format.printf "Gamma: \n%!";
     A.print_subs gamma >>= fun _ ->*)
     (* (8) inverse abstraction from step (2) *)
     (*let gamma' = A.unabstract sigma gamma in*)
     (* remembered in constraints instead *)
     (* (9) make substitutions idempotent *)
     let theta = A.make_all_idempotent gamma in
     (* (10) recursively solve subproblems *)
     unify_with_list (c+1) cs theta
  ) ))
(* Given list (i.e. conjunction) of equations es and set of substitutions
   subs, take union of CSU(es\theta) over all \theta in subs, where CSU(X)
   is complete set of unifiers for X. *)
and unify_with_list c es subs =
 let add csus theta = 
  let compose s = Sub.compose Sub.apply_term theta s in
  let cs = A.sub_constraints theta es in
  unify_conjunction c cs >>= function
  | None -> return csus
  | Some z -> return ((List.map compose z) @ csus)
 in
 (*Format.printf "Called unify list with initial constraints:\n%!";
 A.print_constraints es >>= fun _ ->
 Format.printf "(%i) and substitutions \n%!" c;
 A.print_subs subs >>= fun _ ->*)
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
 | [e1] -> unify c e1
 | e1 :: es -> unify c e1 >>= function
   | None -> return None | Some ss -> unify_with_list c es ss
;;

let unify ts =
(* T.to_stringm (fst ts) >>= fun s ->
 T.to_stringm (snd ts)  >>= fun s' ->
 Format.printf "\nNo Fake: Unification start %s with %s \n%!" s s';*)
 unify_me 0 ts (*>>= fun subs -> (
 match subs with | None -> Format.printf "No Result\n"; return ()
 | Some s -> Format.printf "Result: \n%!"; A.print_subs s) >>
 return subs*)
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
   iter (fun f -> 
    project (Termx.flatten <.> (Sub.apply_term f)) (s,t) >>= fun (s,t) ->
    assert (T.equal s t); return () ) us >>
   foldl 
    (fun s u -> A.print_sub u >>= fun us -> return (s^"\n or\n"^us)) 
    m' us >>= fun s -> return (s, List.length us)
  )) in Format.printf "%s\n" s; n in
 let assert_some ts = 
  assert(ignore (print_subs ts); Option.is_some (run_unify ts)) in
 let assert_more ts n =
  assert(Option.is_some (run_unify ts)); assert (print_subs ts = n) in
 let assert_none ts = assert(Option.is_none (run_unify ts)) in
 assert_some (x,x);
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
 assert_some (faa, fxy);
 assert_some (flist[a_;b_], fxy);
 assert_some (fabc, fxy);
 assert_some (faby, fxy);
 assert_some (T.Fun(f,[u; gfaa]), T.Fun(f, [gfax; y]));
 assert_none (fgax, fax);
 assert_some (fgay, fax);
 assert_none (fgay, faaa);
 assert_some (fgay, fgay);
 assert_some (T.Fun(f, [T.Fun(g,[faa]); u]), faby);
 (* examples with repeated variables *)
 assert_some (fxx,fyy);
 assert_some (fxy,fzz);
(* assert_some (flist [x;x;gx], flist[u;u;gu]);
 assert_some (flist [w;x;gx],flist [y;u;gu]);
 (* some examples from christian/lincoln 88 *)
 assert_some (t_lc88_1,t_lc88_2);
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
 assert_more (flist [x;a_;a_],flist [u;y;c_;d_]) 12; (* not just 8 *)
 assert_more (flist [x;a_;a_],flist [u;y;v;c_]) 30; (* not just 18 *)
 assert_more (flist [x;a_;a_],flist [u;y;z;v]) 56; (* not just 32 *)
 assert_more (flist [x;y;a_],flist [u;c_;d_;e_]) 28;
 assert_more (flist [x;y;a_],flist [u;c_;c_;d_]) 28; (* not just 20 *)
 assert_more (flist [x;y;a_],flist [u;v;c_;d_]) 88;
 assert_more (flist [x;y;a_],flist [u;v;z;c_]) 204;
 assert_more (flist [x;y;z],flist [u;v;c_;d_]) 336;
 assert_more (flist [x;y;z],flist [u;v;w;c_]) 870;*)
;;

(* test () *)
