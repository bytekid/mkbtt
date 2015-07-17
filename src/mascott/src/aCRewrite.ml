(*** OPENS ***************************************************************)
open Util;;

(*** SUBMODULES **********************************************************)
module L = List;;
module Elogic = U.Elogic;;
module Var = Rewriting.Variable;;
module Fun = Rewriting.Function;;
module Pos = Rewriting.Position;;
module T = U.Term;;
module C = U.Context;;
module R = U.Rule;;
module Trs = U.Trs;;
module Sig = U.Signature;;
module Label = U.Label;;
module Sub = U.Substitution;;
module M = U.Monad;;
module ACPos = ACPosition;;
module Tx = Termx;;
module ACL = ACLogic;;
module ACUL = ACLogic;;
module ACD = ACDiscTree;;
module Rx = Rulex;;

(*** OPENS (2) ***********************************************************)
open M;;

(*** FUNCTIONS ***********************************************************)

let t_rewrite1 = ref 0.0

let t_rewrite2 = ref 0.0

let t_match = ref 0.0

let mcount = ref 0

let mrepcount = ref 0

let is_ac_symbol = M.is_theory Label.AC

let mycache f k table =
 try Hashtbl.find table k with
 Not_found -> (
  let v = f k in
  Hashtbl.add table k v;
  v)
;;

let mcache f k table =
 let x = try Some (Hashtbl.find table k) with Not_found -> None in
 match x with
  | None -> (
   let tstart = Unix.gettimeofday () in
   f k >>= fun v ->
   t_rewrite1 := (Unix.gettimeofday () -. tstart) +. !t_rewrite1;
   Hashtbl.add table k v;
   return v)
  | Some v -> return v
;;

let mcache2 f k table =
 let x = try Some (Hashtbl.find table k) with Not_found -> None in
 match x with
  | None -> (
   let tstart = Unix.gettimeofday () in
   f k >>= fun v ->
   t_rewrite2 := (Unix.gettimeofday () -. tstart) +. !t_rewrite2;
   Hashtbl.add table k v;
   return v)
  | Some v -> return v
;;

let mcache1 f k table =
 mcount := !mcount + 1;
 let x = try Some (Hashtbl.find table k) with Not_found -> None in
 match x with
  | None -> (
   let tstart = Unix.gettimeofday () in
   f k >>= fun v ->
   t_match := !t_match +. (Unix.gettimeofday () -. tstart);
   Hashtbl.add table k v;
   return v)
  | Some v -> (mrepcount := !mrepcount + 1; return v)
;;




let matchcache : (T.t * T.t, Sub.t list option) Hashtbl.t = Hashtbl.create 300

let matches (s,l) = mcache1 ACL.matches (s,l) matchcache

let rewrite_at t rule (c,s) =
 let l, r = R.to_terms rule in
 let rewrite_with sigma = C.apply (Sub.apply_term sigma r) c, sigma in
 matches (s,l) >>= function
  | None -> return []
  | Some subs -> 
   (*T.to_stringm s >>= fun ss ->
   T.to_stringm l >>= fun sl ->
   Format.printf "%s matches %s\n%!" ss sl;*)
  map (apply Tx.flatten return) (L.map rewrite_with subs)
;;

let is_variant' s t =
 let rec is_variant (s,t) = function
  | None -> None
  | Some sigma -> ( match (s,t) with
   | T.Fun(f, ss),T.Fun(g,ts) when f = g ->
    if L.length ss <> (L.length ts) then None
    else L.foldl (flip is_variant) (Some sigma) (L.zip ss ts)
   | T.Var x, ((T.Var y) as t) -> 
    (try Some (Sub.add x t sigma) with Sub.Inconsistent -> None)
   | _ -> None)
 in match is_variant (s,t) (Some Sub.empty) with
  None -> false | Some sigma -> Sub.is_renaming sigma
;;

let variants : (T.t * T.t, bool) Hashtbl.t = Hashtbl.create 50;;
let is_variant s t = mycache (Util.uncurry is_variant') (s,t) variants;;

(* covers all cases where rewriting of lhs of rule is allowed according
to Bachmair: Completion for Rewriting module Congruence, i.e.
 - AC-rewriting below root
 - standard rewriting at root when t strictly encompasses the lhs of rl *)
let rewrite2' t p rl = 
 if p = ACPos.root t then
  let are_variants = is_variant t (R.lhs rl) in
  if are_variants then return [] else rewrite_at t rl (C.Hole,t)
 else
  let c,s = Termx.context_subterm t p in
  rewrite_at t rl (c,s)
;;

let rewrite2cache : (T.t * ACPos.t * R.t, (T.t * Sub.t) list) Hashtbl.t 
 = Hashtbl.create 500;;

let rewrite2 t p rl = 
 mcache2 (Util.uncurry3 rewrite2') (t,p,rl) rewrite2cache
;;

(* Overapproximates all cases where the lhs of a rule can not be reduced.
   (Actually standard_rewrite_at_root should be removed, but does not 
   hurt.) *)
let rewrite1' t rl = 
 let are_variants = is_variant t (R.lhs rl) in
 if are_variants then rewrite_at t rl (C.Hole,t) else return []
;;

let rewrite1cache : (T.t * R.t, (T.t * Sub.t) list) Hashtbl.t = Hashtbl.create 500;;
let rewrite1 t rl = mcache (Util.uncurry rewrite1') (t,rl) rewrite1cache;;


let reducts2 t pos rl =
 let are_var = is_variant t (R.lhs rl) in
 let pos = if are_var then List.remove (ACPos.root t) pos else pos in
(* let fpos = if are_variants then funs_pos_below_root else funs_pos in
 fpos t >>= fun pos ->*)
 let cs = L.map (fun p -> Termx.context_subterm t p, p) pos in
 let map_pos p ts = map (fun (t,s) -> return (t,s,p)) ts in
 let rp (cs,p) = rewrite_at t rl cs >>= map_pos p in
 flat_map rp cs
;;

let reducts t pos rl =
 rewrite1 t rl >>= map (fun (t,s) -> return (t,s,ACPos.root t)) >>= fun rs ->
 reducts2 t pos rl >>= fun rs' ->
 return (List.rev_append rs rs')
;;

let reducible_below_root t pos rl =
(* funs_pos_below_root t >>= fun pos ->*)
 let pos = List.remove (ACPos.root t) pos in
 let cs = L.map (Termx.context_subterm t) pos in
 let not_empty = function [] -> return false | _ -> return true in
 let check t rl p = rewrite_at t rl p >>= not_empty in
 let bcheck b p = if b then return b else check t rl p in
 foldl bcheck false cs
;;

let reducible_at t p rl =
 let c,s = Termx.context_subterm t p in
 matches (s,R.lhs rl) >>= function
  | None -> return false
  | Some subs -> return true
;;

let rewrite_with_at t rl p =
 try
  let c,s = Termx.context_subterm t p in rewrite_at t rl (c,s)
 with _ -> failwith "ACRewrite.rewrite_with_at"
;;

let map_none f x = function Some r -> return (Some r) | None -> f x

let rec step trs = function
   | T.Var x -> return None
   | (T.Fun (f,ts) as t) ->
    (*T.to_stringm t >>= fun s -> Format.printf "step with %s\n%!" s;*)
    let rew rl = 
     rewrite_with_at t rl (ACPos.root t) >>= function
     [] -> return None | (t', _) :: _ -> return (Some t')
    in
    let consider rl =
     Rx.extend rl >>= (function None -> return [] | Some r -> return [r]) >>= fun l ->
     foldr (map_none rew) None (rl::l)
    in foldr (map_none consider) None (Trs.to_list trs) >>= function
     | Some u -> return (Some u)
     | None -> (
      let rfold (us,b) ti =
       step trs ti >>= function 
        None -> return (us@[ti], b) | Some ui -> return (us@[ui], true)
      in
      foldl rfold ([], false) ts >>= fun (us, b) ->
      if b then return (Some (T.Fun (f,us))) else return None)

and normalize trs t =
(* T.to_stringm t >>= fun ts ->
 Format.printf "normalize %s\n%!" ts;*)
 step trs t >>= function 
   None -> return t
 | Some u -> Tx.flatten u >>= fun u' -> normalize trs u'
;;

let nfcache : (T.t * R.t, T.t) Hashtbl.t = Hashtbl.create 300

let nf_with simp t p (rl,idx) =
(* T.to_stringm t >>= fun ts ->
 R.to_stringm rl >>= fun rls ->
 Format.printf "NF of %s with %s at %s\n%!" ts rls (ACPos.to_string p);*)
 let rec nf_with t res =
  step (Trs.of_list [rl]) t >>= function 
    None -> return res 
  | Some u -> Tx.flatten u >>= normalize simp >>= 
   Tx.flatten >>= fun u' -> nf_with u' [u']
 in nf_with t []
;;


let narrow_at t rule (c,s) =
(* T.to_stringm s >>= fun ss ->
 Format.printf " Narrow subterm %s\n%!" ss;*)
 let l, r = R.to_terms rule in
 let about sigma = sigma, Sub.apply_term sigma s, Sub.apply_term sigma t in
 let narrow_with sigma = Sub.apply_term sigma (C.apply r c),about sigma in
 (ACUL.unify_cache (s,l) >>= function
  | None -> return []
  | Some subs -> map (apply Tx.flatten return) (L.map narrow_with subs))
;;

let narrow t pos rl =
 let cs = L.map (fun p -> Termx.context_subterm t p, p) pos in
 let map_pos p ts = map (fun (t,s) -> return (t,p,s)) ts in
 let np ((c,s),p) = narrow_at t rl (c,s) >>= map_pos p in
 flat_map np cs
;;

let narrow_below_root t pos rl =
(* T.to_stringm t >>= fun ts ->
 R.to_stringm rl >>= fun rls ->
 Format.printf "Narrow below root: %s with %s at %s\n%!" ts rls (all_to_string pos);*)
 let pos = L.filter (fun p -> p <> (ACPos.root t)) pos in
 let cs = L.map (fun p -> Termx.context_subterm t p,p) pos in
 let map_pos p ts = map (fun (t,s) -> return (t,p,s)) ts in
 let np ((c,s),p) = narrow_at t rl (c,s) >>= map_pos p in
 flat_map np cs
;;

let joinable (s,t) trs =
 normalize trs s >>= fun s' ->
 normalize trs t >>= fun t' ->
 Termx.ac_equivalent s' t'
;;

(* TESTS *)
(*
let sigma = Sig.empty 20;;
let x,sigma = Sig.create_var "x" sigma;;
let y,sigma = Sig.create_var "y" sigma;;
let z,sigma = Sig.create_var "z" sigma;;
let u,sigma = Sig.create_var "u" sigma;;
let v,sigma = Sig.create_var "v" sigma;;
let w,sigma = Sig.create_var "w" sigma;;
let x,y,z,u,v,w = T.Var x, T.Var y, T.Var z, T.Var u, T.Var v, T.Var w;;
let f,sigma = Sig.create_fun 2 "f" sigma;;
let a,sigma = Sig.create_fun 0 "a" sigma;;
let b,sigma = Sig.create_fun 0 "b" sigma;;
let c,sigma = Sig.create_fun 0 "c" sigma;;
let d,sigma = Sig.create_fun 0 "d" sigma;;
let e,sigma = Sig.create_fun 0 "e" sigma;;
let g,sigma = Sig.create_fun 1 "g" sigma;;
let a_ = T.Fun(a, []);;
let b_ = T.Fun(b, []);;
let c_ = T.Fun(c, []);;
let d_ = T.Fun(d, []);;
let e_ = T.Fun(e, []);;
let faa = T.Fun(f, [a_;a_]);;
let faaa = T.Fun(f, [a_;faa]);;
let fabc = T.Fun(f, [a_;T.Fun(f, [b_;c_])]);;
let fcab = T.Fun(f, [c_;T.Fun(f, [a_;b_])]);;
let faby = T.Fun(f, [a_;T.Fun(f, [b_;y])]);;
let fxy = T.Fun(f, [x;y]);;
let rec flist = function
 [s; t] -> T.Fun(f, [s; t]) | s :: ts -> T.Fun(f, [s; flist ts]);;
(* testing funs_pos *)
M.run sigma (flatten(flist[x;a_;ga;gu]) >>= 
 funs_pos >>= fun ps -> return (L.map to_string ps));;
M.run sigma (flatten(flist[a_;b_;c_]) >>= 
 funs_pos >>= fun ps -> return (L.map to_string ps));;
(* testing context_subterms *)
let context_subterms t =
 flatten t >>= fun t -> funs_pos t >>= fun ps ->
 Term.to_stringm t >>= fun ts ->
 Format.printf "Flattened term is %s\n" ts;
 let cs = L.map (context_subterm t) ps in
 map (fun (c,t) -> 
  Term.to_stringm t >>= fun ts -> 
  C.to_stringm c >>= fun cs -> return (cs,ts)) cs;;
M.run sigma (context_subterms fabc);;
(* testing rewrite *)
let reducts f t rule =
 flatten t >>= fun t ->
 Term.to_stringm t >>= fun ts ->
 R.to_stringm rule >>= fun rs ->
 Format.printf "Rewriting %s with %s yields\n" ts rs;
 f t rule >>=
   foldl (fun s u -> T.to_stringm u >>= fun v -> return (s^", "^v)) ""
 >>= fun s -> Format.printf "%s\n" s; return ();;
let rewrite_steps = reducts rewrite;;
M.run sigma (rewrite_steps fabc (R.of_terms fxy d_));;
M.run sigma (rewrite_steps fabc (R.of_terms gx d_));;
M.run sigma (rewrite_steps fabc (R.of_terms fxy gx));;
M.run sigma (rewrite_steps fabc (R.of_terms (flist [x;x]) gx));;
M.run sigma (rewrite_steps (flist[x;x;y]) (R.of_terms (flist [x;x]) gx));;
M.run sigma (rewrite_steps gfaa (R.of_terms (flist [x;x]) gx));;
let narrow_steps = reducts narrow;;
M.run sigma (narrow_steps fabc (R.of_terms (flist [x;x]) gx));;
M.run sigma (narrow_steps (flist[x;x;y]) (R.of_terms (flist [x;x]) gx));;
M.run sigma (narrow_steps (flist[x;x;y]) (R.of_terms (flist [a_;b_; c_]) ga));;
M.run sigma (narrow_steps (flist[x;gu;u]) (R.of_terms ga b_));;
*)

