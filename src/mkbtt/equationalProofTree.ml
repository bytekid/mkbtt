(*** SUBMODULES **********************************************************)
module C = Conversion;;
module W = World;;
module Term = U.Term;;
module Rule = U.Rule;;
module Fun = Rewriting.Function;;
module Pos = Rewriting.Position;;
module Sub = U.Substitution;;
module Elogic = U.Elogic;;

(*** OPENS ***************************************************************)
open Types.Node;;
open World;;
open World.Monad;;
open Util;;

(*** TYPES ***************************************************************)
type t = 
   Refl of Term.t
 | Sym of t
 | Trans of t * t
 | Assm of Rule.t * Sub.t
 | Cong of Fun.t * t list

(*** FUNCTIONS ***********************************************************)
let match_sub (s,t) (l,r) =
try
 let tau1 = Elogic.match_term s l in
 let tau2 = Elogic.match_term t r in
 return (Sub.union tau1 tau2)
with Elogic.Not_matchable ->
 W.M.Rule.to_stringm (Rule.of_terms s t) >>= fun st ->
 W.M.Rule.to_stringm (Rule.of_terms l r) >>= fun lr ->
 failwith (st ^ " does not match " ^ lr)
;;

let rec grow s (ix,b) p t =
(* W.M.Term.to_stringm s >>= fun ss ->
 W.M.Term.to_stringm t >>= fun ts ->
 Format.printf "growing (%s, (%i,%i),%s,%s)\n%!" 
  ss ix (if b then 1 else 0) (Pos.to_string p) ts;*)
 if p = Pos.root then
  if b then
   IndexedNode.brule true ix >>= fun rl ->
   match_sub (s,t) (Rule.to_terms rl) >>= fun sigma ->
   return (Assm (rl, sigma))
  else
   grow t (ix,true) p s >>= fun tree ->
   return (Sym tree)
 else
  let i,q = Pos.split_first p in
  let f = Option.the (Term.root s) in
  let tj j = Term.subterm (Pos.make j) t in
  let subtree j sj =
   if i = j then grow sj (ix,b) q (tj j) else return (Refl sj)
  in
  mapi subtree (Term.args s) >>= fun trees ->
  return (Cong (f, trees))
;;

let rec plant_and_grow = function
 | C.Empty t :: _ -> return (Refl t)
 | C.Step (s,(i,b),p,t) :: seq -> 
  plant_and_grow seq >>= fun tree ->
  grow s (i,b) p t >>= fun branch ->
  return (Trans (branch, tree))
;;
