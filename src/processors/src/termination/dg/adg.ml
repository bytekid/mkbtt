(* Copyright 2008 Martin Korp, Christian Sternagel, Harald Zankl
 * GNU Lesser General Public License
 *
 * This file is part of TTT2.
 * 
 * TTT2 is free software: you can redistribute it and/or modify it under
 * the terms of the GNU Lesser General Public License as published by the
 * Free Software Foundation, either version 3 of the License, or (at your
 * option) any later version.
 * 
 * TTT2 is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public
 * License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public
 * License along with TTT2. If not, see <http://www.gnu.org/licenses/>.
 *)

(*** OPENS ********************************************************************)
open Util;;
open Rewritingx;;

(*** MODULES ******************************************************************)
module Automata = Automata.Default (Rewritingx);;
module A = Automata.Automaton;;
module C = Complexity;;
module F = Format;;
module G = Graph;;
module L = List;;
module M = Automata.Monad;;
module P = Problem;;
module Perv = Pervasives;;
module Pos = Position;;
module R = Rule;;
module Reach = Automata.Reachability;;
module S = Automata.Status;;
module Sub = Substitution;;
module T = Automata.Term;;

module Approximation = struct
 (*** FUNCTIONS ***************************************************************)
 let (>>=) = M.(>>=);;
 let (>>) = M.(>>);;

 let modify f g =
  let of_term = uncurry R.of_terms and combine l = M.lift (pair l) in
  let apply f g r = f (R.lhs r) >>= fun l -> combine l (g (R.rhs r)) in
  let add r m = apply f g r >>= fun r -> M.lift (Trs.add (of_term r)) m in
  M.lift Trs.unique <.> Trs.fold add (M.return Trs.empty)
 ;;
 
 let strong =
  modify (M.liftm <.> Term.ren) (const (M.lift Term.make_var M.fresh_var))
 ;;

 let rename = modify (M.liftm <.> Term.ren) (M.liftm <.> Term.ren);;
 
 let linearize l =
  let min p q = if Pos.length p <= Pos.length q then p else q in
  let position x l = L.foldl1 min (Term.var_pos x l) in
  let ps = L.foldl (fun ps x -> position x l :: ps) [] (Term.vars l) in
  M.liftm (Term.ren ~p:(const <.> not <.> flip L.mem ps) l)
 ;;
 
 let adapt l r =
  let hd ps = try L.hd ps with Failure _ -> Pos.root in
  let check x = 1 >= Pos.length (hd (Term.var_pos x l)) in
  let add s x = M.lift (flip (Sub.add x) s <.> Term.make_var) M.fresh_var in
  let update s x = if check x then M.return s else add s x in
  M.lift (flip Sub.apply_term r) (M.foldl update Sub.empty (Term.vars r))
 ;;
 
 let growing =
  let add r m =
   let l = R.lhs r and r = R.rhs r in
   linearize l >>= fun l -> M.lift (R.of_terms l) (adapt l r) >>= fun r ->
   M.lift (Trs.add r) m
  in
  M.lift Trs.unique <.> Trs.fold add (M.return Trs.empty)
 ;;
end

module Initial = struct
 (*** OPENS *******************************************************************)
 open Automata;;

 (*** FUNCTIONS ***************************************************************)
 let (>>=) = M.(>>=);;
 let (>>) = M.(>>);;

 let min dps trs r =
  let cp = Path.fresh_reuse ~f:(const (const M.fresh_state)) in
  let fresh = const (M.lift List.singleton M.fresh_state) in
  let fs = L.union (L.diff (Trs.funs dps) (Trs.roots dps)) (Trs.funs trs) in
  (if fs = [] then M.fresh_fun >>= fun c -> M.add_ari c 0 >> M.return [c]
  else M.return fs) >>= flip (Initial.instances fresh cp []) [T.of_term r]
 ;;
end

module Map = struct
 (*** MODULES *****************************************************************)
 module Term = struct
  type context = Hole | More of Function.t * context list;;
  type t = Term.t = Var of Variable.t | Fun of Function.t * t list;;

  let rec transform = function
   | Var _ -> Hole
   | Fun (f,ts) -> More (f,List.map transform ts)
  ;;

  let compare s = compare (transform s) <.> transform;;
  let fprintf = Term.fprintf;;
 end

 (*** INCLUDE *****************************************************************)
 include Map.Partial (Term);;
end

(*** TYPES ********************************************************************)
type approximation = Rename | Growing | Strong;;
type flags = {help : bool ref; a : approximation ref};;

type proof = {
 maps : A.t Map.t * A.t Map.t;
 approximation : approximation;
 status : S.t}
;;

type t = P.t * proof * P.t;;

(*** GLOBALS ******************************************************************)
let code = "adg";;
let name = "ADG Processor";;
let keywords = ["dependency graph";"approximation";"termination"];;
let flags = {help = ref false; a = ref Rename;};;

let comment =
 "Remove all edges from the current DG that are not contained in the \
  ADG (approximation of the DG using tree automata techniques and
  regularity presvering approximations of the given TRS)."
;;

let spec =
 let set_approximation = function
  | "growing" -> flags.a := Growing
  | "newvars" -> flags.a := Rename
  | "strong" -> flags.a := Strong
  | s -> raise (Arg.Bad (Format.sprintf "unknown approximation `%s'." s))
 in
 let spec = [
  ("--help",Arg.Set flags.help,"Prints information about flags.");
  ("-help",Arg.Set flags.help,"Prints information about flags.");
  ("-h",Arg.Set flags.help,"Prints information about flags.");
  ("-a",Arg.String set_approximation,
   "Specifies the approximation used to estimate the DG. Possible values \
    are `strong', `newvars', and `growing'. Per default `nv' is used.")]
 in
 Arg.alignx 80 spec
;;

let help = (comment,keywords,L.map Triple.drop_snd spec);;

(*** FUNCTIONS ****************************************************************)
let (>>=) = M.(>>=);;
let init _ = flags.help := false; flags.a := Rename;;

(* Destructors *)
let get_ip = Triple.fst;;
let get_op = Triple.thd;;

(* Processor *)
let approximate dps trs t =
 let growing = Approximation.growing in
 let rename = Approximation.rename <.> Trs.invert in
 let strong = Approximation.strong <.> Trs.invert in
 let det a = if Trs.is_linear dps then M.return a else A.det a in
 let fs = L.union (L.diff (Trs.funs dps) (Trs.roots dps)) (Trs.funs trs) in
 (match !(flags.a) with
  | Growing -> M.lift (pair Reach.predecessors) (growing trs)
  | Rename -> M.lift (pair Reach.successors) (rename trs)
  | Strong -> M.lift (pair Reach.successors) (strong trs)) >>= fun (f,trs) ->
 Initial.min dps trs t >>= f fs trs >>= det
;;

let is_arc ml mr (x,y) =
 let r = R.rhs x and l = R.lhs y in
 let a = Map.find r mr and b = Map.find l ml in
 let root = Option.the <.> Term.root <?> "left-hand side is a variable" in
 if root r = root l then
  A.is_accepted (T.of_term l) a >>= fun c -> 
  if c then A.is_accepted (T.of_term r) b else M.return false
 else M.return false
;;

let generate ml mr ns =
 M.foldl (fun g n -> M.foldl (fun ms m ->
  is_arc ml mr (n,m) >>= fun c -> M.return (if c then m::ms else ms)) [] ns >>=
  (M.return <.> flip (G.add n) g)) G.empty ns
;;

let filter_edges ml mr g =
 G.fold (fun n ms m -> m >>= fun g ->
  let add m ms c = M.return (if c then m::ms else ms) in
  M.foldl (fun ms m -> is_arc ml mr (n,m) >>= add m ms) [] ms >>=
  (M.return <.> flip (G.add n) g)) g (M.return G.empty)
;;

let compute p =
 let trs = P.get_trs p and dps = P.get_dps p and g = P.get_dg p in
 let (ls,rs) = match g with
  | P.Complete -> (Trs.lhs dps,Trs.rhs dps)
  | P.Partial g -> (L.map R.lhs (G.in_nodes g),L.map R.rhs (G.out_nodes g))
 in
 let ls = L.unique ~c:Map.Term.compare ls in
 let rs = L.unique ~c:Map.Term.compare rs in
 let itrs = Trs.invert trs in
 (* configurate computation *)
 let add trs m r = M.lift (flip (Map.add r) m) (approximate dps trs r) in
 M.foldl (add trs) Map.empty ls >>= fun ml ->
 M.foldl (add itrs) Map.empty rs >>= fun mr ->
 (* compute arcs *)
 match g with
  | P.Complete ->
   generate ml mr (Trs.to_list dps) >>= fun g -> M.get >>= fun s ->
   let i = Int.square (Trs.size dps) and j = G.size_edges g in
   let proof = {maps = (ml,mr); approximation = !(flags.a); status = snd s} in
   M.return (if j < i then Some (p,proof,P.set_dg (P.Partial g) p) else None)
  | P.Partial g ->
   let g = G.restrict (Trs.to_list dps) g in
   filter_edges ml mr g >>= fun h -> M.get >>= fun s ->
   let i = G.size_edges g and j = G.size_edges h in
   let proof = {maps = (ml,mr); approximation = !(flags.a); status = snd s} in
   M.return (if j < i then Some (p,proof,P.set_dg (P.Partial h) p) else None)
;;

let (>>=) = Monad.(>>=);;
let (>>) = Monad.(>>);;

let solve fs p =
 let configurate s = F.printf "%s@\n%!" s; flags.help := true in
 (try init (); Arg.parsex code spec fs with Arg.Bad s -> configurate s);
 if !(flags.help) then (Arg.usage spec ("Options for "^code^":"); exit 0);
 if P.for_all Trs.is_trs p then
  if P.is_dp p then
   Monad.get >>= fun s ->
   let (p,s) = Either.right (M.eval (s,S.init) (compute p)) in
   Monad.set (fst s) >> Monad.return p
  else Monad.return None
 else Monad.return None
;;

(* Complexity Bounds *)
let complexity c _ = C.mul c C.other;;

(* Compare Functions *)
let equal p q = P.equal (get_ip p) (get_ip q);;

(* Printers *)
let fprintf fs fmt p =
 F.fprintf fmt "@[<1>%s:@\n" name;
 P.fprintfm ~g:true fmt (get_op p) >>= fun _ ->
 F.fprintf fmt "@\n"; List.hd fs fmt >>= fun _ ->
 Monad.return (F.fprintf fmt "@]")
;;

let fprintfx fs fmt p =
 F.fprintf fmt "@{<dependencyGraph>";
 P.fprintfx ~g:true fmt (get_op p) >>= fun _ ->
 List.hd fs fmt >>= fun _ -> Monad.return (F.fprintf fmt "@}")
;;
