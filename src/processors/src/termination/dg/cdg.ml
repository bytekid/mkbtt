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
module M = Automata.Monad;;
module P = Problem;;
module Perv = Pervasives;;
module S = Automata.Status;;
module T = Automata.Term;;

module Initial = struct
 (*** MODULES *****************************************************************)
 module Initial = Automata.Initial;;
 module Lhs = Automata.Lhs;;
 module Path = Automata.Path;;

 (*** FUNCTIONS ***************************************************************)
 let (>>=) = M.(>>=);;
 let (>>) = M.(>>);;

 let min dps trs r =
  let cp = Path.fresh_reuse ~f:(const (const M.fresh_state)) in
  let fresh = const (M.lift List.singleton M.fresh_state) in
  let fs = List.diff (Trs.funs dps) (Trs.roots dps) in
  let fs = List.union fs (Trs.funs trs) in
  (if fs = [] then M.fresh_fun >>= fun c -> M.add_ari c 0 >> M.return [c]
  else M.return fs) >>= flip (Initial.instances fresh cp []) [T.of_term r]
 ;;

 let normal fs dps trs =
  let fs = List.union fs (List.diff (Trs.funs dps) (Trs.roots dps)) in
  let fs = List.union fs (Trs.funs trs) in
  (if fs = [] then M.fresh_fun >>= fun c -> M.add_ari c 0 >> M.return [c]
  else M.return fs) >>= flip Initial.normal trs >>= fun a ->
  let ps = A.finals a in
  let extend q a = M.foldl (fun a p -> A.update (Lhs.State p) q a) a ps in
  if List.length ps = 1 then let q = List.hd ps in M.return (A.add_final q a)
  else M.fresh_state >>= fun q -> extend q (A.add_final q a)
 ;;

 let rec transform s = function
  | T.Var _ -> s
  | T.State _ as t -> t
  | T.Fun (f,ts) -> T.Fun (f,List.map (transform s) ts)
 ;;

 let normal_instances fs dps trs r =
  if Term.is_ground r then
   M.fresh_state >>= fun p ->
   Path.fresh_max (T.of_term r) p (A.add_final p (A.create 1000))
  else
   normal fs dps trs >>= fun a ->
   let ps = A.finals a in
   if List.length ps = 0 then M.return (A.create 1000) else
    let r = transform (T.State (List.hd ps)) (T.of_term r) in
    M.fresh_state >>= fun p -> Path.fresh_max r p (A.set_finals [p] a)
 ;;

 let rfc f r =
  let cp = Path.fresh_reuse ~f:(const (const M.fresh_state)) in
  let fresh = const (M.lift List.singleton M.fresh_state) in
  Initial.specific fresh cp [transform (T.Fun (f,[])) (T.of_term r)]
 ;;
end

module Completion = struct
 (*** MODULES *****************************************************************)
 module P = Automata.Path;;
 module State = Automata.State;;

 (*** TYPES *******************************************************************)
 type t = {
  adapt : bool;
  automaton : A.t;
  compatible : bool;
  history : A.t * A.t;
  rhs : Term.t;
  steps : int;
  violations : (T.t * State.t) list * (T.t * State.t) list;
 };;
 
 (*** FUNCTIONS ***************************************************************)
 let (>>=) = M.(>>=);;
 
 (* Evaluation Data Type *)
 let init r a adapt n = {
   adapt = adapt;
   automaton = a;
   compatible = false;
   history = (A.create 0,A.create 0);
   rhs = r;
   steps = n;
   violations = ([],[]);
 };;

 let get_adapt x = x.adapt;;
 let get_automaton x = x.automaton;;
 let get_compatible x = x.compatible;;
 let get_h_trs x = fst x.history;;
 let get_h_sharp x = snd x.history;;
 let get_rhs x = x.rhs;;
 let get_steps x = x.steps;;
 let get_vs_trs x = fst x.violations;;
 let get_vs_sharp x = snd x.violations;;
 let set_adapt x = {x with adapt = true};;
 let set_automaton a x = {x with automaton = a};;
 let set_compatible x = {x with compatible = true};;
 let set_h_trs a x = {x with history = (a,get_h_sharp x)};;
 let set_h_sharp a x = {x with history = (get_h_trs x,a)};;
 let set_rhs r x = {x with rhs = r};;
 let set_steps n x = {x with steps = n};;
 let set_vs_trs vs x = {x with violations = (vs,get_vs_sharp x)};;
 let set_vs_sharp vs x = {x with violations = (get_vs_trs x,vs)};;
 let clear_adapt x = {x with adapt = false};;
 let clear_compatible x = {x with compatible = false};;

 (* Execution Functions *)
 let rec execute p f x =
  p x >>= fun c ->
  if c then M.return (true,x)
  else if get_compatible x then M.return (false,x)
  else if get_steps x = 0 then M.return (true,x)
  else f x >>= execute p f
 ;;
 
 (* Configuration Functions *)
 let paths cp_trs cp_sharp qd trs sharp x =
  let update get_h set_h get_vs set_vs qd trs x =
   let rec update = function
    | [] ->
     (if qd then A.quasi_det (get_automaton x) >>= A.reduce
     else M.return (get_automaton x)) >>= fun a ->
     let modify a = if qd then a else A.ec a in
     A.minus (modify (A.copy a)) (get_h x) >>= fun h ->
     A.compatible ~h:(Some h) trs a >>= fun vs ->
     let x = set_automaton a (set_h (A.copy a) x) in
     if vs = [] then M.return (None,set_vs vs x)
     else M.return (Some (List.hd vs),set_vs (List.tl vs) x)
    | (t,p)::vs ->
     A.is_reachable t p (get_automaton x) >>= fun c ->
     if c then update vs else M.return (Some (t,p),set_vs vs x)
   in
   update (get_vs x)
  in
  update get_h_trs set_h_trs get_vs_trs set_vs_trs qd trs x >>= fun (v,x) ->
  if Option.is_some v then M.return (cp_trs,v,x) else
   update get_h_sharp set_h_sharp get_vs_sharp set_vs_sharp false sharp x >>=
   (M.return <.> Triple.insert_fst cp_sharp)
 ;;
 
 let compute cp_trs cp_sharp qd trs sharp x =
  paths cp_trs cp_sharp qd trs sharp x >>= fun (f,v,x) -> match v with
   | None -> M.return (set_compatible x)
   | Some (t,p) ->
    f t p (get_automaton x) >>= fun a ->
    M.return {x with automaton = a; steps = max ~-1 (get_steps x - 1)}
 ;;

 let arity dps trs =
  let max n f = M.find_ari f >>= (M.return <.> max n) in
  M.foldl max 1 (Trs.funs dps) >>= flip (M.foldl max) (Trs.funs trs)
 ;;

 let adapt fs dps trs x =
  let fresh_var _ = M.lift Term.make_var M.fresh_var in
  let transform = function
   | Term.Var _ as t -> M.return t
   | Term.Fun (f,ts) -> M.lift (Term.make_fun f) (M.map fresh_var ts)
  in
  let a = get_automaton x in transform (get_rhs x) >>=
  Initial.normal_instances fs dps trs >>=
  (M.lift (flip set_automaton x) <.> A.inter a)
 ;;

 let plain i dps trs =
  (* configuration *)
  arity dps trs >>= fun n -> 
  let cp_trs = P.tttbox ~p:true ~n:n P.fresh_max in
  let cp_sharp = if n = 1 then P.suffix ~p:true P.fresh_max else cp_trs in
  let qd = not (Trs.is_left_linear trs) in
  (* computation *)
  let plain x =
   compute cp_trs cp_sharp qd trs Trs.empty x >>= fun x ->
   if get_compatible x then
    (if i then adapt [] dps trs x else M.return x) >>= fun x ->
    if Trs.is_left_linear trs && Trs.is_left_linear dps then M.return x
    else M.lift (flip set_automaton x) (A.det (get_automaton x))
   else M.return x
  in
  M.return plain
 ;;

 let rfc i dps trs dps' trs' (f,sharp) =
  (* configuration *)
  arity dps trs >>= fun n -> 
  let cp_trs = P.tttbox ~p:true ~n:n P.fresh_max in
  let cp_sharp = if n = 1 then P.suffix ~p:true P.fresh_max else cp_trs in
  let qd = not (Trs.is_left_linear trs) and trs' = Trs.union trs' dps' in
  (* computation *)
  let rec rfc x =
   (if i && not (get_adapt x) then compute cp_trs cp_sharp qd trs Trs.empty x
   else compute cp_trs cp_sharp false trs' sharp x) >>= fun x ->
   if get_compatible x then
    if i && get_adapt x then
     Initial.normal_instances [f] dps trs (get_rhs x) >>=
     A.inter (get_automaton x) >>= fun a ->
     rfc (clear_compatible (clear_adapt (set_automaton a x)))
    else
     (if i then adapt [f] dps trs x else M.return x) >>= fun x ->
     if Trs.is_left_linear trs && Trs.is_left_linear dps then M.return x
     else M.lift (flip set_automaton x) (A.det (get_automaton x))
   else M.return x
  in
  M.return rfc
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
type flags = {help : bool ref; rfc : bool ref; i : bool ref; steps : int ref};;

type proof = {
 map : A.t Map.t;
 forward_closures : bool;
 innermost : bool;
 status : S.t}
;;

type t = P.t * proof * P.t;;

(*** GLOBALS ******************************************************************)
let code = "cdg";;
let name = "CDG Processor";;
let keywords = ["dependency graph";"completion";"termination"];;

let flags = {
 help = ref false;
 rfc = ref false;
 i = ref false;
 steps = ref ~-1}
;;

let comment =
 "Remove all edges from the current DG that are not contained in the \
  CDG (approximation of the DG using tree automata completion)."
;;

let spec =
 let spec = [
  ("--help",Arg.Set flags.help,"Prints information about flags.");
  ("-help",Arg.Set flags.help,"Prints information about flags.");
  ("-h",Arg.Set flags.help,"Prints information about flags.");
  ("-i",Arg.Set flags.i,"Computes the innermost CDG and ICDG, if possible.");
  ("-rfc",Arg.Set flags.rfc,"Uses right-hand sides of forward colsures.");
  ("-steps",Arg.Set_int flags.steps,
   "Specifies the maximum number of compatibility violations that should be \
    solved. This guarantees that the procedure always terminates. Otherwise \
    it might happen that the graph approximation does not terminate.")]
 in
 Arg.alignx 80 spec
;;

let help = (comment,keywords,List.map Triple.drop_snd spec);;

(*** FUNCTIONS ****************************************************************)
let (>>=) = M.(>>=);;

let init _ =
 flags.help := false; flags.rfc := false; flags.i := false; flags.steps := ~-1
;;

let make_proof m rfc i s =
 {map = m; forward_closures = rfc; innermost = i; status = s}
;;

(* Destructors *)
let get_ip = Triple.fst;;
let get_op = Triple.thd;;

(* Processor *)
let configuration dps trs =
 if !(flags.rfc) && Trs.is_right_linear trs && Trs.is_right_linear dps then
  M.liftm (Trs.linear trs) >>= fun trs' ->
  M.liftm (Trs.linear dps) >>= fun dps' ->
  M.liftm (Trs.sharp (Trs.union dps' trs')) >>= fun (f,sharp) ->
  Completion.rfc !(flags.i) dps trs dps' trs' (f,sharp) >>= fun g ->
  M.return (g,Initial.rfc f,!(flags.i))
 else
  Completion.plain !(flags.i) dps trs >>= fun f ->
  let c = if !(flags.i) then Initial.normal_instances [] else Initial.min in
  M.return (f,c dps trs,false)
;;

let is_arc f m (x,y) =
 let r = Rule.rhs x and l = Rule.lhs y in
 let root = Option.the <.> Term.root <?> "left-hand side is a variable" in
 if root r = root l then
  let p = A.is_accepted (T.of_term l) <.> Completion.get_automaton in
  let q x = if Completion.get_compatible x then p x else M.return false in
  let p = if !(flags.i) then q else p in
  Completion.execute p f (Map.find r m) >>= fun (c,x) ->
  M.return (c,Map.add r x m)
 else M.return (false,m)
;;

let generate f map ns =
 M.foldl (fun (g,map) n -> M.foldl (fun (ms,map) m ->
  is_arc f map (n,m) >>= fun (c,map) ->
  M.return (if c then (m::ms,map) else (ms,map))) ([],map) ns >>=
  (M.return <.> Pair.apply (flip (G.add n) g) id)) (G.empty,map) ns
;;

let filter_edges f map g =
 G.fold (fun n ms m -> m >>= fun (g,map) ->
  let add m ms (c,map) = M.return (if c then (m::ms,map) else (ms,map)) in
  M.foldl (fun (ms,map) m -> is_arc f map (n,m) >>= add m ms) ([],map) ms >>=
  (M.return <.> Pair.apply (flip (G.add n) g) id)) g (M.return (G.empty,map))
;;

let compute p =
 let trs = P.get_trs p and dps = P.get_dps p and g = P.get_dg p in
 let rs = match g with
  | P.Complete -> Trs.rhs dps
  | P.Partial g -> List.map Rule.rhs (G.out_nodes g)
 in
 let rs = List.unique ~c:Map.Term.compare rs in
 flags.i := !(flags.i) && P.is_it p;
 (* configurate computation *)
 configuration dps trs >>= fun (f,c,adapt) ->
 let init r a = Completion.init r a adapt !(flags.steps) in
 let add m r a = M.return (Map.add r (init r a) m) in
 M.foldl (fun m r -> c r >>= add m r) Map.empty rs >>= fun m ->
 (* compute arcs *)
 match g with
  | P.Complete ->
   let ns = Trs.to_list dps in
   generate f m ns >>= fun (g,m) -> M.get >>= fun s ->
   let i = Int.square (Trs.size dps) and j = G.size_edges g in
   let m = Map.map Completion.get_automaton m in
   let proof = make_proof m !(flags.rfc) !(flags.i) (snd s) in
   M.return (if j < i then Some (p,proof,P.set_dg (P.Partial g) p) else None)
  | P.Partial g ->
   let g = G.restrict (Trs.to_list dps) g in
   filter_edges f m g >>= fun (h,m) -> M.get >>= fun s ->
   let i = G.size_edges g and j = G.size_edges h in
   let m = Map.map Completion.get_automaton m in
   let proof = make_proof m !(flags.rfc) !(flags.i) (snd s) in
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
