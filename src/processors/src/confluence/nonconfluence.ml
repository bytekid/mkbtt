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

 let rec transform s = function
  | T.Var _ -> s
  | T.State _ as t -> t
  | T.Fun (f,ts) -> T.Fun (f,List.map (transform s) ts)
 ;;

 let init f u =
  let cp = Path.fresh_reuse ~f:(const (const M.fresh_state)) in
  let fresh = const (M.lift List.singleton M.fresh_state) in
  Initial.specific fresh cp [transform (T.Fun (f,[])) (T.of_term u)]
 ;;
end

module Completion = struct
 (*** MODULES *****************************************************************)
 module P = Automata.Path;;
 module State = Automata.State;;

 (*** TYPES *******************************************************************)
 type t = {
  automaton : A.t;
  compatible : bool;
  history : A.t;
  term : Term.t;
  steps : int;
  violations : (T.t * State.t) list;
 };;
 
 (*** FUNCTIONS ***************************************************************)
 let (>>=) = M.(>>=);;
 
 (* Evaluation Data Type *)
 let init u a n = {
   automaton = a;
   compatible = false;
   history = A.create 0;
   term = u;
   steps = n;
   violations = [];
 };;

 let get_automaton x = x.automaton;;
 let get_compatible x = x.compatible;;
 let get_h x = x.history;;
 let get_term x = x.term;;
 let get_steps x = x.steps;;
 let get_vs x = x.violations;;
 let set_automaton a x = {x with automaton = a};;
 let set_compatible x = {x with compatible = true};;
 let set_h a x = {x with history = a};;
 let set_term r x = {x with term = r};;
 let set_steps n x = {x with steps = n};;
 let set_vs vs x = {x with violations = vs};;

 (* Execution Functions *)
 let rec evaluate f x =
  if get_compatible x || get_steps x = 0 then M.return x else f x
 ;;
 
 (* Configuration Functions *)
 let paths qd trs x =
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
 ;;
 
 let compute cp_trs qd trs x =
  paths qd trs x >>= fun (v,x) -> match v with
   | None -> M.return (set_compatible x)
   | Some (t,p) ->
    cp_trs t p (get_automaton x) >>= fun a ->
    M.return {x with automaton = a; steps = max ~-1 (get_steps x - 1)}
 ;;

 let arity trs =
  let max n f = M.find_ari f >>= (M.return <.> max n) in
  M.foldl max 1 (Trs.funs trs)
 ;;

 let plain trs =
  (* configuration *)
  arity trs >>= fun n -> 
  let cp_trs = P.tttbox ~p:true ~n:n P.fresh_max in
  let qd = not (Trs.is_left_linear trs) in
  (* computation *)
  let plain x =
   compute cp_trs qd trs x >>= fun x ->
   if get_compatible x then
    if Trs.is_left_linear trs then M.return x
    else M.lift (flip set_automaton x) (A.det (get_automaton x))
   else M.return x
  in
  M.return plain
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
type flags = {help : bool ref; steps : int ref};;
type proof = {map : A.t Map.t; cp : Term.t * Term.t; status : S.t};;
type t = P.t * proof * P.t;;

(*** GLOBALS ******************************************************************)
let code = "nonconfluence";;
let name = "nonconfluence Predicate";;
let keywords = ["confluence";"completion";"tree automata"];;

let flags = {help = ref false; steps = ref ~-1};;

let comment =
 "Checks if the given TRS admits a critical pair that is not joinable using \
  tree automata completion. If that is the case an empty problem is returned. \
  Otherwise the input problem remains unchanged."
;;

let spec =
 let spec = [
  ("--help",Arg.Set flags.help,"Prints information about flags.");
  ("-help",Arg.Set flags.help,"Prints information about flags.");
  ("-h",Arg.Set flags.help,"Prints information about flags.");
  ("-steps",Arg.Set_int flags.steps,
   "Specifies the maximum number of compatibility violations that should be \
    solved. This guarantees that the procedure always terminates. Otherwise \
    it might happen that non-confluence check does not terminate.")]
 in
 Arg.alignx 80 spec
;;

let help = (comment,keywords,List.map Triple.drop_snd spec);;

(*** FUNCTIONS ****************************************************************)
let (>>=) = M.(>>=);;
let (>>) = M.(>>);;
let init _ = flags.help := false; flags.steps := ~-1;;
let make_proof m cp s = {map = m; cp = cp; status = s};;

(* Destructors *)
let get_ip = Triple.fst;;
let get_proof = Triple.snd;;
let get_op = Triple.thd;;

let configuration trs =
 Completion.plain trs >>= fun f ->
 M.fresh_fun >>= fun g -> M.add_ari g 0 >>
 M.return (pair f (Initial.init g))
;;

let rec filter f m cps =
 let rec simplify xs = function
  | [] -> M.return (None,xs)
  | (s,t)::cps ->
   let find u = Completion.get_automaton (Map.find u m) in
   let compatible u = Completion.get_compatible (Map.find u m) in
   let stopped u = Completion.get_steps (Map.find u m) = 0 in
   if stopped s && stopped t then simplify xs cps else
    A.inter (find s) (find t) >>= A.is_empty >>= fun c ->
    if c then
     if compatible s && compatible t then
(* FIXME delete
Format.printf "@\nfirst:@\n";
A.fprintfm Format.std_formatter (find s) >>= fun _ ->
Format.printf "@\nsecond:@\n";
A.fprintfm Format.std_formatter (find t) >>= fun _ ->
Format.printf "@\n%!";
*)
      M.return (Some (s,t),[])
(* FIXME delete
)
*)
     else simplify ((s,t)::xs) cps
    else simplify xs cps
 in
 simplify [] cps >>= fun (cp,cps) -> match cp with
  | Some cp -> M.return (Some cp,m)
  | None ->
   if cps = [] then M.return (None,m) else
    let replace u m x = M.return (Map.add u x m) in
    let evaluate u x m = Completion.evaluate f x >>= replace u m in
    Map.fold (fun u x m -> m >>= evaluate u x) m (M.return m) >>=
    flip (filter f) cps
;;

let compute p =
 let trs = P.get_trs p in
 M.liftm (Rewrite.critical_pairs trs) >>= fun cps ->

(* FIXME delete
Format.printf "@\nCPS: %d@\n%!" (List.length cps);
M.fprintf (fun fmt (s,t) ->
M.liftm (Term.fprintfm fmt s) >>= fun _ ->
Format.fprintf fmt ", ";
M.liftm (Term.fprintfm fmt t) >>= fun _ ->
M.return (Format.fprintf fmt "%!"))
"\n%!" Format.std_formatter cps >>= fun _ ->
*)

 let us = List.foldl (fun us (s,t) -> s::t::us) [] cps in
 let us = List.unique ~c:Map.Term.compare us in
 (* configurate computation *)
 configuration trs >>= fun (f,c) ->
 let init u a = Completion.init u a !(flags.steps) in
 let add m u a = M.return (Map.add u (init u a) m) in
 M.foldl (fun m u -> c u >>= add m u) Map.empty us >>= fun m ->
 filter f m cps >>= fun (cp,m) -> M.get >>= fun s -> match cp with
  | None -> M.return None
  | Some cp ->
   let m = Map.map Completion.get_automaton m in
   let proof = make_proof m cp (snd s) in
   M.return (Some (p,proof,P.set_trs Trs.empty p))
;;

let (>>=) = Monad.(>>=);;
let (>>) = Monad.(>>);;

let solve fs p =
 let configurate s = F.printf "%s@\n%!" s; flags.help := true in
 (try init (); Arg.parsex code spec fs with Arg.Bad s -> configurate s);
 if !(flags.help) then (Arg.usage spec ("Options for "^code^":"); exit 0);
 if P.for_all Trs.is_trs p then
  if P.is_sp p then
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
 let proof = get_proof p in
 F.fprintf fmt "@[<1>%s:@\ncritical pair: " name;
 Term.fprintfm fmt (fst proof.cp) >>= fun _ ->
 F.fprintf fmt " = ";
 Term.fprintfm fmt (snd proof.cp) >>= fun _ ->
 F.fprintf fmt "@\n@[<1>problem:@\n";
 P.fprintfm fmt (get_op p) >>= fun _ ->
 F.fprintf fmt "@]@\n"; List.hd fs fmt >>= fun _ ->
 Monad.return (F.fprintf fmt "@]")
;;

let fprintfx fs fmt p =
 let proof = get_proof p in
 F.fprintf fmt "@{<confluence>@{<criticalpair>@{<fst>";
 Term.fprintfm fmt (fst proof.cp) >>= fun _ ->
 F.fprintf fmt "@}@{<snd>";
 Term.fprintfm fmt (snd proof.cp) >>= fun _ ->
 F.fprintf fmt "@}@}";
 P.fprintfx fmt (get_op p) >>= fun _ ->
 List.hd fs fmt >>= fun _ -> Monad.return (F.fprintf fmt "@}")
;;
