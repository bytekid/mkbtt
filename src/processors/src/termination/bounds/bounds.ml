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
module A = Automatax.Automaton;;
module C = Complexity;;
module E = Enrichment;;
module F = Format;;
module G = Graph;;
module I = Automatax.Initial;;
module L = Automatax.Lhs;;
module M = Automatax.Monad;;
module P = Problem;;
module S = Automatax.Status;;
module T = Automatax.Term;;

module Completion = struct
 (*** MODULES *****************************************************************)
 module Path = Automatax.Path;;
 module State = Automatax.State;;

 (*** TYPES *******************************************************************)
 type enrichment = Match | Roof | Top;;
 type catgory = E.t = Plain | DP | RT;;
 type algorithm = Explicit | Implicit;;
 type violations = (T.t * State.t) list;;

 type t = {
  automaton : A.t;
  compatible : bool;
  history : A.t * A.t * A.t;
  steps : int;
  strict : Trs.t;
  violations : violations * violations * violations;
 };;
 
 (*** FUNCTIONS ***************************************************************)
 let (>>=) = M.(>>=);;
 
 (* Evaluation Data Type *)
 let init s a n = {
  automaton = a;
  compatible = false;
  history = (A.create 0,A.create 0,A.create 0);
  steps = n;
  strict = s;
  violations = ([],[],[]);
 };;

 let get_automaton x = x.automaton;;
 let get_compatible x = x.compatible;;
 let get_h_strict x = Triple.fst x.history;;
 let get_h_weak x = Triple.snd x.history;;
 let get_h_sharp x = Triple.thd x.history;;
 let get_steps x = x.steps;;
 let get_strict x = x.strict;;
 let get_vs_strict x = Triple.fst x.violations;;
 let get_vs_weak x = Triple.snd x.violations;;
 let get_vs_sharp x = Triple.thd x.violations;;
 let set_automaton a x = {x with automaton = a};;
 let set_compatible x = {x with compatible = true};;
 let set_h_strict a x = {x with history = (a,get_h_weak x,get_h_sharp x)};;
 let set_h_weak a x = {x with history = (get_h_strict x,a,get_h_sharp x)};;
 let set_h_sharp a x = {x with history = (get_h_strict x,get_h_weak x,a)};;
 let set_steps n x = {x with steps = n};;
 let set_strict r x = {x with strict = r};;

 let set_vs_strict vs x =
  {x with violations = (vs,get_vs_weak x,get_vs_sharp x)}
 ;;

 let set_vs_weak vs x =
  {x with violations = (get_vs_strict x,vs,get_vs_sharp x)}
 ;;

 let set_vs_sharp vs x =
  {x with violations = (get_vs_strict x,get_vs_weak x,vs)}
 ;;

 let copy x = {
  automaton = A.copy (get_automaton x);
  compatible = get_compatible x;
  history = Triple.map A.copy x.history;
  steps = get_steps x;
  strict = get_strict x;
  violations = x.violations;
 };;

 let replicate n = List.replicate ~c:copy n;;
 let extend = List.extend ~c:copy;;

 (* Execution Functions *)
 let evaluate =
  let rec evaluate ys = function
   | [] -> if ys = [] then M.return None else M.return (Some (Right ys))
   | (f,x)::xs ->
    if x.compatible then M.return (Some (Left x))
    else if get_steps x = 0 then evaluate ys xs
    else f x >>= fun y -> evaluate ((f,y)::ys) xs
  in
  evaluate []
 ;;

 let iterate fs xs =
  let rec iterate xs =
   let analyse x = either (M.return <.> Option.some) iterate x in
   evaluate xs >>= Option.fold analyse (M.return None)
  in
  iterate (List.combine fs xs)
 ;;

 let execute fs gs xs =
  let rec combine fs gs xs = match (fs,gs,xs) with
   | [], [], [] -> []
   | fs'::fs, gs'::gs, xs'::xs ->
    (List.combine fs' xs',Some gs') :: combine fs gs xs
   | _, _, _ -> failwith "unequal lengths"
  in
  let rec execute ys = function
   | [] -> if ys = [] then M.return None else execute [] ys
   | (xs,gs)::zs -> evaluate xs >>= function
    | None -> execute ys zs
    | Some (Left x) ->
     Option.fold (fun gs ->
      let adapt x = init (get_strict x) (get_automaton x) (get_steps x) in
      let xs = replicate (List.length gs) (adapt x) in
      execute ((List.combine gs xs,None)::ys) zs) (M.return (Some x)) gs
    | Some (Right xs) -> execute ((xs,gs)::ys) zs
  in
  execute [] (combine fs gs xs)
 ;;
 
 (* Configuration Functions *)
 let paths cp_s cp_w cp_sharp e_s e_w e_sharp ht rc qd qc s w sharp x =
  let max n f = M.lift (max n) (M.liftm (Monad.get_height f)) in
  M.foldl max 0 (A.funs (get_automaton x)) >>= fun n ->
  let update get_h set_h get_vs set_vs e qd trs x =
   let rec update = function
    | [] ->
     (* configurate automaton *)
     (if qd then A.complete ~qc:qc ~rc:rc (get_automaton x) >>= A.reduce
     else M.return (get_automaton x)) >>= fun a ->
     (* configurate history *)
     let modify a = if qd then A.copy a else A.ec (A.copy a) in
     let minus a = A.minus (modify a) (get_h x) in
     (if ht then minus a else M.return (get_h x)) >>= fun h ->
     (* compute violations *)
     let compatible h = A.compatible ~c:true ~h:h ~t:(e n) trs a in
     (if ht then compatible (Some h) else compatible None) >>= fun vs ->
     let x = set_automaton a (set_h (if ht then A.copy a else h) x) in
     if vs = [] then M.return (None,set_vs vs x)
     else M.return (Some (List.hd vs),set_vs (List.tl vs) x)
    | (t,p)::vs ->
     A.is_reachable ~c:qc t p (get_automaton x) >>= fun c ->
     if c then update vs else M.return (Some (t,p),set_vs vs x)
   in
  update (get_vs x)
  in
  (* find violations wrt [s] *)
  let get_h_s = get_h_strict and set_h_s = set_h_strict in
  let get_vs_s = get_vs_strict and set_vs_s = set_vs_strict in
  update get_h_s set_h_s get_vs_s set_vs_s e_s qd s x >>= fun (v,x) ->
  if Option.is_some v then M.return (cp_s,v,x) else
   (* find violations wrt [w] *)
   let get_h_w = get_h_weak and set_h_w = set_h_weak in
   let get_vs_w = get_vs_weak and set_vs_w = set_vs_weak in
   update get_h_w set_h_w get_vs_w set_vs_w e_w false w x >>= fun (v,x) ->
   if Option.is_some v then M.return (cp_w,v,x) else
    (* find violations wrt [sharp] *)
    let get_h_s = get_h_sharp and set_h_s = set_h_sharp in
    let get_vs_s = get_vs_sharp and set_vs_s = set_vs_sharp in
    update get_h_s set_h_s get_vs_s set_vs_s e_sharp false sharp x >>=
    (M.return <.> Triple.insert_fst cp_sharp)
 ;;
 
 let compute cp_s cp_w cp_sharp e_s e_w e_sharp ht rc qc s w sharp x =
  let linear = Trs.is_left_linear s && Trs.is_left_linear w in
  let qd = not (linear && Trs.is_left_linear sharp) in
  let paths = paths cp_s cp_w cp_sharp e_s e_w e_sharp ht rc in
  paths qd qc s w sharp x >>= fun (f,v,x) -> match v with
   | None -> M.return (set_compatible x)
   | Some (t,p) ->
    f t p (get_automaton x) >>= fun a ->
    M.return {x with automaton = a; steps = max ~-1 (get_steps x - 1)}
 ;;

 let enrichment dp rt e =
  let t = if dp then DP else if rt then RT else Plain in
  match e with
   | Match -> (E.all ~t:t ~w:false,E.all ~t:t ~w:true)
   | Roof -> (E.roof ~t:t ~w:false,E.roof ~t:t ~w:true)
   | Top -> (E.top ~t:t ~w:false,E.top ~t:t ~w:true)
 ;;

 let raise_cons = function Explicit -> A.explicit | Implicit -> A.implicit;;

 let arity =
  let max n f = M.find_ari f >>= (M.return <.> max n) in
  M.foldl max 0 <.> Trs.funs
 ;;

 let paths_strict m dp rt qc s =
  let paths_strict n =
   if n > 1 then
    if Trs.is_left_linear s then
     if m = 1 then
      [Path.tttbox ~p:true ~n:1 ~c:qc Path.fresh_funs;
       Path.tttbox ~p:true ~n:n ~c:qc Path.fresh_funs]
     else [Path.tttbox ~p:true ~n:n ~c:qc Path.fresh_funs]
    else if m = 1 then
     [Path.tttbox ~p:true ~n:1 ~c:qc (Path.fresh_reuse ~c:true);
      Path.tttbox ~p:true ~n:n ~c:qc Path.fresh_funs]
    else if rt then [Path.tttbox ~p:true ~n:n ~c:qc Path.fresh_funs]
    else [Path.tttbox ~p:true ~n:n ~c:qc (Path.fresh_reuse ~c:true)]
   else if m = 1 then
    [Path.tttbox ~c:qc ~n:n (Path.fresh_max ~l:true);
     Path.tttbox ~c:qc ~n:n (Path.fresh_max ~l:true)]
   else [Path.tttbox ~c:qc ~n:n (Path.fresh_max ~l:true)]
  in
  M.lift paths_strict (arity s)
 ;;

 let paths_weak m dp rt qc w =
  let paths_weak n =
   if n > 1 then
    if Trs.is_left_linear w then
     if m = 1 then
      [Path.tttbox ~p:true ~n:1 ~c:qc Path.fresh_funs;
       Path.tttbox ~p:true ~n:n ~c:qc Path.fresh_funs]
     else [Path.tttbox ~p:true ~n:n ~c:qc Path.fresh_funs]
    else if m = 1 then
     [Path.tttbox ~p:true ~n:1 ~c:qc (Path.fresh_reuse ~c:true);
      Path.tttbox ~p:true ~n:n ~c:qc Path.fresh_funs]
    else if rt then [Path.tttbox ~p:true ~n:n ~c:qc Path.fresh_funs]
    else [Path.tttbox ~p:true ~n:n ~c:qc (Path.fresh_reuse ~c:true)]
   else if m = 1 then
    [Path.tttbox ~c:qc ~n:n (Path.fresh_max ~l:true);
     Path.tttbox ~c:qc ~n:n (Path.fresh_max ~l:true)]
   else [Path.tttbox ~c:qc ~n:n (Path.fresh_max ~l:true)]
  in
  M.lift paths_weak (arity w)
 ;;
 
 let paths_sharp m dp rt qc sharp =
  let paths_sharp n =
   if n > 1 then
    if m = 1 then
     [Path.tttbox ~c:qc ~n:1 Path.fresh_funs;
      Path.tttbox ~c:qc ~n:n Path.fresh_funs]
    else [Path.tttbox ~c:qc ~n:n Path.fresh_funs]
   else if m = 1 then
    [Path.suffix ~c:qc (Path.fresh_max ~l:true);
     Path.suffix ~c:qc (Path.fresh_max ~l:true)]
   else [Path.suffix ~c:qc (Path.fresh_max ~l:true)]
  in
  M.lift paths_sharp (arity sharp)
 ;;

 let paths n dp rt qc s w sharp =
  let rec combine fs gs hs = match (fs,gs,hs) with
   | [], [], [] -> []
   | f::fs, g::gs, h::hs -> (f,g,h) :: combine fs gs hs
   | _, _, _ -> failwith "unequal lengths"
  in
  paths_strict n dp rt qc s >>= fun fs -> paths_weak n dp rt qc w >>= fun gs ->
  paths_sharp n dp rt qc sharp >>= fun hs -> M.return (combine fs gs hs)
 ;;

 let rec group = function
  | [] -> []
  | x::xs ->
   let equal x y = Trs.equal (get_strict x) (get_strict y) in
   let (ys,xs) = List.partition (equal x) xs in (x::ys) :: group xs
 ;;

 let generate e dp rt ht rc qc s w sharp xs =
  let (e_s,e_w) = enrichment dp rt e and e_z = const E.zero in
  let rc = raise_cons rc and xs = group xs in
  let compute s w (f,g,h) = compute f g h e_s e_w e_z ht rc qc s w sharp in
  let paths s w = paths (List.length xs) dp rt qc s w sharp in
  let generate s w = M.lift (List.map (compute s w)) (paths s w) in
  M.foldr (fun ys (fs,xs) ->
   let s' = get_strict (List.hd ys) in
   generate s' (Trs.union (Trs.diff s s') w) >>= fun gs ->
   let n = List.length gs and m = List.length ys in
   if n >= m then M.return (gs@fs,(extend n ys) @ xs)
   else M.return ((List.extend m gs) @ fs,ys@xs)) ([],[]) xs
 ;;

 let plain e dp rt ht rc qc s w xs =
  generate e dp rt ht rc qc s w Trs.empty xs >>= uncurry iterate
 ;;

 let combined e dp rt ht rc qc s w sharp xs =
  generate e dp rt ht rc qc s w sharp xs >>= uncurry iterate
 ;;

 let generate e dp rt ht rc qc s s' w w' sharp xs =
  let (e_s,e_w) = enrichment dp rt e and e_z = const E.zero in
  let rc = raise_cons rc and xs = group xs and trs = Trs.empty in
  let paths s w = paths (List.length xs) dp rt qc s w sharp in
  let calculate s w (f,g,h) = compute f g h e_s e_w e_z ht rc qc s w trs in
  let generate s w = M.lift (List.map (calculate s w)) (paths s w) in
  let compute (f,g,h) = compute f g h e_z e_z e_z ht rc qc s' w' sharp in
  M.lift (List.map compute) (paths s' w') >>= fun fs' ->
  let n = List.length fs' in
  M.foldr (fun ys (fs,gs,xs) ->
   let s' = get_strict (List.hd ys) in
   let w = Trs.union (Trs.diff s s') w in
   generate s' w >>= fun gs' ->
   let m = List.length ys in
   if n >= m then M.return (fs'::fs,gs'::gs,(extend n ys) :: xs)
   else M.return ((List.extend m fs') :: fs,(List.extend m gs') :: fs,ys::xs))
   ([],[],[]) xs
 ;;

 let splitted e dp rt ht rc qc s s' w w' sharp xs =
  generate e dp rt ht rc qc s s' w w' sharp xs >>= uncurry3 execute
 ;;
end

(*** TYPES ********************************************************************)
type algorithm = Completion.algorithm = Explicit | Implicit;;
type enrichment = Completion.enrichment = Match | Roof | Top;;
type language = All | Constructor | Default;;

type flags = {
 all : bool ref;
 cp : bool ref;
 dp : bool ref;
 e : enrichment ref;
 help : bool ref;
 l : language ref;
 qc : bool ref;
 rc : algorithm ref;
 rfc : bool ref;
 rt : bool ref;
 steps : int ref;
};;

type proof = {
 automaton : A.t;
 complexity : bool;
 enrichment : enrichment;
 dependency_pair : bool;
 forward_closures : bool;
 language : language;
 quasi_compatible : bool;
 relative : bool;
 status : S.t}
;;

type t = P.t * proof * P.t;;

(*** GLOBALS ******************************************************************)
let code = "bounds";;
let name = "Bounds Processor";;
let keywords = ["match-bounds";"automata";"termination"];;

let flags = {
 all = ref false;
 cp = ref false;
 dp = ref false;
 e = ref Match;
 help = ref false;
 l = ref Default;
 qc = ref false;
 rc = ref Explicit;
 rfc = ref false;
 rt = ref false;
 steps = ref ~-1;
};;

let comment =
 "This processor proves termination of a given problem by using the \
 match-bound technique."
;;

let spec =
 let set_language = function
  | "all" -> flags.l := All
  | "constructor" -> flags.l := Constructor
  | "default" -> flags.l := Default
  | s -> raise (Arg.Bad (Format.sprintf "unknown language `%s'." s))
 in
 let set_enrichment = function
  | "match" -> flags.e := Match
  | "roof" -> flags.e := Roof
  | "top" -> flags.e := Top
  | s -> raise (Arg.Bad (Format.sprintf "unknown enrichment `%s'." s))
 in
 let set_rc = function
  | "explicit" -> flags.rc := Explicit
  | "implicit" -> flags.rc := Implicit
  | s -> raise (Arg.Bad (Format.sprintf "unknown raise algorithm `%s'." s))
 in
 let spec = [
  ("-all",Arg.Set flags.all,
   "This flage is only effective if a DP, CP, or relative termination \
    problem is given. In that case, all rewrite rules are proved to be \
    finite (relative terminating) instead of a single rewrite rule.");
  ("-cp",Arg.Set flags.cp,
   "Enables the complexity mode in case that match-bounds are used as a \
    CP processor. Instead of the enrichment `match' the TRS `match-RT' is \
    used if a CP problem is given. Note that match-RT-bounds work currently \
    only for non-duplicating systems.");
  ("-dp",Arg.Set flags.dp,
   "Uses the enrichments `match-DP' and `top-DP' instead of `match' and \
    `top' if a DP problem is given. Make shure that as enrichment either \
    `top' or `match' has been chosen because soundness of `roof-DP' is \
    unknown.");
  ("-e",Arg.String set_enrichment,
   "Specifies the enrichment used to show termination. Possible values \
    are `match', `roof', and `top'. In case that `match' has been chosen, \
    `match' is used for non-duplicating TRSs and `roof' for duplicating \
    TRSs. In case that the flag `-dp' has been specified `top' is used \
    insted of `roof' because soundness of `roof' is unknown. Per default \
    `match' is used.");
  ("--help",Arg.Set flags.help,"Prints information about flags.");
  ("-help",Arg.Set flags.help,"Prints information about flags.");
  ("-h",Arg.Set flags.help,"Prints information about flags.");
  ("-l",Arg.String set_language,
   "Defines the starting language. Possible values are `default' which uses \
    the language that fits the one of the problem description, `constructor' \
    for the set of all ground constructor terms and `all' for all ground \
    terms. Per default `default' is used.");
  ("-qc",Arg.Set flags.qc,
   "Computes quasi-compatible tree automata instead of compatible tree
    automata.");
  ("-rc",Arg.String set_rc,
   "Defines the algorithm that is used to construct raise-consistent tree \
    automata. Possible values are `explicit' and `implicit' where. Per \
    default `implicit' is used.");
  ("-rfc",Arg.Set flags.rfc,"Uses right-hand sides of forward colsures.");
  ("-rt",Arg.Set flags.rt,
   "Uses the enrichments `match-RT' instead of `match' if a relative \
    termination problem is given. Note that match-RT-bounds work currently \
    only for non-duplicating systems.");
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
 flags.all := false;
 flags.cp := false;
 flags.dp := false;
 flags.e := Match;
 flags.help := false;
 flags.l := Default;
 flags.qc := false;
 flags.rc := Implicit;
 flags.rfc := false;
 flags.rt := false;
 flags.steps := ~-1;
;;

let make_proof a e s = {
 automaton = a;
 complexity = !(flags.cp);
 enrichment = e;
 dependency_pair = !(flags.dp);
 forward_closures = !(flags.rfc);
 language = !(flags.l);
 quasi_compatible = !(flags.qc);
 relative = !(flags.rt);
 status = s;
};;

(* Destructors *)
let get_ip = Triple.fst;;
let get_proof = Triple.snd;;
let get_op = Triple.thd;;

(* Processor *)
let enrichment dp rt s w = function
 | Match ->
  let e = if dp then Top else Roof in
  if Trs.is_duplicating s || Trs.is_duplicating w then e else Match
 | e -> e
;;

let language p = match P.get_language p with
 | P.All -> All
 | P.Constructor -> Constructor
;;

let plain e dp rt all ht s w =
 let init s a = Completion.init s a !(flags.steps) in
 let plain = Completion.plain and adapt r s = Trs.union (Trs.remove r s) in
 let instances r s = I.instances (Trs.singleton r) <.> adapt r s in
 if all && !(flags.l) = Constructor then
  I.constructor s w >>= fun a ->
  let xs = Completion.replicate 2 (init s a) in
  plain e dp rt ht !(flags.rc) !(flags.qc) s w xs
 else if all then
  I.split s w >>= fun a -> I.individual s w >>= fun b ->
  plain e dp rt ht !(flags.rc) !(flags.qc) s w [init s a;init s b]
 else if !(flags.l) = All then
  let rs = Trs.to_list s in
  if !(flags.dp) then
   let init r = M.lift (init (Trs.singleton r)) (instances r s w) in
   M.map init rs >>= plain e dp rt ht !(flags.rc) !(flags.qc) s w
  else
   I.split s w >>= fun a -> I.individual s w >>= fun b ->
   let init r = init (Trs.singleton r) <.> A.copy in
   let variants r = [init r a;init r b] in
   plain e dp rt ht !(flags.rc) !(flags.qc) s w (List.flat_map variants rs)
 else
  let rs = Trs.to_list s in I.constructor s w >>= fun a ->
  let init r = init (Trs.singleton r) (A.copy a) in
  plain e dp rt ht !(flags.rc) !(flags.qc) s w (List.map init rs)
;;

let rfc e dp rt all ht s w =
 M.liftm (Trs.linear s) >>= fun s' ->
 M.liftm (Trs.linear w) >>= fun w' ->
 M.liftm (Trs.sharp (Trs.union s' w')) >>= fun (f,sharp) ->
 let init s a = Completion.init s a !(flags.steps) in
 let combined = Completion.combined e dp rt ht !(flags.rc) !(flags.qc) in
 let splitted = Completion.splitted e dp rt ht !(flags.rc) !(flags.qc) in
 let create s = M.lift (init s) (I.specific f s) in
 (if all then M.lift List.singleton (create s)
 else M.map (create <.> Trs.singleton) (Trs.to_list s)) >>= fun xs ->
 let is_ll = Trs.is_left_linear and is_dummy = Trs.is_dummy in
 if (is_ll s && is_ll w && not all) || (all && is_dummy s && is_dummy w) then
  combined s w sharp xs
 else splitted s s' w w' sharp xs
;;

let strategy e dp rt all ht s w =
 let rl = Trs.is_right_linear s && Trs.is_right_linear w in
 (if !(flags.rfc) && rl then rfc else plain) e dp rt all ht s w >>= function
  | None -> M.return None
  | Some x ->
   M.get >>= fun (_,status) ->
   let proof = make_proof (Completion.get_automaton x) e status in
   M.return (Some (Trs.diff s (Completion.get_strict x),proof))
;;

let plain p =
 let s = P.get_trs p and w = Trs.empty in
 let e = enrichment false false s w !(flags.e) in
 strategy e false false true true s w >>= function
  | None -> M.return None
  | Some (s,proof) -> M.return (Some (p,proof,P.set_trs s p))
;;

let dp p =
 let s = P.get_dps p and w = P.get_trs p in
 let e dp = enrichment dp false s w !(flags.e) in
 let strategy e dp = strategy e dp false in
 (if !(flags.dp) then strategy (e true) true !(flags.all) true s w
 else strategy (e false) false true true (Trs.union s w) Trs.empty) >>= function
  | None -> M.return None
  | Some (s',proof) ->
   M.return (Some (p,proof,P.set_dps (Trs.intersect s' s) p))
;;

let rp p =
 let s = P.get_strict p and w = P.get_weak p in
 let e rt = enrichment false rt s w !(flags.e) in
 let strategy e = strategy e false in
 if !(flags.rt) then
  let u = Trs.filter Rule.is_collapsing s in
  let w' = Trs.union w u and s' = Trs.diff s u in
  (if Trs.is_empty s' && not (Trs.is_empty u) then M.return None
  else strategy (e true) true !(flags.all) false s' w') >>= function
   | None -> M.return None
   | Some (s',proof) ->
    let s = Trs.intersect (Trs.union s' u) s in
    M.return (Some (p,proof,P.set_sw s w p))
 else strategy (e false) false true true (Trs.union s w) Trs.empty >>= function
  | None -> M.return None
  | Some (s',proof) ->
   M.return (Some (p,proof,P.set_sw (Trs.intersect s' s) w p))
;;

let cp p =
 let s = P.get_strict p and w = P.get_weak p in
 let e rt = enrichment false rt s w !(flags.e) in
 let strategy e = strategy e false in
 if !(flags.cp) then
  let u = Trs.filter Rule.is_collapsing s in
  let w' = Trs.union w u and s' = Trs.diff s u in
  (if Trs.is_empty s' && not (Trs.is_empty u) then M.return None
  else strategy (e true) true !(flags.all) false s' w') >>= function
   | None -> M.return None
   | Some (s',proof) ->
    let s' = Trs.union s' u in
    let w = Trs.union (Trs.diff s s') w and s = Trs.intersect s' s in
    M.return (Some (p,proof,P.set_sw s w p))
 else strategy (e false) false true true (Trs.union s w) Trs.empty >>= function
  | None -> M.return None
  | Some (s',proof) ->
   let w = Trs.union (Trs.diff s s') w and s = Trs.intersect s' s in
   M.return (Some (p,proof,P.set_sw s w p))
;;

let (>>=) = Monad.(>>=);;
let (>>) = Monad.(>>);;

let solve fs p =
 let configurate s = F.printf "%s@\n%!" s; flags.help := true in
 (try init (); Arg.parsex code spec fs with Arg.Bad s -> configurate s);
 if !(flags.help) then (Arg.usage spec ("Options for "^code^":"); exit 0);
 if !(flags.l) = Default then flags.l := language p;
 let eval f s = Pair.apply id fst (Either.right (M.eval (s,S.init) (f p))) in
 if P.for_all Trs.is_trs p then
  if P.is_sp p then Monad.adopt (eval plain)
  else if P.is_dp p then Monad.adopt (eval dp)
  else if P.is_rp p then Monad.adopt (eval rp)
  else if P.is_cp p then Monad.adopt (eval cp)
  else Monad.return None
 else Monad.return None
;;

(* Complexity Bounds *)
let complexity c p =
 let proof = get_proof p and ip = get_ip p and op = get_op p in
 let l = proof.language = All || proof.language = language ip in
 let dp = proof.dependency_pair and cp = proof.complexity in
 let rfc = proof.forward_closures in
 let (s,w) = P.get_sw ip and empty = P.is_empty op in
 let dup = Trs.is_duplicating s || Trs.is_duplicating w in
 if P.is_sp op then
  if not (rfc || dp || dup) && empty && l then C.mul c C.linear
  else C.mul c C.other
 else if P.is_cp op then
  if not (rfc || dp || dup) && (cp || empty) && l then C.add c C.linear
  else C.add c C.other
 else C.add c C.other
;;

(* Compare Functions *)
let equal p q = P.equal (get_ip p) (get_ip q);;

(* Printers *)
let (>>=) = M.(>>=);;

let fprintf_enrichment dp rt fmt e =
 let e = match e with
  | Match -> "match"
  | Roof -> "roof"
  | Top -> "top"
 in
 let suffix = if dp then "-dp" else if rt then "-rt" else "" in
 F.fprintf fmt "%s%s" e suffix
;;

let fprintf fs fmt p =
 let fprintf fs fmt p =
  let proof = get_proof p in
  A.bound proof.automaton >>= fun bound ->
  F.fprintf fmt "@[<1>%s:@\nbound: %i" name bound;
  let e = proof.enrichment in
  let dp = proof.dependency_pair and rt = proof.relative in
  F.fprintf fmt "@\nenrichment: %a" (fprintf_enrichment dp rt) e;
  F.fprintf fmt "@\n@[<1>automaton:@\n";
  A.fprintfm fmt proof.automaton >>= fun _ ->
  F.fprintf fmt "@]@\n@[<1>problem:@\n";
  M.liftm (P.fprintfm fmt (get_op p)) >>= fun _ ->
  F.fprintf fmt "@]@\n"; M.liftm (List.hd fs fmt) >>= fun _ ->
  M.return (F.fprintf fmt "@]")
 in
 let status = (get_proof p).status in
 let eval s = Either.right (M.eval (s,status) (fprintf fs fmt p)) in
 Monad.update (fst <.> snd <.> eval)
;;

let fprintfx_enrichment dp rt fmt e =
 let e = match e with
  | Match -> "match"
  | Roof -> "roof"
  | Top -> "top"
 in
 let suffix = if dp then "-dp" else if rt then "-rt" else "" in
 F.fprintf fmt "@{<enrichment>%s%s@}" e suffix
;;

let fprintfx fs fmt p =
 let fprintfx fs fmt p =
  let proof = get_proof p in
  A.bound proof.automaton >>= fun bound ->
  F.fprintf fmt "@{<bounds>@{<bound>%i@}" bound;
  let dp = proof.dependency_pair and rt = proof.relative in
  fprintfx_enrichment dp rt fmt proof.enrichment;
  A.fprintfx fmt proof.automaton >>= fun _ ->
  M.liftm (P.fprintfx fmt (get_op p)) >>= fun _ ->
  M.liftm (List.hd fs fmt) >>= fun _ -> M.return (F.fprintf fmt "@}")
 in
 let status = (get_proof p).status in
 let eval s = Either.right (M.eval (s,status) (fprintfx fs fmt p)) in
 Monad.update (fst <.> snd <.> eval)
;;
