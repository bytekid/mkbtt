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
open Prelude;;

(*** MODULES ******************************************************************)
module Make (R : REWRITING)
            (C : CATEGORIZATION with type 'a m = 'a R.Monad.t) = struct
 (*** MODULES *****************************************************************)
 module D = Dependency;;
 module F = Format;;
 module Fun = R.Function;;
 module H = Util.Hashtbl;;
 module Lhs = Lhs.Make (R);;
 module M = Monad.Make (R);;
 module P = Parser.Make (R);;
 module Pos = Rewriting.Position;;
 module Rep = Util.Map.Partial (State);;
 module Rule = R.Rule;;
 module S = Substitution.Make (R);;
 module T = H.Make (Lhs) (Rhs);;
 module Term = Term.Make (R);;
 module Trs = R.Trs;;
 module Var = R.Variable;;
 module I = H.Make (Fun) (T);;

 module Ass = Util.Isomorphism.Make (struct
  type t = State.t * State.t;;
  
  let compare = compare;;
  let copy = Util.id;;
  let hash = H.hash;;

  let fprintf fmt (p,q) =
   F.fprintf fmt "@[(%a,%a)@]" State.fprintf p State.fprintf q
  ;;
 end) (State);;

 module C = struct
  include C;;

  let add f c = M.liftm (add f c);;
  let remove f c = M.liftm (remove f c);;
  let find f c = M.liftm (find f c);;
 end

 module Path = Util.Map.Partial (struct
  type t = Term.t;;

  let compare = compare;;
  let fprintf fmt t = F.fprintf fmt "@[%a@]" Term.fprintf t;;
 end);;

 (*** OPENS *******************************************************************)
 open Util;;

 (*** TYPES *******************************************************************)
 type 'a m = 'a M.t;;
 type 'a p = 'a P.t;;
 type lhs = Lhs.t = State of State.t | Fun of Fun.t * State.t list;;
 type rhs = Rhs.t;;
 type substitution = S.t;;
 type term = Term.t;;
 type trs = Trs.t;;

 type t = {
  finals : State.t list;
  dependency : D.t;
  categorization : C.t;
  epsilons : T.t;
  transitions : I.t;
  mutable quasi_det : bool;
 };;
 
 (*** FUNCTIONS ***************************************************************)
 let (>>=) = M.(>>=);;

 (* Search Functions *)
 let find ?(c = false) l a = match l with
  | State _ -> M.return (try Some (T.find l a.epsilons) with Not_found -> None)
  | Fun (f,_) ->
   let rec find = function
    | [] -> None
    | f::fs ->
     try Some (T.find l (I.find f a.transitions)) with Not_found -> find fs
   in
   M.lift find (if c then C.find f a.categorization else M.return [f])
 ;;

 (* Scan Functions *)
 let is_final p a = List.mem p a.finals;;

 let mem ?(c = false) l =
  M.lift (Option.fold (const true) false) <.> find ~c:c l
 ;;

 let for_all_trans p a = I.for_all (const (T.for_all p)) a.transitions;;

 let for_allc_trans c p a =
  C.find c a.categorization >>= fun fs ->
  M.return (List.for_all (fun g -> T.for_all p (I.find g a.transitions)) fs)
 ;;

 let for_allf_trans f p a =
  try T.for_all p (I.find f a.transitions) with Not_found -> true
 ;;

 let exists_trans p a = I.exists (const (T.exists p)) a.transitions;;

 let existsc_trans c p =
  M.lift not <.> for_allc_trans c (fun l -> not <.> p l)
 ;;

 let existsf_trans f p = not <.> for_allf_trans f (fun l -> not <.> p l);;
 let for_all_eps p a = T.for_all p a.epsilons;;
 let exists_eps p a = T.exists p a.epsilons;;
 let for_all p a = for_all_trans p a && for_all_eps p a;;
 let exists p a = exists_trans p a || exists_eps p a;;

 (* Constructors and Destructors *)
 let add_final p a =
  if List.mem p a.finals then a else {a with finals = p :: a.finals}
 ;;

 let remove_final p a = {a with finals = List.remove_all p a.finals};;
 let set_finals ps a = {a with finals = List.unique_hash ps};;
 let set_quasi_det a = {a with quasi_det = true};;
 let clear_quasi_det a = {a with quasi_det = false};;

 let add_agent p qs a =
  let a = if List.exists (flip is_final a) qs then add_final p a else a in
  {a with dependency = D.add p qs a.dependency}
 ;;
 
 let replace_agent p qs a =
  {a with dependency = D.replace p qs a.dependency}
 ;;

 let create n = {
  finals = [];
  dependency = D.empty n;
  categorization = C.create n;
  epsilons = T.create n;
  transitions = I.create (3 * n);
  quasi_det = true;
 };;

 let modify modify l r a = match l with
  | State p ->
   let r = Rhs.remove p r in
   let modify r = {a with epsilons = modify l r a.epsilons} in
   M.return (Option.fold (clear_quasi_det <.> modify) a r)
  | Fun (f,ps) ->
   C.add f a.categorization >>= fun c ->
   let t = try I.find f a.transitions with Not_found -> T.create 1000 in
   let t = I.replace f (modify l r t) a.transitions in
   M.return {a with categorization = c; transitions = t; quasi_det = false}
 ;;

 let add = modify T.add;;
 let replace = modify T.replace;;

 let update l p a =
  find l a >>= fun r ->
  replace l (Option.fold (Rhs.add p) (Rhs.singleton p) r) a
 ;;

 let extend l ps a =
  find l a >>= fun r ->
  replace l (Option.fold (Rhs.extend ps) (Rhs.create ps) r) a
 ;;

 let remove l a = match l with
  | State _ ->
   M.return {a with epsilons = T.remove l a.epsilons; quasi_det = false}
  | Fun (f,ps) ->
   try
    let t = T.remove l (I.find f a.transitions) in
    (if T.is_empty t then
     M.lift (flip pair (I.remove f a.transitions)) (C.remove f a.categorization)
    else
     M.return (a.categorization,I.replace f t a.transitions)) >>= fun (c,t) ->
    M.return {a with categorization = c; transitions = t; quasi_det = false}
   with Not_found -> M.return a
 ;;

 (* Parsers *)
 let (>>=) = P.(>>=);;
 let (>>) = P.(>>);;
 let (<?>) = P.(<?>);;
 let (<|>) = P.(<|>);;

 let parse ps =
  let id = (P.many1 (P.noneof " \t\n\r()\",") >>= function
   | ['-';'>'] | ['=';'='] | ['-';'>';'='] | ['-';'>';'<';'-'] -> P.fail
   | i -> P.return i) <?> "identifier"
  in
  let create_state n =
   P.get_state >>= fun (s,status) ->
   try
    let (p,status) = Status.create_state (int_of_string n) status in
    P.set_state (s,status) >> P.return p
   with Failure _ -> P.fail
  in
  let parse_transition =
   Lhs.parse ps >>= fun l -> P.lex (P.string "->") >>
   P.lex id >>= (create_state <.> String.of_char_list) >>=
   (P.return <.> pair l)
  in
  P.many parse_transition >>= fun ts -> P.get_state >>= fun s ->
  let update a (l,p) = update l p a in
  let (x,s) = Either.right (M.eval s (M.foldl update (create 1000) ts)) in
  P.set_state s >> P.return x
 ;;

 let of_string ps input =
  M.(>>=) M.get (fun s ->
  let m = parse ps >>= fun a -> P.get_state >>= (P.return <.> Pair.make a) in
  match P.run m s (Parsec.StringInput.of_string input) with
   | Left e -> M.fail (Parsec.Error.to_string e)
   | Right (a,s) -> M.(>>) (M.set s) (M.return a))
 ;;
 
 let (>>=) = M.(>>=);;

 (* Iterators *)
 let fold_trans f a = I.fold (const (T.fold f)) a.transitions;;

 let foldf_trans g f a d =
  try T.fold f (I.find g a.transitions) d with Not_found -> d
 ;;

 let foldc_trans c f a d =
  C.find c a.categorization >>= fun fs ->
  M.return (List.foldl (fun x g -> foldf_trans g f a x) d fs)
 ;;

 let map_trans f a =
  {a with transitions = I.map (T.map f) a.transitions; quasi_det = false}
 ;;

 let mapf_trans g f a =
  try
   let t = T.map f (I.find g a.transitions) in
   {a with transitions = I.replace g t a.transitions; quasi_det = false}
  with Not_found -> a
 ;;

 let mapc_trans c f a =
  C.find c a.categorization >>= fun fs ->
  M.return (List.foldl (fun a g -> mapf_trans g f a) a fs)
 ;;

 let iter_trans f a = fold_trans (drop <.> f) a ();;
 let iterc_trans c f a = foldc_trans c (drop <.> f) a ();;
 let iterf_trans g f a = foldf_trans g (drop <.> f) a ();;
 let fold_eps f a = T.fold f a.epsilons;;
 let map_eps f a = {a with epsilons = T.map f a.epsilons; quasi_det = false};;
 let iter_eps f a = fold_eps (drop <.> f) a ();;
 let fold f a = fold_eps f a <.> fold_trans f a;;
 let map f = map_eps f <.> map_trans f;;
 let iter f a = iter_trans f a; iter_eps f a;;

 (* States, Transitions, and Function Symbols *)
 let agents a =
  let add _ r ps = Option.fold (flip List.cons ps) ps (Rhs.agent r) in
  List.unique_hash (fold_trans add a [])
 ;;

 let category f a = M.liftm (C.category f a.categorization);;
 let funs ?(p = const true) a = List.filter p (C.funs a.categorization);;

 let states ?(i = false) a =
  List.unique_hash (fold (fun l r ps -> 
   let ps = List.rev_append (Rhs.states r) ps in
   if i then ps else List.rev_append (Lhs.states l) ps) a [])
 ;;

 let finals ?(i = false) a = List.intersect a.finals (states ~i:i a);;

 let trans ?(p = const (const true)) a =
  fold (fun l r ts -> if p l r then (l,r) :: ts else ts) a []
 ;;

 (* Epsilon Closures *)
 let ec_states ps a =
  let rec ec ps qs a =
   let qs = List.rev_append ps qs in
   let ps =
    List.foldl (fun ps p ->
     let ec = Rhs.states <.> T.find (State p) in
     let rs = catch Not_found ec [] a.epsilons in
     let rs = List.filter (fun p -> not (List.mem p qs || List.mem p ps)) rs in
     List.rev_append rs ps) [] ps
   in
   if ps = [] then qs else ec ps qs a
  in
  ec ps [] a
 ;;

 let ec_rhs r a =
  let ps = Rhs.states r in
  List.foldl (flip Rhs.add) r (ec_states ps a)
 ;;

 let ec ?(r = false) a =
  let a = map_trans (flip ec_rhs a) a in
  if r then {a with epsilons = T.clear a.epsilons} else a
 ;;

 (* Reachability Analysis *)
 (* which states are reachable from instances of term [t] *)
 let history_position c t a =
  let existsf_trans f p = M.return <.> existsf_trans f p in
  let exists = if c then existsc_trans else existsf_trans in
  let rec history_position p a = function
   | Term.Var _ | Term.State _ -> M.return None
   | Term.Fun (f,ts) ->
    let matches ti pi = match ti with Term.State p -> p = pi | _ -> true in
    exists f (const <.> List.for_all2 matches ts <.> Lhs.args) a >>= fun c ->
    if c then M.return (Some p) else
     M.foldli (fun i q ti ->
      history_position (Pos.add_last i p) a ti >>= function
       | None -> M.return q
       | Some q' -> M.return (Some (Option.fold (const p) q' q))) None ts
  in
  history_position Pos.root a t
 ;;

 let evaluate reach find ec m ts =
  let rec paths i xs m = function
   | [] -> M.return (List.rev xs,m)
   | ti::ts ->
    if Term.is_var ti then paths (i+1) (Left ti::xs) m ts else
     let ec = List.map (fun (ts,qs) -> (ts,ec qs)) in
     let fail m = M.return ([],m) and paths ps = paths (i+1) (Right ps::xs) in
     let paths ps m = if ps = [] then fail m else paths (ec ps) m ts in
     try paths (find i ti m) m with Not_found -> reach i m ti >>= uncurry paths
  in
  paths 0 [] m ts
 ;;

 let compute ec qd xs h l r m =
  m >>= fun ps -> try
   (* combine subpaths *)
   let f = Option.the (Lhs.root l) and qs = Lhs.args l in
   let var = function Term.Var x -> x | _ -> failwith "not a variable" in
   let generate q x = [(x,S.singleton (var x) q,false)] in
   let matches q (ys,qs) = if List.mem q qs then ys else [] in
   let check ys = if ys = [] then raise Not_found else ys in
   let filter q = either (generate q) (check <.> List.flat_map (matches q)) in
   let zs = List.foldl2 (fun zs q ys -> filter q ys :: zs) [] qs xs in
   let add (ti,si,hi) (ts,s,h) = (ti::ts,S.union si s,hi || h) in
   let combine x p = try Option.map (add p) x with S.Inconsistent -> None in
   let ys = List.flat_times combine (Some ([],S.empty,false)) zs in
   let ys = List.foldl (fun ys -> Option.fold (fun y -> y::ys) ys) [] ys in
   let ys = List.unique_hash ys in
   (* compute paths *)
   if ys = [] then M.return ps else
    let mem p r = if Rhs.mem p r then [p] else [] in
    let filter = Option.fold mem (const []) (Rhs.agent r) in
    let agent = List.singleton <.> Option.the <.> Rhs.agent in
    let states f = if qd then f else ec <.> Rhs.states in
    M.lift (Option.fold (states filter) []) (find l h) >>= fun us ->
    let vs = List.diff (states agent r) us in
    let modify h' (ts,s,h) = (Term.Fun (f,ts),s,h || h') in
    let create h' = List.foldl (fun xs y -> modify h' y :: xs) [] ys in
    if us = [] && vs = [] then M.return ps
    else if us = [] then M.return ((create false,vs)::ps)
    else if vs = [] then M.return ((create true,us)::ps)
    else M.return ((create true,us)::(create false,vs)::ps)
  with Not_found -> M.return ps
 ;;

 let restrict hpos pos m ps =
  if Option.fold ((<>) pos) true hpos then M.return (ps,m) else
   M.return (List.rev (List.foldl (fun ps (xs,qs) ->
    let xs = List.filter Triple.thd xs in
    if xs = [] then ps else (xs,qs)::ps) [] ps),m)
 ;;

 let unfold (ps,m) =
  let rec add xs ps' p = function
   | [] -> (xs,p)::ps'
   | (ys,q)::ps ->
    if p = q then List.rev_append ps' ((List.rev_append xs ys,q)::ps)
    else add xs ((ys,q)::ps') p ps
  in
  let ps = List.foldl (fun ps (xs,qs) ->
   let xs = List.map Triple.drop_thd xs in
   List.foldl (flip (add xs [])) ps qs) [] ps
  in
  M.return (List.map (Pair.apply List.unique_hash id) ps,m)
 ;;

 let analyse c h qd dir m t a =
  let foldf_trans g f a = M.return <.> foldf_trans g f a in
  let fold = if c then foldc_trans else foldf_trans in
  let history = Option.fold (const true) false h in
  let generate h = M.lift (flip Pair.make h) (history_position c t a) in
  Option.fold generate (M.return (None,create 0)) h >>= fun (hpos,h) ->
  let rec reach pos m = function
   | Term.Var x as t ->
    let qs = if qd then agents a else states ~i:true a in
    M.return (List.map (fun q -> ([(t,S.singleton x q,false)],[q])) qs,m)
   | Term.State q as t ->
    if dir && Pos.is_root pos then M.return ([([(t,S.empty,false)],[q])],m)
    else M.return ([([(t,S.empty,false)],ec_states [q] a)],m)
   | Term.Fun (f,ts) as t ->
    (* compute subpaths *)
    let extend pos i = Pos.add_last i pos in
    let prefix pos i = Option.fold (Pos.(<=) (extend pos i)) false hpos in
    let find i t = if prefix pos i then raise Not_found else Path.find t in
    let ec = flip ec_states a in
    evaluate (reach <.> extend pos) find ec m ts >>= fun (xs,m) ->
    (* compute paths *)
    if xs = [] && ts <> [] then M.return ([],m) else
     let ec = if dir && Pos.is_root pos then id else flip ec_states a in
     M.join (fold f (compute ec qd xs h) a (M.return [])) >>= fun ps ->

(* FIXME test *)
let ps = List.unique_hash ps in

     let prefix = Option.fold (Pos.(<) pos) false hpos in
     restrict hpos pos (if prefix then m else Path.add t ps m) ps
  in
  if history && hpos = None then M.return ([],m) else try
   let hpos = if history then Some Pos.root else None in
   restrict hpos Pos.root m (Path.find t m) >>= unfold
  with Not_found -> reach Pos.root m t >>= unfold
 ;;

 let paths c h f m t a = match t with
  | Term.Var x ->
   let qs = if a.quasi_det then agents a else states ~i:true a in
   let qs = if f then List.intersect (finals ~i:true a) qs else qs in
   M.return (List.map (fun q -> ([(t,S.singleton x q)],q)) qs,m)
  | Term.State q -> M.return ([([(t,S.empty)],q)],m)
  | Term.Fun (g,_) ->
   analyse c h a.quasi_det true m t a >>= fun (ps,m) ->
   let filter qs = List.filter (flip List.mem qs <.> snd) ps in
   if f then M.return (filter (finals ~i:true a),m) else M.return (ps,m)
 ;;

 (* is state [p] reachable from term [t] *)
 let compute ec qd xs h l r m =
  let rec insert x xs = function
   | [] -> [(x,xs)]
   | (y,ys)::zs ->
    if x = y then (y,List.rev_append xs ys) :: zs
    else (y,ys) :: insert x xs zs
  in
  m >>= fun ps -> try
   (* combine subpaths *)
   let matches q (c,qs) = if List.mem q qs then [c] else [] in
   let check cs = if cs = [] then raise Not_found else cs in
   let filter q = check <.> List.flat_map (matches q) <.> Either.right in
   let combine c q ys = List.exists id (filter q ys) || c in
   let c = List.foldl2 combine false (Lhs.args l) xs in
   (* compute paths *)
   let mem p r = if Rhs.mem p r then [p] else [] in
   let filter = Option.fold mem (const []) (Rhs.agent r) in
   let agent = List.singleton <.> Option.the <.> Rhs.agent in
   let states f = if qd then f else ec <.> Rhs.states in
   M.lift (Option.fold (states filter) []) (find l h) >>= fun us ->
   let vs = List.diff (states agent r) us in
   if us = [] && vs = [] then M.return ps
   else if us = [] then M.return (insert (c || false) vs ps)
   else if vs = [] then M.return (insert true us ps)
   else M.return (insert true us (insert (c || false) vs ps))
  with Not_found -> M.return ps
 ;;

 let restrict hpos pos m ps =
  if Option.fold ((<>) pos) true hpos then M.return (ps,m)
  else M.return (List.filter fst ps,m)
 ;;

 let reachable c h qd dir m t p a =
  let mem (ps,m) = M.return (List.exists (List.mem p <.> snd) ps,m) in
  let foldf_trans g f a = M.return <.> foldf_trans g f a in
  let fold = if c then foldc_trans else foldf_trans in
  let history = Option.fold (const true) false h in
  let generate h = M.lift (flip Pair.make h) (history_position c t a) in
  Option.fold generate (M.return (None,create 0)) h >>= fun (hpos,h) ->
  let rec reach pos m = function
   | Term.Var x -> M.return ([],m)
   | Term.State q ->
    if dir && Pos.is_root pos then M.return ([(false,[q])],m)
    else M.return ([(false,ec_states [q] a)],m)
   | Term.Fun (f,ts) as t ->
    (* compute subpaths *)
    let extend pos i = Pos.add_last i pos in
    let prefix pos i = Option.fold (Pos.(<=) (extend pos i)) false hpos in
    let find i t = if prefix pos i then raise Not_found else Path.find t in
    let ec = flip ec_states a in
    (if List.exists Term.is_var ts then M.return ([],m)
    else evaluate (reach <.> extend pos) find ec m ts) >>= fun (xs,m) ->
    (* compute paths *)
    if xs = [] && ts <> [] then M.return ([],m) else
     let ec = if dir && Pos.is_root pos then id else flip ec_states a in
     M.join (fold f (compute ec qd xs h) a (M.return [])) >>= fun ps ->
     let ps = List.map (Pair.apply id List.unique_hash) ps in
     let prefix = Option.fold (Pos.(<) pos) false hpos in
     restrict hpos pos (if prefix then m else Path.add t ps m) ps
  in
  if history && hpos = None then M.return (false,m) else try
   let hpos = if history then Some Pos.root else None in
   restrict hpos Pos.root m (Path.find t m) >>= mem
  with Not_found -> reach Pos.root m t >>= mem
 ;;

 let compatible ?(c = false) ?(h = None) ?(t = const M.return) trs a =
  let reachable m r p = reachable c None false false m r p a in
  let d = M.return ([],(Path.empty,Path.empty)) in
  Trs.fold (fun r m ->
   m >>= fun (vs,(m,m')) ->
   let l = Rule.lhs r and r = Rule.rhs r in
   paths c h false m (Term.of_term l) a >>= fun (ls,m) ->
   M.foldl (fun x (ls,p) -> M.foldl (fun (vs,m') (l,s) ->
    M.lift (S.apply_term s) (t l (Term.of_term r)) >>= fun r ->
    let add c = if c then vs else (r,p)::vs in
    M.lift (Pair.apply add id) (reachable m' r p)) x ls) (vs,m') ls >>=
   (M.return <.> Pair.apply id (Pair.make m))) d trs >>=
   (M.return <.> List.unique_hash <.> fst)
 ;;

 let paths ?(c = false) ?(h = None) ?(f = false) t =
  M.lift fst <.> paths c h f Path.empty t
 ;;

 (* Miscellaneous *)
 let copy a =
  let size h = 2 * I.length h in
  let copy h = I.fold (fun f -> I.add f <.> T.copy) h (I.create (size h)) in
  { finals = a.finals;
    dependency = D.copy a.dependency;
    categorization = C.copy a.categorization;
    epsilons = T.copy a.epsilons;
    transitions = copy a.transitions;
    quasi_det = a.quasi_det}
 ;;

 let size = flip (fold (const ((+) <.> Rhs.size))) 0;;

 let adapt a b =
  fold (fun l r m ->
   find l b >>= function None -> m >>= remove l | Some r' ->
    m >>= Option.fold (replace l) (remove l) (Rhs.inter r r')) a (M.return a)
 ;;

 let minus a b =
  fold (fun l r m ->
   find l b >>= function None -> m | Some r' ->
    m >>= Option.fold (replace l) (remove l) (Rhs.diff r r')) a (M.return a)
 ;;

 let restrict fs a =
  fold_trans (fun l _ m ->
   if List.mem (Option.the (Lhs.root l)) fs then m else m >>= remove l)
   a (M.return a)
 ;;

 let reduce a =
  let add r ps = List.rev_append (Rhs.states r) ps in
  let args = function State p -> [p] | Fun (_,ps) -> ps in
  let rec reduce ps a =
   M.lift (Pair.apply id List.unique_hash) (fold (fun l r m ->
    if List.is_subset (args l) ps then M.lift (Pair.apply id (add r)) m
    else m >>= fun (a,ps') -> M.lift (flip pair ps') (remove l a))
    a (M.return (a,[]))) >>= fun (a,ps') ->
   if List.length ps' <> List.length ps then reduce ps' a
   else M.return (set_finals (List.intersect a.finals ps') a)
  in
  reduce (states a) a
 ;;

 let inter a b =
  let gen ass p = M.fresh_state >>= fun q -> M.return (Ass.add p q ass,q) in
  let find ass p = M.return (ass,Ass.find_ran p ass) in
  let map ass p = try find ass p with Not_found -> gen ass p in
  let extend (ass,qs) p = map ass p >>= fun (ass,q) -> M.return (ass,q::qs) in
  let product ass ps qs = M.foldl extend (ass,[]) (List.product ps qs) in
  let combine ass ps qs = M.foldl extend (ass,[]) (List.combine ps qs) in
  let combine_lhs ass l l' = match (l,l') with
   | Fun (f,ps), Fun (_,qs) ->
    M.lift (Pair.apply id (Lhs.make_fun f <.> List.rev)) (combine ass ps qs)
   | _, _ -> failwith "lhs is state"
  in
  let product_rhs ass r r' =
   let agent = Rhs.agent r and agent' = Rhs.agent r' in
   let vs = Rhs.violations r and vs' = Rhs.violations r' in
   let ps = Rhs.states r and ps' = Rhs.states r' in
   product ass ps ps' >>= fun (ass,ps) -> product ass vs vs' >>= fun (ass,vs) ->
   let agent = Option.fold (fun p -> Option.map (pair p) agent') None agent in
   let map = M.lift (Pair.apply id Option.some) <.> map ass in
   Option.fold map (M.return (ass,None)) agent >>= fun (ass,agent) ->
   M.return (ass,Rhs.make agent vs ps)
  in
  let fs = List.intersect (funs a) (funs b) and n = size a * size b in
  let ass = Ass.empty n and c = create n in
  List.foldl (fun m f ->
   foldf_trans f (fun l r m -> foldf_trans f (fun l' r' m ->
    let r = ec_rhs r a and r' = ec_rhs r' b in m >>= fun (ass,c) ->
    combine_lhs ass l l' >>= fun (ass,l) ->
    product_rhs ass r r' >>= fun (ass,r) ->
    M.lift (pair ass) (add l r c)) b m) a m)
   (M.return (ass,c)) fs >>= fun (ass,c) ->
  M.lift (flip set_finals c <.> snd) (product ass (finals a) (finals b)) >>=
  (M.lift (pair ass) <.> reduce)
 ;;

 let analogies a b =
  inter a b >>= fun (ass,c) ->
  M.return (List.map (flip Ass.find_dom ass) (states c))
 ;;

 let inter a = M.lift snd <.> inter a;;

 let combine a b =
  fold (fun l r m -> m >>= extend l (Rhs.states r)) b (M.return a)
 ;;

 let union a b =
  let ps = List.unique_hash (List.rev_append (finals b) (finals a)) in
  M.lift (set_finals ps) (combine a b)
 ;;

 (* Predicates *)
 let is_accepted ?(c = false) t = M.lift ((<>) []) <.> paths ~c:c ~f:true t;;
 let is_empty t = reduce (copy t) >>= fun a -> M.return (finals a = []);;

 let is_reachable ?(c = false) ?(h = None) t p =
  M.lift fst <.> reachable c h false false Path.empty t p
 ;;

 (* Rewrite Terms *)
 (* bottom up *)
 let to_terms = either id (List.map Term.make_state);;

 let fail f xs = 
  let ts = List.times (List.rev_map to_terms xs) in
  M.return (Left (List.map (Term.make_fun f) ts))
 ;;

 let rewrite ?(c = false) t a =
  let foldf_trans g f a = M.return <.> foldf_trans g f a in
  let fold = if c then foldc_trans else foldf_trans in
  let rec rewrite qd t = match t with
   | Term.Var _ -> M.return (Left [t])
   | Term.State q -> M.return (Right [q])
   | Term.Fun (f,ts) ->
    M.rev_map (rewrite a.quasi_det) ts >>= fun xs ->
    if List.for_all Either.is_right xs then
     let ys = List.rev_map (flip ec_states a <.> Either.right) xs in
     let get_agent = List.singleton <.> Option.the <.> Rhs.agent in
     let get_states = if qd then get_agent else Rhs.states in
     let add r = M.lift (List.rev_append (get_states r)) in
     let matches l = List.for_all2 List.mem (Lhs.args l) ys in
     let combine l r m = if matches l then add r m else m in
     M.join (fold f combine a (M.return [])) >>= fun qs ->
     if qs <> [] then M.return (Right (List.unique_hash qs)) else fail f xs
    else fail f xs
  in
  M.lift to_terms (rewrite false t)
 ;;

 (* top down *)
 let join c ts ps a =
  let rec join ms ts ps = match (ts,ps) with
   | [], [] -> M.return (Some ms)
   | t::ts, p::ps ->
    is_reachable ~c:c t p a >>= fun c ->
    if c then join ms ts ps
    else if Term.is_state t then M.return None
    else join ((t,p)::ms) ts ps
   | _, _ -> failwith "unequal lengths"
  in
  join [] ts ps
 ;;

 let min cs =
  let value = List.foldl (fun n (t,_) -> n + Term.size t) 0 in
  let c = List.hd cs and cs = List.tl cs in
  let d = ([c],value c) in
  fst (List.foldl (fun (cs,n) ci ->
   let m = value ci in
   if m < n then ([ci],m) else if m = n then (ci::cs,n) else (cs,n)) d cs)
 ;;

 let choose cs =
  let ds = min cs in
  let d = List.hd ds in
  let value = List.foldl (fun n (_,p) -> n + State.to_int p) 0 in
  let min (di,n) dj = let m = value dj in if m < n then (dj,m) else (di,n) in
  fst (List.foldl min (d,value d) (List.tl ds))
 ;;

 let context ?(c = false) ?(n = 1) t p a =
  let fold g f a =
   if c then foldc_trans g f a else M.return <.> foldf_trans g f a
  in
  let rec context t p = match t with
   | Term.Var _ -> failwith "variable occured"
   | Term.State _ ->
    M.ite (is_reachable ~c:c t p a) M.return (M.return <.> List.cons (t,p)) []
   | Term.Fun (f,ts) ->
    M.join (fold f (fun l r m ->
     if Rhs.mem p (ec_rhs r a) then
      let add ms m = if List.length m > n then ms else m::ms in
      m >>= fun ms -> join c ts (Lhs.args l) a >>=
      (M.return <.> Option.fold (add ms) ms)
     else m) a (M.return [])) >>= fun ms ->
    let evaluate = M.lift List.unique_hash <.> M.flat_map (uncurry context) in
    M.lift (choose <.> List.cons [(t,p)]) (M.map evaluate ms)
  in
  context t p
 ;;

 (* Determinization *)
 (* variant 1: only designated states are propagated during computation *)
 let decode ps a =
  let find p = try D.find_ran p a.dependency with Not_found -> [p] in
  List.unique_hash (List.flat_map find ps)
 ;;

 let solve a =
  let solve l r reps a =
   let get_agent =
    let vs = Rhs.violations r in match decode vs a with
     | [p] -> M.return (p,reps,a)
     | qs ->
      let create p a = M.return (p,(p,vs)::reps,a) in
      try create (D.find_dom qs a.dependency) a
      with Not_found -> M.fresh_state >>= fun p -> create p (add_agent p qs a)
   in
   get_agent >>= fun (p,reps,a) ->
   let check q = q = p || not (D.mem_dom q a.dependency) in
   let r = Option.the (Rhs.filter check (Rhs.set_agent p r)) in
   M.lift (pair reps) (replace l r a)
  in
  fold_trans (fun l r m ->
   if Rhs.is_quasi_det r then m else m >>= uncurry (solve l r))
   a (M.return ([],a))
 ;;

 let complete rep reps a =
  let gen (rep,rep') (p,vs) =
   List.foldl (fun (rep,rep') q ->
    let find rep = catch Not_found (Rep.find q) [] rep in
    let ps = find rep and ps' = find rep' in
    let add ps rep = Rep.add q (p::ps) rep in
    if List.mem p ps then (rep,rep')
    else (add ps rep,add ps' rep')) (rep,rep') vs
  in
  let (rep,rep') = List.foldl gen (rep,Rep.empty) reps in
  let variants = function
   | State _ -> failwith "lhs is state"
   | Fun (f,ps) ->
    let map p = try p :: Rep.find p rep' with Not_found -> [p] in
    List.map (Lhs.make_fun f) (List.times (List.map map ps))
  in
  let agent = Option.the <.> Rhs.agent in
  let ts = fold_trans (fun l r ts -> (variants l,agent r) :: ts) a [] in
  let update a (ls,p) = M.foldl (fun a l -> update l p a) a ls in
  M.lift (pair rep) (M.foldl update a ts)
 ;;
 
 let transfer b a =
  fold_trans (fun l r m ->
   let ps = Rhs.states r and agent = Option.the <.> Rhs.agent in
   let modify r = Rhs.set_agent (agent r) (Rhs.extend ps r) in
   m >>= fun a -> find l a >>= fun r ->
   replace l (modify (Option.the r)) a) b (M.return a)
 ;;

 let dependencies a =
  let ps = decode (states ~i:true a) a in
  let qs = List.flat_map id (D.range a.dependency) in
  let ps = List.unique_hash (List.rev_append ps qs) in
  let add qs d = M.fresh_state >>= fun q -> M.return (D.add q qs d) in
  let check qs d = List.length qs = 1 || D.mem_ran qs d in
  let update d qs = if check qs d then M.return d else add qs d in
  let xs = List.powerset ps in M.foldl update a.dependency xs >>= fun d ->
  let qs = finals ~i:true a in
  let ys = List.map (flip decode a <.> List.singleton) qs in
  let is_final ps = List.exists (List.is_supset ps) ys in
  let encode ps = catch Not_found (D.find_dom ps) (List.hd ps) d in
  let ps = List.map encode (List.filter is_final xs) in
  let qs = List.unique_hash (List.rev_append ps qs) in
  M.return (set_finals qs {a with dependency = d})
 ;;

 let saturate a =
  let p = D.find_dom [] a.dependency and fs = funs a in
  let ps = List.flat_map id (D.range a.dependency) in
  let qs = List.remove p (D.domain a.dependency) in
  let ps = List.unique_hash (List.rev_append ps qs) in
  M.foldl (fun i f -> M.lift (max i) (M.find_ari f)) 0 fs >>= fun i ->
  let xs = State.combinations i ps and r = Rhs.singleton p in
  M.foldl (fun a f -> 
    M.lift (fun i -> List.assoc i xs) (M.find_ari f) >>= fun ys ->
    M.foldl (fun a ps -> add (Lhs.Fun (f,ps)) r a) a ys) a fs
 ;;

 let quasi_det ?(c = false) a =
  let rec quasi_det rep a =
   solve a >>= fun (reps,a) ->
   if reps = [] then M.return a
   else complete rep reps a >>= uncurry quasi_det
  in
  let b = copy a in (if c then dependencies a else M.return a) >>= fun a ->
  solve (ec ~r:false a) >>= fun (reps,a) ->
  let reps = D.fold (fun p -> List.cons <.> pair p) reps a.dependency in
  complete Rep.empty reps a >>= uncurry quasi_det >>= transfer b >>=
  (if c then saturate else M.return) >>= (M.return <.> set_quasi_det)
 ;;

 let det ?(c = false) ?(q = false) a =
  let a = if not q then {a with dependency = D.clear a.dependency} else a in
  M.lift (set_quasi_det <.> map_trans Rhs.det) (quasi_det ~c:c (ec ~r:true a))
 ;;

 (* FIXME drop *)
 (* variant 2: all states are propagated during computation (NOT TESTED) *)
 (*
 let solve a =
  let solve l r ps a =
   let get_agent =
    let vs = Rhs.violations r in
    try M.return (D.find_dom vs a.dependency,ps,a) with Not_found ->
     M.fresh_state >>= fun p -> M.return (p,p::ps,add_agent p vs a)
   in
   get_agent >>= fun (p,ps,a) -> M.return (ps,replace l (Rhs.set_agent p r) a)
  in
  fold_trans (fun l r m ->
   if Rhs.is_quasi_det r then m else m >>= uncurry (solve l r))
   a (M.return ([],a))
 ;;

 let complete ps a =
  let update r p =
   let qs = D.find_ran p a.dependency in
   List.foldl (fun r q ->
    let ps = catch Not_found (D.find_ran q) [] r in
    if List.mem p ps then r else D.replace q (p :: ps) r) r qs
  in
  let r = List.foldl update (D.empty (List.length ps)) ps in
  let variants = function
   | State _ -> failwith "lhs is state"
   | Fun (f,ps) ->
    try
     let map p = try p :: D.find_ran p r with Not_found -> [p] in
     List.map (Lhs.make_fun f) (List.times (List.map map ps))
    with Not_found -> []
  in
  let ts = fold_trans (fun l r ts -> (variants l,r) :: ts) a [] in
  List.foldl (fun a (ls,r) -> List.foldl (fun a l -> add l r a) a ls) a ts
 ;;

 let quasi_det a =
  let rec quasi_det a =
   solve a >>= fun (ps,a) ->
   if ps = [] then M.return a else quasi_det (complete ps a)
  in
  quasi_det (ec ~r:false a) >>= (M.return <.> set_quasi_det)
 ;;

 let det a =
  let det = map_trans Rhs.det in
  quasi_det (ec ~r:true a) >>= (M.return <.> set_quasi_det <.> det)
 ;;
 *)

 (* Printers *)
 let fprintf fmt a =
  let fprintf = List.fprintf State.fprintf "," and ps = finals ~i:true a in 
  F.fprintf fmt "@[@[final states: @[{%a}@]@]@\n@[<1>transitions:" fprintf ps;
  iter (fun l -> F.fprintf fmt "@\n@[%a@ ->@ %a@]" Lhs.fprintf l Rhs.fprintf) a;
  F.fprintf fmt "@]@]"
 ;;

 let to_string = F.flush_str_formatter <.> fprintf F.str_formatter;;

 let fprintfm_trans fmt l r =
  F.fprintf fmt "@\n@["; Lhs.fprintfm fmt l >>= fun _ ->
  M.return (F.fprintf fmt "@ ->@ %a@]" Rhs.fprintf r)
 ;;

 let fprintfm fmt a =
  let fprintf = List.fprintf State.fprintf "," and ps = finals ~i:true a in 
  F.fprintf fmt "@[@[final states: @[{%a}@]@]@\n@[<1>transitions:" fprintf ps;
  let fprintfm l r m = m >>= fun _ -> fprintfm_trans fmt l r in
  fold fprintfm a (M.return ()) >>= fun _ ->
  M.return (F.fprintf fmt "@]@]")
 ;;

 let to_stringm a =
  fprintfm F.str_formatter a >>= (M.return <.> F.flush_str_formatter)
 ;;
end
