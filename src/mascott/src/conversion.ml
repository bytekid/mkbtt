(*** SUBMODULES **********************************************************)
module Var = Rewriting.Position;;
module Fun = Rewriting.Position;;
module Pos = Rewriting.Position;;
module T = U.Term;;
module C = U.Context;;
module Rule = U.Rule;;
module Trs = U.Trs;;
module Elogic = U.Elogic;;
module Sub = U.Substitution;;
module INode = IndexedNode;;
module W = World;;
module O = Types.Overlap;;

(*** OPENS ***************************************************************)
open Types.Node;;
open World;;
open World.Monad;;
open Util;;

(*** TYPES ***************************************************************)
type t =
   Empty of T.t
 | Step of T.t * Nodex.t_rule * Pos.t * T.t
;;

(*** GLOBALS *************************************************************)
let blah = ref false

(*** FUNCTIONS ***********************************************************)
let iff b c = (b && c) || (not b && (not c))

let start_term = function
 | Step (s,_,_,_) :: _ -> s
 | Empty s :: _-> s
 | [] -> failwith "Conversion.start_term: empty list"

let rec end_term = function
 | [Step (_,_,_,s)] -> s
 | Empty s :: _-> s
 | Step _ :: s -> end_term s
 | [] -> failwith "Conversion.end_term: empty list"

(*** trace back **********************************************************)
let by_id i = 
 INode.by_id i >>= function
  | None -> failwith ("Conversion.by_id: "^(string_of_int i)^"not found")
  | Some n -> return n
;;

let by_eqn e =
 let l,r = Equation.terms e in
 liftm (Equation.oriented_of_terms l r) >>= fun (e,b) ->
 liftm (Equation.to_stringm e) >>= fun es ->
 INode.node_by_eqn e >>= function
  | None -> failwith ("Conversion.by_eqn: "^es^"not found")
  | Some (i,n) -> return ((i,b),n)
;;

let ac_by_eqn e =
 liftm (Equation.to_stringm e) >>= fun es ->
 INode.node_by_eqn e >>= function
  | None -> failwith ("Conversion.by_eqn: "^es^"not found")
  | Some (i,n) -> return ((i,true),n)
;;

let by_rl rl = by_eqn (Equation.of_rule rl)

let rec print' = function
  | [] -> (*failwith "empty conversion list"*) Format.printf "(empty)\n"; return ()
  | (Empty t) :: [] ->
   W.M.Term.to_stringm t >>= fun ts ->
   Format.printf "%s \n%!" ts;
   return ()
  | (Empty t) :: seq' ->
   W.M.Term.to_stringm t >>= fun ts ->
   Format.printf "%s \n%!" ts;
   print' seq'
  | Step (t, (i,b), p, u) :: seq' ->
   W.M.Termx.unflatten t >>= W.M.Term.to_stringm >>= fun ts ->
   W.M.Termx.unflatten u >>= W.M.Term.to_stringm >>= fun us ->
   INode.brule b i >>= fun rl ->
   W.M.Rulex.unflatten rl >>= W.M.Rule.to_stringm >>= fun rls ->
   Format.printf "%s -> (%s (= %i, %i), %s) -> %s \n%!" ts rls i (if b then 1 else 0)(Pos.to_string p) us;
   print' seq'
;;



let is_axiom i =
 by_id i >>= fun n -> return (Nodex.is_axiom n)
;;

let is_lemma i = is_axiom i >>= fun b -> return (not b)

let is_ac_symbol f = liftm (U.Monad.is_theory U.Label.AC f)

let rec rev' = function
  | [Empty s] -> []
  | [Step (s, (i,b), p, t)] -> [Step (t, (i, not b), p, s)]
  | Step (s, (i,b), p, t) :: seq -> let seq' = rev' seq in
    seq' @ [Step (t, (i, not b), p, s)]
  | [] -> []
  | _ -> failwith "Conversion.rev': unexpected pattern"
;;

let rev seq =
 let rec rev' = function
  | [Empty s] -> [], s
  | [Step (s, (i,b), p, t)] -> [Step (t, (i, not b), p, s)], s
  | Step (s, (i,b), p, t) :: seq -> let (seq', t) = rev' seq in
    seq' @ [Step (t, (i, not b), p, s)], s
  | _ -> failwith "Conversion.rev: unexpected pattern"
 in
 let seq', s = rev' seq in seq' @ [Empty s]
;;

let rec append ss ts =
 match ss with
  | (Empty _) :: _ -> ts
  | (Step _ as s) :: ss' -> s :: (append ss' ts)
  | [] -> ts
  | _ -> failwith "Conversion.append: unexpected input"
;;

let (@@) ss ts = append ss ts

(* "normalize" AC conversions *)
let rec contextualize c = function
 | [Empty t] -> [Empty (C.apply t c)]
 | (Step (s,rl,p,t)) :: seq ->
  let s',t' = C.apply s c, C.apply t c in
  let p' = Pos.append (C.hole_pos c) p in
  (Step (s',rl,p',t')) :: (contextualize c seq)
 | [] -> []
 | _ -> failwith "Conversion.contextualize: unexpected pattern"
;;
(*
let rec cmp s t =
 match (s,t) with
  | T.Var x, T.Var y -> return (Var.compare x y)
  | T.Var _, T.Fun _ -> return (-1)
  | T.Fun _, T.Var _ -> return 1
  | T.Fun (f,_), T.Fun (g,_) when f<>g -> return (Fun.compare f g)
  | T.Fun (f,ss), T.Fun (_,ts) ->
   is_ac_symbol f >>= fun is_ac ->
   if not is_ac then lex ss ts else mul ss ts
and lex ss ts =
 match ss,ts with
  | [],[] -> return 0
  | [],_ -> return -1
  | _,[] -> return 1
  | s::ss,t::ts -> cmp s t >>= function 
   0 -> lex ss ts | v -> return v
and mul ss ts =
 project sort (ss,ts) >>= fun (ss,ts) -> lex ss ts
and insert x = function
 | [] -> return [x]
 | y::ys -> 
  cmp x y >>= fun c -> 
  if c < 0 then return (x::y::ys) 
  else insert x ys >>= fun zs -> return (y::zs)
and sort = function
 | [] -> return []
 | x :: xs -> sort xs >>= fun ys -> insert x ys
;;*)

let gt s t = 
 W.M.Termx.ac_equivalent s t >>= fun e -> 
 return (if e then false else T.compare s t > 0)
;;

let normalize (cr,(ar,_)) f u p = 
 let ft = function T.Fun(f',_) when f = f' -> true | _ -> false in
 let p0, p1 = Pos.make 0, Pos.make 1 in
 let nof t = not (ft t) in
 let fsplit = function  
   T.Fun(f',[t0;tr]) when f=f' -> t0,tr 
 | _ -> failwith "Conversion.split failed"
 in
(* first move parantheses back *)
 let rec associativity u = 
  match u with
  | T.Fun(f', [T.Fun(f'',[s1;s2]); t]) when f = f' && f = f'' ->
     let u' = T.Fun(f', [s1; T.Fun(f'',[s2; t])]) in
     Step (u, (ar,true), Pos.root, u') :: (associativity u')
  | T.Fun(f', [s;t]) when f = f' && nof s ->
     let seq = associativity t in
     let c = C.of_term p1 u in
     contextualize c seq
  | t -> [Empty t]
 in
(* now order terms *)
 let rec order u =
  match u with
   | T.Fun(f', [s; t]) when f = f' && nof s && nof t  ->
    gt s t >>= fun s_gt_t ->
    if s_gt_t then (* reorder *)
     let u' = T.Fun(f', [t; s]) in
     return [Step (u, cr, Pos.root, u'); Empty u']
    else return [Empty u] (* nothing to be done *)
   | T.Fun(f', [s;t]) when f = f' && nof s -> (
    order t >>= fun seq ->
    let t0, tr = fsplit (end_term seq) in
    let seq' = contextualize (C.of_term p1 u) seq in
    gt s t0 >>= fun s_gt_t0 ->
    if s_gt_t0 then (* reorder *)
     let u0 = T.Fun(f,[s;T.Fun(f,[t0;tr])]) in
     let u1 = T.Fun(f, [T.Fun(f,[s;t0]);tr]) in
     let u2 = T.Fun(f, [T.Fun(f,[t0;s]);tr]) in
     let u3 = T.Fun(f, [t0;T.Fun(f,[s;tr])]) in
     let flip =[Step (u0, (ar,false), Pos.root,u1); Step (u1, cr, p0,u2);
      Step (u2, (ar,true), Pos.root,u3)] in
     order u3 >>= fun seq3 ->
     return (seq' @@ flip @@ seq3)
    else
     return seq')
   | _ -> failwith "Conversion.order: unexpected pattern"
 in
 let seq0 = associativity (T.subterm p u) in
 let u' = end_term seq0 in
 order u' >>= fun seq1 ->
 let c = C.of_term p u in
 return (contextualize c (seq0 @@ seq1))
;;

let shorten seq =
 let last t seq =
  let rec last t seq =
   match seq with
   | [] -> None
   | Empty t' :: _ when t = t' -> Some [Empty t]
   | Empty t' :: _ -> None
   | Step (t',_,_,_) :: seq' ->
    if t <> t' then last t seq'
    else match last t seq' with None -> Some seq | res -> res
  in match last t seq with 
   Some s -> s | None -> failwith "Conversion.last messed up"
 in
 let rec srt = function
  | [] -> []
  | Empty t :: _ -> [Empty t]
  | (Step (_,_,_,t) as step) :: seq -> step :: (srt (last t seq))
 in srt seq
;;

let normalize_both_at u v p =
 if T.subterm p u = (T.subterm p v) then return ([],[]) else
 match T.root (T.subterm p u), T.root (T.subterm p v) with
  | None,_ -> failwith "Conversion.normalize_both: variable found"
  | Some f, Some f' when f = f' ->
   is_ac_symbol f >>= fun is_ac ->
   if not is_ac then (
    W.M.Termx.ac_equivalent (T.subterm p u) (T.subterm p v) >>= fun e ->
    if e then return ([],[]) 
    else failwith "Conversion.normalize_both: different terms, no AC")
   else
    ac_by_eqn (Theory.comm f) >>= fun (cr,_) ->
    ac_by_eqn (Theory.assoc f) >>= fun (ar,_) ->
    normalize (cr, ar) f u p >>= fun cu ->
    normalize (cr, ar) f v p >>= fun cv ->
    return (cu, cv)
  | _ -> failwith "Conversion.normalize_both: strange pattern"
;;

let ac_conversion_at u v p = 
 normalize_both_at u v p >>= fun (cu, cv) -> 
 return (append cu (rev' cv))
;;

(*let ac_conversion_at u v p = 
 ac_conversion_at u v p >>= fun seq -> return (shorten seq)
;;*)

let normalize_both u v = 
 let rec convert_at (useq1, vseq1) p =
  let u',v' = end_term useq1, end_term vseq1 in
  normalize_both_at u' v' p >>= fun (useq2,vseq2) ->
  let useq = if useq2 = [] then useq1 else useq1 @@ useq2 in
  let vseq = if vseq2 = [] then vseq1 else vseq1 @@ vseq2 in
(*  Format.printf "u at pos %s\n%!:\n @[" (Pos.to_string p); print' useq >>= fun _ -> Format.printf "@]";
  Format.printf "v at pos %s\n%!:\n @[" (Pos.to_string p); print' vseq >>= fun _ -> Format.printf "@]";*)
  let u'',v'' = end_term useq, end_term vseq in
  match T.subterm p u'', T.subterm p v'' with
   | T.Var _, T.Var _ -> return (useq,vseq)
   | T.Fun(_, ts), T.Fun(_,ts') ->
    assert (List.length ts = (List.length ts')); 
    let convert_at_i i seqi _ = convert_at seqi (Pos.add_last i p) in
    foldli convert_at_i (useq,vseq) ts
 in
 convert_at ([Empty u],[Empty v]) Pos.root
;;

let ac_conversion u v =
 normalize_both u v >>= fun (cu, cv) ->
 return (append cu (rev' cv))
;;

let rec apply_sub sigma = function
 | [] -> []
 | Empty t :: _ -> [Empty (Sub.apply_term sigma t)]
 | Step (s,n,p,t) :: seq -> 
  let s',t' = Pair.map (Sub.apply_term sigma) (s,t) in
  Step (s',n,p,t') :: (apply_sub sigma seq) 
;;

let matcher t1 l1 t2 l2 =
 let tau1 = Elogic.match_term t1 l1 in
 let tau2 = Elogic.match_term t2 l2 in
 Sub.union tau1 tau2
;;

let magic_position (p,s) t =
 liftm U.Monad.fresh_var >>= fun x ->
 let t' = Termx.replace (p,s) (Var x) t in
 W.M.Termx.unflatten t' >>= fun t'' ->
 let ps = T.var_pos x t'' in
 assert (List.length ps = 1);
 return (List.hd ps)
;;

let print_sub sub =
 let sub = Sub.to_list sub in
 let append s (x,t) =
  W.M.Term.to_stringm t >>= fun t ->
  liftm (U.Monad.find_var_name x) >>= fun x ->
  return (s^" "^x^"->"^t)
 in
 foldl append "" sub
;;

let unflatten_sub sigma = 
 let unf (x,t) = W.M.Termx.unflatten t >>= fun t' -> return (x,t') in
 map unf (Sub.to_list sigma) >>= fun sigma' -> 
 return (Sub.of_list sigma')
;;

(* ((i,b),q,(j,c)) is an overlap of rules l1 -> r1 (i) and l2 -> r2 (j) 
 with substitution \sigma. t contains an instance of r1\sigma, i.e.,
 t=C[r1\sigma\tau]_pos and we want to obtain the term C[l1\sigma\tau]. *)
let rec deduce_back t s pos o b =
 let keep = Rule.matches (O.cp o) (Rule.of_terms t s) in
 if not keep then deduce_back s t pos o (not b) >>= fun seq -> return (rev' seq)
 else
  let t',s' = Rule.to_terms (O.cp o) in
  let orule, irule = Triple.fst (O.outer_rule o), Triple.fst (O.inner_rule o) in
  let (mom, mb), (dad, db) = O.mom o, O.dad o in
  (if !blah then (
   W.M.Term.to_stringm t >>= fun ts ->
   W.M.Term.to_stringm s >>= fun ss ->
   W.M.Rule.to_stringm orule >>= fun ors -> 
   W.M.Rule.to_stringm irule >>= fun ir ->
   project W.M.Term.to_stringm (Rule.to_terms (O.cp o)) >>= fun (cp1,cp2) ->
   Format.printf "%s = %s (originally %s = %s) originates from deduce between\n %s and\n %s\n%!" 
    ts ss cp1 cp2 ors ir;
   INode.brule mb mom >>= W.M.Rule.to_stringm >>= fun irs' ->
   INode.brule db dad >>= W.M.Rule.to_stringm >>= fun ors' ->
   Format.printf "Rules accoridng to nodes are %s vs %s\n%!" ors' irs';
   print_sub (O.sub o) >>= fun ssub -> Format.printf "Substitution: %s\n%!" ssub; 
   return ())
  else return ()) >>

  unflatten_sub (O.sub o) >>= fun sigma ->
  W.M.Termx.unflatten (Rule.lhs orule) >>= fun aux ->
  let u1 = Sub.apply_term sigma aux in
  let u1' = Sub.apply_term (O.sub o) (Rule.lhs orule) in
  let u' = Sub.apply_term sigma (Rule.lhs irule) in
  let ((q,_) as qs) = O.position o in
  let u2' = Termx.replace qs u' u1' in
  let v' = Sub.apply_term sigma (Rule.rhs irule) in
  W.M.Termx.unflatten (Termx.replace qs v' u1') >>= fun v2 ->
  W.M.Termx.unflatten (Rule.rhs orule) >>= fun aux ->
  let v1 = Sub.apply_term sigma aux in
  magic_position qs u1' >>= fun q' ->
  W.M.Termx.unflatten u2' >>= fun u2 ->
  (if !blah then (
   project W.M.Term.to_stringm (u1,u2) >>= fun (u1s,u2s) ->
   Format.printf "root of overlap: %s <->* %s\n%!" u1s u2s; return ())
  else return ()) >>
  ac_conversion u2 u1 >>= fun ac ->
  (*W.M.Term.to_stringm v2 >>= fun v2s ->
  Format.printf "post: %s <->* %s\n%!" v2s ss;*)
  let (mom, mb), (dad, db) = O.mom o, O.dad o in
  let pos' = Pos.append (Pos.append pos q) q' in
  let rho = matcher t t' s s' in
  project W.M.Termx.unflatten (s',t') >>= fun (s',t') ->
  ac_conversion t' v2 >>= fun pre ->
  ac_conversion v1 s' >>= fun post ->
  let seq = pre @@ (Step(v2,(dad,not db), pos',u2) :: (ac @@ [Step(u1,(mom,mb), pos, v1)] @@ post)) in
  return (apply_sub rho seq)
;;

let rec rewrite_back t s pos o =
 let t',s' = Rule.to_terms (O.cp o) in
 if not ((s=s') && (t=t')) then 
  (assert (s=t' && s'=t);
  rewrite_back s t pos o >>= fun seq -> return (rev' seq))
 else
  let orule, irule = Triple.fst (O.outer_rule o), Triple.fst (O.inner_rule o) in
  (if !blah then (
   W.M.Term.to_stringm t >>= fun ts ->
   W.M.Term.to_stringm s >>= fun ss ->
   W.M.Rule.to_stringm orule >>= fun ors ->
   W.M.Rule.to_stringm irule >>= fun ir ->
   project W.M.Term.to_stringm (Rule.to_terms (O.cp o)) >>= fun (cp1,cp2) ->
   Format.printf "%s = %s (originally %s = %s) originates from rewrite between %s and %s\n%!"
    ts ss cp1 cp2 ors ir; return () ) else return ()) >>
 
  unflatten_sub (O.sub o) >>= fun sigma -> 
  let ((q,_) as qs) = O.position o in
  let u1 = Rule.rhs orule in 
  let u' = Sub.apply_term sigma (Rule.lhs irule) in
  let u2 = Termx.replace qs u' u1 in
  let v' = Sub.apply_term sigma (Rule.rhs irule) in
  W.M.Termx.unflatten (Termx.replace qs v' u1) >>= fun v2 ->
  magic_position qs u1 >>= fun q' ->
  project W.M.Termx.unflatten (u1,u2) >>= fun (u1,u2) ->
  ac_conversion u1 u2 >>= fun ac ->
  let (mom, mb), (dad, db) = O.mom o, O.dad o in
  let pos' = Pos.append (Pos.append pos q) q' in
  ac_conversion v2 s >>= fun post ->
  (* is pre-AC required? *)
  return (Step(t, (mom,mb), pos, u1) :: ac @ [Step(u2, (dad,db), pos', v2)] @ post)
;;

let extend rule =
 let l, r = Rule.to_terms rule in
 match l with
  | T.Var _ -> failwith "extend_back failed"
  | T.Fun(f,_) ->
   is_ac_symbol f >>= fun f_is_ac ->
   if not f_is_ac then failwith "extend_back failed: no AC symbol"
   else
    liftm U.Monad.fresh_var >>= fun x ->
    let l,r = T.Fun(f,[T.Var x; l]), T.Fun(f,[T.Var x; r]) in
    project W.M.Termx.flatten (l,r) >>= fun (l,r) ->
    return (Rule.of_terms l r,f,x)
;;

let rec extend_back s t pos (n,b) b' =
 let linvar =
  let xs = List.filter T.is_var (T.args s) in
  let xt = List.filter T.is_var (T.args t) in
  let x = List.intersect xs xt in
  assert (List.length x > 0);
  List.hd x
 in
 if b' = b then (
   W.M.Term.to_stringm s >>= fun ss ->
   W.M.Term.to_stringm t >>= fun ts ->
  INode.brule b n >>= fun rl ->
  (if !blah then
    W.M.Rule.to_stringm rl >>= fun rls ->
    Format.printf "searching proof between %s and %s by extending %s %i\n%!"
     ss ts rls (if b then 1 else 0);
    return ()
   else return ()) >>
  let f = Option.the (T.root (Rule.lhs rl)) in
  project W.M.Termx.unflatten (s,t) >>= fun (s,t) ->
  let s' = T.Fun(f,[Rule.lhs rl; linvar]) in
  let t' = T.Fun(f,[Rule.rhs rl; linvar]) in 
  ac_conversion_at s s' Pos.root >>= fun seq_s ->
  ac_conversion_at t' t Pos.root >>= fun seq_t ->
  let res = seq_s @@ [Step (s',(n,b),Pos.make 0,t')] @@ seq_t in
  return res)
 else 
  extend_back t s pos (n, b) (not b') >>= fun seq -> 
  return (rev' (seq))
;;
  
 
let add_last seq = seq @ [Empty (end_term seq) ]

let traces : (T.t * (int * bool) * Pos.t * T.t, t list) Hashtbl.t 
 = Hashtbl.create 100

let rec trace_step step =
 let rec trace (s,(i,b),pos,t) =
  History.smallest_occurrence i >>= (function
   | SAxiom
   | Axiom -> return [Step (s,(i,b),pos,t)]
   | Deduce o -> deduce_back s t pos o b
   | Rewrite o -> rewrite_back s t pos o
   | Extend ((n,b'),_) -> extend_back s t pos (n,b') b)
 in
 try return (Hashtbl.find traces step)
 with Not_found ->
  trace step >>= fun steps ->
  Hashtbl.add traces step steps;
  return steps
;;

let trace lemmas seq =
 let dont_trace i =
  is_axiom i >>= fun is_axiom ->
  return (is_axiom || (List.mem i lemmas))
 in
 let rec trace acc seq =
  (if !blah then (Format.printf "\nin trace: \n%!"; print' seq) 
    else return ()) >>
  match seq with
  | (Empty _) as e :: _ -> return (List.rev (e :: acc))
  | (Step (s,(i,b),pos,t) as step) :: seq ->
    dont_trace i >>= fun dont_trace ->
    by_id i >>= fun n ->
    W.M.Rule.to_stringm (Nodex.brule b n) >>= fun rs -> 
    (*Format.printf "%s not traced: %i \n%!" rs (if dont_trace then 1 else 0);*)
    if dont_trace then trace (step :: acc) seq
    else
     trace_step (s,(i,b),pos,t) >>= fun steps -> 
     (if !blah then (Format.printf "steps:"; print' steps) 
       else return ()) >>
     trace acc (steps @ seq)
  | [] -> failwith "Conversion.trace: empty list"
 in trace [] seq
;;

let lemma_for i = 
 INode.brule true i >>= fun rl ->
 let l,r = Rule.to_terms rl in
 trace_step (l,(i,true),Pos.root,r) >>= fun steps ->
 return (add_last steps)
;;

let joining_conversion e trs =
 let rec nf t = 
  let rewrite t =
   let rewrite t rl = function
    | Some u -> return (Some u)
    | None -> 
     W.M.Termx.funs_pos t >>= fun pos ->
     by_rl rl >>= fun ((i,b),n) -> 
     W.M.ACRewrite.reducts t pos rl >>= function 
      | [] -> return None
      | u :: _ -> return (Some (u,(i,b)))
   in foldr (rewrite t) None (Trs.to_list trs) 
  in
  rewrite t >>= function
   | None -> return [Empty t]
   | Some ((u,sigma,(p,_)),(i,b)) ->  
    nf u >>= fun conv -> return (Step(t,(i,b),p,u) :: conv)
 in
 project nf (Equation.terms e) >>= fun (nfs,nft) -> 
 (if !blah then (
  Format.printf "NF conversion for s:\n%!"; print' nfs >>= fun _ ->
  Format.printf "NF conversion for t:\n%!"; print' nft) else return ()) >>
 return (append nfs (rev nft))
;; 

let store_ac = 
 W.get_options >>= fun o ->
 let ac_symbols = function Theory.AC f -> [f] | _ -> [] in
 let acs = List.flat_map ac_symbols (Completion.theory o) in
 NodeSetAux.acs acs
;;

let for_goal p =
 store_ac >>= fun ns ->
 NodeState.project_r_closed p >>= fun trs ->
 W.get_options >>= fun o ->
 match Completion.goal o with
  | None -> failwith "Conversion.for_goal: there is no goal"
  | Some e -> 
    joining_conversion e trs >>= fun seq' ->
    (*Format.printf "Joining:\n%!"; print' seq' >>*)
    trace [] seq' >>= fun seq -> 
    (*Format.printf "Traced:\n%!"; print' seq >>*) 
    return seq
;;

let lemmas_in seq =
 let lemma = function
  | Empty _ -> []
  | Step (_,(i,_),_,_) -> [i]
 in
 List.sort Pervasives.compare (List.unique (List.flat_map lemma seq))
;;

let for_goal_with_lemmas p =
 store_ac >>= fun _ ->
 NodeState.project_r_closed p >>= fun trs ->
 (* get history of goal *)
 W.get_options >>= fun o ->
 match Completion.goal o with
  | None -> failwith "Conversion.for_goal_with_lemmas: there is no goal"
  | Some e ->
   joining_conversion e trs >>= fun seq' ->
   History.joint_history [] (lemmas_in seq') >>= fun h ->
   let is' = List.unique (List.map snd h) in
   (* create conversions for lemmas *)
   filter is_lemma is' >>= fun is ->
   map lemma_for is >>= fun lemmas ->
   (* and conversion for goal *)
   trace is seq' >>= fun seq ->
   let seq = lemmas @ [seq] in
 (*Format.printf "Final conversion:"; print' seq >>= fun _ ->
 Format.printf "%i Lemmas: %s\n%!" (List.length lemmas) (List.to_string string_of_int " " is );
 iter (fun l -> Format.printf "Lemma:%!"; print' l) lemmas >>= fun _ ->
 Format.printf "now purify:\n%!";*)
 (if !blah then (
  Format.printf "FINALLY:\n%!";
  iteri (fun i s -> Format.printf "Lemma %i:\n%!" i; print' s) seq)
 else return ()) >>
 return seq
;;

let test () =
 let module Sig = U.Signature in
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
 let i,sigma = Sig.create_fun 1 "i" sigma in
 let a_ = T.Fun(a, []) in
 let b_ = T.Fun(b, []) in
 let c_ = T.Fun(c, []) in
 let d_ = T.Fun(d, []) in
 let e_ = T.Fun(e, []) in
 let fab = T.Fun(f, [a_;b_]) in
 let fabc = T.Fun(f, [a_;T.Fun(f, [b_;c_])]) in
 let fba = T.Fun(f, [b_;a_]) in
 let facb = T.Fun(f, [a_;T.Fun(f, [c_;b_])]) in
 let fcba = T.Fun(f, [c_;T.Fun(f, [b_;a_])]) in
 let fbac = T.Fun(f, [b_;T.Fun(f, [a_;c_])]) in
 let fcab = T.Fun(f, [c_;T.Fun(f, [a_;b_])]) in
 let fbacd = T.Fun(f, [fbac;d_]) in
 let fdabc = T.Fun(f, [d_;fabc]) in
 let faa = T.Fun(f, [a_;a_]) in
 let faaa = T.Fun(f, [a_; faa]) in
 let ifab = T.Fun(i, [fab]) in
 let ifba = T.Fun(i, [fba]) in
 let fifabc = T.Fun(f, [T.Fun(i, [fab]); c_]) in
 let fifbac = T.Fun(f, [T.Fun(i, [fba]); c_]) in
 let fcifba = T.Fun(f, [c_; T.Fun(i, [fba])]) in
 let fiabiab = T.Fun(f, [T.Fun(i, [fab]); T.Fun(i, [fab])]) in
 let fibaiba = T.Fun(f, [T.Fun(i, [fba]); T.Fun(i, [fba])]) in
 let t1 = T.Fun(f,[d_; fifabc]) in
 let t2 = T.Fun(f,[fcifba;d_]) in
 let t4 = T.Fun(f,[d_; T.Fun(i, [fifabc])]) in
 let t3 = T.Fun(f,[T.Fun(i, [fcifba]);d_]) in
 let c = W.initial_context in
 ignore (Either.right (U.Monad.run sigma (W.Monad.run c (
 W.get_options >>= fun o ->
 W.set_options {o with theory = [Theory.AC f]} >>
 store_ac >>= fun _ -> 
 ac_by_eqn (Theory.comm f) >>= fun (cr,_) ->
 ac_by_eqn (Theory.assoc f) >>= fun (ar,_) ->
 let test t = 
  normalize (cr,ar) f t Pos.root >>= fun seq ->
  W.M.Term.to_stringm t >>= fun s ->
  Format.printf "\nNF of %s\n%!" s; print' seq
 in
 let test2 t u =
  ac_conversion t u >>= fun seq ->
  W.M.Term.to_stringm t >>= fun s1 ->
  W.M.Term.to_stringm u >>= fun s2 ->
  Format.printf "\nNF of %s <->* %s\n%!" s1 s2; print' seq
 in
 let test2p t u p =
  ac_conversion_at t u p >>= fun seq ->
  W.M.Term.to_stringm t >>= fun s1 ->
  W.M.Term.to_stringm u >>= fun s2 ->
  Format.printf "\nNF at %s of %s <->* %s\n%!" (Pos.to_string p) s1 s2; print' seq
 in
(* test fab >>
 test fba >>
 test fabc >>
 test facb >>
 test fbac >>
 test fcba >>
 test faa >>
 test faaa >>*)
 test2 fab fba >>
 test2 fbacd fdabc >>
 test2 fifabc fifabc >>
 test2p fifabc fifbac (Pos.of_list [0;0]) >>
 test2 ifab ifba >>
 test2 fifabc fifbac >>
 test2 fifabc fcifba >> 
 test2 fiabiab fibaiba >>
 test2 t1 t2 >>
 test2 t3 t4
 ))))
;;

(*test ()*)
 

