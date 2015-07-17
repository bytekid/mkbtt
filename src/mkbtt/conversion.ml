(*** SUBMODULES **********************************************************)
module Pos = Rewriting.Position;;
module Term = U.Term;;
module Rule = U.Rule;;
module Trs = U.Trs;;
module Elogic = U.Elogic;;
module Sub = U.Substitution;;
module INode = IndexedNode;;
module W = World;;

(*** OPENS ***************************************************************)
open Types.Node;;
open World;;
open World.Monad;;
open Util;;

(*** TYPES ***************************************************************)
type t =
   Empty of Term.t
 | Step of Term.t * Nodex.t_rule * Pos.t * Term.t
;;

(*** FUNCTIONS ***********************************************************)

let start_term = function
 | Step (s,_,_,_) :: _ -> s
 | Empty s :: _-> s
 | [] -> failwith "Conversion.start_term: empty list"

let rec end_term = function
 | [Step (s,_,_,_)] -> s
 | Empty s :: _-> s
 | Step _ :: s -> end_term s
 | [] -> failwith "Conversion.end_term: empty list"

(*** trace back **********************************************************)
let matcher t1 l1 t2 l2 =
 let tau1 = Elogic.match_term t1 l1 in
 let tau2 = Elogic.match_term t2 l2 in
 Sub.union tau1 tau2
;;

let by_id i = 
 INode.by_id i >>= function
  | None -> failwith "Conversion.by_id: not found"
  | Some n -> return n
;;

let is_axiom i =
 by_id i >>= fun n -> return (Nodex.is_axiom n)
;;

let rule_rename = World.M.Rule.rename

(* Narrows a term.
  t: term to be narrowed
  rule: to be used
  p: position in t
 Returns narrowed term t', substitution sigma *)
let narrow t rule p =
 let l', r' = Rule.lhs rule, Rule.rhs rule in
 let t' = Term.subterm p t in
 let sigma = Elogic.unify t' l' in
 let u = Sub.apply_term sigma r' in
 let t' = Sub.apply_term sigma t in
 Term.replace p u t', sigma, rule
;;

(* ((i,b),q,(j,c)) is an overlap of rules l1 -> r1 (i) and l2 -> r2 (j) 
 with substitution \sigma. t contains an instance of r1\sigma, i.e.,
 t=C[r1\sigma\tau]_pos and we want to obtain the term C[l1\sigma\tau]. *)
let deduce_back t s pos ((i,b),q,(j,c)) =
 (* get rules *)
 by_id i >>= fun n -> rule_rename (Nodex.brule b n) >>= fun rl1 ->
 let l1, r1 = Rule.lhs rl1, Rule.rhs rl1 in
 by_id j >>= fun n -> rule_rename (Nodex.brule c n) >>= fun rl2 ->
 (* sigma is the used substitution *)
 let l1r2sigma, sigma, _ = narrow l1 rl2 q in
 let r1sigma = Sub.apply_term sigma r1 in
 (* critical pair was l1r2sigma=r1sigma*)
 let l1sigma = Sub.apply_term sigma l1 in
 let t' = Term.subterm pos t in
 let s' = Term.subterm pos s in
 (* (is_matching t' r1sigma) && (is_matching s' l1r2sigma) consistent *)
 let tau, o =
  try matcher t' r1sigma s' l1r2sigma, true
  with Elogic.Not_matchable | Sub.Inconsistent ->
  matcher t' l1r2sigma s' r1sigma, false
 in
 let l1sigmatau = Sub.apply_term tau l1sigma in
 let t' = Term.replace pos l1sigmatau t in
 let pos' = Pos.append pos q in
 let steps = 
  if o then [Step(t, (i,not b), pos, t'); Step(t', (j,c), pos', s)]
  else [Step(t, (j,not c), pos', t'); Step(t', (i,b), pos, s)]
 in return steps
;;

let rewrite_back t s pos ((i,b), q, (j,c)) =
 (* get rules *)
 by_id i >>= fun n -> rule_rename (Nodex.brule (not b) n) >>= fun rl1 ->
 let l1, r1 = Rule.lhs rl1, Rule.rhs rl1 in
 by_id j >>= fun n -> rule_rename (Nodex.brule c n) >>= fun rl2 ->
(* W.M.Term.to_stringm t >>= fun t_s ->
 W.M.Term.to_stringm s >>= fun s_s -> 
 W.M.Rule.to_stringm rl1 >>= fun rl1_s ->
 W.M.Rule.to_stringm rl2 >>= fun rl2_s ->
 Format.printf "%s = %s originates from %s and %s, position %s\n%!"
  t_s s_s rl1_s rl2_s (Pos.to_string q);*)
 let l1' = Rule.rewrite l1 q rl2 in
 let t', s' = Term.subterm pos t, Term.subterm pos s in
 (* check in which order i and j were applied *)
 (*(is_matching t' r1) && (is_matching s' l1') consistent *)
 try
  let tau = matcher t' r1 s' l1' in
  let t'' = Term.replace pos (Sub.apply_term tau l1) t in
  return [Step(t,(i, b),pos,t''); Step(t'',(j,c),Pos.append pos q,s)]
 with Sub.Inconsistent | Elogic.Not_matchable ->
 (*(is_matching t' l1') && (is_matching s' r1) consistent *) (
  let tau = matcher t' l1' s' r1 in
  let t'' = Term.replace pos (Sub.apply_term tau l1) t in
  return [Step(t,(j, not c),Pos.append pos q,t''); Step(t'',(i,not b),pos,s)]
  )
;;

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
   W.M.Term.to_stringm t >>= fun ts ->
   W.M.Term.to_stringm u >>= fun us ->
   INode.brule b i >>= fun rl ->
   W.M.Rule.to_stringm rl >>= fun rls ->
   Format.printf "%s -> (%s (= %i, %i), %s) -> %s \n%!" ts rls i (if b then 1 else 0)(Pos.to_string p) us;
   print' seq'
;;

let rec last_term = function
   [Step(_,_,_,t)] -> t
 | Step (_,_,_,_) :: seq -> last_term seq
 | _ -> failwith "last_term: empty sequence"
;;

let add_last seq = seq @ [Empty (last_term seq) ]

let brule rl =
 let eqn = Equation.of_rule rl in
 let b =  
  let s', t' = Equation.terms eqn in
  let s, t = Rule.to_terms rl in
  (Term.equal s' s) && (Term.equal t t')
 in
 INode.node_by_eqn eqn >>= function
  | None -> failwith "Conversion.brule: rule not found" 
  | Some (i, n) -> return (i,b)
;;

let rec to_nf trs s =
 let reduct_with s rl =
  let app r p = 
   match r with 
     Some _ -> r 
   | None -> try let t = Rule.rewrite s p rl in Some (rl, p, t) with _ -> None
  in List.fold_left app None (Term.funs_pos s)
 in
 let reduct =
  let app r rl = match r with Some _ -> r | None -> reduct_with s rl in
  List.fold_left app None trs
 in
 match reduct with
  | None -> return [Empty s]
  | Some (rl, p, t) -> 
   brule rl >>= fun brl ->
   to_nf trs t >>= fun seq -> return (Step (s, brl, p, t) :: seq)
;;

let rec rev' = function
  | [Empty s] -> []
  | [Step (s, (i,b), p, t)] -> [Step (t, (i, not b), p, s)]
  | Step (s, (i,b), p, t) :: seq -> let seq' = rev' seq in
    seq' @ [Step (t, (i, not b), p, s)]
  | [] -> []
;;

let rev seq =
 let rec rev' = function
  | [Empty s] -> [], s
  | [Step (s, (i,b), p, t)] -> [Step (t, (i, not b), p, s)], s
  | Step (s, (i,b), p, t) :: seq -> let (seq', t) = rev' seq in
    seq' @ [Step (t, (i, not b), p, s)], s
 in
 let seq', s = rev' seq in seq' @ [Empty s]
;;

let rec append ss ts =
 match ss with
  | (Empty _) :: _ -> ts
  | (Step (_,_,_,_) as s) :: ss' -> s :: (append ss' ts)
  | _ -> failwith "Conversion.append: unexpected input"
;;

let traces : (Term.t * (int * bool) * Pos.t * Term.t, t list) Hashtbl.t 
 = Hashtbl.create 100

let rec trace_step step =
 let rec trace (s,(i,b),pos,t) =
  History.smallest_occurrence i >>= (function
   | Axiom -> failwith "Conversion.trace: no axiom expected"
   | Instance ((j,c),_) -> 
    return [Step(s, (j,(b && c) || (not b && not c)), pos, t)]
   | Deduce (n1, q, n2, _) -> (*Format.printf "deduce " ;*) deduce_back s t pos (n1, q, n2)
   | Rewrite (n1, q, n2, _, _) -> (*Format.printf "rewrite " ;*)rewrite_back s t pos (n1, q, n2))
 in
 try return (Hashtbl.find traces step)
 with Not_found ->
  trace step >>= fun steps ->
  Hashtbl.add traces step steps;
  return steps
;;

let is_lemma i =
 is_axiom i >>= fun is_axiom ->
 GoalState.false_term () >>= fun f ->
 INode.by_id i >>= fun n ->
 let s,t = Nodex.data (Option.the n) in
 return (not is_axiom && not (Term.equal s f || (Term.equal t f)))
;;

let trace lemmas seq =
 let dont_trace i =
  is_axiom i >>= fun is_axiom ->
  is_lemma i >>= fun is_lemma ->
  return (is_axiom || (List.mem i lemmas && is_lemma))
 in
 let rec trace acc seq =
(*  Format.printf "in trace: \n%!"; print' seq >>*)
  match seq with
  | (Empty _) as e :: _ -> return (List.rev (e :: acc))
  | (Step (s,(i,b),pos,t) as step) :: seq ->
    dont_trace i >>= fun dont_trace ->
(*    INode.by_id i >>= fun n ->
    W.M.Rule.to_stringm (Nodex.brule b (Option.the n)) >>= fun rs -> 
    Format.printf "%s is axiom: %i \n" rs (if is_axiom then 1 else 0);*)
    if dont_trace then trace (step :: acc) seq
    else
     trace_step (s,(i,b),pos,t) >>= fun steps -> 
     (*Format.printf "steps:"; print' steps >>*)
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

let true_false =
 W.get_goal_state >>= fun s ->
 let (t,f) =
  match s.true_symbol, s.false_symbol with
     Some t, Some f -> (Term.Fun (t, []), Term.Fun(f, []))
   | _ -> failwith "Control.print_proof: no goal found"
 in
 (INode.node_by_eqn (Equation.of_terms t f) >>= function
  | None -> failwith "Conversion.print_goal: rule not found"
  | Some (i, n) -> return (i,n)) >>= fun (i,n) ->
   let s,t = Nodex.data n in
 return (Step(s, (i, true), Pos.root, t) :: [Empty t])
;;

(* given conversion for true = false, constructs conversion
   for goal s = t; exploits fact that conversion starts with
   true <- eq(u,u) for some term u that is convertible to both s and t *)
let purify seq =
 let is_pos_step p = function
  Step (s, (i,b), pos, t) -> Pos.is_prefix p pos | _ -> false
 in
 let project p = function
  Step (s, ib, pos,t) -> 
   let s', t' = Term.subterm p s, Term.subterm p t in
   Step (s', ib, Pos.tail pos, t')
  | _ -> failwith "take_left: empty not expected"
 in
 let p0, p1 = Pos.make 0, Pos.make 1 in 
 let seq_l = List.map (project p0) (List.filter (is_pos_step p0) seq) in
 let seq_r = List.map (project p1) (List.filter (is_pos_step p1) seq) in
 let seq = (rev' seq_l) @ seq_r in
 return (seq @ [Empty (last_term seq)])
;;


let for_goal =
 true_false >>=
 trace [] >>=
 purify >>= fun seq ->
 (*print' seq >>*) return seq
;;

let for_goal_with_lemmas =
 (* get history of goal *)
 GoalState.true_is_false () >>= INode.by_eqn >>= fun i ->
 History.joint_history(*_for p*) [] [Option.the i] >>= fun h ->
 let is = List.unique (List.map snd h) in
 (* create conversions for lemmas *)
 filter is_lemma is >>= fun is ->
 map lemma_for is >>= fun lemmas ->
 (* and conversion for goal *)
 true_false >>= fun seq ->
 trace is seq >>= fun seq ->
 (*Format.printf "Final conversion:"; print' seq >>= fun _ ->
 Format.printf "%i Lemmas: %s\n%!" (List.length lemmas) (List.to_string string_of_int " " is );
 iter (fun l -> Format.printf "Lemma:%!"; print' l) lemmas >>= fun _ ->
 Format.printf "now purify:\n%!";*)
 purify seq >>= fun seq ->
 (*print' seq >>*)
 return (lemmas @ [seq])
;;

let for_trs trs =
 let for_rule rl =
  W.M.Rule.to_stringm rl >>= fun s ->
  Format.printf "for rule %s\n%!";
  let l,r = Rule.to_terms rl in
  INode.node_by_eqn (Equation.of_rule rl) >>= fun ni ->
  let i,n = Option.the ni in
  let l',r' = Nodex.data n in
  let b = Term.equal l l' && Term.equal r r' in
  return [Step (l, (i,b), Pos.root, r); Empty r]
 in
 map 
  (fun rl -> for_rule rl >>= trace [] >>= fun c -> return (rl,c)) 
  (Trs.to_list trs) >>= fun seqs ->
 Format.printf "convs computed\n%!";
 return seqs
;;
