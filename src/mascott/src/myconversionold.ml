(*** SUBMODULES **********************************************************)
module Pos = Rewriting.Position;;
module Term = U.Term;;
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
   Empty of Term.t
 | Step of Term.t * Nodex.t_rule * Pos.t * Term.t
;;

(*** FUNCTIONS ***********************************************************)
let iff b c = (b && c) || (not b && (not c))

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
let by_id i = 
 INode.by_id i >>= function
  | None -> failwith "Conversion.by_id: not found"
  | Some n -> return n
;;

let is_axiom i =
 by_id i >>= fun n -> return (Nodex.is_axiom n)
;;

let is_lemma i = is_axiom i >>= fun b -> return (not b)

let rule_rename = World.M.Rule.rename

(* ((i,b),q,(j,c)) is an overlap of rules l1 -> r1 (i) and l2 -> r2 (j) 
 with substitution \sigma. t contains an instance of r1\sigma, i.e.,
 t=C[r1\sigma\tau]_pos and we want to obtain the term C[l1\sigma\tau]. *)
let deduce_back t s pos o b =
 let u = O.source o in
 let (mom, mb), (dad, db) = O.mom o, O.dad o in
 let q = fst (O.position o) in
 let pos' = Pos.append pos q in
 let (n1,p1),(n2,p2) =
  if b then ((mom, not mb), pos), ((dad,db), pos')
  else ((dad,not db), pos'),  ((mom, mb), pos)
 in
 return [Step(t, n1, p1, u); Step(u, n2, p2, s)]
;;

let rewrite_back t s pos o b =
 let u = O.source o in
 let (mom, mb), (dad, db) = O.mom o, O.dad o in
 let q = fst (O.position o) in
 let pos' = Pos.append pos q in
 let (n1,p1),(n2,p2) = 
  if b then ((mom, mb), pos), ((dad,db), pos') 
  else ((dad,db), pos'),  ((mom, mb), pos)
 in
 return [Step(t, n1, p1, u); Step(u, n2, p2, s)]
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

(*
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
;;*)

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
  | [] -> failwith "Conversion.rev: empty sequence"
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
   | SAxiom
   | Axiom -> failwith "Conversion.trace: no axiom expected"
   | Deduce o -> deduce_back s t pos o b
   | Rewrite o -> rewrite_back s t pos o b
   | Extend ((n,b'),_) -> trace (s,(n,iff b' b),pos,t))
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

(*
let singleton e =
 INode.node_by_eqn e >>= function
  | None -> 
   (liftm (Equation.to_stringm e) >>= fun es ->
   failwith ("Conversion.step: no goal found for "^es))
  | Some (i,n) ->
   let s,t = Equation.terms e in
   let s',t' = Nodex.data n in
   let b = (s = s') && (t = t') in
   return (Step (s,(i,b),Pos.root,t) :: [Empty t])
;;*)

let node_by_rl rl =
 let l,r = Rule.to_terms rl in
 liftm (Equation.oriented_of_terms l r) >>= fun (e,b) -> 
 liftm (Equation.to_stringm e) >>= fun es -> 
 INode.node_by_eqn e >>= function 
   None -> failwith ("Conversion.node_by_eqn: not found "^es)
  | Some (i,n) -> return ((i,b),n)
;;

let joining_conversion e trs =
 let rec nf t = 
  let rewrite t =
   let rewrite t rl = function
    | Some u -> return (Some u)
    | None -> 
     W.M.Termx.funs_pos t >>= fun pos ->
     node_by_rl rl >>= fun ((i,b),n) -> 
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
 return (append nfs (rev nft))
;; 

let for_goal p =
 NodeState.project_r_closed p >>= fun trs ->
 W.get_options >>= fun o ->
 match Completion.goal o with
  | None -> failwith "Conversion.for_goal: there is no goal"
  | Some e -> 
    joining_conversion e trs >>= fun seq' ->
    Format.printf "Joining:\n%!"; print' seq' >>
    trace [] seq' >>= fun seq -> 
    Format.printf "Traced:\n%!";
    print' seq >> return seq
;;

let for_goal_with_lemmas p =
 NodeState.project_r_closed p >>= fun trs ->
 (* get history of goal *)
 W.get_options >>= fun o ->
 match Completion.goal o with
  | None -> failwith "Conversion.for_goal_with_lemmas: there is no goal"
  | Some e ->
   INode.by_eqn e >>= fun i ->
   History.joint_history [] [Option.the i] >>= fun h ->
   let is = List.unique (List.map snd h) in
   (* create conversions for lemmas *)
   filter is_lemma is >>= fun is ->
   map lemma_for is >>= fun lemmas ->
   (* and conversion for goal *)
   joining_conversion e trs >>= trace is >>= fun seq ->
 (*Format.printf "Final conversion:"; print' seq >>= fun _ ->
 Format.printf "%i Lemmas: %s\n%!" (List.length lemmas) (List.to_string string_of_int " " is );
 iter (fun l -> Format.printf "Lemma:%!"; print' l) lemmas >>= fun _ ->
 Format.printf "now purify:\n%!";*)
 print' seq >>
 return (lemmas @ [seq])
;;

