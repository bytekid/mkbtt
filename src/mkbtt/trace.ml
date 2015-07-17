open Util;;

(*** SUBMODULES **********************************************************)
module RM = U.Monad;;
module Pair = Util.Pair;;
module P = Rewriting.Position;;
module Sub = U.Substitution;;
module Term = U.Term;;
module Rule = U.Rule;;
module Trs = U.Trs;;
module E = Equation;;
module El = U.Elogic;;
module C = Completion;;
module W = World;;
module Monad = W.Monad;;
module IN = IndexedNode;;
module N = Nodex;;
module NS = NodeState;;
module M = U.Monad;;
(*** TYPES ***************************************************************)
type step = (Term.t * P.t * Rule.t * bool * Term.t)

(*** OPENS ***************************************************************)
open World;;
open Monad;;

(*** FUNCTIONS ***********************************************************)
let id = Util.id

let (@) = lift2 (@)

let equal_symbol = get_goal_state >>= fun s -> return s.equal_symbol

let binary_symbol =
 let (>=) = RM.(>>=) in
 liftm (RM.fresh_fun >= RM.create_fun_name >= (RM.create_fun 2))
;;

let print (rl,ss) =
 let print_step (s,p,rl,b,t) =
  W.M.Term.to_stringm s >>= fun s ->
  let p = P.to_string p in
  W.M.Rule.to_stringm rl >>= fun rl ->
  W.M.Term.to_stringm t >>= fun t ->
  Format.printf " %s %s %s using %s at %s \n"
   s (if b then "->" else "<-") t rl p;
  return ()
 in
 W.M.Rule.to_stringm rl >>= fun rl ->
 Format.printf "%s:\n" rl;
 iter print_step ss
;;


let rename_terms (s,t) p (s',t') =
 let (>=) = RM.(>>=) in
 liftm (RM.fresh_fun >= RM.create_fun_name >= (RM.create_fun 2)) >>= fun f ->
 let s,t = Term.subterm p s, Term.subterm p t in
 let fst = Term.Fun(f,[s;t]) in
 let fst',fts' = Term.Fun(f,[s';t']), Term.Fun(f,[t';s']) in
 (*map W.M.Term.to_stringm [s;t;s';t'] >>= fun [ss;st;ss';st'] ->
 Format.printf " sub (%s,%s) to match (%s,%s)\n%!" ss' st' ss st;*)
 if El.matches fst fst' then (
  let tau = El.match_term fst fst' in
  return (tau, true))
 else if El.matches fst fts' then (
  let tau = El.match_term fst fts' in
  return (tau, false))
 else failwith "match_terms in trace fails"
;;

let is_variant t t' =
 El.matches t t' && El.matches t' t
;;

let find_rule es rl =
 try
  binary_symbol >>= fun eq ->
  let is_variant rl rl' = 
   let s,t = Rule.to_terms rl in 
   let s', t' = Rule.to_terms rl' in
(*   let is_variant t t' = (El.is_variant t t') && (El.is_variant t' t) in*)
   is_variant (Term.Fun(eq, [s;t])) (Term.Fun(eq, [s';t']))
  in
  let lr = Rule.of_terms (Rule.rhs rl) (Rule.lhs rl) in
 (*let variant rl' = (Rule.is_variant rl' rl) || (Rule.is_variant rl' lr) in*)
  let variant rl' = (is_variant rl' rl) || (is_variant rl' lr) in
  return (List.find variant (List.map Equation.to_rule es))
 with Not_found -> failwith "Trace.find_rule: not found"
;;

let axiom_step p (s,t) i = 
 IN.by_id i >>= function
  | Some n ->
   W.get_options >>= fun o ->
   (*W.M.Rule.to_stringm (N.brule true n) >>= fun rls ->
   W.M.Trsx.to_stringm (Trs.of_list (List.map Equation.to_rule (C.axioms o))) >>= fun axs ->
   Format.printf "search for rule %s in %s" rls axs;*)
   find_rule (C.axioms o) (N.brule true n) >>= fun rl ->
   W.M.Rule.to_stringm rl >>= fun rls ->
   (*Format.printf "%s is variant\n%!" rls; *)
   rename_terms (s,t) p (Rule.to_terms rl) >>= fun (_, left_right) ->
   return [s,p,rl,left_right,t]
  | None -> failwith "no node found for id"
;;

let normalize s u = E.terms (fst (Equation.oriented_of_terms s u))

let overlap_term ((i,b),q,(i',b')) p (s,t) =
 (*map W.M.Term.to_stringm [s;t] >>= fun [ss;st] ->
 Format.printf "DEDUCE produced %s = %s \n%!" ss st;*)
 IN.brule b i >>= W.M.Rule.rename >>= fun rl ->
 IN.brule b' i' >>= W.M.Rule.rename >>= fun rl' ->
 W.M.Rulex.narrow (Rule.lhs rl) rl' q >>= function
  | Some (s',sigma,_) ->
   let u = Term.replace p (Sub.apply_term sigma (Rule.lhs rl)) s in
   let t' = Sub.apply_term sigma (Rule.rhs rl) in
   (*map W.M.Rule.to_stringm [rl; rl'] >>= fun [sr;sr'] ->
   Format.printf " Overlap between %s (%i) and %s (%i)\n%!" sr i sr' i';*)
   rename_terms (s,t) p (s',t') >>= fun (tau, b) ->
   let u = Sub.apply_term tau u in
   let (s',t') = (Sub.apply_term tau s', Sub.apply_term tau t') in
   let (sc,tc) = (Term.replace p s' s,Term.replace p t' s) in
   (*map W.M.Term.to_stringm [s;t;sc;tc;u] >>= fun [ss;st;ss';st';su] -> 
   Format.printf " Intermediate term %s\n%!" su;
   Format.printf " Want %s=%s but obtain %s <- %s -> %s \n%!" 
    ss st ss' su st';*)
   (if not ((Term.compare s sc == 0) || (Term.compare s tc == 0)) then
    failwith "wrong terms in overlap");
   return (u,b)
  | None -> failwith "no unifier in overlap term"
;;

let rename_eqn rl =
 if Rule.is_rewrite_rule rl then
  liftm (Rule.rename rl)
 else
  liftm (Rule.rename (Rule.of_terms (Rule.rhs rl) (Rule.lhs rl))) >>= fun rl' ->
  return (Rule.of_terms (Rule.rhs rl') (Rule.lhs rl'))
;;

let rewrite_term ((i,b),q,(i',b')) p (s,t) =
 (*map W.M.Term.to_stringm [s;t] >>= fun [ss;st] ->
 Format.printf "REWRITE produced %s = %s \n%!" ss st;*)
 IN.brule b i >>= rename_eqn >>= fun rl ->
 IN.brule b' i' >>= rename_eqn >>= fun rl' ->
 W.M.Rulex.rewrite (Rule.rhs rl) rl' q >>= function
  | Some (t',sigma,_) ->
   let u = Term.replace p (Sub.apply_term sigma (Rule.rhs rl)) s in 
   let (s',t') = (Rule.lhs rl,t') in 
   (*map W.M.Rule.to_stringm [rl; rl'] >>= fun [sr;sr'] ->
   Format.printf " Rewrite %s (%i) with %s (%i) at %s \n%!" 
    sr i sr' i' (P.to_string q);*)
   rename_terms (s,t) p (s',t') >>= fun (tau, b) ->
   let u = Sub.apply_term tau u in
   (*let (s',t') = (Sub.apply_term tau s', Sub.apply_term tau t') in
   let (sc,tc) = (Term.replace p s' s,Term.replace p t' s) in 
   map W.M.Term.to_stringm [s;t;u;sc;tc] >>= fun [ss;st;su;ss';st'] -> 
   Format.printf " Intermediate term %s\n%!" su;
   Format.printf " Want %s=%s obtain intermediate term %s\n%!" ss st su;
   Format.printf " (what I want is %s=%s) \n%!" 
   (if b then ss' else st') (if b then st' else ss');*)
   return (u,b)
  | None -> failwith "no matching possible in overlap term"
;;

let rec seq h p (s,t) i =
 match fst (List.find (fun (_,i') -> i==i') h) with
  | Types.Node.Axiom -> axiom_step p (s,t) i
  | Types.Node.Instance (j, _) -> seq h p (s,t) (fst j)
  | Types.Node.Deduce (rl,q,rl',_) -> 
   overlap_term (rl,q,rl') p (s,t) >>= fun (u,b) ->
   let rlp,rlp' = (fst rl,p),(fst rl',P.append p q) in
   let (rl,p),(rl',p') = (if not b then id else Pair.flip) (rlp,rlp') in
   (seq h p (s,u) rl) @ (seq h p' (u,t) rl')
  | Types.Node.Rewrite (rl,q,rl',_,_) ->   
   rewrite_term (rl,q,rl') p (s,t) >>= fun (u,b) ->
   let rlp,rlp' = (fst rl,p),(fst rl',P.append p q) in
   let (rl,p),(rl',p') = (if b then id else Pair.flip) (rlp,rlp') in   
   (seq h p (s,u) rl) @ (seq h p' (u,t) rl')
;;

let to_sequence h rl =
 IN.by_eqn (E.of_rule rl) >>= function
 | Some i ->  seq h P.root (Rule.to_terms rl) i >>= fun s -> return (rl,s)
 | None -> failwith "no node found for rule"
;;
 

let trs p trs =
 NS.er_contain_closed p >>= History.joint_history [] >>= fun h ->
 map (to_sequence h) (Trs.to_list trs) 
;;

(***** NOW WITH LEMMAS *************************************************)
let base_step p (s,t) i =
 IN.by_id i >>= function
  | Some n ->
   if Nodex.is_axiom n then axiom_step p (s,t) i
   else
    let rl = N.brule true n in
    rename_terms (s,t) p (Rule.to_terms rl) >>= fun (_, left_right) ->
    return [s,p,rl,left_right,t]
  | None -> failwith "no node found for id"
;;

let rec seq' h p (s,t) i =
 match fst (List.find (fun (_,i') -> i==i') h) with
  | Types.Node.Axiom -> axiom_step p (s,t) i
  | Types.Node.Instance (j,_) ->  seq' h p (s,t) (fst j)
  | Types.Node.Deduce (rl,q,rl',_) ->
   overlap_term (rl,q,rl') p (s,t) >>= fun (u,b) ->
   let rlp,rlp' = (fst rl,p),(fst rl',P.append p q) in
   let (rl,p),(rl',p') = (if not b then id else Pair.flip) (rlp,rlp') in
(*   (seq h p (s,u) rl) @ (seq h p' (u,t) rl')*)
   (base_step p (s,u) rl) @ (base_step p' (u,t) rl')
  | Types.Node.Rewrite (rl,q,rl',_,_) ->
   rewrite_term (rl,q,rl') p (s,t) >>= fun (u,b) ->
   let rlp,rlp' = (fst rl,p),(fst rl',P.append p q) in
   let (rl,p),(rl',p') = (if b then id else Pair.flip) (rlp,rlp') in
(*   (seq h p (s,u) rl) @ (seq h p' (u,t) rl')*)
   (base_step p (s,u) rl) @ (base_step p' (u,t) rl')
;;

let trs2 p trs =
 W.get_options >>= fun o ->
 let orig_sig rl =
  if not (C.is_ordered o) then true else
   let axs = Trs.of_list (List.map Equation.to_rule (C.axioms o)) in
   Rule.is_build (Trs.funs axs) rl
 in
 let filter_required_rules trs = (* if ordered, filter out equal, true, false *)
   Trs.of_list (List.filter orig_sig (Trs.to_list trs))
 in
 let to_sequence h rl =
  IN.by_eqn (E.of_rule rl) >>= function
  | Some i ->  seq' h P.root (Rule.to_terms rl) i >>= fun s -> return (rl,s)
  | None -> failwith "no node found for rule"
 in
 let to_sequence' h (_,i) =
  IN.by_id i >>= function
   | Some n ->  
     seq' h P.root (N.data n) i >>= fun s -> 
     return (N.brule true n,s)
   | None -> failwith "to_sequence': no node found for id"
 in
 W.get_options >>= fun o ->
 NS.er_contain_closed p >>= History.joint_history [] >>= fun h ->
 let no_axiom (_,i) = 
  IN.brule true i >>= fun rl ->
  return ((not (List.mem i (Completion.axiom_ids o))) && (orig_sig rl))
 in
 filter no_axiom h >>= fun h' ->
 map (to_sequence' h) h' >>= fun hseq ->
 let trs = filter_required_rules trs in
 map (to_sequence h) (Trs.to_list trs) >>= fun trs_seq ->
(* iter print (List.append hseq trs_seq) >>*)
 return (trs, List.append hseq trs_seq)
;;

