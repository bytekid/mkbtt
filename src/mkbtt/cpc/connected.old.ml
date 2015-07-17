(* connected.ml *)
(** Critical pair criterion CCP
@author Sarah Winkler
@since  2009/11/06
*)

(*** TYPES ***************************************************************)
(*** EXCEPTIONS **********************************************************)
(*** SUBMODULES **********************************************************)
module CProcess = CompletionProcess;;
module RIdx = Node.RewriteIndex;;

(*** GLOBALS *************************************************************)
let conntable : (Term.t * Position.t * Rule.t, CProcess.set) Hashtbl.t = 
 (Hashtbl.create 1000);;

let caching_active = true;;

(*** FUNCTIONS ***********************************************************)

(***** cache redundant critical pairs (incomplete but faster) *****)

let get_conn_key (rule1, p, rule2) =
 let l1_p_sigma = Term.subterm_at (Rule.lhs rule1) p in
 let t = fst (Termxx.normalize l1_p_sigma) in
 (t, p, rule2)
;;

(* returns set of processes for which this overlap was already
recognized as redundant *)
let lookup_redundant_processes (rule1, p, rule2, _) =
 let key = get_conn_key (rule1, p, rule2) in
 Hashtbl.find conntable key
;;

let store_redundant_processes (rule1, p, rule2, _) pset =
 let key = get_conn_key (rule1, p, rule2) in
 Hashtbl.add conntable key pset
;;

(***** compute processes for which equation is redundant *****)

(* predicate checks if process pid already considered s=t *)
let node_is_present_for s t nodeset =
 try
  let s', t' = fst (Termxx.normalize s), fst (Termxx.normalize t) in
  let n = Node.by_terms (Node.twin s' t') in
  Node.label_union n 
  (* have to check whether n in nodeset? *)
 with Not_found -> CProcess.empty ()
;;

(* predicate checking whether peak (rl1, p, rl2) is superfluous as it
   can also be rewritten with rl3, according to connectedness criterion *)
(* assume variables of rl1, rl2, rl3 pairwise disjoint *)
(* sigma unifies l1|_p and l2, tau unifies l3|_q and l3 *)
(* have peaks
   r1' <- l1' -> l1'[r2\sigma]_p and
   r1' <- l1' -> l1\sigma[r3\tau]_q *)
let redundant_for rl1 rl2 rl3 p q tau allnodes =
 let l1', r1' = Rule.lhs rl1, Rule.rhs rl1 in (* already instatiated *)
 let r3' = Term.substitute tau (Rule.rhs rl3) in
 (* critical pair corresponding to P2 is composite anyway*)
 (* critical pair corresponding to P1: *)
 let s, t = r1', Term.replace_subterm_at l1' q r3' in
 (* check if CP already computed for process pid *)
 node_is_present_for s t allnodes
;;

let redundant_processes rl1 rl2 rl3 p q tau allnodes process_set =
 let pset_for_cp = redundant_for rl1 rl2 rl3 p q tau allnodes in
 CProcess.inter process_set pset_for_cp
;;

let get_rewrite_label i b =
 let n = Node.by_id i in
 if b == (Node.orientation n) then Node.l1all n else Node.l2all n
;;

(* get rule and rewrite label from encompassment match *)
let get_pset_for_enc rl1 rl2 p ((i, b), q) allnodes =
 try
  let rl3 = Node.bool_rule b (Node.by_id i) in
  (* filter out rule2=rule3 && p=q *)
  if (Rule.is_variant rl3 rl2) && ((Position.compare p q) == 0) then
   Setx.empty ()
  else
   let rl3' = Rule.renaming rl3 in
   let lh3 = Rule.lhs rl3' in
   let l1sigma = Rule.lhs rl1 in
   let tau = Term.matches (Term.subterm_at l1sigma q) lh3 in
   let r_n = get_rewrite_label i b in
   (* reduce to processes of interest in advance, to avoid effort *)
   redundant_processes rl1 rl2 rl3 p q tau allnodes r_n
 with (* due to nonlinearity in indexing *)
  | Term.Not_matchable -> Setx.empty ()
;;

let compute_redundant (rl1, p, rl2, _) idx allnodes =
 let l1sigma = Rule.lhs rl1 in
 let encs = RIdx.encompassment_candidates_below_root idx l1sigma in
  Setx.fold
   ( fun enctuple red_procs ->
    (* get processes redundant according to this encompassment *)
    let more_red_procs = get_pset_for_enc rl1 rl2 p enctuple allnodes in
    CProcess.union red_procs more_red_procs
   )
  encs
  (CProcess.empty ())
;;


(* looks up redundant processes in hashtable, and returns different set
   if entry was found for this overlap. otherwise, redundnats are
   computed and stored. note that due to caching, fewer redundants might
   be returned than actually possible *)
let filter_nonredundant overlap idx process_set allnodes =
 if caching_active then
  try
   CProcess.diff process_set (lookup_redundant_processes overlap)
  with
  Not_found ->
   begin
    let redundants = compute_redundant overlap idx allnodes in
    let result = CProcess.diff process_set redundants in
    if not (CProcess.is_empty redundants) (* some redundants found *)
     then store_redundant_processes overlap redundants;
    result
   end
 else
  let redundants = compute_redundant overlap idx allnodes in
  CProcess.diff process_set redundants
;;
