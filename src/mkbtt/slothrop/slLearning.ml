open SlPrefs
open SlDebug
open SlPrint
open SlHeap
open SlTerms
open SlRewriting

(* learn status of TRSs *)

let terminating = ref TRSSet.empty;;
let nonterminating = ref TRSSet.empty;;

let rec remove_all test set = 
  TRSSet.fold 
    (fun oldtrs t -> 
       if test oldtrs then t
       else TRSSet.add oldtrs t) 
    set TRSSet.empty
;;

let subtrs trs1 trs2 = 
  TRS.subset trs1 trs2 
;;

let register_terminating_trs _rules = (* () ;; *)
  let rules = List.map canon_rule _rules in
  let newtrs = trs_of_rules rules in
  let _ = debug_if_l TRACE (!debug_learning)
    (lazy ("[ Registering terminating TRS: " ^ (string_of_trs rules))) in
  let subtrstest oldtrs = subtrs oldtrs newtrs in
  let t' = remove_all subtrstest !terminating in
  let _ = debug_if_l TRACE (!debug_learning)
    (lazy (" term subsumed=" ^
             (string_of_int ((TRSSet.cardinal !terminating) -
                               (TRSSet.cardinal t'))) ^
             ", card=" ^ (string_of_int ((TRSSet.cardinal t')+1)) ^
             " ]")) in
  let t'' = TRSSet.add newtrs t' in
    terminating := t''
;;

let register_nonterminating_trs _rules = (* () ;; *)
  let rules = List.map canon_rule _rules in
  let newtrs = trs_of_rules rules in
  let _ = debug_if_l TRACE (!debug_learning)
    (lazy ("[ Registering nonterminating TRS: " ^ (string_of_trs rules))) in
  let suptrstest oldtrs = subtrs newtrs oldtrs in
  let t' = remove_all suptrstest !nonterminating in
  let _ = debug_if_l TRACE (!debug_learning)
    (lazy (" nonterm subsumed=" ^
             (string_of_int ((TRSSet.cardinal !nonterminating) -
                               (TRSSet.cardinal t'))) ^
             ", card=" ^ (string_of_int ((TRSSet.cardinal t')+1)) ^
             " ]")) in
  let t'' = TRSSet.add newtrs t' in
    nonterminating := t''
;;

let recall_term_count = ref 0;;
let recall_nonterm_count = ref 0;;

let recall_terminating_trs _rules = (* false ;; *)
  let rules = List.map canon_rule _rules in
  let newtrs = trs_of_rules rules in
  let trstest oldtrs = subtrs newtrs oldtrs in
    if TRSSet.exists trstest !terminating then
      (debug_if_l TRACE (!debug_learning) (lazy "Recall: terminating");
       incr recall_term_count; true)
    else false
;;

let recall_nonterminating_trs _rules = (* false ;; *)
  let rules = List.map canon_rule _rules in
  let newtrs = trs_of_rules rules in
  let trstest oldtrs = subtrs oldtrs newtrs in
    if TRSSet.exists trstest !nonterminating then
      (debug_if_l TRACE (!debug_learning) (lazy "Recall: nonterminating");
       incr recall_nonterm_count; true)
    else false
;;

  
