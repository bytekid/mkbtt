open SlPrefs
open SlDebug
open SlPrint
open SlHeap
open SlTerms
open SlRewriting
open SlChecker
open SlLearning

type orientation_t = 
    LeftToRight | RightToLeft | Either | Neither
;;

let axioms = Hashtbl.create 1;;

let add_axiom (l',r') =
  Format.printf "add axiom \n";
  let l,r = canon_rule (l',r') in
    if Hashtbl.mem axioms (r,l) then
      failwith "Axiom already oriented"
    else Hashtbl.add axioms (l,r) ()
;;

let is_axiom (l,r) = 
  Hashtbl.mem axioms (canon_rule (l,r))
;;

let axiom_filter (l,r) = function
    Neither  -> Neither
  | LeftToRight -> if is_axiom (r,l) then Neither else LeftToRight
  | RightToLeft -> if is_axiom (l,r) then Neither else RightToLeft
  | Either -> 
      if is_axiom (l,r) then LeftToRight
      else if is_axiom (r,l) then RightToLeft
      else Either
;;

let orient_all (s,t) rules = 
  orientations := !orientations + 2;
  let status = (* sufficient conditions for non-termination *)
    (* s m.w. t then s -> t is non-term. *)
    if try_match_subterm (s,t) then 
      (register_nonterminating_trs [s,t]; (* is this necessary? *)
       if try_match_subterm(t,s) then 
         (register_nonterminating_trs [t,s];
          Neither) 
       else 
         RightToLeft)
    else if try_match_subterm (t,s) then 
      (register_nonterminating_trs [t,s];
       if try_match_subterm(s,t) then 
         (register_nonterminating_trs [s,t];
          Neither) 
       else LeftToRight)
        (* rules with new variables in t are non-terminating *)
    else
      let s_subset_t = subset_vars s t in
      let t_subset_s = subset_vars t s in
        if s_subset_t && not t_subset_s then
          (register_nonterminating_trs [s,t];
           RightToLeft)
        else if not s_subset_t && t_subset_s then
          (register_nonterminating_trs [t,s];
           LeftToRight)
        else if not s_subset_t && not t_subset_s then
          (register_nonterminating_trs [s,t];
           register_nonterminating_trs [t,s];
           Neither)
        else (* ask aprove *)
          Either
  in
  let status' = 
    if !trust_axiom_orientation then 
      axiom_filter (s,t) status
    else 
      status
  in
  let status'',lr_t,rl_t = 
    match status' with
      | Neither -> 
          Neither,max_float,max_float
      | LeftToRight -> 
          let start_t = Unix.gettimeofday() in
          let result = 
            if recall_nonterminating_trs ((s,t)::rules) then Neither
            else if recall_terminating_trs ((s,t)::rules) || 
              is_terminating ((s,t)::rules) then
              (register_terminating_trs ((s,t)::rules); 
               LeftToRight)
            else Neither
          in
          let total_t = Unix.gettimeofday() -. start_t in
            result,total_t,max_float
      | RightToLeft -> 
          let start_t = Unix.gettimeofday() in
          let result = 
            if recall_nonterminating_trs ((t,s)::rules) then Neither
            else if recall_terminating_trs ((t,s)::rules) || 
              is_terminating ((t,s)::rules) then
              (register_terminating_trs ((t,s)::rules); 
               RightToLeft)
            else Neither
          in
          let total_t = Unix.gettimeofday() -. start_t in
            result,max_float,total_t
      | Either ->
          let lr_start_t = Unix.gettimeofday() in
          let left = 
            if recall_nonterminating_trs ((s,t)::rules) then false
            else
              recall_terminating_trs ((s,t)::rules) || 
                is_terminating ((s,t)::rules) 
          in
          let lr_t = (Unix.gettimeofday()) -. lr_start_t in
          let rl_start_t = Unix.gettimeofday() in
            begin
              let right = 
                if recall_nonterminating_trs ((t,s)::rules) then false
                else 
                  recall_terminating_trs ((t,s)::rules) || 
                    is_terminating ((t,s)::rules) 
              in
              let rl_t = (Unix.gettimeofday()) -. rl_start_t in
              let final_status = 
                match left,right with
                    true,true -> Either
                  | false,false -> Neither
                  | true,false -> LeftToRight
                  | false,true -> RightToLeft
              in final_status,lr_t,rl_t
            end
  in
    status'',lr_t,rl_t
;;








