open SlPrefs
open SlDebug
open SlPrint
open SlHeap
open SlTerms
open SlRewriting
open SlCp
open SlLearning
open SlOrient
open SlChecker
open SlConjectures

exception CONJECTURES_SOLVED;;
exception FAIL;;

let string_of_float =
  Printf.sprintf "%g"
;;

type state_t = 
    (((term * term) * eq_status) list) *   (* unoriented equations *)
      RuleHeap.t *           (* unmarked rules *)
      ((term * term) list) * (* marked rules *)
      ((term * term) list)   (* constraint TRS *)
;;

type metastate_t = 
    int list *               (* branch prefix *)
      int *                    (* step *)
      float *                  (* time spent *)
      float                    (* max call time *)
;;

type cost_t = float;;

module ProbHeap = Imperative
  (struct 
     type t = cost_t * (metastate_t * state_t)
     let compare = fun (i1,_) (i2,_) -> 
       let epsilon = i2 -. i1 in
         if epsilon > 0. then 1
         else if epsilon < 0. then -1
         else 0
   end)
;; 

let clone_heap h = 
  let h' = RuleHeap.create (RuleHeap.size h) in
    RuleHeap.load h' (RuleHeap.dump h); h'
;;

let frontier = ProbHeap.create 16;;

let problem_counter_global = ref 0;;

let string_of_id b = 
  let rec string_of_id_h = function
    | b::[] -> string_of_int b
    | b::bs -> (string_of_id_h bs) ^ (string_of_int b)
    | [] -> ""
  in "[" ^ (string_of_id_h b) ^ "]"
;;

let size_of_trs trs = 
  let weight_f = 
    (fun a (l,r) -> 
       a + (SlTerms.size l) + (SlTerms.size r)) 
  in
    List.fold_left weight_f 0 trs
;;

let extract = List.fold_left (fun a ((l,r),s) -> (l,r)::a) [];;

let freshen = List.fold_left (fun a (l,r) -> ((l,r),FreshEQ)::a) [];;

let refresh = List.fold_left (fun a ((l,r),_) -> ((l,r),FreshEQ)::a) [];;


let rules_of_prob (eqs,unmarked,marked,const) = 
  RuleHeap.fold 
    (fun ((l1,r1),i1) rs -> 
       (l1,r1)::rs) unmarked marked
;;

let problem_size(eqs,unmarked,marked,const) =
  (size_of_trs (extract eqs)) + (size_of_trs marked) +
    (RuleHeap.fold (fun (_,i) a -> a + i) unmarked 0)
;;

let find_all_nontrivial_critical_pairs (eqs,unmarked,marked,const) = 
  (* Normalize pair (s,t) with const, add to list a if nontrivial *)
  let n a (s,t) = 
    let s',t' = normalize const s, normalize const t in
      if s' <> t' then (s',t')::a else a
  in

  let rules = rules_of_prob (eqs,unmarked,marked,const) in
    RuleHeap.fold
      (fun (b,_) a ->
         let a' = List.fold_left n a (critical_pairs [b] [b]) in
         let a'' = List.fold_left n a' (critical_pairs [b] rules) in
           List.fold_left n a'' (critical_pairs rules [b])) unmarked []
;;

let max_exp_term t = 
  let map_with_index f l = (* is this in the library somewhere? *)
    let rec map_h i = function
        [] -> []
      | x::xs -> 
          (f i x)::(map_h (i+1) xs)
    in
      map_h 0 l
  in
  let rec max_exp_h pos = function
      V(x,i) -> 0
    | T(f,ts) -> 
        let vals = map_with_index 
          (fun i -> 
             function 
                 (V(x,_)) -> 1
               | (T(f',ts')) as t' when f' = f && 
                   (match pos with None -> true | Some(p) -> i = p) -> 
                   1 + (max_exp_h pos t')
               | (T(f',ts')) as t' ->
                   max_exp_h None t') ts
        in
          maxs (1::vals)
  in
  max_exp_h None t
;;

let max_exp_eqs eqs = 
  let ls,rs = List.split eqs in
    max (maxs (List.map max_exp_term ls))
      (maxs (List.map max_exp_term rs))
;;

let max_exp_prob ((eqs,unmarked,marked,const) as prob) = 
  max_exp_eqs (rules_of_prob prob)
;;

let problem_cost ((eqs,unmarked,marked,const) as prob) (b,s,t,m) =
  if [] = List.filter (function _,StaleEQ -> true | _ -> false) eqs then
    let cps = find_all_nontrivial_critical_pairs prob in
    let init_cost = float 
      ((size_of_trs cps) + (size_of_trs (extract eqs)) + (size_of_trs const))
    in
      if !detect_divergence then 
        let max_exp = (max_exp_prob prob) - (!max_exp_threshold) + 1 in
          if max_exp > 0 then
            let mult = (1. +. ((float max_exp) /. 5.)) in
              (trace (lazy 
                        ((string_of_id b) ^ 
                           ": multiplying cost by " ^ 
                           (string_of_float mult)));
               init_cost *. mult)
          else
            init_cost
      else
        init_cost
    else
      float (max_int)
;;

let checkpoint ((branch,step,time,max) as ms) prob =
  let size = problem_size prob in
  let cost = problem_cost prob ms in
  let min = 
    if ProbHeap.is_empty frontier then 0.
    else let m =  fst (ProbHeap.peek frontier) in m 
  in
    debug
      (lazy 
         ((string_of_id branch) ^ ": Checkpoint " ^
            "(size=" ^ (string_of_int size) ^
            ", time=" ^ (string_of_float time) ^ 
            ", max=" ^ (string_of_float max) ^ 
            ", num=" ^ (string_of_int !problem_counter_global) ^ 
            ", cost=" ^ (string_of_float cost) ^
            ", pending=" ^ (string_of_int (ProbHeap.size frontier)) ^ 
            ", min=" ^ (string_of_float min) ^ ")"
         ));
    ProbHeap.add frontier (cost,((branch,step,time,max),prob))
;;


let backtrack id = 
  debug 
    (lazy ((string_of_id id) ^ ": Backtrack (" ^ 
             "orientations=" ^ (string_of_int !orientations) ^ ", " ^ 
             "checked=" ^ (string_of_int !call_count) ^ ", " ^
             "T,NT,?=" ^ (string_of_int !yes_count) ^ "," ^ 
             (string_of_int !no_count) ^ "," ^
             (string_of_int !maybe_count) ^ ")"
          ));
  raise FAIL
;;

let add_constraint rl const =
  let (l,r) = canon_rule rl in
    (* dbg ("Constrain: " ^ (string_of_rule (l,r))); *)
    if List.exists (fun (s,t) -> rule_generalization (s,t) (l,r)) const then
      ((* dbg ("Instance.");*) const)
    else
      List.fold_left 
        (fun rs (s,t) -> 
           if rule_instance (s,t) (l,r) then 
             ((* dbg ("Generalizes: " ^ (string_of_rule (s,t))); *) rs)
           else (s,t)::rs) 
        [(l,r)] const 
;;

let choose_rule unmarked = 
  fst (RuleHeap.pop unmarked)
;;

let simplify = function
    (l,r,eqs,unmarked,marked) ->
      let rules = rules_of_prob (eqs,unmarked,marked,[]) in
      let rec simpl_marked = function
          ([],eqs',marked') -> (eqs',marked')
        | ((g,d)::_U,eqs',_U') ->
            let g' = normalize [(l,r)] g in 
              if g' = g then 
                let d' = normalize ((l,r)::rules) d in 
                  simpl_marked(_U, eqs', (g,d')::_U') 
              else simpl_marked(_U, ((g',d),FreshEQ)::eqs', _U') 
      in
      let rec simpl_unmarked (eqs',unmarked') =
        if RuleHeap.is_empty unmarked then eqs',unmarked'
        else 
          let ((g,d),_) = RuleHeap.pop unmarked in
          let g' = normalize [(l,r)] g in 
            if g' = g then 
              let d' = normalize ((l,r)::rules) d in 
                simpl_unmarked(eqs', (g,d')::unmarked') 
            else simpl_unmarked(((g',d),FreshEQ)::eqs', unmarked') in
      let eqs',unmarked' = simpl_unmarked((refresh eqs),[]) in
      let _ = 
        RuleHeap.clear unmarked ; 
        List.iter 
          (fun (l,r) -> 
             RuleHeap.add unmarked ((l,r), (size l) + (size r))) unmarked' 
      in
      let (eqs'',marked') = simpl_marked(marked,eqs',[]) in 
        (eqs'', marked') 
;;


let choose_smallest = function
    [] -> raise FAIL
  | x::xs ->
      List.fold_left 
        (fun (((b1,b2),_),rest) ((s,t),_) -> 
           if size s + size t < size b1 + size b2 then 
             ((s,t),FreshEQ),((b1,b2),FreshEQ)::rest
           else
             ((b1,b2),FreshEQ),((s,t),FreshEQ)::rest) (x,[]) xs
;;

let choose_smallest_fresh eqs = 
  let fresh_eqs = 
    (List.filter
       (function 
            (l,r),FreshEQ -> true | _ -> false) eqs) 
  in
  let stale_eqs =
    (List.filter
       (function 
            (l,r),StaleEQ -> true | _ -> false) eqs)
  in
  let sm_eq,other_fresh_eqs = choose_smallest fresh_eqs in
    sm_eq,(other_fresh_eqs@stale_eqs)
;;

let choose_equation = choose_smallest_fresh;;

let show_status (_eqs,unmarked,marked,const) ((branch,step,time,max) as ms) pr = 
  let um = (RuleHeap.fold 
              (fun ((l1,r1),i1) rs -> (l1,r1)::rs) unmarked []) 
  in
    pr (lazy ((string_of_id branch) ^ ": State (" ^ 
                   "size=" ^ (string_of_int (problem_size (_eqs,unmarked,marked,const))) ^ ", " ^
                   "cost=" ^ (string_of_float (problem_cost (_eqs,unmarked,marked,const) ms))
                ));
    if _eqs <> [] then pr (lazy (string_of_theory _eqs));
    if um <> [] then pr (lazy (string_of_trs_tab um));
    if marked <> [] then pr (lazy (string_of_trs_marked  marked));
;;

(* orient all equations *)
let rec orient ((branch,step,time,max) as ms) = function
    ([],unmarked,marked,const) -> ms,(unmarked,marked,const)
  | (_eqs,unmarked,marked,const) as prob ->
      let () = show_status prob ms trace in
      let ((s,t),_),eqs = choose_equation _eqs in
      let () = trace
        (lazy ((string_of_id branch) ^ ": Select " ^ 
                 (string_of_identity (s,t)))) in
      let rules = 
        RuleHeap.fold (fun ((l1,r1),i1) rs -> (l1,r1)::rs) unmarked marked in
      let s' = normalize rules s in
      let t' = (* norm rules t in *) 
        if s <> t then normalize rules t else s' in (* XXX is this safe? *)
        if s' = t' then 
          orient ms (eqs,unmarked,marked,const) 
        else 
          let size_s = size s' in
          let size_t = size t' in
          let i = size_s + size_t in
          let status,lr_t,rl_t = orient_all (s',t') const in
            match status with
              | Either -> 
                  let l,r = s',t' in
                  let this_branch',that_branch' = 0::branch,1::branch in
                  let this_t,that_t = lr_t,rl_t in
                  let this_time',that_time' = time+.this_t,time+.that_t in
                  let this_max' = if this_t > max then this_t else max in
                  let that_max' = if that_t > max then that_t else max in
                  let this_ms' = this_branch',step,this_time',this_max' in
                  let that_ms' = that_branch',step,that_time',that_max' in
                  let this_unmarked' = clone_heap unmarked in
                  let that_unmarked' = clone_heap unmarked in
                  let this_eqs',this_marked' = 
                    simplify(l,r,eqs,this_unmarked',marked) in
                  let that_eqs',that_marked' = 
                    simplify(r,l,eqs,that_unmarked',marked) in 
                  let this_const' = add_constraint (l,r) const in
                  let that_const' = add_constraint (r,l) const in
                  let this_prob = 
                    this_eqs',this_unmarked',this_marked',this_const' in
                  let that_prob = 
                    that_eqs',that_unmarked',that_marked',that_const' 
                  in
                    RuleHeap.add this_unmarked' ((l,r),i);
                    RuleHeap.add that_unmarked' ((r,l),i);
                    incr problem_counter_global; 
                    checkpoint this_ms' this_prob;
                    checkpoint that_ms' that_prob;
                    backtrack branch
              | LeftToRight -> 
                  let eqs',marked' = simplify(s',t',eqs,unmarked,marked) in
                  let const' = add_constraint (s',t') const in
                  let time' = time +. lr_t in
                  let max' = if lr_t > max then lr_t else max in
                  let this_prob = eqs',unmarked,marked',const' in
                  let this_ms = branch,step,time',max' in
                    RuleHeap.add unmarked ((s',t'), i);
                    if ProbHeap.is_empty frontier ||
                      fst (ProbHeap.peek frontier) > problem_cost this_prob this_ms
                    then begin
                      orient this_ms this_prob
                    end else begin
                      checkpoint this_ms this_prob;
                      backtrack branch
                    end
              | RightToLeft -> 
                  let eqs',marked' = simplify(t',s',eqs,unmarked,marked) in
                  let const' = add_constraint (t',s') const in
                  let time' = time +. rl_t in
                  let max' = if rl_t > max then rl_t else max in
                  let this_prob = eqs',unmarked,marked',const' in
                  let this_ms = branch,step,time',max' in
                    RuleHeap.add unmarked ((t',s'), i);
                    if ProbHeap.is_empty frontier ||
                      fst (ProbHeap.peek frontier) > problem_cost this_prob this_ms
                    then begin
                      orient this_ms this_prob
                    end else begin
                      checkpoint this_ms this_prob;
                      backtrack branch
                    end
              | Neither -> 
                  let time' = time +. rl_t +. lr_t in
                  let max' = 
                    let mu = if rl_t > lr_t then rl_t else lr_t 
                    in
                      if mu > max then mu else max 
                  in
                    debug
                      (lazy ((string_of_id branch) ^ ": Close "));
                    checkpoint (branch,step,time',max') 
                      (((s',t'),StaleEQ)::eqs,unmarked,marked,const);
                    backtrack branch
;;


let rec complete ((branch,step,time,max) as ms) 
    ((eqs,unmarked,marked,const) as prob) =
  let cost = problem_cost prob ms in
  let size = problem_size prob in
    begin
      debug
        (lazy 
           ((string_of_id branch) ^ ": Orient " ^
              "(size=" ^ (string_of_int size) ^ 
              ", time=" ^ (string_of_float time) ^ 
              ", max=" ^ (string_of_float max) ^ 
              ", cost=" ^ (string_of_float cost) ^ 
              ", step=" ^ (string_of_int step) ^ ")"));
      let (branch',_,time',max'), (unmarked',marked',const') = 
        orient ms prob
      in
      let step' = step + 1 in
        if RuleHeap.is_empty unmarked' then 
          ((branch',step',time',max'),
           ([],unmarked',marked',const'))
        else
          let rl = choose_rule unmarked' in
          let cps = critical_pairs [rl] [rl] @ 
            critical_pairs [rl] marked' @
            critical_pairs marked' [rl]
          in 
            complete (branch',step',time',max') 
              ((freshen cps),unmarked',rl::marked',const')
    end
;;

let grind _identities _conj find_all =
  let identities = freshen _identities in
  let start_t = Unix.gettimeofday() in
  let rec _do_completion conj =
    if !timed_out then
        failwith "Timeout";
    let cost,(((branch,step,time,max) as ms),problem) = 
      ProbHeap.pop frontier in
    let size = problem_size problem in
    let now_t = (Unix.gettimeofday()) -. start_t in
      info
        ((string_of_id branch) ^ ": Open " ^
           "(size=" ^ (string_of_int size) ^ 
           ", cost=" ^ (string_of_float cost) ^ 
           ", time=" ^ (string_of_float time) ^
           ", max=" ^ (string_of_float max) ^
           ", total=" ^ (string_of_float now_t) ^
           ", pending=" ^ (string_of_int (ProbHeap.size frontier)) ^ ")"
        );
      show_status problem ms debug;
      let conj' = semidecide_conjectures (rules_of_prob problem) conj in
        if conj' = [] && not (!completion_mode) then
          raise CONJECTURES_SOLVED
        else
          try 
            let (branch',step',time',max'), 
              (eqs,unmarked,completion,const') = 
              complete ms problem 
            in
            (*let total_t = (Unix.gettimeofday()) -. start_t in
              alert "******* COMPLETION *******";
              alert (string_of_trs completion);
              alert "**************************";
              info ("On branch: " ^ (string_of_id branch')); 
              info ("With size, cost: " ^ 
                       (string_of_int (size_of_trs completion)) ^ ", " ^ 
                       (string_of_float (problem_cost prob' ms')));
              info ("Orientations: " ^ 
                       (string_of_int !orientations));
              info ("Calls to " ^ 
                       (checker_cmd ()) ^ " (all, T, NT, ?): " ^ 
                       (string_of_int !call_count) ^ ", " ^ 
                       (string_of_int !yes_count) ^ ", " ^ 
                       (string_of_int !no_count) ^ ", " ^
                       (string_of_int !maybe_count));
              info ("Time (all, branch, " ^ 
                       (checker_cmd ()) ^ "): " ^ 
                       (string_of_float total_t) ^ ", " ^ 
                       (string_of_float time') ^ ", " ^ 
                       (string_of_float !call_time));
              info ("Time/call in " ^ (checker_cmd ()) ^ 
                       " (Avg, Min, Max): " ^ 
                       (string_of_float 
                          ((!call_time) /. 
                             (float_of_int !call_count))) ^ ", " ^
                       (string_of_float !max_call_time) ^ ", " ^ 
                       (string_of_float !min_call_time));
              info ("Time/T-call in " ^ (checker_cmd ()) ^ 
                       " (Avg, Min, Max): " ^ 
                       (string_of_float 
                          ((!total_yes_time) /. 
                             (float_of_int !yes_count))) ^ ", " ^
                       (string_of_float !max_yes_time) ^ ", " ^
                       (string_of_float max'));*)
(*               info ("Registered TRSs (T, NT): " ^  *)
(*                        (string_of_int (TRSSet.cardinal !terminating)) ^  *)
(*                        ", " ^ *)
(*                        (string_of_int (TRSSet.cardinal !nonterminating))); *)
(*               info ("Learned TRSs (T, NT): " ^  *)
(*                        (string_of_int !recall_term_count) ^  *)
(*                        ", " ^ *)
(*                        (string_of_int !recall_nonterm_count)); *)
              completion,conj'
          with FAIL  -> 
              _do_completion conj'
  in
  let rec keep_going i _conj = 
    info ("Searching for completion " ^ (string_of_int i));
    let dp = _do_completion _conj in
      if ProbHeap.is_empty frontier then 
        dp
      else
        keep_going (i+1) []
  in
  let initial_id = [] in
  let initial_prob = identities,RuleHeap.create 32,[],[] in
  let initial_ms = initial_id,1,0.,0. in
    ProbHeap.add frontier (0.,(initial_ms,initial_prob));
    if !timeout > 0 then
      begin
        debug (lazy ("Setting timer (" ^ (string_of_int !timeout) ^ ")"));
        ignore (Unix.sigprocmask Unix.SIG_UNBLOCK [Sys.sigalrm]);
        ignore (Sys.signal Sys.sigalrm 
                  (Sys.Signal_handle 
                     (fun i -> 
                        error ("Timeout (" ^ 
                                 (string_of_int (!timeout)) ^ "s)");
                        exit 1)));
        ignore (Unix.alarm !timeout)
      end;
    if !detect_divergence then 
      begin
        max_exp_threshold := !max_exp_multiplier * 
          (max_exp_eqs (extract identities));
        debug 
          (lazy 
             ("Setting max_exp threshold: " ^ 
                (string_of_int (!max_exp_threshold))))
      end;
    if find_all then 
      keep_going 1 _conj
    else _do_completion _conj
;;
