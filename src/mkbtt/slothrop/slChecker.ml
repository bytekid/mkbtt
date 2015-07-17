
(*** MODULES *************************************************************)
module Term = U.Term;;
module Rule = U.Rule;;
module Trs = U.Trs;;
module S = U.Signature;;
module RM = U.Monad;;
module WMonad = World.Monad;;
module P = Util.Pair;;

(*** OPENS ********************************************************************)
open Util;;

open SlPrefs
open SlDebug
open SlTptp
open SlTerms
open SlPrint
open Unix
open SlStatus
open WMonad

(*** GLOBALS *************************************************************)
let options = ref Completion.default_options;;

(* some globals from original checker *)
let call_count = ref 0;;
let call_time = ref 0.;;
let max_call_time = ref 0.;;
let min_call_time = ref max_float;;
let total_yes_time = ref 0.;;
let max_yes_time = ref 0.;;
let yes_count = ref 0;;
let no_count = ref 0;;
let maybe_count = ref 0;;
let orientations = ref 0;;
let duplicates = ref 0;;

let checker_name = "Some Prover ...";;

(*let checker_cmd = ref "blah";;*)
let checker_cmd () = Completion.command !options;;

let read_answer (ic,oc) = 
  output_string oc ".\n";
  flush oc;
  let ans = input_line ic in
    ignore (input_line ic); 
    ignore (input_line ic); 
    ans
;;

let tell (ic,oc) msg =
  debug_if TRACE (!debug_checker) msg;
  output_string oc msg;
  output_char oc '\n'
;;

let ask (ic,oc) = 
  match read_answer (ic,oc) with 
      "YES" -> TERM_YES
    | "NO" -> TERM_NO
    | "MAYBE" -> TERM_UNKNOWN
    | "timed out" -> TERM_UNKNOWN
    | x -> failwith ("Error communicating with " ^ (checker_cmd ()) ^ 
                       ": responded with " ^ x)
;;

let start cmd = 
  let ic,oc = Unix.open_process cmd in
    set_binary_mode_in ic false ; 
    set_binary_mode_out oc false ;
    (ic,oc)
;;

let stop (ic,oc) = 
  close_in ic;
  ignore (Unix.close_process(ic,oc))
;;

let _handle = ref None;;

let set_handle cmd h =
  _handle := Some(cmd,h); h
;;

let get_handle cmd = 
  match !_handle with 
      None -> 
        debug_if_l TRACE (!debug_checker) 
          (lazy ("Starting AProVE (" ^ cmd ^ ") ..."));
        let h = start cmd in
          set_handle cmd h
    | Some(s,h) when s = cmd -> h
    | Some(s,h) -> 
        debug_if_l TRACE (!debug_checker) 
          (lazy ("Restarting AProVE (" ^ cmd ^ ") ..."));
        stop h;
        let h' = start cmd in
          set_handle cmd h'
;;


let shutdown () = 
  match !_handle with 
      Some(cmd,h) -> 
        stop h
    | None -> ()
;;

let fresh_var = liftm RM.fresh_var;;
let fresh_fun = liftm RM.fresh_fun;;

(* is_terminating originally in checker module *)
let rec to_term vm fm = function
 | V _ as x ->
  let find = 
   try return (List.assoc x vm,vm) 
   with Not_found -> fresh_var >>= fun y -> return (y,(x,y)::vm) 
  in
  find >>= fun (x,vm) -> return (Term.Var x,vm,fm)
 | T (f,ts) ->
  let find = 
   try return (List.assoc f fm,fm) 
   with Not_found -> fresh_fun >>= fun g -> return (g,(f,g)::fm) 
  in find >>= fun (f,fm) ->
  let foldup ti (ts,vm,fm) = 
   lift (Triple.apply (flip List.cons ts) id id) (to_term vm fm ti) 
  in
  foldr foldup ([],vm,fm) ts >>= 
  fun (ts,vm,fm) -> return (Term.Fun (f,ts),vm,fm)
;;
 

let to_trs = 
 let add_rule (trs, fm) (s, t) =
  to_term [] fm s >>= fun (s, vm, fm) ->
  to_term vm fm t >>= fun (t, _, fm) ->
  return (Trs.add (Rule.of_terms s t) trs, fm)
 in
 (lift fst) <.> (foldl add_rule (Trs.empty, []))
;;

let get_trs rs = 
 let m = run World.initial_context (to_trs rs) in 
 match RM.run (S.empty 100) m with
  | Left e -> raise (Completion.GiveUp "Error when checking termination")
  | Right r -> r
;;

let is_terminating rules =
 let check = 
  to_trs rules >>= fun trs ->
  get >>= fun s ->
  if Completion.check_externally !options then
   ExternalTermination.check trs
  else
   InterfaceTTT.check trs
 in
 let c = World.initial_context in
 match RM.run (S.empty 100) (run c check) with
  | Left e -> raise (Completion.GiveUp "Error when checking termination")
  | Right r -> r
;;
(*
let is_terminating all_rules vars =
  match all_rules with
      r::rs ->
        let vars_msg = string_of_vars_tpdb vars in
        let rules_msg = string_of_trs_tpdb all_rules in
        let handle = get_handle !checker_cmd in
          incr call_count;
          debug_if_l TRACE (!debug_checker)
            (lazy ("tell: " ^ (string_of_int !call_count)));
          List.iter (tell handle) [vars_msg;rules_msg];
          let start_t = gettimeofday() in
          let status = ask handle in
          let stop_t = gettimeofday() in
          let this_call_time = stop_t -. start_t in
            call_time := !call_time +. this_call_time;
            if (compare this_call_time !max_call_time) > 0 then
              max_call_time := this_call_time;
            if (compare this_call_time !min_call_time) < 0 then
              min_call_time := this_call_time;
            debug_if_l TRACE (!debug_checker)
              (lazy ("ask: " ^ (string_of_int !call_count) ^ " " ^
                       (string_of_checker_status status) ^ " " ^
                       (string_of_float this_call_time)));
            begin
              match status with
                  TERM_YES ->
                    incr yes_count;
                    total_yes_time := !total_yes_time +. this_call_time;
                    if this_call_time > !max_yes_time then
                      max_yes_time := this_call_time;
                    SlLearning.register_terminating_trs all_rules;
                    true
                | TERM_NO ->
                    incr no_count;
                    SlLearning.register_nonterminating_trs all_rules;
                    false
                | TERM_UNKNOWN ->
                    incr maybe_count;
                    false
            end
    | [] -> failwith "Testing empty TRS for termination"
;;*)
