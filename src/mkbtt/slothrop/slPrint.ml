open SlTerms
open SlTptp

let rec string_of_suffix i =
  match i with 
      0 -> "0"
    | x -> (string_of_int x) (* "'" ^ (string_of_suffix (i-1)) *)
;;
  
let string_of_variable s i =
  s ^ (string_of_suffix i)
;;

let rec string_of_term = function
  | V(s,i) -> string_of_variable s i
  | T(s,[]) -> s 
  | T(s,t::[]) -> s ^ "(" ^ (string_of_term t) ^ ")"
  | T(s,t::tl) -> s ^ "(" ^ 
      (List.fold_left (fun s t -> s ^ "," ^ (string_of_term t)) 
         (string_of_term t) tl  ) ^ ")"
;;

let rec reflect_term = function
  | V(s,i) -> "V(\"" ^ s ^ "\"," ^ (string_of_int i) ^ ")"
  | T(s,ts) -> "T(\"" ^ s ^ "\",[" ^ 
      (List.fold_left (fun a b -> a ^ (reflect_term b) ^ ";") "" ts)
      ^ "])"
;;

let reflect_rule (l,r) = 
  (reflect_term l) ^ " -> " ^ 
    (reflect_term r) 
;;

let string_of_equation (s,t) =
  (string_of_term s) ^ " = " ^ 
    (string_of_term t) 
;;

let string_of_disequation (s,t) =
  (string_of_term s) ^ " = " ^ 
    (string_of_term t) 
;;

let string_of_conjecture (s,t) =
  (string_of_term s) ^ " =? " ^ 
    (string_of_term t) 
;;

let string_of_identity (s,t) = 
  (string_of_term s) ^ " = " ^ 
    (string_of_term t) 
;;

let string_of_rule (s,t) = 
  (string_of_term s) ^ " -> " ^ 
    (string_of_term t)
;;

let string_of_rewrite s t = 
  (string_of_term s) ^ " -->! " ^ 
    (string_of_term t)
;;

let string_of_rewrite_inv s t = 
  (string_of_term t) ^ " !<-- " ^ 
    (string_of_term s)
;;

(* let rec string_of_relation f s = function *)
(*     x::xs -> (f x) ^ (match s with Some(x) -> " " ^ x | _ -> "") ^  *)
(*       (match xs with [] -> "" | _ -> "\n") ^  *)
(*       (string_of_relation f s xs) *)
(*   | [] -> "" *)
(* ;; *)

let string_of_relation_old f s x =
  let rec string_of_rel_h f s i = function
      x::xs -> 
        (Printf.sprintf "%02d. " i) ^ 
        (f x) ^ (match s with Some(x) -> " " ^ x | _ -> "") ^ 
        (match xs with [] -> "" | _ -> "\n") ^ 
        (string_of_rel_h f s (i+1) xs)
    | [] -> ""
  in
    string_of_rel_h f s 0 x
;;

let string_of_relation f pre post eqs =
  let rec string_of_rel_h i = function
      x::xs -> 
        (pre i) ^ (f x) ^ (post i) ^ 
        (match xs with [] -> "" | _ -> "\n") ^ 
        (string_of_rel_h (i+1) xs)
    | [] -> ""
  in
    string_of_rel_h 0 eqs
;;

let f_empty = fun i -> "";;
let f_mark = fun i -> ". ";;
let f_tab = fun i -> "  ";;
let f_fresh = fun i -> "+ ";;
let f_stale = fun i -> "- ";;
let f_number = fun i -> Printf.sprintf "%02d. " i;;

let string_of_trs_marked t = 
  string_of_relation string_of_rule f_mark f_empty t
;;

let string_of_trs_tab t = 
  string_of_relation string_of_rule f_tab f_empty t
;;

let string_of_trs_numbered t = 
  string_of_relation string_of_rule f_number f_empty t
;;

let string_of_trs_tpdb t = 
  "(RULES \n" ^ 
    (string_of_relation string_of_rule f_empty f_empty t) ^ ")"
;;

let string_of_theory t = 
  let t1,t2 =
    List.fold_left (fun (acc_fr,acc_st) ((s,t),f) -> 
                      if f = FreshEQ then ((s,t)::acc_fr),acc_st
                      else acc_fr,((s,t)::acc_st)) ([],[]) t
  in
  let s1 = string_of_relation string_of_identity f_fresh f_empty t1 in
  let s2 = string_of_relation string_of_identity f_stale f_empty t2 in
    if s1 <> "" then 
      if s2 <> "" then 
        s1 ^ "\n" ^ s2
      else s1
    else s2
;;

let string_of_trs t = 
  string_of_relation string_of_rule f_empty f_empty t
;;

let string_of_unifier sigma =
  Hashtbl.fold 
    (fun k d l -> 
       k ^ " -> " ^ (string_of_term d) ^ 
         (if l = "" then l else (", " ^ l))) 
    sigma ""
;;

let string_of_vars vars = 
  "[" ^ 
    (List.fold_left 
       (fun s (x,i) -> 
          (if s = "" then "" else (s ^ ",")) ^ (string_of_variable x i))
       "" vars) ^ "]"
;;

let string_of_vars_tpdb vars = 
  "(VAR " ^ 
    (List.fold_left 
       (fun s (x,i) -> 
          (if s = "" then "" else (s ^ " ")) ^ (string_of_variable x i))
       "" vars) ^ ")"
;;

let string_of_status = function
  | Axiom -> "axiom"
  | Hypothesis -> "hypothesis"
  | Conjecture -> "conjecture"
;;

let rec string_of_formula = function
  | Equal(t1, t2) -> "++equal(" ^ (string_of_term t1) ^ "," ^ 
      (string_of_term t2) ^ ")"
  | DisEqual(t1, t2) -> "--equal(" ^ (string_of_term t1) ^ "," ^ 
      (string_of_term t2) ^ ")"
;;

let string_of_clause = function
  | InputClause(s,t,[]) -> "input_clause(" ^ s ^ "," ^ 
      (string_of_status t) ^ ",\n\t[ ])."
  | InputClause(s,t,f::[]) -> "input_clause(" ^ s ^ "," ^ 
      (string_of_status t) ^ ",\n\t[ " ^ (string_of_formula f) ^ " ])."
  | InputClause(s,t,f::fl) -> "input_clause(" ^ s ^ "," ^ 
      (string_of_status t) ^ ",\n\t[ " ^ 
        (List.fold_left (fun s f -> s ^ ", " ^ (string_of_formula f)) 
           (string_of_formula f) fl) ^ " ])."
;;
