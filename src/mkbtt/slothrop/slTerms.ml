open SlDebug
open SlHeap

(*** The basics of term rewriting: terms, unification, matching, normalization

ML Programs from Chapter 4 of

  Term Rewriting and All That
  by Franz Baader and Tobias Nipkow,
  (Cambridge University Press, 1998)

Copyright (C) 1998 by Cambridge University Press.
Permission to use without fee is granted provided that this copyright
notice is included in any copy.
***)

type vname = string * int;;

type term = V of vname | T of string * term list;;

type order = GR | EQ | NGE;;

type ids = (term * term) list;;

type eq_status = FreshEQ | StaleEQ;;

(* maxs: int list -> int *)
let rec maxs = function
    (i::is) -> max i (maxs is)
  | []      -> 0;;

(* size: term -> int *)
let rec size = function
    (V _) -> 1
  | (T(_,ts)) -> sizes ts + 1
and sizes = function
    [] -> 0
  | (t::ts) -> size t + sizes ts;;

(* indom: vname -> subst -> bool *)
let indom x s = Hashtbl.mem s x;;

(* app: subst -> vname -> term *)
let rec app s x  = Hashtbl.find s x;;

(* lift: subst -> term -> term *)
let rec lift s = function
    (V x) -> if indom x s then app s x else V x
  | (T(f,ts)) -> T(f, List.map (lift s) ts);;

(* occurs: vname -> term -> bool *)
let rec occurs x = function 
    (V y) -> x=y
  | (T(_,ts)) -> List.exists (occurs x) ts
;;

let rec subterm t1 t =
  if t1 = t then true 
  else match t with 
      V x -> false
    | T(f,ts) -> List.exists (subterm t1) ts
;;

(* solve: (term * term)list * subst -> subst *)
let solve ts = 
  let rec solve_h s = function
      [] -> true
    | (V x, t) :: _S ->
        if V x = t then solve_h s _S else elim s (x,t,_S)
    | (t, V x) :: _S -> elim s (x,t,_S)
    | (T(f,ts),T(g,us)) :: _S ->
        if f = g then solve_h s ((List.combine ts us) @ _S) else false

  (* elim: vname * term * (term * term) list * subst -> subst *)
  and elim s (x,t,_S) =
    if occurs x t then false
    else 
      let s' = Hashtbl.create 1 in
      let _ = Hashtbl.add s' x t in
      let xt = lift s' in 
        solve_h (Hashtbl.add s x t; 
               Hashtbl.iter (fun y u -> Hashtbl.replace s y (xt u)) s; s)
          (List.map (fun (t1,t2) -> (xt t1, xt t2)) _S)
  in
  let s = Hashtbl.create 1 in
    if solve_h s ts then
      Some s
    else
      None
;;

(* unify: term * term -> subst *)
let unify(t1,t2) = solve([(t1,t2)]);;

(* matchs: (term * term) list * subst -> subst *)
let matchs ts = 
  let rec matchs_h s = function
      [] -> true
    | (V x, t) :: _S ->
        if indom x s then if app s x = t then matchs_h s _S else false
        else 
          (Hashtbl.add s x t;
           matchs_h s _S)
    | (t, V x) :: _S -> false
    | (T(f,ts),T(g,us)) :: _S ->
        if f = g then 
          matchs_h s ((List.combine ts us) @ _S)
        else 
          false
  in
  let s = Hashtbl.create 1 in
    if matchs_h s ts then 
      Some s
    else 
      None
;;


(* match_with: term * term -> subst *)
let match_with(pat,obj) = matchs [(pat,obj)];;

let try_match(pat,obj) = 
  match match_with (pat,obj) with 
      Some _ -> true
    | None -> false
;;

let generalization pat obj = 
  try_match(pat,obj)
;;

let rule_generalization (patl,patr) (objl,objr) = 
  match matchs [(patl,objl);(patr,objr)] with
      Some _ -> true
    | None -> false
;;

let instance obj pat = 
  try_match(pat,obj)
;;

let rule_instance (objl,objr) (patl,patr) = 
  match matchs [(patl,objl);(patr,objr)] with
      Some _ -> true
    | None -> false
;;

let find_all_generalizations tbl (l,r) = 
  Hashtbl.fold 
    (fun (s,t) _ is -> 
       if rule_generalization (s,t) (l,r) then
         (s,t)::is
       else 
         is
    ) tbl []
;;

let find_all_instances tbl (l,r) = 
  Hashtbl.fold 
    (fun (s,t) _ is -> 
       if rule_instance (s,t) (l,r) then
         (s,t)::is
       else 
         is
    ) tbl []
;;

let rec try_match_subterm(pat,obj) = 
  match obj with 
      V x -> try_match (pat,obj)
    | T(f,ts) -> 
        try_match(pat,obj) || List.exists (fun o -> try_match_subterm(pat,o)) ts
;;
        
let rec count_matches(pat,obj) = 
  let one_match p o = if try_match (p,o) then 1 else 0 in
    match obj with 
        V x -> one_match pat obj
      | T(f,ts) -> 
          let start = one_match pat obj in
            List.fold_left 
              (fun i o -> i + (count_matches(pat,o))) 
              start ts
;;

let count_matches_against(pat,rules) =
  let rec cma pat rules acc = 
    match rules with 
        (l,r)::rs -> 
          let acc' = acc + (count_matches(pat,l)) + (count_matches(pat,r)) in
            cma pat rs acc'
      | [] -> acc
  in
    cma pat rules 0
;;
        

let find_syms_all xs = 
  let syms = Hashtbl.create 1 in
  let rec find_syms_h = function
    | T(f,xs) -> 
        Hashtbl.replace syms f (); 
        List.iter (fun t -> find_syms_h t) xs
    | _ -> ()
  in
    List.iter find_syms_h xs;
    Hashtbl.fold (fun k d l -> k::l) syms []
;;

(* list of symbols occuring in a single term *)
let find_syms s = find_syms_all [s];;


(* list of vars occuring in a list of terms *)
let find_vars_all xs =
  let vars = Hashtbl.create 1 in
  let rec find_vars_h = function
      V(x,i) when not (Hashtbl.mem vars (x,i)) ->
        Hashtbl.add vars (x,i) ()
    | T(f,xs) -> 
        List.iter (fun t -> find_vars_h t) xs
    | _ -> ()
  in
    List.iter find_vars_h xs;
    Hashtbl.fold (fun k d l -> k::l) vars []
;;

(* list of vars occuring in a single term *)
let find_vars s = find_vars_all [s];;

let subset_vars s t = 
  let rec subset _vs vt =
    match _vs with
      v::vs ->
        if List.exists (fun x -> x = v) vt then
          subset vs vt
        else
          false
    | [] -> true
  in
    subset (find_vars s) (find_vars t)
;;

let rec count_occurs sym = function
    V _ -> 0
  | T(s,ts) when s = sym -> 
      List.fold_left (fun a b -> a + count_occurs sym b) 1 ts
  | T(_,ts) -> 
      List.fold_left (fun a b -> a + count_occurs sym b) 0 ts
;;

let _canon subst i t =
  let v = "X" in
  let rec ttn = function
      V(x,n) -> 
        if Hashtbl.mem subst (x,n) then
          V(v,(Hashtbl.find subst (x,n)))
        else begin
          incr i; Hashtbl.add subst (x,n) !i; V(v,!i)
        end
    | T(f,ts) -> 
        T(f,(List.map ttn ts))
  in
    ttn t
;;

let canonicalize t = 
  _canon (Hashtbl.create 1) (ref (-1)) t
;;
  
let canon_rule (l,r) = 
  let s = Hashtbl.create 1 in
  let i = ref (-1) in
    _canon s i l, _canon s i r
;;
        
    
module Rule_sig = struct
  type t = term * term
  let compare = Pervasives.compare
end;;

module TRS = Set.Make(Rule_sig);;
module RuleSet = Set.Make(Rule_sig);;

let trs_of_rules rules =
  List.fold_left (fun s r -> TRS.add r s) TRS.empty rules
;;

let rules_of_trs set =
  TRS.elements set
;;

module TRS_sig = struct
  type t = TRS.t
  let compare = TRS.compare
end;;

module TRSSet = Set.Make(TRS_sig);;
    
module RuleHeap = Imperative(
  struct 
    type t = (term * term) * int
    let compare = fun ((l1,r1),i1) ((l2,r2),i2) -> i2 - i1
  end
);; 

let skolem_constant var_name var_num = 
  T("S_" ^ var_name ^ (string_of_int var_num), [])
;;

let rec skolemize = function
    V(x,i) -> skolem_constant x i
  | T(f,ts) -> T(f, List.map skolemize ts)
;;

