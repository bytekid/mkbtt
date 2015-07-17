(* Copyright 2008 Martin Korp, Christian Sternagel, Harald Zankl
 * GNU Lesser General Public License
 *
 * This file is part of TTT2.
 * 
 * TTT2 is free software: you can redistribute it and/or modify it under
 * the terms of the GNU Lesser General Public License as published by the
 * Free Software Foundation, either version 3 of the License, or (at your
 * option) any later version.
 * 
 * TTT2 is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public
 * License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public
 * License along with TTT2. If not, see <http://www.gnu.org/licenses/>.
 *)

(*** OPENS ********************************************************************)
open Util;;
open Rewritingx;;
open Logic.Operators;;

(*** MODULES ******************************************************************)
module AF = Filtering;;
module C = Coefficient;;
module Co = Complexity;;
module F = Format;;
module Fun = Function;;
module H = Hashtbl;;
module M = Rewritingx.Monad;;
module Number = Logic.Number;;
module Pos = Position;;
module Prec = Precedence;;
module Sig = Signature;;
module Var = Variable;;
module WF = Weights;;
module Mono = Monomial.Make (C) (Var);;
module P = Polynomial.Make (Mono);;

module NM = Matrix.Make (Number);;
module Intp = Interpretation.Make (NM);;

(*** TYPES ********************************************************************)
type goal = Prec | Weight;;

type context = {
 arith                 : Logic.arith;
 w0                    : Logic.a ref;
 solver                : Logic.solver;
 prec_num              : int;
 af_l                  : (Fun.t, Logic.p) H.t;
 af_p                  : (Fun.t * int, Logic.p) H.t;
 usable                : (Fun.t, Logic.p) H.t;
 (* weights               : (Fun.t, Logic.a) H.t; *)
 precs                 : (Fun.t, Logic.a) H.t;
 state                 : Signature.t;
 gt_encodings          : (Rule.t, Logic.p) H.t;
 eq_af_encodings       : (Fun.t*Fun.t*Term.t list*Term.t list, Logic.p) H.t;
 emb_gt_encodings      : (Rule.t, Logic.p) H.t;
 emb_eq_encodings      : (Rule.t, Logic.p) H.t;
 kbo_af_same_encodings : (Fun.t*Term.t list*Term.t list,Logic.p) H.t;
 kbo_af_diff_encodings : (Fun.t*Fun.t*Term.t list*Term.t list,Logic.p) H.t;
 pbc_gt                : (Fun.t*Fun.t, Logic.a) H.t;
 pbc_eq                : (Fun.t*Fun.t, Logic.a) H.t;
 pbc_no                : (Fun.t*Fun.t, Logic.a) H.t;
 pbc_kbo               : (Term.t*Term.t, Logic.a) H.t;
 pbc_kbo2              : (Term.t*Term.t, Logic.a) H.t;
 goal                  : goal option;
 subterm_encodings     : (Term.t,P.t) Hashtbl.t;
 coefficients          : (Fun.t*int,C.t) Hashtbl.t;
 constants             : (Fun.t,C.t) Hashtbl.t;
 p_gt_encodings        : (Rule.t,Logic.p) Hashtbl.t;
 p_geq_encodings       : (Rule.t,Logic.p) Hashtbl.t;
};;

type flags = {
 dp : bool ref;
 ep : bool ref;
 help : bool ref;
 min : int ref;
 minp : bool ref;
 minw : bool ref;
 ob : int ref;
 pbc : bool ref;
 quasi : bool ref;
 rat : int ref;
 sat : bool ref;
 ur : bool ref;
};;

type t = {
 af : AF.t option;
 input : Problem.t;
 output : Problem.t;
 precedence : Prec.t;
 usable_rules : Trs.t option;
 intp : Intp.t;
};;

(*** GLOBALS ******************************************************************)
let code = "tkbo";;
let name = "TKBO Processor";;
let comment = "Applies transfinite Knuth-Bendix order."
let keywords = ["transfinite knuth-bendix order";"simplification order";"termination"];;

let flags = {
 dp = ref false;
 ep = ref false;
 help = ref false;
 min = ref 3;
 minp = ref false;
 minw = ref false;
 ob = ref max_int;
 pbc = ref false;
 quasi = ref false;
 rat = ref 1;
 sat = ref true;
 ur = ref false;
};;

let spec =
 let spec = [
  ("-af",Arg.Set flags.dp,
   "Allows non-monotone interpretations (argument filterings). This flag \
    cannot be used in combination with `.pbc'.");
  ("-dp",Arg.Set flags.dp,
   "Equivalent to `-af'.");
  ("-ep",Arg.Set flags.ep,"Demands an empty precedence (only for `-pbc').");
  ("--help",Arg.Set flags.help,"Prints information about flags.");
  ("-help",Arg.Set flags.help,"Prints information about flags.");
  ("-h",Arg.Set flags.help,"Prints information about flags.");
  ("-ib",Arg.Int ((:=) flags.min <.> Int.bit_max),
   "Defines the number of bits used to represent weigths (same as \
   `-min' but in bits).");
  ("-max",Arg.Int ((:=) flags.ob <.> Int.bits),
   "Defines the maximum number that can appear as intermediate result.");
  ("-min",Arg.Set_int flags.min,
   "Defines the minimum weight that should be representable.");
  ("-minp",Arg.Set flags.minp,
   "Minimizes the precedence comparisons (only for `-pbc').");
  (* ("-minw",Arg.Set flags.minw, *)
   (* "Minimize the sum of weights (only for `-pbc')."); *)
  ("-ob",Arg.Set_int flags.ob,
   "Defines the number of bits used to represent intermediate results
    (same as `-max' but in bits)");
  ("-pbc",Arg.Set flags.pbc,"Uses PBC backend.");
  ("-quasi",Arg.Set flags.quasi,"Allows quasi-precedences.");
  ("-rat",Arg.Set_int flags.rat,
   "Sets the denumerator (only in combination with `-sat' or `-smt').");
  ("-sat",Arg.Set flags.sat,"Uses SAT backend (default).");
  ("-smt",Arg.Clear flags.sat,"Uses SMT backend.");
  ("-ur",Arg.Set flags.ur,
   "Uses usable rules with respect to argument filtering.")]
 in
 Arg.alignx 80 spec
;;

let help = (comment,keywords,List.map Triple.drop_snd spec);;

(*** MODULES (part 2) *********************************************************)
module Statex = struct type t = context end;;
module Made = Util.Monad.Transformer.State (Statex) (Logic.Monad);;
open Made;;

(*** FUNCTIONS ****************************************************************)
let init _ =
 flags.dp := false;
 flags.ep := false;
 flags.help := false;
 flags.min := 1;
 flags.minp := false;
 flags.minw := false;
 flags.ob := max_int;
 flags.pbc := false;
 flags.quasi := false;
 flags.rat := 1;
 flags.sat := true;
 flags.ur := false;
;;

(* Constructors and Destructors *)
let make af input output precedence ur intp = {
 af = af;
 input = input;
 output = output;
 precedence = precedence;
 usable_rules = ur;
 intp = intp;
};;

let get_ip p = p.input;;
let get_op p = p.output;;

(* Complexity Bounds *)
let complexity c _ = Co.mul c Co.other;;

(* Compare Functions *)
let equal p q =
 Problem.equal p.input q.input && Problem.equal p.output q.output
;;

(* Printers *)
let (>>=) = M.(>>=);;

let fprintf_af fmt = function
 | None -> Monad.return ()
 | Some af ->
  F.fprintf fmt "@\n@[<1>argument filtering:@\n";
  AF.fprintfm fmt af >>= fun _ -> Monad.return (F.fprintf fmt "@]")
;;

let fprintf_ur fmt = function
 | None -> Monad.return ()
 | Some trs ->
  F.fprintf fmt "@\n@[<1>usable rules:@\n";
  Trs.fprintfm fmt trs >>= fun _ -> Monad.return (F.fprintf fmt "@]")
;;

let fprintf fs fmt p  = 
 F.fprintf fmt "@[<1>%s:" name;
 fprintf_af fmt p.af >>= fun _ ->
 fprintf_ur fmt p.usable_rules >>= fun _ ->
 F.fprintf fmt "@\n@[<1>transfinite weight function:@\n";
 Intp.fprintf fmt p.intp >>= fun _ ->
 F.fprintf fmt "@]@\n@[<1>precedence:@\n";
 Prec.fprintfm fmt p.precedence >>= fun _ ->
 F.fprintf fmt "@]@\n@[<1>problem:@\n";
 Problem.fprintfm fmt p.output >>= fun _ ->
 F.fprintf fmt "@]@\n"; List.hd fs fmt >>= fun _ ->
 Monad.return (F.fprintf fmt "@]")
;;

let fprintfx _ _ = failwith "Tkbo.fprintfx: XML output not supported";;

(* general functions *)
let (>>=) = Made.(>>=);;

let rec symbols_list = function 
 | Term.Var _ -> []
 | Term.Fun (f,ts) -> f::List.flat_map symbols_list ts;;

let rec vars_list = function 
 | Term.Var x -> [x]
 | Term.Fun (_,ts) -> List.flat_map vars_list ts;;

let diff a b = List.foldr List.remove a b;;

(* functions lifted from Logic into Made *)
let fresh_arith_spec spec = liftm (Logic.fresh_arith spec);;
let fresh_arith = get >>= fun s -> liftm (Logic.fresh_arith s.arith);;
let fresh_bool = get >>= fun s -> liftm Logic.fresh_bool;;

let eval_a a ass = a >>= fun a -> liftm (Logic.eval_a a ass);;
let eval_p p ass = p >>= fun p -> liftm (Logic.eval_p p ass);;

let ($>=$) a b = lift2 (<>=>) a b;;
let ($=$) a b = lift2 (<=>) a b;;
let ($>$) a b = lift2 (<>>) a b;;
let ($->$) a b = lift2 (<->>) a b;;
let ($|$) a b = lift2 (<|>) a b;;
let ($&$) a b = lift2 (<&>) a b;;
let ($*$) a b = lift2 (<*>) a b;;
let ($+$) a b = lift2 (<+>) a b;;
let ($-$) a b = lift2 (<->) a b;;

let map_op op f ls = sequence (List.map f ls) >>= (return <.> op);;
let mapi_op op f ls = sequence (List.mapi f ls) >>= (return <.> op);;
let gen_op op f n = sequence (List.gen f n) >>= (return <.> op);;
let map_and f = map_op Logic.big_and f;;
let mapi_and f = mapi_op Logic.big_and f;;
let gen_and f = gen_op Logic.big_and f;;
let map_or f = map_op Logic.big_or f;;
let mapi_or f = mapi_op Logic.big_or f;;
let gen_or f = gen_op Logic.big_or f;;

(* actual content starts here *)
let context state fs =
 let arith = {
  Logic.min = Int64.of_int !(flags.min);
  neg = false;
  rat = !(flags.rat);
  real = false;
  minf = false}
 in
 let solver =
  if !(flags.sat) then Logic.MiniSat else
  if !(flags.pbc) then Logic.MiniSatP else Logic.Yices
 in
 let goal =
  if !(flags.minp) then Some Prec else
  if !(flags.minw) then Some Weight else None
 in
 {
  arith             = arith; 
  w0                = ref Logic.one;
  solver            = solver;
  prec_num          = max 0 (List.length fs - 1);
  af_l              = H.create 512;
  af_p              = H.create 512;
  usable            = H.create 512;
  (* weights           = H.create 512; *)
  precs             = H.create 512;
  state             = state;
  eq_af_encodings   = H.create 512;
  gt_encodings      = H.create 512;
  emb_gt_encodings  = H.create 512;
  emb_eq_encodings  = H.create 512;
  kbo_af_same_encodings = H.create 512;
  kbo_af_diff_encodings = H.create 512;
  pbc_gt            = H.create 512; 
  pbc_eq            = H.create 512; 
  pbc_no            = H.create 512; 
  pbc_kbo           = H.create 512; 
  pbc_kbo2          = H.create 512; 
  goal              = goal;
  subterm_encodings = H.create 512;
  coefficients      = H.create 512;
  constants         = H.create 512;
  p_gt_encodings      = H.create 512;
  p_geq_encodings     = H.create 512;
 }
;;

(* administrative functions *)
let cache_m tbl f k = 
 if H.mem tbl k then return (H.find tbl k)
 else (f k >>= fun v -> (H.add tbl k v; return v))
;;

(* KBO encoding starts here *)
let make_coefficient f _ = get >>= fun c ->
 fresh_arith_spec c.arith
;;

let make_constant f = get >>= fun c ->
 fresh_arith_spec c.arith
;;

let coefficient f i = get >>= fun c -> cache_m c.coefficients (uncurry make_coefficient) (f,i);;

let constant f = get >>= fun c -> cache_m c.constants make_constant f;;

(* let weight f = get >>= fun c -> cache_m c.weights (const fresh_arith)
f;; *)

let prec f = get >>= fun c -> 
 let arith = Logic.nat c.prec_num in
 cache_m c.precs (const (fresh_arith_spec arith)) f
;;

let prec_gt f g = prec f $>$ prec g;;
let prec_ge f g = prec f $>=$ prec g;;

let u t =
 get >>= fun c ->
 cache_m c.usable (const fresh_bool) (Option.the(Term.root t))
;;

let af_l f = get >>= fun c -> cache_m c.af_l (const fresh_bool) f;;
let af_n f = af_l f >>= (return <.> Logic.neg);;
let af_p f i = get >>= fun c -> cache_m c.af_p (const fresh_bool) (f,i);;

(* unary(u) \land w(u) = 0 \to u maximal element *)
let max u fs = get >>= fun c ->
 if !(flags.quasi) then map_and (prec_ge u) fs 
 else map_and (fun f -> if f = u then return Logic.top else prec_gt u f) fs
;;

let arity f = get >>= fun c -> return (Sig.find_ari f c.state);;
let n_ary n fs = Made.filter (fun f -> lift2 (=) (arity f) (return n)) fs;;
let w0 = get >>= fun c -> return !(c.w0);;

let adm fs = get >>= fun c -> 
 let adm_c fs = n_ary 0 fs >>= fun cs ->
  map_and (fun con -> constant con $>$ return C.zero) cs
 in
 let adm_u fs = n_ary 1 fs >>= fun us ->
  map_and (fun u -> (constant u $=$ return C.zero) $->$ (max u fs)) us
 in
 adm_c fs $&$ adm_u fs
;;


let rec encode_term t = get >>= fun c ->
 cache_m c.subterm_encodings (etc c) t
and etc c = function
 | Term.Var x -> return (P.make [Mono.make C.one [x]; Mono.make C.zero []])
 | Term.Fun (f, ts) ->
  (List.fold_lefti (fun i p t -> 
   lift2 P.add p (lift2 P.scale (coefficient f i) (encode_term t)))
  (lift P.make (sequence [lift2 Mono.make (constant f) (return [])]))
  ts)
;;

let greater_mono m0 m1 = C.gt (Mono.coefficient m0) (Mono.coefficient m1);;
let greater_equal_mono m0 m1 = C.geq (Mono.coefficient m0) (Mono.coefficient m1);;

let greater_equal_poly p0 p1 =
 let (v0,v1) = Pair.map P.variables (p0,p1) in
 if not (List.is_subset v1 v0) then 
   Logic.bot
 else
  let p0 = P.filter (fun m -> List.mem (Mono.variables m) v1) p0 in
  let cs = P.map2 greater_equal_mono p0 p1 in 
  Logic.big_and cs 
;; 

(*TODO HZ: improve comparison by (taking w_0 into account)*)
let greater_poly p0 p1 = 
 let c0 = P.constant_part p0 in
 let c1 = try P.constant_part p1 with Not_found -> 
  let m = Mono.coefficient c0 in
  Mono.make C.zero []
 in
 let (nc0, nc1) = Pair.map P.non_constant_part (p0,p1) in
 greater_mono c0 c1 <&> greater_equal_poly nc0 nc1
;;

let greater_equal_rule rule = 
 lift2 greater_equal_poly (encode_term (Rule.lhs rule))
  (encode_term (Rule.rhs rule))
;;

let greater_equal_rule_s rule = 
 let ncp = P.non_constant_part in
 encode_term (Rule.lhs rule) >>= fun l ->
 encode_term (Rule.rhs rule) >>= fun r ->
 return (greater_equal_poly (ncp l) (ncp r)) 
;;

let greater_equal_rule rule = get >>= fun c ->
 cache_m c.p_geq_encodings greater_equal_rule rule
;;

let greater_rule rule =
 lift2 greater_poly (encode_term (Rule.lhs rule))
  (encode_term (Rule.rhs rule))
;;

let greater_rule rule = get >>= fun c ->
 (* cache_m c.p_gt_encodings greater_rule rule *)
 greater_rule rule
;;

let mon_m m = return (C.geq m C.one);;

let coefficients f = arity f >>= fun a ->
 Made.sequence (List.gen (coefficient f) a)
;;

let matrices fs = Made.flat_map coefficients fs;;

let mon fs = matrices fs >>= fun ms -> map_and mon_m ms;;

let rec kbo_gt rule =
 let helper rule = 
  if (not (Rule.is_rewrite_rule rule)) || Rule.is_embedded (Rule.invert rule) then
   return Logic.bot
  else if Rule.is_proper_embedded rule then
   return Logic.top
  else (
   greater_rule rule >>= fun w1 ->
   greater_equal_rule rule >>= fun w2 -> 
   match Rule.to_terms rule with
    | Term.Var _, _ -> return Logic.bot 
    | Term.Fun _, Term.Var _ -> return Logic.top
     (* all dangerous cases are considered above*)
    | Term.Fun (f0, ts0), Term.Fun (f1, ts1) -> get >>= fun c ->
     (if f0 = f1 then begin
      let ts0', ts1' = List.remove_equal ts0 ts1 in
      if (ts0' = [] || ts1' = []) then return Logic.bot
      else kbo_gt (Rule.of_terms (List.hd ts0') (List.hd ts1'))
     end
     else begin (* different function symbols *)
      (if !(flags.quasi) then 
      let ts0', ts1' = List.remove_equal ts0 ts1 in
      (if ts0' = [] || ts1' = [] then return Logic.bot
       else kbo_gt (Rule.of_terms (List.hd ts0') (List.hd ts1'))) >>= fun aux ->
       prec_gt f0 f1 $|$ (prec_ge f0 f1 $&$ return aux)
      else prec_gt f0 f1) end) >>= fun aux ->
  return (w1 <|> (w2 <&> aux))
  ) in
  get >>= fun c -> cache_m c.gt_encodings helper rule
;;

let kbo_ge rule =
 if Rule.uncurry (=) rule then return Logic.top else kbo_gt rule
;;

(*dummies*)
let encode_gt = map_or kbo_gt
let encode_ge s w = map_and kbo_ge (s@w)
let af _ = return Logic.top

(*
(* encode with argument filtering *)
(* admissible weight function for argument filtering *)
let c_af f = arity f >>= fun a ->
 af_l f $&$ gen_and (fun i -> lift Logic.neg (af_p f i)) a
;;

let af_collapsing f a i =
 let make j = if i = j then af_p f i else (lift Logic.neg (af_p f j)) in
 gen_and make a
;;

let u_af f = arity f >>= fun a -> af_l f $&$ gen_or (af_collapsing f a) a;;

let adm_af_f fs f = 
 (c_af f $->$ (weight f $>=$ w0)) $&$
 ((u_af f $&$ (weight f $=$ return C.zero)) $->$
  map_and (fun g -> (af_l g) $->$ (prec_ge f g)) fs)
;;

let adm_af fs = (w0 $>$ (return C.zero)) $&$ map_and (adm_af_f fs) fs;;

let weight_list t =
 let rec wt acc = function
  | Term.Var _ -> w0 >>= fun w0 -> return [Logic.ite acc w0 C.zero]
  | Term.Fun (f, ts) ->
   weight f >>= fun wf ->
   af_l f >>= fun fl ->
   sequence (List.mapi (fun i ti -> af_p f i >>= fun pi ->
    wt (pi <&> acc) ti) ts) >>= fun deep ->
    return (Logic.ite (fl <&> acc) wf C.zero::(List.concat deep))
in
 wt Logic.top t
;;

let weight_rule rule = 
 let l, r = Rule.to_terms rule in
 weight_list l >>= fun l ->
 weight_list r >>= fun r ->
 return (C.big_sum (diff l r),C.big_sum (diff r l))
;;

(* useful shorthands. *)
let exists f p ts = mapi_or (fun i ti -> (af_p f i) $&$ (p ti)) ts;;
let for_all f p ts = mapi_and (fun i ti -> (af_p f i) $->$ (p ti)) ts;;
let exists2 f p ss ts = exists f p (List.map2 Rule.of_terms ss ts);;
let for_all2 f p ss ts = for_all f p (List.map2 Rule.of_terms ss ts);;

let lex f eq gt ss ts =
 let rec lex' f eq gt ss ts i =
  match ss, ts with
   | [],[] -> return Logic.bot
   | [], _ | _, [] -> failwith "lists of different length"
   | sn::sss,tn::tss -> 
     ((af_p f i) $&$ (gt (Rule.of_terms sn tn))) 
     $|$
     (((af_p f i) $->$ (eq (Rule.of_terms sn tn)))
      $&$
      (lex' f eq gt sss tss (i+1)))
 in
 lex' f eq gt ss ts 0 
;;

(* equality, embedding *)
let eq_af_diff eq f g ss ts =
 let helper eq (f,g,ss,ts) =
 let s = Term.Fun (f, ss) and t = Term.Fun (g, ts) in
  (af_n f $&$ exists f (fun si -> eq (Rule.of_terms si t)) ss) $|$ 
   (af_n g $&$ exists g (fun tj -> eq (Rule.of_terms s tj)) ts)
 in get >>= fun c -> cache_m c.eq_af_encodings (helper eq) (f,g,ss,ts)
;;

let rec eq_af rule = 
 let helper r =
 let (s,t) = Rule.to_terms rule in match (s,t) with
  | _, _ when s = t -> return Logic.top
  | Term.Var x, Term.Var y (* then, x <> y holds *) -> return Logic.bot
  | Term.Var _, Term.Fun (g, ts) -> 
   af_n g $&$ exists g (fun tj -> eq_af (Rule.of_terms s tj)) ts
  | Term.Fun (f, ss), Term.Var _ -> 
  (*HZ added af_n f*)
   af_n f $&$ exists f (fun si -> eq_af (Rule.of_terms si t)) ss
  | Term.Fun (f, ss), Term.Fun (g, ts) when f = g ->
   for_all2 f eq_af ss ts (*equality holds when AF(f) is added*)
  | Term.Fun (f, ss), Term.Fun (g, ts) ->
   eq_af_diff eq_af f g ss ts
 in get >>= fun c -> cache_m c.emb_eq_encodings helper rule
;;

let rec emb_af_ge rule = emb_af_gt rule $|$ eq_af rule
and emb_af_gt rule =
 let helper rule =
 let s, t = Rule.to_terms rule in match s, t with
  | Term.Var _, _ -> return Logic.bot
  | Term.Fun (f, ss), Term.Var _ -> 
    exists f (fun si -> emb_af_gt (Rule.of_terms si t)) ss $|$
    (af_l f $&$ exists f (fun si -> eq_af (Rule.of_terms si t)) ss)
  | Term.Fun (f, ss), Term.Fun (g, ts) when f = g ->
    (af_l f $&$ exists f (fun si -> emb_af_ge (Rule.of_terms si t)) ss) $|$
     (((af_l f $&$ for_all2 f emb_af_ge ss ts) $|$ af_n f)
     $&$ exists2 f emb_af_gt ss ts)
  | Term.Fun (f, ss), Term.Fun (g, ts) ->
    (af_l f $&$
     ((af_l g $&$ exists f (fun si -> eq_af (Rule.of_terms si t)) ss) $|$
      (af_n g $&$ exists g (fun tj -> emb_af_gt (Rule.of_terms s tj)) ts)))
    $|$
    ((af_n f $|$ af_l g) $&$ 
     exists f (fun si -> emb_af_gt (Rule.of_terms si t)) ss)
 in get >>= fun c -> cache_m c.emb_gt_encodings helper rule
 (*in helper rule *)
;;

let dup_x x t =
 let rec dup_x x acc = function
 | Term.Var y -> 
  return (if x = y then Logic.ite acc C.one C.zero else C.zero)
 | Term.Fun (f, ts) ->
  sequence (List.mapi (fun i t -> af_p f i >>= fun pi ->
   (dup_x x (acc <&> pi) t)) ts)
  >>= (return <.> Logic.big_sum)
 in
 dup_x x Logic.top t
;;

let dup rule = 
 match Rule.lhs rule, Rule.rhs rule with
  | Term.Var _, _ -> return Logic.bot
  (*
  FIXME: WAS THIS COMPLETE? (YES, due to KBO test)
  | Term.Fun _, Term.Var _ -> 
   return (if Rule.is_embedded rule then Logic.top else Logic.bot)
  *)
  | s, t -> (* both have function symbol as root *)
   map_and (fun x -> (dup_x x s) $>=$ (dup_x x t)) (Term.vars t)
;;

let kbo_af_same eq gt f0 ts0 ts1 =
 let helper (eq,gt) (f0,ts0,ts1) = 
  (af_l f0 $&$ lex f0 eq gt ts0 ts1) $|$ (af_n f0 $&$ exists2 f0 gt ts0 ts1)
 in get >>= fun c -> cache_m c.kbo_af_same_encodings (helper (eq,gt))
 (f0,ts0,ts1)
;;

let kbo_af_diff gt f0 f1 ts0 ts1 =
 let helper gt (f0,f1,ts0,ts1)=
 let s = Term.Fun (f0, ts0) and t = Term.Fun (f1, ts1) in
 (af_l f0 $&$ af_l f1 $&$ prec_gt f0 f1) $|$
 (*HZ question wasn't af_n f1 missing? *)
 (af_n f1 $&$ exists f1 (fun tj -> gt (Rule.of_terms s tj)) ts1) $|$
 (af_n f0 $&$ exists f0 (fun si -> gt (Rule.of_terms si t)) ts0)
 in get >>= fun c -> cache_m c.kbo_af_diff_encodings (helper gt) (f0,f1,ts0,ts1)
;;

let rec kbo_af_gt rule = 
 let rec helper rule = match Rule.to_terms rule with
  | Term.Var _, _ -> return Logic.bot
  | Term.Fun _, Term.Var _ -> emb_af_gt rule
  | _, _ -> (* both terms start with function symbol *)
  weight_rule rule >>= fun (wl, wr) ->
  dup rule $&$ 
   ((return (wl <>> wr)) $|$ ((return (wl <=> wr)) $&$ (kbo_af_gt' rule)))
and kbo_af_gt' rule =
 match Rule.to_terms rule with
  | Term.Var _, _ (* fallthrough *)
  | Term.Fun _, Term.Var _ -> 
   return Logic.bot (*failwith ".kbo_af_gt': case shall not happen"*)
  | Term.Fun (f0, ts0), Term.Fun (f1, ts1) ->
   if f0 = f1 then kbo_af_same eq_af kbo_af_gt f0 ts0 ts1
   else kbo_af_diff kbo_af_gt f0 f1 ts0 ts1
 in get >>= fun c -> cache_m c.gt_encodings helper rule
;;

let kbo_af_ge rule = kbo_af_gt rule $|$ eq_af rule;;

(* encoding *)
let kbo_gt rule =
 get >>= fun c -> if !(flags.dp) then kbo_af_gt rule else kbo_gt rule
;;
 
let kbo_ge rule =
 get >>= fun c -> if !(flags.dp) then kbo_af_ge rule else kbo_ge rule
;;

(* stuff for usable rules w.r.t. interpretation similar to GI03_01 *)
let rec process_rule df rule =
 let (l,r)  = Rule.to_terms rule in
 let dp = List.flat_map (flip Term.fun_pos r) df in
 let rec conj_of_smaller_pos p r =
  if p = Pos.root then return Logic.top else
  let q, i = Pos.split_last p in
  ((af_p (Option.the(Term.root (Term.subterm q r))) i)
    $&$ (conj_of_smaller_pos q r))
 in
 u l $->$ 
  map_and (fun p -> (conj_of_smaller_pos p r) $->$ (u (Term.subterm p r))) dp
;;

let ge_ur op ge rule = lift2 op (u(Rule.lhs rule)) (kbo_ge rule);;
let encode_gt s = map_or kbo_gt s;;

let encode_ge s w = get >>= fun c ->
 if !(flags.ur) then (
  let ds = Trs.def_symbols (Trs.of_list (s@w)) in
  map_and (ge_ur (<&>) kbo_ge) s $&$
  map_and (ge_ur (<->>) kbo_ge) w $&$
  map_and (process_rule ds) (s@w)
 ) else 
 map_and kbo_ge (s@w)
;;

let adm fs = get >>= fun c -> if !(flags.dp) then adm_af fs else adm fs;;
let af_f f = arity f >>= fun a -> af_l f $|$ gen_or (af_collapsing f a) a;;
let af fs = map_and af_f fs;;
let af fs = get >>= fun c -> if !(flags.dp) then af fs else return Logic.top;;

*)

let encode s w = get >>= fun c ->
 fresh_arith >>= fun w0 -> c.w0 := w0;
 let fs = (Trs.funs (Trs.union s w)) in
 let s, w = Pair.map Trs.to_list (s, w) in
 encode_gt s $&$
 encode_ge s w $&$
 adm fs $&$
 af fs $&$
 mon fs
;;

(* decode from assignment *)
let decode_rule ass rule = get >>= fun c ->
  lift not (eval_p (kbo_gt rule) ass)
;;

let decode_trs ass trs = 
 Made.filter (decode_rule ass) (Trs.to_list trs) >>= (return <.> Trs.of_list)
;;

let decode ass s w = get >>= fun c -> 
 lift2 Pair.make
  (decode_trs ass s) (if !(flags.dp) then return w else decode_trs ass w)
;;

let decode_ur_rule ass rule = eval_p (u (Rule.lhs rule)) ass;;

let decode_trs_ur ass w =
 lift Trs.of_list (Made.filter (decode_ur_rule ass) (Trs.to_list w))
;;

let decode_ur ass w = get >>= fun c ->
 if !(flags.ur) then (decode_trs_ur ass w >>= (return <.> Option.some))
 else (return None)
;;

let eval_matrix ass m = m >>= fun m -> 
 liftm (Logic.eval_a m ass) >>= fun c ->
 return (NM.make 1 1 c)
;;

let get_interpretation ass f = arity f >>= fun a ->
 Made.sequence 
  (List.gen (fun i -> eval_matrix ass (coefficient f i)) a) >>= fun coeffs ->
 eval_matrix ass (constant f) >>= fun const ->
 return (f, coeffs, const);;

let decode_intp ass trs = 
 Made.sequence (List.map (get_interpretation ass) (Trs.funs trs)) >>=
  (return <.> List.foldr (fun fi acc -> uncurry3 (Intp.add acc) fi)
   (Intp.empty ()))
;;

let decode_prec_f ass f =
 eval_a (prec f) ass >>= fun p -> 
 return (f,int_of_string (Number.to_string p))
;;

let decode_prec ass s w =
 let fs = Trs.funs (Trs.union s w) in
 Made.sequence (List.map (decode_prec_f ass) fs) >>= fun ps ->
 return (Prec.of_list ps)
;;

let decode_af_f ass f =
 arity f >>= fun a ->
 sequence (List.gen (fun i -> eval_p (af_p f i) ass) a) >>= fun ps -> 
 let psi = List.mapi Pair.make ps in
 let ps = List.filter snd psi in
 let ps = List.map fst ps in
 eval_p (af_l f) ass >>= fun l ->
 if l then return (f,AF.List ps)
 else (assert (List.length ps = 1); return (f,AF.Collapsing (List.hd ps)))
;;

let decode_af ass s w = get >>= fun c ->
 if !(flags.dp) then (
 let fs = Trs.funs (Trs.union s w) in
 Made.sequence (List.map (decode_af_f ass) fs) >>= fun af ->
 return (Some (AF.of_list af))
 ) else return None
;;


let solve signature fs p = 
 let configurate s = F.printf "%s@\n%!" s; flags.help := true in
 (try init (); Arg.parsex code spec fs with Arg.Bad s -> configurate s);
 if !(flags.help) then (Arg.usage spec ("Options for "^code^":"); exit 0);
 if !(flags.minp) && !(flags.minw) then
  failwith "kbo: options -minp and -minw are exclusive";
 let (s,w) = Problem.get_sw p in
 let c = context signature (Trs.funs (Trs.union s w)) in
 Logic.run ~obits:!(flags.ob) (
  Made.run c (encode s w >>= fun phi -> 
  Made.liftm (Logic.solve ~solver:c.solver ~goal:None phi) >>= function
   | None -> return None
   | Some ass ->
    decode ass s w >>= fun (s',w') -> 
    decode_af ass s w >>= fun af ->
    decode_intp ass (Trs.union s w) >>= fun intp ->
    decode_prec ass s w >>= fun prec -> 
    decode_ur ass w >>= fun ur ->
    return (Some (make af p (Problem.set_sw s' w' p) prec ur intp))))
;;

(* wrap into state monad *)
let (>>=) = M.(>>=);;
let solve fs p = M.get >>= fun s -> M.return (solve s fs p);;
