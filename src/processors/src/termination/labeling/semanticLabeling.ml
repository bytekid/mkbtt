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

(*** MODULES (part 1) *********************************************************)
module AF = Filtering;;
module C = Coefficient;;
module Co = Complexity;;
module F = Format;;
module Fun = Function;;
module H = Hashtbl;;
module Number = Logic.Number;;
module NM = Matrix.Make (Number);;
module Intp = Interpretation.Make (NM);;
module Monad = Rewritingx.Monad;;
module NMono = Monomial.Make (NM) (String);;
module NP = Polynomial.Make (NMono);;
module Prec = Precedence;;
module Pos = Position;;
module Sig = Signature;;
module Var = Variable;;

module M = struct 
 include Matrix.Make (C);;

 let copy = id;;
 let combine = map2 Logic.max;;
 let compare = compare;;
end

module Mono = Monomial.Make (M) (Var);;
module P = Polynomial.Make (Mono);;
module Pdx = struct
 include P;;
 let hash = Hashtbl.hash;;
 let copy = id;;
 let compare = Pervasives.compare;;
end
module Idx = Index.Standard (Pdx);;

(*** TYPES ********************************************************************)
type t = {
 weakly_monotone : bool;
 dimension : int; 
 interpretation : Intp.t;
 labeled : Fun.t list;
 usablem  : Fun.t list;
 af : AF.t option;
 prec : Prec.t;
 input : Problem.t;
 output :Problem.t;
 usable_rules : Trs.t option;
};;

type kind = Semantic | Predictive;;

type flags = {
 db : int ref;
 dim : int ref;
 dp : bool ref;
 help : bool ref;
 min : int ref;
 ob : int ref;
 rat : int ref;
 real : bool ref;
 triangle : bool ref;
 ur : bool ref;
 kind : kind ref;
 right : bool ref; (* use only rhss to compute usable rules for labeling *)
};;

type context = {
 arith              : Logic.arith;
 usable             : (Fun.t,Logic.p) Hashtbl.t;
 coefficients       : (Fun.t * int, M.t) Hashtbl.t;
 constants          : (Fun.t,M.t) Hashtbl.t;
 (* gt_encodings       : (Rule.t,Logic.p) Hashtbl.t; *)
 geq_encodings      : (Rule.t,Logic.p) Hashtbl.t;
 state              : Sig.t ref;
 subterm_encodings  : (Term.t,P.t) Hashtbl.t;
 label_encodings    : (Term.t,P.t) Hashtbl.t;
 d_tbl              : (Rule.t * Rule.t * int,M.t) Hashtbl.t; (* distances *)
 constant_encodings : (M.t,M.t) Hashtbl.t; (* experimental *)
 lab                : Idx.t ref;
 (* LPO stuff *)
 prec_num           : int;
 precs              : (Fun.t, Logic.a*Logic.a) H.t;
 af_l                  : (Fun.t, Logic.p) H.t;
 af_p                  : (Fun.t * int, Logic.p) H.t;
 lpo_gt_encodings          : (Rule.t, Logic.p) H.t;
 eq_af_encodings       : (Fun.t*Fun.t*Term.t list*Term.t list, Logic.p) H.t;
 emb_gt_encodings      : (Rule.t, Logic.p) H.t;
 emb_eq_encodings      : (Rule.t, Logic.p) H.t;
 lpo_af_same_encodings : (Fun.t*Term.t list*Term.t list,Logic.p) H.t;
 lpo_af_diff_encodings : (Fun.t*Fun.t*Term.t list*Term.t list,Logic.p) H.t;
 (* labeling stuff *)
 labeling           : kind;
 lab_coefficients   : (Fun.t * int, M.t) Hashtbl.t;
 lab_constants      : (Fun.t,M.t) Hashtbl.t;
 (* indicates if [f] is labeled *)
 lab_tbl            : (Fun.t,Logic.p) Hashtbl.t;
 (* indicates if [f] is usable (for labeling) *)
 um_tbl             : (Fun.t,Logic.p) Hashtbl.t;
 (* use only rhss when computing usable rules for labeling *)
 lright             : bool;
};;

(*** GLOBALS ******************************************************************)
let code = "sl";;
let name = "Semantic Labeling Processor";;
let comment = "Applies semantic labeling."
let keywords = ["semantic";"predictive";"labeling";"labelling";"termination"];;

let flags = {
 db = ref max_int;
 dim = ref 1;
 dp = ref false;
 help = ref false;
 min = ref 1;
 ob = ref max_int;
 rat = ref 1;
 real = ref false;
 triangle = ref false;
 ur = ref false;
 kind = ref Semantic;
 right = ref false;
};;

let spec =
 let spec = [
  ("-dim",Arg.Set_int flags.dim,"Specifies the dimension of the matrices.");
  ("-dp",Arg.Set flags.dp,
   "Allows non-monotone interpretations, i.e., `0' as coefficient.");
  ("--help",Arg.Set flags.help,"Prints information about flags.");
  ("-help",Arg.Set flags.help,"Prints information about flags.");
  ("-h",Arg.Set flags.help,"Prints information about flags.");
  ("-ib",Arg.Int ((:=) flags.min <.> Int.bit_max),
   "Defines the number of bits used to represent matrix entries (same as \
   `-min' but in bits).");
  ("-max",Arg.Int ((:=) flags.ob <.> Int.bits),
   "Defines the maximum number that can appear as intermediate result.");
  ("-min",Arg.Set_int flags.min,
   "Defines the minimum matrix entry that should be representable.");
  ("-ob",Arg.Set_int flags.ob,
   "Defines the number of bits used to represent intermediate results \
    (same as `-max' but in bits)");
  ("-pl",Arg.Unit (fun _ -> flags.kind := Predictive),
   "Use predictive labeling.");
  ("-sl",Arg.Unit (fun _ -> flags.kind := Semantic),
   "Use semantic labeling (default).");
  ("-right",Arg.Set flags.right, 
   "Use only rhss when computing usable rules for labeling");
  ("-ur",Arg.Set flags.ur,"Uses usable rules with respect to interpretation.")]
 in
 Arg.alignx 80 spec
;;

let help = (comment,keywords,List.map Triple.drop_snd spec);;

(*** MODULES (part 2) *********************************************************)
module Statex = struct type t = context end;;
module Made = Util.Monad.Transformer.State(Statex)(Logic.Monad);;
open Made;;

(*** FUNCTIONS ****************************************************************)
let init _ = 
 flags.db := max_int;
 flags.dim := 1;
 flags.dp := false;
 flags.help := false;
 flags.min := 1;
 flags.ob := max_int;
 flags.rat := 1;
 flags.real := false;
 flags.triangle := false;
 flags.ur := false;
 flags.kind := Semantic;
 flags.right := false;
;;

(* Constructors and Destructors *)
let make wm dim i l u af prec input output ur = {
 weakly_monotone = wm;
 dimension = dim;
 interpretation = i;
 labeled = l;
 usablem = u;
 af = af;
 prec = prec;
 input = input;
 output = output;
 usable_rules = ur;
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
let (>>=) = Monad.(>>=);;

let rec interpret_term d i = function
 | Term.Var x ->
  Monad.to_string_var x >>= fun x ->
  Monad.return (NP.make [NMono.make (NM.identity d) [x]])
 | Term.Fun (f,ts) ->
  let (cs,c) = Intp.get i f in
  List.foldr2 (fun ci ti ->
   Monad.lift2 NP.add (Monad.lift (NP.scale ci) (interpret_term d i ti)))
   (Monad.return (NP.make [NMono.make c []])) cs ts
;;

let interpret_term d i t =
 interpret_term d i t >>= fun p ->
 let ms = NP.map (fun m ->
  (NMono.coefficient m,List.join id "*" (NMono.variables m)))
  (NP.non_constant_part p)
 in
 let c =
  try NMono.coefficient (NP.constant_part p)
  with Not_found -> NM.make d 1 Number.zero
 in
 let (cs,xs) = List.split ms in Monad.return (cs,xs,c)
;;

let fprintf_orient d i fmt p =
 let fprintf_rule d i fmt rule =
  interpret_term d i (Rule.lhs rule) >>= fun l ->
  interpret_term d i (Rule.rhs rule) >>= fun r ->
  Term.to_stringm (Rule.lhs rule) >>= fun ls ->
  Term.to_stringm (Rule.rhs rule) >>= fun rs ->
  F.fprintf fmt "@[%a@]" NM.fprintf_rule (ls^" = ",l," >= ",r," = "^rs);
  Monad.return ()
 in
 let (s,w) = Problem.get_sw p in
 Monad.fprintf (fprintf_rule d i) "@\n@\n" fmt (Trs.to_list (Trs.union s w))
;;

let fprintf_ur fmt = function
 | None -> Monad.return ()
 | Some trs ->
  F.fprintf fmt "@\n@[<1>usable rules:@\n";
  Trs.fprintfm fmt trs >>= fun _ -> Monad.return (F.fprintf fmt "@]")
;;

let fprintf_af fmt = function
 | None -> Monad.return ()
 | Some trs ->
  F.fprintf fmt "@\n@[<1>argument filtering:@\n";
  AF.fprintfm fmt trs >>= fun _ -> Monad.return (F.fprintf fmt "@]")
;;

let fprintf_funs fmt s fs =
 F.fprintf fmt "@\n@[<1>%s:" s;
 let fprintf f = F.fprintf fmt "@\n"; Monad.fprintf_fun fmt f in
 Monad.iter fprintf fs >>= fun _ -> Monad.return (F.fprintf fmt "@]") 
;;

let fprintf fs fmt p  = 
 F.fprintf fmt "@[<1>%s:@\ndimension: %i" name p.dimension;
 fprintf_ur fmt p.usable_rules >>= fun _ ->
 F.fprintf fmt "@\n@[<1>interpretation:@\n";
 Intp.fprintf fmt p.interpretation >>= fun _ ->
 fprintf_funs fmt "labeled" p.labeled >>= fun _ ->
 fprintf_funs fmt "usable (for model)" p.usablem >>= fun _ ->
 fprintf_af fmt p.af >>= fun _ ->
 F.fprintf fmt "@]@\n@[<1>precedence:@\n";
 Prec.fprintfm fmt p.prec >>= fun _ ->
 F.fprintf fmt "@]@\n@[<1>problem:@\n";
 Problem.fprintfm fmt p.output >>= fun _ ->
 F.fprintf fmt "@]@\n"; List.hd fs fmt >>= fun _ ->
 Monad.return (F.fprintf fmt "@]")
;;

(* Processor *)
(* administrative functions *)
let (>>=) = Made.(>>=);;

let context state problem =
 let (s,w) = Problem.get_sw problem in
 let fs = Trs.funs (Trs.union s w) in
 let arith = {
  Logic.min = Int64.of_int !(flags.min);
  neg       = false;
  rat       = !(flags.rat);
  real      = !(flags.real);
  minf      = false}
 in {
  arith              = arith;
  usable             = Hashtbl.create 512;
  coefficients       = Hashtbl.create 512;
  constants          = Hashtbl.create 512;
  (*gt_encodings       = Hashtbl.create 512; *)
  geq_encodings      = Hashtbl.create 512;
  state              = ref state;
  subterm_encodings  = Hashtbl.create 512;
  label_encodings    = Hashtbl.create 512;
  d_tbl              = Hashtbl.create 512;
  constant_encodings = Hashtbl.create 512;
  lab                = ref (Idx.empty 512);
  (*LPO stuff*)
  prec_num           = max 0 (List.length fs - 1);
  precs              = H.create 512;
  af_l               = H.create 512;
  af_p               = H.create 512;
  eq_af_encodings   = H.create 512;
  lpo_gt_encodings      = H.create 512;
  emb_gt_encodings  = H.create 512;
  emb_eq_encodings  = H.create 512;
  lpo_af_same_encodings = H.create 512;
  lpo_af_diff_encodings = H.create 512;
  (*labeling stuff*)
  labeling           = !(flags.kind);
  lab_coefficients   = Hashtbl.create 512;
  lab_constants      = Hashtbl.create 512;
  lab_tbl            = Hashtbl.create 512;
  um_tbl             = Hashtbl.create 512;
  lright              = !(flags.right);
 } ;;

let cache_m tbl f k = 
 if Hashtbl.mem tbl k then return (Hashtbl.find tbl k)
 else (f k >>= fun v -> (Hashtbl.add tbl k v; return v))
;;

(* functions lifted from Logic into Made *)
let fresh_arith = get >>= fun s -> liftm (Logic.fresh_arith s.arith);;
let fresh_arith_spec arith = liftm (Logic.fresh_arith arith);;
let fresh_arith_spec2 arith = 
 fresh_arith_spec arith >>= fun a ->
 fresh_arith_spec arith >>= fun b ->
 return (a,b)
;;
let fresh_bool = get >>= fun s -> liftm Logic.fresh_bool;;
let ($&$) = lift2 (<&>);;
let ($|$) = lift2 (<|>);;
let ($->$) = lift2 (<->>);;
let ($<$) = lift2 (<<>);;
let ($>$) = lift2 (<>>);;
let ($>=$) = lift2 (<>=>);;
let ($<=$) = lift2 (<<=>);;
let ($+$) = lift2 (<<>);;
let eval_a a ass = a >>= fun a -> liftm (Logic.eval_a a ass);;
let eval_p p ass = p >>= fun p -> liftm (Logic.eval_p p ass);;
let map_op op f ls = sequence (List.map f ls) >>= (return <.> op);;
let mapi_op op f ls = sequence (List.mapi f ls) >>= (return <.> op);;
let gen_op op f n = sequence (List.gen f n) >>= (return <.> op);;
let map_and f = map_op Logic.big_and f;;
let mapi_and f = mapi_op Logic.big_and f;;
let gen_and f = gen_op Logic.big_and f;;
let map_or f = map_op Logic.big_or f;;
let mapi_or f = mapi_op Logic.big_or f;;
let gen_or f = gen_op Logic.big_or f;;

(* state monad interaction *)
let is_dp f = get >>= fun c -> return (Sig.is_dp f !(c.state));;
let arity f = get >>= fun c -> return (Sig.find_ari f !(c.state));;
 
(* matrix encoding starts here *)
let gen fresh triangle m n k = get >>= fun c ->
 let i,j = k / n, k mod n in
 if triangle && i > j then return Logic.zero
 else if triangle && i = j then
  (*restrict to 1*)
  fresh_arith_spec {c.arith with Logic.min = Int64.of_int c.arith.Logic.rat}
 else fresh
;;

let make_matrix ?(fresh=fresh_arith) triangle m n = 
 Made.sequence (List.gen (gen fresh triangle m n) (m*n)) >>=
 (return <.> M.of_list m n)
;;

let make_coefficient f i = get >>= fun c ->
 (*is_dp f >>= fun b ->  *)
 (*let d = if b then 1 else !(flags.dim) in *)
 let d = !(flags.dim) in
 make_matrix !(flags.triangle) d !(flags.dim)
;;

let make_constant f = get >>= fun c ->
 (*is_dp f >>= fun b ->  *)
 (*let d = if b then 1 else !(flags.dim) in *)
 let d = !(flags.dim) in
 make_matrix false d 1
;;

let coefficient f i = get >>= fun c ->
 cache_m c.coefficients (uncurry make_coefficient) (f, i)
;;

let constant f = get >>= fun c -> 
 cache_m c.constants make_constant f;;

let lab_coefficient f i = get >>= fun c ->
 cache_m c.lab_coefficients (uncurry make_coefficient) (f, i)
;;

let lab_constant f = get >>= fun c -> 
 cache_m c.lab_constants make_constant f;;

(*let u t = get >>= fun c ->  *)
 (*cache_m c.usable (const fresh_bool) (Option.the(Term.root t));; *)


let rec encode_term t = get >>= fun c ->
 cache_m c.subterm_encodings (etc c) t
and etc c = function
 | Term.Var x -> return (P.make [Mono.make (M.identity !(flags.dim)) [x]])
 | Term.Fun (f, ts) ->
  (List.fold_lefti (fun i p t -> 
   lift2 P.add p (lift2 P.scale (coefficient f i) (encode_term t)))
  (lift P.make (sequence [lift2 Mono.make (constant f) (return [])]))
  ts)
;;

let equal_matrix m0 m1 = Logic.big_and (M.map2list C.eq m0 m1);;
let greater_equal_matrix m0 m1 = Logic.big_and (M.map2list C.geq m0 m1);;

let greater_matrix m0 m1 = 
 (C.gt (M.get 0 0 m0) (M.get 0 0 m1)) <&> (greater_equal_matrix m0 m1)
;;

let greater_equal_mono m0 m1 =
 greater_equal_matrix (Mono.coefficient m0) (Mono.coefficient m1)
;;

let greater_mono m0 m1 =
 greater_matrix (Mono.coefficient m0) (Mono.coefficient m1)
;;

let equal_mono m0 m1 =
 equal_matrix (Mono.coefficient m0) (Mono.coefficient m1)
;;

let same_variables p0 p1 =
 let (v0,v1) = Pair.map (P.variables <.> P.non_constant_part) (p0,p1) in
 let (x0,x1) = List.diff v1 v0,List.diff v0 v1 in
 let c = (M.zero !(flags.dim) !(flags.dim)) in
 let z = Mono.make (M.zero !(flags.dim) 1) [] in
 let zero_p0 = P.make (List.map (Mono.make c) x0) in
 let zero_p1 = P.make (z::List.map (Mono.make c) x1) in
 (P.add p0 zero_p0, P.add p1 zero_p1)
;;

let greater_poly p0 p1 = 
 let (p0,p1) = same_variables p0 p1 in
 let (nc0, nc1) = Pair.map P.non_constant_part (p0,p1) in
 let v = P.variables nc1 in
 let nc0 = P.filter (fun m -> List.mem (Mono.variables m) v) nc0 in
 let cs = P.map2 greater_equal_mono nc0 nc1 in
 let cs' = greater_mono (P.constant_part p0) (P.constant_part p1) in
 Logic.big_and (cs'::cs)
;;

let greater_equal_poly p0 p1 =
 let (p0,p1) = same_variables p0 p1 in
 Logic.big_and (P.map2 greater_equal_mono p0 p1)
;; 

let equal_poly p0 p1 = 
 let (p0,p1) = same_variables p0 p1 in
 Logic.big_and (P.map2 equal_mono p0 p1)
;;

let greater_equal_rule rule = 
 lift2 greater_equal_poly (encode_term (Rule.lhs rule))
  (encode_term (Rule.rhs rule))
;;

let greater_equal_rule rule = get >>= fun c ->
 cache_m c.geq_encodings greater_equal_rule rule
;;

(*
let greater_rule rule =
 lift2 greater_poly (encode_term (Rule.lhs rule))
  (encode_term (Rule.rhs rule))
;;

let greater_rule rule = get >>= fun c ->
 cache_m c.gt_encodings greater_rule rule
;;

let monf f = arity f >>= fun a ->
 gen_and (fun i -> coefficient f i >>= fun c ->
  return (C.geq (M.get 0 0 c) C.one)) a
;;

let mon fs = map_and monf fs;;

let triangle c = get >>= fun s ->
 return (Logic.big_and (List.gen (fun i -> C.geq C.one (M.get i i c))
  !(flags.dim)))
;;

let triangle f = arity f >>= fun a ->
 gen_and (fun i -> coefficient f i >>= fun c -> triangle c) a ;;

let triangle fs = map_and triangle fs;;

(* not zero matrix *)
let nzm f i =
 coefficient f i >>= fun c ->
 let zm = M.fold (fun elt -> (<&>) (C.eq elt C.zero)) c Logic.top in
 return (Logic.neg zm)
;;

(* stuff for usable rules w.r.t. interpretation similar to GI03_01 *)
let rec process_rule df rule =
 let (l,r)  = Rule.to_terms rule in
 let dp = List.flat_map (flip Term.fun_pos r) df in
 let rec conj_of_smaller_pos p r =
 if p = Pos.root then return Logic.top else
  let q, i = Pos.split_last p in
  ((nzm (Option.the(Term.root (Term.subterm q r))) i)
    $&$ (conj_of_smaller_pos q r))
 in
 (u l) $->$ 
  map_and (fun p -> (conj_of_smaller_pos p r) $->$ (u (Term.subterm p r))) dp
;;

let geq_ur op geq rule = lift2 op (u(Rule.lhs rule)) (geq rule);;

let encode_geq s w = get >>= fun c ->
 let geq = if !(flags.direct) then greater_rule else greater_equal_rule in
 if !(flags.ur) then (
  let ds = Trs.def_symbols (Trs.of_list (s@w)) in
  map_and (geq_ur (<&>) geq) s $&$
  map_and (geq_ur (<->>) geq) w $&$
  map_and (process_rule ds) (s@w) 
 ) else 
  map_and geq (s@w)
;;

let encode_gt s = get >>= fun c -> 
  let op = (if !(flags.direct) then Logic.big_and else Logic.big_or) in
  map_op op greater_rule s
;;

let encode strict weak = get >>= fun c ->
 let (s, w) = Pair.map Trs.to_list (strict, weak) in
 let fs = Trs.funs (Trs.union strict weak) in
 encode_geq s w $&$
 encode_gt s $&$
 (if !(flags.dp) then return Logic.top else mon fs) $&$
 (if !(flags.triangle) then triangle fs else return Logic.top)
;;

let decode_rule ass rule = lift not (eval_p (greater_rule rule) ass);;

let decode_trs ass trs = 
 Made.filter (decode_rule ass) (Trs.to_list trs) >>= (return <.> Trs.of_list)
;;

let decode_weak ass w = if !(flags.dp) then return w else decode_trs ass w;;

let decode ass p = get >>= fun c -> 
 let (s,w) = Problem.get_sw p in
  lift3 Problem.set_sw (decode_trs ass s) (decode_weak ass w) (return p)
;;

let decode_ur_rule ass rule = eval_p (u (Rule.lhs rule)) ass;;

let decode_trs_ur ass w =
 Made.filter (decode_ur_rule ass) (Trs.to_list w) >>= (return <.> Trs.of_list)
;;

let decode_ur ass w = get >>= fun c ->
 if !(flags.ur) then (decode_trs_ur ass w >>= (return <.> Option.some))
 else (return None)
;;
*)

let eval_matrix ass m = m >>= fun m -> 
 let r, c = M.rows m, M.columns m in
 Made.sequence 
  (List.map (fun (i,j) -> liftm (Logic.eval_a (M.get i j m) ass))
  (List.product (List.range 0 r) (List.range 0 c))) >>= fun ms ->
 return (NM.of_list r c ms);;

let get_interpretation ass f = arity f >>= fun a ->
 Made.sequence 
  (List.gen (fun i -> eval_matrix ass (coefficient f i)) a) >>= fun coeffs ->
 eval_matrix ass (constant f) >>= fun const ->
 return (f, coeffs, const);;

let decode_interpretation ass p =
 (* only non-dp symbols needed for model *)
 let trs = Util.uncurry Trs.union (Problem.get_sw p) in
 Made.filter (fun x -> lift not (is_dp x)) (Trs.funs trs) >>= fun fs ->
 Made.sequence (List.map (get_interpretation ass) fs) >>=
  (return <.> List.foldr (fun fi acc -> uncurry3 (Intp.add acc) fi)
   (Intp.empty ()))
;;

(*
let solve s fs p = 
 (try Arg.parsex name spec fs with Arg.Bad s -> failwith s);
 if !(flags.help) then (Arg.usage spec ("Options for "^name^":"); exit 0);
 let c = context s p in
 let (s,w) = Problem.get_sw p in
 Logic.run ~dbits:!(flags.db) ~obits:!(flags.ob) (
  Made.run c (encode s w >>= fun phi ->
   Made.liftm (Logic.solve phi) >>= function
    | None -> return None
    | Some ass -> 
     decode ass p >>= fun p' -> decode_ur ass w >>= fun ur ->
     make_interpretation ass (Trs.union s w) >>= fun i ->
     return (Some (make !(flags.dp) !(flags.dim) i p p' ur))))
;;
*)

let set_label f l = get >>= fun c -> 
 c.lab := Idx.index l !(c.lab);
 let (f,s) = Sig.set_slab f (Idx.find_key l !(c.lab)) !(c.state) in
 c.state := s;
 return f
;;

let get_label f = get >>= fun c -> 
 return (Idx.find_elt (Sig.get_slab f !(c.state)) !(c.lab))

(*TODO: maybe drop try block*)
let rsl f =
 get >>= fun c -> return (try Sig.drop_slab f !(c.state) with Not_found -> f)
;;

let rec rsl_term = function
 | Term.Var x as t -> return t
 | Term.Fun (f,ts) -> lift2 Term.make_fun (rsl f) (Made.map rsl_term ts)
;;

let rsl_rule rule = 
 lift2 Rule.of_terms (rsl_term (Rule.lhs rule)) (rsl_term (Rule.rhs rule))
;;

let rsl_trs trs = lift Trs.of_list (Made.map rsl_rule (Trs.to_list trs));;

let rsl_p p = 
 let (s,w) = Problem.get_sw p in
 lift3 Problem.set_sw (rsl_trs s) (rsl_trs w) (return p)
;;

let lab f = get >>= fun c -> rsl f >>= fun f ->
 cache_m c.lab_tbl (const fresh_bool) f;;

let um f = get >>= fun c -> rsl f >>= fun f ->
 cache_m c.um_tbl (const fresh_bool) f;;

let encode_lab_term t = get >>= fun c ->
 cache_m c.label_encodings (etc c) t
and etc c = function
 | Term.Var x -> return (P.make [Mono.make (M.identity !(flags.dim)) [x]])
 | Term.Fun (f, ts) ->
  (List.fold_lefti (fun i p t -> 
   lift2 P.add p (lift2 P.scale (lab_coefficient f i) (encode_term t)))
  (lift P.make (sequence [lift2 Mono.make (lab_constant f) (return [])]))
  ts)
;;

(* TODO: use different interpretation for labeling function and
interpretation *)
let rec label_term t = match t with
 | (Term.Var x) -> return t
 | Term.Fun (f,ts) -> encode_lab_term t >>= fun l ->
  lift2 Term.make_fun (set_label f l) (Made.map label_term ts)
;;

let label_rule rule = 
 label_term (Rule.lhs rule) >>= fun l ->
 label_term (Rule.rhs rule) >>= fun r ->
 return (Rule.of_terms l r);;

let label_trs trs = lift Trs.of_list (Made.map label_rule (Trs.to_list trs));;

let label p =
 let (s,w) = Problem.get_sw p in
 label_trs s >>= fun s ->
 label_trs w >>= fun w ->
 return (Problem.set_sw s w p)
;;

let qmodel_rule rule = 
 (um (Rule.left_root rule)) $->$ greater_equal_rule rule;;
let qmodel_trs trs = map_and qmodel_rule (Trs.to_list trs);;
let qmodel p = qmodel_trs (Problem.get_trs p);;

(* reduction pair (LPO) begin *)
(* caching variables *)
let prec f = get >>= fun c -> rsl f >>= fun f ->
 let arith = Logic.nat c.prec_num in
 cache_m c.precs (const (fresh_arith_spec2 arith)) f
;;

let prec_gt f g = 
 prec f >>= fun (lf,sf) ->
 prec g >>= fun (lg,sg) ->
 lab f >>= fun isf ->
 lab g >>= fun isg ->
 get_label f >>= fun labf -> get_label g >>= fun labg ->
 return (
  (lf <>> lg) <|> 
   ((lf <=> lg) <&> isf <&> isg <&>
    (greater_poly labf labg <|>
     (greater_equal_poly labf labg <&> (sf <>> sg)))))
;;

let prec_ge f g = 
 prec f >>= fun (lf,sf) ->
 prec g >>= fun (lg,sg) ->
 get_label f >>= fun labf -> get_label g >>= fun labg ->
 return (
  (lf <>> lg) <|> 
   ((lf <=> lg) <&> (greater_equal_poly labf labg))
 )

let u t = get >>= fun c -> rsl_term t >>= fun t -> 
 cache_m c.usable (const fresh_bool) (Option.the(Term.root t));;

let af_l f =
 get >>= fun c -> rsl f >>= fun f -> cache_m c.af_l (const fresh_bool) f
;;

let af_n f = lift Logic.neg (af_l f);;

let af_p f i =
 get >>= fun c -> rsl f >>= fun f -> cache_m c.af_p (const fresh_bool) (f,i)
;;

let arity f = get >>= fun c -> return (Sig.find_ari f !(c.state));;
let n_ary n fs = Made.filter (fun f -> lift2 (=) (arity f) (return n)) fs;;

(* encode with argument filtering *)
(* useful shorthands. *)
let exists f p ts = mapi_or (fun i ti -> (af_p f i) $&$ (p ti)) ts;;
let for_all f p ts = mapi_and (fun i ti -> (af_p f i) $->$ (p ti)) ts;;
let exists2 f p ss ts = exists f p (List.map2 Rule.of_terms ss ts);;
let for_all2 f p ss ts = for_all f p (List.map2 Rule.of_terms ss ts);;
let rot f s t = f (Rule.of_terms s t);;
let rot2 f s t = f (Rule.of_terms t s);;

let af_collapsing f a i =
 let make j = if i = j then af_p f i else (lift Logic.neg (af_p f j)) in
 gen_and make a
;;

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

(* TODO: change if not all symbols are labeled *)
let same_labels one two f g ss ts = 
 lift2 equal_poly (get_label f) (get_label g) >>= fun eq ->
 lab f >>= fun l -> 
 ((return (eq <|> (~! l)) $->$ two)) $&$
 ((return ((~!eq) <&> l)) $->$ one)

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
   af_n f $&$ exists f (fun si -> eq_af (Rule.of_terms si t)) ss
  | Term.Fun (f, ss), Term.Fun (g, ts) when f = g ->
   for_all2 f eq_af ss ts (*equality holds when AF(f) is added*)
  | Term.Fun (f, ss), Term.Fun (g, ts) ->
   rsl f >>= fun fr ->
   rsl g >>= fun gr ->
   let one = eq_af_diff eq_af f g ss ts in
   if fr = gr then 
    let two = for_all2 f eq_af ss ts in
    same_labels one two f g ss ts
   else
    one 
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
;;

let lpo_af_same eq gt f0 ts0 ts1 =
 let s = Term.Fun (f0,ts0) in
 let t = Term.Fun (f0,ts1) in
 let geq rule = eq rule $|$ gt rule in
 let helper (eq,gt) (f0,ts0,ts1) = 
  (af_l f0 $&$ lex f0 eq gt ts0 ts1 $&$ for_all f0 (rot gt s) ts1) $|$ 
  (af_l f0 $&$ exists f0 (rot2 geq t) ts0) $|$ 
  (af_n f0 $&$ exists2 f0 gt ts0 ts1)
 in get >>= fun c -> cache_m c.lpo_af_same_encodings (helper (eq,gt))
 (f0,ts0,ts1)
;;

let for_all f p ts = mapi_and (fun i ti -> (af_p f i) $->$ (p ti)) ts;;

let lpo_af_diff eq gt f0 f1 ts0 ts1 =
 let helper gt (f0,f1,ts0,ts1) =
 let s = Term.Fun (f0, ts0) and t = Term.Fun (f1, ts1) in
 (af_l f0 $&$ af_l f1 $&$ exists f0 (rot2 eq t) ts0) $|$
 ((af_n f0 $|$ af_l f1) $&$ exists f0 (rot2 gt t) ts0) $|$
 (((af_l f0 $&$ af_l f1 $&$ prec_gt f0 f1) $|$ af_n f1) $&$ 
  for_all f1 (rot gt s) ts1)
 in get >>= fun c -> cache_m c.lpo_af_diff_encodings (helper gt) (f0,f1,ts0,ts1)
;;

let rec lpo_af_gt rule = 
 let rec helper rule = match Rule.to_terms rule with
  | Term.Var _, _ -> return Logic.bot
  | Term.Fun _, Term.Var _ -> emb_af_gt rule
  | Term.Fun (f,ss), Term.Fun (g,ts) ->
   if f = g then lpo_af_same eq_af lpo_af_gt f ss ts
   else 
    let one = lpo_af_diff eq_af lpo_af_gt f g ss ts in
    rsl f >>= fun fr -> rsl g >>= fun gr ->
    if fr = gr then
     let two = lpo_af_same eq_af lpo_af_gt f ss ts in
     same_labels one two f g ss ts
    else
     one
 in get >>= fun c -> cache_m c.lpo_gt_encodings helper rule
;;

let lpo_af_ge rule = lpo_af_gt rule $|$ eq_af rule;;

(* encoding *)
let lpo_gt rule = lpo_af_gt rule;;
 
let lpo_ge rule = lpo_af_ge rule;;

(* stuff for usable rules w.r.t. interpretation similar to GI03_01 *)
let rec process_rule df rule =
 let (l,r)  = Rule.to_terms rule in
 rsl_term r >>= fun r ->
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

let ge_ur op ge rule = lift2 op (u(Rule.lhs rule)) (lpo_ge rule);;

let encode_gt s = map_or lpo_gt s;;

let encode_ge s w = get >>= fun c ->
 if !(flags.ur) then (
  let ds = Trs.def_symbols (Trs.of_list (s@w)) in
  Made.map rsl ds >>= fun ds ->
  map_and (ge_ur (<&>) lpo_ge) s $&$
  map_and (ge_ur (<->>) lpo_ge) w $&$
  map_and (process_rule ds) (s@w)
 ) else 
 map_and lpo_ge (s@w)
;;

let af_f f = arity f >>= fun a -> af_l f $|$ gen_or (af_collapsing f a) a;;
let af fs = map_and af_f fs;;
let af fs = Made.map rsl fs >>= fun fs -> af fs;;

let redpair p = get >>= fun c ->
 let (s,w) = Problem.get_sw p in
 let fs = (Trs.funs (Trs.union s w)) in
 let s, w = Pair.map Trs.to_list (s, w) in
 encode_gt s $&$ encode_ge s w $&$ af fs
;;

(* decode from assignment *)
let decode_rule ass rule = get >>= fun c ->
  lift not (eval_p (lpo_gt rule) ass)
;;

let decode_trs ass trs = 
 Made.filter (decode_rule ass) (Trs.to_list trs) >>= (return <.> Trs.of_list)
;;

let decode_p ass p =
 let (s,w) = Problem.get_sw p in
 decode_trs ass s >>= fun s ->
 return (Problem.set_sw s w p)
;;

(*
let decode ass s w = get >>= fun c -> 
 lift2 Pair.make
  (decode_trs ass s) (if !(flags.dp) then return w else decode_trs ass w)
;;

*)
let decode_ur_rule ass rule = eval_p (u (Rule.lhs rule)) ass;;

let decode_trs_ur ass w =
 lift Trs.of_list (Made.filter (decode_ur_rule ass) (Trs.to_list w))
;;

let decode_ur ass p = get >>= fun c ->
 let (_,w) = Problem.get_sw p in
 if !(flags.ur) then (decode_trs_ur ass w >>= (return <.> Option.some))
 else (return None)
;;

let decode_prec_f ass f =
 prec f >>= fun (lf,sf) ->
 eval_a (return lf) ass >>= fun lf -> 
 eval_a (return sf) ass >>= fun sf -> 
 return (f,int_of_string (Number.to_string lf), int_of_string (Number.to_string sf))
;;

let decode_prec ass p =
 let fs = Trs.funs (Util.uncurry Trs.union (Problem.get_sw p)) in
 let n = List.length fs * 2 in
 Made.sequence (List.map (decode_prec_f ass) fs) >>= fun ps ->
 let ps = List.map (fun (f,lf,sf) -> (f,lf*n+sf)) ps in
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

let decode_af ass p = 
 let (s,w) = Problem.get_sw p in
 decode_af ass s w
;;
(* reduction pair (LPO) end *)

let rec usable_fs fs trs = 
 let n = List.flat_map (Rule.right_funs) 
  (List.filter (fun r -> List.mem (Rule.left_root r) fs) trs) in
 let fs' = List.union n fs in
 if List.is_subset fs' fs then fs else usable_fs fs' trs
;;

let usable_fs fs trs = usable_fs fs (Trs.to_list trs);;

let trs_subterms rule_subterms trs =
 List.flat_map rule_subterms (Trs.to_list trs);;

let foot fs trs = get >>= fun c -> 
 let foo = (fun rule -> if c.lright then Term.subterms (Rule.rhs rule)
           else Util.uncurry List.union (Rule.map Term.subterms rule)) in
 let st = List.filter (fun t -> not (Term.is_var t)) (trs_subterms foo trs) in
 let st = List.filter (fun t -> List.mem (Option.the (Term.root t)) fs) st in
 return (List.flat_map Term.funs st)
;;

let ur_f f trs = foot [f] trs >>= fun fs -> map_and um (usable_fs fs trs);;

let ur p = get >>= fun c ->
 let trs = Util.uncurry Trs.union (Problem.get_sw p) in
 let fs = Trs.funs trs in
 match c.labeling with
  | Predictive -> map_and (fun f -> lab f $->$ ur_f f trs) fs
  | Semantic   -> map_and (fun f -> lab f $&$ um f) fs
;;

(* putting everything together *)
let encode p plab = qmodel p $&$ redpair plab $&$ ur p;;

let decode_um_f ass f = eval_p (um f) ass;;
let decode_um ass p =
 let fs = Trs.funs (Util.uncurry Trs.union (Problem.get_sw p)) in
 Made.filter (decode_um_f ass) fs;;

let decode_lab_f ass f = eval_p (lab f) ass;;
let decode_lab ass p =
 let fs = Trs.funs (Util.uncurry Trs.union (Problem.get_sw p)) in
 Made.filter (decode_lab_f ass) fs;;

let decode ass p plab = 
 decode_p ass plab >>= fun p' -> rsl_p p' >>= fun p' ->
 decode_ur ass p >>= fun ur ->
 decode_interpretation ass p >>= fun i ->
 decode_af ass p >>= fun af ->
 decode_um ass p >>= fun um ->
 decode_lab ass p >>= fun l ->
 decode_prec ass p >>= fun prec ->
 return (Some (make !(flags.dp) !(flags.dim) i l um af prec p p' ur))
 (* TODO: return stuff *)
;;

let solve s fs p = 
 let configurate s = F.printf "%s@\n%!" s; flags.help := true in
 (try init (); Arg.parsex code spec fs with Arg.Bad s -> configurate s);
 if !(flags.help) then (Arg.usage spec ("Options for "^code^":"); exit 0);
 let c = context s p in
 (* TODO: look why this is not printed *)
 if not !(flags.dp) then failwith "Semantic Labeling: flag -dp required";
 Logic.run ~dbits:!(flags.db) ~obits:!(flags.ob) (
  Made.run c (
   label p >>= fun plab ->
   encode p plab >>= fun phi ->
   Made.liftm (Logic.solve phi) >>= function
    | None -> return None
    | Some ass -> decode ass p plab))
;;

(* wrap into state monad *)
let (>>=) = Monad.(>>=);;
let solve fs p = Monad.get >>= fun s -> Monad.return (solve s fs p);;
