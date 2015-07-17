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
module Anumber = struct 
 type t = Logic.Number.t;;

 let zero = Logic.Number.get_minf ();;
 let one = Logic.Number.zero;;
 let is_zero = Logic.Number.is_minf;;
 let is_one = Logic.Number.is_zero;;
 let add = Logic.Number.max;;
 let mul = Logic.Number.add;;
 let to_string = Logic.Number.to_string;;
 let fprintfx = Logic.Number.fprintfx;;
 let fprintf = Logic.Number.fprintf;;
end

module C = ArcticCoefficient;;
module Co = Complexity;;
module F = Format;;
module Fun = Function;;
module M = Matrix.Make (C);;
module Monad = Rewritingx.Monad;;
module NM = Matrix.Make (Anumber);;
module NMono = Monomial.Make (NM) (String);;
module NP = Polynomial.Make (NMono);;
module Pos = Position;;
module Sig = Signature;;
module Var = Variable;;
module Intp = Interpretation.Make (NM);;
module Mono = Monomial.Make (M) (Var);;
module P = Polynomial.Make (Mono);;

(*** TYPES ********************************************************************)
type t = {
 complexity : bool;
 dimension : int; 
 direct : bool;
 monotone : bool;
 interpretation : Intp.t;
 input : Problem.t;
 output : Problem.t;
 usable_rules : Trs.t option;
};;

type flags = {
 bz : bool ref;
 cp : bool ref;
 dim : int ref;
 dir : bool ref;
 dp : bool ref;
 help : bool ref;
 min : int ref;
 ob : int ref;
 p : bool ref;
 rat : int ref;
 real : bool ref;
 str : bool ref;
 ur : bool ref;
};;

type context = {
 arith             : Logic.arith;
 usable            : (Fun.t, Logic.p) Hashtbl.t;
 coefficients      : (Fun.t * int, M.t) Hashtbl.t;
 constants         : (Fun.t, M.t) Hashtbl.t;
 gt_encodings      : (Rule.t, Logic.p) Hashtbl.t;
 geq_encodings     : (Rule.t, Logic.p) Hashtbl.t;
 state             : Sig.t;
 subterm_encodings : (Term.t, P.t) Hashtbl.t;
};;

(*** GLOBALS ******************************************************************)
let code = "arctic";;
let name = "Arctic Interpretation Processor";;
let comment = "Applies arctic matrix interpretations."

let keywords = [
 "arctic interpretation";
 "matrix interpretation";
 "linear interpretation";
 "termination";
];;

let flags = {
 bz = ref false;
 cp = ref false;
 dim = ref 1;
 dir = ref false;
 dp = ref false;
 help = ref false;
 min = ref 1;
 ob = ref max_int;
 p  = ref false;
 rat = ref 1;
 real = ref false;
 str = ref false;
 ur = ref false;
};;

let spec =
 let spec = [
  ("-bz",Arg.Set flags.bz,
   "Matrix entries can be below zero. Only possible in combination with `-dp'");
  ("-cp",Arg.Set flags.cp,"copy oriented rules from strict to weak.");
  ("-dim",Arg.Set_int flags.dim,"Specifies the dimension of the matrices.");
  ("-dir",Arg.Set flags.dir,"Try to finish termination proof.");
  ("-direct",Arg.Set flags.dir,"see -dir.");
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
  ("-p",Arg.Set flags.p,
   "Print encoding in SMT-LIB format and fail");
  ("-rat",Arg.Set_int flags.rat,"Sets the denumerator.");
  ("-real",Arg.Set flags.real,"Uses reals.");
  ("-strict",Arg.Set flags.str,"s \\cap > \\neq \\varnothing, \
    r \\subseteq \\geqslant.");
  ("-ur",Arg.Set flags.ur,"Uses usable rules with respect to interpretation.")]
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
 flags.bz := false;
 flags.cp := false;
 flags.dim := 1;
 flags.dir := false;
 flags.dp := false;
 flags.help := false;
 flags.min := 1;
 flags.ob := max_int;
 flags.rat := 1;
 flags.real := false;
 flags.ur := false;
;;

(* Constructors and Destructors *)
let make dim dir dp cp i input output ur = {
 complexity = cp;
 dimension = dim;
 direct = dir;
 monotone = not dp;
 interpretation = i;
 input = input;
 output = output;
 usable_rules = ur;
};;

let get_ip p = p.input;;
let get_op p = p.output;;

(* Complexity Bounds *)
let complexity c p =
 if Problem.is_sp (get_ip p) then
  if p.monotone && p.direct then Co.mul c Co.linear
  else Co.mul c Co.other
 else if Problem.is_cp (get_ip p) then
  if p.monotone && (p.direct || p.complexity) then Co.add c Co.linear
  else Co.add c Co.other
 else Co.mul c Co.other
;;

(* Compare Functions *)
let equal p q =
 Problem.equal (get_ip p) (get_ip q) && Problem.equal (get_op p) (get_op q)
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
  with Not_found -> NM.make d 1 Anumber.zero
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

let fprintf fs fmt p  = 
 F.fprintf fmt "@[<1>%s:@\ndimension: %i" name p.dimension;
 fprintf_ur fmt p.usable_rules >>= fun _ ->
 F.fprintf fmt "@\n@[<1>interpretation:@\n";
 Intp.fprintf fmt p.interpretation >>= fun _ ->
 F.fprintf fmt "@]@\n@[<1>orientation:@\n";
 fprintf_orient p.dimension p.interpretation fmt p.input >>= fun _ ->
 F.fprintf fmt "@]@\n@[<1>problem:@\n";
 Problem.fprintfm fmt p.output >>= fun _ ->
 F.fprintf fmt "@]@\n"; List.hd fs fmt >>= fun _ ->
 Monad.return (F.fprintf fmt "@]")
;;

let fprintfx_dom fmt p =
 if p.dimension = 1 then F.fprintf fmt "@{<domain><arcticBelowZero/>@}" else
 F.fprintf fmt 
  "@{<domain>@{<matrices>@{<dimension>%d@}@{<strictDimension>1@}\
   @{<domain><arcticBelowZero/>@}@}@}"
  p.dimension
;;

let fprintfx_intp fmt p =
 F.fprintf fmt
  "@{<redPair>@{<interpretation>@{<type>@{<polynomial>%a@{<degree>1@}@}@}"
 fprintfx_dom p;
 Intp.fprintfx p.dimension fmt p.interpretation >>= fun _ ->
 Monad.return(F.fprintf fmt "@}@}")
;;

let fprintfx_ur fmt = function
  | Some ur ->
    F.fprintf fmt "@{<usableRules>";
    Trs.fprintfx fmt ur >>= fun _ ->
    Monad.return(F.fprintf fmt "@}") 
  | None    -> Monad.return()
;;

let fprintfx fs fmt p = if Problem.is_dp p.input then (
  let tag = if p.monotone then "monoRedPairProc" else "redPairProc" in
  F.fprintf fmt "@{<%s>" tag;
  fprintfx_intp fmt p            >>= fun _ ->
  Problem.fprintfx fmt p.output  >>= fun _ ->
  fprintfx_ur fmt p.usable_rules >>= fun _ ->
  List.hd fs fmt                 >>= fun _ ->
  Monad.return(F.fprintf fmt "@}");
) else if Problem.is_sp p.input then (
  F.fprintf fmt "@{<ruleRemoval>";
  fprintfx_intp fmt p           >>= fun _ ->
  Problem.fprintfx fmt p.output >>= fun _ ->
  List.hd fs fmt                >>= fun _ ->
  Monad.return(F.fprintf fmt "@}");
) else (
  failwith "arctic relative not supported!"
)
;;

(* Processor *)
(* administrative functions *)
let (>>=) = Made.(>>=);;

let context s =
 let arith = {
  Logic.min = Int64.of_int !(flags.min);
  neg = !(flags.bz);
  rat = !(flags.rat);
  real = !(flags.real);
  minf = true}
 in
 {arith             = arith; 
  usable            = Hashtbl.create 512;
  coefficients      = Hashtbl.create 512;
  constants         = Hashtbl.create 512;
  gt_encodings      = Hashtbl.create 512;
  geq_encodings     = Hashtbl.create 512;
  state             = s;
  subterm_encodings = Hashtbl.create 512}
;;

let cache_m tbl f k = 
 if Hashtbl.mem tbl k then return (Hashtbl.find tbl k)
 else (f k >>= fun v -> (Hashtbl.add tbl k v; return v))
;;

(* functions lifted from Logic into Made *)
let fresh_arith = get >>= fun s -> liftm (Logic.fresh_arith s.arith);;
let fresh_bool = get >>= fun s -> liftm Logic.fresh_bool;;

let ($&$) = lift2 (<&>);;
let ($->$) = lift2 (<->>);;

(* state monad interaction *)
let is_dp f = get >>= fun c -> return (Sig.is_dp f c.state);;
let arity f = get >>= fun c -> return (Sig.find_ari f c.state);;
 
(* matrix encoding starts here *)
let make_matrix m n = 
 Made.sequence (List.gen (const fresh_arith) (m*n)) >>= fun elts ->
 return (M.of_list m n elts)
;;

let make_coefficient f i = get >>= fun c ->
 is_dp f >>= fun b -> 
 let d = if b then 1 else !(flags.dim) in
 make_matrix d !(flags.dim)
;;

let make_constant f = get >>= fun c ->
 is_dp f >>= fun b -> 
 let d = if b then 1 else !(flags.dim) in
 make_matrix d 1
;;

let coefficient f i = get >>= fun c ->
 cache_m c.coefficients (uncurry make_coefficient) (f, i)
;;

let constant f = get >>= fun c -> cache_m c.constants make_constant f;;

let u t = get >>= fun c ->
 cache_m c.usable (const fresh_bool) (Option.the(Term.root t))
;;

let rec encode_term t = get >>= fun c ->
 cache_m c.subterm_encodings (etc c) t
and etc c = function
 | Term.Var x -> return (P.make [
  Mono.make (M.identity !(flags.dim)) [x];
  Mono.make (M.make !(flags.dim) 1 Logic.minf) []])
 | Term.Fun (f, ts) ->
  (List.fold_lefti (fun i p t -> 
   lift2 P.add p (lift2 P.scale (coefficient f i) (encode_term t)))
  (lift P.make (sequence [lift2 Mono.make (constant f) (return [])]))
  ts)
;;

let greater_equal_matrix m0 m1 = Logic.big_and (M.map2list C.geq m0 m1);;
let greater_matrix m0 m1 = Logic.big_and (M.map2list C.gt m0 m1);;

let greater_equal_mono m0 m1 =
 greater_equal_matrix (Mono.coefficient m0) (Mono.coefficient m1)
;;

let greater_mono m0 m1 =
 greater_matrix (Mono.coefficient m0) (Mono.coefficient m1)
;;

let greater_poly p0 p1 = 
 let c0 = P.constant_part p0 in
 let c1 = try P.constant_part p1 with Not_found -> 
  let m = Mono.coefficient c0 in
  Mono.make (M.zero (M.rows m) (M.columns m)) []
 in
 let (nc0, nc1) = Pair.map P.non_constant_part (p0,p1) in
 let v = P.variables nc1 in
 let nc0 = P.filter (fun m -> List.mem (Mono.variables m) v) nc0 in
 let cs = P.map2 greater_mono nc0 nc1 in
 let cs' = greater_mono c0 c1 in
 Logic.big_and (cs'::cs)
;;

let greater_equal_poly p0 p1 =
 let v = P.variables p1 in
 let p0 = P.filter (fun m -> List.mem (Mono.variables m) v) p0 in
 Logic.big_and (P.map2 greater_equal_mono p0 p1)
;; 

let greater_equal_rule rule = 
 lift2 greater_equal_poly (encode_term (Rule.lhs rule))
  (encode_term (Rule.rhs rule))
;;

let greater_equal_rule rule = get >>= fun c ->
 cache_m c.geq_encodings greater_equal_rule rule
;;

let greater_rule rule =
 lift2 greater_poly (encode_term (Rule.lhs rule))
  (encode_term (Rule.rhs rule))
;;

let greater_rule rule = get >>= fun c ->
 cache_m c.gt_encodings greater_rule rule
;;

(* monotonicity and so on *)
let finite_matrix m = m >>= fun m -> return (~! (M.get 0 0 m <=> C.zero));;
let positive_matrix m = m >>= fun m -> return (M.get 0 0 m <>=> C.one);;

let somewhere_positive_f f = get >>= fun c ->
 arity f >>= fun a -> 
 sequence (positive_matrix (constant f) ::
  List.gen (fun i -> positive_matrix (coefficient f i)) a) >>=
 (return <.> Logic.big_or)
;;

let somewhere_positive fs =
 sequence (List.map somewhere_positive_f fs) >>= (return <.> Logic.big_and)
;;

let dummy_f f = arity f >>= fun a ->
 if a <= 1 then return Logic.top else return Logic.bot
;;

let dummy fs = 
 Made.sequence (List.map dummy_f fs) >>= (return <.> Logic.big_and)
 (* return Logic.top *)
;;

let zero_matrix m = m >>= fun m ->
 return (Logic.big_and (M.map2list C.eq m (M.zero (M.rows m) (M.columns m))))
;;

let mon_f f = arity f >>= fun a ->
 if a = 0 then finite_matrix (constant f) else
 if a = 1 then finite_matrix (coefficient f 0) $&$ (zero_matrix (constant f))
 else return Logic.bot
 (* else  *)
   (* Made.sequence (List.gen (fun i -> finite_matrix (coefficient f i))
   a) *)
   (* >>= fun cs ->  *)
   (* (return (Logic.big_and cs)) $&$ (zero_matrix (constant f)) *)
;;

let mon fs = sequence (List.map mon_f fs) >>= (return <.> Logic.big_and);;
let monotone fs = dummy fs $&$ mon fs;;

(* not zero matrix *)
let nzm f i = lift Logic.neg (zero_matrix (coefficient f i));;

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
 Made.sequence (List.map (fun p -> 
 (conj_of_smaller_pos p r) $->$ (u (Term.subterm p r))) dp) >>= fun a ->
 (u l) $->$ (return (Logic.big_and a))
;;

let geq_ur op geq rule =
 lift2 op (u(Rule.lhs rule)) (geq rule)
;;

let encode_geq s w = 
 let s = if !(flags.str) then [] else s in
 get >>= fun c ->
 let geq = if !(flags.dir) then greater_rule else greater_equal_rule in
 if !(flags.ur) then
  let ds = Trs.def_symbols (Trs.of_list (s@w)) in
  Made.sequence (List.map (geq_ur (<&>) geq) s) >>= fun x ->
  Made.sequence (List.map (geq_ur (<->>) geq) w) >>= fun y ->
  Made.sequence (List.map (process_rule ds) (s@w)) >>= fun ur ->
  return (Logic.big_and (x@y@ur))
 else Made.sequence (List.map geq (s@w)) >>= (return <.> Logic.big_and)
;;

let encode_gt s w = 
 let s = if !(flags.dp) || !(flags.cp) || !(flags.dir) then s else s@w in
 get >>= fun c -> 
 let op = (if !(flags.dir) then Logic.big_and else Logic.big_or) in
 Made.sequence (List.map greater_rule s) >>= (return <.> op)
;;

let monotone fs =
 (if !(flags.dp) then
  (*(if !(flags.bz) then absolute_positive fs else somewhere_finite fs) original version*)
  (*(if !(flags.bz) then somewhere_positive fs else somewhere_finite fs) RT optimization*)
  somewhere_positive fs (*equivalent to above*)
 else monotone fs) 
;;

let str strict =
 if !(flags.str) && (Trs.is_duplicating strict) 
  then return Logic.bot else return Logic.top
;;

let encode strict weak = 
 let (s, w) = Pair.map Trs.to_list (strict, weak) in
 let fs = Trs.funs (Trs.union strict weak) in
 get >>= fun c ->
 encode_geq s w >>= fun geq ->
 encode_gt s w >>= fun gt ->
 monotone fs >>= fun mon -> 
 str strict >>= fun str -> 
 return (Logic.big_and [mon;gt;geq;str])

;;

let decode_rule ass rule = 
 greater_rule rule >>= fun enc ->
 liftm (Logic.eval_p enc ass) >>= (return <.> not)
;;

let decode_trs ass trs = 
 Made.filter (decode_rule ass) (Trs.to_list trs) >>= (return <.> Trs.of_list)
;;

let decode_weak ass w = if !(flags.dp) || !(flags.cp) then return w else decode_trs ass w;;

let decode ass p = 
 let (s,w) = Problem.get_sw p in
 (lift3 Problem.set_sw (decode_trs ass s) (decode_weak ass w) (return p)
 ) >>= fun p ->
 if !(flags.cp) then (
  let s',_ = Problem.get_sw p in
  let t = Trs.diff s s' in 
  return (Problem.set_sw s' (Trs.union t w) p)
  ) else return p
;;

let decode_ur_rule ass rule =
 u (Rule.lhs rule) >>= fun u -> liftm (Logic.eval_p u ass)
;;

let decode_trs_ur ass w =
 Made.filter (decode_ur_rule ass) (Trs.to_list w) >>= (return <.> Trs.of_list)
;;

let decode_ur ass w =
 get >>= fun c ->
 if !(flags.ur) then (decode_trs_ur ass w >>= (return <.> Option.some))
 else (return None)
;;

let eval_matrix ass m = m >>= fun m -> 
 let r, c = M.rows m, M.columns m in
 Made.sequence 
  (List.map (fun (i,j) -> liftm (Logic.eval_a (M.get i j m) ass))
  (List.product (List.range 0 r) (List.range 0 c))) >>= fun ms ->
 return (NM.of_list r c ms)
;;

let get_interpretation ass f = 
 arity f >>= fun a ->
 Made.sequence 
  (List.gen (fun i -> eval_matrix ass (coefficient f i)) a) >>= fun coeffs ->
 eval_matrix ass (constant f) >>= fun const ->
 return (f, coeffs, const)
;;

let make_interpretation ass trs = 
 Made.sequence (List.map (get_interpretation ass) (Trs.funs trs)) >>=
 (return <.> List.foldr (fun fi acc -> uncurry3 (Intp.add acc) fi)
  (Intp.empty ()))
;;

let solve s fs p = 
 let configurate s = F.printf "%s@\n%!" s; flags.help := true in
 (try init (); Arg.parsex code spec fs with Arg.Bad s -> configurate s);
 if !(flags.help) then (Arg.usage spec ("Options for "^code^":"); exit 0);
 let c = context s in
 let (s,w) = Problem.get_sw p in 
 Logic.run ~obits:!(flags.ob) (
  Made.run c (encode s w >>= fun phi ->
   if !(flags.p) then (
    Format.fprintf Format.std_formatter "@[%a@]@\n" 
     (fun ppt -> Logic.fprintf_smt ppt) phi;
    return None
   ) else
   Made.liftm (Logic.solve phi) >>= function
    | None -> return None
    | Some ass -> 
     decode ass p >>= fun p' ->
     decode_ur ass w >>= fun ur ->
     make_interpretation ass (Trs.union s w) >>= fun i ->
     let dim = !(flags.dim) and dir = !(flags.dir) and dp = !(flags.dp) in
     return (Some (make dim dir dp !(flags.cp) i p p' ur))))
;;

(* wrap into state monad *)
let (>>=) = Monad.(>>=);;
let solve fs p = Monad.get >>= fun s -> Monad.return (solve s fs p);;
