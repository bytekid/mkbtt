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
module C = Coefficient;;
module Co = Complexity;;
module F = Format;;
module Fun = Function;;
module Number = Logic.Number;;
module NM = Matrix.Make (Number);;
module Intp = Interpretation.Make (NM);;
module Monad = Rewritingx.Monad;;
module NMono = Monomial.Make (NM) (String);;
module NP = Polynomial.Make (NMono);;
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

(*** TYPES ********************************************************************)
type shape = Linear | SQuadratic | SMixed ;;

type t = {
 degree : int;
 dimension : int; 
 direct : bool; 
 monotone : bool;
 triangle : bool;
 interpretation : Intp.t;
 input : Problem.t;
 output :Problem.t;
 usable_rules : Trs.t option;
};;

type flags = {
 ac : bool ref;
 db : int ref;
 dim : int ref;
 dir : bool ref;
 dp : bool ref;
 help : bool ref;
 min : int ref;
 neg : bool ref;
 o  : bool ref;
 ob : int ref;
 p : bool ref;
 rat : int ref;
 real : bool ref;
 shp : shape ref;
 heuristic : int ref;
 tri : bool ref;
 ur : bool ref;
};;

type context = {
 arith              : Logic.arith;
 usable             : (Fun.t,Logic.p) Hashtbl.t;
 coefficients       : (Fun.t * int, M.t) Hashtbl.t;
 coefficients2      : (Fun.t*(int*int), M.t) Hashtbl.t;
 constants          : (Fun.t,M.t) Hashtbl.t;
 gt_encodings       : (Rule.t,Logic.p) Hashtbl.t;
 geq_encodings      : (Rule.t,Logic.p) Hashtbl.t;
 state              : Sig.t;
 subterm_encodings  : (Term.t,P.t) Hashtbl.t;
 shape              : shape;
 nlfs               : Fun.t list;
 orient             : bool;
 d_tbl              : (Rule.t * Rule.t * int,M.t) Hashtbl.t; (* distances *)
 graph              : Graph.t;
 constant_encodings : (M.t,M.t) Hashtbl.t; (* experimental *)
};;

(*** GLOBALS ******************************************************************)
let code = "poly";;
let name = "Polynomial Interpretation Processor";;
let comment = "Applies polynomial interpretations."
let keywords = ["polynomial interpretation";"linear interpretation";"termination"];;

let flags = {
 ac = ref false;
 db = ref max_int;
 dim = ref 1;
 dir = ref false;
 dp = ref false;
 help = ref false;
 min = ref 1;
 neg = ref false;
 o   = ref false;
 ob = ref max_int;
 p = ref false;
 rat = ref 1;
 real = ref false;
 shp = ref Linear;
 heuristic = ref 0;
 tri = ref false;
 ur = ref false;
};;

let spec =
 let spec = [
  ("-ac",Arg.Set flags.ac,"Use AC-compatible interpretations.");
  ("-db",Arg.Set_int flags.db,"Specifies the bits after the decimal point.");
  ("-dim",Arg.Set_int flags.dim,"Specifies the dimension of the matrices.");
  ("-direct",Arg.Set flags.dir,"Try to finish termination proof.");
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
  ("-neg",Arg.Set flags.neg,
   "Allow negative numbers (only for non-linear interpretations).");
  ("-o",Arg.Set flags.o, "Orient (non-linear) polynomials smartly.");
  ("-ob",Arg.Set_int flags.ob,
   "Defines the number of bits used to represent intermediate results \
    (same as `-max' but in bits)");
  ("-p",Arg.Set flags.p,
   "Print encoding in SMT-LIB format and fail");
  ("-rat",Arg.Set_int flags.rat,"Sets the denumerator.");
  ("-real",Arg.Set flags.real,"Uses reals.");
  ("-nl",Arg.Unit (fun _ -> flags.shp := SQuadratic), 
   "non-linear interpretations");
  ("-heuristic",Arg.Set_int flags.heuristic,
   "-1 -> all symbols" ^
   " 0 -> no symbols" ^
   " 1 -> symbols appearing at most once in each lhs/rhs" ^
   " 2 -> symbols appearing at most twice in each lhs/rhs" ^
   " 3 -> symbols appearing at parallel positions in each lhs/rhs" ^
   " 4 -> defined symbols" ^
   "");
  ("-sm",Arg.Unit (fun _ -> flags.shp := SMixed),
   "simple-mixed non-linear interpretations");
  ("-triangle",Arg.Set flags.tri,"Use triangular matrices.");
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
 flags.db := max_int;
 flags.dim := 1;
 flags.dir := false;
 flags.dp := false;
 flags.help := false;
 flags.min := 1;
 flags.neg := false;
 flags.ob := max_int;
 flags.rat := 1;
 flags.real := false;
 flags.shp := Linear;
 flags.tri := false;
 flags.ur := false;
;;

(* Constructors and Destructors *)
let make deg dim dir dp tri i input output ur = {
 degree = deg;
 dimension = dim;
 direct = dir;
 monotone = not dp;
 triangle = tri;
 interpretation = i;
 input = input;
 output = output;
 usable_rules = ur;
};;

let get_ip p = p.input;;
let get_op p = p.output;;

(* Complexity Bounds *)
let complexity c p =
 let n = p.dimension in
 if Problem.is_sp (get_ip p) then
  if p.monotone && p.triangle && p.direct then Co.mul c (Co.poly (Some n))
  else Co.mul c Co.other
 else if Problem.is_cp (get_ip p) then
  if p.monotone && p.triangle && p.direct then Co.add c (Co.poly (Some n))
  else Co.add c Co.other
 else Co.mul c Co.other
;;

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
  let (cs,ds) = List.split_at (List.length ts) cs in
  let cs = List.zip cs ds in
  assert (List.length cs = List.length ts);
  List.foldr2 (fun (ci,di) ti acc ->
   interpret_term d i ti >>= fun iti ->
   let iti2 = NP.mul iti iti in
   let pi = NP.add (NP.scale ci iti) (NP.scale di iti2) in
   Monad.lift (NP.add pi) acc)
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

let fprintf fs fmt p  = 
 F.fprintf fmt "@[<1>%s:@\ndimension: %i" name p.dimension;
 fprintf_ur fmt p.usable_rules >>= fun _ ->
 F.fprintf fmt "@\n@[<1>interpretation:@\n";
 Intp.fprintf fmt p.interpretation >>= fun _ ->
 (**)
 F.fprintf fmt "@]@\n@[<1>orientation:@\n";
 fprintf_orient p.dimension p.interpretation fmt p.input >>= fun _ ->
 (**)
 F.fprintf fmt "@]@\n@[<1>problem:@\n";
 Problem.fprintfm fmt p.output >>= fun _ ->
 F.fprintf fmt "@]@\n"; List.hd fs fmt >>= fun _ ->
 Monad.return (F.fprintf fmt "@]")
;;

let fprintfx_dom fmt p =
 if p.dimension = 1 then F.fprintf fmt "@{<domain><naturals/>@}" else
 F.fprintf fmt 
  "@{<domain>@{<matrices>@{<dimension>%d@}@{<strictDimension>1@}@{<domain><rationals/>@}@}@}"
  p.dimension
;;

let fprintfx_intp fmt p =
 F.fprintf fmt
  "@{<redPair>@{<interpretation>@{<type>@{<polynomial>%a@{<degree>%d@}@}@}"
  fprintfx_dom p
  p.degree;
 Intp.fprintfx p.dimension fmt p.interpretation >>= fun _ ->
 Monad.return(F.fprintf fmt "@}@}")
;;

let fprintfx fs fmt p =
 let skip _ = Monad.return() in
 let print tag trs fmt =
   F.fprintf fmt "@{<%s>" tag;
   Trs.fprintfx fmt trs >>= fun _ ->
   Monad.return(F.fprintf fmt "@}")
 in
 let (kind,dps,trs,ur) = if Problem.is_dp p.input then (
   if p.monotone then (
    match p.usable_rules with
     | None    -> (
      "monoRedPairProc",
      print "dps" (Problem.get_dps p.output),
      print "trs" (Problem.get_trs p.output),
      skip
     )
     | Some ur -> (
      "monoRedPairUrProc",
      print "dps" (Problem.get_dps p.output),
      print "trs" (Problem.get_trs p.output),
      print "usableRules" ur
     )
   ) else (
    match p.usable_rules with
     | None    -> (
      "redPairProc",
      print "dps" (Problem.get_dps p.output),
      skip,
      skip
     )
     | Some ur -> (
      "redPairUrProc",
      print "dps" (Problem.get_dps p.output),
      skip,
      print "usableRules" ur
     )
   )
 ) else if Problem.is_sp p.input then (
  "ruleRemoval",
  skip,
  print "trs" (Problem.get_trs p.output),
  skip
 ) else (
  failwith "matrix relative not supported!"
 ) in
 F.fprintf fmt "@{<%s>" kind;
 fprintfx_intp fmt p >>= fun _ ->
 dps fmt             >>= fun _ ->
 trs fmt             >>= fun _ ->
 ur  fmt             >>= fun _ ->
 List.hd fs fmt      >>= fun _ ->
 Monad.return(F.fprintf fmt "@}")
;;


(* Processor *)
(* administrative functions *)
let (>>=) = Made.(>>=);;

let count f t = List.length (Term.fun_pos f t);;
let count_rule p f r = let lc,rc = Rule.map (count f) r in p lc || p rc;;
let rec parallel = function
 | [] -> true
 | p::ps -> List.for_all (Position.are_parallel p) ps && parallel ps
;;
let nested f t = not (parallel (Term.fun_pos f t));;
let nested_rule f r = let lc,rc = Rule.map (nested f) r in lc || rc;;

let print state f = 
 let _ = Sig.fprintf_fun Format.std_formatter f state in
 let _ = Format.printf " -> ari:%d@\n" (Sig.find_ari f state) in
 ()
;;

let name state f ls =
 let f' = fst (Sig.to_string_fun f state) in
 List.mem f' ls
;;

let apply_heuristic state problem h = 
 let trs = let s,w = Problem.get_sw problem in Trs.union s w in
 let fs = Trs.funs trs in
 let filterfun f = match h with
  | (-1) -> true
  | 0 -> false
  | 1 -> Trs.for_all (fun rule -> not (count_rule ((<) 1) f rule)) trs 
  | 2 -> Trs.for_all (fun rule -> not (count_rule ((<) 2) f rule)) trs 
  | 3 -> Trs.for_all (fun rule -> not (nested_rule f rule)) trs 
  | 4 -> List.mem f (Trs.def_symbols trs)
  | 100 -> name state f ["f";"p"]
  | _ -> failwith "illegal value"
 in
 let fs = List.filter filterfun fs in
 (* List.iter (print state) fs; *)
 fs
;;

let context state problem =
 if (!(flags.rat) > 1 || !(flags.real)) && !(flags.shp) <> Linear && !(flags.neg) then 
  failwith "non-linear negative real polynomials not supported";
 let (s,_) = Pair.map Trs.to_list (Problem.get_sw problem) in
 let g =
  try match Problem.get_dg problem with 
   | Problem.Complete -> Graph.generate (fun n m -> [(n,m)]) s 
   | Problem.Partial x -> x
  with Failure _ -> Graph.generate (fun _ _ -> []) []
 in
 let g = Graph.restrict s g in
 let arith = {
  Logic.min = Int64.of_int !(flags.min);
  neg       = false;
  rat       = !(flags.rat);
  real      = !(flags.real);
  minf      = false}
 in
 {arith              = arith;
  usable             = Hashtbl.create 512;
  coefficients       = Hashtbl.create 512;
  coefficients2      = Hashtbl.create 512;
  constants          = Hashtbl.create 512;
  gt_encodings       = Hashtbl.create 512;
  geq_encodings      = Hashtbl.create 512;
  state              = state;
  shape              = !(flags.shp);
  nlfs               = apply_heuristic state problem !(flags.heuristic);
  orient             = !(flags.o);
  subterm_encodings  = Hashtbl.create 512;
  d_tbl              = Hashtbl.create 512;
  graph              = g;
  constant_encodings = Hashtbl.create 512}
;;

let cache_m tbl f k = 
 if Hashtbl.mem tbl k then return (Hashtbl.find tbl k)
 else (f k >>= fun v -> (Hashtbl.add tbl k v; return v))
;;

(* functions lifted from Logic into Made *)
let fresh_arith = get >>= fun s -> liftm (Logic.fresh_arith s.arith);;
let fresh_arith_spec arith = liftm (Logic.fresh_arith arith);;
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
let is_dp f = get >>= fun c -> return (Sig.is_dp f c.state);;
let arity f = get >>= fun c -> return (Sig.find_ari f c.state);;
let id f = get >>= fun c -> return (fst (Sig.to_string_fun f c.state));;
 
(* matrix encoding starts here *)
let gen fresh triangle m n k = get >>= fun c ->
(*
 let i,j = k / n, k mod n in
 if triangle && i > j then return Logic.zero
 else if triangle && i = j then
  (*restrict to 1*)
  fresh_arith_spec {c.arith with Logic.min = c.arith.Logic.rat}
 else fresh
 *)
fresh
;;

let make_matrix ?(fresh=fresh_arith) triangle m n = 
 Made.sequence (List.gen (gen fresh triangle m n) (m*n)) >>=
 (return <.> M.of_list m n)
;;

let make_coefficient ?(neg=false) f _ = get >>= fun c ->
 is_dp f >>= fun b -> 
 let d = if b then 1 else !(flags.dim) in
 let arith = {c.arith with Logic.neg = neg} in
 make_matrix ~fresh:(fresh_arith_spec arith) !(flags.tri) d !(flags.dim)
;;

let make_constant f = get >>= fun c ->
 is_dp f >>= fun b -> 
 let d = if b then 1 else !(flags.dim) in
 make_matrix false d 1
;;

let zero_matrix = M.make 1 1 C.zero;;
let zero_mono = Mono.make zero_matrix [];;
let zero_poly = P.make [zero_mono];;

let equal_matrix m0 m1 = Logic.big_and (M.map2list C.eq m0 m1);;
let is_zero_matrix m = Logic.top = equal_matrix zero_matrix m;;
let greater_equal_matrix m0 m1 = Logic.big_and (M.map2list C.geq m0 m1);;
let greater_matrix m0 m1 = 
 (C.gt (M.get 0 0 m0) (M.get 0 0 m1)) <&> (greater_equal_matrix m0 m1);;


let coefficient f i = get >>= fun c ->
 cache_m c.coefficients (uncurry (make_coefficient ~neg:!(flags.neg))) (f, i)
;;

let coefficient2 f i j = get >>= fun c ->
 arity f >>= fun a ->
 id f >>= fun fs ->
 if (c.shape = SQuadratic || c.shape = SMixed) && List.mem f c.nlfs then 
  (* let _ = Sig.fprintfx_fun Format.std_formatter f c.state in *)
  cache_m c.coefficients2 (uncurry make_coefficient) (f,(i,j))
 else
  return zero_matrix 
;;

let constant f = get >>= fun c -> cache_m c.constants make_constant f;;

let u t = get >>= fun c ->
 cache_m c.usable (const fresh_bool) (Option.the(Term.root t));;


let cancel_poly p = 
 P.filter (fun m -> not (is_zero_matrix (Mono.coefficient m))) p;;

let squadratic f i te = get >>= fun c ->
 coefficient2 f i i >>= fun fii ->
 if is_zero_matrix fii then return zero_poly else
 match c.shape with
  | SQuadratic -> lift cancel_poly
   (lift2 P.mul (lift2 P.scale (return fii) te) te)
  | _ -> return zero_poly
;;

let smixed f tes = get >>= fun c ->
 arity f >>= fun a ->
 if a < 2 then return zero_poly 
 else
  match c.shape with
   | SMixed -> 
    let ps = List.zip (List.take (a-1) tes) (List.tl tes) in
    List.fold_lefti 
     (fun i p (ti,tj) ->
      coefficient2 f i (i+1) >>= fun cij ->
      lift2 P.add p (return (P.mul (P.scale cij ti) tj)))
     (return zero_poly)
     ps
   | _ -> return zero_poly
;;

let rec encode_term t = get >>= fun c ->
 cache_m c.subterm_encodings (etc c) t
and etc c = function
 | Term.Var x -> return (P.make [Mono.make (M.identity !(flags.dim)) [x]])
 | Term.Fun (f, ts) ->
  map encode_term ts >>= fun tes ->
  (List.fold_lefti (fun i p (t,te) -> 
   (*let te = encode_term t in*)
   lift2 P.add
    (lift2 P.add p (lift2 P.scale (coefficient f i) (return te))) 
    (squadratic f i (return te)) )
  (lift2 P.add (smixed f tes) 
   (lift P.make (sequence [lift2 Mono.make (constant f) (return [])])))
  (List.zip ts tes))
;;


let greater_equal_mono m0 m1 =
 greater_equal_matrix (Mono.coefficient m0) (Mono.coefficient m1)
;;

let greater_mono m0 m1 =
 greater_matrix (Mono.coefficient m0) (Mono.coefficient m1)
;;

let same_variables p0 p1 = 
 let v0,v1 = P.variables p0, P.variables p1 in
 let v0',v1' = List.diff v1 v0@[], List.diff v0 v1@[] in
 let z = M.make 1 1 Logic.zero in
 let zero_p0' = P.make (List.map (Mono.make z) v0') in
 let zero_p1' = P.make (List.map (Mono.make z) v1') in
 let p0,p1 = P.add zero_p0' p0, P.add zero_p1' p1 in
 (p0,p1)
;;

let rec greater_equal_many cs0 cs1 =
 Triple.fst
  (List.foldl (fun (phi,a0,a1) (c0,c1) ->
   ((greater_equal_matrix (M.add a0 c0) (M.add a1 c1) <&> phi),M.add a0 c0,M.add a1 c1)) 
   (Logic.top,zero_matrix,zero_matrix) 
   (List.zip cs0 cs1)
  );;

let greater_equal_poly_x p0 p1 vars = 
 let ps0 = P.split p0 and ps1 = P.split p1 in 
 if List.is_singleton (List.unique vars) then
  let ps0 = List.filter (fun (c,xs) -> vars = List.unique xs) ps0 in
  let ps1 = List.filter (fun (c,xs) -> vars = List.unique xs) ps1 in
  (*3x + 9x^2 + 10x^3 -> [10;9;3] *)
  let cs0, cs1 = List.rev (List.map fst ps0), List.rev (List.map fst ps1) in
  greater_equal_many cs0 cs1
 else (
  let m0 = List.filter (fun (c,xs) -> vars = xs) ps0 in
  let m1 = List.filter (fun (c,xs) -> vars = xs) ps1 in
  match (m0,m1) with
   | [m0], [m1] -> greater_equal_matrix (fst m0) (fst m1)
   | _, _ -> failwith "illegal value"
  )
;;

let greater_equal_poly ?(inc=None) orient p0 p1 =
 let p0,p1 = same_variables p0 p1 in
 if orient then
  let vars = List.unique (List.map (fun vs -> if List.is_singleton
  (List.unique vs) then List.unique vs else vs) (P.variables p0)) in
  Logic.big_and (List.map (greater_equal_poly_x p0 p1) vars)
 else
  Logic.big_and (P.map2 greater_equal_mono p0 p1)
;; 

let greater_poly o p0 p1 = 
 let p0,p1 = same_variables p0 p1 in
 let (c0,c1) = Pair.map P.constant_part (p0,p1) in
 greater_equal_poly o p0 p1 <&> greater_mono c0 c1
;;


(*
let negate p = P.scale (M.make 1 1 Logic.minus_one) p;;
let sub_poly p0 p1 = P.add p0 (negate p1);;
let zero_matrix = M.make 1 1 Logic.zero;;
let zero_mono = Mono.make zero_matrix [];;
let zero_poly = P.make [zero_mono];;

let greater_equal_poly p0 p1 =
 let p = sub_poly p0 p1 in
 let cs = P.coefficients p in
 Logic.big_and (List.map (fun m -> greater_equal_matrix m zero_matrix) cs)
;; 

let greater_poly p0 p1 = 
 let p = sub_poly p0 p1 in
 let c0 = P.constant_part p in
 greater_equal_poly p0 p1 <&> greater_mono c0 zero_mono
;;
*)

let greater_equal_rule rule = get >>= fun c ->
 lift2 (greater_equal_poly c.orient) (encode_term (Rule.lhs rule))
  (encode_term (Rule.rhs rule))
;;

let greater_equal_rule rule = get >>= fun c ->
 cache_m c.geq_encodings (greater_equal_rule) rule
;;

let greater_rule rule = get >>= fun c ->
 lift2 (greater_poly c.orient) (encode_term (Rule.lhs rule))
  (encode_term (Rule.rhs rule))
;;

let greater_rule rule = get >>= fun c ->
 cache_m c.gt_encodings greater_rule rule
;;

(*TODO: sound for weakly monotone negative coefficients*)
let mono cii ci x = 
 if !(flags.real) || !(flags.rat) > 1 then
  (*Lucas condition for reals*)
  (if !(flags.neg) then Logic.bot else C.geq cii C.zero <&> C.geq ci x)
  (*Friedl condition for naturals*)
  else C.geq cii C.zero <&> C.geq (C.add cii ci) x

let mf f x = arity f >>= fun a ->
 gen_and (fun i -> 
  coefficient f i >>= fun ci ->
  coefficient2 f i i >>= fun cii ->
  let ci = M.get 0 0 ci and cii = M.get 0 0 cii in
  return (mono cii ci x)) a
;;

let  monf f = mf f C.one;;
let wmonf f = mf f C.zero;;

let  mon fs = map_and  monf fs;;
let wmon fs = map_and wmonf fs;;

let triangle c = get >>= fun s ->
 return (Logic.big_and (List.gen (fun i -> C.geq C.one (M.get i i c))
  !(flags.dim)))
;;

let triangle f = arity f >>= fun a ->
 gen_and (fun i -> coefficient f i >>= fun c -> triangle c) a ;;

let triangle fs = map_and triangle fs;;

(* not zero matrix *)
let nzm f i =
 coefficient f i >>= fun ci ->
 coefficient2 f i i >>= fun cii ->
 let zm = M.fold (fun elt -> (<&>) (C.eq elt C.zero)) ci Logic.top in
 let zm2 = M.fold (fun elt -> (<&>) (C.eq elt C.zero)) cii zm in
 return (Logic.neg zm2)
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
 let geq = if !(flags.dir) then greater_rule else greater_equal_rule in
 if !(flags.ur) then (
  let ds = Trs.def_symbols (Trs.of_list (s@w)) in
  map_and (geq_ur (<&>) (geq )) s $&$
  map_and (geq_ur (<->>) geq) w $&$
  map_and (process_rule ds) (s@w) 
 ) else 
  map_and geq (s@w)
;;

let encode_gt s w = get >>= fun c -> 
 let s = if !(flags.dp) || !(flags.tri) || !(flags.dir) then s else List.rev_append s w in
 let op = (if !(flags.dir) then Logic.big_and else Logic.big_or) in
 map_op op greater_rule s
;;

let ac_rules fs =
 get >>= fun c ->
 let is_ac f = Sig.is_theory Label.AC f c.state in
 let is_c f = Sig.is_theory Label.C f c.state in
 let acs = List.filter (fun f -> is_ac f || is_c f) fs  in
 let fresh =
  get >>= fun c ->
  let x, s = Sig.fresh_var c.state in
  let n, s = Sig.create_var_name x s in
  set {c with state = Sig.add_var x n s} >>
  return (Term.Var x)
 in
 fresh >>= fun x -> fresh >>= fun y -> fresh >>= fun z ->
 get >>= fun c -> let s = c.state in
 let ac_rules f =
  let c = Rule.of_terms (Term.Fun(f, [x;y])) (Term.Fun(f, [y;x])) in
  let f' = if Sig.is_dp f s then Sig.drop_dp f s else f in
  let a = Rule.of_terms (Term.Fun(f, [x;Term.Fun(f', [y;z])]))
   (Term.Fun(f, [Term.Fun(f', [x;y]); z])) in
  if is_ac f then [a;Rule.invert a;c] else [c]
 in
 return (List.flat_map ac_rules acs)
;;

let encode strict weak = get >>= fun c ->
(*
 Format.printf "%a@\n%!" Trs.fprintf strict;
 Made.iter (fun f ->
 arity f >>= fun a ->
 let is = List.range 0 (a+1) in
 Made.iter (fun i -> coefficient f i >>= fun ci ->
  Format.printf "(%a,%d) = %a@\n%!" 
  Fun.fprintf f i Logic.fprintf_a (M.get 0 0 ci);
  Made.return ()) is >>
  Made.return ()) (Trs.funs (Trs.union strict weak)) >>
*)
 
 let (s, w) = Pair.map Trs.to_list (strict, weak) in
 let fs = Trs.funs (Trs.union strict weak) in
 encode_geq s w $&$
 encode_gt s w $&$
 (if !(flags.dp) then wmon fs else mon fs) $&$
 (if !(flags.tri) then triangle fs else return Logic.top) $&$
 (if !(flags.ac) then ac_rules fs >>= map_and greater_equal_rule
  else return Logic.top)
;;

let decode_rule ass rule = lift not (eval_p (greater_rule rule) ass);;

let decode_trs ass trs = 
 Made.filter (decode_rule ass) (Trs.to_list trs) >>= (return <.> Trs.of_list)
;;

let decode_weak ass w = if !(flags.dp) || !(flags.tri) then return w else decode_trs ass w;;

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

let eval_matrix ass m = m >>= fun m -> 
 let r, c = M.rows m, M.columns m in
 Made.sequence 
  (List.map (fun (i,j) -> liftm (Logic.eval_a (M.get i j m) ass))
  (List.product (List.range 0 r) (List.range 0 c))) >>= fun ms ->
 return (NM.of_list r c ms);;

let get_interpretation ass f = arity f >>= fun a ->
 Made.sequence 
  (List.gen (fun i -> eval_matrix ass (coefficient f i)) a) >>= fun coeffs ->
 Made.sequence 
  (List.gen (fun i -> eval_matrix ass (coefficient2 f i i)) a) >>= fun coeffs2 ->
 let coeffs2 = match !(flags.shp) with
  | Linear -> []
  | SQuadratic -> coeffs2 
 in
 eval_matrix ass (constant f) >>= fun const ->
 return (f, coeffs@coeffs2, const);;

let make_interpretation ass trs = 
 Made.sequence (List.map (get_interpretation ass) (Trs.funs trs)) >>=
  (return <.> List.foldr (fun fi acc -> uncurry3 (Intp.add acc) fi)
   (Intp.empty ()))
;;

let get_degree = function
 | Linear -> 1
 | SQuadratic -> 2
;;

let solve s fs p = 
 let configurate s = F.printf "%s@\n%!" s; flags.help := true in
 (try init (); Arg.parsex code spec fs with Arg.Bad s -> configurate s);
 if !(flags.help) then (Arg.usage spec ("Options for "^code^":"); exit 0);
 let c = context s p in
 let (s,w) = Problem.get_sw p in
 Logic.run ~dbits:!(flags.db) ~obits:!(flags.ob) (
  Made.run c (encode s w >>= fun phi ->
   if !(flags.p) then (
    let logic = 
     if !(flags.real) || !(flags.rat) <> 1 then "QF_NRA" else "QF_NIA"
    in
    Format.fprintf Format.std_formatter "@[%a@]@\n" 
     (Logic.fprintf_smt ~logic:logic) phi;
    return None
   ) else
   Made.liftm (Logic.solve phi) >>= function
    | None -> return None
    | Some ass -> 
     decode ass p >>= fun p' -> decode_ur ass w >>= fun ur ->
     make_interpretation ass (Trs.union s w) >>= fun i ->
     let dim = !(flags.dim) and dir = !(flags.dir) in
     let deg = get_degree !(flags.shp) in
     return (Some (make deg dim dir !(flags.dp) !(flags.tri) i p p' ur))))
;;

(* wrap into state monad *)
let (>>=) = Monad.(>>=);;
let solve fs p = Monad.get >>= fun s -> Monad.return (solve s fs p);;
