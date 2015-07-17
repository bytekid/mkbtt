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
module L = Logic;;
module AF = Filtering;;
module C = Coefficient;;
module Co = Complexity;;
module F = Format;;
module Fun = Function;;
module H = Hashtbl;;
module M = Rewritingx.Monad;;
module Number = L.Number;;
module P = Problem;;
module Pos = Position;;
module Prec = Precedence;;
module Sig = Signature;;
module Var = Variable;;
module R = Rule;;
module WF = Weights;;
module Mono = Monomial.Make (C) (Var);;
module Poly = Polynomial.Make (Mono);;
module NM = Matrix.Make (Number);;
module Intp = Interpretation.Make (NM);;

module Status = struct
 (*** INCLUDES ****************************************************************)
 include Index.Make (Fun) (Bool);;

 (*** FUNCTIONS ***************************************************************)
 let (>>=) = M.(>>=);;

 let is_lex s f = not (find f s);;
 let is_mul s f = find f s;;

 let fprintf fmt p =
  let rec fprintf fmt = function
   | [] -> ()
   | (f,b) :: p ->
    if b then F.fprintf fmt "%a:mul@ @ %a" Fun.fprintf f fprintf p
    else F.fprintf fmt "%a:lex@ @ %a" Fun.fprintf f fprintf p
 in
 F.fprintf fmt "@[%a@]" fprintf (to_list p)
;;

let fprintfm fmt p =
 let rec fprintfm = function
  | [] -> M.return ()
  | (f,b) :: p ->
   M.fprintf_fun fmt f >>= fun _ ->
   if b then F.fprintf fmt ":lex @ " else F.fprintf fmt ":mul @ ";
   fprintfm p
 in
 M.filter (fun (f,_) -> M.is_theory Label.AC f >>= (M.return <.> not)) (to_list p) >>=
 M.filter (fun (f,_) -> M.find_ari f >>= fun a -> M.return (a > 1)) >>=
 (F.fprintf fmt "@["; fprintfm) >>= fun _ ->
 M.return (F.fprintf fmt "@]")
;;

let fprintfx fmt p s =
  let p = Prec.to_list p in
  F.fprintf fmt "@{<statusPrecedence>";
  M.iter (fun(f,b) ->
    F.fprintf fmt "@{<statusPrecedenceEntry>";
    M.fprintfx_fun fmt f >>= fun _ ->
    M.find_ari f         >>= fun a ->
    F.fprintf fmt "@{<arity>%i@}" a;
    F.fprintf fmt "@{<precedence>%i@}" (List.assoc f p);
    F.fprintf fmt "@{<%s>@}" (if b then "mul" else "lex");
    M.return(F.fprintf fmt "@}")
  ) (to_list s) >>= fun _ ->
  M.return(F.fprintf fmt "@}")
;;

end;;




(*** TYPES ********************************************************************)

type context = {
 arith                 : L.arith;
 solver                : L.solver;
 prec_num              : int;
 state                 : Signature.t;
 precs                 : (Fun.t, L.a) H.t;
 gt_encodings          : (R.t, L.p) H.t;
 (* encoding statuses of function symbols; true is multiset *)
 stats                 : (Fun.t, L.p) H.t;
 w0                    : L.a ref;
 weights               : (Fun.t, L.a) H.t;
 subterm_encodings     : (Term.t,Poly.t) Hashtbl.t;
 coefficients          : (Fun.t*int,C.t) Hashtbl.t;
 function_symbols : Fun.t list;
 constants             : (Fun.t,C.t) Hashtbl.t;
 p_gt_encodings        : (R.t,L.p) Hashtbl.t;
 p_geq_encodings       : (R.t,L.p) Hashtbl.t;
 af_l                  : (Fun.t, L.p) H.t;
 af_p                  : (Fun.t * int, L.p) H.t;
 emb_gt_encodings      : (R.t, L.p) H.t;
 emb_eq_encodings      : (R.t, L.p) H.t;
 eq_af_encodings       : (Fun.t*Fun.t*Term.t list*Term.t list, L.p) H.t;
 kbo_af_same_encodings : (Fun.t*Term.t list*Term.t list,L.p) H.t;
 kbo_af_diff_encodings : (Fun.t*Fun.t*Term.t list*Term.t list,L.p) H.t;
 usable_funs           : (Fun.t, L.p) H.t
};;

type flags = {
 ac0 : bool ref;
 direct : bool ref;
 dp : bool ref;
 help : bool ref;
 kv : bool ref;
 kv2 : bool ref;
 min : int ref;
 ob : int ref;
 quasi : bool ref;
 sat : bool ref ;
 sc : bool ref;
 st : bool ref;
 steinbach : bool ref;
 total_prec : bool ref;
 ur : bool ref
};;

type w = Weight of WF.t | Inter of Intp.t 

type t = {
 af : AF.t option; 
 input : P.t;
 output : P.t;
 precedence : Prec.t;
 status : Status.t;
 urs : Trs.t;
 wf : w;
};;

(*** GLOBALS ******************************************************************)
let code = "ackbo";;
let name = "AC-KBO Processor";;
let comment = "Applies AC-KBO."

let keywords =
 ["AC";"knuth-bendix order";"simplification order";"termination"]
;;

let flags = {
 ac0 = ref false;
 direct = ref false;
 dp = ref false;
 help = ref false;
(* quasi = ref false;*)
 kv = ref false;
 kv2 = ref false;
 min = ref 1;
 ob = ref max_int;
 quasi = ref false;
 sat = ref true;
 sc = ref false;
 st = ref false;
 steinbach = ref false;
 total_prec = ref true;
 ur = ref false
};;

let spec =
 let spec = [
  ("-ac0",Arg.Set flags.ac0,
   "in case of Steinbach's order, give AC symbols weight 0.");
  ("-af",Arg.Set flags.dp,
   "Allows non-monotone interpretations (argument filterings).");
  ("-direct",Arg.Set flags.direct,
   "Try to finish termination proof.");
  ("--help",Arg.Set flags.help,"Prints information about flags.");
  ("-help",Arg.Set flags.help,"Prints information about flags.");
  ("-h",Arg.Set flags.help,"Prints information about flags.");
  ("-ib",Arg.Int ((:=) flags.min <.> Int.bit_max),
   "Defines the number of bits used to represent weigths (same as \
   `-min' but in bits).");
  ("-kv",Arg.Set flags.kv,"Uses Korovin & Voronkov's ordering.");
  ("-kv2",Arg.Unit (fun _ -> flags.kv := true; flags.kv2 := true),
   "Uses corrested Korovin & Voronkov's ordering.");
  ("-max",Arg.Int ((:=) flags.ob <.> Int.bits),
   "Defines the maximum number that can appear as intermediate result.");
  ("-min",Arg.Set_int flags.min,
   "Defines the minimum weight that should be representable.");
  ("-ob",Arg.Set_int flags.ob,
   "Defines the number of bits used to represent intermediate results
    (same as `-max' but in bits)");
  ("-q",Arg.Unit (fun _ ->  flags.quasi := true; flags.total_prec := false),
   "Uses quasi-precedences.");
  ("-sat",Arg.Set flags.sat,"Uses SAT backend (default).");
  ("-sc",Arg.Set flags.sc,"Uses subterm coefficients.");
  ("-smt",Arg.Clear flags.sat,"Uses SMT backend.");
  ("-st",Arg.Set flags.steinbach,"Uses Steinbach's ordering");
  ("-nt",Arg.Clear flags.total_prec,"Allow non-total precedences \
   (not compatible with -kv).");
  ("-ur",Arg.Set flags.ur,"Apply usable rules.");
 ]
 in
 Arg.alignx 80 spec
;;

let help = (comment,keywords,List.map Triple.drop_snd spec);;

(*** MODULES (part 2) *********************************************************)
module Statex = struct type t = context end;;
module Made = Util.Monad.Transformer.State (Statex) (L.Monad);;
open Made;;

(*** FUNCTIONS ****************************************************************)
let init _ =
 flags.direct := false;
 flags.dp := false;
 flags.help := false;
 flags.ob := max_int;
 flags.sat := true;
 flags.st := false;
 flags.ur := false
;;

(* Constructors and Destructors *)
let make af input output precedence (*st*) urs wf = {
 af = af;
 input = input;
 output = output;
 precedence = precedence;
 status = (*st*)Status.empty;
 urs = urs;
 wf = wf
};;

let get_ip p = p.input;;
let get_op p = p.output;;

(* Complexity Bounds *)
let complexity c _ = Co.mul c Co.other;;

(* Compare Functions *)
let equal p q =
 P.equal p.input q.input && P.equal p.output q.output
;;

(* Printers *)
let (>>=) = M.(>>=);;

let fprintf_af fmt = function
 | None -> Monad.return ()
 | Some af ->
  F.fprintf fmt "@\n@[<1>argument filtering:@\n";
  AF.fprintfm fmt af >>= fun _ -> Monad.return (F.fprintf fmt "@]")
;;

let fprintf fs fmt p  = 
 F.fprintf fmt "@[<1>%s:" name;
 fprintf_af fmt p.af >>= fun _ ->
 F.fprintf fmt "@]@\n@[<1>precedence:@\n";
 Prec.fprintfm fmt p.precedence >>= fun _ ->
 (match p.wf with
 | Weight wf -> 
  F.fprintf fmt "@\n@[<1>weight function:@\n";
  WF.fprintfm fmt wf
 |Inter i ->
  F.fprintf fmt "@\n@[<1>weight function:@\n";
  Intp.fprintf fmt i
 ) >>= fun _ ->
 (if !(flags.ur) then
  (F.fprintf fmt "@\n@[<1>usable rules:@\n";
  Trs.fprintfm fmt p.urs)
 else Monad.return ()) >>= fun _ ->
 (*F.fprintf fmt "@]@\n@[<1>status:@\n";
 Status.fprintfm fmt p.status >>= fun _ ->*)
 F.fprintf fmt "@]@\n@[<1>problem:@\n";
 P.fprintfm fmt p.output >>= fun _ ->
 F.fprintf fmt "@]@\n"; List.hd fs fmt >>= fun _ ->
 Monad.return (F.fprintf fmt "@]")
;;

let fprintfx_af fmt = function
  | None    -> Monad.return()
  | Some af -> AF.fprintfx fmt af
;;

let fprintfx_redpair fmt p =
  F.fprintf fmt "@{<redPair>@{<pathOrder>";
  Status.fprintfx fmt p.precedence p.status >>= fun _ ->
  fprintfx_af fmt p.af           >>= fun _ ->
  M.return(F.fprintf fmt "@}@}")
;;

let fprintfx fs fmt p = 
 if (Problem.is_dp p.input) || (Problem.is_edp p.input) then (
  let tag = "redPairProc" in
  F.fprintf fmt "@{<%s>" tag;
  fprintfx_redpair fmt p         >>= fun _ ->
  Problem.fprintfx fmt p.output  >>= fun _ ->
  List.hd fs fmt                 >>= fun _ ->
  Monad.return(F.fprintf fmt "@}");
) else if (Problem.is_sp p.input) || (Problem.is_ep p.input)  then (
  failwith "AC-RPO full termination not supported!"
) else (
  failwith "for this problem AC-RPO is not supported!"
)
;;

(* general functions *)
let (>>=) = Made.(>>=);;

(* functions lifted from L into Made *)
let fresh_arith_spec spec = liftm (L.fresh_arith spec);;
let fresh_arith = get >>= fun s -> liftm (L.fresh_arith s.arith);;
let fresh_bool = get >>= fun s -> liftm L.fresh_bool;;

let eval_a a ass = a >>= fun a -> liftm (L.eval_a a ass);;
let eval_p p ass = p >>= fun p -> liftm (L.eval_p p ass);;

let ($>=$) a b = lift2 (<>=>) a b;;
let ($=$) a b = lift2 (<=>) a b;;
let ($>$) a b = lift2 (<>>) a b;;
let ($->$) a b = lift2 (<->>) a b;;
let ($|$) a b = lift2 (<|>) a b;;
let ($&$) a b = lift2 (<&>) a b;;
let ($*$) a b = lift2 (<*>) a b;;
let ($+$) a b = lift2 (<+>) a b;;
let ($-$) a b = lift2 (<->) a b;;
let mnot a = lift (~!) a;;
let zero = L.zero;;
let one = L.one;;
let constant = L.constant <.> Number.of_int;; 
let sum f = List.foldl (fun s x -> f x <+> s) L.zero;;
let big_xor ps = sum (fun p -> p <?> one <:> zero) ps <=> one;;
let mtop _ = return L.top;;

let map_op op f ls = sequence (List.map f ls) >>= (return <.> op);;
let mapi_op op f ls = sequence (List.mapi f ls) >>= (return <.> op);;
let gen_op op f n = sequence (List.gen f n) >>= (return <.> op);;
let map_and f = map_op L.big_and f;;
let mapi_and f = mapi_op L.big_and f;;
let mapi_or f = mapi_op L.big_or f;;
let map_or f = map_op L.big_or f;;
let map_add f = map_op L.big_sum f;;
let top = L.top;;
let bot = L.bot;;

(* actual content starts here *)
let context state fs =
 let solver = if !(flags.sat) then L.MiniSat else L.Yices in
 let arith = {
  L.min = Int64.of_int !(flags.min);
  neg = false;
  rat = 1;
  real = false;
  minf = false}
 in
 {
  arith             = arith;
  solver            = solver;
  prec_num          = max 0 (List.length fs - 1);
  precs             = H.create 512;
  state             = state;
  gt_encodings      = H.create 512;
  stats             = H.create 512;
  w0                = ref L.one;
  weights           = H.create 512;
  subterm_encodings = H.create 512;
  coefficients      = H.create 512;
  function_symbols = [];
  constants         = H.create 512;
  p_gt_encodings      = H.create 512;
  p_geq_encodings     = H.create 512;
  (* afs *)
  af_l              = H.create 512;
  af_p              = H.create 512;
  emb_gt_encodings  = H.create 512;
  emb_eq_encodings  = H.create 512;
  eq_af_encodings   = H.create 512;
  kbo_af_same_encodings = H.create 512;
  kbo_af_diff_encodings = H.create 512;
  usable_funs = H.create 64;
 }
;;

(* administrative functions *)
let cache_m tbl f k = 
 if H.mem tbl k then return (H.find tbl k)
 else (f k >>= fun v -> (H.add tbl k v; return v))
;;

(* encoding starts here *)
(* caching variables *)
let weight f = get >>= fun c -> cache_m c.weights (const fresh_arith) f;;

let prec f = get >>= fun c -> 
 let arith = L.nat c.prec_num in
 cache_m c.precs (const (fresh_arith_spec arith)) f
;;

let prec_gt f g = prec f $>$ prec g;;
let prec_lt f g = prec g $>$ prec f;;
let prec_le f g = prec g $>=$ prec f;;
let prec_ge f g = prec f $>=$ prec g;;
let prec_eq f g = return (if f = g then L.top else L.bot);; (* not quasi *)
let prec_n_lt f g = mnot (prec_lt f g);;
let prec_n_le f g = mnot (prec_le f g);;
let is_mul f = get >>= fun c -> cache_m c.stats (const fresh_bool) f;;
let is_lex = mnot <.> is_mul;;
let is_ac f = get >>= fun c -> return (Sig.is_theory Label.AC f c.state);;
let is_c f = get >>= fun c -> return (Sig.is_theory Label.C f c.state);;
let set_ac f =
 get >>= fun c -> let (f, s) = Sig.set_theory f Label.AC c.state in
 Made.set {c with state = s} >>
 return f
;;
let af_l f = get >>= fun c -> cache_m c.af_l (const fresh_bool) f
let af_n f = af_l f >>= (return <.> L.neg)
let af_p f i = get >>= fun c -> cache_m c.af_p (const fresh_bool) (f,i)
let af_ac_all f = (af_l f) $&$ (af_p f 0) $&$ (af_p f 1)
let af_ac_none f = (af_l f) $&$ (mnot ((af_p f 0) $|$ (af_p f 1)))

let us_f f = get >>= fun c -> cache_m c.usable_funs (const fresh_bool) f

let set_fs fs = get >>= fun c -> Made.set {c with function_symbols = fs};;
let get_fs = get >>= fun c -> return c.function_symbols;;
let ac_symbols = get_fs >>= Made.filter is_ac;;
let c_symbols = get_fs >>= Made.filter is_c;;
let count_ac_symbols = ac_symbols >>= (return <.> List.length);;

(* quasi precedence admissible, i.e., AC-symbol never equal to
   non-AC symbol *)
let quasi_adm =
 get_fs >>= fun fs -> ac_symbols >>= fun acs ->
 get_fs >>= fun fs -> c_symbols >>= fun cs ->
 let ps = List.product (*(List.diff fs acs)*) fs (acs @ cs) in
 let prec_neq (f,g) = if f=g then return top else mnot (prec f $=$ (prec g)) in
 map_and prec_neq ps
;;

let max u fs = get >>= fun c ->
(* if not !(flags.total_prec) then
  map_and (fun f -> if f = u then return L.top else mnot (prec_lt u f)) fs
 else*)
  map_and (fun f -> if f = u then return L.top else prec_gt u f) fs
;;

let min g fs =
 if not !(flags.total_prec) then
  map_and (fun f -> if f = g then return L.top else mnot (prec_lt g f)) fs
 else 
  map_and (fun f -> if f = g then return L.top else prec_lt f g) fs
;;

let arity f = get >>= fun c -> return (Sig.find_ari f c.state);;
let n_ary n fs = Made.filter (fun f -> lift2 (=) (arity f) (return n)) fs;;
let w0 = get >>= fun c -> return !(c.w0);;

let make_constant f = get >>= fun c ->
 fresh_arith_spec c.arith
;;

let make_coefficient f _ = get >>= fun c ->
 fresh_arith_spec c.arith
;;

let coefficient_of f i = 
 is_ac f >>= fun f_is_ac ->
 if f_is_ac then 
  return C.one
 else
  get >>= fun c -> cache_m c.coefficients (uncurry make_coefficient) (f,i)
;;

let constant_of f = get >>= fun c -> cache_m c.constants make_constant f;;

let constants fs = n_ary 0 fs >>= fun cs ->
 map_and (fun con -> weight con $>=$ w0) cs
;;

let unary fs = n_ary 1 fs >>= fun us ->
 map_and (fun u -> (weight u $=$ return C.zero) $->$ (max u fs)) us
;;

let adm fs = get >>= fun c ->
 (w0 $>$ return C.zero) $&$ constants fs $&$ unary fs
;;

let steinbach fs = 
 let admissible f = 
  (if !(flags.ac0) then (weight f $=$ return C.zero) else (return top))
  $&$ min f fs
 in ac_symbols >>= map_and admissible
;;

(* admissibility with subterm coefficients *)
let adm_sc fs = get >>= fun c ->
 let c = n_ary 0 fs >>= map_and (fun c -> constant_of c $>$ return C.zero) in
 let u = n_ary 1 fs >>= 
  map_and (fun u -> (constant_of u $=$ return C.zero) $->$ (max u fs)) 
 in
 let ac = ac_symbols >>= fun acs -> c_symbols >>= fun cs ->
  let coeff_is_one f i = coefficient_of f i $=$ (return C.one) in
  map_and (fun ac -> coeff_is_one ac 0 $&$ (coeff_is_one ac 1)) (cs@acs)
 in c $&$ u $&$ ac
;;

let mon fs = 
 let cs f = arity f >>= (Made.sequence <.> (List.gen (coefficient_of f))) in
 Made.flat_map cs fs >>= map_and (fun m -> return (C.geq m C.one))
;;

(* polynomial operations for subterm coefficients *)
let rec encode_term t = get >>= fun c ->
 cache_m c.subterm_encodings (etc c) t
and etc c = function
 | Term.Var x -> return (Poly.make [Mono.make C.one [x]; Mono.make C.zero []])
 | Term.Fun (f, ts) ->
  (List.fold_lefti (fun i p t ->
   lift2 Poly.add p (lift2 Poly.scale (coefficient_of f i) (encode_term t)))
  (lift Poly.make (sequence [lift2 Mono.make (constant_of f) (return [])]))
  ts)
;;

let greater_mono m0 m1 = C.gt (Mono.coefficient m0) (Mono.coefficient m1);;
let greater_equal_mono m0 m1 = C.geq (Mono.coefficient m0) (Mono.coefficient m1);;

let greater_equal_poly p0 p1 =
 let (v0,v1) = Pair.map Poly.variables (p0,p1) in
 if not (List.is_subset v1 v0) then
   L.bot
 else
  let p0 = Poly.filter (fun m -> List.mem (Mono.variables m) v1) p0 in
  let cs = Poly.map2 greater_equal_mono p0 p1 in
  L.big_and cs
;;

let equal_poly p0 p1 = 
 greater_equal_poly p0 p1 <&> greater_equal_poly p1 p0
;;

let greater_poly p0 p1 =
 let c0 = Poly.constant_part p0 in
 let c1 = try Poly.constant_part p1 with Not_found ->
  Mono.make C.zero []
 in
 let (nc0, nc1) = Pair.map Poly.non_constant_part (p0,p1) in
 greater_mono c0 c1 <&> greater_equal_poly nc0 nc1
;;

let greater_equal_rule rule =
 lift2 greater_equal_poly (encode_term (R.lhs rule))
  (encode_term (R.rhs rule))
;;

let greater_equal_rule_s rule =
 let ncp = Poly.non_constant_part in
 encode_term (R.lhs rule) >>= fun l ->
 encode_term (R.rhs rule) >>= fun r ->
 return (greater_equal_poly (ncp l) (ncp r))
;;

let greater_rule rule =
 lift2 greater_poly (encode_term (R.lhs rule))
  (encode_term (R.rhs rule))
;;

let mon_m m = return (C.geq m C.one);;

let coefficients f = arity f >>= fun a ->
 Made.sequence (List.gen (coefficient_of f) a)
;;

let matrices fs = Made.flat_map coefficients fs;;

let mon fs = matrices fs >>= fun ms -> map_and mon_m ms;;
(* end polynomials *)


let rec symbols_list = function
 | Term.Var _ -> []
 | Term.Fun (f,ts) -> f::List.flat_map symbols_list ts;;

let rec diff a b = List.foldr List.remove a b;;


let rec vars_list = function
 | Term.Var x -> [x]
 | Term.Fun (_,ts) -> List.flat_map vars_list ts;;

let weight_fs rule =
 let fsl, fsr = R.map symbols_list rule in
 let v = Util.uncurry diff (R.map vars_list rule) in
 let wl, wr = diff fsl fsr, diff fsr fsl in
 let v = C.of_int (List.length v) in
 w0 >>= fun w0 ->
 let xs = sequence (return (C.scale v w0)::List.map weight wl) in
 lift C.big_sum xs >>= fun sl ->
 lift C.big_sum (sequence (List.map weight wr)) >>= fun sr ->
 return (sl, sr)
;;

(* *** *)
let find_term_with f =
 let rooted_f = function Term.Fun (g,_) when f=g -> true | _ -> false in
 let rec find ys = function
  | [] -> None
  | x::xs when rooted_f x -> Some(x, ys@xs)
  | x::xs -> find (x::ys) xs
 in find []
;;

(* sorts list of terms such that constants come before compound
   terms where the root has positive arity come before variables *)
let rec lex = function
  | [],[] -> 0
  | [],_ -> -1
  |_, [] -> 1
  | x::xs,y::ys -> let c = my_compare x y in if c=0 then lex (xs,ys) else c
and my_compare t t' =
 match t, t' with
  | Term.Var x, Term.Var y -> Var.compare x y
  | Term.Fun _, Term.Var _ -> -1
  | Term.Var _, Term.Fun _ -> 1
  | Term.Fun(_,[]), Term.Fun(_,_::_) -> -1
  | Term.Fun(_,_::_), Term.Fun(_,[]) -> 1
  | Term.Fun(f,fs), Term.Fun(g,gs) when f=g -> lex (fs,gs)
  | Term.Fun(f,_), Term.Fun(g,_) -> Fun.compare f g
;;

(* recursively flatten term with respect to all ac symbols *)
let rec top_flat f = function
 | [] -> []
 | (Term.Fun(f', ts)) :: ts' when f = f' -> top_flat f (ts @ ts')
 | t :: ts -> t :: (top_flat f ts)
;;

let top_flatq f ts =
 let rec flat f p = function
  | [] -> return []
  | (Term.Fun(f', ts)) :: ts' when f = f' -> flat f p (ts @ ts')
  | ((Term.Fun(h, ts)) as t) :: ts' ->
   is_ac h >>= fun h_is_ac -> if h_is_ac then
    prec_eq f h >>= fun f_eq_h -> 
    lift2 (@) (flat f (p <&> f_eq_h) ts) (flat f p ts')
   else  flat f p ts' >>= fun res -> return ((p,t) :: res)
  | _ -> failwith "no variable expected in top_flatq"
 in flat f L.top ts
;;

let rec args_roots = function
 | [] -> []
 | ((Term.Fun(f, _)) as t) :: ts -> (f,t)::(args_roots ts)
 | _ :: ts -> args_roots ts
;;


let rec term_is_embedded s t = match (s,t) with
  | Term.Var x, Term.Var y -> x = y
  | Term.Fun (f,ss), Term.Fun (g,ts) ->
   (f = g 
    && List.length ss = List.length ts 
    && List.for_all2 term_is_embedded ss ts) || List.exists (term_is_embedded s) ts
  | _, Term.Fun (f,ts) -> List.exists (term_is_embedded s) ts
  | _, _ -> false
;;

let is_embedded = Util.uncurry (flip term_is_embedded) <.> R.to_terms;;
let is_proper_embedded r =
  List.exists (term_is_embedded (R.rhs r)) (Term.subterms (R.lhs r))
 ;;
(* *** *)

(* multiset cover arith variable caching, where key=(ss,tt,j) *)
let mcov_a_var key =
 let m = List.length (Triple.fst key) -1 in
 get >>= fun c -> let arith = L.nat m in
(* cache_m c.mcov_a_encodings*) (const (fresh_arith_spec arith)) key
;;

(* encoding *)
let rec lex gt eq ts ss = match ts,ss with
 | _::_,[] -> return L.top
 | [], _ -> return L.bot
 | ti::ts,si::ss -> 
  gt (R.of_terms ti si) $|$ (eq (R.of_terms ti si) $&$ lex gt eq ts ss)
;;

(* multiset cover *)
let mul_cover ss ts =
 let aux (ss,ts) =
  Made.mapi (fun j _ -> mcov_a_var (ss,ts,j)) ts >>= fun cs ->
  let m = List.length ss in
  let sm = L.big_and (List.map (fun ci -> ci <<> (constant m)) cs) in
  return (sm,cs)
 in (*get >>= fun c -> cache_m c.mcov_encodings*) aux (ss,ts)
;;


let mul gt eq ss ts =
 mul_cover ss ts >>= fun (cover,cs) ->
 let tcs = List.zip ts cs in
 mapi_and (fun i si -> mapi_and (fun j (tj,cj) -> 
  (return (cj <=> (constant i))) $->$ (gt (R.of_terms si tj))
  ) tcs) ss >>= fun phi ->
 return (cover <&> phi)
;;

(* recursively flatten term with respect to all ac symbols;
   in order to check equality modulo AC *)
let rec flatten = function
 | Term.Fun(f, ts) ->
  is_ac f >>= fun f_is_ac -> (
  if f_is_ac then
   Made.map flatten ts >>= fun ts' ->
   match find_term_with f ts' with
    | None -> Made.return (Term.Fun(f, List.sort my_compare ts'))
    | Some (ti,ts'') -> flatten (Term.Fun(f, (ts''@(Term.args ti))))
  else
   Made.map flatten ts >>= fun us -> Made.return (Term.Fun(f,us)))
 | v -> Made.return v
;;

let l_if b = if b then L.top else L.bot

let ac_eq l r =
 project flatten (l,r) >>= fun (l,r) ->
 return (Term.compare l r = 0)
;;


let diffm xs ys =
 let rec rem x = function
  | [] -> return []
  | y::ys ->
   ac_eq x y >>= fun c ->
   if c then return ys else (rem x ys >>= fun ys' -> return (y::ys'))
 in foldl (flip rem) xs ys
;;

let rec remove_equal = function
 | [],[] -> return ([],[])
 | x::xs,y::ys ->
  ac_eq x y >>= fun b ->
  if b then remove_equal (xs,ys) else return (x::xs,y::ys)
 | _ -> failwith "remove_equal: lists must have equal length"
;;

let ac_eq = (lift l_if) <.> R.uncurry ac_eq


let diffm2 xs ys = 
 diffm xs ys >>= fun xs' ->
 diffm ys xs >>= fun ys' ->
 return (xs',ys')
;;

(* conditional multiset comparisons *)
let mul_g' cs p gt eq ss ts =
 let tcs = List.zip ts cs in
 let gt si tj = gt (R.of_terms si tj) in
 let has_val cj i = return (cj <=> (constant i)) in
 mapi_and (fun j (tj,cj) -> 
  (p tj) $->$ 
  (mapi_and (fun i si -> has_val cj i $->$ (gt si tj $&$ (p si))) ss)) 
 tcs
;;

let mul_ge' p gt eq ss ts =
 diffm2 ss ts >>= fun (ss,ts) ->
 mul_cover ss ts >>= fun (cover,cs) ->
 mul_g' cs p gt eq ss ts >>= fun phi ->
 return (cover <&> phi)
;;

(* v2 *)
let mul_g2' cs p gt eq ss ts =
 let tcs = List.zip ts cs in
 let geq si tj = let r = R.of_terms si tj in gt r $|$ (eq r) in
 let gt si tj = gt (R.of_terms si tj) in
 let has_val cj i = return (cj <=> (constant i)) in
 mapi_and (fun j (tj,cj) ->
  (p tj) $->$
  (mapi_and (fun i si -> has_val cj i $->$ ((geq si tj) $&$ (p si))) ss))
 tcs >>= fun all_covered ->
 mapi_or (fun j (tj,cj) ->
  (p tj) $&$
  (mapi_and (fun i si -> has_val cj i $->$ (gt si tj $&$ (p si))) ss))
 tcs >>= fun some_strict ->
 map_or p ss $&$ map_and (mnot <.> p) ts >>= fun ts_empty ->
 return (all_covered <&> (some_strict <|> ts_empty))
;;


let mul_eq' p eq ss ts =
  mul_cover ss ts >>= fun (cover,cs) ->
  let tcs = List.zip ts cs in
  let eq si tj = eq (R.of_terms si tj) in
  let has_val cj i = return (cj <=> (constant i)) in
  mapi_and (fun j (tj,cj) ->
   (p tj) $->$
   (mapi_and (fun i si -> has_val cj i $->$ ((eq si tj) $&$ (p si))) ss))
  tcs >>= fun equal ->
  mapi_and (fun j (tj,cj) ->  
   mapi_and (fun j' (tj',cj') ->  
    if j = j' then return L.top 
    else (p tj $&$ (p tj')) $->$ (return (~! (cj <=> cj')))
   ) tcs
  ) tcs >>= fun all_different ->
 return (cover <&> equal <&> all_different) 
;;

let mul_gt2_aux p gt eq ss ts =
 diffm2 ss ts >>= fun (ss,ts) ->
 mul_cover ss ts >>= fun (cover,cs) ->
 mul_g2' cs p gt eq ss ts >>= fun phi ->
 return (cover <&> phi)
;;

let mul_geq' p gt geq ss ts =
 mul_gt2_aux p gt geq ss ts >>= fun gt ->
 mul_eq' p geq ss ts >>= fun eq ->
 mul_eq' p geq ts ss >>= fun eq' ->
 return ((eq <&> eq') <|> gt)
;;
(* *)


let mul_gt' p gt eq ss ts =
 diffm ss ts >>= fun ss ->
(* if !(flags.kv) then*)
  mul_gt2_aux p gt eq ss ts
(* else (* more efficient *)
  mul_ge' p gt eq ss ts $&$ (map_or p ss)*)
;;

let mul_gt = mul_gt' (fun _ -> return L.top)

let size = List.length

let fun_cond cmp f u = 
 match Term.root u with
  None -> return L.bot | Some h -> cmp h f 
;;

(* auxiliary relations for Korovin/Voronkov *)
let eq_w_prec rule =
 if R.is_duplicating rule || (R.is_duplicating (R.invert rule))
  then return L.bot
  else let l,r = R.to_terms rule in 
   if Term.root l <> (Term.root r) then return L.bot
   else weight_fs rule >>= fun (wl, wr) -> return (wl <=> wr)
;;

let geq_w_prec rule =
 if R.is_duplicating rule || (R.is_duplicating (R.invert rule))
  then return L.bot
  else 
   weight_fs rule >>= fun (wl, wr) ->
   match R.to_terms rule with
    | Term.Fun(f,_), Term.Var _ -> 
     (return (wl <=> wr)) $&$ (get_fs >>= max f)
    | Term.Fun (f, _), Term.Fun (g, _) when f = g -> return (wl <=> wr)
    | _ -> return L.bot
;;
 

let gt_w_prec rule =
 if R.is_duplicating rule then return L.bot
 else
  weight_fs rule >>= fun (wl, wr) ->
  match R.to_terms rule with
   | Term.Fun (f, _), Term.Fun (g, _) -> 
    ((return (wl <=> wr)) $&$ (prec_gt f g)) $|$ (return (wl <>> wr))
   | _ -> return (wl <>> wr) 
;;

let compare_weights rule =
 if !(flags.sc) then Made.pair (greater_rule, greater_equal_rule) rule
 else weight_fs rule >>= fun (wl, wr) -> return (wl <>> wr,wl <=> wr)
;;

let rec kbo_gt rule =
 let helper rule =
  if (R.is_duplicating rule && (not !(flags.sc))) || 
   is_embedded (R.invert rule) then
   return L.bot
  else if is_proper_embedded rule then
   return L.top
  else (
   compare_weights rule >>= fun (w1,w2) ->
   (match R.to_terms rule with
    | Term.Var _, _ -> return L.bot
    | Term.Fun _, Term.Var _ -> return L.top
     (* all dangerous cases are considered above*)
    | Term.Fun (f0, ts0), Term.Fun (f1, ts1) -> 
     get >>= fun c ->
     (if f0 = f1 then (
        is_ac f0 >>= fun f0_is_ac -> is_c f0 >>= fun f0_is_c ->
        if not (f0_is_ac || f0_is_c) then ((* case 3 *)
          remove_equal (ts0,ts1) >>= fun (ts0',ts1') ->
          if (ts0' = [] || ts1' = []) then return L.bot
          else kbo_gt (R.of_terms (List.hd ts0') (List.hd ts1')))
        else if f0_is_c then st_gt f0 ts0 ts1 (* multiset extension *)
        else 
          if !(flags.steinbach) then st_gt f0 ts0 ts1
          else if !(flags.kv) then kv_gt f0 ts0 ts1
          else gt f0 ts0 ts1
     ) else  (* case 2 - different function symbols *)
      prec_gt f0 f1
     )) >>= fun aux -> return (w1 <|> (w2 <&> aux))) in
  get >>= fun c -> cache_m c.gt_encodings helper rule

and kbo_ge rule = kbo_gt rule $|$ (ac_eq rule)

(* standard setting *)
and gt f ts0 ts1 =
 let ss, ts = top_flat f ts0, top_flat f ts1 in
 let vs = diff (List.filter Term.is_var ts) (List.filter Term.is_var ss) in
 let sf = List.filter Term.is_fun ss in
 let tf = List.filter Term.is_fun ts in
 let ts' = vs @ tf in
(* Format.printf "compare %a %a\n%!" Term.fprintf (Term.Fun(f,sf))
  Term.fprintf (Term.Fun(f,ts'));*)
 (* case 4a *)
 (*let gt_f = fun_cond prec_gt f in*)
 let nlt_f = fun_cond prec_n_lt f in
 mul_gt' nlt_f kbo_gt ac_eq sf ts' >>= fun mgt ->
 (* case 4b *)
 mul_ge' nlt_f kbo_gt ac_eq sf ts' >>= fun mge ->
 let sgt, sge = size ss > (size ts), size ss >= (size ts) in
(* Format.printf "%i > %i\n%!" (size ss) (size ts);*)
 (* case 4c *)
 let lt_f = fun_cond prec_lt f in
 mul_gt' lt_f kbo_gt ac_eq sf tf >>= fun mgt' ->
 return (mgt <|> (mge <&> ((l_if sgt) <|> ((l_if sge) <&> mgt'))))

(* Steinbach's ordering *)
and st_gt f ts0 ts1 =
 let ss, ts = top_flat f ts0, top_flat f ts1 in
 mul_gt kbo_gt ac_eq ss ts

(* Korovin/Voronkov's ordering *)
and kv_gt f ts0 ts1 =
 let ss, ts = top_flat f ts0, top_flat f ts1 in
 let ts' = diff ts (List.filter Term.is_var ss) in
 let ss = List.filter Term.is_fun ss in 
 let nlt_f = fun_cond prec_n_lt f in
 (* case 4a + b *)
 (if !(flags.kv2) then ( 
  mul_gt' nlt_f gt_w_prec geq_w_prec ss ts' >>= fun mgt ->
  mul_geq' nlt_f gt_w_prec geq_w_prec ss ts' >>= fun mge ->
  return (mgt,mge))
 else
  mul_gt' nlt_f gt_w_prec eq_w_prec ss ts' >>= fun mgt ->
  mul_eq' nlt_f eq_w_prec ss ts' >>= fun mge ->
  return (mgt,mge)) >>= fun (mgt,mge) ->
 let sgt, sge = size ss > (size ts), size ss >= (size ts) in
 (* case 4c *)
 mul_gt kbo_gt ac_eq ss ts >>= fun mgt' ->
 return (mgt <|> (mge <&> ((l_if sgt) <|> ((l_if sge) <&> mgt'))))
;;

(* quasi precedence *)
let lex_geq gt geq ss ts =
 let rec lex = function
  | _,[] -> return L.top 
  | [], _ -> return L.bot
  | s::ss,t::ts -> 
   let st = R.of_terms s t in
   (gt st) $|$ ((geq st) $&$ (lex (ss,ts)))
 in lex (ss, ts)
;;

let lex_gt gt geq ss ts =
 let rec lex = function
  | _::_,[] -> return L.top 
  | [],_ -> return L.bot
  | s::ss,t::ts -> 
   let st = R.of_terms s t in (gt st) $|$ ((geq st) $&$ (lex (ss,ts)))
 in lex (ss,ts)
;;

let rec qkbo_gt rule =
 let helper rule =
  if (R.is_duplicating rule && (not !(flags.sc))) ||
   is_embedded (R.invert rule) then
   return L.bot
  else if is_proper_embedded rule then
   return L.top
  else (
   compare_weights rule >>= fun (w1,w2) ->
   (match R.to_terms rule with
    | Term.Var _, _ -> return L.bot
    | Term.Fun _, Term.Var _ -> return L.top
     (* all dangerous cases are considered above*)
    | Term.Fun (f0, ts0), Term.Fun (f1, ts1) ->
        is_ac f0 >>= fun f0_is_ac -> is_c f0 >>= fun f0_is_c ->
        (* case 3: f0 and f1 equivalent *)
        (if not (f0_is_ac || f0_is_c) then (* case 3 *)
          if f0_is_c then
           mul_gt' (fun _ -> return L.top) qkbo_gt qkbo_geq ts0 ts1
          else
           lex_gt qkbo_gt qkbo_geq ts0 ts1
        else q_gt f0 ts0 ts1) >>= fun if_eq ->
        (* case 2 *)
        (* combine *)
        (return if_eq $&$ (prec_eq f0 f1)) $|$ (prec_gt f0 f1))
     >>= fun aux -> return (w1 <|> (w2 <&> aux))) in
  get >>= fun c -> cache_m c.gt_encodings helper rule

and q_gt f ts0 ts1 =
 (*let ss, ts = top_flatq f ts0, top_flatq f ts1 in*)
 let ss, ts = top_flat f ts0, top_flat f ts1 in
 let ts' = diff ts (List.filter Term.is_var ss) in
 (* case 4a *)
 let gt_f = fun_cond prec_gt f in
 mul_gt' gt_f qkbo_gt qkbo_geq ss ts' >>= fun mgt ->
 (* case 4b *)
 mul_ge' gt_f qkbo_gt qkbo_geq ss ts' >>= fun mge ->
 let sgt, sge = size ss > (size ts), size ss >= (size ts) in
 (* case 4c *)
 let lt_f = fun_cond prec_lt f in
 mul_gt' lt_f qkbo_gt qkbo_geq ss ts >>= fun mgt' ->
 return (mgt <|> (mge <&> ((l_if sgt) <|> ((l_if sge) <&> mgt'))))

and qkbo_geq rule =
 if (R.is_duplicating rule && (not !(flags.sc))) ||
   is_embedded (R.invert rule) then
   return L.bot
  else if is_proper_embedded rule then
   return L.top
  else 
   compare_weights rule >>= fun (w1,w2) -> (
   match R.to_terms rule with
    | Term.Var _, Term.Var _ -> return L.top
    | Term.Var _, Term.Fun (c,[]) -> 
     get_fs >>= n_ary 0 >>= 
     map_and (fun a -> (weight a $=$ w0) $->$ (prec_gt a c))
    | Term.Var _, Term.Fun _ -> return L.bot
    | Term.Fun (f0, ts0), Term.Fun (f1, ts1) -> (
     is_ac f0 >>= fun f0_is_ac -> is_c f0 >>= fun f0_is_c ->
     if f0_is_ac then (
      let ss, ts = top_flat f0 ts0, top_flat f0 ts1 in
      mul_geq' (fun _ -> return L.top) qkbo_gt qkbo_geq ss ts
       $&$ (prec_eq f0 f1))
     else if f0_is_c then
      mul_geq' (fun _ -> return L.top) qkbo_gt qkbo_geq ts0 ts1
       $&$ (prec_eq f0 f1)
     else
      (prec_eq f0 f1) $&$ (lex_geq qkbo_gt qkbo_geq ts0 ts1)))
    >>= fun aux -> (return ((w1 <|> w2) <&> aux)) $|$ (ac_eq rule)
;;

let qkbo_gt_or_geq rule = (qkbo_geq rule) $|$ (qkbo_gt rule) $|$ (ac_eq rule)
(* end quasi *)

(* encode with argument filtering *)

let rec flatten = function
 | Term.Fun(f, ts) ->
  is_ac f >>= fun f_is_ac -> (
  if f_is_ac then
   Made.map flatten ts >>= fun ts' ->
   match find_term_with f ts' with
    | None -> Made.return (Term.Fun(f, List.sort Acrpo.my_compare ts'))
    | Some (ti,ts'') -> flatten (Term.Fun(f, (ts''@(Term.args ti))))
  else
   Made.map flatten ts >>= fun us -> Made.return (Term.Fun(f,us)))
 | v -> Made.return v
;;

let flat_rule r =
  Made.project flatten (R.to_terms r) >>= fun (l,r) -> 
  return (R.of_terms l r)
;;
(* some shorthands *)
let gen_and f = gen_op L.big_and f;;
let gen_or f = gen_op L.big_or f;;
let exists f p ts = mapi_or (fun i ti -> (af_p f i) $&$ (p ti)) ts;;
let exists2 f p ss ts = exists f p (List.map2 R.of_terms ss ts);;
let exists22 f g p ss ts = 
 exists f (fun si -> exists g (fun ti -> p (R.of_terms si ti)) ts) ss
;;
let for_all f p ts = mapi_and (fun i ti -> (af_p f i) $->$ (p ti)) ts;;
let for_all2 f p ss ts = for_all f p (List.map2 R.of_terms ss ts);;
let rot f s t = f (R.of_terms s t);;
let l_if = return <.> l_if;;

(* admissibility etc *)
let c_af f = arity f >>= fun a ->
 af_l f $&$ gen_and (fun i -> lift L.neg (af_p f i)) a
;;

(* f with arity a collapses to position i, i.e., \pi(f)=i *)
let af_collapsing f a i =
 let make j = if i = j then af_p f i else (lift L.neg (af_p f j)) in
 gen_and make a $&$ (lift L.neg (af_l f))
;;

let u_af f = 
 arity f >>= fun a -> 
 let single_p i =
  let make j = if i = j then af_p f i else (lift L.neg (af_p f j)) in
  gen_and make a
 in af_l f $&$ gen_or single_p a
;;

let adm_af_f fs f =
 (c_af f $->$ (weight f $>=$ w0)) $&$
 ((u_af f $&$ (weight f $=$ return C.zero)) $->$
  map_and (fun g -> (af_l g) $->$ (prec_ge f g)) fs)
;;

let adm_af fs = (w0 $>$ (return C.zero)) $&$ map_and (adm_af_f fs) fs;;

(* admissibility with subterm coefficients *)
let adm_sc_af fs =
 get >>= fun c ->
 let mono f =
  arity f >>= fun a ->
  let fi_z i = coefficient_of f i $=$ return C.zero in
  let fi_o i = coefficient_of f i $=$ return C.one in
  let fi_pos i = coefficient_of f i $>=$ return C.one in
  let cs = gen_and (fun i -> 
   ((af_p f i) $->$ (fi_pos i)) $&$ 
   ((mnot (af_p f i)) $->$ (fi_z i)) $&$
   (af_collapsing f a i $->$ (fi_o i))
  ) a in
  let c = af_n f $->$ (constant_of f $=$ return C.zero) in
  cs $&$ c
 in
 let adm_f f =
  (c_af f $->$ (constant_of f $>$ return C.zero)) $&$
  ((u_af f $&$ (constant_of f $=$ return C.zero)) $->$ (max f fs))
 in
 let ac = ac_symbols >>= fun acs -> c_symbols >>= fun cs ->
(*  let coeff_is_one f i = coefficient_of f i $=$ (return C.one) in
  map_and (fun ac -> coeff_is_one ac 0 $&$ (coeff_is_one ac 1))*)
 let p f = arity f >>= fun a ->
  ((af_ac_all f) $|$ (af_ac_none f)) $&$
  (mnot (af_collapsing f a 0)) $&$ (mnot (af_collapsing f a 1)) $&$
  ((coefficient_of f 0) $=$ (coefficient_of f 1))
 in map_and p (cs@acs)
 in ac $&$ (map_and adm_f fs) $&$ (map_and mono fs)
;;

let prec_gt_l f g = prec f $>$ prec g $&$ af_l f;;
let prec_lt_l f g = prec g $>$ prec f $&$ af_l f;;

(* lex and mul comparisons *)
let lex f eq gt ss ts =
 let rec lex' f eq gt ss ts i =
  match ss, ts with
   | [],[] -> return bot
   | [], _ | _, [] -> failwith "lists of different length"
   | sn::sss,tn::tss ->
     ((af_p f i) $&$ (gt (R.of_terms sn tn)))
     $|$
     (((af_p f i) $->$ (eq (R.of_terms sn tn)))
      $&$
      (lex' f eq gt sss tss (i+1)))
 in
 lex' f eq gt ss ts 0
;;

(* comparing conditional multisets *)
let cond_mul gt eq ss ts =
 mul_cover ss ts >>= fun (cover_aux,cs) ->
 let tcs = List.zip ts cs in
 let gt si tj = gt (R.of_terms si tj) in
 let is cj i = return (cj <=> (constant i)) in
 let cov_i_j_imp (tj,cj) i (si,psi) = is cj i $->$ (gt si tj $&$ return psi) in
 let cov_j_and tcj = mapi_and (cov_i_j_imp tcj) ss in
 let covered = mapi_and (fun j ((tj,ptj),cj) -> return ptj $->$ cov_j_and (tj,cj)) tcs in
 let cov_i_j_and (tj,cj) i (si,psi) = is cj i $&$ gt si tj $&$ return psi in
 let cov_j_or tcj = mapi_or (cov_i_j_and tcj) ss in
 let strict = mapi_or (fun j ((tj,ptj),cj) -> return ptj $&$ cov_j_or (tj,cj)) tcs in
 let s_nonempty = return (L.big_or (List.map snd ss)) in
 let t_empty = return (~! (L.big_and (List.map snd ts))) in
 strict $|$ (s_nonempty $&$ t_empty) >>= fun is_strict ->
 return cover_aux $&$ covered >>= fun is_covered ->
 return (is_covered, is_strict)
;;

let cond_mul_gt gt eq ss ts = cond_mul gt eq ss ts >>= fun (c,s) -> return (c <&> s)
let cond_mul_ge gt eq ss = lift fst <.> (cond_mul gt eq ss)

(* weights with argument filters *)
let encode_term_af t =
 let rec etc c = function
  | Term.Var x -> 
   return (Poly.make [Mono.make C.one [x]; Mono.make C.zero []])
  | Term.Fun (f, ts) ->
   af_l f >>= fun fl ->
   constant_of f >>= fun f0 ->
   let f0 = return (L.ite fl f0 C.zero) in
   (List.fold_lefti (fun i p t ->
    af_p f i >>= fun pi -> 
    coefficient_of f i >>= fun ci ->
    let ci = return (L.ite pi (L.ite fl ci C.one) C.zero) in
    lift2 Poly.add p (lift2 Poly.scale ci (etc c t)))
   (lift Poly.make (sequence [lift2 Mono.make f0 (return [])]))
   ts)
 in get >>= fun c -> cache_m c.subterm_encodings (etc c) t
;;

let af_greater_equal_rule rule =
 lift2 greater_equal_poly (encode_term_af (R.lhs rule))
  (encode_term_af (R.rhs rule))
;;

let af_greater_rule rule =
 lift2 greater_poly (encode_term_af (R.lhs rule))
  (encode_term_af (R.rhs rule))
;;

let af_equal_rule rule =
 lift2 equal_poly (encode_term_af (R.lhs rule))
  (encode_term_af (R.rhs rule))
;;

let weight_list t =
 let rec wt acc = function
  | Term.Var _ -> w0 >>= fun w0 -> return [L.ite acc w0 C.zero]
  | Term.Fun (f, ts) ->
   weight f >>= fun wf ->
   af_l f >>= fun fl ->
   sequence (List.mapi (fun i ti -> af_p f i >>= fun pi ->
    wt (pi <&> acc) ti) ts) >>= fun deep ->
    return (L.ite (fl <&> acc) wf C.zero::(List.concat deep))
in wt L.top t
;;

let weight_rule_af rule =
 project weight_list (R.to_terms rule) >>= fun (l,r) ->
 return (C.big_sum (diff l r),C.big_sum (diff r l))
;;

let compare_af_weights rule =
 if !(flags.sc) then Made.pair (af_greater_rule, af_greater_equal_rule) rule
 else weight_rule_af rule >>= fun (wl, wr) -> return (wl <>> wr,wl <=> wr)
;;

let equal_af_weights rule =
 if !(flags.sc) then af_equal_rule rule
 else weight_rule_af rule >>= fun (wl, wr) -> return (wl <=> wr)
;;

let dup_af rule =
 if !(flags.sc) then return top (* handled by polynomial comparison *)
 else
 let rec d x acc = function
  | Term.Var y -> return (if x = y then L.ite acc C.one C.zero else C.zero)
  | Term.Fun (f, ts) -> 
   sequence (List.mapi (fun i t -> af_p f i >>= fun pi -> (d x (acc <&> pi) t)) ts)
   >>= (return <.> L.big_sum) in
 let dup_x x t = d x L.top t in
 match R.lhs rule, R.rhs rule with
  | Term.Var _, _ -> return L.bot
  | s, t -> (* both have function symbol as root *)
   map_and (fun x -> (dup_x x s) $>=$ (dup_x x t)) (Term.vars t)
;;

let cond_topflat f0 ts =
 (* f collapses to i *)
 let coll f i = af_n f $&$ af_p f i in
 (* recursively collecting subterm candidates *)
 let rec cs c = function
  | (Term.Var _) as x -> return [x,c]
  | Term.Fun (f, ts) when f = f0 -> lift List.concat (map (cs c) ts)
  | (Term.Fun (f, ts) as t) ->
   mapi (fun i ti -> coll f i >>= fun c' -> cs (c'<&> c) ti) ts >>= fun tcs ->
   af_l f >>= fun f_noncoll ->
   return ((t,f_noncoll <&> c) :: (List.concat tcs))
 in lift List.concat (map (cs top) ts)
;;

let add_tcond p = map (fun (t,c) -> p t >>= fun c' -> return (t,c <&> c'))

let cond_size = map_add (fun (_,b) -> return (L.ite b L.one L.zero))

(* ts \ ss : difference of (term,condition) sets *)
let cond_diff ts ss =
 let check ts' (s,c') =
  let remove_from (t,c) = if s = t then (t,c <&> (~! c')) else (t,c) in
  List.map remove_from ts'
 in List.fold_left check ts ss
;;

(* KV with argument filterings *)
(* equality wrt auxiliary ordering: nonduplication, equal weights, same root *)
let eq_w_prec_af rule =
 let l,r = R.to_terms rule in
 if Term.root l <> (Term.root r) then return L.bot
 else
  (equal_af_weights rule) $&$ (mnot (dup_af rule)) $&$ 
  (mnot (dup_af (R.invert rule)))
;;

(* geq in modified kv ordering *)
let rec geq_w_prec_af rule =
 compare_af_weights rule >>= fun (wgt, _) ->
 let weq = equal_af_weights rule in
 let c = 
  match R.to_terms rule with
   | Term.Fun(f,ts), (Term.Var _  as x) ->
    ((af_l f) $&$ weq $&$ (get_fs >>= max f)) $|$
    ((af_n f) $&$ exists f (fun ti -> geq_w_prec_af (R.of_terms ti x)) ts)
   | Term.Fun (f, ss), Term.Fun (g, ts) when f = g -> 
    ((af_l f) $&$ weq) $|$
    ((af_n f) $&$ exists2 f geq_w_prec_af ss ts)
   | (Term.Fun (f, ss) as s), (Term.Fun (g, ts) as t) ->
    ((af_n f) $&$ (af_l g) $&$ exists f (fun si -> geq_w_prec_af (R.of_terms si t)) ss) $|$
    ((af_n g) $&$ (af_l f) $&$ exists f (fun ti -> geq_w_prec_af (R.of_terms s ti)) ts) $|$
    ((af_n f) $&$ (af_n g) $&$ exists22 f g geq_w_prec_af ss ts)
   | _ -> return L.bot
 in (mnot (dup_af rule)) $&$ (mnot (dup_af (Rule.invert rule))) $&$ 
  ((return wgt) $|$ c)
;;

(* gt in kv ordering *)
let rec gt_w_prec_af rule =
 compare_af_weights rule >>= fun (wgt, wge) ->
 let weq = equal_af_weights rule in
 let c =
  match R.to_terms rule with
   | Term.Fun (f, ss), Term.Fun (g, ts) when f = g ->
    ((af_n f) $&$ exists2 f gt_w_prec_af ss ts)
   | Term.Fun (f, ss) as s, (Term.Fun (g, ts) as t) ->
    ((af_l f) $&$ (af_l g) $&$ weq $&$ prec_gt f g) $|$
    ((af_n f) $&$ (af_l g) $&$ exists f (fun si -> gt_w_prec_af (R.of_terms si t)) ss) $|$
    ((af_n g) $&$ (af_l f) $&$ exists f (fun ti -> gt_w_prec_af (R.of_terms s ti)) ts) $|$
    ((af_n f) $&$ (af_n g) $&$ exists22 f g gt_w_prec_af ss ts)
 (*exists f (fun si -> exists g (fun ti -> gt_w_prec_af (R.of_terms si ti)) ts) ss)*)
   | _ -> return L.bot
 in (mnot (dup_af rule)) $&$ (return wge) $&$ (c $|$ (return wgt))
;;

(* equality, embedding *)
let eq_af_diff eq f g ss ts =
 let helper eq (f,g,ss,ts) =
 let s = Term.Fun (f, ss) and t = Term.Fun (g, ts) in
  (af_n f $&$ exists f (fun si -> eq (R.of_terms si t)) ss) $|$
   (af_n g $&$ exists g (fun tj -> eq (R.of_terms s tj)) ts)
 in get >>= fun c -> cache_m c.eq_af_encodings (helper eq) (f,g,ss,ts)
;;

let rec eq_af rule =
 flat_rule rule >>= fun rule ->
 let helper r =
  compare_af_weights rule >>= fun (_, wge) ->
  let s,t = R.to_terms rule in
  (match s,t with
   | _, _ when s = t -> return L.top
   | Term.Var x, Term.Var y (* then, x <> y holds *) -> return L.bot
   | Term.Var _, Term.Fun (g, ts) ->
    af_n g $&$ exists g (fun tj -> eq_af (R.of_terms s tj)) ts
   | Term.Fun (f, ss), Term.Var _ ->
    af_n f $&$ exists f (fun si -> eq_af (R.of_terms si t)) ss
   | Term.Fun (f, ss), Term.Fun (g, ts) when f = g ->
    if List.length ss <> List.length ts then return L.bot
    else for_all2 f eq_af ss ts (*equality holds when AF(f) is added*)
   | Term.Fun (f, ss), Term.Fun (g, ts) ->
    eq_af_diff eq_af f g ss ts) >>= fun c -> 
  return (wge <&> c)
 in get >>= fun c -> cache_m c.emb_eq_encodings helper rule
;;

let rec emb_af_ge rule = emb_af_gt rule $|$ eq_af rule
and emb_af_gt rule =
 let helper rule =
 let s, t = Rule.to_terms rule in match s, t with
  | Term.Var _, _ -> return bot
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

let kbo_af_same eq gt f0 ts0 ts1 =
 let helper (eq,gt) (f0,ts0,ts1) =
  (af_l f0 $&$ lex f0 eq gt ts0 ts1) $|$ (af_n f0 $&$ exists2 f0 gt ts0 ts1)
 in get >>= fun c -> cache_m c.kbo_af_same_encodings (helper (eq,gt))
 (f0,ts0,ts1)
;;

let kbo_af_diff gt f0 f1 ts0 ts1 =
 let helper gt (f0,f1,ts0,ts1)=
 let s = Term.Fun (f0, ts0) and t = Term.Fun (f1, ts1) in
 let gt' s = gt <.> (Rule.of_terms s) in
 ((af_l f0 $&$ af_l f1 $&$ prec_gt f0 f1)) $|$
 (af_n f1 $&$ exists f1 (fun tj -> gt (R.of_terms s tj)) ts1) $|$
 (af_n f0 $&$ exists f0 (fun si -> gt (R.of_terms si t)) ts0)
 in get >>= fun c -> cache_m c.kbo_af_diff_encodings (helper gt) (f0,f1,ts0,ts1)
;;

let rec kbo_af_gt rule =
 let rec helper rule =
  match R.to_terms rule with
  | Term.Var _, _ -> return L.bot
  | Term.Fun _, Term.Var _ -> emb_af_gt rule
  | _, _ -> (* both terms start with function symbol *)
  compare_af_weights rule >>= fun (wgt, wge) ->
(*  weight_rule_af rule >>= fun (wl, wr) ->*)
  dup_af rule $&$
   ((return wgt) $|$ ((return wge) $&$ (kbo_af_gt' rule)))
and kbo_af_gt' rule =
 match R.to_terms rule with
  | Term.Var _, _ (* fallthrough *)
  | Term.Fun _, Term.Var _ ->
   return L.bot (*failwith ".kbo_af_gt': case shall not happen"*)
  | Term.Fun (f0, ts0), Term.Fun (f1, ts1) ->
   is_ac f0 >>= fun f0_is_ac -> is_c f0 >>= fun f0_is_c ->
   let gt = 
     if !(flags.steinbach) then st_af_gt else
     if !(flags.kv) then kv_af_gt else gt_af
   in
   if f0 <> f1 then kbo_af_diff kbo_af_gt f0 f1 ts0 ts1
   else if f0_is_ac then gt f0 ts0 ts1
   else if f0_is_c then af_ac_all f0 $&$ (mul_gt' mtop kbo_af_gt eq_af ts0 ts1)
   else kbo_af_same eq_af kbo_af_gt f0 ts0 ts1
 in get >>= fun c -> cache_m c.gt_encodings helper rule

and kbo_af_ge rule = kbo_af_gt rule $|$ eq_af rule

(* standard setting *)
and gt_af f ts0 ts1 =
 project (cond_topflat f) (ts0,ts1) >>= fun (ss,ts) ->
 (*Format.printf "compare %a %a\n%!" Term.fprintf (Term.Fun(f,List.map fst ss))
  Term.fprintf (Term.Fun(f,List.map fst ts));*)
 let sv,sf = List.partition (Term.is_var <.> fst) ss in
 let tv,tf = List.partition (Term.is_var <.> fst) ts in
 let t = Term.Fun(f,ts1) in
 (* case 4a *)
 project (add_tcond (fun_cond prec_n_lt f)) (sf,tf) >>= fun (ss0, ts0) ->
 let ts0 = (diff tv sv) @ ts0 in
 let mgt = cond_mul_gt kbo_af_gt eq_af ss0 ts0 in
 (* case 4b *)
 let mge = cond_mul_ge kbo_af_gt eq_af ss0 ts0 in
 let sgt = cond_size ss $>$ cond_size ts in
 let sge = cond_size ss $>=$ cond_size ts in
 (* case 4c *)
 project (add_tcond (fun_cond prec_lt f)) (sf,tf) >>= fun (ss1, ts1) ->
 let mgt' = cond_mul_gt kbo_af_gt eq_af ss1 ts1 in
 (* case larger subterm *)
 let subt_gt = map_or (fun (si,psi) -> return psi $&$ (rot kbo_af_ge si t)) ss in
 (* combine *)
 af_ac_all f $&$ (subt_gt $|$ mgt $|$ (mge $&$ ((sgt $|$ (sge $&$ mgt')))))

(* Steinbach *)
and st_af_gt f ts0 ts1 =
 project (cond_topflat f) (ts0,ts1) >>= fun (ss,ts) ->
 cond_mul_gt kbo_af_gt eq_af ss ts

(* Korovin/Voronkov *)
and kv_af_gt f ts0 ts1 =
 let vs = List.filter Term.is_var (List.intersect ts0 ts1) in
 let ts0, ts1 = diff ts0 vs, diff ts1 vs in
 project (cond_topflat f) (ts0,ts1) >>= fun (ss,ts) ->
 let sv,sf = List.partition (Term.is_var <.> fst) ss in
 let ts' = cond_diff ts sv in
 (* case 4a + b *)
(* let sstr = Term.to_string (Term.Fun(f,sf)) in
 let tstr = Term.to_string (Term.Fun(f,ts')) in
 Format.printf " topflattened: %s > %s ?\n%!" sstr tstr;*)
 project (add_tcond (fun_cond prec_n_lt f)) (sf,ts') >>= fun (ss0, ts0) ->
 (if !(flags.kv2) then 
   cond_mul_gt gt_w_prec_af geq_w_prec_af ss0 ts0 >>= fun mgt ->
   cond_mul_ge gt_w_prec_af geq_w_prec_af ss0 ts0 >>= fun mge ->
   return (mgt,mge)
  else
   cond_mul_gt gt_w_prec_af eq_w_prec_af ss0 ts0 >>= fun mgt ->
   cond_mul_ge gt_w_prec_af eq_w_prec_af ss0 ts0 >>= fun mge -> 
   return (mgt,mge)) >>= fun (mgt, mge) ->
 let sgt = cond_size ss $>$ cond_size ts in
 let sge = cond_size ss $>=$ cond_size ts in
 (* case 4c *)
 let mgt' = cond_mul_gt kbo_af_gt eq_af ss ts in
 af_ac_all f $&$ (return mgt $|$ (return mge $&$ (sgt $|$ (sge $&$ mgt'))))
;;

let adm_af_f fs f =
 (c_af f $->$ (weight f $>=$ w0)) $&$
 ((u_af f $&$ (weight f $=$ return C.zero)) $->$
  map_and (fun g -> (af_l g) $->$ (prec_ge f g)) fs)
;;

let adm_af fs = (w0 $>$ (return C.zero)) $&$ map_and (adm_af_f fs) fs;;

(* encoding *)
let total_prec fs = 
 let comparable (f,g) = prec_gt f g $|$ prec_gt g f in
 map_and comparable (List.filter (fun (a,b) -> compare a b < 0) (List.square fs))
;;

let af_f f =
 arity f >>= fun a ->
 is_ac f >>= fun ac -> is_c f >>= fun c ->
 if ac || c then ((af_ac_all f) $|$ (af_ac_none f)) $&$
  (mnot (af_collapsing f a 0)) $&$ (mnot (af_collapsing f a 1))
 else af_l f $|$ gen_or (af_collapsing f a) a
;;

let af fs = 
 get >>= fun c -> if !(flags.dp) then map_and af_f fs else return L.top
;;

(* encode which function symbols are potential roots for usable rules *)
let ur s w =
 let rhss = List.map Rule.rhs s in
 let fs = List.unique (List.flat_map Rule.funs (s@w)) in
 if !(flags.dp) then 
   let rec us_in c = function
    | Term.Var _ -> return Logic.top
    | Term.Fun (f,ts) -> 
     let us_in' i ti = af_p f i >>= fun pi -> us_in (c <&> pi) ti in
     (return c (*$&$ af_l f*) $->$ (us_f f)) $&$ mapi_and us_in' ts
   in 
   let reachable f =
    let root_f rl = Option.the (Term.root (R.lhs rl)) = f in 
    let rhss = List.map Rule.rhs (List.filter root_f w) in
    us_f f >>= fun usf -> map_and (us_in usf) rhss
   in (map_and (us_in Logic.top) rhss) $&$ (map_and reachable fs)
 else
   let reachable f =
    let root_f rl = Option.the (Term.root (R.lhs rl)) = f in
    let rs = List.filter root_f w in
    let fs' = List.unique (List.flat_map (fun r -> Term.funs (Rule.rhs r)) rs) in
    (us_f f) $->$ (map_and us_f fs') 
   in (map_and us_f fs) $&$ (map_and reachable fs)
;;

let encode_gt s = 
 let gt = if !(flags.quasi) then qkbo_gt else 
 if !(flags.dp) then kbo_af_gt else kbo_gt in
 (if !(flags.direct) then map_and else map_or) gt s

let encode_ge s w es =
 let geq = if !(flags.quasi) then qkbo_gt_or_geq else 
  if !(flags.dp) then kbo_af_ge else kbo_ge in 
 if !(flags.ur) then
  let geq' r = (us_f (Option.the (Term.root (R.lhs r)))) $->$ (geq r) in
  (map_and geq s) $&$ (map_and geq' w) $&$ (map_and geq es)
 else
  map_and geq (s@w@es)
;;

let encode s w es = get >>= fun c ->
 fresh_arith >>= fun w0 -> c.w0 := w0;
 let fs = Trs.funs (Trs.union s w) in
 set_fs fs >>
 let s, w = Pair.map Trs.to_list (s, w) in
 let ac = if !(flags.steinbach) then steinbach fs else return L.top in
 let total = if !(flags.total_prec) then total_prec fs else return top in
 let adm = if !(flags.sc) && !(flags.dp) then adm_sc_af fs else (* adm_sc covers monotonicity *)
           if !(flags.sc) then adm_sc fs $&$ (mon fs) else
           if !(flags.dp) then adm_af fs else adm fs in
 let quasi = if !(flags.quasi) then quasi_adm else return top in
 encode_gt s $&$ encode_ge s w (Trs.to_list es)
  $&$ total $&$ adm $&$ ac $&$ quasi $&$ af fs $&$ ur s w
;;

(* decode from assignment *)
let decode_rule ass rule = lift not (eval_p (kbo_gt rule) ass)

let decode_trs ass trs = 
 Made.filter (decode_rule ass) (Trs.to_list trs) >>= (return <.> Trs.of_list)
;;

let decode ass s w = get >>= fun c -> 
 lift2 Pair.make (decode_trs ass s) ((*decode_trs ass*) return w)
;;

let decode_status_f ass f = eval_p (is_mul f) ass >>= fun b -> return (f,b)

let decode_status ass s w =
 let fs = Trs.funs (Trs.union s w) in
 Made.sequence (List.map (decode_status_f ass) fs) >>= fun ps ->
 return (Status.of_list ps)
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

let decode_ur ass s w =
 let lrt r = Option.the (Term.root (R.lhs r)) in
 filter (fun r -> eval_p (us_f (lrt r)) ass) (Trs.to_list w) >>= fun l ->
 return (Trs.of_list l)
;;

let get_inter ass f = arity f >>= fun a ->
 let eval_matrix ass m = m >>= fun m ->
  liftm (L.eval_a m ass) >>= fun c ->
  return (NM.make 1 1 c)
 in
 Made.sequence
  (List.gen (fun i -> eval_matrix ass (coefficient_of f i)) a) >>= fun cs ->
 eval_matrix ass (constant_of f) >>= fun const ->
 return (f, cs, const)
;;

let decode_wf_f ass f = eval_a (weight f) ass >>= fun w -> return (f,w);;

let decode_wf ass s w = get >>= fun c ->
 let fs,trs = Trs.funs (Trs.union s w), Trs.union s w in
 if !(flags.sc) then (
  Made.sequence (List.map (get_inter ass) (Trs.funs trs)) >>= fun is ->
  let i = List.foldr (fun fi acc -> uncurry3 (Intp.add acc) fi) (Intp.empty ()) is in
  return (Inter i))
 else (
  eval_a w0 ass >>= fun w0 ->
  Made.sequence (List.map (decode_wf_f ass) fs) >>= fun ws ->
  return (Weight (WF.make ws w0)))
;;

let decode_af_f ass f =
 arity f >>= fun a ->
 sequence (List.gen (fun i -> eval_p (af_p f i) ass) a) >>= fun ps ->
 let psi = List.mapi Pair.make ps in
 let ps = List.filter snd psi in
 let ps = List.map fst ps in
 eval_p (af_l f) ass >>= fun l ->
 if l then return (f,AF.List ps)
 else ( assert (List.length ps = 1); return (f,AF.Collapsing (List.hd ps)))
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
(* if (not !(flags.total_prec)) && 
  (not (!(flags.steinbach) || !(flags.quasi))) then
  failwith "ackbo: option -nt is only allowed with -st";*)
 let s,w = P.get_sw p in
 let ess = if P.is_edp p then P.get_eqssharp p else Trs.empty in
 let es = Trs.union (P.get_eqs p) (Trs.union (P.get_srules p) ess) in
 let w = Trs.of_list (List.diff (Trs.to_list w) (Trs.to_list es)) in
 let c = context signature (Trs.funs (Trs.union s (Trs.union es w))) in
 L.run (
  Made.run c (encode s w es >>= fun phi ->
  Made.liftm (L.solve ~solver:c.solver phi) >>= function
   | None -> return None
   | Some ass ->
    decode_af ass s w >>= fun af ->
    decode ass s w >>= fun (s',w') -> 
    decode_prec ass s w >>= fun prec -> 
    (*decode_status ass s w >>= fun status ->*)
    decode_ur ass s w >>= fun us ->
    decode_wf ass s w >>= fun wf ->
    return (Some (make af p (P.set_sw s' w' p) prec us wf))))
;;

(* wrap into state monad *)
let (>>=) = M.(>>=);;
let solve fs p = M.get >>= fun s -> M.return (solve s fs p);;
