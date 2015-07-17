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
module P = Problem;;
module Pos = Position;;
module Prec = Precedence;;
module Sig = Signature;;
module Var = Variable;;
module WF = Weights;;

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
 weights               : (Fun.t, Logic.a) H.t;
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
 input : P.t;
 output : P.t;
 precedence : Prec.t;
 usable_rules : Trs.t option;
 wf : WF.t;
};;

(*** GLOBALS ******************************************************************)
let code = "kbo";;
let name = "KBO Processor";;
let comment = "Applies Knuth-Bendix order."
let keywords = ["knuth-bendix order";"simplification order";"termination"];;

let flags = {
 dp = ref false;
 ep = ref false;
 help = ref false;
 min = ref 1;
 minp = ref false;
 minw = ref false;
 ob = ref max_int;
 pbc = ref false;
 quasi = ref false;
 rat = ref 1;
 sat = ref false;
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
  ("-minw",Arg.Set flags.minw,
   "Minimize the sum of weights (only for `-pbc').");
  ("-ob",Arg.Set_int flags.ob,
   "Defines the number of bits used to represent intermediate results
    (same as `-max' but in bits)");
  ("-pbc",Arg.Set flags.pbc,"Uses PBC backend.");
  ("-quasi",Arg.Set flags.quasi,"Allows quasi-precedences.");
  ("-rat",Arg.Set_int flags.rat,
   "Sets the denumerator (only in combination with `-sat' or `-smt').");
  ("-sat",Arg.Set flags.sat,"Uses SAT backend.");
  ("-smt",Arg.Clear flags.sat,"Uses SMT backend (default).");
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
 flags.sat := false;
 flags.ur := false;
;;

(* Constructors and Destructors *)
let make af input output precedence ur wf = {
 af = af;
 input = input;
 output = output;
 precedence = precedence;
 usable_rules = ur;
 wf = wf;
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
 F.fprintf fmt "@\n@[<1>weight function:@\n";
 WF.fprintfm fmt p.wf >>= fun _ ->
 F.fprintf fmt "@]@\n@[<1>precedence:@\n";
 Prec.fprintfm fmt p.precedence >>= fun _ ->
 F.fprintf fmt "@]@\n@[<1>problem:@\n";
 P.fprintfm fmt p.output >>= fun _ ->
 F.fprintf fmt "@]@\n"; List.hd fs fmt >>= fun _ ->
 Monad.return (F.fprintf fmt "@]")
;;

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
  weights           = H.create 512;
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
 }
;;

(* administrative functions *)
let cache_m tbl f k = 
 if H.mem tbl k then return (H.find tbl k)
 else (f k >>= fun v -> (H.add tbl k v; return v))
;;

(* KBO encoding starts here *)
let weight f = get >>= fun c -> cache_m c.weights (const fresh_arith) f;;

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

let constant fs = n_ary 0 fs >>= fun cs ->
 map_and (fun con -> weight con $>=$ w0) cs
;;

let unary fs = n_ary 1 fs >>= fun us ->
 map_and (fun u -> (weight u $=$ return C.zero) $->$ (max u fs)) us
;;

let adm fs = get >>= fun c -> 
 (w0 $>$ return C.zero) $&$ constant fs $&$ unary fs
;;

let weight_fs rule = 
 let fsl, fsr = Rule.map symbols_list rule in
 let v = Util.uncurry diff (Rule.map vars_list rule) in
 let wl, wr = diff fsl fsr, diff fsr fsl in
 let v = C.of_int (List.length v) in
 w0 >>= fun w0 ->
 let xs = sequence (return (C.scale v w0)::List.map weight wl) in
 lift C.big_sum xs >>= fun sl ->
 lift C.big_sum (sequence (List.map weight wr)) >>= fun sr ->
 return (sl, sr)
;;

let rec kbo_gt rule =
 let helper rule = 
  if Rule.is_duplicating rule || Rule.is_embedded (Rule.invert rule) then
   return Logic.bot
  else if Rule.is_proper_embedded rule then
   return Logic.top
  else (
   weight_fs rule >>= fun (wl, wr) -> 
   let w1 = wl <>> wr and w2 = wl <=> wr in
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

(* encoding PBC *)
(* useful shorthands *)
let num x = Logic.constant (Number.of_int x);;
let nat1 = Logic.nat 1;;
let fresh_pb = const (fresh_arith_spec nat1);;
let fresh_pb2 = const (fresh_arith_spec nat1);;

(* get PB variables *)
let pbc_kbo s t = get >>= fun c -> cache_m c.pbc_kbo fresh_pb (s,t);;
let pbc_kbo' s t = get >>= fun c -> cache_m c.pbc_kbo2 fresh_pb (s,t);;
let pbc_prec_gt f g = get >>= fun c -> cache_m c.pbc_gt fresh_pb2 (f,g);;
let pbc_prec_eq f g = get >>= fun c -> cache_m c.pbc_eq fresh_pb2 (f,g);;
let pbc_prec_no f g = get >>= fun c -> cache_m c.pbc_no fresh_pb2 (f,g);;

(* admissible weight function *)
let pbc_adm_c fs = get >>= fun c ->
 n_ary 0 fs >>= fun cs ->
 map_and (fun c -> ((weight c) $-$ w0) $>=$ (return Logic.zero)) cs
;;

let pbc_sum_prec f fs = get >>= fun c ->
 let sum f g = 
  if !(flags.quasi) then (pbc_prec_gt f g) $+$ (pbc_prec_eq f g)
  else pbc_prec_gt f g
 in
 sequence (List.map (sum f) fs) >>= (return <.> Logic.big_sum)
;;

let pbc_adm_u fs = n_ary 1 fs >>= fun us -> get >>= fun c ->
 let n = List.length fs in
 let n = if !(flags.quasi) then n else n - 1 in
 let nc = Logic.constant (Number.of_int n) in
 map_and (fun u -> ((return nc $*$ weight u) $+$ 
  (pbc_sum_prec u fs)) $>=$ return nc) us
;;

let pbc_adm fs =
 (w0 $>$ (return Logic.zero)) $&$ pbc_adm_c fs $&$ pbc_adm_u fs
;;

(* properties for precedences *)
let pbc_prec_fg ll f g = 
 let c2 = Logic.constant (Number.of_int 2) in
  (((return c2 $*$ pbc_prec_gt f g) $+$ 
    pbc_prec_eq f g $+$ pbc_prec_eq g f $+$ 
    (return c2 $*$ pbc_prec_no f g)) 
   $=$ (return c2))
  $&$
   ((((return ll $*$ pbc_prec_eq f g) $+$ 
     (return ll $*$ pbc_prec_no f g) $+$
     (prec f $-$ prec g)) $-$ (pbc_prec_gt f g)) 
   $>=$ (return Logic.zero))
  $&$
   (((return ll $*$ pbc_prec_gt f g) $+$
    (pbc_prec_eq f g) $+$
    (return ll $*$ pbc_prec_no f g) $+$
    (prec f $-$ prec g))
   $>=$ (return Logic.one))
;;

let pbc_prec_f fs f = 
 let bits = Pervasives.max 0 (Int.bits (List.length fs)) in
 let ll = Logic.constant (Number.of_int (Int.pow 2 bits)) in 
 map_and (pbc_prec_fg ll f) fs
;;

let pbc_prec fs = map_and (pbc_prec_f fs) fs;;

(* kbo orientation *)
let rec pbc_kbo_gt rule = get >>= fun c ->
 let s, t = Rule.to_terms rule in
 if Rule.is_duplicating rule || Rule.is_embedded (Rule.invert rule) then
   pbc_kbo s t $=$ return Logic.zero
 else if Rule.is_proper_embedded rule then pbc_kbo s t $=$ return Logic.one
 else
  weight_fs rule >>= fun (wl, wr) -> 
  let max_approx = ((Int64.to_int c.arith.Logic.min) + 1) * (Term.size t) in
  (((return (num (-(max_approx + 1))) $*$ pbc_kbo s t) $+$
    (return wl $-$ return wr) $+$ pbc_kbo' s t) $>=$
   (return (num (-max_approx)))) 
   $&$ pbc_kbo_gt' rule
  and pbc_kbo_gt' rule = 
   let s, t = Rule.to_terms rule in match s, t with
    | Term.Var _, _ -> pbc_kbo' s t $=$ return Logic.zero 
    | _, Term.Var _ -> pbc_kbo' s t $=$ return Logic.one
    | Term.Fun (f0, ts0), Term.Fun (f1, ts1) -> get >>= fun c ->
     if f0 = f1 then begin (* same function symbol *)
      let ts0', ts1' = List.remove_equal ts0 ts1 in
      if ts0' = [] || ts1' = [] then
       pbc_kbo' s t $=$ return Logic.zero
      else 
      let ts0i, ts1i = List.hd ts0, List.hd ts1 in
      ((pbc_kbo ts0i ts1i $-$ pbc_kbo' s t) $>=$ return Logic.zero) 
       $&$ pbc_kbo_gt (Rule.of_terms ts0i ts1i)
     end else 
     if !(flags.quasi) then begin (* different function symbol *)
      let ts0', ts1' = List.remove_equal ts0 ts1 in
      if ts0' = [] || ts1' = [] then
       (pbc_prec_gt f0 f1 $-$ pbc_kbo' s t) $>=$ return Logic.zero
      else
       let ts0i, ts1i = List.hd ts0, List.hd ts1 in
       (((return (num (-2)) $*$ pbc_kbo' s t) $+$
        (return (num 2) $*$ pbc_prec_gt f0 f1) $+$
        pbc_prec_eq f0 f1 $+$ pbc_kbo ts0i ts1i) 
        $>=$ return Logic.zero)
       $&$
        (pbc_kbo_gt (Rule.of_terms ts0i ts1i))
      end else (* strict precedence *)
       ((return (num (-1)) $*$ pbc_kbo' s t) $+$ pbc_prec_gt f0 f1)
        $>=$ (return Logic.zero)
;;

let goal s w = get >>= fun c -> 
 let fs = Trs.funs (Trs.union s w) in
 match c.goal with
 | None -> return None
 | Some Prec -> sequence (List.map (uncurry pbc_prec_gt) (List.square fs)) >>= 
  (return <.> Option.some <.> Logic.big_sum)
 | Some Weight -> sequence (List.map weight fs) >>=
  (return <.> Option.some <.> Logic.big_sum)
;;

let pbc_aux fs = get >>= fun c ->
 if !(flags.ep) then map_and (fun f -> prec f $=$ return Logic.zero) fs
 else return Logic.top
;;

let pbc_encode s w = 
 let b = s@w in
 let fs = Trs.funs (Trs.of_list b) in
 pbc_adm fs $&$
 map_and pbc_kbo_gt b $&$
 map_and (fun r -> uncurry pbc_kbo (Rule.to_terms r) $=$ return Logic.one) b $&$
 pbc_prec fs $&$
 pbc_aux fs
;;

(* encoding PBC end *)
let encode s w = get >>= fun c ->
 fresh_arith >>= fun w0 -> c.w0 := w0;
 let fs = (Trs.funs (Trs.union s w)) in
 let s, w = Pair.map Trs.to_list (s, w) in
 if c.solver = Logic.MiniSatP then
  pbc_encode s w
 else
  encode_gt s $&$ encode_ge s w $&$ adm fs $&$ af fs
;;

(* decode from assignment *)
let decode_rule ass rule = get >>= fun c ->
 if c.solver = Logic.MiniSatP then
  eval_a (uncurry pbc_kbo (Rule.to_terms rule)) ass >>= fun x ->
  return (Number.eq x Number.zero)
 else
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

let decode_wf_f ass f = eval_a (weight f) ass >>= fun w -> return (f,w);;

let decode_wf ass s w = get >>= fun c ->
 let fs = Trs.funs (Trs.union s w) in
 eval_a w0 ass >>= fun w0 ->
 Made.sequence (List.map (decode_wf_f ass) fs) >>= fun ws ->
 return (WF.make ws w0)
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
 let (s,w) = P.get_sw p in
 let c = context signature (Trs.funs (Trs.union s w)) in
 Logic.run ~obits:!(flags.ob) (
  Made.run c (encode s w >>= fun phi -> goal s w >>= fun goal ->
  Made.liftm (Logic.solve ~solver:c.solver ~goal:goal phi) >>= function
   | None -> return None
   | Some ass ->
    decode ass s w >>= fun (s',w') -> 
    decode_af ass s w >>= fun af ->
    decode_wf ass s w >>= fun wf ->
    decode_prec ass s w >>= fun prec -> 
    decode_ur ass w >>= fun ur ->
    return (Some (make af p (P.set_sw s' w' p) prec ur wf))))
;;

(* wrap into state monad *)
let (>>=) = M.(>>=);;
let solve fs p = M.get >>= fun s -> M.return (solve s fs p);;
