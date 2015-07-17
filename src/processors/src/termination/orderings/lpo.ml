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

(*** TYPES ********************************************************************)

type context = {
 solver                : Logic.solver;
 prec_num              : int;
 af_l                  : (Fun.t, Logic.p) H.t;
 af_p                  : (Fun.t * int, Logic.p) H.t;
 usable                : (Fun.t, Logic.p) H.t;
 precs                 : (Fun.t, Logic.a) H.t;
 state                 : Signature.t;
 gt_encodings          : (Rule.t, Logic.p) H.t;
 eq_af_encodings       : (Fun.t*Fun.t*Term.t list*Term.t list, Logic.p) H.t;
 emb_gt_encodings      : (Rule.t, Logic.p) H.t;
 emb_eq_encodings      : (Rule.t, Logic.p) H.t;
 lpo_af_same_encodings : (Fun.t*Term.t list*Term.t list,Logic.p) H.t;
 lpo_af_diff_encodings : (Fun.t*Fun.t*Term.t list*Term.t list,Logic.p) H.t;
};;

type flags = {
 dp : bool ref;
 direct : bool ref;
 help : bool ref;
 quasi : bool ref;
 sat : bool ref;
 ur : bool ref;
};;

type t = {
 af : AF.t option;
 input : P.t;
 output : P.t;
 precedence : Prec.t;
 usable_rules : Trs.t option;
};;

(*** GLOBALS ******************************************************************)
let code = "lpo";;
let name = "LPO Processor";;
let comment = "Applies Lexicographic Path Order."

let keywords =
 ["lexicographic path order";"simplification order";"termination"]
;;

let flags = {
 dp = ref false;
 direct = ref false;
 help = ref false;
 quasi = ref false;
 sat = ref false;
 ur = ref false;
};;

let spec =
 let spec = [
  ("-af",Arg.Set flags.dp,
   "Allows non-monotone interpretations (argument filterings).");
  ("-dp",Arg.Set flags.dp,
   "Equivalent to `-af'.");
  ("-direct",Arg.Set flags.direct,
   "Try to finish termination proof.");
  ("--help",Arg.Set flags.help,"Prints information about flags.");
  ("-help",Arg.Set flags.help,"Prints information about flags.");
  ("-h",Arg.Set flags.help,"Prints information about flags.");
  ("-quasi",Arg.Set flags.quasi,"Allows quasi-precedences (currently not \
   supported together with -dp flag).");
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
 flags.direct := false;
 flags.help := false;
 flags.quasi := false;
 flags.sat := false;
 flags.ur := false;
;;

(* Constructors and Destructors *)
let make af input output precedence ur = {
 af = af;
 input = input;
 output = output;
 precedence = precedence;
 usable_rules = ur;
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
 F.fprintf fmt "@]@\n@[<1>precedence:@\n";
 Prec.fprintfm fmt p.precedence >>= fun _ ->
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
  Prec.fprintfx fmt p.precedence >>= fun _ ->
  fprintfx_af fmt p.af           >>= fun _ ->
  M.return(F.fprintf fmt "@}@}")
;;

let fprintfx_ur fmt = function
  | Some ur ->
    F.fprintf fmt "@{<usableRules>";
    Trs.fprintfx fmt ur >>= fun _ ->
    Monad.return(F.fprintf fmt "@}") 
  | None    -> Monad.return()
;;

let fprintfx fs fmt p = if Problem.is_dp p.input then (
  let tag = "redPairProc" in
  F.fprintf fmt "@{<%s>" tag;
  fprintfx_redpair fmt p         >>= fun _ ->
  Problem.fprintfx fmt p.output  >>= fun _ ->
  fprintfx_ur fmt p.usable_rules >>= fun _ ->
  List.hd fs fmt                 >>= fun _ ->
  Monad.return(F.fprintf fmt "@}");
) else if Problem.is_sp p.input then (
  failwith "LPO full termination not supported!"
) else (
  failwith "LPO relative not supported!"
)
;;

(* general functions *)
let (>>=) = Made.(>>=);;

(* functions lifted from Logic into Made *)
let fresh_arith_spec spec = liftm (Logic.fresh_arith spec);;
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
 let solver = if !(flags.sat) then Logic.MiniSat else Logic.Yices in
 {
  solver            = solver;
  prec_num          = max 0 (List.length fs - 1);
  af_l              = H.create 512;
  af_p              = H.create 512;
  usable            = H.create 512;
  precs             = H.create 512;
  state             = state;
  eq_af_encodings   = H.create 512;
  gt_encodings      = H.create 512;
  emb_gt_encodings  = H.create 512;
  emb_eq_encodings  = H.create 512;
  lpo_af_same_encodings = H.create 512;
  lpo_af_diff_encodings = H.create 512;
 }
;;

(* administrative functions *)
let cache_m tbl f k = 
 if H.mem tbl k then return (H.find tbl k)
 else (f k >>= fun v -> (H.add tbl k v; return v))
;;

(* LPO encoding starts here *)
(* caching variables *)
let prec f = get >>= fun c -> 
 let arith = Logic.nat c.prec_num in
 cache_m c.precs (const (fresh_arith_spec arith)) f
;;

let prec_gt f g = prec f $>$ prec g;;

let prec_eq f g =
 if !(flags.quasi) then prec f $=$ prec g
 else return (if f = g then Logic.top else Logic.bot)
;;

let u t =
 get >>= fun c ->
 cache_m c.usable (const fresh_bool) (Option.the(Term.root t))
;;

let af_l f = get >>= fun c -> cache_m c.af_l (const fresh_bool) f;;
let af_n f = af_l f >>= (return <.> Logic.neg);;
let af_p f i = get >>= fun c -> cache_m c.af_p (const fresh_bool) (f,i);;
let arity f = get >>= fun c -> return (Sig.find_ari f c.state);;
let n_ary n fs = Made.filter (fun f -> lift2 (=) (arity f) (return n)) fs;;

(* encoding *)
let rec lex gt eq ts ss = match ts,ss with
 | _::_,[] -> return Logic.top
 | [], _ -> return Logic.bot
 | ti::ts,si::ss -> 
  gt (Rule.of_terms ti si) $|$ (eq (Rule.of_terms ti si) $&$ lex gt eq ts ss)
;;

let rec lpo_eq rule = match Rule.to_terms rule with
 | Term.Var x, Term.Var y -> return (if x = y then Logic.top else Logic.bot)
 | Term.Fun _, Term.Var _ (* fallthrough *)
 | Term.Var _, Term.Fun _ -> return Logic.bot
 | Term.Fun (f,ts), Term.Fun (g,ss) ->
  lift2 (=) (arity f) (arity g) >>= fun b ->
  if b then prec_eq f g 
   $&$ map_and (fun (t,s) -> lpo_eq (Rule.of_terms t s)) (List.combine ts ss)
  else return Logic.bot
;;

let rec lpo_gt rule =
 let helper rule = 
  if Rule.is_embedded (Rule.invert rule) || not (Rule.is_rewrite_rule rule) then
   return Logic.bot
  else if Rule.is_proper_embedded rule then return Logic.top
  else
   let t,s = Rule.to_terms rule in
   match t,s with
   | Term.Var _, _ -> return Logic.bot
   | Term.Fun _, Term.Var x -> 
    return (if List.mem x (Term.vars s) then Logic.top else Logic.bot)
   | Term.Fun (f,ts), Term.Fun (g,ss) ->
    ((if f = g then lex lpo_gt lpo_eq ts ss 
               else prec_gt f g $|$ (prec_eq f g $&$ lex lpo_gt lpo_eq ts ss)
    ) $&$ map_and (fun sj -> lpo_gt (Rule.of_terms t sj)) ss
    ) $|$ map_or (fun ti -> lpo_ge (Rule.of_terms ti s)) ts 
  in
  get >>= fun c -> cache_m c.gt_encodings helper rule
and lpo_ge rule = lpo_gt rule $|$ lpo_eq rule
;;

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
 let rec lex' eq gt ss ts i =
  match ss, ts with
   | [],[] -> return Logic.bot
   | [], _ | _, [] -> failwith "lists of different length"
   | sn::sss,tn::tss -> 
     (af_p f i $&$ gt (Rule.of_terms sn tn)) 
     $|$
     ((af_p f i $->$ eq (Rule.of_terms sn tn)) $&$ lex' eq gt sss tss (i+1))
 in
 lex' eq gt ss ts 0 
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
  | Term.Fun (f0, ts0), Term.Fun (f1, ts1) ->
   if f0 = f1 then lpo_af_same eq_af lpo_af_gt f0 ts0 ts1
   else lpo_af_diff eq_af lpo_af_gt f0 f1 ts0 ts1
 in get >>= fun c -> cache_m c.gt_encodings helper rule
;;

let lpo_af_ge rule = lpo_af_gt rule $|$ eq_af rule;;

(* encoding *)
let lpo_gt rule =
 get >>= fun c -> if !(flags.dp) then lpo_af_gt rule else lpo_gt rule
;;
 
let lpo_ge rule =
 get >>= fun c -> if !(flags.dp) then lpo_af_ge rule else lpo_ge rule
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

let ge_ur op ge rule = 
 lift2 op (u(Rule.lhs rule)) (ge rule)
;;

let encode_gt s = 
 let map_op = if !(flags.direct) then map_and else map_or in
 map_or lpo_gt s
;;

let encode_ge s w = get >>= fun c ->
 if !(flags.ur) then (
  let ds = Trs.def_symbols (Trs.of_list (s@w)) in
  map_and (ge_ur (<&>) lpo_ge) s $&$
  map_and (ge_ur (<->>) lpo_ge) w $&$
  map_and (process_rule ds) (s@w)
 ) else 
 map_and lpo_ge (s@w)
;;

let af_f f = arity f >>= fun a -> af_l f $|$ gen_or (af_collapsing f a) a;;
let af fs = map_and af_f fs;;
let af fs = get >>= fun c -> if !(flags.dp) then af fs else return Logic.top;;

let encode s w = get >>= fun c ->
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
 if !(flags.dp) && !(flags.quasi) then
  failwith "LPO: -dp with -quasi not supported";
 let (s,w) = P.get_sw p in
 let c = context signature (Trs.funs (Trs.union s w)) in
 Logic.run (
  Made.run c (encode s w >>= fun phi ->
  Made.liftm (Logic.solve ~solver:c.solver phi) >>= function
   | None -> return None
   | Some ass ->
    decode ass s w >>= fun (s',w') -> 
    decode_af ass s w >>= fun af ->
    decode_prec ass s w >>= fun prec -> 
    decode_ur ass w >>= fun ur ->
    return (Some (make af p (P.set_sw s' w' p) prec ur))))
;;

(* wrap into state monad *)
let (>>=) = M.(>>=);;
let solve fs p = M.get >>= fun s -> M.return (solve s fs p);;
