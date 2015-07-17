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

module Status = struct
 (*** INCLUDES ****************************************************************)
 include Index.Make (Fun) (Bool);;

 (*** FUNCTIONS ***************************************************************)
 let (>>=) = M.(>>=);;

 let is_lex s f = not (find f s);;
 let is_mul s f = find f s;;
 let is_ac_or_c f = 
  M.is_theory Label.AC f >>= fun ac ->
  M.is_theory Label.C f >>= fun c ->
  M.return (ac || c) 
 ;;

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
 M.filter (fun (f,_) -> is_ac_or_c f >>= (M.return <.> not)) (to_list p) >>=
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
 solver             : L.solver;
 prec_num           : int;
 state              : Signature.t;
 precs              : (Fun.t, L.a) H.t;
 gt_encodings       : (R.t, L.p) H.t;
 (* encoding statuses of function symbols; true is multiset *)
 stats              : (Fun.t, L.p) H.t;
 (* multiset cover: if (s->t, j) has value i then both s,t are rooted by f and 
    in multiset comparison of args(s), args(t) term t_j is covered by s_i *)
 mcov_a_encodings   : (Term.t list * Term.t list * int, L.a) H.t;
 (* multiset comparison formula caching *)
 mcov_encodings     : (Term.t list*Term.t list, L.p*L.a list) H.t;
 (* for argument filterings *)
 af_l                  : (Fun.t, L.p) H.t;
 af_p                  : (Fun.t * int, L.p) H.t;
 emb_gt_encodings      : (Rule.t, Logic.p) H.t;
 emb_eq_encodings      : (Rule.t, Logic.p) H.t;
 eq_af_encodings       : (Fun.t*Fun.t*Term.t list*Term.t list, L.p) H.t;
 rpo_af_same_encodings : (Fun.t*Term.t list*Term.t list,L.p) H.t;
 rpo_af_diff_encodings : (Fun.t*Fun.t*Term.t list*Term.t list,L.p) H.t;
 usable_funs           : (Fun.t, L.p) H.t
};;

type flags = {
 dp : bool ref;
 direct : bool ref;
 help : bool ref;
(* quasi : bool ref;*)
 sat : bool ref;
 ur: bool ref
};;

type t = {
 af : AF.t option;
 input : P.t;
 output : P.t;
 precedence : Prec.t;
 status : Status.t;
 urs : Trs.t
};;

(*** GLOBALS ******************************************************************)
let code = "acrpo";;
let name = "AC-RPO Processor";;
let comment = "Applies AC-Recursive Path Order."

let keywords =
 ["AC";"recursive path order";"simplification order";"termination"]
;;

let flags = {
 dp = ref false;
 direct = ref false;
 help = ref false;
(* quasi = ref false;*)
 sat = ref false;
 ur = ref false;
};;

let spec =
 let spec = [
  ("-af",Arg.Set flags.dp,
   "Allows non-monotone interpretations (argument filterings).");
  ("-direct",Arg.Set flags.direct,
   "Try to finish termination proof.");
  ("--help",Arg.Set flags.help,"Prints information about flags.");
  ("-help",Arg.Set flags.help,"Prints information about flags.");
  ("-h",Arg.Set flags.help,"Prints information about flags.");
 (* ("-quasi",Arg.Set flags.quasi,"Allows quasi-precedences (currently not \
   supported together with -dp flag).");*)
  ("-sat",Arg.Set flags.sat,"Uses SAT backend.");
  ("-smt",Arg.Clear flags.sat,"Uses SMT backend (default).");
  ("-ur",Arg.Set flags.ur,"Apply usable rules.");]
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
 flags.dp := false;
 flags.direct := false;
 flags.help := false;
(* flags.quasi := false;*)
 flags.sat := false;
 flags.ur := false
;;

(* Constructors and Destructors *)
let make af input output precedence st urs = {
 af = af;
 input = input;
 output = output;
 precedence = precedence;
 status = st;
 urs = urs
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
 F.fprintf fmt "@]@\n@[<1>status:@\n";
 Status.fprintfm fmt p.status >>= fun _ ->
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

let map_op op f ls = sequence (List.map f ls) >>= (return <.> op);;
let mapi_op op f ls = sequence (List.mapi f ls) >>= (return <.> op);;
let gen_op op f n = sequence (List.gen f n) >>= (return <.> op);;
let map_and f = map_op L.big_and f;;
let mapi_and f = mapi_op L.big_and f;;
let map_or f = map_op L.big_or f;;
let mapi_or f = mapi_op Logic.big_or f;;
let map_add f = map_op L.big_sum f;;
let gen_or f = gen_op Logic.big_or f;;
let gen_and f = gen_op Logic.big_and f;;

(* actual content starts here *)
let context state fs =
 let solver = if !(flags.sat) then L.MiniSat else L.Yices in
 {
  solver            = solver;
  prec_num          = max 0 (List.length fs - 1);
  precs             = H.create 512;
  state             = state;
  gt_encodings      = H.create 512;
  stats             = H.create 512;
  mcov_a_encodings  = H.create 512;
  mcov_encodings    = H.create 512;
  af_l              = H.create 512;
  af_p              = H.create 512;
  emb_gt_encodings  = H.create 512;
  emb_eq_encodings  = H.create 512;
  eq_af_encodings   = H.create 512;
  rpo_af_same_encodings = H.create 512;
  rpo_af_diff_encodings = H.create 512;
  usable_funs       = H.create 64
 }
;;

(* administrative functions *)
let cache_m tbl f k = 
 if H.mem tbl k then return (H.find tbl k)
 else (f k >>= fun v -> (H.add tbl k v; return v))
;;

(* encoding starts here *)
(* caching variables *)
let prec f = get >>= fun c -> 
 let arith = L.nat c.prec_num in
 cache_m c.precs (const (fresh_arith_spec arith)) f
;;

let prec_gt f g = prec f $>$ prec g;;
let not_prec_lt f g = mnot (prec g $>$ prec f);;
let prec_eq f g = return (if f = g then L.top else L.bot);; (* not quasi *)
let is_mul f = get >>= fun c -> cache_m c.stats (const fresh_bool) f;;
let is_lex = mnot <.> is_mul;;
let is_ac f = get >>= fun c -> return (Sig.is_theory Label.AC f c.state);;
let is_c f = get >>= fun c -> return (Sig.is_theory Label.C f c.state);;
let set_ac f =
 get >>= fun c -> let (f, s) = Sig.set_theory f Label.AC c.state in
 Made.set {c with state = s} >>
 return f
;;
let arity f = get >>= fun c -> return (Sig.find_ari f c.state);;
let af_l f = get >>= fun c -> cache_m c.af_l (const fresh_bool) f;;
let af_n f = af_l f >>= (return <.> L.neg);;
let af_p f i = get >>= fun c -> cache_m c.af_p (const fresh_bool) (f,i);;
let af_ac_all f = (af_l f) $&$ (af_p f 0) $&$ (af_p f 1)
let af_ac_none f = (af_l f) $&$ (mnot ((af_p f 0) $|$ (af_p f 1)))

let us_f f = get >>= fun c -> cache_m c.usable_funs (const fresh_bool) f

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
 (*cache_m c.mcov_a_encodings*) (const (fresh_arith_spec arith)) key
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

let diff ?(cmp=compare) xs ys =
 let rec rem x = function
  | [] -> []
  | y::ys when cmp x y = 0 -> ys
  | y::ys -> y::(rem x ys)
 in List.foldl (flip rem) xs ys
;;


let mul_ge gt eq ss ts =
 let ss,ts = diff ~cmp:compare ss ts, diff ~cmp:compare ts ss in 
 if List.is_empty ss && List.is_empty ts then 
  return L.top
 else
  mul gt eq ss ts 
;;

let mul_gt gt eq ss ts = 
 let ss,ts = diff ~cmp:compare ss ts, diff ~cmp:compare ts ss in
 if ss = [] then return L.bot else mul gt eq ss ts 
;;

let mul_eq gt eq ss ts =
 let ss,ts = diff ~cmp:compare ss ts, diff ~cmp:compare ts ss in 
 if List.is_empty ss && List.is_empty ts then 
  return L.top
 else 
  return L.bot
;;

(* to compute no_small_head and big_head multiset comparisons *)
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
 let ss,ts = diff ~cmp:compare ss ts, diff ~cmp:compare ts ss in
 mul_cover ss ts >>= fun (cover,cs) ->
 mul_g' cs p gt eq ss ts >>= fun phi ->
 return (cover <&> phi)
;;

let mul_gt' p gt eq ss ts =
 let ss,ts = diff ~cmp:compare ss ts, diff ~cmp:compare ts ss in
 mul_cover ss ts >>= fun (cover,cs) ->
 mul_g' cs p gt eq ss ts >>= fun phi ->
 map_or p ss >>= fun non_empty ->
 return (cover <&> phi <&> non_empty)
;;

(* emb_small candidates *)
let emb_sm f ts =
 let tf t = if Term.root t = Some f then Term.args t else [t] in 
 let rec emb hd =  function
  | [] -> []
  | ((Term.Var _) as t)::ts -> emb (hd@[t]) ts 
  | (Term.Fun(h,hs) as t)::ts when f=h -> emb (hd@[t]) ts
  | (Term.Fun(h,hs) as t)::ts -> 
   (List.map (fun hi -> (hd@(tf hi)@ts,h)) hs)@(emb (hd@[t]) ts)
 in List.map (Pair.apply (Term.make_fun f) id) (emb [] ts )
;;

(* #s > #t *)
let size_gt ss ts = 
 let (vs,fs),(vt,ft) = Pair.map (List.partition Term.is_var) (ss,ts) in
 let vs,vt = diff vs vt,diff vt vs in 
 (List.is_empty vt) && (List.length (vs@fs) > (List.length ft))
;;

(* #s = #t *)
let size_ge ss ts =
 let (vs,fs),(vt,ft) = Pair.map (List.partition Term.is_var) (ss,ts) in
 let vs,vt = diff vs vt,diff vt vs in
 (List.is_empty vt) && (List.length (vs@fs) >= (List.length ft))
;;

(* until quasi prec *)
let rpo_eq r = 
 let s,t = R.to_terms r in 
 return (if Term.compare s t = 0 then L.top else L.bot)
;;

let flat_rule r =
  Made.project flatten (R.to_terms r) >>= fun (l,r) ->
  return (R.of_terms l r)
;;

let rec rpo_gt rule =
 flat_rule rule >>= fun rule ->
(* Format.printf "check: %s > %s\n%!" (Term.to_string s) (Term.to_string t);*)
 let helper rule = 
  if is_embedded (R.invert rule) || not (R.is_rewrite_rule rule) then
   return L.bot
  else if is_proper_embedded rule then 
   return L.top
  else
   let s,t = R.to_terms rule in 
   match s,t with
   | Term.Var _, _ -> return L.bot
   | Term.Fun _, Term.Var x -> 
    return (if List.mem x (Term.vars s) then L.top else L.bot)
   | Term.Fun (f,ss), Term.Fun (g,ts) when f <> g ->
    map_or (fun si -> rpo_ge (R.of_terms si t)) ss  $|$                  (* (1) *)
    (prec_gt f g $&$ map_and (fun tj -> rpo_gt (R.of_terms s tj)) ts)      (* (2) *)
   | Term.Fun (f,ss), Term.Fun (_,ts)  ->
    is_ac f >>= fun f_is_ac -> is_c f >>= fun f_is_c ->
    if not (f_is_ac || f_is_c) then
     map_or (fun si -> rpo_ge (R.of_terms si t)) ss  $|$                 (* (1) *)
     ((is_lex f) $&$ (lex rpo_gt rpo_eq ss ts)) $|$                         (* (3) *)
     ((is_mul f) $&$ (mul_gt rpo_gt rpo_eq ss ts))                          (* (4) *)
    else if f_is_c then
     mul_gt rpo_gt rpo_eq ss ts (* multiset extension *)
    else (
     let gt s t = rpo_gt (R.of_terms s t) in
     let ge s t = rpo_ge (R.of_terms s t) in
     let cover h s' = prec_gt f h $&$ (ge s' t)  in
     map_or (fun (s',h) -> flatten s' >>= cover h) (emb_sm f ss) $|$        (* (5) *)
     (map_and (fun (t',h) -> prec_gt f h $->$ (gt s t')) (emb_sm f ts) $&$
      (no_small_head f ss ts)  $&$                                          (* (6) *)
      (if size_gt ss ts then return L.top                               (* (6b) *)
       else if not (size_ge ss ts) then big_head f ss ts                    (* (6a) *)
       else ((big_head f ss ts) $|$                                             
           (mul_gt rpo_gt rpo_eq ss ts))))                                  (* (6c) *)
)
  in
  get >>= fun c -> cache_m c.gt_encodings helper rule

and no_small_head f ss ts =
 let p u = match Term.root u with 
  None -> return L.top | Some h -> mnot (prec_gt f h) in
 mul_ge' p rpo_gt rpo_eq ss ts

and big_head f ss ts =
 if List.is_empty ss then return L.bot 
 else
  let p u = match Term.root u with    
   None -> return L.bot | Some h -> prec_gt h f in
  mul_gt' p rpo_gt rpo_eq ss ts

and rpo_ge rule = rpo_gt rule $|$ (rpo_eq rule) 
;;

(* encode with argument filtering *)
(* useful shorthands. *)
let exists f p ts = mapi_or (fun i ti -> (af_p f i) $&$ (p ti)) ts;;
let existsi f p ts = mapi_or (fun i ti -> (af_p f i) $&$ (p i ti)) ts;;
let for_all f p ts = mapi_and (fun i ti -> (af_p f i) $->$ (p ti)) ts;;
let exists2 f p ss ts = exists f p (List.map2 R.of_terms ss ts);;
let for_all2 f p ss ts = for_all f p (List.map2 R.of_terms ss ts);;
let rot f s t = f (R.of_terms s t);;
let rot2 f s t = f (R.of_terms t s);;

let fun_cond cmp f u =
 match Term.root u with
  None -> return L.bot | Some h -> cmp h f
;;


let af_collapsing f a i =
 let make j = if i = j then af_p f i else (lift L.neg (af_p f j)) in
 (gen_and make a) $&$  (lift L.neg (af_l f))
;;

let lex_af f eq gt ss ts =
 let rec lex' eq gt ss ts i =
  match ss, ts with
   | [],[] -> return L.bot
   | [], _ | _, [] -> failwith "lists of different length"
   | sn::sss,tn::tss ->
     (af_p f i $&$ gt (R.of_terms sn tn))
     $|$
     ((af_p f i $->$ eq (R.of_terms sn tn)) $&$ lex' eq gt sss tss (i+1))
 in
 lex' eq gt ss ts 0
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
 let (s,t) = R.to_terms rule in match (s,t) with
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
   eq_af_diff eq_af f g ss ts
 in get >>= fun c -> cache_m c.emb_eq_encodings helper rule
;;

let mul_af f gt eq ss ts =
 mul_cover ss ts >>= fun (cover,cs) ->
 let tcs = List.zip ts cs in
 let is cj i = return (cj <=> (constant i)) in
 mapi_and (fun i si -> mapi_and (fun j (tj,cj) ->
  af_p f j $->$
   ((is cj i) $->$ (gt (R.of_terms si tj) $&$ (af_p f i)))
  ) tcs) ss >>= fun phi ->
 return (cover <&> phi)
;;

let mul_ge_af f gt eq ss ts =
 let ss,ts = diff ~cmp:compare ss ts, diff ~cmp:compare ts ss in
 if List.is_empty ss && List.is_empty ts then return L.top
 else 
  let rl = R.of_terms (Term.Fun (f, ss)) (Term.Fun (f, ts)) in
  (mul_af f gt eq ss ts) $|$ (eq_af rl) 
;;

let mul_gt_af f gt eq ss ts =
 let ss,ts = diff ~cmp:compare ss ts, diff ~cmp:compare ts ss in
 if ss = [] then return L.bot 
 else
  mul_cover ss ts >>= fun (cover,cs) ->
  let tcs = List.zip ts cs in
  let is cj i = return (cj <=> (constant i)) in
  let phi = mapi_and (fun i si -> mapi_and (fun j (tj,cj) ->
   (af_p f j $&$ af_l f) $->$
    ((is cj i) $->$ (gt (R.of_terms si tj) $&$ (af_p f i)))
   ) tcs) ss
  in
  let is_strict i si j tj = is (List.nth cs j) i $&$ gt (R.of_terms si tj) in
  let strict = existsi f (fun i si -> existsi f (is_strict i si) ts) ss in
  let s_nonempty = exists f (fun _ -> return L.top) ss in
  let t_empty = mnot (exists f (fun _ -> return L.top) ts) in
  phi $&$ (strict $|$ (s_nonempty $&$ t_empty))
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

let cond_mul_gt gt eq ss ts = 
 cond_mul gt eq ss ts >>= fun (c,s) -> return (c <&> s)

let cond_mul_ge gt eq ss = lift fst <.> (cond_mul gt eq ss)

(* to compute no_small_head and big_head multiset comparisons *)
(*let mul_g_af' cs (ps,pt) gt eq ss ts =
 let tcs = List.zip ts cs in
 let gt si tj = gt (R.of_terms si tj) in
 let has_val cj i = return (cj <=> (constant i)) in
 mapi_and (fun j (tj,cj) ->
  (pt tj) $->$
  (mapi_and (fun i si -> has_val cj i $->$ (gt si tj $&$ (ps si))) ss))
 tcs
;;

let mul_ge_af' p gt eq ss ts =
 let ss,ts = diff ~cmp:compare ss ts, diff ~cmp:compare ts ss in
 mul_cover ss ts >>= fun (cover,cs) ->
 mul_g_af' cs p gt eq ss ts >>= fun phi ->
 return (cover <&> phi)
;;

let mul_gt_af' (ps,pt) gt eq ss ts =
 let ss,ts = diff ~cmp:compare ss ts, diff ~cmp:compare ts ss in
 mul_cover ss ts >>= fun (cover,cs) ->
 let tcs = List.zip ts cs in
 let gt si tj = gt (R.of_terms si tj) in
 let has_val cj i = return (cj <=> (constant i)) in
 let phi = 
  mapi_and (fun j (tj,cj) ->
   (pt tj) $->$
   (mapi_and (fun i si -> has_val cj i $->$ (gt si tj $&$ (ps si))) ss))
  tcs
 in
 let strict =
  mapi_or (fun j (tj,cj) ->
   (pt tj) $&$ (mapi_or (fun i si -> has_val cj i $&$ (gt si tj $&$ (ps si))) ss))
  tcs
 in
 let s_nonempty = map_or ps ss in
 let t_empty = mnot (map_or pt ts) in
 phi $&$ (strict $|$ (s_nonempty $&$ t_empty))
;;*)


(* generate list of conditional direct subterms *)
let cond_sub =
 let add t p l = lift2 List.cons (cross (return, id) (t,p)) l in
 let rec cs c = function
  | (Term.Var _) as x -> add x c (return [])
  | (Term.Fun (f, ts) as t) ->
   let tcs = mapi (fun i ti -> cs (af_n f $&$ af_p f i $&$ c) ti) ts in
   add t (af_l f $&$ c) (lift List.concat tcs)
 in cs (return Logic.top)
;;

let cond_prop_sub t = flat_map cond_sub (Term.args t)

(* emb_small candidates for af*)
let emb_sm_af f ts =
 let consider (t,c) = match t with Term.Fun(h,hs) -> cond_prop_sub t >>= 
  map (fun (u,c') -> prec_gt f h >>= fun fh -> return (u, c <&> c' <&> fh))
 in
 let rec emb (hd,tl) acc = 
  match tl with
   | [] -> return acc
   | t :: tl ->
    cond_sub t >>= fun ts ->
    let ts = List.filter (Term.is_fun <.> fst) ts in
    flat_map consider ts >>= fun us ->
    let acc' =  (List.map (fun (u,c) -> Term.Fun(f, hd @ (u::tl)), c) us) in
    emb (hd@[t],tl) (acc' @ acc)
 in emb ([],ts) [] 
;;


let rec emb_af_ge rule = emb_af_gt rule $|$ eq_af rule
and emb_af_gt rule =
  flat_rule rule >>= fun rule ->
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

let rpo_af_same eq gt f ts0 ts1 =
 let s = Term.Fun (f,ts0) in
 let t = Term.Fun (f,ts1) in
 let geq rule = eq rule $|$ gt rule in
 let nc = is_c f >>= fun c -> return (if c then L.top else L.bot) in
 let helper (eq,gt) (f0,ts0,ts1) =
  let rec_gt = for_all f (rot gt s) ts1 in
  (af_l f $&$ is_lex f $&$ lex_af f eq gt ts0 ts1 $&$ rec_gt $&$ nc) $|$
  (af_l f $&$ is_mul f $&$ mul_gt_af f gt eq ts0 ts1 $&$ rec_gt) $|$
  (af_l f $&$ exists f (rot2 geq t) ts0) $|$
  (af_n f $&$ exists2 f gt ts0 ts1)
 in get >>= fun c -> cache_m c.rpo_af_same_encodings (helper (eq,gt))
 (f,ts0,ts1)
;;

let rpo_af_diff eq gt f0 f1 ts0 ts1 =
 let helper gt (f0,f1,ts0,ts1) =
 let s = Term.Fun (f0, ts0) and t = Term.Fun (f1, ts1) in
 (af_l f0 $&$ af_l f1 $&$ exists f0 (rot2 eq t) ts0) $|$
 ((af_n f0 $|$ af_l f1) $&$ exists f0 (rot2 gt t) ts0) $|$
 (((af_l f0 $&$ af_l f1 $&$ prec_gt f0 f1) $|$ af_n f1) $&$
  for_all f1 (rot gt s) ts1)
 in get >>= fun c -> cache_m c.rpo_af_diff_encodings (helper gt) (f0,f1,ts0,ts1)
;;

let p_term v f = function Term.Var _ -> v | Term.Fun(h,_) -> f h;;

(* produces list ts' of pairs (t,c) of term t and condition c such that
   t appears in the topflattening of f0(ts) iff c holds *)
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
 in lift List.concat (map (cs Logic.top) ts)
;;

let tcond p = map (fun (t,c) -> p t >>= fun c' -> return (t,c <&> c'))

(* computes conditions sge, sgt that have to hold in order to
   satisfy #ss >= #ts and #ss > #ts, respectively *)
let cond_size (ss,ts) =
 (* conditionally count variable occurrences *) 
 let sx x (t,c) = 
  match t with Term.Var y when x = y -> L.ite c L.one L.zero | _ -> L.zero
 in
 let map_add f = L.big_sum <.> (List.map f) in
 let cmp_x cmp x = cmp (map_add (sx x) ss) (map_add (sx x) ts) in
 (* conditionally count occurrences of functional terms *)
 let s (t,c) = match t with Term.Fun _ -> L.ite c L.one L.zero | _ -> L.zero in
 let vs = List.unique (List.flat_map (Term.vars <.> fst) (ss @ ts)) in
 let geqx = L.big_and (List.map (cmp_x (<>=>)) vs) in
 let gtx = L.big_or (List.map (cmp_x (<>>)) vs) in
 let cs, ct = map_add s ss, map_add s ts in
 geqx <&> (cs <>=> ct), geqx <&> ((cs <>> ct) <|> (gtx <&> (cs <>=> ct)))
;;

let rec rpo_gt_af rule =
 flat_rule rule >>= fun rule ->
 let helper rl =
   let s,t = R.to_terms rl in
   match s,t with
   | Term.Var _, _ -> return L.bot
   | Term.Fun _, Term.Var x -> emb_af_gt rl
   | Term.Fun (f,ss), Term.Fun (g,ts) when f <> g ->
    rpo_af_diff eq_af rpo_gt_af f g ss ts
   | Term.Fun (f,ss), Term.Fun (_,ts)  ->
    is_ac f >>= fun f_is_ac -> is_c f >>= fun f_is_c ->
    if not (f_is_ac || f_is_c) then
     rpo_af_same eq_af rpo_gt_af f ss ts
    else if f_is_c then
     mul_gt rpo_gt_af eq_af ss ts (* multiset extension *)
    else(
     (*Format.printf "\nR: %s\n%!" (R.to_string (Rule.of_terms s t));*)
     project (cond_topflat f) (ss,ts) >>= fun (ss',ts') ->
     let sge, sgt = cond_size (ss', ts') in
     (* case (5) *)
     project (emb_sm_af f) (ss,ts) >>= fun (es,et) ->
     let c5 = map_or (fun (si,psi) -> return psi $&$ (rot rpo_ge_af si t)) es in
     (* case (6) *)
     project (tcond (fun_cond not_prec_lt f)) (ss',ts') >>= fun (ss0, ts0) ->
     let c6 = map_and (fun (ti,pti) -> return pti $->$ (rot rpo_gt_af s ti)) et in
     let c6' = cond_mul_ge rpo_gt_af eq_af ss0 ts0 in
     (* case (6a) *)
     project (tcond (fun_cond prec_gt f)) (ss',ts') >>= fun (ss1, ts1) ->
     let c6a = cond_mul_gt rpo_gt_af eq_af ss1 ts1 in
     (* case (6b) *)
     let c6b = return sgt in
     (* case (6c) *)
     let c6c = cond_mul_gt rpo_gt_af eq_af ss' ts' $&$ (return sge) in
     af_ac_all f $&$ (c5 $|$ (c6 $&$ c6' $&$ (c6a $|$ c6b $|$ c6c))) )
  in get >>= fun c -> cache_m c.gt_encodings helper rule

and rpo_ge_af rule = rpo_gt_af rule $|$ (eq_af rule)
;;

(* encoding *)
let rpo_gt r = get >>= fun c -> if !(flags.dp) then rpo_gt_af r else rpo_gt r

let rpo_ge r = get >>= fun c -> if !(flags.dp) then rpo_ge_af r else rpo_ge r

let total_prec fs = 
 let comparable (f,g) = prec_gt f g $|$ prec_gt g f in
 map_and comparable (List.filter (fun (a,b) -> compare a b < 0) (List.square fs))
;;

let flat s w =
 let s, w = Pair.map Trs.to_list (s, w) in
 Made.project ((lift Trs.of_list) <.> (Made.map flat_rule)) (s,w)
;;

let encode_gt s = 
 (if !(flags.direct) then map_and else map_or) rpo_gt s

let encode_ge s w = 
 let geq =  if !(flags.dp) then rpo_ge_af else rpo_ge in
 if !(flags.ur) then
  let ge r = (us_f (Option.the (Term.root (R.lhs r)))) $->$ (geq r) in
  (map_and geq s) $&$ map_and ge w
 else map_and geq (s@w)
;;

let af_f f = 
 arity f >>= fun a -> 
 is_ac f >>= fun ac -> is_c f >>= fun c -> 
 if ac || c then ((af_ac_all f) $|$ (af_ac_none f)) $&$ 
  (mnot (af_collapsing f a 0)) $&$ (mnot (af_collapsing f a 1))
 else af_l f $|$ gen_or (af_collapsing f a) a
;;
let af fs = map_and af_f fs;;
let af fs = get >>= fun c -> if !(flags.dp) then af fs else return Logic.top;;


(* encode which function symbols are potential roots for usable rules *)
let ur s w =
 let rhss = List.map Rule.rhs s in
 let fs = List.unique (List.flat_map Term.funs rhss) in
 if !(flags.dp) then 
   let rec us_in c = function
    | Term.Var _ -> return Logic.top
    | Term.Fun (f,ts) ->
     let us_in' i ti = af_p f i >>= fun pi -> us_in (c <&> pi) ti in
     (return c (*$&$ af_l f*) $->$ (us_f f)) $&$ mapi_and us_in' ts
   in
   let reachable f =
    let root_f rl = Option.the (Term.root (R.lhs rl)) = f in
    let rs = List.filter root_f w in
    let rhss = List.map Rule.rhs rs in
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

let encode s w = get >>= fun c ->
 flat s w >>= fun (s,w) ->
 let fs = Trs.funs (Trs.union s w) in
 let s, w = Pair.map Trs.to_list (s, w) in
 encode_gt s $&$ encode_ge s w $&$ total_prec fs $&$ af fs $&$ ur s w
;;

(* decode from assignment *)
let decode_rule ass rule = 
 get >>= fun c -> 
 flat_rule rule >>= fun rule ->
 (* L.fprintf_assignment Format.std_formatter ass;*)
 lift not (eval_p (rpo_gt rule) ass)
;;

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

let decode_af_f ass f =
 arity f >>= fun a ->
 sequence (List.gen (fun i -> eval_p (af_p f i) ass) a) >>= fun ps ->
 let psi = List.mapi Pair.make ps in
 let ps = List.filter snd psi in
 let ps = List.map fst ps in
 eval_p (af_l f) ass >>= fun l ->
 if l then return (f,AF.List ps)
 else ((*assert (List.length ps = 1);*) return (f,AF.Collapsing (List.hd ps)))
;;

let decode_af ass s w = get >>= fun c ->
 if !(flags.dp) then (
 let fs = Trs.funs (Trs.union s w) in
 Made.sequence (List.map (decode_af_f ass) fs) >>= fun af ->
 return (Some (AF.of_list af))
 ) else return None
;;

let decode_ur ass s w =
 let lrt r = Option.the (Term.root (R.lhs r)) in
 filter (fun r -> eval_p (us_f (lrt r)) ass) (Trs.to_list w) >>= fun l ->
 return (Trs.of_list l)
;;

let solve signature fs p = 
 let configurate s = F.printf "%s@\n%!" s; flags.help := true in
 (try init (); Arg.parsex code spec fs with Arg.Bad s -> configurate s);
 if !(flags.help) then (Arg.usage spec ("Options for "^code^":"); exit 0);
 let (s,w) = P.get_sw p in
 let c = context signature (Trs.funs (Trs.union s w)) in
 L.run (
  Made.run c (encode s w >>= fun phi ->
  Made.liftm (L.solve ~solver:c.solver phi) >>= function
   | None -> return None
   | Some ass ->
    decode ass s w >>= fun (s',w') -> 
    decode_af ass s w >>= fun af ->
    decode_prec ass s w >>= fun prec -> 
    decode_status ass s w >>= fun status ->
    decode_ur ass s w >>= fun urs ->
    return (Some (make af p (P.set_sw s' w' p) prec status urs))))
;;

(* wrap into state monad *)
let (>>=) = M.(>>=);;
let solve fs p = M.get >>= fun s -> M.return (solve s fs p);;
