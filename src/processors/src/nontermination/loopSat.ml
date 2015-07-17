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
module F = Format;;
module Fun = Function;;
module Var = Variable;;
module Sig = Signature;;
module Number = Logic.Number;;

(*** TYPES ********************************************************************)
type flags = {
 help : bool ref;
 plain : bool ref;
 col : int ref;
 row : int ref;
 sat : bool ref;
 transform : bool ref;
};;

type context = {
 cols    : int;
 rows    : int;
 dp      : bool;
 arith   : Logic.arith;
 obits   : int;
 e_tbl   : (int, Logic.a) Hashtbl.t;
 p_tbl   : (int, Logic.a) Hashtbl.t;
 vat_tbl : ((Fun.t*int*Logic.a), Logic.p) Hashtbl.t;
 vct_tbl : ((Fun.t*int*int), Logic.p) Hashtbl.t;
 vr_tbl  : ((int*Rule.t), Logic.p) Hashtbl.t;
 solver  : Logic.solver;
 state   : Sig.t;
 x       : Term.t;
};;

type t = Problem.t * Loop.t;;

(*** GLOBALS ******************************************************************)
let code = "loop";;
let name = "Loop Processor";;
let keywords = ["sat";"loop";"nontermination"];;

let comment =
 "This processor searches for loops (SRSs only) by using a SAT solver."
;;

let flags = {
 help = ref false;
 plain = ref true;
 col = ref 10;
 row = ref 10;
 sat = ref true;
 transform = ref true;
};;

let spec =
 let spec = [
  ("-c",Arg.Set_int flags.col,"Specifies columns of matrix.");
  ("-dp",Arg.Clear flags.plain,"Search loops in DP setting (required).");
  ("--help",Arg.Set flags.help,"Prints information about flags.");
  ("-help",Arg.Set flags.help,"Prints information about flags.");
  ("-h",Arg.Set flags.help,"Prints information about flags.");
  ("-r",Arg.Set_int flags.row,"Specifies rows of matrix.");
  ("-sat",Arg.Set flags.sat,"Use SAT backend (default).");
  ("-smt",Arg.Clear flags.sat,"Use SMT backend.");
  ("-nt",Arg.Clear flags.transform,"Do not transform DP-loop into loop in \
  original system.");
  ] in
 Arg.alignx 80 spec
;;

let help = (comment,keywords,List.map Triple.drop_snd spec);;

(*** MODULES (part 2) *********************************************************)
module Statex = struct type t = context end;;
module Made = Util.Monad.Transformer.State(Statex)(Logic.Monad);;
open Made;;

(*** FUNCTIONS ****************************************************************)
let init _ =
 flags.help := false;
 flags.plain := true;
 flags.col := 10;
 flags.row := 10;
 flags.sat := true;
 flags.transform := true
;;

(* Processor *)
(*TODO copy from KBO*)
let rec symbols_list = function 
 | Term.Var _ -> []
 | Term.Fun (f,ts) -> f::List.flat_map symbols_list ts
;;

(*HZ TODO move to somewhere*)
(*a function checking if string [s] is a prefix of string [t] *)
let is_prefix s t = 
 Util.uncurry List.is_prefix (Pair.map symbols_list (s,t))
;;

(***********functionality starts here***************)
let get_loop = id;;

(******** administrative functions **********)
let context state arith obits solver trs = 
 let vs = Trs.vars trs in
 (* FIXME: if no variable in [trs] then printing raises error *)
 let x = if vs = [] then (fst (Sig.create_var "x" state)) else List.hd vs in
 {cols    = !(flags.col);
  rows    = !(flags.row);
  dp      = not !(flags.plain);
  arith   = arith; 
  obits   = obits;
  e_tbl   = Hashtbl.create 512;
  p_tbl   = Hashtbl.create 512;
  vat_tbl = Hashtbl.create 512;
  vct_tbl = Hashtbl.create 512;
  vr_tbl  = Hashtbl.create 512;
  solver  = solver;
  state   = state;
  x       = Term.Var x}
;;

let cache_m tbl f k = 
 if Hashtbl.mem tbl k then return (Hashtbl.find tbl k)
 else (f k >>= fun v -> (Hashtbl.add tbl k v; return v))
;;

(******* functions lifted from Logic into Made *******)
let fresh_arith = get >>= fun s -> liftm (Logic.fresh_arith s.arith);;
let fresh_bool = get >>= fun s -> liftm Logic.fresh_bool;;
let of_nat n = Logic.constant (Number.of_int n);;

let ($|$) = lift2 (<|>);;
let ($&$) = lift2 (<&>);;
let ($->$) = lift2 (<->>);;
let ($+$) = lift2 (<+>);;
let ($<->$) = lift2 (<<->>);;
let ($=$) = lift2 (<=>);;
let ($>$) = lift2 (<>>);;
let ($>=$) = lift2 (<>=>);;

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

(*******getting variables***********)
let vat a i j = get >>= fun c -> cache_m c.vat_tbl (const fresh_bool) (a,i,j);;
let vct a i j = get >>= fun c -> cache_m c.vct_tbl (const fresh_bool) (a,i,j);;
let vr i j = get >>= fun c -> cache_m c.vr_tbl (const fresh_bool) (i,j);;
let get_e i = get >>= fun c -> cache_m c.e_tbl (const fresh_arith) i;;
let get_p i = get >>= fun c -> cache_m c.p_tbl (const fresh_arith) i;;

(* get all matrix variables performed at abstract positions  *)
let get_vat pf = get >>= fun c ->
 return (Hashtbl.fold (fun k v acc -> k::acc) c.vat_tbl []);;

(* match abstract and conrete variables *)
let vm_aij row col (a,i,j) = 
 if i <> row  then return Logic.top 
 else ((return (of_nat col <=> j)) $->$ (vat a i j $<->$ (vct a row col)))
;;

let vm_ij ls row col = map_and (vm_aij row col) ls;;
let vm_i ls i = get >>= fun c -> gen_and (vm_ij ls i) c.cols;;
let vm ls = get >>= fun c -> gen_and (vm_i ls) c.rows;;

let exclude i j f fs = 
 (vct f i j) $->$ (map_and (fun g -> lift (~!) (vct g i j)) fs);;

let rec at_most_one i j = function
 | [] -> return Logic.top
 | f::fs -> (exclude i j f fs) $&$ (at_most_one i j fs)
;; 

(* exactly one entry in M(i, j)*)
let eo_ij fs i j =
 (map_or (fun a -> vct a i j) fs) $&$ (at_most_one i j fs)
;;
let eo_i dp_fs fs i = get >>= fun c ->
 sequence ((eo_ij dp_fs i 0) ::
  (List.map (fun a -> lift (~!) (vct a i 0)) fs) @
  (List.gen (fun j -> eo_ij fs i (j+1)) (c.cols - 1))) >>=
 (return <.> Logic.big_and)
;;
let eo dp_fs fs = get >>= fun c -> gen_and (eo_i dp_fs fs) c.rows;;

(* find a loop in rows i > 0 (or ~rows/2) *)
let loop_ij dp_fs fs i j = get >>= fun c -> 
 if i < max 1 (c.rows / 2 - 1) then return Logic.bot
 else
  if j = 0 then map_and (fun a -> (vct a 0 j) $->$ (vct a i j)) dp_fs
  else map_and (fun a -> (vct a 0 j) $->$(vct a i j)) fs
;;

(* copy transition before redex, replace redex, copy transitions after *)
let cp_ijr2 i j0 j1 dp_fs fs = 
 let funs = (if j0 = 0 && j1 = 0 then dp_fs else fs) in
 map_and (fun a -> (vct a i j0 $->$ vct a (i+1) j1)) funs
;;
let cp_ijr dp_fs fs rule i j = get >>= fun c -> 
 let l,r = Rule.map Term.depth rule in
 if j + max l r > c.cols then return (Logic.top)
 else if List.mem (Option.the(Term.root (Rule.lhs rule))) dp_fs then
  cp_ijr2 i (j+l) (j+r) [] fs
 else
  let bj = of_nat j in 
  get_p i >>= fun p_i ->
  ((return (p_i <>> bj)) $&$ (cp_ijr2 i j j dp_fs fs)) 
   $|$ ((return (bj <>=> p_i)) $&$ (cp_ijr2 i (j+l) (j+r) [] fs))
;; 

(* apply string [l] in line [i] starting at column [aux] *)
let apply_string l i aux =
 mapi_and (fun j a -> vat a i (aux <+> (of_nat j))) (symbols_list l);;
(* apply string [l] in line [i] starting at column 0 *)
let apply_string' l i = mapi_and (fun j a -> vct a i j) (symbols_list l);;
(* apply [rule] in line [i] *)
let applies_i_lr ?(strict=[]) i rule =
 let l, r = Rule.lhs rule, Rule.rhs rule in
 let sl, sr = Pair.map Term.depth (l,r) in 
 get_e i >>= fun e_i ->
 get_e (i+1) >>= fun e_ip1 ->
 get_p i >>= fun p_i ->
 let tmp2 = e_ip1 <+> (of_nat sl) in
 let tmp3 = e_i <+> (of_nat sr) in
 (*let tmp4 = p_i <+> (of_nat sl) in*)
 (if List.mem rule strict 
  then apply_string' l i else apply_string l i p_i) >>= fun p1 ->
 (if List.mem rule strict 
  then apply_string' r (i+1) else apply_string r (i+1) p_i) >>= fun p2 ->
 (*OPT*)
 let p3 = tmp3 <=> tmp2 in
 (*
 let p3 = BinNat.greater_equal (add e_i (BinNat.of_nat sr))
  (add e_ip1 (BinNat.of_nat sl))
 in
 *)
 let p4 = e_i <>=> (p_i <+> (of_nat sl)) in
 (* let p5 = BinNat.greater_equal e_ip1 (add p_i (BinNat.of_nat sr)) in *)
 return (Logic.big_and ([p1;p2;p3;p4;(*p5*)]))
;;

(* one rule of [rs] applies at line [i] <= [!rows] - 1*)
let one_rule_applies rs i = get >>= fun c -> 
 if i >= c.rows - 1 then return Logic.top else map_or (vr i) rs;;
(* apply rule [r] in line i *)
let apply_rule_i_lr ?(strict = []) fs_dp fs rs i r = get >>= fun c -> 
 (vr i r $->$
 (applies_i_lr ~strict:strict i r $&$
  (if List.mem r strict then (get_p i) $=$ (return Logic.zero)
   else (get_p i) $>$ (return Logic.zero)
  ) $&$ (gen_and (cp_ijr fs_dp fs r i) c.cols)))
 $&$ (one_rule_applies rs i)
;;

(* rules are applied correctly *)
let apply_rule dp_fs fs dp_rs rs = get >>= fun c ->
 let apply_rule_i ?(strict = []) fs rs r = 
  gen_and (fun i -> apply_rule_i_lr ~strict:strict dp_fs fs rs i r) c.rows
 in
 let all_rs = dp_rs @ rs in
 (*BUG*)
 (*let all_fs = dp_fs @ fs in*)
 let all_fs = fs in
 map_and (apply_rule_i all_fs all_rs) rs $&$
 map_and (apply_rule_i ~strict:dp_rs all_fs all_rs) dp_rs
;;
 
(* check if a loop is found *)
let loop_i dp_fs fs i = get >>= fun c ->
 (get_e i $>=$ get_e 0) $&$ gen_and (loop_ij dp_fs fs i) c.cols
;;

let loop dp_fs fs = get >>= fun c -> gen_or (loop_i dp_fs fs) c.rows;;

(* combine all constraints *)
let encode s w = get >>= fun c -> 
 if not (Trs.is_srs s) || not (Trs.is_srs w) then return Logic.bot else (
 let dp_fs = (Trs.roots s) in
 let fs = List.diff (Trs.funs (Trs.union s w)) dp_fs in
 let dp_rs = Trs.to_list s and rs = Trs.to_list w in
 let mb = of_nat c.cols in

 gen_and (fun i -> (return mb) $>$ (get_e i)) c.rows $&$
 eo dp_fs fs $&$
 apply_rule dp_fs fs dp_rs rs $&$
 loop dp_fs fs >>= fun phi ->
 get_vat phi >>= fun tmp -> 
 vm tmp >>= fun var -> (* order does matter *)
 return (phi <&> var)
);;

(* reconstruct loop *)
(* get looping reduction from [assign] *)
let get_f fs ass i j =
 List.fold_left
  (fun x a -> eval_p (vct a i j) ass >>= fun b -> 
   if b then (x >>= fun x -> return (a::x)) else x)
  (return [])
  fs
;;

(* sanity check *)
let get_f fs ass i j =
 get_f fs ass i j >>= fun fs ->
 assert (List.length fs = 1);
 return (List.hd fs);;

let get_i fs ass i = get >>= fun c ->
 eval_a (get_e i) ass >>= fun e -> 
 let e = int_of_string (Number.to_string e) in
 sequence (List.gen (get_f fs ass i) c.cols) >>= fun ts -> 
 let ts = List.take (e + 1) ts in
 return (List.fold_right (fun f t -> Term.Fun (f, [t]) ) ts c.x)
;;

(* get terms [\[t_1,...,t_n,C_\[t_1\sigma\]\]] *)
let get_terms fs ass = get >>= fun c ->
 (* chop tail which is not necessary for looping reduction*)
 let rec chop_tail s acc = function 
  | [] -> acc
  | x :: xs ->
   if is_prefix s x then x :: acc else chop_tail s (x :: acc) xs
 in
 sequence (List.gen (get_i fs ass) c.rows) >>= fun ts ->
 let fst = List.hd ts in
 let res = List.rev (chop_tail fst [fst] (List.tl ts)) in
 return res
;;

let decode ass s w =
 let trs = Trs.union s w in
 get_terms (Trs.funs trs) ass >>= fun ts ->
 return (Loop.of_term_list trs ts)
;;

let solve state fs p = 
 let configurate s = F.printf "%s@\n%!" s; flags.help := true in
 (try init (); Arg.parsex code spec fs with Arg.Bad s -> configurate s);
 if !(flags.help) then (Arg.usage spec ("Options for "^code^":"); exit 0);
 if Problem.is_dp p then (
  let arith = Logic.nat !(flags.col) in
  let solver = if !(flags.sat) then Logic.MiniSat else Logic.Yices in
  let (s,w) = Problem.get_sw p in
  let c = context state arith max_int solver (Trs.union s w) in
  if (not c.dp) then failwith "loop: flag -dp requried";
  Logic.run ~obits:c.obits (
   Made.run c (
   encode s w >>= fun phi ->
   Made.liftm (Logic.solve ~solver:c.solver phi) >>= function
    | None -> return None
    | Some ass -> decode ass s w >>= fun loop ->
     return (Some (!(flags.transform),p,loop)))))
 else None
;;

(* wrap into state monad *)
module M = Rewritingx.Monad;;
let (>>=) = M.(>>=);;

let solve flags p = M.get >>= fun state -> 
 let (s,w) = Problem.get_sw p in
 match solve state flags p with
  | None -> M.return None
  | Some (t,p,((ts,c,sub) as loop)) ->
    if t then (
     M.map Rewritingx.Term.unlabel_dp ts >>= fun ts ->
     M.map (fun r -> let l,r = Rule.to_terms r in
     Rewritingx.Term.unlabel_dp l >>= fun l ->
     Rewritingx.Term.unlabel_dp r >>= fun r ->
     M.return (Rule.of_terms l r)) (Trs.to_list s) >>= fun s ->
     let s = Trs.of_list s in 
     let loop = Loop.transform (Problem.set_sw s w p) (ts,c,sub) in
     M.return (Some (p,loop))
    ) else M.return (Some (p,loop))
;;

(* Destructors *)
let make = Pair.make;;
let get_loop = snd;;
let get_ip = fst;;

(* Compare Functions *)
let equal p q = Problem.equal (get_ip p) (get_ip q);;

(* Printers *)
let fprintf fs fmt p =
 F.fprintf fmt "@[<1>%s:@\n" name; Loop.fprintf fmt (get_loop p) >>= fun _ ->
 F.fprintf fmt "@\n"; List.hd fs fmt >>= fun _ -> M.return (F.fprintf fmt "@]")
;;

let fprintfx fs fmt p =
 Loop.fprintfx fmt (get_loop p) (get_ip p) >>= fun _ -> List.hd fs fmt
;;
