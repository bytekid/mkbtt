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

(*** MODULES ******************************************************************)
module C = Complexity;;
module F = Format;;
module Fun = Function;;
module M = Monad;;
module Number = Logic.Number;;
module P = Problem;;
module Var = Variable;;

(*** TYPES ********************************************************************)
type flags = {
  a : bool ref;
  curry : bool ref;
  help : bool ref;
  mirror : bool ref;
  u1 : bool ref;
  u2 : bool ref;
 };;
type head = Var of Var.t | Fun of Fun.t;;
type t = P.t * P.t;;

(*** GLOBALS ******************************************************************)
let code = "uncurry";;
let name = "Uncurry Processor";;
let keywords = ["uncurrying";"transformation"];;
let comment = "Implements uncurrying for applicative systems.";;
let flags = {
  a = ref false;
  curry = ref false;
  help = ref false;
  mirror = ref false;
  u1 = ref false;
  u2 = ref false;
};;

let spec =
 let spec = [
  ("--help",Arg.Set flags.help,"Prints information about flags.");
  ("-help",Arg.Set flags.help,"Prints information about flags.");
  ("-h",Arg.Set flags.help,"Prints information about flags.");
  ("-a",Arg.Set flags.a,"Transformation A.");
  ("-curry",Arg.Set flags.curry,"Curry the system.");
  ("-mirror",Arg.Set flags.mirror,"Mirrors the TRS before it is uncurried.");
  ("-u1",Arg.Set flags.u1,"processor U1.");
  ("-u2",Arg.Set flags.u2,"processor U2.");
  ] in
 Arg.alignx 80 spec
;;

let help = (comment,keywords,List.map Triple.drop_snd spec);;

(*** FUNCTIONS ****************************************************************)
let (>>=) = M.(>>=);;
let (>>) = M.(>>);;
let init _ = 
 flags.a := false;
 flags.curry := false;
 flags.help := false;
 flags.mirror := false;
 flags.u1 := false;
 flags.u2 := false;
;;

(* Processor *)
let max_ari trs =
 List.foldr (M.lift2 max <.> M.find_ari) (M.return ~-1) (Trs.funs trs)
;;

let constants trs = M.filter (M.is_ari 0) (Trs.funs trs);;
let binary trs = M.filter (M.is_ari 2) (Trs.funs trs);;
let sharps fs = M.filter M.is_dp fs;;
let not_sharps fs = sharps fs >>= fun ss -> M.return (List.diff fs ss);;

let fresh_var = M.fresh_var >>= (M.return <.> Term.make_var);;

let uncurry circ f a = 
 if a = 0 then M.return f else M.set_curry ~arity:a f a >>= fun f ->
 M.is_dp circ >>= fun b -> if b then M.set_dp f else M.return f
;;

let drop_curry f = 
 M.is_curry f >>= fun b -> 
 if b then M.drop_curry f else M.return f
;;

let curry circ f = 
 M.is_dp f >>= fun b -> 
 if b then M.set_dp circ else M.return circ
;;

(* one binary sharped symbol and at most one binary unsharped symbols*)
let is_dp_applicative p =
 let (s,w) = P.get_sw p in
 let sw = Trs.union s w in
 constants sw >>= fun cs ->
 binary sw >>= fun bs ->
 sharps bs >>= fun fss ->
 not_sharps bs >>= fun fsn ->
 M.return (List.equal (Trs.funs sw) (cs@bs) && List.length fss = 1 && List.length fsn <= 1)
;;

let app trs = 
 Trs.is_applicative trs >>= fun b ->
 if b then () else failwith "TRS not applicative";
 M.lift List.hd (binary trs)
;;

let is_applicative_p p = 
 if P.is_sp p then Trs.is_applicative (P.get_trs p)
 else if P.is_dp p then is_dp_applicative p
 else failwith "is_applicative: unknown problem"
;;

let app_dp p = 
 if not (P.is_dp p) then failwith "app_dp:1";
 let (s,w) = P.get_sw p in
 is_dp_applicative p >>= fun b ->
 if not b then failwith "app_dp:2";
 binary (Trs.union s w)
;;


let rec head = function 
 | Term.Var x -> Var x
 | Term.Fun (f,[]) -> Fun f
 | Term.Fun (f,[s;_]) -> head s
 | _ -> failwith "term not applicative"
;;

let rec head_variable_free t = match head t with
 | Var _ -> false
 | Fun _ -> true
;;

let head_variable_free ts = 
 let subterms = Term.subterms ~p:(not <.> Term.is_var) in
 let st = List.flat_map subterms ts in
 List.for_all head_variable_free st
;;

let left_head_variable_free trs = head_variable_free (Trs.lhs trs);;

let head_variable_free trs = head_variable_free (Trs.lhs trs @ Trs.rhs trs);;

let head_variable_free_p p = 
 let (s,w) = P.get_sw p in
 head_variable_free s && head_variable_free w
;;

let pick_arguments bs t =
 let rec pick_arguments acc = function
  | Term.Var _ -> failwith "head variable"
  | Term.Fun (_,[]) -> acc
  | Term.Fun (f,[s;t]) when List.mem f bs -> pick_arguments (t :: acc) s 
  | _ -> failwith "term not applicative"
 in
 pick_arguments [] t
;;

let problem_map_sw f p = 
 let s,w = P.get_sw p in
 P.set_sw (f s) (f w) p
;;

(* here starts the dirty part *)

(* applicative arity *)
let rec aa_term circs = function
 | Term.Var _ -> 0
 | Term.Fun (_,[]) -> 0
 | Term.Fun (f,[t;_]) when List.mem f circs -> 1 + aa_term circs t 
 | Term.Fun (_,[t;_]) -> 0
 | _ -> failwith "aa_t: term not applicative"
;;

let aa_term_f bs f t = 
 let equal h f = match (h,f) with Fun f,g -> f = g | _ -> false in
 let ts = List.filter (fun t -> equal (head t) f) (Term.subterms t) in
 List.foldr max 0 (List.map (aa_term bs ) ts)
;;

let aa_rule bs f rule = 
 let (l,r) = Rule.map (aa_term_f bs f) rule in
 List.filter (fun aa -> aa >= 0) [l;r]
;;

(* applicative arity for [f] *)
let aa_f bs trs f = 
 (* not_sharps bs >>= fun circs -> *)
 let aa = List.flat_map (aa_rule bs f) (Trs.to_list trs) in
 M.return (List.foldr max 0 aa)
;;

(* applicative arity for [t] *)
let rec aa_t aa = function
 | Term.Var _ -> failwith "non left-head-variable-free term"
 | Term.Fun (f,[]) -> aa f  
 | Term.Fun (f,[t;_]) -> aa_t aa t >>= fun aa -> M.return (aa - 1)
 | _ -> failwith "term not in applicative form"
;;

(* proper systems *)
let rec proper_t circs trs t =
 match head t with
  | Var _ -> M.return true
  | Fun f ->
   aa_f circs trs f >>= fun aa ->
   let args = pick_arguments circs t in
   Monad.for_all (proper_t circs trs) args >>= fun p_args ->
   M.return (List.length args = aa && p_args)
;;

let proper circs trs = M.for_all (proper_t circs trs) (Trs.terms trs);;

(* uncurried system *)
let rec uncurried_term aa bs cs t = match t with
 | Term.Var _ -> M.return t
 | Term.Fun (_,[]) -> M.return t
 | Term.Fun (app,[u;v]) when List.mem app bs ->
  (match head t with 
   | Var _ ->
    M.lift2 Term.make_fun (M.return app)
     (M.sequence [uncurried_term aa bs cs u;uncurried_term aa bs cs v])
   | Fun f -> 
    aa f >>= fun aaf ->
    let args = pick_arguments bs t in
    let a = List.length args in 
    if a > aaf then
     M.lift2 Term.make_fun (M.return app)
      (M.sequence [uncurried_term aa bs cs u;uncurried_term aa bs cs v])
    else 
     M.lift2 Term.make_fun (uncurry app f a) (M.map (uncurried_term aa bs cs) args))
 | Term.Fun (f,[u;v]) ->
  M.lift2 Term.make_fun (M.return f) (M.map (uncurried_term aa bs cs) [u;v])
 (* | t -> failwith "term not applicative" *)
 | t -> M.return t
;;

let uncurried_rule aa bs cs rule =
 let (l,r) = Rule.to_terms rule in
 uncurried_term aa bs cs l >>= fun l -> uncurried_term aa bs cs r >>= fun r ->
 M.return (Rule.of_terms l r)
;;

let uncurried_system aa bs cs trs =
 M.lift Trs.of_list (M.sequence (Trs.map (uncurried_rule aa bs cs) trs))
;;

(* curried system *)
let rec curried_term circ = function
 | Term.Var x as v -> M.return v
 | Term.Fun (f,[]) -> drop_curry f >>= fun f -> M.return (Term.Fun(f,[]))
 | Term.Fun (f,ts) -> 
  drop_curry f >>= fun f ->
  List.foldl (fun acc t -> 
   acc >>= fun acc' ->
   curried_term circ t >>= fun t' ->
   M.return (Term.Fun (circ,[acc';t']))
  ) (M.return (Term.Fun (f,[]))) ts
;;

let sharp_term t =
 Term.unlabel_dp t >>= fun t -> (*to prevent two labels (head variables)*)
 Term.label_dp t
;;

let curried_term circ t = 
 curried_term circ t >>= fun t' ->
 match Term.root t with
  | None -> M.return t'
  | Some r -> M.is_dp r >>= fun b -> if b then sharp_term t' else M.return t'
;;
 
let curried_rule circ rule = 
 let (l,r) = Rule.to_terms rule in
 curried_term circ l >>= fun l ->
 curried_term circ r >>= fun r ->
 M.return (Rule.of_terms l r)
;;

let curried_system circ trs = 
 M.lift Trs.of_list (M.sequence (Trs.map (curried_rule circ) trs))
;;

(* eta saturation *)
let eta_sat_rule circs trs aa rule = 
 let (l,r) = Rule.to_terms rule in
 aa_t aa l >>= fun aa_t ->
 let rec add f l r aa_t acc =
  if aa_t <= 0 then acc
  else
   fresh_var >>= fun x -> acc >>= fun acc ->
   let l' = Term.Fun (f,[l;x]) and r' = Term.Fun (f,[r;x]) in
   add f l' r' (aa_t - 1) (M.return (Rule.of_terms l' r' :: acc))
 in
 M.flat_map (fun circ -> add circ l r aa_t (M.return [rule])) circs
;;

let eta_sat bs aa trs =
 not_sharps bs >>= fun circs ->
 M.lift Trs.of_list (M.flat_map (eta_sat_rule circs trs aa) (Trs.to_list trs))
;;

(* uncurrying system *)
let rec uncurrying_rules_f circ var_list aa f =
 if aa < 0 then
  M.return Trs.empty
 else (
  let vars = List.take (aa + 1) var_list in
  assert (List.length vars > 0);
  let y, var = List.hd vars, List.tl vars in
  let i = List.length var in
  uncurry circ f i >>= fun fi ->
  uncurry circ f (i+1) >>= fun fi' ->
  let l = Term.Fun (circ, [Term.Fun (fi, var); y]) in
  let r = Term.Fun (fi', var @ [y]) in
  (uncurrying_rules_f circ var_list (aa - 1)) f >>= fun aux ->
  M.return (Trs.add (Rule.of_terms l r) aux))
;;

let uncurrying_system circs var_list aa cs =
 List.foldr
  (fun f trs -> aa f >>= fun aaf ->
   M.lift2 Trs.union (uncurrying_rules_f circs var_list (aaf - 1) f) trs)
  (M.return Trs.empty) cs
;;

let uncurrying_system bs var_list aa cs = 
 not_sharps bs >>= fun circs -> match circs with
  | [] -> M.return Trs.empty
  | [circ] -> uncurrying_system circ var_list aa cs
;;

let gen_var aa fs =
 M.map aa fs >>= fun ma ->
 let max_ari = List.foldr Pervasives.max 0 ma in 
 (M.lift List.rev) (M.sequence (List.gen (const fresh_var) max_ari))
;;

(* putting everything together *)
let transform bs aa cs atrs = 
 gen_var aa cs >>= fun vars ->
 not_sharps bs >>= fun circs ->
 uncurrying_system circs vars aa cs >>= fun u ->
 if Trs.is_empty atrs then M.return (Some u) else
 Trs.is_applicative atrs >>= fun c ->
 if c then 
  if left_head_variable_free atrs then (
   eta_sat circs aa atrs >>= fun atrs_eta ->
   uncurried_system aa bs cs atrs_eta >>= fun trs ->
   (M.return (Option.some (Trs.union trs u)))
  ) else M.return None
 else M.return None
;;

let t_a circs atrs = 
 proper circs atrs >>= fun b ->
 if b then
  if Trs.is_empty atrs then M.return None else
   if left_head_variable_free atrs then (
     let aa = aa_f circs atrs in
     constants atrs >>= fun cs ->
     uncurried_system aa circs cs atrs >>= fun trs ->
     M.return (Some trs)
    ) else M.return None
 else
  M.return None

let transformation_a atrs = 
  if head_variable_free atrs then 
   app atrs >>= fun app ->
   t_a [app] atrs 
  else M.return None
;;

let rule_is_dp rule = M.is_dp (Option.the (Term.root (Rule.lhs rule)));;

let transformation_a_dp p =
 let s,w = P.get_sw p in
 if head_variable_free_p p then
  app_dp p >>= fun bs ->
  t_a bs (Trs.union s w) >>= fun sw' -> match sw' with
   | None -> M.return None
   | Some sw' ->
  let sw' = Trs.to_list sw' in
  M.filter (rule_is_dp) sw' >>= fun s' ->
  let w' = List.diff sw' s' in
  M.return (Some (P.set_sw (Trs.of_list s') (Trs.of_list w') p))
 else M.return None
;;

let u1 bs p = 
 let (s,w) = P.get_sw p in
  let sw = Trs.union s w in
  constants sw >>= fun cs ->
  let aa = aa_f bs sw in
  uncurried_system aa bs cs s >>= fun s' ->
  not_sharps bs >>= fun circs ->
  let aa = aa_f circs sw in
  transform circs aa cs w >>= fun w' ->
  match w' with 
   | None -> M.return None
   | Some w' -> M.return (Some (Problem.set_sw s' w' p))
;;

let transform_u1 p =
 app_dp p >>= fun bs ->
 not_sharps bs >>= fun circs ->
 if circs = [] then M.return None
 else u1 circs p
;;

let rs w = function
 | [] -> M.return true
 | t::_ -> Trs.tcap t w >>= (M.return <.> not <.> Term.is_var)
;;

let rs_rule w rule = rs w (Term.args (Rule.rhs rule));;

let strongly_root_stable w s = 
 M.sequence (List.map (rs_rule w) (Trs.to_list s)) >>= fun l ->
 M.return (List.for_all ((=) true) l)
;;

let sharp_rule rule =
 let (l,r) = Rule.to_terms rule in
 sharp_term l >>= fun l -> sharp_term r >>= fun r ->
 M.return (Rule.of_terms l r)
;;

let sharp_trs trs =
 M.sequence (List.map sharp_rule (Trs.to_list trs)) >>= fun trs ->
 M.return (Trs.of_list trs)
;;

let transform_u2 p = 
 let (s,w) = P.get_sw p in
  app_dp p >>= fun bs ->
  sharps bs >>= fun circs_s ->
  not_sharps bs >>= fun circs_w ->
  u1 circs_w p >>= fun p' ->
  (match p' with 
   | None -> M.return None
   | Some p' ->
   let (s',w') = P.get_sw p' in
   strongly_root_stable w' s' >>= fun b ->
   if b then
    u1 bs p >>= (fun p -> match p with
     | None -> M.return None
     | Some p -> let (s,w) = P.get_sw p in
      sharp_trs s >>= fun s ->
      M.return (Some (P.set_sw s w p))
     )
   else
    M.return (Some p')
   ) >>= fun p' -> match p' with
   | None -> M.return None
   | Some p' -> M.return (if P.equal p p' then None else Some p')
;;

let transform_uncurry p = 
 let trs = P.get_trs p in
 app trs >>= fun circ ->
 let aa = aa_f [circ] trs in
 constants trs >>= fun cs ->
 let f = if !(flags.a) then transformation_a else transform [circ] aa cs in
 f trs >>= fun trs' ->
 let p' = Option.map (flip P.set_trs p) trs' in
 M.return p'
;;

let transform_curry p =
 is_applicative_p p >>= fun c -> 
 if c then 
  M.return None
 else
  let (s,w) = P.get_sw p in
  M.fresh_fun >>= fun circ ->
  M.add_ari circ 2 >>= fun _ ->
  curried_system circ s >>= fun s ->
  curried_system circ w >>= fun w ->
  M.return (Some (P.set_sw s w p))
;;

let wrapper f p = is_applicative_p p >>= fun b ->
 if b then f p else M.return None
;;

let standard p = 
 if !(flags.curry) then transform_curry p
 else wrapper transform_uncurry p
;;

let dp p =
 let f = 
  if !(flags.a) then wrapper transformation_a_dp 
  else if !(flags.u1) then wrapper transform_u1
  else if !(flags.u2) then wrapper transform_u2
  else if !(flags.curry) then transform_curry
  else fun _ -> M.return None
 in
 f p
;;
 
let solve p =
 let p = (if !(flags.mirror) then problem_map_sw Trs.reflect else id) p in
 if P.is_sp p then standard p 
 else if P.is_dp p then dp p 
 else M.return None
;;

let solve fs p = 
 let configurate s = F.printf "%s@\n%!" s; flags.help := true in
 (try init (); Arg.parsex code spec fs with Arg.Bad s -> configurate s);
 if !(flags.help) then (Arg.usage spec ("Options for "^code^":"); exit 0);
 solve p >>= (M.return <.> Option.map (Pair.make p))
;;

(* Destructors *)
let get_ip = fst;;
let get_op = snd;;

(* Complexity Bounds *)
let complexity c _ = C.mul c C.constant;;

(* Compare Functions *)
let equal p q =
 P.equal (get_ip p) (get_ip q) && P.equal (get_op p) (get_op q)
;;

(* Printers *)
let fprintf fs fmt p =
 F.fprintf fmt "@[<1>%s:@\n" name; P.fprintfm fmt (get_op p) >>= fun _ ->
 F.fprintf fmt "@\n"; List.hd fs fmt >>= fun _ -> M.return (F.fprintf fmt "@]")
;;

let fprintfx fs fmt p =
 F.fprintf fmt "@{<uncurry>"; P.fprintfx fmt (get_op p) >>= fun _ ->
 List.hd fs fmt >>= fun _ -> M.return (F.fprintf fmt "@}")
;;
