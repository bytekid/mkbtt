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
module C   = Complexity;;
module F   = Format;;
module Fun = Function;;
module M   = Monad;;
module Sig = Signature;;

module FunList = struct
  type t = Fun.t list;;
  let compare = compare;;
  let fprintf = List.fprintf Fun.fprintf ", ";;
end

module IntPair = struct
  type t = (int * int);;
  let compare = compare;;
  let fprintf fmt (m, n)= F.fprintf fmt "(%d, %d)" m n;;
end

module FunMap = Map.Make (Fun) (FunList);;
module AAMap  = Map.Make (Fun) (IntPair);;

(*** TYPES ********************************************************************)
type appsym_heuristic =
  | MostFrequent
  | Unique
;;

type aarity_heuristic =
  | Maximum (* maximal occurring applicative arity of TRS *)
  | Minimum (* minimal occurring applicative arity of TRS *)
  | MinLeft
;;

type flags = {
  aarity_heuristic : aarity_heuristic ref;
  appsym_heuristic : appsym_heuristic ref;
  help : bool ref;
  top : bool ref;
};;

type info = {
  appsym : Fun.t;
  symtab : FunMap.t;
};;

type t = {
  info : info;
  input : Problem.t;
  output : Problem.t;
  applicative_top : bool;
  eta_rules : Trs.t;
};;

(*** GLOBALS ******************************************************************)
let code = "uncurryx";;
let name = "Extended Uncurrying Processor";;
let keywords = ["uncurrying";"transformation"];;
let comment = "Implements uncurrying for applicative systems.";;
let flags = {
  appsym_heuristic = ref MostFrequent;
  aarity_heuristic = ref Maximum;
  help = ref false;
  top = ref false;
};;

let no_such_heur h = failwith ("'"^h^"': no such heuristic");;

let spec =
 let spec = [
  ("--help",Arg.Set flags.help,"Prints information about flags.");
  ("-help",Arg.Set flags.help,"Prints information about flags.");
  ("-h",Arg.Set flags.help,"Prints information about flags.");
  ("-top",Arg.Set flags.top,"Special version of uncurrying for DP symbols.");
  ("-appsym",Arg.String (function
    | "max" -> flags.appsym_heuristic := MostFrequent
    | "unique" -> flags.appsym_heuristic := Unique
    | h     -> no_such_heur h),
    "Heuristic to determine the application symbol (max, unique).");
  ("-aarity",Arg.String (function
    | "max" -> flags.aarity_heuristic := Maximum
    | "min" -> flags.aarity_heuristic := Minimum
    | "minlhs" -> flags.aarity_heuristic := MinLeft
    | h     -> no_such_heur h),
    "Heuristic to determine the applicative arity (max, min, minlhs)."); 
  ] in
 Arg.alignx 80 spec
;;

let help = (comment,keywords,List.map Triple.drop_snd spec);;

(*** FUNCTIONS ****************************************************************)
let (>>=) = M.(>>=);;
let (>>) = M.(>>);;

let init _ =
  flags.appsym_heuristic := MostFrequent;
  flags.aarity_heuristic := Maximum;
  flags.help := false;
  flags.top := false;
;;

(* Printers for Debugging *)
let fprintf_aamap fmt tab =
  AAMap.fold (fun f (min, max) m ->
    M.fprintf_fun fmt f >>= fun _ ->
    F.fprintf fmt " has arities from %d to %d@\n" min max;
    m
  ) tab (M.return ())
;;

(* Printers *)
let fprintf_entry fmt f k fs =
  M.fprintf_fun fmt f >>= fun _ ->
  F.fprintf fmt " ==>";
  M.iter (fun f ->
    F.fprintf fmt " ";
    M.fprintf_fun fmt f >>= fun _ ->
    F.fprintf fmt "/";
    M.find_ari f        >>= fun n ->
    M.return (F.fprintf fmt "%d" n)
  ) fs                  >>= fun _ ->
  M.return (F.fprintf fmt "@\n")
;;

let fprintf_symtab fmt tab =
  FunMap.fold (fun f fs m ->
    M.find_ari f             >>= fun k ->
    fprintf_entry fmt f k fs >>= fun _ ->
    m
  ) tab (M.return ())
;;

let fprintf_info fmt p =
  F.fprintf fmt "application symbol: ";
  M.fprintf_fun fmt p.info.appsym  >>= fun _ ->
  F.fprintf fmt "@\n@[<1>symbol table:@\n";
  fprintf_symtab fmt p.info.symtab >>= fun _ ->
  F.fprintf fmt "@]";
  F.fprintf fmt "@\n@[<1>eta-rules:@\n";
  Trs.fprintfm fmt p.eta_rules     >>= fun _ ->
  F.fprintf fmt "@]";
  M.return ()
;;

let fprintf fs fmt p =
  F.fprintf fmt "@[<1>%s:@\n" name;
  fprintf_info fmt p             >>= fun _ ->
  F.fprintf fmt "@\n@[<1>problem:@\n";
  Problem.fprintfm fmt p.output  >>= fun _ ->
  F.fprintf fmt "@]@\n";
  List.hd fs fmt                 >>= fun _ ->
  M.return (F.fprintf fmt "@]")
;;

(* Processor *)

(* unapply o (((t o s1) o ...) o sN) = (t, [s1; ...; sN]) *)
let rec unapply o = function
  | Term.Fun (f, [t1; t2]) when Fun.equal o f ->
    let (h, ts) = unapply o t1
    in (h, ts @ [t2])
  | t -> (t, [])
;;
(* apply o t [s1; ...; sN] = ((t o s1) o ...) o sN *)
let apply o = List.foldl (fun t s -> Term.Fun (o, [t; s]));;


(* unapply_top # #(t1, t2, ..., tN) = (t1, [t2; ...; tN]) *)
let unapply_top o = function
  | Term.Fun (f, t::ts) when Fun.equal o f -> (t, [ts])
  | t -> (t, [])
;;
(* apply_top # t [t1; ...; tN] = #(t, t1, ..., tN) *)
let apply_top o t ts = Term.Fun (o, t::ts);;

let update_used_aarities f tab aa =
  if AAMap.mem f tab then (
    let (aa_min, aa_max) = AAMap.find f tab in
    let tab' = AAMap.remove f tab in
    AAMap.add f (min aa_min aa, max aa_max aa) tab'
  ) else AAMap.add f (aa, aa) tab
;;

let rec used_aarities_term o tab t = match unapply o t with
  | (Term.Fun (f, ts), ss) ->
    let len  = List.length ss in
    let tab' = List.foldl (used_aarities_term o) tab (ts@ss) in
    update_used_aarities f tab' len
  | (_, ss) -> List.foldl (used_aarities_term o) tab ss
;;

let used_aarities_term_top o tab t = match unapply_top o t with
  | (Term.Fun (f, _), ss) -> 
    let len = List.length ss in
    update_used_aarities f tab len
  | _ -> tab
;;

let used_aarities_rule o tab = Rule.fold (flip (used_aarities_term o)) tab;;
let used_aarities_rule_top o tab = Rule.fold (flip (used_aarities_term_top o)) tab;;

let used_aarities_trs o = Trs.fold (flip (used_aarities_rule o));;
let used_aarities_trs_top o = Trs.fold (flip (used_aarities_rule_top o));;

let rec is_head_var_free o t = match unapply o t with
  | (Term.Var _, ts) -> ts = []
  | (Term.Fun (f, ts), ss) ->
    List.for_all (is_head_var_free o) ts &&
    List.for_all (is_head_var_free o) ss
;;

let is_head_var_free_top o t = match unapply_top o t with
  | (Term.Var _, ts) -> ts = []
  | (Term.Fun (f, _), _) -> true
;;

let is_left_head_var_free o = List.for_all (is_head_var_free o) <.> Trs.lhs;;
let is_left_head_var_free_top o = List.for_all (is_head_var_free_top o) <.> Trs.lhs;;

let get_symbol tab f = List.nth (FunMap.find f tab);;

let get_default_symbol tab f =
  if FunMap.mem f tab then get_symbol tab f 0
                      else f
;;

let aarity_fun tab f = List.length (FunMap.find f tab) - 1;;

let aarity_term_gen unapp o tab t = match unapp o t with
  | (Term.Fun (f, _), ss) ->
    Some (max 0 (aarity_fun tab f - List.length ss))
  |  _ -> None
;;
let aarity_term = aarity_term_gen unapply;;
let aarity_term_top = aarity_term_gen unapply_top;;

let rec uncurry_term o tab t = match unapply o t with
  | (Term.Var _ as x, ss) -> apply o x (List.map (uncurry_term o tab) ss)
  | (Term.Fun (f, ts), ss) ->
    let uts = List.map (uncurry_term o tab) ts in
    let uss = List.map (uncurry_term o tab) ss in
    let aa  = aarity_fun tab f in
    let k   = min (List.length ss) aa in
    let fk  = get_symbol tab f k in
    apply o (Term.Fun (fk, (uts @ List.take k uss))) (List.drop k uss)
;;

let uncurry_term_top o tab = function
  | Term.Var _ as x -> x
  | Term.Fun (f, (Term.Fun (g, ss))::ts)
    when Fun.equal f o && aarity_fun tab g > 0 ->
    Term.Fun (get_symbol tab g 1, ss @ ts)
  | Term.Fun (f, ts) ->
    Term.Fun (get_default_symbol tab f, ts)
;;

let uncurry_trs o tab = Trs.project (Rule.project (uncurry_term o tab));;
let uncurry_trs_top o tab = Trs.project (Rule.project (uncurry_term_top o tab));;

let fresh_var = M.fresh_var >>= (M.return <.> Term.make_var);;
let fresh_vars n = M.replicate n fresh_var;;

let eta_rules_rule_gen aarity o tab rule =
  let (l, r) = Rule.to_terms rule in
  let Some aa = aarity o tab l in
  let rec add l r aa rules =
    if aa <= 0 then rules else (
      M.find_ari o     >>= fun n  ->
      fresh_vars (n-1) >>= fun xs ->
      rules            >>= fun rs ->
      let l' = Term.Fun (o, l :: xs) in
      let r' = Term.Fun (o, r :: xs) in
      add l' r' (aa-1) (M.return (Rule.of_terms l' r' :: rs))
    )
  in add l r aa (M.return [])
;;
let eta_rules_trs o tab trs =
  M.lift Trs.of_list (M.flat_map
    (eta_rules_rule_gen aarity_term o tab) (Trs.to_list trs))
;;
let eta_rules_trs_top o tab trs =
  M.lift Trs.of_list (M.flat_map
    (eta_rules_rule_gen aarity_term_top o tab) (Trs.to_list trs))
;;

(*
let fun_k f i =
  M.find_ari f >>= fun n ->
  M.set_curry ~arity:(n+i) f i
;;
*)
let fun_k f i =
  M.find_fun_name f               >>= fun id   ->
  M.find_ari f                    >>= fun n    ->
  let name = id ^ "_" ^ (string_of_int i) in
  M.create_fun (n+i) name         >>= fun fk   ->
  M.is_dp f                       >>= function
    | true  -> M.set_dp fk
    | false -> M.return fk
;;

let fun_ks f n = M.replicatei (n+1) (fun_k f);;

(*
let fun_k_top o f i = if i = 0 then (
  fun_k f 0
) else (
  M.find_ari f                    >>= fun n  ->
  M.find_ari o                    >>= fun k  ->
  M.set_curry ~arity:(n+k-1) f i  >>= fun fk ->
  M.set_dp fk
);;
*)
let fun_k_top o f i = if i = 0 then (
  fun_k f 0
) else (
  M.find_fun_name f               >>= fun id   ->
  M.find_fun_name o               >>= fun ap   ->
  M.find_ari f                    >>= fun n    ->
  M.find_ari o                    >>= fun k    ->
  let name = id ^ "_" ^ ap ^ "_" ^ (string_of_int i) in
  M.create_fun (n+k-1) name       >>= fun fk   ->
  M.set_dp fk
);;

let fun_ks_top o f n = M.replicatei (n+1) (fun_k_top o f);;

let uncurry_rules_fun o tab f =
  M.find_fun_name f >>= fun id ->
  let aa = aarity_fun tab f in
  M.find_ari f        >>= fun n  ->
  fresh_vars (n+aa+1) >>= fun xs ->
  let rec add k rules =
    if k >= aa then rules else (
      rules         >>= fun rs  ->
      fun_k f k     >>= fun fk  ->
      fun_k f (k+1) >>= fun fk1 ->
      let (xs, y::_) = List.split_at (n+k) xs in
      let l = apply o (Term.Fun (fk, xs)) [y] in
      let r = Term.Fun (fk1, xs @ [y]) in
      add (k+1) (M.return (Rule.of_terms l r :: rs))
    )
  in
  add 0 (M.return [])
;;

let uncurrying_rules o tab fs =
  M.lift Trs.of_list (M.flat_map (uncurry_rules_fun o tab) fs);;

let uncurry_rules_fun_top o tab =
  FunMap.fold (fun f fs m -> match fs with
    | [_; f_sharp] ->
      m                  >>= fun rs ->
      M.find_ari f       >>= fun n  ->
      M.find_ari o       >>= fun k  ->
      fresh_vars (n+k-1) >>= fun xs ->
      let (ys, zs) = List.split_at n xs in
      let l = apply_top o (Term.Fun (f, ys)) zs in
      let r = Term.Fun (f_sharp, xs) in
      M.return (Rule.of_terms l r :: rs)
    | _ -> m
  ) tab (M.return []) 
;;

let uncurrying_rules_top o tab = M.lift Trs.of_list (uncurry_rules_fun_top o tab);;

(* heuristics for finding the application symbol *)
let weighted_funas fs trs =
  M.map (fun f -> M.find_ari f >>= fun n -> M.return (f, n)) fs   >>= fun fs1 ->
  M.map (fun fn -> M.return (fn, Trs.count_fun (fst fn) trs)) fs1 >>= fun fs2 ->
  let sorted = List.sort (fun (_,v) (_,w) -> compare (-v) (-w)) fs2 in
  M.return (List.map fst fs2)
;;

let find_appsym_with_arity p get fs trs =
  weighted_funas fs trs >>= (M.return <.> (get <.> List.filter (p <.> snd)))
;;

let get_first = function (f,_)::_ -> Some f | _ -> None;;
let get_unique = function [(f,_)] -> Some f | _ -> None;;
let find_most_frequent_binary_appsym = find_appsym_with_arity ((=) 2) get_first;;
let find_most_frequent_appsym = find_appsym_with_arity ((<) 0) get_first;;
let find_unique_binary_appsym = find_appsym_with_arity ((=) 2) get_unique;;
let find_unique_appsym = find_appsym_with_arity ((<) 0) get_unique;;

(* heuristics for computing the symbol table *)
let symtab_for_maximal_occurring_aarity_gen used_aas funs tab o trs =
  let uas = used_aas o tab trs in
  AAMap.fold (fun f (_, u) m ->
    m        >>= fun tab ->
    funs f u >>= fun fks ->
    M.return (FunMap.add f fks tab)
  ) uas (M.return FunMap.empty)
;;
let symtab_for_maximal_occurring_aarity =
  symtab_for_maximal_occurring_aarity_gen used_aarities_trs fun_ks AAMap.empty
;;
let symtab_for_maximal_occurring_top_aarity o trs =
  let fs = List.remove o (Trs.funs trs) in
  (* make sure that all function symbols are part of the map *)
  let tab = List.foldl (fun tab f -> AAMap.add f (0, 0) tab) AAMap.empty fs in
  symtab_for_maximal_occurring_aarity_gen
    used_aarities_trs_top (fun_ks_top o) tab o trs
;;

let symtab_for_minimal_occurring_aarity_gen used_aas funs tab o trs =
  let uas = used_aas o tab trs in
  AAMap.fold (fun f (l, _) m ->
    m        >>= fun tab ->
    funs f l >>= fun fks ->
    M.return (FunMap.add f fks tab)
  ) uas (M.return FunMap.empty)
;;
let symtab_for_minimal_occurring_aarity =
  symtab_for_minimal_occurring_aarity_gen used_aarities_trs fun_ks AAMap.empty
;;
let symtab_for_minimal_occurring_top_aarity o trs =
  let fs = List.remove o (Trs.funs trs) in
  let tab = List.foldl (fun tab f -> AAMap.add f (0, 0) tab) AAMap.empty fs in
  symtab_for_minimal_occurring_aarity_gen
    used_aarities_trs_top (fun_ks_top o) tab o trs
;;

let symtab_for_minimal_lhs_aarity_gen aarity maximal o trs =
  maximal o trs >>= fun tab ->
  let tab = Trs.fold (fun rule tab ->
    let l       = Rule.lhs rule in
    let Some f  = Term.root l in
    let Some aa = aarity o tab l in
    if FunMap.mem f tab && aa > 0 then (
      let fs = FunMap.find f tab in
      let tab' = FunMap.remove f tab in
      FunMap.add f (List.take (List.length fs - aa) fs) tab'
    ) else tab
  ) tab trs
  in
  M.return tab
;;
let symtab_for_minimal_lhs_aarity =
  symtab_for_minimal_lhs_aarity_gen
    aarity_term symtab_for_maximal_occurring_aarity;;
let symtab_for_minimal_lhs_top_aarity =
  symtab_for_minimal_lhs_aarity_gen
    aarity_term_top symtab_for_maximal_occurring_top_aarity;;

let rec apply_symtab_term o tab = function
  | Term.Var _ as x -> x
  | Term.Fun (f, ts) ->
    let ts' = List.map (apply_symtab_term o tab) ts in
    if FunMap.mem f tab
      then Term.Fun (get_symbol tab f 0, ts')
      else Term.Fun (f, ts')
;;

let apply_symtab_trs o tab =
  Trs.project (Rule.project (apply_symtab_term o tab));;

let apply_symtab p = 
  let o   = p.info.appsym in
  let tab = p.info.symtab in
  let (dps, trs) = Problem.get_sw p.output in
  let dps' = apply_symtab_trs o tab dps in
  let trs' = apply_symtab_trs o tab trs in
  let output' = Problem.set_sw dps' trs' p.output in { p with
    (*reset graph, since all function symbols changed*)
    output = if Problem.is_dp output'
      then Problem.set_dg Problem.Complete output'
      else output';
  }
;;

let uncurry_dp_top find_o mk_symtab p =
  let (dps,trs) = Problem.get_sw p in
  let sharps    = Trs.roots dps in
  let both      = Trs.union dps trs in
  find_o sharps dps >>= function None -> M.return None | Some o -> (
    if not (is_left_head_var_free_top o dps) then M.return None else (
      mk_symtab o both >>= fun tab ->
      M.exists (M.lift Term.is_var <.>
        flip Trs.etcap trs <.> fst <.> unapply_top o <.> Rule.rhs)
        (Trs.to_list dps) >>= (function
        | false -> M.return (uncurry_trs_top o tab dps, trs, Trs.empty)
        | true  ->
          uncurrying_rules_top o tab  >>= fun us  ->
          eta_rules_trs_top o tab trs >>= fun eta ->
          let uncurried_eta = uncurry_trs_top o tab eta in
          let uncurried_dps = uncurry_trs_top o tab dps in
          let dps' = Trs.union us (Trs.union uncurried_eta uncurried_dps) in
          M.return (dps', trs, eta)
      ) >>= fun (dps', trs', eta) ->
      let p'   = Problem.set_sw dps' trs' p in
      let info = { appsym = o; symtab = tab } in
      M.return (Some (info, p', eta))
    )
  )
;;

let uncurry_dp find_o mk_symtab p =
  let (dps,trs) = Problem.get_sw p in
  let funs      = List.union (Trs.funs trs) (Trs.funs dps) in
  let nonsharps = List.diff funs (Trs.roots dps) in
  let both      = Trs.union dps trs in
  find_o nonsharps both >>= function None -> M.return None | Some o -> (
    if not (is_left_head_var_free o both) then M.return None else (
      let fs' = List.remove o funs in
      mk_symtab o both           >>= fun tab ->
      eta_rules_trs o tab trs    >>= fun eta ->
      uncurrying_rules o tab fs' >>= fun us ->
      let dps' = uncurry_trs o tab dps in
      let trs' = Trs.union (uncurry_trs o tab (Trs.union trs eta)) us in
      let p'   = Problem.set_sw dps' trs' p in
      let info = { appsym = o; symtab = tab } in
      M.return (Some (info, p', eta))
    )
  )
;;

let uncurry_sp find_o mk_symtab p = 
  let trs = Problem.get_trs p in
  let fs  = Trs.funs trs in
  find_o fs trs >>= function None -> M.return None | Some o -> (
    if not (is_left_head_var_free o trs) then M.return None else (
      let fs' = List.remove o fs in
      mk_symtab o trs            >>= fun tab ->
      eta_rules_trs o tab trs    >>= fun eta ->
      uncurrying_rules o tab fs' >>= fun us  ->
      let trs' = Trs.union (uncurry_trs o tab (Trs.union trs eta)) us in
      let p'   = Problem.set_trs trs' p in
      let info = { appsym = o; symtab = tab } in
      M.return (Some (info, p', eta))
    )
  )
;;

let solve_aux find_o mk_symtab top p =
  if Problem.is_sp p then uncurry_sp find_o mk_symtab p
  else if top && Problem.is_dp p then uncurry_dp_top find_o mk_symtab p
  else if Problem.is_dp p then uncurry_dp find_o mk_symtab p 
  else M.return None
;;

let solve fs p = 
  let configurate s = F.printf "%s@\n%!" s; flags.help := true in
  (try init (); Arg.parsex code spec fs with Arg.Bad s -> configurate s);
  if !(flags.help) then (Arg.usage spec ("Options for "^code^":"); exit 0);
  let top = !(flags.top) in
  let find_o = match !(flags.appsym_heuristic) with
    | MostFrequent -> if top
      then find_most_frequent_appsym
      else find_most_frequent_binary_appsym
    | Unique  -> if top
      then find_unique_appsym
      else find_unique_binary_appsym
  in
  let mk_symtab = match !(flags.aarity_heuristic) with
    | Maximum -> if top
      then symtab_for_maximal_occurring_top_aarity
      else symtab_for_maximal_occurring_aarity
    | Minimum -> if top
      then symtab_for_minimal_occurring_top_aarity
      else symtab_for_minimal_occurring_aarity
    | MinLeft -> if top
      then symtab_for_minimal_lhs_top_aarity
      else symtab_for_minimal_lhs_aarity
  in
  solve_aux find_o mk_symtab top p >>= function
    | None -> M.return None
    | Some (info, p', eta) ->
      let p = {
        info = info;
        applicative_top = top;
        input = p;
        output = p';
        eta_rules = eta;
      } in M.return (Some (apply_symtab p))
;;


(* Destructors *)
let get_ip p = p.input;;
let get_op p = p.output;;

(* Complexity Bounds *)
let complexity c _ = C.mul c C.constant;;

(* Compare Functions *)
let equal p q =
 Problem.equal (get_ip p) (get_ip q) && Problem.equal (get_op p) (get_op q)
;;

(* XML Printing *)
let fprintfx_entry fmt f k fs =
  F.fprintf fmt "@{<uncurriedSymbolEntry>";
  M.fprintfx_fun fmt f  >>= fun _ ->
  F.fprintf fmt "@{<arity>%d@}" k;
  M.iter (fun f ->
    M.fprintfx_fun fmt f    
  ) fs                  >>= fun _ ->
  M.return (F.fprintf fmt "@}")
;;

let fprintfx_symtab fmt tab =
  FunMap.fold (fun f fs m ->
    M.find_ari f              >>= fun k ->
    fprintfx_entry fmt f k fs >>= fun _ ->
    m
  ) tab (M.return ())
;;

let fprintfx_info fmt p =
  F.fprintf fmt "@{<uncurryInformation>";
  M.fprintfx_fun fmt p.info.appsym  >>= fun _ ->
  F.fprintf fmt "@{<uncurriedSymbols>";
  fprintfx_symtab fmt p.info.symtab >>= fun _ ->
  F.fprintf fmt "@}";
  F.fprintf fmt "@{<etaRules>";
  Trs.fprintfx fmt p.eta_rules      >>= fun _ ->
  F.fprintf fmt "@}";
  M.return (F.fprintf fmt "@}")
;;

let fprintfx_applicative_top fmt p =
  if p.applicative_top then (
    M.find_ari p.info.appsym >>= fun n ->
    F.fprintf fmt "@{<applicativeTop>%d@}" n;
    M.return ()
  ) else M.return ()
;;

let fprintfx fs fmt p =
  let tag = if Problem.is_sp (get_op p) then "uncurry" else "uncurryProc" in
  F.fprintf fmt "@{<%s>" tag;
  fprintfx_applicative_top fmt p  >>= fun _ ->
  fprintfx_info fmt p             >>= fun _ ->
  Problem.fprintfx fmt (get_op p) >>= fun _ ->
  List.hd fs fmt                  >>= fun _ ->
  M.return (F.fprintf fmt "@}")
;;
