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

(*** MODULES (part 1) *********************************************************)
module C   = Complexity;;
module F   = Format;;
module Fun = Function;;
module M   = Monad;;
module P   = Problem;;
module Var = Variable;;
module S   = Signature;;
module TVar = struct
  type t = int;;
  let compare = compare;;
  let fprintf fmt = F.fprintf fmt "%i";;
end

type typ = V of TVar.t (* a type variable *)
         | F of (typ list * typ) (* argument types -> result type *)
;;
type term = Term.t = Var of Var.t | Fun of Fun.t * term list;;

module Type = struct
  type t = typ;;
  let compare = compare;;
  let rec fprintf fmt = function
    | V x     -> TVar.fprintf fmt x
    | F(ts,t) -> F.fprintf fmt "[%a] -> %a"
      (List.fprintf fprintf ",") ts
      fprintf t
  ;;
  let result = function F(_,t) -> t | _ -> failwith "Type.result";;
  let args   = function F(ts,_) -> ts | _ -> failwith "Type.args";;
  let vars t = 
    let rec vars = function
      | V x     -> [x]
      | F(ts,t) -> vars t @ List.flat_map vars ts
    in
    List.unique(vars t)
  ;;
  let of_fun env f = List.assoc (Right f) env;;
  let of_var env x = List.assoc (Left x) env;;
  let of_term env = function
    | Var x     -> of_var env x
    | Fun(f,[]) -> of_fun env f
    | Fun(f,_) -> result(of_fun env f)
  ;;
end
module Sub = struct
  include Replacement.Make(TVar)(Type);;
  let rec apply_type s = function
    | V x as y -> apply x y s
    | F(ts,t)  -> F(List.map (apply_type s) ts,apply_type s t)
  ;;
end

(*** TYPES ********************************************************************)
type env =  ((Var.t,Fun.t)Util.either * Type.t)list;;
type t = P.t * env * (P.t list);;

type flags = {
 help : bool ref;
};;

(*** GLOBALS ******************************************************************)
let code     = "ti";;
let name     = "Type Introduction";;
let comment  = "Applies type introduction"
let keywords = ["type";"persistency";"introduction";"modular"];;

let flags = {
  help = ref false;
};;

let spec =
 let spec = [
  ("--help",Arg.Set flags.help,"Prints information about flags.");
  ("-help",Arg.Set flags.help,"Prints information about flags.");
  ("-h",Arg.Set flags.help,"Prints information about flags.");
 ] in
 Arg.alignx 80 spec
;;

let help = (comment,keywords,List.map Triple.drop_snd spec);;

(*** FUNCTIONS ****************************************************************)
let (>>=) = Monad.(>>=);;

let init _ = 
  flags.help := false;
;;

(* Destructors *)
let get_ip(p,_,_) = p;;
let get_ops(_,_,ps) = ps;;
let get_env(_,t,_) = t;;

let rec trans (env:env) = function
  | (u,[])                 -> u
  | (u,(Var x,t)::xs)      -> trans env (((Type.of_var env x,t)::u),xs)
  | (u,(Fun(f,[]),t0)::xs) ->
    trans env (((t0,Type.of_fun env f)::u),xs)
  | (u,(Fun(f,ts),t0)::xs) ->
    let t1 = Type.of_fun env f in
    trans env (((t0,Type.result t1)::u),(List.combine ts (Type.args t1) @ xs))
;;

let sub_pair s = Pair.map (Sub.apply_type s);;
let (<**>) s t = Sub.compose Sub.apply_type s t;;

let rec unify mgu = function
  | []                        -> mgu
  | (t1,t2)::xs when t1 = t2  -> unify mgu xs
  | (V x,t)::xs | (t,V x)::xs ->
    if List.mem x (Type.vars t) then failwith "Type.unify: occurs check"
                                else let mgu = mgu <**> Sub.singleton x t in
                                     unify mgu (List.map (sub_pair mgu) xs)
  | (F(ts1,t1),F(ts2,t2))::xs ->
    if List.length ts1 = List.length ts2
      then unify mgu ((t1,t2)::List.combine ts1 ts2 @ xs)
      else failwith "Type.unify: different number of arguments"
;;

let of_symbol (i,env) s = match s with
  | Left _  -> M.return(i+1,(s,V i)::env)
  | Right f ->
    M.find_ari f >>= fun a ->
    if a < 1 then M.return(i+1,(s,V i)::env) else (
      let j = i + a in
      M.return(j+1,(s,F(List.map (fun x -> V x) (List.range i j),V j))::env)
    )
;;

let of_symbols i = M.foldl of_symbol (i,[]);;

let root t = match Term.root t with Some f -> f
                                  | None   -> failwith "variable as lhs"
;;

let infer_types trs =
  of_symbols 0 (Trs.symbols trs) >>= fun(_,env) ->
  let eqs = Trs.fold (fun rule eqs ->
    let (l,r) = Rule.to_terms rule in match Type.of_fun env (root l) with
      | V _ as t | F(_,t) ->
        trans env ([],[(l,t)]) @ trans env ([],[(r,t)]) @ eqs
  ) [] trs
  in
  let mgu = unify Sub.empty eqs in
  let env = List.map (fun(p,t) -> (p,Sub.apply_type mgu t)) env in
  M.return env
;;

(* Compute for a given type [s], all types that arguments of
function symbols with result type [s] may inhabit. *)
let arg_types_of_type env fs s = List.unique(List.flat_map (fun f ->
  match Type.of_fun env f with
    | V _ -> [s]
    | F(ts,t) -> if s = t then s::ts else [s]
) fs);;

(* Compute for a list of types [ts], all types that subterms of
terms with a type in [ts] may inhabit. *)
let rec all_types (env:env) fs ts =
  let ts' = List.unique(List.flat_map (arg_types_of_type env fs) ts) in
  if List.length ts = List.length ts'
    then ts
    else all_types env fs ts'
;;

let split_trs (env:env) trs =
  let fs    = Trs.funs trs in
  let symbs = Trs.symbols trs in
  let types = List.unique(List.flat_map (fun s -> Type.vars(List.assoc s env)) symbs) in
  let c s t = if Trs.is_subset s t && Trs.is_subset t s then 0 else -1 in
  List.filter (not <.> Trs.is_empty) (List.unique ~c:c(List.map (fun typ ->
    let ts = all_types env fs [V typ] in
    Trs.filter (fun r -> List.mem (Type.of_term env (Rule.lhs r)) ts) trs
  ) types))
;;

let solve fs p = 
  let configurate s = F.printf "%s@\n%!" s; flags.help := true in
  (try init(); Arg.parsex code spec fs with Arg.Bad s -> configurate s);
  if !(flags.help) then (Arg.usage spec ("Options for "^code^":"); exit 0);
  if P.is_sp p then (
    let trs = P.get_trs p in
    if Trs.is_collapsing trs && Trs.is_duplicating trs then (
      M.return None
    ) else (
      Trs.rename trs  >>= fun trs ->
      infer_types trs >>= fun env ->
      let trss = split_trs env trs in
      if List.mem trs trss then M.return None else
        let s = P.get_strategy p and l = P.get_language p in
        M.return (Some(p,env,List.map (P.make_sp l s) trss))
    )
  ) else failwith(
    F.sprintf "%s: no type introduction for DP framework or relative \
               termination" name
  )
;;

(* Complexity Bounds *)
let complexity c _ = C.mul c C.constant;;

(* Compare Functions *)
let equal p q = P.equal (get_ip p) (get_ip q);;

(* Printers *)
let fprintf_env fmt = M.iter (function
  | (Left _,_)  -> M.return()
  | (Right f,t) ->
    M.get >>= fun s ->
    let s = S.fprintf_fun fmt f s in
    F.fprintf fmt " : %a@\n" Type.fprintf t;
    M.set s
);;

let fprintf fs fmt p =
  let fprintfi i fmt p =
   P.fprintfm fmt p >>= fun _ -> F.fprintf fmt "@\n"; List.nth fs i fmt
  in
  F.fprintf fmt "@[<1>%s:@\n" name;
  F.fprintf fmt "@\n@[<1>Type Environment:@\n";
  fprintf_env fmt (get_env p) >>= fun _ ->
  F.fprintf fmt "@\n@]";
  M.fprintfi fprintfi "@\n@\n" fmt (get_ops p) >>= fun _ ->
  M.return(F.fprintf fmt "@]")
;;

let fprintfx fs fmt p = failwith "No XML output for type introduction";;
