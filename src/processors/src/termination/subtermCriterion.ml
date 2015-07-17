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
module Co = Complexity;;
module F = Format;;
module Fun = Function;;
module H = Hashtbl;;
module RewriteM = Rewritingx.Monad;;
module P = Problem;;
module Sig = Signature;;
module SP = Projection;;

(*** TYPES ********************************************************************)
type context = {
  solver : Logic.solver;
  af_p   : (Fun.t * int,Logic.p) H.t;
  state  : Sig.t;
};;

type flags = {
  help : bool ref;
  sat : bool ref;
};;

type t = {
  sp : SP.t;
  input : P.t;
  output : P.t;
};;

(*** GLOBALS ******************************************************************)
let code = "sc";;
let name = "Subterm Criterion Processor";;
let comment = "Applies the subterm criterion processor.";;
let keywords = ["simple projection";"subterm criterion";"termination"];;

let flags = {
  help = ref false;
  sat = ref false;
};;

let spec =
  let help = "Prints information about flags." in
  let spec = [
    ("--help", Arg.Set flags.help, help);
    ("-help", Arg.Set flags.help, help);
    ("-h", Arg.Set flags.help, help);
    ("-sat", Arg.Set flags.sat, "Uses SAT backend.");
    ("-smt", Arg.Clear flags.sat, "Uses SMT backend (default).");
  ] in
  Arg.alignx 80 spec
;;

let help = (comment,keywords,List.map Triple.drop_snd spec);;

(*** MODULES (part 2) *********************************************************)
module Statex = struct type t = context end;;
module StateM = Util.Monad.Transformer.State (Statex) (Logic.Monad);;
open StateM;;

(*** FUNCTIONS ****************************************************************)
let init _ =
  flags.help := false;
  flags.sat := false;
;;

(* Constructors and Destructors *)
let make sp input output = {
  sp = sp;
  input = input;
  output = output;
};;

let get_ip p = p.input;;
let get_op p = p.output;;

(* Complexity Bounds *)
let complexity c _ = Co.mul c Co.other;;

(* Compare Functions *)
let equal p q = P.equal p.input q.input && P.equal p.output q.output;;

(* Printers *)
let (>>=) = RewriteM.(>>=);;

let fprintf fs fmt p = 
  F.fprintf fmt "@[<1>%s:@\n@[<1>simple projection:@\n" name;
  SP.fprintfm fmt p.sp >>= fun _ ->
  F.fprintf fmt "@]@\n@[<1>problem:@\n";
  P.fprintfm fmt p.output >>= fun _ ->
  F.fprintf fmt "@]@\n"; List.hd fs fmt >>= fun _ ->
  Monad.return (F.fprintf fmt "@]")
;;

let fprintfx fs fmt p =
  let tag = "spscProc" in
  F.fprintf fmt "@{<%s>" tag;
  SP.fprintfx fmt p.sp >>= fun _ ->
  P.fprintfx fmt p.output >>= fun _ ->
  List.hd fs fmt >>= fun _ ->
  Monad.return(F.fprintf fmt "@}")
;;

(* general functions *)
let (>>=) = StateM.(>>=);;

(* functions lifted from Logic into StateM *)
let fresh_bool = get >>= fun s -> liftm Logic.fresh_bool;;
let eval_p p ass = p >>= fun p -> liftm (Logic.eval_p p ass);;

let ($->$) a b = lift2 (<->>) a b;;
let ($|$) a b = lift2 (<|>) a b;;
let ($&$) a b = lift2 (<&>) a b;;

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
let context state =
  let solver = if !(flags.sat) then Logic.MiniSat else Logic.Yices in
  {
    solver = solver;
    af_p   = H.create 512;
    state  = state
  }
;;

(* administrative functions *)
let cache_m tbl f k = 
  if H.mem tbl k
    then return (H.find tbl k)
    else (f k >>= fun v -> (H.add tbl k v; return v))
;;

(* subterm criterion encoding starts here *)
(* caching variables *)
let pos f i = get >>= fun c -> cache_m c.af_p (const fresh_bool) (f,i);;
let arity f = get >>= fun c -> return (Sig.find_ari f c.state);;

(* encoding *)
let encode enc rule = match Rule.to_terms rule with
  | (Term.Var _), _ (* falltrough *)
  | _, (Term.Var _) -> failwith "encode: shall not happen"
  | Term.Fun (f, ts), Term.Fun (g, ss) when f = g ->
    mapi_or (fun i (ti, si) -> enc ti si $&$ pos f i) (List.combine ts ss)
  | Term.Fun (f, ts), Term.Fun (g, ss) ->
    mapi_or (fun j sj -> 
      (mapi_or (fun i ti -> enc ti sj $&$ pos f i) ts) $&$ pos g j)
      ss
;;

let plog = function true -> Logic.top | false -> Logic.bot;;
let pst s t = return (plog (Term.is_proper_subterm t s));;
let st s t  = return (plog (Term.is_subterm t s));;

let gt_rule = encode pst;;
let ge_rule = encode st;; 

let encode_gt = map_or gt_rule;;
let encode_ge = map_and ge_rule;;

let collapsing f a i =
  let make j = if i = j then pos f i else (lift Logic.neg (pos f j)) in
  gen_and make a
;;

let sp_f f = arity f >>= fun a -> gen_or (collapsing f a) a;;

let sp fs = map_and sp_f fs;;

let encode s = get >>= fun c ->
  let fs = Trs.roots s in
  let s = Trs.to_list s in
  encode_gt s $&$ encode_ge s $&$ sp fs
;;

(* decode from assignment *)
let decode_rule ass rule =
  get >>= fun c ->
  lift not (eval_p (gt_rule rule) ass)
;;

let decode_trs ass trs = 
  StateM.filter (decode_rule ass) (Trs.to_list trs) >>= (return <.> Trs.of_list)
;;

let decode_sp_f ass f =
  arity f >>= fun a ->
  sequence (List.gen (fun i -> eval_p (pos f i) ass) a) >>= fun ps -> 
  List.mapi Pair.make ps
  |> List.filter snd
  |> List.map fst
  |> List.hd
  |> Pair.make f
  |> return
;;

let decode_sp ass s = get >>= fun c ->
  let fs = Trs.roots s in
  StateM.sequence (List.map (decode_sp_f ass) fs) >>= fun sp ->
  return (SP.of_list sp)
;;

let solve signature fs p = 
  let configurate s = F.printf "%s@\n%!" s; flags.help := true in
  (try init (); Arg.parsex code spec fs with Arg.Bad s -> configurate s);
  if !(flags.help) then (Arg.usage spec ("Options for "^code^":"); exit 0);
  if P.is_dp p then (
    let s = P.get_dps p in
    let c = context signature in
    Logic.run (
      StateM.run c (encode s >>= fun phi ->
      StateM.liftm (Logic.solve ~solver:c.solver phi) >>= function
        | None -> return None
        | Some ass ->
          decode_trs ass s >>= fun s' -> 
          decode_sp ass s >>= fun sp ->
          return (Some (make sp p (P.set_dps s' p)))))
  ) else None
;;

(* wrap into state monad *)
let (>>=) = RewriteM.(>>=);;
let solve fs p = RewriteM.get >>= fun s -> RewriteM.return (solve s fs p);;
