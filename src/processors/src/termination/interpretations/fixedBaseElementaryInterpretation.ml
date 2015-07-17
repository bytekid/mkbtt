(* Copyright 2013 Martin Korp, Christian Sternagel, Harald Zankl
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
module Co = Complexity;;
module F = Format;;
module Fun = Function;;
module Var = Variable;;
module Number = Logic.Number;;
module Monad = Rewritingx.Monad;;
module Pos = Position;;
module Sig = Signature;;
module E = FixedBaseElementary;;

(*** TYPES ********************************************************************)

type t = {
 interpretation : (string * E.t) list;
 input : Problem.t;
 output :Problem.t;
 t_flags : string;
 name : string;
 code : string;
};;

type flags = {
 db : int ref;
 dir : bool ref;
 help : bool ref;
 eval : bool ref;
 id : int ref;
 min : int ref;
 (* ob : int ref; *)
 od : int ref;
 p : bool ref;
 p2 : bool ref;
 pr : bool ref;
 pr2 : bool ref;
 time : bool ref; 
 test : bool ref;
};;


(*** GLOBALS ******************************************************************)
let code = "fbi";;
let name = "Fixed Base elementary Interpretation Processor";;
let comment = "Applies fixed based elementary interpretations."
let keywords = ["elementary interpretation";"termination"];;

let flags = {
 db = ref max_int;
 dir = ref false;
 help = ref false;
 eval = ref false;
 id = ref 3;
 min = ref 1;
 (* ob = ref max_int; *)
 od = ref 5;
 p = ref false;
 p2 = ref false;
 time = ref false; 
 pr = ref false; (*debugging*)
 pr2 = ref false; (*debugging*)
 test = ref false; (*debugging*)
};;

let spec =
 let spec = [
  ("-direct",Arg.Set flags.dir,"Try to finish termination proof.");
  ("--help",Arg.Set flags.help,"Prints information about flags.");
  ("-help",Arg.Set flags.help,"Prints information about flags.");
  ("-h",Arg.Set flags.help,"Prints information about flags.");
  ("-base",Arg.Set_int E.t_base , F.sprintf "Set least element in carrier (default %d)." !E.t_base);
  ("-cache",Arg.Set E.cache, "Switch on caching (default: off).");
  ("-carrier",Arg.Set_int E.t_carrier, F.sprintf "Set least element in carrier (default %d)." !E.t_carrier);
  ("-desired",Arg.Set_string E.t_desired, "Add some desired constraints (default: no contraints)).");
  ("-ib",Arg.Int ((:=) flags.min <.> Int.bit_max),
   "Defines the number of bits used to represent matrix entries (same as \
   `-min' but in bits).");
  ("-id",Arg.Set_int flags.id,F.sprintf "Input degree of ordinal (default %d)." !(flags.id));
  ("-max",Arg.Int ((:=) E.t_ob <.> Int.bits),
   "Defines the maximum number that can appear as intermediate result.");
  ("-min",Arg.Set_int flags.min,
   "Defines the minimum matrix entry that should be representable.");
  ("-ob",Arg.Set_int E.t_ob,
   "Defines the number of bits used to represent intermediate results \
    (same as `-max' but in bits)");
  ("-od",Arg.Set_int flags.od, F.sprintf "Output degree of ordinal (default %d)." !(flags.od));
  ("-p",Arg.Set flags.p,
   "Print encoding in SMT-LIB format v1.2 and fail");
  ("-p2",Arg.Set flags.p2,
   "Print encoding in SMT-LIB format v2.0 and fail");
  ("-pc",Arg.Set E.t_pc,
   "Print comparisons interpretation (debugging)");
  ("-pd",Arg.Set E.t_pd,
   "Print debugging output (debugging)");
  ("-pi",Arg.Set E.t_pi,
   "Print abstract interpretation (debugging)");
  ("-pm",Arg.Set E.t_pm,
   "Print multiplication (debugging)");
  ("-pp",Arg.Set E.t_pp,
   "Print comPosition (debugging)");
  ("-pr",Arg.Set flags.pr,
   "Print rules (evaluated wrt abstract interpretation) (debugging)");
  ("-pr2",Arg.Set flags.pr2,
   "Print rules (evaluated wrt concrete interpretation) (debugging)");
  ("-itp",Arg.Set_string E.t_itp,"use concrete interpretation (for debugging).");
  ("-fresh",Arg.Set E.t_fresh,"use fresh variables for concrete interpretation (for debugging).");
  ("-t",Arg.Set flags.time,
   "Print timing information (default:off)");
  ("-eval",Arg.Set E.t_eval,"Evaluate expressions (for debugging).");
  ("-test",Arg.Set flags.test,"(for debugging).");
  ] in
 Arg.alignx 80 spec
;;

let help = (comment,keywords,List.map Triple.drop_snd spec);;

(*** MODULES (part 2) *********************************************************)
module Made = E.Made
open E.Made;;
open E.Made.Operators;;

(*** FUNCTIONS ****************************************************************)

(* Constructors and Destructors *)
let make fs i input output = {
 interpretation = i;
 input = input;
 output = output;
 name = name;
 code = code;
 t_flags = String.concat " " fs;
};;

let get_ip p = p.input;;
let get_op p = p.output;;

(* Complexity Bounds *)
let complexity c p = Co.other;;

(* Compare Functions *)
let equal p q =
 Problem.equal p.input q.input && Problem.equal p.output q.output
;;

(* Printers *)
let fprintf_var c fmt x = F.fprintf fmt "%s" (Sig.find_var_name x c);;

let (>>=) = Monad.(>>=);;
let (>>) = Monad.(>>);;

let fprintf fs fmt p  = 
 F.fprintf fmt "@[<1>%s:@\n%s %s" p.name p.code p.t_flags;
 F.fprintf fmt "@\n@[<1>interpretation:@\n";
 E.fprintf_intp fmt p.interpretation >>= fun _ ->
 (* F.fprintf fmt "@\n@[<1>orientation:@\n"; *)
 (* fprintf_orient fmt p.input >>= fun _ -> *)
 F.fprintf fmt "@]@\n@[<1>problem:@\n";
 Problem.fprintfm fmt p.output >>= fun _ ->
 F.fprintf fmt "@]@\n"; List.hd fs fmt >>= fun _ ->
 Monad.return (F.fprintf fmt "@]")
;;

let fprintfx fs fmt p  = failwith "Fbi: fprintfx not implemented";;

(* Processor *)
(* administrative functions *)
let (>>=) = Made.(>>=);;

(* encoding starts here *)
let degree () = return !(flags.id)

let rec encode_term t = get >>= fun c ->
 E.cache_m c.E.subterm_encodings etc t
and etc = function
 | Term.Var x -> E.var_name x >>= fun x ->
  let o = E.make_var x in return (o,o)
 | (Term.Fun (f,ts)) as t ->
  E.interpretation f >>= fun o ->
(*
  if List.for_all Term.is_var ts && (List.unique ts = ts) then
   let to_var = function Term.Var x -> E.var_name x | _ -> failwith "no var" in
   map to_var ts >>= fun xs ->
   let o' = E.substitute_vars xs o in
   return (o',o')
  else
*)
   (* name f >>= fun s -> *)
   (* get >>= fun c -> *)
   (* if !E.t_pi then Format.printf "%a" E.fprintf_intp_p (*c.state*) (s,o); *)
   map encode_term ts >>= fun os ->
   E.interpret_with_args os o 
;;

let fprintf_orient_rule fmt rule =
 get >>= fun c ->
 let (s,l) = Term.to_strings c.E.state (Rule.lhs rule) in
 let (s,r) = Term.to_strings c.E.state (Rule.rhs rule) in
 F.fprintf fmt "@\n[%s -> %s]" l r;
 encode_term (Rule.lhs rule) >>= fun (_,ll) ->
 encode_term (Rule.rhs rule) >>= fun (uu,_) ->
 F.fprintf fmt "@\n@[%a >= %a@]" E.fprintf ll E.fprintf uu;
 E.geq ll uu >>= fun phi -> 
 let _ = if !E.t_pc then F.printf "@\ngeq: %a" Logic.fprintf_p phi in
 return ()
;;

let greater_equal_rule rule = 
 encode_term (Rule.lhs rule) >>= fun (_,ll) ->
 encode_term (Rule.rhs rule) >>= fun (ur,_) ->
 get >>= fun c ->
 let (s,l) = Term.to_strings c.E.state (Rule.lhs rule) in
 let (s,r) = Term.to_strings c.E.state (Rule.rhs rule) in
 E.geq ll ur >>= E.eval (Format.sprintf "[[%s >= %s]]" l r) >>= fun phi ->
 let _ = if !E.t_pc then F.printf "@\ngeq: %a@\n%!" Logic.fprintf_p phi in
 return phi
;;

let greater_equal_rule rule = get >>= fun c ->
 if Rule.is_embedded rule then
  return Logic.top 
 else
  E.cache_m c.E.geq_encodings (greater_equal_rule) rule
;;

let greater_rule rule = 
 encode_term (Rule.lhs rule) >>= fun (_,ll) ->
 encode_term (Rule.rhs rule) >>= fun (ur,_) ->
 get >>= fun c ->
 let (s,l) = Term.to_strings c.E.state (Rule.lhs rule) in
 let (s,r) = Term.to_strings c.E.state (Rule.rhs rule) in
 E.gt ll ur >>= E.eval (Format.sprintf "[[%s > %s]]" l r) 
;;

let greater_rule rule = get >>= fun c ->
 E.cache_m c.E.gt_encodings greater_rule rule
;;

let encode_geq s w = Made.map_and greater_equal_rule (s@w);;

let encode_gt s w = get >>= fun c -> 
 let s = if !(flags.dir) then s else List.rev_append s w in
 let op = (if !(flags.dir) then Logic.big_and else Logic.big_or) in
 Made.map_op op greater_rule s
;;

let encode strict weak = get >>= fun c ->
 let (s, w) = Pair.map Trs.to_list (strict, weak) in
 let trs = Trs.union strict weak in
 let fs = Trs.funs trs in
 (*run heuristics*)
 map (E.set_degree_f trs !(flags.id)) fs >>= fun _ ->
 (*encoding*)
 encode_geq s w $&$
 encode_gt s w $&$
 Made.map_and E.desired_f fs >>= E.eval "desired" $&$
 Made.map_and E.well_defined_f fs >>= E.eval "well-defined" $&$
 Made.map_and E.monotone_f fs >>= E.eval "monotone" $&$
 (E.side_conditions ()) >>= E.eval "side-conditions"$&$
 return Logic.top
;;

let get_interpretation ass f = E.name f >>= fun name ->
 E.interpretation f >>= fun i ->
 E.eval_interpretation ass i >>= fun evi ->
 (*overwrite parametric interpretation for pretty printing*)
  get >>= fun c -> 
   Hashtbl.clear c.E.subterm_encodings;
   Hashtbl.replace c.E.interpretations f evi;
 return (name,evi)
;;

let decode_interpretation ass trs =
 Made.sequence (List.map (get_interpretation ass) (Trs.funs trs))
;;

let decode_rule ass rule = lift not (Made.eval_p (greater_rule rule) ass);;

let decode_trs ass trs = 
 Made.filter (decode_rule ass) (Trs.to_list trs) >>= (return <.> Trs.of_list)
;;

let decode_weak ass w = decode_trs ass w;;

let decode ass p = get >>= fun c -> 
 let (s,w) = Problem.get_sw p in
 decode_trs ass s >>= fun s' ->
 decode_weak ass w >>= fun w' ->
 return (Problem.set_sw s' w' p)
;;

(*printing *)

let fprintf_orient fmt p =
 let (s,w) = Problem.get_sw p in
 let rs = Trs.to_list (Trs.union s w) in
 F.fprintf fmt "@[<1>orient:";
 Made.sequence (List.map (fprintf_orient_rule fmt) rs) >>= fun _ ->
 F.fprintf fmt "@]@\n";
 return ();
;;

let print_formula fs phi = get >>= fun c -> 
(* let fprintf_formula = 
  if !(flags.p) then Logic.fprintf_smt 
  else if !(flags.p2) then Logic.fprintf_smt2
  else failwith (code ^ ".flag p or p2 expected")
  in
 let logic = "QF_NIA" in
 let source = List.join Util.id " " ("ttt2"::code::fs) in
 Format.fprintf Format.std_formatter "@[%a@]@\n" 
  (fun ppt -> fprintf_formula ~logic:logic ~source:source ppt) phi;*)
 return None
;;

let context state problem =
 let arith = {
  Logic.min = Int64.of_int !(flags.min);
  neg       = false;
  rat       = 1;
  real      = false;
  minf      = false}
 in
 {E.arith            = arith;
  ds                 = Hashtbl.create 512;
  ht_avars           = Hashtbl.create 512;
  ht_con             = Hashtbl.create 512;
  ht_eq              = Hashtbl.create 512;
  ht_geq             = Hashtbl.create 512;
  ht_gt              = Hashtbl.create 512;
  ht_pvars           = Hashtbl.create 512;
  ht_zero            = Hashtbl.create 512;
  interpretations    = Hashtbl.create 512;
  gt_encodings       = Hashtbl.create 512;
  geq_encodings      = Hashtbl.create 512;
  out_deg            = !(flags.od);
  p_constraints      = Logic.top;
  p_compatible       = Logic.top;
  state              = state;
  subterm_encodings  = Hashtbl.create 512;
 }
;;

let solve s fs p =
 let configurate s = F.printf "%s@\n%!" s; flags.help := true in
 (try (*init ();*) Arg.parsex code spec fs with Arg.Bad s -> configurate s);
 if !(flags.help) then (Arg.usage spec ("Options for "^code^":"); exit 0);
 let c = context s p in
 let (s,w) = Problem.get_sw p in
 Logic.run ~dbits:!(flags.db) ~obits:!E.t_ob (
  Made.run c (encode s w >>= fun phi ->
   if !(flags.p) || !(flags.p2) then print_formula fs phi else (
   (if !(flags.pr) then fprintf_orient F.std_formatter p else return ()) >>= fun _ ->
   let t = Unix.gettimeofday () in
   Made.liftm (Logic.solve phi) >>= function
    | None -> return None
    | Some ass -> 
    if !(flags.time) then (
     let t = Unix.gettimeofday () -. t in
     Format.printf "time in solver: %f\n%!" t;
     E.print_timing ();
    );
     decode ass p >>= fun p' -> 
     decode_interpretation ass (Trs.union s w) >>= fun i ->
     (if !(flags.pr2) then fprintf_orient F.std_formatter p else return ()) >>= fun _ ->
     get >>= fun c -> 
     return (Some (c.E.state,make fs i p p')))))
;;

(* wrap into state monad *)
let (>>=) = Monad.(>>=);;
let (>>) = Monad.(>>);;
let solve fs p =
 if (Problem.is_sp p) || (Problem.is_rp p) || (Problem.is_dp p)  then
  Monad.get >>= fun s -> match solve s fs p with
    | None -> Monad.return None
    | Some (s,p) -> Monad.set s >> Monad.return (Some p)
 else
  Monad.return None
;;

(* Complexity Bounds *)
let complexity c _ = Co.mul c Co.other;;
