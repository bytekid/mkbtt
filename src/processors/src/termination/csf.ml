(*** OPENS ********************************************************************)
open Util;;
open Rewritingx;;

(*** MODULES ******************************************************************)
module F = Format;;
module M = Monad;;
module P = Problem;;
module Co = Complexity;;

(*** TYPES ********************************************************************)
type flags = {help : bool ref};;
type t = {
 input: P.t;
 output: P.t;
 f: Function.t
}

(*** GLOBALS ******************************************************************)
let code = "csf";;
let name = "CSF Processor";;
let comment = "Counting function symbols.";;
let keywords = ["counting";"termination"];;
let flags = {help = ref false};;

let spec =
 let spec = [
  ("--help",Arg.Set flags.help,"Prints information about flags.");
  ("-help",Arg.Set flags.help,"Prints information about flags.");
  ("-h",Arg.Set flags.help,"Prints information about flags.")]
 in
 Arg.alignx 80 spec
;;

let help = (comment,keywords,List.map Triple.drop_snd spec);;

(*** FUNCTIONS ****************************************************************)
let (>>=) = M.(>>=);;
let init _ = flags.help := false;;
let get_ip t = t.input;;
let get_op t = t.output;;

let fun_count f t = List.length (Term.fun_pos f t);;

let is_weak f trs =
 let decrease r = fun_count f (Rule.lhs r) >= fun_count f (Rule.rhs r) in
 Trs.for_all decrease trs
;;

let filter_strict f trs =
 let change = ref true in
 let trs' =
  Trs.filter
   (fun r ->
    let check = (fun_count f (Rule.lhs r) <= (fun_count f (Rule.rhs r))) in
    change := !change && check;
    check
   )
   trs
 in
 (trs', not (!change))
;;

let solve' strict weak =
 if (Trs.is_duplicating strict) || (Trs.is_duplicating weak) then
  None
 else
  let fs_strict = Trs.left_funs strict in
  let fs_weak = Trs.left_funs weak in
  let fs = List.union fs_strict fs_weak in
  let check f = function
   | None ->
     if (is_weak f strict) && (is_weak f weak) then
      let (strict',cs) = filter_strict f strict in
      let (weak',cw) = filter_strict f weak in
      if (cs || cw) then Some (f, strict', weak') else None
     else None 
   | r -> r
  in List.fold_right check fs None
;;


let solve s fs p =
 let configurate s = F.printf "%s@\n%!" s; flags.help := true in
 (try init (); Arg.parsex code spec fs with Arg.Bad s -> configurate s);
 if !(flags.help) then (Arg.usage spec ("Options for "^code^":"); exit 0);
 (* do something *)
 let (s,w) = P.get_sw p in
 match solve' s w with
  | None -> None
  | Some(f,s,w) -> Some {input=p; output=P.set_sw s w p; f=f}
;;

let fprintf fs fmt p  =
 F.fprintf fmt "@[<1>%s:" name;
 F.fprintf fmt "@]@\n@[<1>function symbol:@\n";
 Monad.fprintf_fun fmt p.f >>= fun _ ->
 F.fprintf fmt "@]@\n@[<1>problem:@\n";
 P.fprintfm fmt p.output >>= fun _ ->
 F.fprintf fmt "@]@\n"; List.hd fs fmt >>= fun _ ->
 Monad.return (F.fprintf fmt "@]")
;;

let equal p q =
 P.equal p.input q.input && P.equal p.output q.output
;;

(* Complexity Bounds *)
let complexity c _ = Co.mul c Co.other;;

(* wrap into state monad *)
let (>>=) = M.(>>=);;
let solve fs p = M.get >>= fun s -> M.return (solve s fs p);;


