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
open Processors;;
open Rewritingx;;

(*** MODULES ******************************************************************)
module C = Complexity;;
module F = Format;;
module M = Monad;;
module O = Option;;
module P = Processor;;
module S = Status;;
module Split = Transformation.Split;;

(*** TYPES ********************************************************************)
type t = Open | Qed | Processor of P.t * t list;;

(*** FUNCTIONS ****************************************************************)
let (>>=) = M.(>>=);;

(* Constructors *)
let unfinished = Open;;
let finished = Qed;;
let make pr ps = Processor (pr,ps);;

let rec append p = function
 | Open -> p
 | Qed -> failwith "finished proof object"
 | Processor (pr,ps) -> Processor (pr,List.map (append p) ps)
;;

let merge =
 let equal p q = match (p,q) with
  | Processor (pr,_), Processor (pr',_) -> P.equal pr pr'
  | _, _ -> false
 in
 let rec merge p q = match (p,q) with
  | Open, Open -> Open
  | Qed, Qed -> Qed
  | Processor (pr,ps), Processor (pr',qs) ->
   let combine (ps,qs) p =
    try
     let ps = (merge p (List.find (equal p) qs)) :: ps in
     let compare p = (-) 1 <.> Bool.to_int <.> equal p in
     let qs = List.remove_all ~c:compare p qs in
     (ps,qs)
    with Not_found -> (p :: ps,qs)
   in
   if P.equal pr pr' then
    let (ps,qs) = List.foldl combine ([],qs) ps in
    Processor (pr,List.rev (List.rev_append qs ps))
   else failwith "conflicting proof structures"
  | _, _ -> failwith "conflicting proof structures"
 in
 List.foldl1 merge <?> "empty list"
;;

let optimize pr p q =
 let rec optimize = function
  | Open -> Open
  | Qed -> Qed
  | Processor (pr',ps') ->
   if P.equal pr pr' then
    let ip = P.input pr' and op = P.output pr' in
    let op' = match q with Processor (pr,_) -> P.input pr | _ -> [] in
    if List.length ip = 1 && List.length op = 1 then
     if Problem.is_empty (List.hd op) then q
     else if Problem.is_cp (List.hd ip) then
(* FIXME DELETE
(
Format.printf "@\ninput:@\n";
Problem.fprintf Format.std_formatter (List.hd ip);
Format.printf "@\nnew:@\n";
Problem.fprintf Format.std_formatter (List.hd op');
Format.printf "@\nold:@\n%!";
Problem.fprintf Format.std_formatter (List.hd op);
Format.printf "@\n@\n%!";
*)
      match Split.generate [] (List.hd ip) (op' @ op) with
       | Some proof -> Processor (P.P_split proof,q :: ps')
       | None -> failwith "illegal input"
(*
)
*)
     else failwith "illegal input"
    else failwith "illegal input"
   else Processor (pr',List.rev (List.map optimize ps'))
 in
 optimize p
;;

(* Predicates *)
let is_qed = function Qed -> true | _ -> false;;
let is_open = function Open -> true | _ -> false;;
let is_processor = function Processor _ -> true | _ -> false;;

(* Complexity Bounds *)
let rec complexity = function
 | Open -> None
 | Qed -> Some C.constant
 | Processor (pr,ps) ->
  let collect p = flip O.map (complexity p) <.> C.add in
  let combine c p = O.fold (collect p) None c in
  let c = List.foldl combine (Some C.constant) ps in
  O.fold (flip P.complexity pr) None c
;;

let critical p =
 let rec critical r = function
  | Open | Qed -> r
  | Processor (pr,ps) -> match List.foldl critical r ps with
   | None -> None
   | Some (c,prs) ->
    let c' = P.complexity C.constant pr in
    O.map (fun c' ->
     if C.equal c c' then (c,pr::prs)
     else if C.(<) c c' then (c',[pr]) else (c,prs)) c'
 in
 O.fold (fun (c,prs) ->
  if C.equal C.constant c || prs = [] then None
  else Some (c,prs)) None (critical (Some (C.constant,[])) p)
;;

(* Printers *)
let rec fprintf fmt = function
 | Open -> M.return (F.fprintf fmt "@[Open@]")
 | Qed -> M.return (F.fprintf fmt "@[Qed@]")
 | Processor (pr,ps) ->
  let fs = List.map (flip fprintf) ps in
  F.fprintf fmt "@[<1>"; P.fprintf fs fmt pr >>= fun _ ->
  M.return (F.fprintf fmt "@]")
;;

let to_string p =
 fprintf F.str_formatter p >>= (M.return <.> F.flush_str_formatter)
;;

let fprintfx s fmt p =
 let rec fprintfx dp fmt = function
 | Open -> M.return (F.fprintf fmt "<open/>")
 | Qed ->
  if Status.is_nonterminating s then M.return ()
  else if dp then M.return (F.fprintf fmt "<pIsEmpty/>")
  else M.return (F.fprintf fmt "<rIsEmpty/>")
 | Processor (pr,ps) ->
  let dp = match pr with P.P_dp _ -> true | _ -> dp in
  let fs = List.map (flip (fprintfx dp)) ps in
  P.fprintfx s fs fmt pr >>= fun _ -> M.return ()
 in
 F.fprintf fmt "@{<proof>";
 fprintfx false fmt p >>= fun _ ->
 M.return (F.fprintf fmt "@}")
;;

let to_stringx s p =
 fprintfx s F.str_formatter p >>= (M.return <.> F.flush_str_formatter)
;;
