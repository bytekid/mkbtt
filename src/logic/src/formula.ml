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

(*** FUNCTIONS ****************************************************************)
open Util;;

(*** GLOBALS ******************************************************************)
let print_formula = ref false;;
let set_print_formula _ = print_formula := true;;

type arith = {
  min  : Int64.t;  (* minimal value that must be representable *)
  neg  : bool; (* negative values allowed *)
  rat  : int;  (* denumerator for rational values *)
  real : bool; (* use reals *)
  minf : bool; (* -infinity value allowed *)
};;

type prop = int;; (* identifier *)

type p = 
 | Top
 | Bot
 | P of prop
 | Not of p
 | And of p * p
 | Or of p * p
 (*| Implies of p * p *)
 | Iff of p * p
 | Eq of a * a
 | Gt of a * a
 | Ge of a * a
 | Obits of int * p
and a = 
 | A of (int * arith)
 | C of Number.t
 | Fresh of a
 | Add of a * a
 | Sub of a * a
 | Mul of a * a
 | Ite of p * a * a
 | Max of a * a
 | Min of a * a
;;

let a_id = Pervasives.fst;;
let a_spec = Pervasives.snd;;
let a_build id spec = (id, spec);;
let unwrap_a = function 
 | (A x) -> x
 | _ -> failwith "value not allowed"
;;

let rec fpfa ppf = function
 | A x -> Format.fprintf ppf "x%d" (a_id x)
 | C x -> Format.fprintf ppf "%a" Number.fprintf x
 | Add (a,b) -> Format.fprintf ppf "(+ %a %a)" fpfa a fpfa b
 | Sub (a,b) -> Format.fprintf ppf "(- %a %a)" fpfa a fpfa b
 | Mul (a,b) -> Format.fprintf ppf "(* %a %a)" fpfa a fpfa b
 | Ite (x,a,b) -> Format.fprintf ppf "(ite %a %a %a)" fpfp x fpfa a fpfa b
 | Fresh a -> Format.fprintf ppf "(fresh %a)" fpfa a
 (* | Max (a,b) -> Format.fprintf ppf "(max %a %a)" fpfa a fpfa b *)
 | Max (a,b) -> fpfa ppf (Ite(Gt(a,b),a,b))
 (* | Min (a,b) -> Format.fprintf ppf "(min %a %a)" fpfa a fpfa b *)
 | Min (a,b) -> fpfa ppf (Ite(Gt(b,a),a,b))
and fpfp ppf = function
 | Top -> Format.fprintf ppf "true"
 | Bot -> Format.fprintf ppf "false"
 | P x -> Format.fprintf ppf "p%d" x
 | Not x -> Format.fprintf ppf "(not %a)" fpfp x
 | And (x,y) -> Format.fprintf ppf "(and %a %a)" fpfp x fpfp y
 | Or (x,y) -> Format.fprintf ppf "(or %a %a)" fpfp x fpfp y
 (*| Implies (x,y) -> Format.fprintf ppf "(%a -> %a)" fpfp x fpfp y *)
 | Iff (x,y) -> Format.fprintf ppf "(iff %a %a)" fpfp x fpfp y
 | Eq (a,b) -> Format.fprintf ppf "(= %a %a)" fpfa a fpfa b
 | Gt (a,b) -> Format.fprintf ppf "(> %a %a)" fpfa a fpfa b
 | Ge (a,b) -> Format.fprintf ppf "(>= %a %a)" fpfa a fpfa b
 | Obits (n,x) -> Format.fprintf ppf "%a" fpfp x
;;

let fprintf_a = fpfa;;
let fprintf_p = fpfp;;

let to_string_p f =
 Format.fprintf Format.str_formatter "%a" fpfp f;
 Format.flush_str_formatter ()
;;

let to_string_a f = 
 Format.fprintf Format.str_formatter "%a" fpfa f;
 Format.flush_str_formatter ()
;;

(*real functionality *)
let is_atom = function
 | Bot | Top | P _ -> true
 | _ -> false

let is_literal = function 
 | Not a | a -> is_atom a
;;

(************************** CONSTRUCTORS **************************************)
(* construct arithmetical variables *)
let arith ?(spec=None) id = match spec with 
  | None -> 
   A (a_build id {min=Int64.of_int 1; neg=false; rat=1; real=false; minf=false})
  | Some spec -> A (a_build id spec)
;;

(* construct propositional variables *)
let prop x = P x;;
let constant c = C c;;


(************************** CONSTANTS *****************************************)
let top = Top;;
let bot = Bot;;
let zero = constant Number.zero;;
let one = constant Number.one;;
let minf = constant Number.minf;;

(************************** COMBINATORS ***************************************)
let (<&>) x y = 
 if x = bot || y = bot then bot 
 else if x = top then y
 else if y = top then x
 else And (x,y);;

let (~!) x = if x = bot then top else if x = top then bot else Not x;;
let neg x = ~!x;;

let (<|>) x y = 
 if x = top || y = top then top 
 else if x = bot then y
 else if y = bot then x
 else Or (x,y)
;;

(*let (<->>) x y = if x = bot || y = top then top else if x = top then y
else if y = bot then neg x else Implies (x,y);; *)
let (<->>) x y = (neg x) <|> y;;

let (<<->>) x y = if x = y then top else if x = ~!y then bot else Iff (x,y);;

let xor a b = neg (a <<->> b);;

let (<<->) x y = y <->> x;;

let (<+>) a b = if a = zero then b else if b = zero then a else Add (a,b);;
let (<->) a b = if b = zero then a else Sub (a,b);;
let (<*>) a b = if a = zero || b = zero then zero 
 else if a = one then b else if b = one then a else  Mul (a,b);;
let ite x a b = if x = top then a else if x = bot then b else Ite (x, a, b);;
let (<:>) ifa b = ifa b;;
let (<?>) = ite;;
let (<=>) a b = if a = b then top else Eq (a,b);;
let (<>>) a b = if a = b then bot else Gt (a,b);;
let (<>=>) a b = if a = b then top else Ge (a,b);;
let (<<>) a b = b <>> a;;
let (<<=>) a b = b <>=> a;;
let max a b = if a = b then a else Max (a,b);;
let min a b = if a = b then a else Min (a,b);;

let big_and = List.fold_left (<&>) top;;
let big_or = List.fold_left (<|>) bot;;
let big_sum = List.fold_left (<+>) zero;;

let obits n x = Obits(n,x);;

(*experimental*)
(*TODO: make clearer*)
let spec x = 
 let a = a_spec x in
 if a.real then "Real" else
 if a.rat > 1 then "Rat" else
 if a.neg then "Int" else "Nat"
;;

let extra f = 
let a_tbl, b_tbl = Hashtbl.create 512, Hashtbl.create 512 in
 let rec ef = function
 | A x as a -> Hashtbl.replace a_tbl (a,x) ()
 | C x -> ()
 | Add (a,b) | Sub (a,b) | Mul (a,b) | Max (a,b) | Min (a,b) -> ef a; ef b
 | Ite (x,a,b) -> ep x; ef a; ef b
 | Fresh a -> ef a; 
and ep = function
 | Top | Bot -> ()
 | P x as b -> Hashtbl.replace b_tbl b ()
 | Not x -> ep x
 | And (x,y) | Or (x,y) | Iff (x,y) -> ep x; ep y
 | Eq (a,b)  | Gt (a,b) | Ge (a,b) -> ef a; ef b
 | Obits(_,x) -> ep x
 in
 ep f;
 let elements tbl = Hashtbl.fold (fun k _ acc -> k::acc) tbl [] in
 (elements a_tbl, elements b_tbl)
;;

let spec_to_string spec = 
 let a = a_spec spec in
 if a.real then "Real" else
 if a.rat > 1 then "Rat" else
 "Int"
;;

let pf ppf ef = if ef <> [] then
 (*since Nat is not supported print Int*)
 let pff ppf (v,spec) = Format.fprintf ppf "(%a %s)" fprintf_a v (spec_to_string spec) in
 Format.fprintf ppf ":extrafuns (%a)@\n" (List.fprintf pff "@\n") ef;
;;
 
let fprintf_p2 ppf x = Format.fprintf ppf "(%a)" fprintf_p x;;

let pp ppf ep = if ep <> [] then
 Format.fprintf ppf ":extrapreds (%a)@\n" (List.fprintf fprintf_p2 "@\n") ep;
;;

(* since Nat is not supported *)
let pa ppf ef = 
 let paa ppf (v,_) = Format.fprintf ppf ":assumption (>= %a 0)" fprintf_a v in
 let nnegs = List.filter (fun (_,spec) -> (a_spec spec).neg = false) ef in
 Format.fprintf ppf "%a@\n" (List.fprintf paa "@\n") nnegs 
;;


let fprintf_smt ?(logic="QF_NIA") ppf f =
 let p = Format.fprintf in
 let (ef,ep) = extra f in
 p ppf "@[(benchmark ttt2@\n";
 p ppf ":logic %s@\n" logic;
 p ppf ":status unknown@\n";
 pf ppf ef;
 pp ppf ep;
 pa ppf ef;
 p ppf ":formula %a)" fprintf_p f;
;;

(**************experimental**************)
let rec is_muli = function
 | Mul (a,b) -> is_muli a && is_muli b
 | C x -> true
 | A x -> true
 | _ -> false

let rec mula acc = function
 | Mul (a,b) -> mula (mula acc a) b
 | C x -> C x::acc
 | A x -> A x::acc
;;

let m_as_list a = 
 let ui a = a_id (unwrap_a a) in
 List.sort (fun a b -> Pervasives.compare (ui a) (ui b)) (mula [] a)
;;

let mule a = 
 let m = m_as_list a in
 List.foldl (<*>) (List.hd m) (List.tl m)

let rec nfa = function
 | A x -> A x
 | C x -> C x
 | Add (a,b) -> Add (nfa a,nfa b) 
 | Sub (a,b) -> Sub (nfa a,nfa b)
 | Mul (Add (a,b),c) -> nfa (Add(Mul(a,c),Mul(b,c)))
 | Mul (c,Add (a,b)) -> nfa (Add(Mul(c,a),Mul(c,b)))
 | Mul (a,Mul(b,c)) -> nfa (Mul(Mul(a,b),c))
 | Mul (a,b) -> Mul(nfa a, nfa b)
 | Ite (x,a,b) -> Ite(nfb x,nfa a,nfa b)
 | Fresh a -> Fresh (nfa a)
 | Max (a,b) -> Max (nfa a,nfa b)
 | Min (a,b) -> Min (nfa a,nfa b)
and nfb = function
 | Top -> Top
 | Bot -> Bot
 | P x -> P x
 | Not x -> Not (nfb x)
 | And (x,y) -> And (nfb x,nfb y)
 | Or (x,y) -> Or (nfb x,nfb y)
 (*| Implies (x,y) -> Format.fprintf ppf "(%a -> %a)" fpfp x fpfp y *)
 | Iff (x,y) -> Iff (nfb x,nfb y)
 | Eq (a,b) -> Eq (nfa a,nfa b)
 | Gt (a,b) -> Gt (nfa a,nfa b)
 | Ge (a,b) -> Ge (nfa a,nfa b)
;;

(*prerequisite: multiplications are on bottom level*)
let rec nf = function
 | A x -> A x
 | C x -> C x
 | Add (a,b) -> Add (nf a,nf b) 
 | Sub (a,b) -> Sub (nf a,nf b)
 | (Mul (a,b)) as m -> mule m
 | Ite (x,a,b) -> Ite(nf2 x,nf a,nf b)
 | Fresh a -> Fresh (nf a)
 | Max (a,b) -> Max (nf a,nf b)
 | Min (a,b) -> Min (nf a,nf b)
and nf2 = function
 | Top -> Top
 | Bot -> Bot
 | P x -> P x
 | Not x -> Not (nf2 x)
 | And (x,y) -> And (nf2 x,nf2 y)
 | Or (x,y) -> Or (nf2 x,nf2 y)
 (*| Implies (x,y) -> Format.fprintf ppf "(%a -> %a)" fpfp x fpfp y *)
 | Iff (x,y) -> Iff (nf2 x,nf2 y)
 | Eq (a,b) -> Eq (nf a,nf b)
 | Gt (a,b) -> Gt (nf a,nf b)
 | Ge (a,b) -> Ge (nf a,nf b)
;;

let rec get_mule = function
 | A x -> []
 | C x -> []
 | Add (a,b) -> get_mule a @ get_mule b
 | Sub (a,b) -> get_mule a @ get_mule b
 | (Mul (a,b)) as m -> [m]
 | Ite (x,a,b) -> get_mulb x @ get_mule a @ get_mule b
 | Fresh a -> get_mule a 
 | Max (a,b) -> get_mule a @ get_mule b
 | Min (a,b) -> get_mule a @ get_mule b
and get_mulb = function
 | Top -> []
 | Bot -> []
 | P x -> []
 | Not x -> get_mulb x
 | And (x,y) -> get_mulb x @ get_mulb y
 | Or (x,y) -> get_mulb x @ get_mulb y
 (*| Implies (x,y) -> Format.fprintf ppf "(%a -> %a)" fpfp x fpfp y *)
 | Iff (x,y) -> get_mulb x @ get_mulb y
 | Eq (a,b) -> get_mule a @ get_mule b 
 | Gt (a,b) -> get_mule a @ get_mule b
 | Ge (a,b) -> get_mule a @ get_mule b
;;

let get_mulc ms = List.map m_as_list ms;;

let count_one p m = 
 let c = uncurry Pervasives.min (Pair.map (flip List.count m) p) in
 if fst p = snd p && c = 1 then 0 else c
;;
let count p ms = List.foldr (fun m -> (+) (count_one p m)) 0 ms;;

let to_string_pair (a,b) = 
 Format.sprintf "(%s,%s)" (to_string_a a) (to_string_a b);;
 
let foo ms = 
let vs = List.unique (List.flatten ms) in
(*TODO: remove symmetric pairs*)
let ps = List.product vs vs in
let ts = List.map (fun p -> (p,count p ms)) ps in 
let ts = List.rev (List.sort (fun e0 e1 -> compare (snd e0) (snd e1)) ts) in
(*let hd = List.hd ts in
Format.printf "(%s,%d)" (to_string_pair (fst hd)) (snd hd);
*)
(*List.hd ts*)
ts
;;

let substitute (a,b) n m = 
 if a = b then 
  if List.count a m > 1 then n::List.diff m [a;a] else m
 else
  if count_one (a,b) m > 0 then n::List.diff m [a;b] else m
;;
let substitute p n ms = List.map (substitute p n) ms;;

let rec map_a fa fb a = match fa a with
 | A x -> A x
 | C x -> C x
 | Add (a,b) -> Add (map_a fa fb a, map_a fa fb b)
 | Sub (a,b) -> Sub (map_a fa fb a, map_a fa fb b)
 | Mul (a,b) -> Mul (map_a fa fb a, map_a fa fb b)
 | Ite (x,a,b) -> Ite (map_b fa fb x, map_a fa fb a, map_a fa fb b)
 | Fresh a -> Fresh (map_a fa fb a)
 | Max (a,b) -> Max (map_a fa fb a, map_a fa fb b)
 | Min (a,b) -> Max(map_a fa fb a, map_a fa fb b)
and map_b fa fb x = match fb x with
 | Top -> Top
 | Bot -> Bot
 | P x -> P x
 | Not x -> Not (map_b fa fb x)
 | And (x,y) -> And (map_b fa fb x, map_b fa fb y)
 | Or (x,y) -> Or (map_b fa fb x, map_b fa fb y)
 (*| Implies (x,y) -> Format.fprintf ppf "(%a -> %a)" fpfp x fpfp y *)
 | Iff (x,y) -> Iff (map_b fa fb x, map_b fa fb y)
 | Eq (a,b) -> Eq (map_a fa fb a, map_a fa fb b)
 | Gt (a,b) -> Gt (map_a fa fb a, map_a fa fb b)
 | Ge (a,b) -> Ge (map_a fa fb a, map_a fa fb b)
;;

let apply x t f = map_b (fun elt -> if x = elt then t else elt) id f;;

let rec simplify_b = function
 | Top -> Top
 | Bot -> Bot
 | P x -> P x
 | Not (Not x) -> simplify_b x
 | Not (Top) -> Bot
 | Not (Bot) -> Top
 | Not (x) -> Not (simplify_b x)
 | And (Bot,x) |  And (x,Bot) -> Bot
 | And (Top,x) |  And (x,Top) -> simplify_b x
 | And (x,y)                  ->  And(simplify_b x,simplify_b y)
 | Or (Bot,x)  |  Or (x,Bot) -> simplify_b x
 | Or (x,Top)  |  Or (Top,x) -> Top
 | Or (x,y)                  -> Or (simplify_b x,simplify_b y)
 | Iff(Top,x)  |  Iff(x,Top) -> x
 | Iff(Bot,x)  | Iff(x,Bot) -> simplify_b (Not x)
 | Iff(x,y)                  -> Iff (simplify_b x,simplify_b y)
 | Eq _ | Gt _ | Ge _ -> failwith "simplify: not boolean"
;;

let rec fixpoint f x = 
 let y = f x in
 if x = y then x else fixpoint f y
;;

let simplify_b phi = fixpoint simplify_b phi;;
