(*** SUBMODULES **********************************************************)
module Fun = Rewriting.Function;;
module Pos = Rewriting.Position;;
module T = U.Term;;
module Sig = U.Signature;;
module Label = Types.Label;;
module M = U.Monad;;

(*** OPENS ***************************************************************)
open Util;;
open M;;

(*** TYPES ***************************************************************)
type symbol = 
 | FunSym of Fun.t 
 | StarSym 
 | ACSym of (Fun.t * T.t list * Pos.t)
and t = Nil | Element of symbol * t * t

(*** EXCEPTIONS **********************************************************)
exception Empty_pstring

exception Not_ac_symbol

(*** FUNCTIONS ***********************************************************)
let is_ac_symbol = M.is_theory U.Label.AC

let of_term = 
 let rec of_term' p next after = function
  | T.Var _ -> return (Element(StarSym, next, after))
  | T.Fun(f, ts) ->
   is_ac_symbol f >>= fun f_is_ac ->
   if f_is_ac then
    (*let of_term = of_term' Nil Nil in
    map (fun t -> of_term t >>= (return <.> (Pair.make t))) ts >>= fun pts ->*)
    let pts = ts in
    return (Element(ACSym (f,pts,p),next,after))
   else
    let dup x = return (x,x) in
    let of_ti i ti (n,a) = of_term' (Pos.add_last i p) n a ti >>= dup in
    foldri of_ti (next,after) ts  >>= fun (next',_) -> 
    return (Element(FunSym f, next', after))
in of_term' Pos.root Nil Nil
;;

let is_empty p = p = Nil

let rec to_stringm = function
 | Nil -> return "[]"
 | Element(FunSym f, p, _) -> 
  find_fun_name f >>= fun fs -> 
  to_stringm p >>= fun s -> 
  return (fs^"."^s)
 | Element(StarSym, p, _) -> to_stringm p >>= fun s -> return ("*."^s)
 | Element(ACSym(f,ts,_), p, _) ->
  let app s t = of_term t >>= to_stringm >>= fun s' -> return (s^", "^s') in
  foldl app "" ts >>= fun tss ->
  to_stringm p >>= fun s ->
  find_fun_name f >>= fun fs -> return (fs^"["^tss^"]."^s)
;;

let next = function
 | Nil -> raise Empty_pstring
 | Element(_, n, _) -> n
;;

let after = function
 | Nil -> raise Empty_pstring
 | Element(_, _, a)  -> a
;;

let top_and_next = function 
   Nil -> None
 | Element(FunSym l,n,_) -> Some (Label.Fun l,n)
 | Element(ACSym(l,_,_),n,_) -> Some (Label.AC l,n)
 | Element(StarSym,n,_) -> Some (Label.Star,n)
;;

let top_fun_label = function
   Nil -> raise Empty_pstring
 | Element(FunSym l,n,_) -> Some (Label.Fun l)
 | Element(ACSym(l,_,_),n,_) -> Some (Label.AC l)
 | Element(StarSym,n,_) -> None
;;

let top_label = function
   Nil -> raise Empty_pstring
 | Element(FunSym l,_,_) -> Label.Fun l
 | Element(ACSym(l,_,_),_,_) -> Label.AC l
 | Element(StarSym,_,_) -> Label.Star
;;

let ac_subterms = function
 | Element(ACSym(_,ts,_),_,_) -> ts
 | _ -> raise Not_ac_symbol
;;

let ac_term = function
 | Element(ACSym(f,ts,_),_,_) -> T.Fun(f,ts)
 | _ -> raise Not_ac_symbol
;;

let pos = function
 | Element(ACSym(_,_,p),_,_) -> p
 | _ -> raise Not_ac_symbol
;;

(* TESTS *)

let sigma = Sig.empty 20;;
let x,sigma = Sig.create_var "x" sigma;;
let y,sigma = Sig.create_var "y" sigma;;
let z,sigma = Sig.create_var "z" sigma;;
let u,sigma = Sig.create_var "u" sigma;;
let v,sigma = Sig.create_var "v" sigma;;
let w,sigma = Sig.create_var "w" sigma;;
let x,y,z,u,v,w = T.Var x, T.Var y, T.Var z, T.Var u, T.Var v, T.Var w;;
let f,sigma = Sig.create_fun 2 "f" sigma;;
let a,sigma = Sig.create_fun 0 "a" sigma;;
let b,sigma = Sig.create_fun 0 "b" sigma;;
let c,sigma = Sig.create_fun 0 "c" sigma;;
let d,sigma = Sig.create_fun 0 "d" sigma;;
let e,sigma = Sig.create_fun 0 "e" sigma;;
let g,sigma = Sig.create_fun 1 "g" sigma;;
let h,sigma = Sig.create_fun 2 "h" sigma;;
let a_ = T.Fun(a, []);;
let b_ = T.Fun(b, []);;
let c_ = T.Fun(c, []);;
let d_ = T.Fun(d, []);;
let e_ = T.Fun(e, []);;
let faa = T.Fun(f, [a_;a_]);;
let faaa = T.Fun(f, [a_;faa]);;
let fabc = T.Fun(f, [a_;T.Fun(f, [b_;c_])]);;
let fcab = T.Fun(f, [c_;T.Fun(f, [a_;b_])]);;
let faby = T.Fun(f, [a_;T.Fun(f, [b_;y])]);;
let fxy = T.Fun(f, [x;y]);;
let fax = T.Fun(f, [a_;x]);;
let hfaxgd = T.Fun(h,[fax;T.Fun(g,[d_])]);;
let hhxfaad = T.Fun(h,[T.Fun(h,[x;faa]); d_]);;
let run_pstring t = Either.right (M.run sigma (of_term t >>= to_stringm));;
let run_after t = Either.right (M.run sigma (of_term t >>= (to_stringm <.> after <.> next)));;
