(*** SUBMODULES (1) ******************************************************)
module List = Util.List;;
module M = U.Monad;;
module Sig = U.Signature;;
module T = U.Term;;
module Trs = U.Trs;;
module L = U.Label;;
module E = Equation;;
module R = U.Rule;;
module Ren = TrsRenaming;;
module SymSub = Ren.SymbolSubstitution;;

(*** GLOBALS *************************************************************)
let sigma = Sig.empty 200;;
let x,sigma = Sig.create_var "x" sigma;;
let y,sigma = Sig.create_var "y" sigma;;
let z,sigma = Sig.create_var "z" sigma;;
let x,y,z = T.Var x, T.Var y, T.Var z;;
let f,sigma = Sig.create_fun 2 "f" sigma;;
let f,sigma = Sig.set_theory f U.Label.AC sigma;;
let zero,sigma = Sig.create_fun 0 "0" sigma;;
let zero = T.Fun(zero,[]);;
let one,sigma = Sig.create_fun 0 "1" sigma;;
let one = T.Fun(one,[]);;
let i,sigma = Sig.create_fun 1 "i" sigma;;
let m,sigma = Sig.create_fun 2 "m" sigma;;
let mac,sigma = Sig.create_fun 2 "mac" sigma;;
let mac,sigma = Sig.set_theory mac U.Label.AC sigma;;

let eqn = E.of_terms
let rl = R.of_terms;;

(*** OPENS ***************************************************************)
open Util;;
open M;;

(*** SUBMODULES (2) ******************************************************)
module AC = struct
 let axioms = [];;
 let s = [];;
end

module ACU = struct
 let axioms = let f0x = T.Fun(f, [zero; x]) in [eqn f0x x];;
 let s = let f0x = T.Fun(f, [zero; x]) in Trs.of_list [rl f0x x];;
end

module Group = struct
 let axioms =
   (eqn (T.Fun(f,[T.Fun(i,[x]);x])) zero) :: ACU.axioms
 ;;

 let s =
  let inv = rl (T.Fun(f, [T.Fun(i, [x]); x])) zero in
  let invzero = rl (T.Fun(i, [zero])) zero in
  let invinv = rl (T.Fun(i, [T.Fun(i, [x])])) x in
  let invf = 
   let ifxy = T.Fun(i,[T.Fun(f,[x;y])]) in 
   let fixiy = T.Fun(f,[T.Fun(i,[x]);T.Fun(i,[y])]) in
   rl ifxy fixiy in
  Trs.union (Trs.of_list [inv;invzero;invinv;invf]) ACU.s
 ;;

 let s' =
  let inv = rl (T.Fun(f, [T.Fun(i, [x]); x])) zero in
  let invzero = rl (T.Fun(i, [zero])) zero in
  let invinv = rl (T.Fun(i, [T.Fun(i, [x])])) x in
  let invf =
   let ifxy = T.Fun(i,[T.Fun(f,[x;y])]) in
   let fixiy = T.Fun(f,[T.Fun(i,[x]);T.Fun(i,[y])]) in
   rl fixiy ifxy in
  let invf' =
   let t = T.Fun(i,[T.Fun(f,[T.Fun(i,[x]);y])]) in
   let t' = T.Fun(f,[x;T.Fun(i,[y])]) in
   rl t t' in
  let invf'' =
   let t = T.Fun(f,[T.Fun(i,[T.Fun(f,[x;y])]); y]) in
   let t' = T.Fun(i,[x]) in
   rl t t' in
  Trs.union (Trs.of_list [inv;invzero;invinv;invf;invf';invf'']) ACU.s
 ;;

end

module RingUnitRight = struct
 let r_axioms =
  let one = let mx1 = T.Fun(m, [x;one]) in eqn mx1 x in
  let dist =
   let mpxyz = T.Fun(m,[z;T.Fun(f,[x;y])]) in
   let pmxzmyz = T.Fun(f,[T.Fun(m,[z;x]);T.Fun(m,[z;y])]) in
   eqn mpxyz pmxzmyz in
   [one;dist]
 ;;

 let axioms = r_axioms @ Group.axioms

 let r_s =
  let one = let mx1 = T.Fun(m, [x;one]) in rl mx1 x in
  let dist =
   let mpxyz = T.Fun(m,[z;T.Fun(f,[x;y])]) in
   let pmxzmyz = T.Fun(f,[T.Fun(m,[z;x]);T.Fun(m,[z;y])]) in
   rl mpxyz pmxzmyz in
  let zero = rl (T.Fun(m, [x;zero])) zero in
  let inv = rl (T.Fun(m, [x;T.Fun(i,[y])])) (T.Fun(i,[T.Fun(m,[x;y])])) in
   (Trs.of_list [one;dist;zero;inv])
 ;;

 let s = Trs.union r_s Group.s
end

module RingUnit = struct
 let r_axioms =
  let one = let mx1 = T.Fun(mac, [one; x]) in eqn mx1 x in
  let dist =
   let mpxyz = T.Fun(mac,[z;T.Fun(f,[x;y])]) in
   let pmxzmyz = T.Fun(f,[T.Fun(mac,[z;x]);T.Fun(mac,[z;y])]) in
   eqn mpxyz pmxzmyz in
   [one;dist]
 ;;

 let axioms = r_axioms @ Group.axioms

 let r_s =
  let one = let mx1 = T.Fun(mac, [one;x]) in rl mx1 x in
  let dist =
   let mpxyz = T.Fun(mac,[z;T.Fun(f,[x;y])]) in
   let pmxzmyz = T.Fun(f,[T.Fun(mac,[z;x]);T.Fun(mac,[z;y])]) in
   rl mpxyz pmxzmyz in
  let zero = rl (T.Fun(mac, [zero; x])) zero in 
  let inv = rl (T.Fun(mac, [T.Fun(i,[y]);x])) (T.Fun(i,[T.Fun(mac,[x;y])])) in
   (Trs.of_list [one;dist;zero;inv])                          
 ;;

 let s = Trs.union r_s Group.s
end


let theories = [
 (ACU.axioms,ACU.s,"ACU");
 (Group.axioms,Group.s,"G");
 (RingUnitRight.axioms,RingUnitRight.s,"RU-R");
 (RingUnit.axioms,RingUnit.s,"RU")
];;

module Find = struct

 let ac_preserving theta =
  let check b (f,f') =
   if not b then return false else
    M.is_theory L.AC f >>= fun f_is_ac ->
    M.is_theory L.AC f' >>= fun f_is_ac' ->
    return (f_is_ac = f_is_ac')
  in foldl check true (SymSub.to_list theta)
 ;;

 let eqs_to_stringm eqs =
 foldr
  (fun eq s ->
   Equation.to_stringm eq >>= fun s' ->
   return (s' ^ "\n" ^ s)) "" eqs
;;


 let renamable es axioms =
  map (E.mapm Termx.flatten) es >>= fun es ->
  map (E.mapm Termx.flatten) axioms >>= fun axioms ->
  let rens = Ren.equation_sets_renamable es axioms in
(*      eqs_to_stringm es >>= fun est ->
     eqs_to_stringm axioms >>= fun ast ->
  Format.printf "\nCHECK\n%s\n%s\n%i renamings found\n%!" est ast (List.length rens);*)
  filter ac_preserving rens
 ;; 

 let rename = SymSub.rename_trs;;

 let largest eqs =
(*  eqs_to_stringm eqs >>= fun s ->
  Format.printf "Checking theory of \n%s\n%!" s;*)
  let peqs = List.powerset eqs in
  let eqss n = List.filter (fun l -> List.length l = n) peqs in
  let map axioms = 
   let find i eqs res =
    match res with
     | None -> 
      (renamable axioms eqs >>= function
       | [] -> return None
       | theta :: _ -> return (Some theta))
     | some -> return some
    in foldri find None (eqss (List.length axioms))
  in
  let find res (a,s,n) =
   map a >>= function
    | None -> return res
    | Some theta -> return (Some (theta,rename s theta,n))
  in
  foldl find None theories >>= function
   | None -> return (SymSub.empty,Trs.empty,"none")
   | Some r -> return r
;;
end



(* TESTS *)
let test () =
 let x,sigma = Sig.create_var "x" sigma in
 let y,sigma = Sig.create_var "y" sigma in
 let z,sigma = Sig.create_var "z" sigma in
 let x,y,z = T.Var x, T.Var y, T.Var z in
 let h,sigma = Sig.create_fun 2 "f" sigma in
 let h,sigma = Sig.set_theory h U.Label.AC sigma in
 let a,sigma = Sig.create_fun 0 "a" sigma in
 let g,sigma = Sig.create_fun 1 "g" sigma in
 let k,sigma = Sig.create_fun 2 "k" sigma in
 let k,sigma = Sig.set_theory k U.Label.AC sigma in
 let b,sigma = Sig.create_fun 0 "b" sigma in
 let one = let mx1 = T.Fun(k, [T.Fun(b,[]);x]) in eqn mx1 x in
  let dist =
   let mpxyz = T.Fun(k,[z;T.Fun(h,[x;y])]) in
   let pmxzmyz = T.Fun(h,[T.Fun(k,[z;x]);T.Fun(k,[z;y])]) in
   eqn mpxyz pmxzmyz in
 let uniteq = eqn (T.Fun(h,[x;T.Fun(a,[])])) x in
 let acu = [uniteq] in
 let inv = eqn (T.Fun(h,[T.Fun(g,[x]);x])) (T.Fun(a,[])) in
 let group = [uniteq;inv] in
 let ring = [one; dist;uniteq;inv] in 
 (* *)
 let run_check eqs = Either.right (M.run sigma (
  Find.largest eqs >>= function (t,trs,s) -> 
   let ts = SymSub.to_string t in
   Trs.to_stringm trs >>= fun trss ->
   Format.printf "Theory %s matches\n%s\n%s\n%!" s ts trss; return s
 )) in
 run_check acu;
 run_check group;
 run_check ring;
;;

(*test ()*)
