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

(* Specifying a new processor:
 * If you want to specify the information of a new processor you have modify
 * on the on hand the type [t] and on the other hand the functions [equal],
 * [derivational], [runtime], [fprintf], and [fprintfx]. In order to be able
 * to do that you have your processor module has to provide a type [t] which
 * defines the type of the output of your processor as well as the functions
 * [derivational], [runtime], [fprintf] and [fprintfx]:
 * - The type [t] is used to define the output of your processor. So, assuming
 *   that your processor has the name <proc> and is implemented in a module
 *   called <Proc>, you have to add the line [P_<proc> of <Proc>.t] to type [t].
 * - The function [equal] checks whether two given processors are similar.
 *   To modify the function you have to add the line
 *   [P_<proc> p, P_<proc> q -> <Proc>.equal p q].
 * - The functions [derivational] and [runtime] define the complexity bound
 *   induced by your processor. To modify the functions add the line
 *   [P_<proc> p -> <Proc>.derivational p] to the function [derivational] and
 *   the line [P_<proc> p -> <Proc>.runtime p] to the function [runtime].
 * - The functions [fprintf] and [fprintfx] are used to print the proofs of the
 *   registerd processors. To modify the functions add the line
 *   [P_<proc> p -> <Proc>.fprintf fmt p] to the function [fprintf] and
 *   the line [P_<proc> p -> printx <Proc>.fprintfx fmt p] to the function
 *   [fprintfx].
 *)

(*** OPENS ********************************************************************)
open Util;;
open Processors;;
open Modifier;;
open Nontermination;;
open Rewritingx;;
open Termination;;
open Transformation;;

(*** TYPES ********************************************************************)
module M = Monad;;
module SL = SemanticLabeling;;
module SC = SubtermCriterion;;
module SCT = SizeChangeTermination;;
module RLab = RootLabeling;;
module QLab = QuasiRootLabeling;;
module Ti = TypeIntroduction;;

(*** TYPES ********************************************************************)
type t =
 (* modifiers *)
 | P_restore of Restore.t
 (* nontermination processors *)
 | P_con of Contained.t
 | P_loop of LoopSat.t
 | P_unfold of Unfolding.t
 | P_var of Variables.t
 (* termination processors *)
 | P_adg of Dg.Adg.t
 | P_arctic of Arctic.t
 | P_bounds of Bounds.t
 | P_cdg of Dg.Cdg.t
 | P_csf of Csf.t 
 (* FIXME delete processor P_cf *)
 | P_ncf of Confluence.Nonconfluence.t
 | P_rl of Confluence.RuleLabeling.t
 | P_shift of Confluence.Shift.t
 (* FIXME delete processor P_cf *)
 | P_acdg of Dg.Acdg.t
 | P_dp of Dp.t
 | P_edg of Dg.Edg.t
 | P_lpo of Lpo.t
 | P_kbo of Kbo.t
 | P_tkbo of Tkbo.t
 | P_acrpo of Acrpo.t
 | P_ackbo of Ackbo.t
 | P_udpac of Udpac.t
 | P_fbi of Fbi.t
 | P_matrix of Matrix.t
 | P_odg of Dg.Odg.t
 | P_poly of Poly.t
 | P_sc of SC.t
 | P_sccs of Sccs.t
 | P_sct of SCT.t
 | P_sl of SL.t
 | P_trivial of Trivial.t
 | P_tdg of Dg.Tdg.t
 (* transformation processors *)
 | P_cp of Cp.t
 | P_dpify of Dpify.t
 | P_dup of Dup.t
 | P_linear of Linear.t
 | P_qlab of QLab.t
 | P_ref of Reflect.t
 | P_rev of Reverse.t
 | P_rlab of RLab.t
 | P_rt of Rt.t
 | P_split of Split.t
 | P_st of St.t
 | P_star of Star.t
 | P_ti of Ti.t
 | P_uncurry of Uncurry.t
 | P_uncurryx of Uncurryx.t
 | P_ur of Ur.t
;;

(*** FUNCTIONS ****************************************************************)
let (>>=) = M.(>>=);;

(* Access Functions *)
let input = function
 (* modifiers *)
 | P_restore p -> let ips = Restore.get_ips p in [fst ips;snd ips]
 (* nontermination processors *)
 | P_con p -> [Contained.get_ip p]
 | P_loop p -> [LoopSat.get_ip p]
 | P_unfold p -> [Unfolding.get_ip p]
 | P_var p -> [Variables.get_ip p]
 (* termination processors *)
 | P_adg p -> [Dg.Adg.get_ip p]
 | P_arctic p -> [Arctic.get_ip p]
 | P_bounds p -> [Bounds.get_ip p]
 | P_cdg p -> [Dg.Cdg.get_ip p]
 (* FIXME delete processor P_cf *)
 | P_ncf p -> [Confluence.Nonconfluence.get_ip p]
 | P_rl p -> [Confluence.RuleLabeling.get_ip p]
 | P_shift p -> [Confluence.Shift.get_ip p]
 (* FIXME delete processor P_cf *)
 | P_acdg p -> [Dg.Acdg.get_ip p]
 | P_dp p -> [Dp.get_ip p]
 | P_edg p -> [Dg.Edg.get_ip p]
 | P_lpo p -> [Lpo.get_ip p]
 | P_acrpo p -> [Acrpo.get_ip p]
 | P_ackbo p -> [Ackbo.get_ip p]
 | P_kbo p -> [Kbo.get_ip p]
 | P_matrix p -> [Matrix.get_ip p]
 | P_fbi p -> [Fbi.get_ip p]
 | P_poly p -> [Poly.get_ip p]
 | P_sc p -> [SC.get_ip p]
 | P_sccs p -> [Sccs.get_ip p]
 | P_sct p -> [SCT.get_ip p]
 | P_sl p -> [SL.get_ip p]
 | P_tdg p -> [Dg.Tdg.get_ip p]
 | P_trivial p -> [Trivial.get_ip p]
 | P_udpac p -> [Udpac.get_ip p]
 (* transformation processors *)
 | P_cp p -> [Cp.get_ip p]
 | P_dpify p -> [Dpify.get_op p]
 | P_dup p -> [Dup.get_op p]
 | P_linear p -> [Linear.get_ip p]
 | P_qlab p -> [QLab.get_ip p]
 | P_ref p -> [Reflect.get_ip p]
 | P_rev p -> [Reverse.get_ip p]
 | P_rlab p -> [RLab.get_ip p]
 | P_rt p -> [Rt.get_ip p]
 | P_split p -> [Split.get_ip p]
 | P_st p -> [St.get_ip p]
 | P_star p -> [Star.get_ip p]
 | P_ti p -> [Ti.get_ip p]
 | P_uncurry p -> [Uncurry.get_ip p]
 | P_uncurryx p -> [Uncurryx.get_ip p]
 | P_ur p -> [Ur.get_ip p]
;;

let output = function
 (* modifiers *)
 | P_restore p -> [Restore.get_op p]
 (* termination processors *)
 | P_adg p -> [Dg.Adg.get_op p]
 | P_arctic p -> [Arctic.get_op p]
 | P_bounds p -> [Bounds.get_op p]
 | P_cdg p -> [Dg.Cdg.get_op p]
 (* FIXME delete processor P_cf *)
 | P_ncf p -> [Confluence.Nonconfluence.get_op p]
 | P_rl p -> [Confluence.RuleLabeling.get_op p]
 | P_shift p -> [Confluence.Shift.get_op p]
 (* FIXME delete processor P_cf *)
 | P_acdg p -> [Dg.Acdg.get_op p]
 | P_dp p -> [Dp.get_op p]
 | P_edg p -> [Dg.Edg.get_op p]
 | P_lpo p -> [Lpo.get_op p]
 | P_acrpo p -> [Acrpo.get_op p]
 | P_ackbo p -> [Ackbo.get_op p]
 | P_kbo p -> [Kbo.get_op p]
 | P_matrix p -> [Matrix.get_op p]
 | P_fbi p -> [Fbi.get_op p]
 | P_poly p -> [Poly.get_op p]
 | P_sc p -> [SC.get_op p]
 | P_sccs p -> Sccs.get_ops p
 | P_sct p -> [SCT.get_op p]
 | P_sl p -> [SL.get_op p]
 | P_tdg p -> [Dg.Tdg.get_op p]
 | P_trivial p -> [Trivial.get_op p]
 (* transformation processors *)
 | P_cp p -> [Cp.get_op p]
 | P_dpify p -> [Dpify.get_op p]
 | P_dup p -> [Dup.get_op p]
 | P_linear p -> [Linear.get_op p]
 | P_qlab p -> [QLab.get_op p]
 | P_ref p -> [Reflect.get_op p]
 | P_rev p -> [Reverse.get_op p]
 | P_rlab p -> [RLab.get_op p]
 | P_rt p -> [Rt.get_op p]
 | P_split p -> Split.get_ops p
 | P_st p -> [St.get_op p]
 | P_star p -> [Star.get_op p]
 | P_ti p -> Ti.get_ops p
 | P_udpac p -> [Udpac.get_op p]
 | P_uncurry p -> [Uncurry.get_op p]
 | P_uncurryx p -> [Uncurryx.get_op p]
 | P_ur p -> [Ur.get_op p]
 (* nontermination processors *)
 | P_con _ | P_loop _ | P_unfold _ | P_var _ -> []
;;

(* Compare Functions *)
let equal p q = match (p,q) with
 (* modifiers *)
 | P_restore p, P_restore q -> Restore.equal p q
 (* nontermination processors *)
 | P_con p, P_con q -> Contained.equal p q
 | P_loop p, P_loop q -> LoopSat.equal p q
 | P_unfold p, P_unfold q -> Unfolding.equal p q
 | P_var p, P_var q -> Variables.equal p q
 (* termination processors *)
 | P_adg p, P_adg q -> Dg.Adg.equal p q
 | P_arctic p, P_arctic q -> Arctic.equal p q
 | P_bounds p, P_bounds q -> Bounds.equal p q
 | P_cdg p, P_cdg q -> Dg.Cdg.equal p q
 (* FIXME delete processor P_cf *)
 | P_ncf p, P_ncf q -> Confluence.Nonconfluence.equal p q
 | P_rl p, P_rl q -> Confluence.RuleLabeling.equal p q
 | P_shift p, P_shift q -> Confluence.Shift.equal p q
 (* FIXME delete processor P_cf *)
 | P_acdg p, P_acdg q -> Dg.Acdg.equal p q
 | P_dp p, P_dp q -> Dp.equal p q
 | P_edg p, P_edg q -> Dg.Edg.equal p q
 | P_lpo p, P_lpo q -> Lpo.equal p q
 | P_acrpo p, P_acrpo q -> Acrpo.equal p q
 | P_ackbo p, P_ackbo q -> Ackbo.equal p q
 | P_kbo p, P_kbo q -> Kbo.equal p q
 | P_tkbo p, P_tkbo q -> Tkbo.equal p q
 | P_matrix p, P_matrix q -> Matrix.equal p q
 | P_fbi p, P_fbi q -> Fbi.equal p q
 | P_odg p, P_odg q -> Dg.Odg.equal p q
 | P_poly p, P_poly q -> Poly.equal p q
 | P_sc p, P_sc q -> SC.equal p q
 | P_sccs p, P_sccs q -> Sccs.equal p q
 | P_sct p, P_sct q -> SCT.equal p q
 | P_sl p, P_sl q -> SL.equal p q
 | P_tdg p, P_tdg q -> Dg.Tdg.equal p q
 | P_trivial p, P_trivial q -> Trivial.equal p q
 (* transformation processors *)
 | P_cp p, P_cp q -> Cp.equal p q
 | P_dup p, P_dup q -> Dup.equal p q
 | P_linear p, P_linear q -> Linear.equal p q
 | P_qlab p, P_qlab q -> QLab.equal p q
 | P_ref p, P_ref q -> Reflect.equal p q
 | P_rev p, P_rev q -> Reverse.equal p q
 | P_rlab p, P_rlab q -> RLab.equal p q
 | P_rt p, P_rt q -> Rt.equal p q
 | P_split p, P_split q -> Split.equal p q
 | P_st p, P_st q -> St.equal p q
 | P_star p, P_star q -> Star.equal p q
 | P_ti p, P_ti q -> Ti.equal p q
 | P_udpac p, P_udpac q -> Udpac.equal p q
 | P_uncurry p, P_uncurry q -> Uncurry.equal p q
 | P_uncurryx p, P_uncurryx q -> Uncurryx.equal p q
 | P_ur p, P_ur q -> Ur.equal p q
 | _, _ -> false
;;

(* Complexity Bounds *)
let complexity c = function
 (* modifiers *)
 | P_restore p -> Some (Restore.complexity c p)
 (* termination processors *)
 | P_adg p -> Some (Dg.Adg.complexity c p)
 | P_arctic p -> Some (Arctic.complexity c p)
 | P_bounds p -> Some (Bounds.complexity c p)
 | P_cdg p -> Some (Dg.Cdg.complexity c p)
 | P_csf p -> Some (Csf.complexity c p)
 (* FIXME delete processor P_cf *)
 (* | P_ncf p -> Some (Confluence.complexity c p) *)
 (* | P_rl p -> Some (RuleLabeling.complexity c p) *)
 (* | P_shift p -> Some (Shift.complexity c p) *)
 (* FIXME delete processor P_cf *)
 | P_acdg p -> Some (Dg.Acdg.complexity c p)
 | P_dp p -> Some (Dp.complexity c p)
 | P_edg p -> Some (Dg.Edg.complexity c p)
 | P_lpo p -> Some (Lpo.complexity c p)
 | P_acrpo p -> Some (Acrpo.complexity c p)
 | P_ackbo p -> Some (Ackbo.complexity c p)
 | P_kbo p -> Some (Kbo.complexity c p)
 | P_matrix p -> Some (Matrix.complexity c p)
 | P_fbi p -> Some (Fbi.complexity c p)
 | P_odg p -> Some (Dg.Odg.complexity c p)
 | P_poly p -> Some (Poly.complexity c p)
 | P_sc p -> Some (SC.complexity c p)
 | P_sccs p -> Some (Sccs.complexity c p)
 | P_sct p -> Some (SCT.complexity c p)
 | P_sl p -> Some (SL.complexity c p)
 | P_tdg p -> Some (Dg.Tdg.complexity c p)
 | P_tkbo p -> Some (Tkbo.complexity c p)
 | P_trivial p -> Some (Trivial.complexity c p)
 (* transformation processors *)
 | P_cp p -> Some (Cp.complexity c p)
 | P_dpify p -> Some (Dpify.complexity c p)
 | P_dup p -> Some (Dup.complexity c p)
 | P_linear p -> Some (Linear.complexity c p)
 | P_qlab p -> Some (QLab.complexity c p)
 | P_ref p -> Some (Reflect.complexity c p)
 | P_rev p -> Some (Reverse.complexity c p)
 | P_rlab p -> Some (RLab.complexity c p)
 | P_rt p -> Some (Rt.complexity c p)
 | P_split p -> Some (Split.complexity c p)
 | P_st p -> Some (St.complexity c p)
 | P_star p -> Some (Star.complexity c p)
 | P_ti p -> Some (Ti.complexity c p)
 | P_udpac p -> Some (Udpac.complexity c p)
 | P_uncurry p -> Some (Uncurry.complexity c p)
 | P_uncurryx p -> Some (Uncurryx.complexity c p)
 | P_ur p -> Some (Ur.complexity c p)
 (* nontermination processors *)
 | _ -> None
;;

(* Printers *)
(* TXT printer *)
let fprintf fs fmt = function
 (* modifiers *)
 | P_restore p -> Restore.fprintf fs fmt p
 (* nontermination processors *)
 | P_con p -> Contained.fprintf fs fmt p
 | P_loop p -> LoopSat.fprintf fs fmt p
 | P_unfold p -> Unfolding.fprintf fs fmt p
 | P_var p -> Variables.fprintf fs fmt p
 (* termination processors *)
 | P_adg p -> Dg.Adg.fprintf fs fmt p
 | P_arctic p -> Arctic.fprintf fs fmt p
 | P_bounds p -> Bounds.fprintf fs fmt p
 | P_cdg p -> Dg.Cdg.fprintf fs fmt p
 | P_csf p -> Csf.fprintf fs fmt p
 (* FIXME delete processor P_cf *)
 | P_ncf p -> Confluence.Nonconfluence.fprintf fs fmt p
 | P_rl p -> Confluence.RuleLabeling.fprintf fs fmt p
 | P_shift p -> Confluence.Shift.fprintf fs fmt p
 (* FIXME delete processor P_cf *)
 | P_acdg p -> Dg.Acdg.fprintf fs fmt p
 | P_dp p -> Dp.fprintf fs fmt p
 | P_edg p -> Dg.Edg.fprintf fs fmt p
 | P_lpo p -> Lpo.fprintf fs fmt p
 | P_acrpo p -> Acrpo.fprintf fs fmt p
 | P_ackbo p -> Ackbo.fprintf fs fmt p
 | P_kbo p -> Kbo.fprintf fs fmt p
 | P_matrix p -> Matrix.fprintf fs fmt p
 | P_fbi p -> Fbi.fprintf fs fmt p
 | P_odg p -> Dg.Odg.fprintf fs fmt p
 | P_poly p -> Poly.fprintf fs fmt p
 | P_sc p -> SC.fprintf fs fmt p
 | P_sccs p -> Sccs.fprintf ~s:true fs fmt p
 | P_sct p -> SCT.fprintf fs fmt p
 | P_sl p -> SL.fprintf fs fmt p
 | P_tdg p -> Dg.Tdg.fprintf fs fmt p
 | P_tkbo p -> Tkbo.fprintf fs fmt p
 | P_trivial p -> Trivial.fprintf fs fmt p
 (* transformation processors *)
 | P_cp p -> Cp.fprintf fs fmt p
 | P_dpify p -> Dpify.fprintf fs fmt p
 | P_dup p -> Dup.fprintf fs fmt p
 | P_linear p -> Linear.fprintf fs fmt p
 | P_qlab p -> QLab.fprintf fs fmt p
 | P_ref p -> Reflect.fprintf fs fmt p
 | P_rev p -> Reverse.fprintf fs fmt p
 | P_rlab p -> RLab.fprintf fs fmt p
 | P_rt p -> Rt.fprintf fs fmt p
 | P_split p -> Split.fprintf fs fmt p
 | P_st p -> St.fprintf fs fmt p
 | P_star p -> Star.fprintf fs fmt p
 | P_ti p -> Ti.fprintf fs fmt p
 | P_udpac p -> Udpac.fprintf fs fmt p
 | P_uncurry p -> Uncurry.fprintf fs fmt p
 | P_uncurryx p -> Uncurryx.fprintf fs fmt p
 | P_ur p -> Ur.fprintf fs fmt p
;;

(* XML printer *)
let printx s f fs fmt p =
 if Status.is_nonterminating s then M.iter (fun f -> f fmt) fs else f fs fmt p
;;

let fprintfx s fs fmt = function
 (* modifiers *)
 | P_restore p -> printx s Restore.fprintfx fs fmt p
 (* nontermination processors *)
 | P_con p -> Contained.fprintfx fs fmt p
 | P_loop p -> LoopSat.fprintfx fs fmt p
 | P_unfold p -> Unfolding.fprintfx fs fmt p
 | P_var p -> Variables.fprintfx fs fmt p
 (* termination processors *)
 | P_adg p -> printx s Dg.Adg.fprintfx fs fmt p
 | P_arctic p -> printx s Arctic.fprintfx fs fmt p
 | P_bounds p -> printx s Bounds.fprintfx fs fmt p
 | P_cdg p -> printx s Dg.Cdg.fprintfx fs fmt p
 (* FIXME delete processor P_cf *)
 | P_ncf p -> printx s Confluence.Nonconfluence.fprintfx fs fmt p
 | P_rl p -> printx s Confluence.RuleLabeling.fprintfx fs fmt p
 | P_shift p -> printx s Confluence.Shift.fprintfx fs fmt p
 (* FIXME delete processor P_cf *)
 | P_acdg p -> printx s Dg.Acdg.fprintfx fs fmt p
 | P_dp p -> printx s Dp.fprintfx fs fmt p
 | P_edg p -> printx s Dg.Edg.fprintfx fs fmt p
 | P_lpo p -> printx s Lpo.fprintfx fs fmt p
 | P_acrpo p -> printx s Acrpo.fprintfx fs fmt p
 | P_ackbo p -> printx s Ackbo.fprintfx fs fmt p
 (* | P_kbo p -> printx s Kbo.fprintfx fs fmt p 
 | P_tkbo p -> printx s Tkbo.fprintfx fs fmt p *)
 | P_matrix p -> printx s Matrix.fprintfx fs fmt p
(* | P_fbi p -> printx s Fbi.fprintfx fs fmt p*)
 | P_odg p -> printx s Dg.Odg.fprintfx fs fmt p
 | P_poly p -> printx s Poly.fprintfx fs fmt p
 | P_sc p -> printx s SC.fprintfx fs fmt p
 | P_sccs p -> printx s Sccs.fprintfx fs fmt p
 | P_sct p -> printx s SCT.fprintfx fs fmt p
 (* | P_sl p -> printx s SL.fprintfx fs fmt p *)
 | P_tdg p -> printx s Dg.Tdg.fprintfx fs fmt p
 | P_trivial p -> printx s Trivial.fprintfx fs fmt p
 (* transformation processors *)
 | P_cp p -> printx s Cp.fprintfx fs fmt p
 | P_linear p -> printx s Linear.fprintfx fs fmt p
 | P_qlab p -> printx s QLab.fprintfx fs fmt p
 | P_ref p -> printx s Reflect.fprintfx fs fmt p
 | P_rev p -> Reverse.fprintfx fs fmt p
 | P_rlab p -> printx s RLab.fprintfx fs fmt p
 | P_rt p -> printx s Rt.fprintfx fs fmt p
 | P_split p -> printx s Split.fprintfx fs fmt p
 | P_st p -> printx s St.fprintfx fs fmt p
 | P_star p -> printx s Star.fprintfx fs fmt p
 (* | P_ti p -> printx s Ti.fprintfx fs fmt p *)
 | P_udpac p -> printx s Udpac.fprintfx fs fmt p
 | P_uncurry p -> printx s Uncurry.fprintfx fs fmt p
 | P_uncurryx p -> printx s Uncurryx.fprintfx fs fmt p
 | P_ur p -> printx s Ur.fprintfx fs fmt p
 | _ -> failwith "XML printing is not supported for this method"
;;
