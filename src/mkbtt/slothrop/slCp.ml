open SlTerms
open List


(* maxindex: term -> int *)
let rec maxindex = function
    (V(x,i))  -> i
  | (T(_,ts)) -> maxs(List.map maxindex ts);;

(* rename: int -> term -> term *)
let rec rename n = function
    (V(x,i))  -> V(x,i+n)
  | (T(f,ts)) -> T(f, List.map (rename n) ts);;

(* _CP: (term -> term) -> term * term -> term * term -> (term * term) list *)
let _CP _C (t,r) (l2,r2) = 
  match unify(t,l2) with 
      Some s -> 
            let sigma = lift s in 
              [(sigma r, sigma(_C r2))]
    | None -> []
;;

(* _CP: (term -> term) -> term * term -> term * term -> (term * term) list *)
let _CP_new _C (t,r) (l2,r2) = 
  match unify(t,l2) with 
      Some s -> 
            let sigma = lift s in 
              Some(sigma r, sigma(_C r2))
    | None -> None
;;

(* _CPs: (term * term) list -> term * term -> (term * term) list *)


(* let critical_pairs_old _R1 _R2 =  *)
(*   let _critical_pairs _R (l,r) = *)
(*     let rec cps _C = function *)
(*         (V _, _) -> [] *)
(*       | (T(f,ts),r) -> *)
(*           List.concat *)
(*             (List.map (_CP _C (T(f,ts),r)) _R) @ (innercps _C (f,[],ts,r)) *)
(*     and innercps _C = function *)
(*         (_, _, [], _) -> [] *)
(*       | (f, ts0, t::ts1, r) -> *)
(*           let rec _Cf s = _C(T(f, ts0 @ [s] @ ts1)) in  *)
(*             (cps _Cf (t,r)) @ (innercps _C (f, ts0 @ [t], ts1, r))  *)
(*     in *)
(*     let m =  *)
(*       maxs (List.map  *)
(*               (fun (l,r) -> max (maxindex l) (maxindex r)) _R) + 1  *)
(*     in *)
(*       cps (fun t -> t) (rename m l, rename m r)  *)
(*   in *)
(*     List.concat *)
(*       (List.map (_critical_pairs _R1) _R2) *)
(* ;; *)

let critical_pairs _R1 _R2 = 
  let _critical_pairs _R (l,r) acc =
    let rec cps _C acc = function
        (V _, _) -> acc
      | (T(f,ts),r) ->
          let toplevel_cps = List.fold_left 
            (fun a b -> 
               match _CP_new _C (T(f,ts),r) b with
                   Some(pair) -> pair::a
                 | None -> a) 
            acc _R
          in
            innercps _C (f,[],ts,r,toplevel_cps)
    and innercps _C = function
        (_, _, [], _,acc) -> acc
      | (f, ts0, t::ts1, r,acc) ->
          let _Cf s = 
            _C (T(f, ts0 @ [s] @ ts1)) 
          in 
            innercps _C (f, ts0 @ [t], ts1, r, (cps _Cf acc (t,r))) 
    in
    let m = 1 + maxs 
      (List.map (fun (l,r) -> max (maxindex l) (maxindex r)) _R)
    in
      
      cps (fun t -> t) acc (rename m l, rename m r) 
  in
    List.fold_left (fun acc b -> _critical_pairs _R1 b acc) [] _R2
;;


