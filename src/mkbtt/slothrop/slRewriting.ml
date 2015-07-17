open SlPrefs
open SlDebug
open SlTerms
open SlPrint

(* rewrite: (term * term) list -> term -> term *)
let rec rewrite _l t = 
  match _l with 
      [] -> None
    | ((l,r)::_R) -> 
        match match_with(l,t) with
            None -> rewrite _R t
          | Some s -> 
              let s' = lift s r in
                Some s'
;;

let rec norm _R t =
  match t with 
      V x -> V x
    | (T(f,ts)) ->
        let u = T(f, List.map (norm _R) ts) in 
          match rewrite _R u with 
            | None -> u
            | Some u' -> norm _R u'
;;

let normalize _R t = 
  debug_if_l TRACE (!debug_rewriting) (lazy "Normalizing with rules: "); 
  debug_if_l TRACE (!debug_rewriting) (lazy (string_of_trs _R)); 
  let t' = norm _R t in
    debug_if_l TRACE (!debug_rewriting) 
      (lazy ((string_of_term t) ^ " -->! " ^ (string_of_term t')));
    t'
;;
