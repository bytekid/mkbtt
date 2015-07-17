open SlPrint
open SlDebug
open SlRewriting

let true_s =  "**** CONJECTURE: TRUE ****"
let false_s = "**** CONJECTURE: FALSE ***"
let stars_s = "**************************"

let decide_conjecture dp (b,(s,t)) =
  let s' = normalize dp s in
  let t' = normalize dp t in
    if s' = t' then
      begin
        if b then
          alert true_s
        else
          alert false_s;
        alert ((string_of_rewrite s s') ^ " = " ^ 
                         (string_of_rewrite_inv t t'));
      end
    else
      begin
        if b then
          alert false_s
        else
          alert true_s;
        alert ((string_of_rewrite s s') ^ " != " ^ 
                         (string_of_rewrite_inv t t'));
      end;
    alert stars_s
;;

let rec semidecide_conjectures rules = function
    [] -> []
  | ((b,(s,t)) as conj)::cs -> 
      let s' = normalize rules s in
      let t' = normalize rules t in
        if s' = t' then
          begin
            if b then
              alert true_s
            else
              alert false_s;
            alert ((string_of_rewrite s s') ^ " = " ^ 
                             (string_of_rewrite_inv t t')) ;
            alert stars_s;
            semidecide_conjectures rules cs
          end
        else
          conj::(semidecide_conjectures rules cs)
;;


