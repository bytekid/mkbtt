
open Interface;;

let test () =
 Format.printf "foo\n%!";
 let s = get_solver () in
 add s (Array.of_list [1;]) 1;
 (*add s (Array.of_list [-1;]) 1; *)
 let _ = match is_sat s with
  | 0 -> Format.printf "no\n%!";
  | _ -> Format.printf "yes\n%!";
 in
 ()
;;

test ();;
