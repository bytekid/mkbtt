
(*constants*)
let don = ref false;;
let t0 = ref (Unix.gettimeofday ());;

(*functions*)
let eprintf s = 
 Format.eprintf "--- %s (%f)@\n%!" s (Unix.gettimeofday () -. !t0);;

let debug s = if !don then eprintf s;;
let force s = eprintf s;;

let on () = 
 (* debug "debugging on"; *)
 don := true;
;;

let off () = 
 (* debug "debugging off"; *)
 don := false;
;;

