type verbosity_t = 
    QUIET | INFO | DEBUG | TRACE;;

let verbosity = ref QUIET;;

let parse_verbosity = function
    "quiet" | "0" -> QUIET
  | "info" | "1" -> INFO
  | "debug" | "2" | "" -> DEBUG
  | "trace" | "3" -> TRACE
  | x -> failwith ("Unable to parse debug level: " ^ x)
;;

let set_verbosity s = 
  verbosity := (parse_verbosity s)
;;

let should_display msg_level = 
  match msg_level,(!verbosity) with 
      QUIET,_ -> true
    | INFO,QUIET -> false
    | INFO,_ -> true
    | DEBUG,QUIET | DEBUG,INFO -> false
    | DEBUG,_ -> true
    | TRACE,TRACE -> true
    | TRACE,_ -> false
;;

let display_fun level = 
  match level with 
      INFO -> print_endline
    | _ -> prerr_endline
;;

let maybe_debug level msg = 
  if should_display level then
    let print = display_fun level in
      print msg
;;
  
let debug_if level b msg = 
  if b then maybe_debug level msg
;;

let debug_l level f = 
  if should_display level then 
    let print = display_fun level in
      print (Lazy.force f)
;;

let debug_if_l level b f = 
  if b then debug_l level f
;;

let alert msg = maybe_debug QUIET msg;; 
let error msg = maybe_debug QUIET ("ERROR:\n" ^ msg);; 
let info msg = maybe_debug INFO msg;; 
let debug msg = debug_l DEBUG msg;; 
let trace msg = debug_l TRACE msg;; 

