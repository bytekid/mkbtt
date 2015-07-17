type checker_status = 
    TERM_YES | TERM_NO | TERM_UNKNOWN
;;

let string_of_checker_status = function
    TERM_YES -> "YES"
  | TERM_NO -> "NO"
  | TERM_UNKNOWN -> "UNKNOWN"
;;
