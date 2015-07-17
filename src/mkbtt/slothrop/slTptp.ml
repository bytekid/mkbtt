open SlTerms

type clause_status_t = 
    Axiom | Hypothesis | Conjecture
;;

type formula_t = 
    Equal of term * term
  | DisEqual of term * term
;;

type clause_t = 
    InputClause of string * clause_status_t * formula_t list
;;

