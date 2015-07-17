let debug_learning = ref false;;
let debug_checker = ref (false);;
let debug_rewriting = ref (false);;

let completion_mode = ref false;;
let refutation_mode = ref false;;
let find_all_completions = ref false;;

let timeout = ref 0;;
let timed_out = ref false;;

let trust_axiom_orientation = ref false;;

let detect_divergence = ref false;;
let max_exp_multiplier = ref 3;;
let max_exp_threshold = ref 10;;
