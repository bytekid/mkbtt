(**
@author Martin Korp, Sarah Winkler
@since  Thu Feb 19 13:28:01 CET 2008
*)

(** Slothrop-like completion *)

(*** TYPES ***************************************************************)
(*** EXCEPTIONS **********************************************************)
(*** SUBMODULES **********************************************************)
(*** GLOBALS *************************************************************)
let options = ref Completion.default_options;;

(*** FUNCTIONS ***********************************************************)

let complete eqs o =
 options := o;
 Format.printf "Oh, it's a fake!i\n";
 Setx.empty ()
;;
