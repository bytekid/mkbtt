let (>>=) = Monad.(>>=);;
type t = unit Monad.t;;

let rec seq fs fmt = match fs with
  | []    -> Monad.return ()
  | f::fs -> f fmt >>= fun _ -> seq fs fmt
;;

let node s fs fmt =
  Format.fprintf fmt "@{<%s>" s;
  seq fs fmt >>= fun _ ->
  Monad.return (Format.fprintf fmt "@}")
;;

let int s i = node s [fun fmt -> Monad.return (Format.fprintf fmt "%d" i)];;

let bool s b = node s [fun fmt -> Monad.return (Format.fprintf fmt "%B" b)];;

let string s t = node s [fun fmt -> Monad.return (Format.fprintf fmt "%s" t)];;

let leaf s fmt = Monad.return (Format.fprintf fmt "<%s/>" s);;

let id _ = Monad.return ();;
