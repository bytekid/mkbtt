module UM = U.Monad;;
module T = U.Term;;
module List = Util.List;;
open UM;;
let (<.>) = Util.(<.>);;

let solve mx ss ts =
 if (List.is_empty ss) || (List.is_empty ts) then return [] else (
 let a = BinMatrixSolver2.solve mx ss ts in
 let a' = BinMatrixSolver3.solve mx ss ts in
 let cmp l l' = if List.equal l l' then 0 else compare l l' in
 (if not (List.equal ~c:cmp a a') then (
  map T.to_stringm ss >>= fun sss ->
  let sss = List.foldl (^) "" sss in
  map T.to_stringm ts >>= fun tss ->
  let tss = List.foldl (^) "" tss in
  Format.printf "Unify %s %i and %s %i\n%!" sss (List.length ss) tss (List.length ts);
  let to_str = UM.find_var_name in
  let print_listm = UM.foldl (fun s v -> to_str v >>= fun vn -> return (s^" "^vn)) "\n" in
  UM.map print_listm a >>= fun l ->
  let s = List.foldl (^) "" l in
  Format.printf "There are %i assignments:\n%s\n%!" (List.length l) s;
  UM.map print_listm a' >>= fun l ->
  let s = List.foldl (^) "" l in
  Format.printf "New solver: %i assignments:\n%s\n%!" (List.length l) s;
  let ml =  Array.to_list (Array.map Array.to_list mx) in
  let oto_str = function Some v -> UM.find_var_name v | _ -> return "0" in
  let print_olistm = UM.foldl (fun s v -> oto_str v >>= fun vn -> return (s^" "^vn)) "\n" in
  UM.foldl (fun s v -> print_olistm v >>= fun vn -> return (s^" "^vn)) "" ml >>= fun s ->
  Format.printf "Matrix is \n%s\n%!" s;
  assert (List.length a = (List.length a'));
  return ()
  ) else return ()) >>= fun _ ->
 return a
 )
;;

