(* Copyright 2008 Martin Korp, Christian Sternagel, Harald Zankl
 * GNU Lesser General Public License
 *
 * This file is part of TTT2.
 * 
 * TTT2 is free software: you can redistribute it and/or modify it under
 * the terms of the GNU Lesser General Public License as published by the
 * Free Software Foundation, either version 3 of the License, or (at your
 * option) any later version.
 * 
 * TTT2 is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public
 * License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public
 * License along with TTT2. If not, see <http://www.gnu.org/licenses/>.
 *)

(*** MODULES ******************************************************************)
module Ass = Assignment;;
open Monad;;
open Formula;;
open Util;;

(*** FUNCTIONS ****************************************************************)
(* replace by new process module begin *)
open Util;;

exception Stop;;

let set_signals h = 
 Sys.set_signal Sys.sigterm h; Sys.set_signal Sys.sigalrm h;
;;

let child dout f = 
 set_signals Sys.Signal_default;
 let chout = Unix.out_channel_of_descr dout in
 set_binary_mode_out chout true;
 let r = f () in
 Marshal.to_channel chout (Unix.getpid ()) [];
 Marshal.to_channel chout r [Marshal.Closures];
 close_out chout;
 exit 0
;;

let fork_pipe f =
 let (din,dout) = Unix.pipe () in
 match Unix.fork () with
  | 0 -> Unix.close din; child dout f;
  | pid -> Unix.close dout; (pid,din)
;;

let create f ps = fork_pipe f::ps;;

let rec waitpid pid =
 try ignore (Unix.waitpid [] pid)
 with Unix.Unix_error (Unix.EINTR,_,_) -> waitpid pid
;;

let killpid pid = 
 (try Unix.kill pid Sys.sigterm with Unix.Unix_error (Unix.ESRCH,_,_) -> ());
 waitpid pid;
;;

let kill (pid,din) = 
 killpid pid;
 Unix.close din;
;;

let kill_all ps = List.iter kill ps;;
let kill_by_descr ps r = 
 kill (List.find ((=) r <.> snd) ps);
 (* Unix.close r; *)
;;

let signal = ref 0;;

(* set handler at exit to what *)
let read ps =
 try
  set_signals (Sys.Signal_handle (fun _ -> raise Stop));
  if !signal = 1 then raise Stop;
  let r =
   try 
   let (f,_,_) = (Unix.select (List.map snd ps) [] [] ~-.1.0) in
   List.hd f
   with Unix.Unix_error (Unix.EINTR,_,_) -> raise Stop
  in
  let chin = Unix.in_channel_of_descr r in
  try
   let pid = Marshal.from_channel chin in
   let result = Marshal.from_channel chin in
   kill (pid,r);
   set_signals (Sys.Signal_handle (fun _ -> signal := 1));
   (List.filter ((<>) pid <.> fst) ps, Some result)
  with End_of_file -> 
   set_signals (Sys.Signal_handle (fun _ -> signal := 1));
   kill_by_descr ps r;
   (List.filter ((<>) r <.> snd) ps, None);
 with
 Stop -> kill_all ps; ([], None)
;;

let rec run_parallel acc ps n fs = 
 (* Format.printf "%d of %d active, %d remaining@\n%!" (List.length ps) n
 (List.length fs);  *)
 (* everything computed *)
 if ps = [] && fs = [] then acc 
 (* waiting for result *)
 else if List.length ps >= n || fs = [] then 
  let (ps,r) = read ps in 
 match r with 
   | None -> acc 
   | Some r -> run_parallel (r::acc) ps n fs
 (* one core vacant *)
 else 
  let ps = create (List.hd fs) ps in 
  run_parallel acc ps n (List.tl fs)
;;

let run_parallel n fs = 
 set_signals (Sys.Signal_handle (fun _ -> signal := 1));
 run_parallel [] [] n fs
;;

let create_process cmd args din dout derr = 
 let t0 = Unix.gettimeofday () in
 let pid = Unix.create_process cmd args din dout derr in
 (try
  if !signal = 1 then raise Stop;
  set_signals (Sys.Signal_handle (fun _ -> raise Stop));
  waitpid pid;
 set_signals Sys.Signal_default;
 with Stop -> 
 set_signals Sys.Signal_default;
 killpid pid);
 (* Unix.close din; *)
 (* Unix.close dout; *)
 (* Unix.close derr; *)
 let t1 = Unix.gettimeofday () in
 (t1 -. t0)
;;


(* replace by new process module end *)
(* replac eby new scheduler module begin *)
(*
 read first line from buffer (including \n, if present)
 return string without \n
*)
let read_line dout = 
 let s = String.create 1 and line = Buffer.create 1 and n = ref 1 in
 try
 while !n <> 0 do
  n := Unix.read dout s 0 1;
  if !n = 0 then failwith "empty";
  if s = "\n" then n := 0 else Buffer.add_string line s;
 done;
 Buffer.contents line
 with
  Failure "empty" -> if Buffer.length line = 0 then "" else
  Buffer.contents line
;;
(* replace by new scheduler module begin *)

let wperm = 0o644;;
let wflags = [Unix.O_CREAT;Unix.O_WRONLY;Unix.O_TRUNC];;

(* monadic if then else *)
let mite mx ma mb = mx >>= fun x -> if x then ma else mb;;

let rat_of_string c = match String.split ~d:"/" c with
 | [n] -> (Integer.of_string n,Integer.one)
 | [n;d] -> (Integer.of_string n,Integer.of_string d)

let rat_of_real_string c = match String.split ~d:"sqrt(2)" c with
 | [] -> Rational.one
 | [c] -> rat_of_string c

let number_of_string v = 
 (* Format.eprintf "v = %s@\n%!" v; *)
 match String.split ~d:"+" v with
  | [c] -> 
   (try (uncurry Number.of_64_rat (rat_of_string c))
   with _ -> Number.of_64_real Rational.zero (rat_of_real_string c))
  | [c;d] -> Number.of_64_real (rat_of_string c) (rat_of_real_string d)
;;

let id_of_name x = int_of_string (String.sub x 1 (String.length x - 1));;

let add l ass = 
 let [x;_;v] = String.split l in
 let x = id_of_name x in
 try
  Ass.add_p (P x) (bool_of_string v) ass
 with Invalid_argument "bool_of_string" ->
  Ass.add_a ((arith x)) (number_of_string v) ass;;

let rec read_ass dout =
 let l = read_line dout in
 if l = "" then Ass.empty else add l (read_ass dout)
;;

let read_answer dout =
 let answer = read_line dout in
 if answer = "sat" then Some (read_ass dout) else None
;;

let solve flags f = 
 (* Format.eprintf "flags = %s@\n%!" (List.join id "," flags); *)
 let fname = Filename.temp_file "ttt" ".smt" in
 (* Format.eprintf "file = %s@\n%!" fname; *)
 let file = Unix.openfile fname wflags wperm in
 let cout = Unix.out_channel_of_descr file in
 let ppf = Format.formatter_of_out_channel cout in
 Format.fprintf ppf "%a%!" (F.fprintf_smt ~logic:"QF_NIA") f;
 Unix.close file;
 let (dout,din) = Unix.pipe () in
 let flags = "minismt"::"-neg"::"-m"::fname::"-t"::"3600"::flags in
 ignore (create_process "./minismt" (Array.of_list flags) Unix.stdin din Unix.stderr);
 Unix.close din;
 let answer = read_answer dout in
 Unix.close dout;
 return answer
;;

let rec ea a ass = match a with
 | A l         -> return (try Ass.find_a (A l) ass with | Not_found -> Number.zero)
 | C r         -> return r
 | Fresh a     -> eval_a a ass
 | Add (a,b)   -> lift2 Number.add (eval_a a ass) (eval_a b ass)
 | Sub (a,b)   -> lift2 Number.sub (eval_a a ass) (eval_a b ass)
 | Mul (a,b)   -> lift2 Number.mul (eval_a a ass) (eval_a b ass)
 | Ite (x,a,b) -> mite (eval_p x ass) (eval_a a ass) (eval_a b ass)
 | Min (a,b)   -> lift2 Number.min (eval_a a ass) (eval_a b ass)
 | Max (a,b)   -> lift2 Number.max (eval_a a ass) (eval_a b ass)
and ep f ass = match f with
 | Top       -> return true
 | Bot       -> return false
 | P _       -> return (try Assignment.val_of f ass with | Not_found -> false)
 | Not x     -> lift not (eval_p x ass)
 | And (x,y) -> lift2 (&&) (eval_p x ass) (eval_p y ass)
 | Or (x,y)  -> lift2 (||) (eval_p x ass) (eval_p y ass)
 | Iff (x,y) -> lift2 (=) (eval_p x ass) (eval_p y ass) 
 | Eq (a,b)  -> lift2 Number.eq (eval_a a ass) (eval_a b ass)
 | Gt (a,b)  -> lift2 Number.gt (eval_a a ass) (eval_a b ass)
 | Ge (a,b)  -> lift2 Number.ge (eval_a a ass) (eval_a b ass)
(*
and eval_a a ass = get >>= fun s -> cache s.State.ea_tbl (flip ea ass) a
and eval_p p ass = get >>= fun s -> cache s.State.ep_tbl (flip ep ass) p
*)
and eval_a a ass = ea a ass
and eval_p p ass = ep p ass
;;

