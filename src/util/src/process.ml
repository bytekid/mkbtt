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

(*** OPENS ********************************************************************)
open Prelude;;

(*** TYPES ********************************************************************)
type time = Global of float | Local of float;;

(*** EXCEPTIONS ***************************************************************)
exception Stop;;

(*** FUNCTIONS ****************************************************************)
let set_signals h = Sys.set_signal Sys.sigterm h; Sys.set_signal Sys.sigalrm h;;

(* Parallel Evaluations *)
let child t w f i =
 set_signals Sys.Signal_default;
 ignore (Unix.write w "sync" 0 4);
 let t = Option.fold (max 0.001 <.> flip (-.) (Unix.gettimeofday ())) 0.0 t in
 if t <= 0.0015 && t >= 0.0005 then exit 0;
 let status = {Unix.it_interval = 0.0; Unix.it_value = t} in
 ignore (Unix.setitimer Unix.ITIMER_REAL status);
 let chout = Unix.out_channel_of_descr w in
 set_binary_mode_out chout true;
 let fs = format_of_string "failure `%s' in subprocess %i@\n%!" in
 let error e = Format.eprintf fs (Printexc.to_string e) (Unix.getpid ()) in
 let r = try Some (f i) with e -> error e; None in
 let status = Unix.getitimer Unix.ITIMER_REAL in
 ignore (Unix.setitimer Unix.ITIMER_REAL {status with Unix.it_value = 0.0});
 Marshal.to_channel chout (Unix.getpid ()) [];
 Marshal.to_channel chout r [Marshal.Closures];
 Marshal.to_channel chout (status.Unix.it_value) [Marshal.Closures];
 close_out chout;
 exit 0
;;

let create_processes t fs i =
 Listx.foldl (fun (pids,rs) f ->
  let (r,w) = Unix.pipe () in
  match Unix.fork () with
   | 0 -> Unix.close r; child t w f i
   | pid -> Unix.close w; (pid::pids, r::rs)) ([],[]) fs
;;

let rec read_sync rs n signal = match rs with
 | [] -> ()
 | r::rs ->
  (try ignore (Unix.read r (String.create n) 0 n); read_sync rs n signal
   with Unix.Unix_error (Unix.EINTR,_,_) -> read_sync (r::rs) n signal);
;;

let kill pids =
 let rec waitpid pid =
  try ignore (Unix.waitpid [] pid)
  with Unix.Unix_error (Unix.EINTR,_,_) -> waitpid pid
 in
 let kill pid =
  (try Unix.kill pid Sys.sigterm with Unix.Unix_error (Unix.ESRCH,_,_) -> ());
  waitpid pid
 in
 Listx.iter kill pids
;;

let parallel t collect_results fs i =
 let signal = ref 0 in
 set_signals (Sys.Signal_handle (fun _ -> signal := 1));
 let (pids,rs) = create_processes t fs i in
 let r =
  try
   read_sync rs 4 signal;
   set_signals (Sys.Signal_handle (fun _ -> raise Stop));
   if !signal = 1 then raise Stop;
   let r = collect_results pids rs in
   set_signals (Sys.Signal_handle (fun _ -> signal := 1)); Some r
  with Stop -> set_signals Sys.Signal_ignore; None
 in 
 kill pids; Listx.iter Unix.close rs;
 match r with 
  | None -> exit 0 
  | Some r -> 
   set_signals Sys.Signal_default; 
   if !signal = 1 then exit 0 else r
;;

let read_result rs =
 if rs = [] then failwith "no channel to read from" else
  let r =
   try Listx.hd (Triple.fst (Unix.select rs [] [] ~-.1.0))
   with Unix.Unix_error (Unix.EINTR,_,_) -> raise Stop
  in
  let chin = Unix.in_channel_of_descr r in
  set_binary_mode_in chin true;
  let rs = Listx.filter ((<>) r) rs in
  try
   let pid = Some (Marshal.from_channel chin) in
   let result = Marshal.from_channel chin in
   let t = Marshal.from_channel chin in
   ((pid,(result,t)),rs)
  with End_of_file -> ((None,(None,0.0)),rs)
;;

let collect_results pids rs =
 let rec collect_results results rs =
  if rs = [] then
   let assoc pid = Listx.assoc (Some pid) in
   let assoc pid = catch Not_found (assoc pid) (None,0.0) results in
   Listx.rev_map assoc pids
  else
   let (r,rs) = read_result rs in
   collect_results (r :: results) rs
 in
 collect_results [] rs
;;

let filter_result p pids rs = 
 let rec filter_result = function
  | [] -> (None,0.0)
  | rs ->
   let ((_,r),rs) = read_result rs in
   if Option.fold p false (fst r) then r else filter_result rs
 in
 filter_result rs
;;

let run_parallel fs i = Listx.map fst (parallel None collect_results fs i);;
let run_parallel_until p fs i = fst (parallel None (filter_result p) fs i);;

(* Timed Evaluations *)
let run_timed t f i =
 let t = Some (match t with Global t -> t | Local t -> Unix.gettimeofday () +. t) in
 parallel t (filter_result (const true)) [f] i
;;

let parallel _ = failwith "run_parallel2: not implemented";
