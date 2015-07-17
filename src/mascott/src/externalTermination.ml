(* Copyright 2010 Sarah Winkler
 * GNU Lesser General Public License
 *
 * This file is part of MKBtt.
 * 
 * MKBtt is free software: you can redistribute it and/or modify it under
 * the terms of the GNU Lesser General Public License as published by the
 * Free Software Foundation, either version 3 of the License, or (at your
 * option) any later version.
 * 
 * MKBtt is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public
 * License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public
 * License along with MKBtt. If not, see <http://www.gnu.org/licenses/>.
 *)

(** check termiantion with external tool 
@author Sarah Winkler
@since  2008/02/19
*)

(** Calls external termination checker *)

(*** OPENS ********************************************************************)
open Util;;

(*** SUBMODULES **********************************************************)
module Var = Rewriting.Variable;;
module Fun = Rewriting.Function;;
module Term = U.Term;;
module Rule = U.Rule;;
module Trs = U.Trs;;
module Signature = U.Signature;;
module W = World;;
module Monad = W.Monad;;

(*** OPENS (2) ***********************************************************)
open World;;

(*** TYPES ***************************************************************)
type outcome = Result of bool * float | Timeout | Unknown

(*** FUNCTIONS ***********************************************************)
let (>>=) = Monad.(>>=)
let (>>) = Monad.(>>)
let return = Monad.return

(*
let read_from indescr =
 let line = String.make 3 ' ' in
 let l = Unix.read indescr line 0 3 in
 let line = String.sub line 0 l in
 (String.compare line "YES") == 0
;;

let run_external cmd s timeout = 
 let rfd0, wfd0 = Unix.pipe () in (* comm ocaml -> process *)
 let rfd1, wfd1 = Unix.pipe () in (* comm process -> ocaml *)
 let finish pid =
    (try Unix.kill pid 1 with _ -> ());
    (try ignore (Unix.waitpid [] pid) with _ -> ());
    (try Unix.close rfd1 with _ -> ()) in
  (* write in wfd0 *)
  let l = String.length s in
  let l' = Unix.write wfd0 s 0 l in (* l' ... chars written *)
  assert(l' = l);
  flush (Unix.out_channel_of_descr wfd0);
  Unix.close wfd0;
  let pid = Unix.create_process cmd (Array.of_list [])
    rfd0 wfd1 (Unix.descr_of_out_channel stderr)
  in Unix.close wfd1; Unix.close rfd0;
  let run () =
   try
    let outpt = read_from rfd1 in finish pid; Result (outpt)
   with
    | End_of_file -> finish pid; Unknown
  in
  match Process.run_timed (Process.Local timeout) run () with
   | None, t -> finish pid; (Unknown, t)
   | Some r, t -> r, t
;; *)

(***      WITH FILES ***)
let write_to_tmp s =
 (* Open temp file *)
 let name = Filename.temp_file "mkbtt" ".trs" in
 let descr = Unix.openfile name [Unix.O_RDWR] 0o600 in
 (* Write output *)
 let out_channel = Unix.out_channel_of_descr descr in
 Printf.fprintf out_channel "%s\n" s;
 flush out_channel;
 (* Close the underlying file descriptor, return tmp file name *)
 Unix.close descr;
 name
;;

let remove_tmp_file name =
 Sys.remove name
;;

let read indescr =
 let line = String.make 3 ' ' in
 let l = Unix.read indescr line 0 3 in
 let line = String.sub line 0 l in
 (String.compare line "YES") = 0
;;

(* run external command *)
let check_exit_status = function
  | Unix.WEXITED 0 -> ()
  | Unix.WEXITED r -> Printf.eprintf "warning: the process terminated with exit code (%d)\n%!" r
  | Unix.WSIGNALED n -> Printf.eprintf "warning: the process was killed by a signal (number: %d)\n%!" n
  | Unix.WSTOPPED n -> Printf.eprintf "warning: the process was stopped by a signal (number: %d)\n%!" n
;;
 
let syscall ?(env=[| |]) cmd =
  let ic, oc, ec = Unix.open_process_full cmd env in
  let buf1 = Buffer.create 96
  and buf2 = Buffer.create 48 in
  (try
     while true do Buffer.add_channel buf1 ic 1 done
   with End_of_file -> ());
  (try
     while true do Buffer.add_channel buf2 ec 1 done
   with End_of_file -> ());
  let exit_status = Unix.close_process_full (ic, oc, ec) in
  check_exit_status exit_status;
  (Buffer.contents buf1,
   Buffer.contents buf2)
;;
(* *)

let rec contains_yes s =
 if String.length s <= 2 then false else try 
  let i = String.index s 'Y' in
  let s' = String.sub s (i+1) (String.length s - i - 1) in
  if String.compare (String.sub s' 0 2) "ES" = 0 then 
   ((*Format.printf "YES\n%!";*) true)
  else contains_yes s'
 with Not_found -> ((*Format.printf "MAYBE of NO\n%!";*) false)
;;

let run_external2 cmd s timeout =
 let filename = write_to_tmp s in
 let path = "export PATH=/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/usr/games:/usr/kerberos/bin:/usr/local/bin:/bin:/usr/bin:/home/ami/swinkler/bin:.; " in
 let itimeout = truncate (ceil timeout) in
 let cmd = path^cmd^filename^" "^(string_of_int itimeout) in
 (*Format.printf "%s\n" cmd;*)
 (*Format.printf "TRS is %s\n%!" s;*)
 let t = Process.Local timeout in
 match Process.run_timed t syscall cmd with
 | Some (r,e), time ->
  (*remove_tmp_file filename;*)
  (*Format.printf "%s\n%!" (if contains_yes r then "YES" else "NO/MAYBE");*)
  if String.length r > 2 then 
   Result (contains_yes r, timeout -. time)
  else 
   ((*Format.printf "UNKNOWN\n%!";*) Unknown)
 | None,_ -> (*Format.printf "TMO\n%!";*) remove_tmp_file filename; Timeout
;;

(* The following functions use pipes - apparently not safe!

(* execute an external program with a certain timeout *)
let rec wait p_in p_er read_er () =
 match Unix.select [p_in;p_er] [] [] ~-.1.0 with
  | (p,[],[]) ->
   if List.mem p_er p then read_er p_er;
   if not (List.mem p_in p) then wait p_in p_er read_er ()
  | _ -> raise (Invalid_argument "Process.wait")
;;

let my_execute_timed t cmd s =
 let (my_out, tool_in) = Unix.pipe () in
 let (tool_out, my_in) = Unix.pipe () in
(* fill pipe *)
 let l = String.length s in
 let l' = Unix.write tool_in s 0 l in (* l' ... chars written *)
 assert(l' = l);
 flush (Unix.out_channel_of_descr tool_in);
 Unix.close tool_in;
 let number = inc () in
(* pipe filled *)
 let pid = Unix.fork () in
 if pid = 0 then ( (* child *)
 Printf.fprintf stderr "child %i\n" number; flush stderr;
  let path = Filename.dirname cmd in
  let cmd = "./" ^ Filename.basename cmd in
  Unix.chdir path;
  ignore (Unixx.setpgrp ());
  Unix.close tool_out;
  Printf.fprintf stderr "before %i - " number; flush stderr;
  let pid = Unix.create_process cmd [|cmd|] my_out my_in Unix.stderr in
(* close stuff? *)
  Printf.fprintf stderr "finished with %i (pid %i)" number pid;
(*  Unix.close stderr;*)
  flush (Unix.out_channel_of_descr my_in);
  Unix.close my_out;
  Unix.close my_in;
(* *)
  match Unix.waitpid [] pid with
   | (_,Unix.WEXITED s) -> Printf.fprintf stderr "exit s\n"; exit s
   | _                  -> Printf.fprintf stderr "exit 1\n"; exit 1
 ) else try (* parent *)
 Printf.fprintf stderr "parent %i\n" number;
  Unix.close my_out;
  Unix.close my_in;
  Printf.fprintf stderr "Waiting for %i (pid %i)\n" number pid;
(*  Unix.close stderr;*)
  let (_, rest) = Process.run_timed t (wait tool_out Unix.stderr print_errs) in
  read_and_store tool_out;
  Unix.close tool_out;
(*  Unix.close p_er;*)
  match Unix.waitpid [] pid with
   | (_,Unix.WEXITED s) -> (t -. rest,s)
   | _                  -> (t -. rest,1)
 with
 | Process.Timeout ->
  Format.printf "%i timed out\n" number; flush stdout;
  Unix.kill pid Sys.sigkill;
  Unix.close tool_out;
(*  Unix.close p_er;*)
  ignore (Unix.waitpid [] pid);
  (t,0)
;;

(* safe with pipes *)
let run_external3 cmd s timeout =
 let (time, status) =
  my_execute_timed timeout cmd s
 in
 (!current_result, time)
;;
*)


let check trs =
 W.get_options >>= fun options ->
 Statistics.inc_n_termination_calls >>
 W.M.Trsx.to_wst_stringm trs >>= fun s ->
 (* important to add ./ , otherwise command not found - no error :( *)
 let cmd = "./" ^ (Completion.command options)^" " in
 let timeout = Completion.ttimeout options in
 match run_external2 cmd s timeout with
  | Result(true, dtime) ->
   (*Format.printf "YES\n%!";*)
   Statistics.add_t_termination dtime >>
   Statistics.inc_n_termination_yes >> return true
  | Timeout ->
   (*Format.printf "TIMEOUT\n%!";*)
   Statistics.add_t_termination timeout >>
   Statistics.inc_n_termination_timeouts >> return false
  | _ -> (* like MAYBE *) return false
;;

let check_parallel' trs0 trs1 =
 W.get_options >>= fun options ->
 let cmd = "./" ^ (Completion.command options)^" " in
 let timeout = Completion.ttimeout options in
 Statistics.inc_n_termination_calls >>
 Statistics.inc_n_termination_calls >>
 W.M.Trsx.to_wst_stringm trs0 >>= fun trs0 ->
 W.M.Trsx.to_wst_stringm trs1 >>= fun trs1 ->
 let check s = run_external2 cmd s in
 match Process.run_parallel [check trs0; check trs1] timeout with
  | [Some a0; Some a1] -> return (a0, a1)
  | _ -> raise (Completion.GiveUp "No two answers")
;;

let eval = function
 | Result(true, dtime) ->
  (*Format.printf "YES\n%!";*)
  Statistics.inc_n_termination_yes >> return true
 | Timeout ->
  (*Format.printf "TIMEOUT\n%!";*)
  Statistics.inc_n_termination_timeouts >> return false
 | _ -> (* like MAYBE *) return false
;;

let check_parallel trs0 trs1 =
 let t = Unix.gettimeofday () in
 check_parallel' trs0 trs1 >>= fun result ->
 Statistics.add_t_termination (Unix.gettimeofday () -. t) >>
 Monad.project eval result
;;
   
let test () =
  let path = "export PATH=/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/usr/games;" in
 let cmd = path^"./muterm /tmp/mkbtt19b003.trs 5" in
 let t = Process.Local 5.0 in
 match Process.run_timed t syscall cmd with
 | Some (r,e), time ->
  Format.printf "Result of test is: %s\n" r;

 | None,_ -> Format.printf "Test timeout";
;;

