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

(*** GLOBALS ******************************************************************)
let buff_size = 1024;;
let xsltproc  = "xsltproc";;

let close chs = match Unix.close_process chs with
  | Unix.WEXITED status   ->
    if status <> 0 then Format.eprintf "exited with status %i\n%!" status
  | Unix.WSIGNALED signum ->
    Format.eprintf "killed by signal %i\n%!" signum
  | Unix.WSTOPPED signum  ->
    Format.eprintf "stopped by signal %i\n%!" signum
;;

let write ch s = output_string ch s;flush ch;;

let read ch =
  let loop = ref true in
  let buff = String.create buff_size in
  let res  = Buffer.create(1024*1024) in
  while !loop do
    let n = input ch buff 0 buff_size in
    if n = 0 then loop := false
             else Buffer.add_substring res buff 0 n
  done;
  Buffer.contents res
;;

let translate ?style input =
  let style = match style with None -> "" | Some s -> s in
  let (chout,chin) =
    Unix.open_process(xsltproc^" "^style^" -")
  in
  write chin input;
  close_out chin;
  let res = read chout in
  ignore(Unix.close_process(chout,chin));
  res
;;
