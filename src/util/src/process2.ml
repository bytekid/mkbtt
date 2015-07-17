
open Prelude;;

exception Stop;;

type process = {
  pid   : int;              (* pid of process *)
  i     : int;              (* index of process *)
  descr : Unix.file_descr;  (* descriptor to read result from *)
  cores : int;              (* number of cores needed *)
};;

let make pid i descr cores = {
  pid   = pid;
  i     = i;
  descr = descr;
  cores = cores;
};;

let status ?(text = "") ps = 
 let pids = Listx.join string_of_int ", " ps in
 Debug.debug (Format.sprintf "%s [%d] <- [%s]" text (Unix.getpid ()) pids)
;;

let report_exc f = 
 (* try f () with  *)
 (* | e -> Debug.force (Format.sprintf "%s" (Printexc.to_string e)); raise e *)
 f ()
;;

let set_signals h = 
 Sys.set_signal Sys.sigterm h; Sys.set_signal Sys.sigalrm h;
;;

let signal = ref 0;;

(*functionality starts here*)
let child t dout f = 
 let status = {Unix.it_interval = 0.0; Unix.it_value = t} in
 ignore (Unix.setitimer Unix.ITIMER_REAL status);
 set_signals Sys.Signal_default;
 if !signal = 1 then exit 0;
 let chout = Unix.out_channel_of_descr dout in
 set_binary_mode_out chout true;
   (* Debug.on (); *)
   (* Debug.debug (Format.sprintf "[%d] starts computing" (Unix.getpid
   ())); *)
   (* Debug.off (); *)
 let r = report_exc f in
   (* Debug.on (); *)
   (* Debug.debug (Format.sprintf "[%d] writes in pipe" (Unix.getpid
   ())); *)
   (* Debug.off (); *)
 let status = {Unix.it_interval = 0.0; Unix.it_value = 0.0} in
 ignore (Unix.setitimer Unix.ITIMER_REAL status);
 Marshal.to_channel chout (Unix.getpid ()) [];
 Marshal.to_channel chout r [Marshal.Closures];
 close_out chout;
   (* Debug.on (); *)
   (* Debug.debug (Format.sprintf "[%d] wrote in pipe" (Unix.getpid ())); *)
   (* Debug.off (); *)
 exit 0
;;

let fork_pipe t i (f,cores) =
 let (din,dout) = Unix.pipe () in
 match Unix.fork () with
  | 0 -> Unix.close din; child t dout f;
  | pid -> 
   (* Debug.on (); *)
   (* Debug.debug (Format.sprintf "[%d] forks [%d]" (Unix.getpid ())
   pid); *)
   (* Debug.off (); *)
   Unix.close dout; make pid i din cores 
;;

let create t procs (i,f) ps = (procs+snd f,fork_pipe t i f::ps);;

let rec waitpid pid =
 try 
  ignore (Unix.waitpid [] pid) 
 with Unix.Unix_error (Unix.EINTR,_,_) -> waitpid pid
;;

let killpid pid = 
 (* Debug.on (); *)
 (* Debug.debug (Format.sprintf "[%d] killing [%d]" (Unix.getpid ())
 pid); *)
 (* Debug.off (); *)
 (try Unix.kill pid Sys.sigterm with Unix.Unix_error (Unix.ESRCH,_,_) -> ());
 waitpid pid;
 (* Debug.on (); *)
 (* Debug.debug (Format.sprintf "[%d] waited for [%d]" (Unix.getpid ())
 pid); *)
 (* Debug.off (); *)
;;

let kill p = 
 killpid p.pid;
 Unix.close p.descr;
;;

let kill_by_descr ps r = kill (List.find (fun p -> p.descr = r) ps);;
let kill_all ps = List.iter kill ps;;

(* set handler at exit to what *)
let read procs ps =
 try
  set_signals (Sys.Signal_handle (fun _ -> raise Stop));
  if !signal = 1 then raise Stop;
  let r =
   try 
   (* Debug.on (); *)
   (* Debug.debug (Format.sprintf "[%d] waiting" (Unix.getpid ())); *)
   (* Debug.off (); *)
   let (f,_,_) = (Unix.select (List.map (fun p -> p.descr) ps) [] [] ~-.1.0) in
   (* Debug.on (); *)
   (* Debug.debug (Format.sprintf "[%d] reads something" (Unix.getpid
   ())); *)
   (* Debug.off (); *)
   List.hd f
   with Unix.Unix_error (Unix.EINTR,_,_) -> (
    set_signals (Sys.Signal_handle (fun _ -> signal := 1));
    raise Stop
   ) in  
  set_signals (Sys.Signal_handle (fun _ -> signal := 1));
  let chin = Unix.in_channel_of_descr r in
  try
   let pid = Marshal.from_channel chin in
   let result = Marshal.from_channel chin in
   (* Debug.on (); *)
   (* Debug.debug (Format.sprintf "[%d] read from [%d]" (Unix.getpid ())
   pid); *)
   (* Debug.off (); *)
   (* kill (pid,r,0); *)
   kill_by_descr ps r;
   (* set_signals (Sys.Signal_handle (fun _ -> signal := 1)); *)
   let ([found],ps) = List.partition (fun p -> p.pid = pid) ps in
   Some (pid, Some result, procs-found.cores, ps)
  with End_of_file -> 
   (* set_signals (Sys.Signal_handle (fun _ -> signal := 1)); *)

   kill_by_descr ps r;
   let ([found],ps) = List.partition (fun p -> p.descr = r) ps in
   Some (found.pid, None, procs-found.cores,ps);

   (* kill_all ps; *)
   (* exit 0; *)
   (* None *)
 with
 | Stop -> 
   (* Debug.on (); *)
   (* Debug.debug (Format.sprintf "[%d] stopping" (Unix.getpid ())); *)
   (* Debug.off (); *)
  (* set_signals (Sys.Signal_handle (fun _ -> Format.eprintf "[%d] multistop@\n%!" (Unix.getpid ()))); *)
  (* set_signals (Sys.Signal_handle (fun _ -> ())); *)
  kill_all ps; 
  exit 0;
  None;
;;

let return p acc = match p with
 | Some _ -> Either.make_left None
 | None -> Either.make_right
 (List.map snd (List.sort ((fun (i,_) (j,_) -> compare i j)) acc))
;;

let rec parallel acc procs ps t n p fs = 
  Debug.debug (Format.sprintf "%d of %d active" procs n);
  Debug.debug (Format.sprintf "%d remaining" (List.length fs));
 (* everything computed *)
 if ps = [] && fs = [] then return p acc
 (* waiting for result *)
 else if fs = [] || procs + snd (snd (List.hd fs)) > n then (*order does matter*)
  match read procs ps with
   | None -> return p acc
   | Some (i,r,procs,ps) -> 
    (match p,r with 
     | Some q, Some s when q s -> kill_all ps; Either.make_left r
     | _ -> parallel ((i,r)::acc) procs ps t n p fs
    )
 (* sufficiently cores vacant *)
 else 
  let (procs,ps) = create t procs (List.hd fs) ps in 
  parallel acc procs ps 0.0 n p (List.tl fs)
;;

(*run [fs] on [n] cores, every (fst f) takes (snd f) processors*)
let parallel_main t n p fs = 
 set_signals (Sys.Signal_handle (fun _ -> signal := 1));
 parallel [] 0 [] t n p (Listx.mapi (fun i f -> (i,f)) fs)
;;

(*differences to Unix.create_process:
 - does not return immediately
 - returns time consumed by [cmd]
 - can be used inside run_timed, run_parallel, etc
*)
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

(*run [fs] on [n] cores, every f takes one processor*)
let parallel2 t n fs = Either.right (parallel_main 0.0 n None fs);;

(*run [fs] on [n] cores, every f takes one processor*)
let parallel_until2 t n p fs = Either.left (parallel_main t n (Some p) fs);;

(*run [fs] on [n] cores, every f takes one processor*)
let parallel t n fs = parallel2 t n (List.map (fun f -> (f,1)) fs);;

(*run [fs] on [n] cores, every f takes one processor*)
let parallel_until t n p fs = parallel_until2 t n p (List.map (fun f -> (f,1)) fs);;


let run_timed t f =
 if t < 0.01 then None else

 (* let status = {Unix.it_interval = 0.0; Unix.it_value = t} in *)
 (* ignore (Unix.setitimer Unix.ITIMER_REAL status); *)
 (* Debug.on (); *)
 (* Debug.debug (Format.sprintf "[%d] running %f s" (Unix.getpid ()) t);
 *)
 (* Debug.off (); *)
 let r = match report_exc (fun _ -> parallel t 1 [f]) with
  | [] -> None (*execution interrupted*)
  | [r] -> r   (*execution completed *)
 in
 (* ignore (Unix.setitimer Unix.ITIMER_REAL status); *)
 r
;;



(*wrapper to conform with old module*)
let unity_elt elt f = fun _ -> f elt;;
let unity elt fs = List.map (unity_elt elt) fs;;
type time = 
 | Global of float
 | Local of float
;;

let run_parallel fs elt = parallel 0.0 max_int (unity elt fs);;

let run_parallel_until p fs elt = 
 parallel_until 0.0 max_int p (unity elt fs)
;;

let run_timed t f elt = 
 let t0 = Unix.gettimeofday () in
 let r = run_timed t (unity_elt elt f) in
 let delta = Unix.gettimeofday () -. t0 in
 (r, t -. delta)
;;

let run_timed t f elt = match t with
 | Global t -> 
  let t0 = Unix.gettimeofday () in
  let (r,tx) = run_timed (t -. t0) f elt in
  (r, tx -. t0)
 | Local t -> run_timed t f elt
;;
