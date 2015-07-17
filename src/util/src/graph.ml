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

(*** MODULES ******************************************************************)
module F = Format;;
module O = Option;;
module T = Triple;;

(*** MODULE TYPES *************************************************************)
module type LABEL = sig
 type t

 val combine : t -> t -> t
 val compare : t -> t -> int
 val copy : t -> t
 val fprintf : F.formatter -> t -> unit
end

module type NODE = sig
 type t

 val compare : t -> t -> int
 val copy : t -> t
 val hash : t -> int
 val fprintf : F.formatter -> t -> unit
end

module type SIGNATURE = sig
 type node
 type edge
 type t

 val make : node list -> edge list -> t
 val add_edge : edge -> t -> t
 val add_node : node -> t -> t
 val edges : t -> edge list
 val empty : t
 val generate : (node -> node -> edge list) -> node list -> t
 val in_nodes : t -> node list
 val nodes : t -> node list
 val of_list : edge list -> t
 val out_nodes : t -> node list
 val remove_edge : edge -> t -> t
 val remove_node : node -> t -> t
 val restrict : node list -> t -> t
 val successors : node -> t -> node list
 val to_list : t -> edge list
 val fold_edges : (edge -> 'a -> 'a) -> t -> 'a -> 'a
 val foldi_edges : (int -> edge -> 'a -> 'a) -> t -> 'a -> 'a
 val fold_nodes : (node -> 'a -> 'a) -> t -> 'a -> 'a
 val foldi_nodes : (int -> node -> 'a -> 'a) -> t -> 'a -> 'a
 val iter_edges : (edge -> unit) -> t -> unit
 val iteri_edges : (int -> edge -> unit) -> t -> unit
 val iter_nodes : (node -> unit) -> t -> unit
 val iteri_nodes : (int -> node -> unit) -> t -> unit
 val cycles : t -> node list list
 val paths : node -> node -> t -> node list list
 val sccs : ?trivial:bool -> t -> node list list
 val filter_edges : (edge -> bool) -> t -> t
 val filter_nodes : (node -> bool) -> t -> t
 val find_edge : (edge -> bool) -> t -> edge
 val find_node : (node -> bool) -> t -> node
 val mem_edge : edge -> t -> bool
 val mem_node : node -> t -> bool
 val for_all_edges : (edge -> bool) -> t -> bool
 val for_all_nodes : (node -> bool) -> t -> bool
 val exists_edge : (edge -> bool) -> t -> bool
 val exists_node : (node -> bool) -> t -> bool
 val is_empty : t -> bool
 val is_trivial_scc : node list -> t -> bool
 val bypass : node -> t -> t
 val partition : (edge -> bool) -> t -> t * t
 val size_edges : t -> int
 val size_nodes : t -> int
 val compare : t -> t -> int
 val equal : t -> t -> bool
 val fprintf : F.formatter -> t -> unit
 val to_string : t -> string
end

module type UNLABELED = sig
 include SIGNATURE

 val add : node -> node list -> t -> t
 val replace : node -> node list -> t -> t
 val fold : (node -> node list -> 'a -> 'a) -> t -> 'a -> 'a
 val foldi : (int -> node -> node list -> 'a -> 'a) -> t -> 'a -> 'a
 val iter : (node -> node list -> unit) -> t -> unit
 val iteri : (int -> node -> node list -> unit) -> t -> unit
 val is_acyclic : t -> bool
 val floyd_warshall : t -> ((node * int)list * int option array array)
 val transitive_closure : t -> t
end

module type LABELED = sig
 type label

 include SIGNATURE

 val add : node -> (label option * node) list -> t -> t
 val replace : node -> (label option * node) list -> t -> t
 val fold : (node -> (label option * node) list -> 'a -> 'a) -> t -> 'a -> 'a
 val foldi : (int -> node -> (label option * node) list -> 'a -> 'a) -> t
  -> 'a -> 'a
 val iter : (node -> (label option * node) list -> unit) -> t -> unit
 val iteri : (int -> node -> (label option * node) list -> unit) -> t -> unit
 val labels : node -> node -> t -> label list
 val combine_edges : t -> t
 val transitive_closure : ?n:int -> t -> t
end

(*** MODULES ******************************************************************)
module Labeled (L : LABEL) (N : NODE) = struct
 (*** MODULES *****************************************************************)
 module Label = struct
  type t = L.t option;;

  let combine l l' = match (l,l') with
   | None,_ -> l'
   | _,None -> l
   | Some l,Some l' -> Some (L.combine l l')
  ;;
 
  let compare = O.compare L.compare;;
  let equal l l' = compare l l' = 0;;
 end

 module N = struct
  include N

  let equal n m = N.compare n m = 0;;
 end

 module R = struct
  type t = (Label.t * N.t) list;;

  (* Scan Functions *)
  let mem = Listx.mem;;
  let exists = Listx.exists <.> Pair.uncurry;;
  let for_all p = Listx.for_all (Pair.uncurry p);;

  (* Constructors and Destructors *)
  let empty = [];;
  let singleton n = [n];;
  let add n ns = if mem n ns then ns else n :: ns;;
  let strip ns = Listx.map snd ns;;

  let remove l =
   let compare (l,n) (l',m) =
    let c = N.compare n m in if c = 0 then Label.compare l l' else c
   in
   Listx.remove_all ~c:compare <.> Pair.make l
  ;;
  
  (* Iterators *)
  let iter = Listx.iter;;
  let iteri = Listx.iteri;;
  let fold = Listx.foldl;;

  (* Search Functions *)
  let find = Listx.find;;
  let filter_nodes p = Listx.filter (p <.> snd);;

  let labels n =
   fold (fun ls (l,m) ->
    if N.equal n m then O.fold (flip Listx.cons ls) ls l else ls) []
  ;;

  (* Compare Functions *)
  let compare ns ms =
   let rec compare ns ms = match (ns,ms) with
    | [], [] -> 0
    | (l,n) :: ns, (l',m) :: ms ->
     let c = N.compare n m in
     if c <> 0 then c
     else let c = Label.compare l l' in if c = 0 then compare ns ms else c
    | _, _ -> failwith "unequal lengths"
   in
   let c = Listx.length ns - Listx.length ms in
   if c = 0 then compare ns ms else c
  ;;

  (* Miscellaneous *)
  let size = Listx.length;;

  let combine =
   let rec combine ns = function
    | [] -> ns
    | m :: ms ->
     let equal n = N.equal (snd n) <.> snd in
     let max n m = if Label.compare (fst n) (fst m) > 0 then n else m in
     let max (m,ms) n = if equal n m then (max n m,ms) else (m,n::ms) in
     let (m,ms) = fold max (m,[]) ms in
     combine (m::ns) ms
   in
   combine []
  ;;

  (* Printers *)
  let fprintf fmt =
   let print fmt (l,n) = match l with
    | None -> F.fprintf fmt "@[%a@]" N.fprintf n
    | Some l -> F.fprintf fmt "@[%a(l:@ %a)@]" N.fprintf n L.fprintf l
   in
   F.fprintf fmt "@[%a@]" (Listx.fprintf print "@ |@ ")
  ;;
 end

 module H = Hashtblx.Partial (N);;
 module M = Mapx.Make (N) (R);;

 (*** TYPES *******************************************************************)
 type label = L.t;;
 type node = N.t;;
 type edge = node * label option * node;;
 type t = M.t;;

 type state = {
  mutable stacked : unit H.t; (* stacked nodes *)
  mutable graph   : t; (* input-graph *)
  mutable gidx    : int; (* global index *)
  mutable llnn    : int H.t; (* low-link numbers of nodes *)
  mutable nidx    : int H.t; (* node index *)
  mutable result  : (node list) Queue.t; (* queue of SCCs *)
  mutable stack   : node Stack.t; (* stack of visited nodes *)
 };;
 
 (*** EXCEPTIONS **************************************************************)
 exception Found of edge;;
 
 (*** FUNCTIONS ***************************************************************)
 (* Iterators *)
 let fold = M.fold;;
 let foldi = M.foldi;;
 let iter = M.iter;;
 let iteri = M.iteri;;

 let iteri_edges f g =
  ignore (fold (fun n ns i ->
   R.fold (fun i (l,m) -> f i (T.make n l m); i + 1) i ns) g 0)
 ;;

 let iter_edges f = iteri_edges (const f);;
 let iteri_nodes f g = iteri (fun i n _ -> f i n) g;;
 let iter_nodes f = iteri_nodes (const f);;

 let foldi_edges f g d =
  snd (fold (fun n ns x ->
   R.fold (fun (i,x) (l,m) -> (i + 1,f i (T.make n l m) x)) x ns) g (0,d))
 ;;

 let fold_edges f = foldi_edges (const f);;
 let foldi_nodes f g d = foldi (fun i n _ x -> f i n x) g d;;
 let fold_nodes f = foldi_nodes (const f);;

 (* Constructors and Destructors *)
 let add = M.add;;
 let replace = M.replace;;

 let add_edge e g =
  let n = T.fst e and l = T.snd e and m = T.thd e in
  let g = if M.mem m g then g else add m R.empty g in
  try replace n (R.add (l,m) (M.find n g)) g
  with Not_found -> add n (R.singleton (l,m)) g
 ;;

 let add_node n g = if M.mem n g then g else add n R.empty g;;
 let edges = flip (fold_edges Listx.cons) [];;
 let empty = M.empty;;
 let of_list es = Listx.foldr add_edge empty es;;
 let make ns es = Listx.foldr add_node (of_list es) ns;;
 let to_list = edges;;
 let nodes g = fold (drop Listx.cons) g [];;

 let in_nodes g =
  let mem n ns = Listx.mem ~c:N.compare n ns in
  let add ns (_,n) = if mem n ns then ns else n :: ns in
  fold (fun _ ms ns -> R.fold add ns ms) g []
 ;;

 let out_nodes g =
  fold (fun n ms ns -> if R.size ms > 0 then n :: ns else ns) g []
 ;;

 let remove_edge e g =
  let n = T.fst e and l = T.snd e and m = T.thd e in
  try replace n (R.remove l m (M.find n g)) g with Not_found -> g
 ;;

 let filter_nodes p g =
  fold (fun n ns g -> if p n then add n (R.filter_nodes p ns) g else g)
   g empty
 ;;
 
 let remove_node n = filter_nodes (not <.> N.equal n);;

 let restrict ns =
  let t = Hashtblx.create 1000 in
  Listx.iter (flip (Hashtblx.add t) ()) ns;
  filter_nodes (Hashtblx.mem t)
 ;;

 let generate f ns =
  Listx.foldl (fun g n ->
   add n (Listx.flat_map (Listx.map T.drop_fst <.> f n) ns) g) empty ns
 ;;

 (* Search Functions *)
 let filter_edges p g =
  fold (fun n ns g ->
   let add ns (l,m) = if p (T.make n l m) then R.add (l,m) ns else ns in
   M.add n (R.fold add [] ns) g) g empty
 ;;

 let find_edge p g =
  try 
   iter (fun n ns ->
    let p (l,m) = p (T.make n l m) in
    try raise (Found (T.insert_fst n (R.find p ns))) with Not_found -> ()) g;
   raise Not_found
  with Found e -> e
 ;;

 let find_node p = fst <.> M.search (fun n -> p <.> const n);;
 let successors n g = try R.strip (M.find n g) with Not_found -> [];;
 let labels n m = R.labels m <.> M.find n;;

 let in_edges n g = Listx.filter (fun e -> T.thd e = n) (edges g);;
 let out_edges n g = Listx.filter (fun e -> T.fst e = n) (edges g);;

 (* Scan Functions *)
 let exists_edge p =
  M.exists (fun n ns -> R.exists (fun l -> p <.> T.make n l) ns)
 ;;

 let for_all_edges p =
  M.for_all (fun n ns -> R.for_all (fun l -> p <.> T.make n l) ns)
 ;;

 let mem_edge e =
  let n = T.fst e and l = T.snd e and m = T.thd e in
  exists_edge (T.for_all (N.equal n) (Label.equal l) (N.equal m))
 ;;

 let exists_node p = M.exists (fun n -> p <.> const n);;
 let for_all_nodes p = M.for_all (fun n -> p <.> const n);;
 let mem_node n = exists_node (N.equal n);;

 (* Properties *)
 let is_empty = not <.> exists_edge (const true);;

 let is_trivial_scc = function
  | [] -> failwith "the list of SCCs should never contain empty components"
  | [n] -> not <.> Listx.mem ~c:N.compare n <.> successors n
  | _ -> const false
 ;;

 (* Graph Analysis *)
 let paths n m g =
  let rec paths ns n m g = if N.equal n m then [[n]] else subpaths ns n m g
  and subpaths ns n m g =
   Listx.flat_map (fun l ->
    if Listx.mem ~c:N.compare l ns then []
    else Listx.map (Listx.cons n) (paths (l :: ns) l m g)) (successors n g)
  in
  if is_empty g then []
  else if N.equal n m then subpaths [n] n m g else paths [n] n m g
 ;;
 
 let cycles g = Listx.flat_map (fun n -> paths n n g) (nodes g);;

 (* scc computation (tarjan algorithm) *)
 let rec tarjan s n =
  let rec add n scc =
   let n' = Stack.pop !s.stack in
   H.remove !s.stacked n';
   if N.equal n' n
     then n' :: scc
     else add n (n' :: scc)
  in
  H.add !s.nidx n !s.gidx;
  H.add !s.llnn n !s.gidx;
  !s.gidx <- !s.gidx + 1;
  Stack.push n !s.stack;
  H.add !s.stacked n ();
  Listx.iter (fun n' ->
   let undefined = not (H.mem !s.nidx n') in
   if undefined || H.mem !s.stacked n' then (
    if undefined then tarjan s n';
    H.add !s.llnn n (min (H.find !s.llnn n) (H.find !s.llnn n')))
  )
  (successors n !s.graph);
  if H.find !s.llnn n = H.find !s.nidx n then Queue.add (add n []) !s.result
 ;;
 
 let sccs ?(trivial = true) g =
  let s = ref {
   stacked = H.create 1000;
   graph = g;
   gidx = 0;
   llnn = H.create 1000;
   nidx = H.create 1000;
   stack = Stack.create ();
   result = Queue.create ()
  } in
  Listx.iter (fun n -> if not (H.mem !s.nidx n) then (tarjan s n)) (nodes g);
  let filter sccs scc =
    if is_trivial_scc scc g
      then sccs
      else scc :: sccs
  in
  if trivial
    then Queue.fold (flip Listx.cons) [] !s.result
    else Queue.fold filter [] !s.result
 ;;

 (* Miscellaneous *)
 let size_edges g = fold (fun n ns s -> s + R.size ns) g 0;;
 let size_nodes = M.size;;
 let combine_edges g = fold (fun n -> add n <.> R.combine) g empty;;

 let partition p g =
  fold (fun n ns (g,g') ->
   let add (ns,ns') (l,m) =
    if p (T.make n l m) then (R.add (l,m) ns,ns') else (ns,R.add (l,m) ns')
   in
   let (ns,ns') = R.fold add ([],[]) ns in
   (M.add n ns g, M.add n ns' g')) g (empty,empty)
 ;;

 let bypass n g = 
  let es = Listx.product (in_edges n g) (out_edges n g) in
  let add g (e,e') =
   let u = T.fst e and l = T.snd e and l' = T.snd e' and v = T.thd e' in
   if N.equal u n then failwith "looping edge"
   else replace u (R.add (Label.combine l l',v) (M.find u g)) g
  in
  Listx.foldl add (remove_node n g) es
 ;;

 let rec transitive_closure ?(n = ~-1) g =
  let add g n =
   let es = Listx.product (in_edges n g) (out_edges n g) in
   Listx.foldl (fun g (e,e') ->
    let n = T.fst e and l = T.snd e and l' = T.snd e' and m = T.thd e' in
    replace n (R.add (Label.combine l l',m) (M.find n g)) g) g es
  in
  if n = 0 then g else
   let n = if n < 0 then n else n - 1 and g' = Listx.foldl add g (nodes g) in
   if size_edges g = size_edges g' then g else transitive_closure ~n:n g'
 ;;

 (* Compare Function *)
 let compare = M.compare;;
 let equal = M.equal;;

 (* Printers *)
 let fprintf fmt g =
  Format.fprintf fmt "@[";
  iteri_edges (fun i e ->
   let n = T.fst e and l = T.snd e and m = T.thd e in
   if i > 0 then F.fprintf fmt "@\n"; match l with
    | None -> F.fprintf fmt "@[%a@ ->@ %a@]" N.fprintf n N.fprintf m
    | Some l ->
     F.fprintf fmt "@[%a@ ->(%a)@ %a@]" N.fprintf n L.fprintf l N.fprintf m) g;
  Format.fprintf fmt "@]"
 ;;

 let to_string = F.flush_str_formatter <.> fprintf F.str_formatter;;
end

module Make (N : NODE) = struct
 (*** MODULE ******************************************************************)
 module Label = struct
  type t = unit
 
  let add _ _ = ();;
  let combine _ _ = ();;
  let compare _ _ = 0;;
  let copy _ = ();;
  let fprintf fmt _ = F.fprintf fmt "@[@]";;
 end

 module G = Labeled (Label) (N);;
 
 (*** TYPES *******************************************************************)
 type node = G.node;;
 type edge = node * node;;
 type t = G.t;;
 
 (*** FUNCTIONS ***************************************************************)
 let to_edge = T.drop_snd;;
 let to_ledge e = T.insert_snd None e;;

 (* Constructors and Destructors *)
 let add n = G.add n <.> List.map (Pair.make None);;
 let add_edge e = G.add_edge (to_ledge e);;
 let add_node = G.add_node;;
 let edges g = Listx.map to_edge (G.edges g);;
 let empty = G.empty;;
 let generate f = G.generate (fun n -> Listx.map to_ledge <.> f n);;
 let in_nodes = G.in_nodes;;
 let nodes = G.nodes;;
 let of_list = G.of_list <.> Listx.map to_ledge;;
 let make ns es = Listx.foldr add_node (of_list es) ns;;
 let out_nodes = G.out_nodes;;
 let remove_edge = G.remove_edge <.> to_ledge;;
 let remove_node = G.remove_node;;
 let replace n = G.replace n <.> List.map (Pair.make None);;
 let restrict = G.restrict;;
 let successors = G.successors;;
 let to_list g = Listx.map to_edge (G.to_list g);;

 (* Iterators *)
 let fold f = G.fold (fun n -> f n <.> List.map snd);;
 let foldi f = G.foldi (fun i n -> f i n <.> List.map snd);;
 let fold_edges f = G.fold_edges (f <.> to_edge);;
 let foldi_edges f = G.foldi_edges (fun i -> f i <.> to_edge);;
 let fold_nodes = G.fold_nodes;;
 let foldi_nodes = G.foldi_nodes;;
 let iter f = G.iter (fun n -> f n <.> List.map snd);;
 let iteri f = G.iteri (fun i n -> f i n <.> List.map snd);;
 let iter_edges f = G.iter_edges (f <.> to_edge);;
 let iteri_edges f = G.iteri_edges (fun i -> f i <.> to_edge);;
 let iter_nodes = G.iter_nodes;;
 let iteri_nodes = G.iteri_nodes;;

 (* Graph Analysis *)
 let cycles = G.cycles;;
 let paths = G.paths;;
 let sccs = G.sccs;;

 (* Search Functions *)
 let filter_edges p = G.filter_edges (p <.> to_edge);;
 let filter_nodes = G.filter_nodes;;
 let find_edge p = to_edge <.> G.find_edge (p <.> to_edge);;
 let find_node = G.find_node;;

 (* Scan Functions *)
 let mem_edge = G.mem_edge <.> to_ledge;;
 let mem_node = G.mem_node;;
 let for_all_edges p = G.for_all_edges (p <.> to_edge);;
 let for_all_nodes = G.for_all_nodes;;
 let exists_edge p = G.exists_edge (p <.> to_edge);;
 let exists_node = G.exists_node;;

 (* Properties *)
 let is_empty = G.is_empty;;
 let is_trivial_scc = G.is_trivial_scc;;

 (* Miscellaneous *)
 let bypass = G.bypass;;
 let partition p = G.partition (p <.> to_edge);;
 let size_edges = G.size_edges;;
 let size_nodes = G.size_nodes;;
 let transitive_closure g = G.transitive_closure g;;

 let floyd_warshall g =
  let floyd_warshall n m =
   for k = 0 to n - 1 do
    for i = 0 to n - 1 do
     for j = 0 to n - 1 do
      m.(i).(j) <- match (m.(i).(j),m.(i).(k),m.(k).(j)) with 
       | (Some ij,Some ik,Some kj) ->
         let ikj = ik + kj in
         if ij < ikj then Some ij else Some ikj
       | (None,Some ik,Some kj)    -> Some(ik + kj)
       | (Some ij,_,_)             -> Some ij
       | _                         -> None
     done
    done
   done
  in
  let (ns,n) = foldi_nodes (fun i n (xs,_) -> ((n,i)::xs,i+1)) g ([],0) in
  let n = size_nodes g in
  let path = Array.make_matrix n n None in
  iter_edges (fun (x,y) -> path.(List.assoc x ns).(List.assoc y ns) <- Some 1) g;
  floyd_warshall n path;
  (ns,path)
 ;;

 let is_acyclic g =
   let (_,path) = floyd_warshall g in
   List.exists (fun i -> path.(i).(i) <> None) (Listx.range 0 (Array.length path))
 ;;

 (* Compare Functions *)
 let compare = G.compare;;
 let equal = G.equal;;

 (* Printers *)
 let fprintf = G.fprintf;;
 let to_string = G.to_string;;
end
