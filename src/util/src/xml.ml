open Prelude;;

type attrs = (string * string) list;;

type node  = {
  id : string;
  attrs : attrs;
  succs : node list;
  text : string option;
};;

type meta = (string * attrs) list;;

type t = (meta * node);;

let mk_node id attrs succs text = {
  id = id; attrs = attrs; succs = succs; text = text;
};;

let rec fpf_attrs fmt = function
  | (k,v)::kvs -> Format.fprintf fmt " %s=\"%s\"%a" k v fpf_attrs kvs
  | _          -> ()
;;

let fpf_text fmt = function
  | Some cs -> Format.fprintf fmt "%s" cs
  | None    -> ()
;;

let rec fpf_nodes fmt = function
  | [n]   -> Format.fprintf fmt "@[%a@]" fprintf n
  | n::ns -> Format.fprintf fmt "%a%a" fpf_nodes [n] fpf_nodes ns
  | _     -> ()
and fprintf fmt n =
  Format.fprintf fmt "@{<%s%a>%a%a@}"
    n.id fpf_attrs n.attrs fpf_text n.text fpf_nodes n.succs
;;

let to_string n =
  let module F = Format in
  let module S = String in
  let buff = Buffer.create (1024) in
  let bfmt = F.formatter_of_buffer buff in
  F.pp_set_tags bfmt true;
  F.pp_set_formatter_tag_functions bfmt {
    F.mark_open_tag   = (fun s -> "<"^s^">");
    F.mark_close_tag  = (fun s ->
      "</"^(if S.contains s ' ' then S.sub s 0 (S.index s ' ')
                                else s)^">"
    );
    F.print_open_tag  = const ();
    F.print_close_tag = const ();
  };
  F.fprintf bfmt "%a@?" fprintf n;
  Buffer.contents buff
;;
