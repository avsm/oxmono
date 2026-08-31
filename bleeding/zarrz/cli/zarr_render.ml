(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Turning metadata into the lines and the JSON the [zarr] command
   prints. Nothing here reads a store: every function takes values the
   caller already has, so the rendering is testable on hand-built
   metadata and the network cost of a command is decided in one
   place. *)

module Ext = Zarrz.Ext
module Metadata = Zarrz.Metadata

(* {1 Numbers, shapes and sizes} *)

let product a = Array.fold_left (fun acc n -> acc * n) 1 a

(* A zero dimensional array has one element and an empty shape, which
   would print as nothing at all. *)
let shape a =
  if Array.length a = 0 then "scalar"
  else String.concat "x" (Array.to_list (Array.map string_of_int a))

let shape_list l = shape (Array.of_list l)

let size_units = [| "B"; "KiB"; "MiB"; "GiB"; "TiB"; "PiB" |]

let human_bytes n =
  if n < 1024 then Printf.sprintf "%d B" n
  else begin
    let f = ref (float_of_int n) and i = ref 0 in
    while !f >= 1024. && !i < Array.length size_units - 1 do
      f := !f /. 1024.;
      incr i
    done;
    Printf.sprintf "%.1f %s" !f size_units.(!i)
  end

let ratio ~stored ~nominal =
  if nominal = 0 then "-"
  else Printf.sprintf "%.2fx" (float_of_int stored /. float_of_int nominal)

(* {1 Generic JSON} *)

let json_string j =
  match Jsont_bytesrw.encode_string Jsont.json j with
  | Ok s -> s
  | Error _ -> "<unencodable>"

let json_doc j =
  match Jsont_bytesrw.encode_string ~format:Jsont.Indent Jsont.json j with
  | Ok s -> s
  | Error m -> failwith m

(* A whole attributes object can run to megabytes, so a long value is
   cut short here and the length says what was lost. [--json] prints the
   attributes in full, so nothing is unreachable. *)
let elide n s =
  let len = String.length s in
  if len <= n then s
  else begin
    let i = ref n in
    while !i > 0 && Char.code s.[!i] land 0xc0 = 0x80 do
      decr i
    done;
    Printf.sprintf "%s ... (%d bytes)" (String.sub s 0 !i) len
  end

let jstr = Jsont.Json.string
let jint = Jsont.Json.int
let jnum = Jsont.Json.number
let jbool = Jsont.Json.bool
let jnull = Jsont.Json.null ()
let jlist l = Jsont.Json.list l
let jints a = jlist (List.map jint (Array.to_list a))
let jopt f = function None -> jnull | Some v -> f v

let jobj mems =
  Jsont.Json.object' (List.map (fun (n, v) -> (Jsont.Json.name n, v)) mems)

(* {1 Extension configurations} *)

let cfg_string e n =
  match Ext.config_mem e n with Some (Jsont.String (s, _)) -> Some s | _ -> None

let cfg_int e n =
  match Ext.config_mem e n with
  | Some (Jsont.Number (f, _)) -> Some (int_of_float f)
  | _ -> None

let cfg_ints e n =
  match Ext.config_mem e n with
  | Some (Jsont.Array (l, _)) ->
      let rec go acc = function
        | [] -> Some (List.rev acc)
        | Jsont.Number (f, _) :: tl -> go (int_of_float f :: acc) tl
        | _ -> None
      in
      go [] l
  | _ -> None

let cfg_exts e n =
  match Ext.config_mem e n with
  | Some j -> Result.to_option (Jsont.Json.decode (Jsont.list Ext.jsont) j)
  | None -> None

(* {1 Codec chains} *)

(* The compact form used on a tree line: every codec as its name and the
   one part of its configuration a reader chooses a store by. A shard
   spells its inner chunk shape, then its inner chain, then its index
   chain after a bar, as in
   [sharding(4x4; bytes(le) gzip(5) | idx bytes(le) crc32c)]. *)
let rec codec_summary exts = String.concat " " (List.map codec_one exts)

and codec_one e =
  match e.Ext.name with
  | "bytes" -> (
      match cfg_string e "endian" with
      | Some "little" -> "bytes(le)"
      | Some "big" -> "bytes(be)"
      | Some s -> Printf.sprintf "bytes(%s)" s
      | None -> "bytes")
  | "transpose" -> (
      match cfg_ints e "order" with
      | Some o ->
          Printf.sprintf "transpose(%s)"
            (String.concat "," (List.map string_of_int o))
      | None -> "transpose")
  | ("gzip" | "zstd") as n -> (
      match cfg_int e "level" with
      | Some l -> Printf.sprintf "%s(%d)" n l
      | None -> n)
  | "blosc" -> (
      match cfg_string e "cname" with
      | Some c -> Printf.sprintf "blosc(%s)" c
      | None -> "blosc")
  | "sharding_indexed" ->
      let inner =
        match cfg_ints e "chunk_shape" with
        | Some s -> shape_list s
        | None -> "?"
      in
      let chain =
        match cfg_exts e "codecs" with Some l -> codec_summary l | None -> "?"
      in
      let idx =
        match cfg_exts e "index_codecs" with
        | Some l -> " | idx " ^ codec_summary l
        | None -> ""
      in
      Printf.sprintf "sharding(%s; %s%s)" inner chain idx
  | n -> n

(* {1 Node summaries} *)

let chunk_shape_of (m : Metadata.array_meta) =
  match Zarrz.Chunk_grid.of_ext m.chunk_grid ~array_shape:m.shape with
  | Ok g -> Some (Zarrz.Chunk_grid.chunk_shape g)
  | Error _ -> None

let grid_shape_of (m : Metadata.array_meta) =
  match Zarrz.Chunk_grid.of_ext m.chunk_grid ~array_shape:m.shape with
  | Ok g -> Some (Zarrz.Chunk_grid.grid_shape g)
  | Error _ -> None

(* The one line a tree prints for a node. A node with no metadata
   document of its own is a group the hierarchy implies. *)
let group_summary = "group"
let implicit_summary = "group (no metadata document)"

let array_summary (m : Metadata.array_meta) =
  let chunks = match chunk_shape_of m with Some c -> shape c | None -> "?" in
  Printf.sprintf "array %s %s chunks %s %s" m.data_type.Ext.name (shape m.shape)
    chunks (codec_summary m.codecs)

(* {1 Trees} *)

type tree = { label : string; kids : tree list }

let rec print_tree prefix nodes =
  let n = List.length nodes in
  List.iteri
    (fun i t ->
      let last = i = n - 1 in
      Printf.printf "%s%s %s\n" prefix (if last then "└──" else "├──") t.label;
      print_tree (prefix ^ if last then "    " else "│   ") t.kids)
    nodes

(* {1 Fields and tables} *)

(* One field a line, the name padded past the longest label of any
   command so that values line up whatever is printed. A sub-field is
   indented by two and padded to the same column. A label that has
   outgrown the column keeps one space after it rather than running into
   its value. *)
let pad_to n s =
  let len = String.length s in
  if len >= n then s ^ " " else s ^ String.make (n - len) ' '

let field name fmt = Printf.printf ("%s" ^^ fmt ^^ "\n") (pad_to 22 name)

let sub name fmt =
  Printf.printf ("%s" ^^ fmt ^^ "\n") (pad_to 22 ("  " ^ name))

(* The first column is a name and is left aligned. Every other column is
   a number and is right aligned, so that magnitudes compare down the
   page. *)
let table ~headers rows =
  let cols = List.length headers in
  let w = Array.make cols 0 in
  let measure r =
    List.iteri (fun i c -> w.(i) <- max w.(i) (String.length c)) r
  in
  measure headers;
  List.iter measure rows;
  let line r =
    let cell i c =
      if i = 0 then Printf.sprintf "%-*s" w.(i) c
      else Printf.sprintf "%*s" w.(i) c
    in
    print_string (String.concat "  " (List.mapi cell r));
    print_newline ()
  in
  line headers;
  List.iter line rows
