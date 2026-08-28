(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* The [zarr] command: inspect a Zarr V3 store given a URL or a
   directory. Each subcommand does its work in one plain function over
   already parsed arguments, and the cmdliner terms at the end of the
   file do nothing but assemble those arguments. *)

open Cmdliner
module Store = Zarrz.Store
module Metadata = Zarrz.Metadata
module Ext = Zarrz.Ext
module Dtype = Zarrz.Dtype
module Fill_value = Zarrz.Fill_value
module Chunk_grid = Zarrz.Chunk_grid
module Chunk_key = Zarrz.Chunk_key
module Codec = Zarrz.Codec
module Consolidated = Zarrz.Consolidated
module R = Zarr_render
module W = Zarr_walk

let version = "0.1.0"

(* {1 Failures} *)

(* A store failure, a bad argument or an I/O error is a one line
   message on stderr and exit 1. A backtrace tells a user of a command
   line tool nothing it can act on. *)
let exit_failure = 1

let one_line s = String.map (function '\n' | '\r' | '\t' -> ' ' | c -> c) s

let report m =
  prerr_string ("zarr: " ^ one_line m ^ "\n");
  exit_failure

let guard f =
  try f () with
  | Zarrz.Error.E e -> report (Zarrz.Error.to_string e)
  | Invalid_argument m -> report m
  | Sys_error m -> report m
  | Eio.Io _ as e -> report (Printexc.to_string e)
  | Failure m -> report m

(* [run spec f] opens the store [spec] names and hands it to [f], under
   the one guard and the one switch every subcommand needs. *)
let run spec f =
  guard @@ fun () ->
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw -> f (W.open_ ~sw env spec)

let emit_json j = print_string (R.json_doc j ^ "\n")

(* {1 Hierarchy assembly} *)

(* The nodes below a path, indexed by parent. A path whose own parent
   has no metadata document still belongs in the tree, so every missing
   ancestor is filled in and marked absent. *)
type index = {
  present : (string, unit) Hashtbl.t;
  kids : (string, string list) Hashtbl.t;  (* Children in reverse order. *)
}

let index rels =
  let present = Hashtbl.create 64 and kids = Hashtbl.create 64 in
  let seen = Hashtbl.create 64 in
  List.iter (fun p -> Hashtbl.replace present p ()) rels;
  let rec add p =
    if not (Hashtbl.mem seen p) then begin
      Hashtbl.replace seen p ();
      let par = W.parent p in
      let sibs = Option.value ~default:[] (Hashtbl.find_opt kids par) in
      Hashtbl.replace kids par (p :: sibs);
      if not (String.equal par "") then add par
    end
  in
  List.iter (fun p -> if not (String.equal p "") then add p) rels;
  { present; kids }

let child_paths idx p =
  List.rev (Option.value ~default:[] (Hashtbl.find_opt idx.kids p))

(* {1 Node summaries} *)

let summary_of = function
  | `Array m -> R.array_summary m
  | `Group _ -> R.group_summary

let meta_opt t idx ~base rel =
  if Hashtbl.mem idx.present rel then Some (W.meta_at t ~path:(W.join base rel))
  else None

let rec subtree t idx ~base rel =
  List.map
    (fun k ->
      let label =
        match meta_opt t idx ~base k with
        | None -> R.implicit_summary
        | Some m -> summary_of m
      in
      {
        R.label = Printf.sprintf "%s  %s" (W.base_name k) label;
        R.kids = subtree t idx ~base k;
      })
    (child_paths idx rel)

(* {1 Child kinds} *)

(* The names and kinds of the nodes one component below [path]. The
   consolidated map answers from what it already holds. A listable store
   pays one metadata read per child, which is why this is never reached
   over HTTP: a store that cannot list has no children to count. *)
let child_kinds t ~path =
  match W.descendants t ~path with
  | W.From_consolidated, _ ->
      Some (Consolidated.children (Option.get t.W.cons) path)
  | W.From_listing, rels ->
      let one r =
        if W.depth r <> 1 then None
        else
          match W.meta_at t ~path:(W.join path r) with
          | `Array _ -> Some (r, `Array)
          | `Group _ -> Some (r, `Group)
      in
      Some (List.filter_map one rels)
  | W.From_neither, _ -> None

(* {1 Extension reporting} *)

type convention = { c_name : string; c_uuid : string; c_spec : string }

(* A generic walk of the attributes object. Any convention registers
   itself in [zarr_conventions], whatever else it defines, so this
   reports conventions no module here knows. *)
let conventions attrs =
  let member n e =
    match Jsont.Json.find_mem n e with
    | Some (_, Jsont.String (v, _)) -> v
    | _ -> "-"
  in
  match attrs with
  | Some (Jsont.Object (o, _)) -> (
      match Jsont.Json.find_mem "zarr_conventions" o with
      | Some (_, Jsont.Array (l, _)) ->
          List.filter_map
            (function
              | Jsont.Object (e, _) ->
                  Some
                    {
                      c_name = member "name" e;
                      c_uuid = member "uuid" e;
                      c_spec = member "spec_url" e;
                    }
              | _ -> None)
            l
      | _ -> [])
  | _ -> []

let must_understand j =
  match j with
  | Jsont.Object (mems, _) -> (
      match Jsont.Json.find_mem "must_understand" mems with
      | Some (_, Jsont.Bool (b, _)) -> b
      | _ -> true)
  | _ -> true

(* The consolidated member is reported on its own line, so listing it
   again as an unmodelled member would say the same thing twice. *)
let unknown_members mems =
  List.filter_map
    (fun ((n, _), j) ->
      if String.equal n "consolidated_metadata" then None
      else Some (n, must_understand j))
    mems

let non_core (m : Metadata.array_meta) =
  let l = [] in
  let l =
    if Dtype.of_name m.data_type.Ext.name = None then
      ("data type", m.data_type.Ext.name) :: l
    else l
  in
  let l =
    if String.equal m.chunk_grid.Ext.name "regular" then l
    else ("chunk grid", m.chunk_grid.Ext.name) :: l
  in
  let n = m.chunk_key_encoding.Ext.name in
  let l =
    if String.equal n "default" || String.equal n "v2" then l
    else ("chunk key encoding", n) :: l
  in
  List.rev l

type geoemb =
  | Geo_none
  | Geo_bad of string * bool
  | Geo_ok of Zarrz_geoemb.t * bool

(* The convention is declared on the group that owns the hierarchy, so
   an array inside one carries none of it. Fall back to the root, whose
   document has already been read. *)
let geoemb t ~attrs =
  let probe j =
    match Zarrz_geoemb.of_attributes j with
    | None -> None
    | Some (Ok g) -> Some (Ok g)
    | Some (Error m) -> Some (Error m)
  in
  let here = Option.bind attrs probe in
  match here with
  | Some (Ok g) -> Geo_ok (g, false)
  | Some (Error m) -> Geo_bad (m, false)
  | None -> (
      let root =
        Option.bind t.W.root_group (fun g ->
            g.Metadata.group_attributes)
      in
      match Option.bind root probe with
      | Some (Ok g) -> Geo_ok (g, true)
      | Some (Error m) -> Geo_bad (m, true)
      | None -> Geo_none)

let geo_kind = function
  | Zarrz_geoemb.Pixel -> "pixel"
  | Zarrz_geoemb.Chip -> "chip"

let geo_layout = function
  | Zarrz_geoemb.Utm_zones -> "utm_zones"
  | Zarrz_geoemb.Global -> "global"

let geo_quant (q : Zarrz_geoemb.Quantization.t) =
  let head =
    Printf.sprintf "%s, %s to %s" q.method_ q.original_dtype
      (Option.value ~default:"?" q.quantized_dtype)
  in
  match q.scale with
  | None -> head
  | Some (Zarrz_geoemb.Quantization.Scale.Scalar s) ->
      Printf.sprintf "%s, scale %g offset %g" head s.scale s.offset
  | Some (Zarrz_geoemb.Quantization.Scale.Array a) ->
      Printf.sprintf "%s, scale array %S" head a.array_name

(* {1 Shard geometry} *)

type shard = {
  s_shape : int array;  (* One shard, which is one chunk of the grid. *)
  s_inner : int array;
  s_per : int array;  (* Inner chunks along each dimension of a shard. *)
  s_count : int;
  s_location : string;
  s_index : int option;  (* Encoded index size in bytes. *)
}

(* The index is a uint64 array of the inner chunk counts and a trailing
   2, run through the index chain, so its size is exactly what that
   chain encodes. Asking the chain rather than assuming 16 bytes an
   entry keeps a store with an unusual index chain honest. *)
let index_bytes ~per ~index_codecs =
  let fill = Fill_value.of_bytes (String.make 8 '\255') in
  match
    Codec.chain_of_exts ~dtype:Dtype.Uint64 ~fill_value:fill index_codecs
  with
  | Error _ -> None
  | Ok chain -> (
      let shape = Array.append per [| 2 |] in
      match Codec.encoded_size chain { Codec.dtype = Dtype.Uint64; shape } with
      | Codec.Fixed n -> Some n
      | Codec.Bounded _ | Codec.Unbounded -> None)

let shard_of (m : Metadata.array_meta) =
  match
    List.find_opt (fun e -> String.equal e.Ext.name "sharding_indexed") m.codecs
  with
  | None -> None
  | Some e -> (
      match (R.chunk_shape_of m, R.cfg_ints e "chunk_shape") with
      | Some outer, Some inner when List.length inner = Array.length outer ->
          let inner = Array.of_list inner in
          let per =
            Array.mapi
              (fun i o -> if inner.(i) = 0 then 0 else o / inner.(i))
              outer
          in
          let index_codecs =
            Option.value ~default:[] (R.cfg_exts e "index_codecs")
          in
          Some
            {
              s_shape = outer;
              s_inner = inner;
              s_per = per;
              s_count = R.product per;
              s_location =
                Option.value ~default:"end" (R.cfg_string e "index_location");
              s_index = index_bytes ~per ~index_codecs;
            }
      | _ -> None)

(* {1 Fill values} *)

(* Re-encoding through the data type puts the value into the lexicon
   the specification defines, so a canonical NaN prints as "NaN" and not
   as whatever the writer chose. A data type this library does not know
   leaves the member as it was written. *)
let fill_value (m : Metadata.array_meta) =
  match Dtype.of_name m.data_type.Ext.name with
  | None -> R.json_string m.fill_value
  | Some dt -> (
      match Fill_value.of_json dt m.fill_value with
      | Ok fv -> R.json_string (Fill_value.to_json dt fv)
      | Error _ -> R.json_string m.fill_value)

(* {1 tree} *)

let source_name = function
  | W.From_consolidated -> "consolidated"
  | W.From_listing -> "listing"
  | W.From_neither -> "none"

let rec count_tree l =
  List.fold_left (fun n (t : R.tree) -> n + 1 + count_tree t.R.kids) 0 l

let tree_footer src ~total ~shown ~depth =
  let at =
    match depth with
    | Some d when shown < total ->
        Printf.sprintf ", %d shown at depth %d" shown d
    | _ -> ""
  in
  match src with
  | W.From_consolidated ->
      Printf.printf "%d nodes in the consolidated metadata at the root%s.\n"
        total at
  | W.From_listing ->
      Printf.printf "%d nodes found by listing store keys%s.\n" total at
  | W.From_neither ->
      print_string
        "Only this node is shown: the root has no consolidated metadata and \
         the store cannot list keys.\n"

type jnode = { j_rel : string; j_meta : W.meta option; j_kids : jnode list }

let rec flat_nodes ~base l =
  List.concat_map
    (fun { j_rel = rel; j_meta = m; j_kids = kids } ->
      let path = W.disp (W.join base rel) in
      let name = W.base_name rel in
      let node =
        match m with
        | None -> R.jobj [ ("path", R.jstr path); ("name", R.jstr name);
                           ("node_type", R.jstr "group");
                           ("metadata_document", R.jbool false) ]
        | Some (`Group _) ->
            R.jobj [ ("path", R.jstr path); ("name", R.jstr name);
                     ("node_type", R.jstr "group");
                     ("metadata_document", R.jbool true) ]
        | Some (`Array (a : Metadata.array_meta)) ->
            R.jobj
              [
                ("path", R.jstr path);
                ("name", R.jstr name);
                ("node_type", R.jstr "array");
                ("metadata_document", R.jbool true);
                ("data_type", R.jstr a.data_type.Ext.name);
                ("shape", R.jints a.shape);
                ( "chunk_shape",
                  R.jopt R.jints (R.chunk_shape_of a) );
                ("codecs", R.jstr (R.codec_summary a.codecs));
              ]
      in
      node :: flat_nodes ~base kids)
    l

(* The same walk as the printed tree, kept apart so that neither shape
   of output has to be derived from the other. *)
let rec json_subtree t idx ~base rel =
  List.map
    (fun k ->
      {
        j_rel = k;
        j_meta = meta_opt t idx ~base k;
        j_kids = json_subtree t idx ~base k;
      })
    (child_paths idx rel)

let tree_cmd spec path depth as_json =
  run spec @@ fun t ->
  let p = W.norm path in
  let self = W.meta_opt_at t ~path:p in
  let src, rels = W.descendants t ~path:p in
  (* A node with neither a document nor a descendant is not there at
     all, which is a failure rather than an empty tree. *)
  if self = None && rels = [] then
    Zarrz.Error.raise_
      (Zarrz.Error.Store (W.meta_key p ^ ": not found"));
  let total = List.length rels in
  let rels =
    match depth with
    | None -> rels
    | Some d -> List.filter (fun r -> W.depth r <= d) rels
  in
  let idx = index rels in
  if as_json then begin
    let nodes = flat_nodes ~base:p (json_subtree t idx ~base:p "") in
    let self_json =
      List.hd
        (flat_nodes ~base:"" [ { j_rel = p; j_meta = self; j_kids = [] } ])
    in
    emit_json
      (R.jobj
         [
           ("store", R.jstr t.W.spec);
           ("path", R.jstr (W.disp p));
           ("discovery", R.jstr (source_name src));
           ("nodes", R.jint total);
           ("shown", R.jint (List.length nodes));
           ("tree", R.jlist (self_json :: nodes));
         ])
  end
  else begin
    let kids = subtree t idx ~base:p "" in
    Printf.printf "%s  %s\n" (W.disp p)
      (match self with None -> R.implicit_summary | Some m -> summary_of m);
    R.print_tree "" kids;
    tree_footer src ~total ~shown:(count_tree kids) ~depth
  end;
  0

(* {1 info} *)

let print_conventions l =
  List.iter
    (fun c -> R.sub "convention" "%s %s %s" c.c_name c.c_uuid c.c_spec)
    l

let print_geoemb g from_root =
  let g : Zarrz_geoemb.t = g in
  R.sub "geoemb declared by" "%s"
    (if from_root then "the root group" else "this node");
  R.sub "geoemb type" "%s" (geo_kind g.kind);
  R.sub "geoemb dimensions" "%d" g.dimensions;
  R.sub "geoemb data type" "%s" g.data_type;
  R.sub "geoemb model" "%s" g.model;
  (match g.gsd with None -> () | Some v -> R.sub "geoemb gsd" "%g m" v);
  (match g.spatial_layout with
  | None -> ()
  | Some l -> R.sub "geoemb layout" "%s" (geo_layout l));
  match g.quantization with
  | None -> ()
  | Some q -> R.sub "geoemb quantisation" "%s" (geo_quant q)

let print_extensions t ~attrs ~unknown ~nc =
  let convs = conventions attrs in
  let geo = geoemb t ~attrs in
  let cons =
    Option.map (fun c -> List.length (Consolidated.paths c)) t.W.cons
  in
  (* The block always prints, since whether the root carries
     consolidated metadata is news either way. *)
  print_string "extensions\n";
  print_conventions convs;
  List.iter
    (fun (n, mu) -> R.sub "unknown member" "%s (must_understand %b)" n mu)
    unknown;
  List.iter (fun (what, n) -> R.sub ("non-core " ^ what) "%s" n) nc;
  (match cons with
  | None -> R.sub "consolidated" "%s" "absent"
  | Some n -> R.sub "consolidated" "present, %d nodes" n);
  match geo with
  | Geo_none -> ()
  | Geo_bad (m, from_root) ->
      R.sub "geoemb declared by" "%s"
        (if from_root then "the root group" else "this node");
      R.sub "geoemb" "declared but not valid: %s" m
  | Geo_ok (g, from_root) -> print_geoemb g from_root

let print_attributes attrs =
  match attrs with
  | Some (Jsont.Object (o, _)) when o <> [] ->
      print_string "attributes\n";
      List.iter
        (fun ((n, _), v) -> R.sub n "%s" (R.elide 56 (R.json_string v)))
        o
  | _ -> R.field "attributes" "%s" "none"

(* A shard carries whole codec chains in its configuration, so those
   two members are printed as chains rather than as one long line of
   JSON. Only those two names are treated this way: a bare string is a
   valid extension point, so any array of strings would otherwise
   decode as a chain. *)
let nested_exts n v =
  if String.equal n "codecs" || String.equal n "index_codecs" then
    match Jsont.Json.decode (Jsont.list Ext.jsont) v with
    | Ok (_ :: _ as l) -> Some l
    | Ok [] | Error _ -> None
  else None

let rec print_exts ~indent l =
  let ind = String.make indent ' ' in
  List.iter
    (fun e ->
      print_string
        (ind ^ e.Ext.name
        ^ (if e.Ext.must_understand then "" else " (must_understand false)")
        ^ "\n");
      List.iter
        (fun ((n, _), v) ->
          match nested_exts n v with
          | Some sub ->
              print_string (ind ^ "  " ^ n ^ "\n");
              print_exts ~indent:(indent + 4) sub
          | None ->
              Printf.printf "%s%s\n"
                (R.pad_to 22 (ind ^ "  " ^ n))
                (R.elide 56 (R.json_string v)))
        (Ext.config_mems e))
    l

let print_codecs exts =
  match exts with
  | [] -> R.field "codecs" "%s" "none"
  | l ->
      print_string "codecs\n";
      print_exts ~indent:2 l

let print_shard s =
  print_string "shard geometry\n";
  R.sub "shard shape" "%s" (R.shape s.s_shape);
  R.sub "inner chunk shape" "%s" (R.shape s.s_inner);
  R.sub "inner chunks" "%s (%d)" (R.shape s.s_per) s.s_count;
  R.sub "index location" "%s" s.s_location;
  match s.s_index with
  | None -> R.sub "index size" "%s" "unknown, the index chain has no fixed size"
  | Some n -> R.sub "index size" "%d bytes" n

let dim_names = function
  | None -> "none"
  | Some l ->
      String.concat " " (List.map (Option.value ~default:"_") l)

let info_array t (m : Metadata.array_meta) =
  let elements = R.product m.shape in
  R.field "node type" "%s" "array";
  R.field "data type" "%s" m.data_type.Ext.name;
  R.field "shape" "%s" (R.shape m.shape);
  R.field "elements" "%d" elements;
  (match Dtype.of_name m.data_type.Ext.name with
  | None -> ()
  | Some dt ->
      R.field "nominal size" "%s" (R.human_bytes (elements * Dtype.size dt)));
  R.field "dimension names" "%s" (dim_names m.dimension_names);
  R.field "chunk grid" "%s" m.chunk_grid.Ext.name;
  (match R.chunk_shape_of m with
  | None -> ()
  | Some c -> R.field "chunk shape" "%s" (R.shape c));
  (match R.grid_shape_of m with
  | None -> ()
  | Some g ->
      R.field "grid shape" "%s" (R.shape g);
      R.field "chunk count" "%d" (R.product g));
  (match Chunk_key.of_ext m.chunk_key_encoding with
  | Ok (Chunk_key.Default { separator } | Chunk_key.V2 { separator }) ->
      R.field "chunk key encoding" "%s separator %C"
        m.chunk_key_encoding.Ext.name separator
  | Error _ -> R.field "chunk key encoding" "%s" m.chunk_key_encoding.Ext.name);
  R.field "fill value" "%s" (fill_value m);
  print_codecs m.codecs;
  (match shard_of m with None -> () | Some s -> print_shard s);
  R.field "storage transformers" "%s"
    (match m.storage_transformers with
    | [] -> "none"
    | l -> String.concat " " (List.map (fun e -> e.Ext.name) l));
  print_attributes m.attributes;
  print_extensions t ~attrs:m.attributes ~unknown:(unknown_members m.unknown)
    ~nc:(non_core m)

let info_group t ~path (m : Metadata.group_meta) =
  R.field "node type" "%s" "group";
  (match child_kinds t ~path with
  | None -> R.field "children" "%s" "unknown, the store cannot list keys"
  | Some l ->
      let arrays = List.length (List.filter (fun (_, k) -> k = `Array) l) in
      let groups = List.length l - arrays in
      R.field "children" "%d (%d groups, %d arrays)" (List.length l) groups
        arrays);
  print_attributes m.group_attributes;
  print_extensions t ~attrs:m.group_attributes
    ~unknown:(unknown_members m.group_unknown) ~nc:[]

let jconvs l =
  R.jlist
    (List.map
       (fun c ->
         R.jobj
           [
             ("name", R.jstr c.c_name);
             ("uuid", R.jstr c.c_uuid);
             ("spec_url", R.jstr c.c_spec);
           ])
       l)

let jgeoemb = function
  | Geo_none -> R.jnull
  | Geo_bad (m, from_root) ->
      R.jobj
        [
          ("valid", R.jbool false);
          ("error", R.jstr m);
          ("from_root", R.jbool from_root);
        ]
  | Geo_ok (g, from_root) ->
      R.jobj
        [
          ("valid", R.jbool true);
          ("from_root", R.jbool from_root);
          ("type", R.jstr (geo_kind g.Zarrz_geoemb.kind));
          ("dimensions", R.jint g.Zarrz_geoemb.dimensions);
          ("data_type", R.jstr g.Zarrz_geoemb.data_type);
          ("model", R.jstr g.Zarrz_geoemb.model);
          ("gsd", R.jopt R.jnum g.Zarrz_geoemb.gsd);
          ( "spatial_layout",
            R.jopt
              (fun l -> R.jstr (geo_layout l))
              g.Zarrz_geoemb.spatial_layout );
          ( "quantisation",
            R.jopt
              (fun q -> R.jstr (geo_quant q))
              g.Zarrz_geoemb.quantization );
        ]

let jextensions t ~attrs ~unknown ~nc =
  R.jobj
    [
      ("conventions", jconvs (conventions attrs));
      ( "unknown_members",
        R.jlist
          (List.map
             (fun (n, mu) ->
               R.jobj
                 [ ("name", R.jstr n); ("must_understand", R.jbool mu) ])
             unknown) );
      ( "non_core",
        R.jlist
          (List.map
             (fun (what, n) ->
               R.jobj [ ("kind", R.jstr what); ("name", R.jstr n) ])
             nc) );
      ( "consolidated_metadata",
        R.jopt
          (fun c -> R.jint (List.length (Consolidated.paths c)))
          t.W.cons );
      ("geoemb", jgeoemb (geoemb t ~attrs));
    ]

let jexts l =
  match Jsont.Json.encode (Jsont.list Ext.jsont) l with
  | Ok j -> j
  | Error _ -> R.jlist []

let jinfo_array t ~path (m : Metadata.array_meta) =
  let elements = R.product m.shape in
  let dt = Dtype.of_name m.data_type.Ext.name in
  R.jobj
    [
      ("store", R.jstr t.W.spec);
      ("path", R.jstr (W.disp path));
      ("node_type", R.jstr "array");
      ("data_type", R.jstr m.data_type.Ext.name);
      ("shape", R.jints m.shape);
      ("elements", R.jint elements);
      ( "nominal_bytes",
        R.jopt (fun d -> R.jint (elements * Dtype.size d)) dt );
      ( "dimension_names",
        R.jopt
          (fun l -> R.jlist (List.map (R.jopt R.jstr) l))
          m.dimension_names );
      ("chunk_grid", R.jstr m.chunk_grid.Ext.name);
      ("chunk_shape", R.jopt R.jints (R.chunk_shape_of m));
      ("grid_shape", R.jopt R.jints (R.grid_shape_of m));
      ("chunk_key_encoding", R.jstr m.chunk_key_encoding.Ext.name);
      ("fill_value", m.fill_value);
      ("codecs", jexts m.codecs);
      ("codec_summary", R.jstr (R.codec_summary m.codecs));
      ( "shard",
        R.jopt
          (fun s ->
            R.jobj
              [
                ("shard_shape", R.jints s.s_shape);
                ("inner_chunk_shape", R.jints s.s_inner);
                ("inner_chunks", R.jints s.s_per);
                ("inner_chunk_count", R.jint s.s_count);
                ("index_location", R.jstr s.s_location);
                ("index_bytes", R.jopt R.jint s.s_index);
              ])
          (shard_of m) );
      ("storage_transformers", jexts m.storage_transformers);
      ("attributes", Option.value ~default:R.jnull m.attributes);
      ( "extensions",
        jextensions t ~attrs:m.attributes ~unknown:(unknown_members m.unknown)
          ~nc:(non_core m) );
    ]

let jinfo_group t ~path (m : Metadata.group_meta) =
  R.jobj
    [
      ("store", R.jstr t.W.spec);
      ("path", R.jstr (W.disp path));
      ("node_type", R.jstr "group");
      ( "children",
        R.jopt
          (fun l ->
            R.jlist
              (List.map
                 (fun (n, k) ->
                   R.jobj
                     [
                       ("name", R.jstr n);
                       ( "node_type",
                         R.jstr
                           (match k with
                           | `Array -> "array"
                           | `Group -> "group") );
                     ])
                 l))
          (child_kinds t ~path) );
      ("attributes", Option.value ~default:R.jnull m.group_attributes);
      ( "extensions",
        jextensions t ~attrs:m.group_attributes
          ~unknown:(unknown_members m.group_unknown) ~nc:[] );
    ]

let info_cmd spec path as_json =
  run spec @@ fun t ->
  let p = W.norm path in
  let m = W.meta_at t ~path:p in
  if as_json then
    emit_json
      (match m with
      | `Array a -> jinfo_array t ~path:p a
      | `Group g -> jinfo_group t ~path:p g)
  else begin
    R.field "store" "%s" t.W.spec;
    R.field "path" "%s" (W.disp p);
    match m with
    | `Array a -> info_array t a
    | `Group g -> info_group t ~path:p g
  end;
  0

(* {1 stats} *)

type row = {
  r_path : string;
  r_elements : int;
  r_nominal : int option;
  r_chunks : int;
  r_inner : int option;
  r_objects : int option;
  r_stored : int option;
  r_sampled : int option;
  r_found : int option;
  r_estimate : int option;
}

type tier = Nominal | Exact | Sampled

(* Evenly spaced through the grid in C order, so the sample is spread
   over the array rather than over one corner of it, and is the same
   sample on every run. *)
let sample_indices ~total ~n =
  let l = List.init n (fun i -> i * total / n) in
  List.sort_uniq Int.compare l

let unlinear gs lin =
  let d = Array.length gs in
  let out = Array.make d 0 in
  let r = ref lin in
  for i = d - 1 downto 0 do
    out.(i) <- !r mod gs.(i);
    r := !r / gs.(i)
  done;
  out

let sample_array t ~path (m : Metadata.array_meta) ~n =
  match
    ( Chunk_grid.of_ext m.chunk_grid ~array_shape:m.shape,
      Chunk_key.of_ext m.chunk_key_encoding )
  with
  | Ok g, Ok ke ->
      let gs = Chunk_grid.grid_shape g in
      let total = R.product gs in
      if total = 0 then Some (0, 0, 0)
      else
        let idx = sample_indices ~total ~n in
        let sizes =
          List.filter_map
            (fun lin ->
              let key =
                Chunk_key.data_key ~path:(W.disp path)
                  (Chunk_key.encode ke (unlinear gs lin))
              in
              t.W.store.Store.size ~key)
            idx
        in
        Some
          ( List.length idx,
            List.length sizes,
            List.fold_left ( + ) 0 sizes )
  | _ -> None

let exact_array t ~path =
  match t.W.store.Store.list with
  | None -> None
  | Some list ->
      let prefix = W.data_prefix path in
      let keys =
        List.filter
          (fun k -> not (String.equal (Filename.basename k) "zarr.json"))
          (list ~prefix)
      in
      let bytes =
        List.fold_left
          (fun acc k ->
            acc + Option.value ~default:0 (t.W.store.Store.size ~key:k))
          0 keys
      in
      Some (List.length keys, bytes)

let arrays_under t ~path =
  let src, rels = W.descendants t ~path in
  let self =
    match W.meta_opt_at t ~path with
    | Some (`Array m) -> [ (path, m) ]
    | Some (`Group _) | None -> []
  in
  let rest =
    List.filter_map
      (fun r ->
        let full = W.join path r in
        match W.meta_at t ~path:full with
        | `Array m -> Some (full, m)
        | `Group _ -> None)
      rels
  in
  (src, self @ rest)

let row_of t ~tier ~sample (path, (m : Metadata.array_meta)) =
  let elements = R.product m.shape in
  let nominal =
    Option.map
      (fun d -> elements * Dtype.size d)
      (Dtype.of_name m.data_type.Ext.name)
  in
  let chunks =
    match R.grid_shape_of m with Some g -> R.product g | None -> 0
  in
  let inner = Option.map (fun s -> s.s_count) (shard_of m) in
  let base =
    {
      r_path = W.disp path;
      r_elements = elements;
      r_nominal = nominal;
      r_chunks = chunks;
      r_inner = inner;
      r_objects = None;
      r_stored = None;
      r_sampled = None;
      r_found = None;
      r_estimate = None;
    }
  in
  match tier with
  | Nominal -> base
  | Exact -> (
      match exact_array t ~path with
      | None -> base
      | Some (n, bytes) ->
          { base with r_objects = Some n; r_stored = Some bytes })
  | Sampled -> (
      match sample_array t ~path m ~n:sample with
      | None -> base
      | Some (asked, found, bytes) ->
          let est =
            if found = 0 then 0
            else int_of_float (float_of_int bytes /. float_of_int found
                               *. float_of_int chunks)
          in
          {
            base with
            r_sampled = Some asked;
            r_found = Some found;
            r_stored = Some bytes;
            r_estimate = Some est;
          })

let sum f l = List.fold_left (fun acc r -> acc + f r) 0 l
let sum_opt f l =
  List.fold_left (fun acc r -> acc + Option.value ~default:0 (f r)) 0 l

let cell_opt f = function None -> "-" | Some v -> f v

let mean_cell r =
  match (r.r_found, r.r_stored) with
  | Some f, Some b when f > 0 -> R.human_bytes (b / f)
  | _ -> "-"

let stats_table tier rows =
  let headers, cells =
    match tier with
    | Nominal ->
        ( [ "node"; "elements"; "nominal"; "chunks"; "inner" ],
          fun r ->
            [
              r.r_path;
              string_of_int r.r_elements;
              cell_opt R.human_bytes r.r_nominal;
              string_of_int r.r_chunks;
              cell_opt string_of_int r.r_inner;
            ] )
    | Exact ->
        ( [ "node"; "elements"; "nominal"; "chunks"; "inner"; "objects";
            "stored"; "ratio" ],
          fun r ->
            [
              r.r_path;
              string_of_int r.r_elements;
              cell_opt R.human_bytes r.r_nominal;
              string_of_int r.r_chunks;
              cell_opt string_of_int r.r_inner;
              cell_opt string_of_int r.r_objects;
              cell_opt R.human_bytes r.r_stored;
              (match (r.r_stored, r.r_nominal) with
              | Some s, Some n -> R.ratio ~stored:s ~nominal:n
              | _ -> "-");
            ] )
    | Sampled ->
        ( [ "node"; "elements"; "nominal"; "chunks"; "inner"; "sampled";
            "found"; "mean"; "estimate"; "ratio" ],
          fun r ->
            [
              r.r_path;
              string_of_int r.r_elements;
              cell_opt R.human_bytes r.r_nominal;
              string_of_int r.r_chunks;
              cell_opt string_of_int r.r_inner;
              cell_opt string_of_int r.r_sampled;
              cell_opt string_of_int r.r_found;
              mean_cell r;
              cell_opt R.human_bytes r.r_estimate;
              (match (r.r_estimate, r.r_nominal) with
              | Some s, Some n -> R.ratio ~stored:s ~nominal:n
              | _ -> "-");
            ] )
  in
  R.table ~headers (List.map cells rows)

let total_row rows =
  {
    r_path = "total";
    r_elements = sum (fun r -> r.r_elements) rows;
    r_nominal = Some (sum_opt (fun r -> r.r_nominal) rows);
    r_chunks = sum (fun r -> r.r_chunks) rows;
    r_inner = None;
    r_objects =
      (if List.exists (fun r -> r.r_objects <> None) rows then
         Some (sum_opt (fun r -> r.r_objects) rows)
       else None);
    r_stored =
      (if List.exists (fun r -> r.r_stored <> None) rows then
         Some (sum_opt (fun r -> r.r_stored) rows)
       else None);
    r_sampled =
      (if List.exists (fun r -> r.r_sampled <> None) rows then
         Some (sum_opt (fun r -> r.r_sampled) rows)
       else None);
    r_found =
      (if List.exists (fun r -> r.r_found <> None) rows then
         Some (sum_opt (fun r -> r.r_found) rows)
       else None);
    r_estimate =
      (if List.exists (fun r -> r.r_estimate <> None) rows then
         Some (sum_opt (fun r -> r.r_estimate) rows)
       else None);
  }

let jrow r =
  R.jobj
    [
      ("path", R.jstr r.r_path);
      ("elements", R.jint r.r_elements);
      ("nominal_bytes", R.jopt R.jint r.r_nominal);
      ("chunks", R.jint r.r_chunks);
      ("inner_chunks_per_shard", R.jopt R.jint r.r_inner);
      ("objects", R.jopt R.jint r.r_objects);
      ("stored_bytes", R.jopt R.jint r.r_stored);
      ("sampled", R.jopt R.jint r.r_sampled);
      ("found", R.jopt R.jint r.r_found);
      ("estimated_bytes", R.jopt R.jint r.r_estimate);
    ]

let tier_name = function
  | Nominal -> "metadata"
  | Exact -> "listing"
  | Sampled -> "sample"

let stats_footer tier ~sample ~ignored =
  (match tier with
  | Nominal ->
      print_string
        "Sizes are nominal: the store neither lists its keys nor was \
         sampled.\n"
  | Exact ->
      print_string "Stored sizes are exact, from a walk of the store's keys.\n"
  | Sampled ->
      Printf.printf
        "Stored sizes are a sampled estimate from %d size requests \
         per array.\n"
        sample);
  if ignored then
    print_string
      "The store lists its keys, so --sample was not used.\n"

let stats_cmd spec path sample as_json =
  run spec @@ fun t ->
  let p = W.norm path in
  let src, arrays = arrays_under t ~path:p in
  let listable = t.W.store.Store.list <> None in
  let tier =
    if listable then Exact else if sample > 0 then Sampled else Nominal
  in
  let rows = List.map (row_of t ~tier ~sample) arrays in
  if as_json then
    emit_json
      (R.jobj
         [
           ("store", R.jstr t.W.spec);
           ("path", R.jstr (W.disp p));
           ("discovery", R.jstr (source_name src));
           ("tier", R.jstr (tier_name tier));
           ("sample", R.jint (if tier = Sampled then sample else 0));
           ("arrays", R.jlist (List.map jrow rows));
           ("total", jrow (total_row rows));
         ])
  else if rows = [] then print_string "No array below this path.\n"
  else begin
    stats_table tier (rows @ [ total_row rows ]);
    stats_footer tier ~sample ~ignored:(listable && sample > 0)
  end;
  0

(* {1 Arguments} *)

let store_t =
  let doc =
    "Store to inspect. An $(b,https://) or $(b,http://) value is fetched \
     over HTTP, anything else is a local directory."
  in
  Arg.(required & pos 0 (some string) None & info [] ~docv:"STORE" ~doc)

let path_t =
  let doc = "Node path within the store. A leading $(b,/) is optional." in
  Arg.(value & pos 1 string "/" & info [] ~docv:"PATH" ~doc)

(* A count, rejected at the parser rather than by the command, so that
   a bad one is a usage error with the usual cmdliner message. *)
let count =
  let parse s =
    match int_of_string_opt s with
    | Some n when n >= 0 -> Ok n
    | Some n -> Error (`Msg (Printf.sprintf "%d is negative" n))
    | None -> Error (`Msg (Printf.sprintf "%S is not a whole number" s))
  in
  Arg.conv ~docv:"N" (parse, Format.pp_print_int)

let depth_t =
  let doc =
    "Show at most $(docv) levels below $(i,PATH). Every level is shown by \
     default."
  in
  Arg.(value & opt (some count) None & info [ "depth" ] ~docv:"N" ~doc)

let sample_t =
  let doc =
    "Ask the store for the size of $(docv) chunks of each array, evenly \
     spaced through its grid. $(b,0), the default, makes no request at \
     all. Ignored on a store that lists its keys, which is measured \
     exactly instead."
  in
  Arg.(value & opt count 0 & info [ "sample" ] ~docv:"N" ~doc)

let json_t =
  let doc = "Print one JSON document instead of aligned text." in
  Arg.(value & flag & info [ "json" ] ~doc)

(* {1 Manuals} *)

let exits =
  Cmd.Exit.info exit_failure
    ~doc:"on a store, metadata or argument failure."
  :: Cmd.Exit.defaults

let common_man =
  [
    `S Manpage.s_common_options;
    `S Manpage.s_exit_status;
    `S Manpage.s_bugs;
    `P "Report issues at $(b,https://anil.recoil.org/ocaml-zarrz).";
  ]

let network_man =
  `P
    "An $(b,https://) store is read with metadata requests alone by \
     $(b,tree) and $(b,info). Only $(b,zarr stats --sample) asks for \
     anything else, and what it asks for is one size request a sampled \
     chunk. Range support is therefore needed for sampling and for \
     nothing else."

let discovery_man =
  `P
    "The hierarchy is found in one of three ways, tried in order and \
     named in the output. Consolidated metadata in the root document \
     describes every node, so one request covers the whole store. \
     Failing that, a store that lists its keys is walked for \
     $(b,zarr.json) documents, one read a node. Failing both, only the \
     named node can be shown."

let tree_cmd_t =
  let doc = "Print the hierarchy below a node." in
  let man =
    `S Manpage.s_description
    :: `P
         "Prints one line a node: its name, whether it is an array or a \
          group, and for an array its data type, shape, chunk shape and \
          codec chain."
    :: `P
         "A codec is its name and the part of its configuration a reader \
          chooses a store by. A shard spells its inner chunk shape, its \
          inner chain, then its index chain after a bar. Two examples:"
    :: `Pre "  bytes(le) zstd(3)"
    :: `Pre "  sharding(4x4; bytes(le) gzip(5) | idx bytes(le) crc32c)"
    :: discovery_man :: network_man
    :: `P
         "$(b,--json) prints an object whose $(b,tree) member is the \
          nodes in print order, the named node first, each with its full \
          path, so a script can rebuild the shape from the paths."
    :: `S Manpage.s_examples
    :: `Pre
         "  zarr tree https://data.source.coop/tessera/tessera/zarr/v1 \
          --depth 1"
    :: `Pre "  zarr tree ./hierarchy.zarr /a"
    :: common_man
  in
  Cmd.v
    (Cmd.info "tree" ~doc ~man ~exits)
    Term.(const tree_cmd $ store_t $ path_t $ depth_t $ json_t)

let info_cmd_t =
  let doc = "Print one node in full." in
  let man =
    `S Manpage.s_description
    :: `P
         "For an array: shape, data type, fill value in the lexicon the \
          specification defines, chunk grid, chunk key encoding, every \
          codec with every member of its configuration, shard geometry \
          when the chain is $(b,sharding_indexed), dimension names, \
          storage transformers and attributes. For a group: attributes \
          and the number of children by kind."
    :: `P
         "Both end with an extensions block: every $(b,zarr_conventions) \
          entry, every metadata member this library does not model with \
          its $(b,must_understand), whether the root carries \
          consolidated metadata, and any data type, chunk grid or chunk \
          key encoding outside the core. A store declaring the \
          geoembeddings convention adds its parsed summary, taken from \
          the node's own attributes or, failing that, from the root \
          group's."
    :: `P
         "An attribute value longer than 56 bytes is cut short and its \
          length reported. $(b,--json) prints the attributes whole, \
          beside the same fields under stable member names."
    :: `P
         "The root document is read whichever node is named, since it is \
          the only place consolidated metadata and a hierarchy-wide \
          convention can be."
    :: network_man :: `S Manpage.s_examples
    :: `Pre "  zarr info https://data.source.coop/tessera/tessera/zarr/v1"
    :: `Pre "  zarr info ./sharded.zarr /group/array"
    :: common_man
  in
  Cmd.v
    (Cmd.info "info" ~doc ~man ~exits)
    Term.(const info_cmd $ store_t $ path_t $ json_t)

let stats_cmd_t =
  let doc = "Report what the arrays below a node cost." in
  let man =
    `S Manpage.s_description
    :: `P
         "One row an array and a total row. From metadata alone: element \
          count, nominal size, the number of chunks the grid has, and \
          for a sharded array the number of inner chunks in one shard. A \
          chunk of a sharded array is a shard, so the chunk count is \
          also the number of stored objects."
    :: `P
         "A store that lists its keys adds the exact number of stored \
          objects below each array, their total size and the ratio to \
          nominal. A store that does not, with $(b,--sample N), adds the \
          size of N evenly spaced chunks of each array, how many of \
          them were there, their mean size and a total extrapolated from \
          it. The spacing is fixed, so the same store answers the same \
          way every run."
    :: `P
         "A ratio above $(b,1.00x) means the stored bytes exceed the \
          nominal ones, which a compressor can do on data that does not \
          compress."
    :: discovery_man :: network_man
    :: `S Manpage.s_examples
    :: `Pre
         "  zarr stats https://data.source.coop/tessera/tessera/zarr/v1 \
          /utm30/embeddings --sample 8"
    :: `Pre "  zarr stats ./sharded.zarr"
    :: common_man
  in
  Cmd.v
    (Cmd.info "stats" ~doc ~man ~exits)
    Term.(const stats_cmd $ store_t $ path_t $ sample_t $ json_t)

let main =
  let doc = "Inspect a Zarr V3 store." in
  let man =
    [
      `S Manpage.s_description;
      `P
        "Answers three questions about a store a user has only a URL \
         for: what is in it, what one node is exactly, and what it costs \
         to read. $(b,STORE) is an $(b,https://) or $(b,http://) URL or \
         a local directory, and $(b,PATH) is a node within it, the root \
         by default.";
      `P
        "Output is aligned plain text on stdout. $(b,--json) prints one \
         JSON document instead, so a script reads the same information. \
         A failure is one line on stderr and exit 1.";
      `S Manpage.s_common_options;
      `S Manpage.s_exit_status;
    ]
  in
  Cmd.group
    (Cmd.info "zarr" ~version ~doc ~man ~exits)
    [ tree_cmd_t; info_cmd_t; stats_cmd_t ]

let () =
  match Cmd.eval_value main with
  | Ok (`Ok code) -> exit code
  | Ok (`Help | `Version) -> exit Cmd.Exit.ok
  | Error _ -> exit Cmd.Exit.cli_error
