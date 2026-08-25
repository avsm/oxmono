(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

module Ia = Stdlib_stable.Iarray

type t = {
  store : Store.t;
  path : string;
  meta : Metadata.array_meta;
  dtype : Dtype.t;
  fill : Fill_value.t;
  fill_bytes : string;
  grid : Chunk_grid.t;
  key_enc : Chunk_key.t;
  chain : Codec.chain;
  shape : int array;
  chunk_shape : int array;
  grid_shape : int array;
  shape_ia : int iarray;
  chunk_shape_ia : int iarray;
  elem_size : int;
  chunk_elements : int;
}

let meta_err fmt =
  Printf.ksprintf (fun m -> Error.raise_ (Error.Metadata m)) fmt

let codec_err fmt = Printf.ksprintf (fun m -> Error.raise_ (Error.Codec m)) fmt

let of_result f = function Ok v -> v | Error m -> f m

(* [ia_eq a b] is shape equality between the [int iarray] a slab
   carries and the [int array] this module works in. *)
let ia_eq a b =
  Ia.length a = Array.length b
  &&
  let eq = ref true in
  for d = 0 to Array.length b - 1 do
    if Ia.get a d <> b.(d) then eq := false
  done;
  !eq

(* Binding *)

let of_meta ?codecs store ~path (meta : Metadata.array_meta) =
  let dt = meta.data_type in
  if Option.is_some dt.Ext.config then
    meta_err "data type %S: a configuration is not supported" dt.Ext.name;
  if not dt.Ext.must_understand then
    meta_err "data type %S: must_understand must be true" dt.Ext.name;
  let dtype =
    match Dtype.of_name dt.Ext.name with
    | Some d -> d
    | None -> meta_err "unknown data type %S" dt.Ext.name
  in
  List.iter
    (fun (e : Ext.t) ->
      if e.Ext.must_understand then
        meta_err "storage transformer %S is not supported" e.Ext.name)
    meta.storage_transformers;
  let fill =
    of_result
      (fun m -> meta_err "%s" m)
      (Fill_value.of_json dtype meta.fill_value)
  in
  let grid =
    of_result
      (fun m -> meta_err "%s" m)
      (Chunk_grid.of_ext meta.chunk_grid ~array_shape:meta.shape)
  in
  let key_enc =
    of_result
      (fun m -> meta_err "%s" m)
      (Chunk_key.of_ext meta.chunk_key_encoding)
  in
  let chain =
    of_result
      (fun m -> codec_err "%s" m)
      (Codec.chain_of_exts ?resolver:codecs ~dtype ~fill_value:fill meta.codecs)
  in
  let chunk_shape = Chunk_grid.chunk_shape grid in
  let shape = Chunk_grid.array_shape grid in
  {
    store;
    path;
    meta;
    dtype;
    fill;
    fill_bytes = Fill_value.to_bytes fill;
    grid;
    key_enc;
    chain;
    shape;
    chunk_shape;
    grid_shape = Chunk_grid.grid_shape grid;
    shape_ia = Ia.of_array shape;
    chunk_shape_ia = Ia.of_array chunk_shape;
    elem_size = Dtype.size dtype;
    chunk_elements = Array.fold_left ( * ) 1 chunk_shape;
  }

let of_json ?codecs store ~path j =
  of_meta ?codecs store ~path
    (of_result (fun m -> meta_err "%s" m) (Metadata.array_of_json j))

let open_ ?codecs store ~path =
  let key = Chunk_key.meta_key ~path in
  of_json ?codecs store ~path (Store.get_json store ~key)

let store_set store ~key b =
  match store.Store.set with
  | Some f -> f ~key b
  | None -> Error.raise_ (Error.Store "the store does not support writing")

let default_codecs =
  [
    Ext.v "bytes"
      ~config:
        (Jsont.Json.object'
           [ (Jsont.Json.name "endian", Jsont.Json.string "little") ]);
  ]

let create ?attributes ?dimension_names ?codecs ?chunk_key_encoding ?resolver
    ~shape ~chunk_shape ~dtype ~fill_value store ~path =
  let grid =
    of_result
      (fun m -> meta_err "%s" m)
      (Chunk_grid.v ~array_shape:shape ~chunk_shape)
  in
  if Fill_value.length fill_value <> Dtype.size dtype then
    meta_err "fill value of %d bytes for data type %s, which is %d wide"
      (Fill_value.length fill_value) (Dtype.name dtype) (Dtype.size dtype);
  (match dimension_names with
  | Some l when List.length l <> Array.length shape ->
      meta_err "%d dimension names for an array of rank %d" (List.length l)
        (Array.length shape)
  | _ -> ());
  let codecs = match codecs with Some c -> c | None -> default_codecs in
  let key_enc = Option.value chunk_key_encoding ~default:Chunk_key.default in
  let meta : Metadata.array_meta =
    {
      shape = Array.copy shape;
      data_type = Ext.v (Dtype.name dtype);
      chunk_grid = Chunk_grid.to_ext grid;
      chunk_key_encoding = Chunk_key.to_ext key_enc;
      fill_value = Fill_value.to_json dtype fill_value;
      codecs;
      attributes;
      dimension_names;
      storage_transformers = [];
      unknown = [];
    }
  in
  (* Bind before writing, so a chain that cannot be built leaves no
     document behind. *)
  let t = of_meta ?codecs:resolver store ~path meta in
  let json = Metadata.array_to_json meta in
  let s =
    of_result (fun m -> meta_err "%s" m)
      (Jsont_bytesrw.encode_string Jsont.json json)
  in
  store_set store ~key:(Chunk_key.meta_key ~path)
    (Base_bigstring.of_string s);
  t

(* Properties *)

let store t = t.store
let path t = t.path
let metadata t = t.meta
let shape t = Array.copy t.shape
let dtype t = t.dtype
let fill_value t = t.fill
let attributes t = t.meta.attributes
let dimension_names t = t.meta.dimension_names
let chunk_shape t = Array.copy t.chunk_shape
let grid_shape t = Array.copy t.grid_shape

let chunk_key t i =
  Chunk_key.data_key ~path:t.path (Chunk_key.encode t.key_enc i)

(* Chunks *)

let check_index t i =
  let n = Array.length t.grid_shape in
  if Array.length i <> n then
    invalid_arg
      (Printf.sprintf "Zarrz.Arr: chunk index of rank %d, expected %d"
         (Array.length i) n);
  for d = 0 to n - 1 do
    if i.(d) < 0 || i.(d) >= t.grid_shape.(d) then
      invalid_arg
        (Printf.sprintf
           "Zarrz.Arr: chunk index %d in dimension %d, grid extent %d" i.(d) d
           t.grid_shape.(d))
  done

let chunk_repr t = { Codec.dtype = t.dtype; shape = t.chunk_shape }

let fill_chunk t =
  let s = Slab.create t.dtype t.chunk_shape_ia in
  Slab.fill s t.fill_bytes;
  s

let read_chunk_opt t i =
  check_index t i;
  match t.store.Store.get ~key:(chunk_key t i) with
  | None -> None
  | Some b -> Some (Codec.decode_chunk t.chain (chunk_repr t) b)

let read_chunk t i =
  match read_chunk_opt t i with Some s -> s | None -> fill_chunk t

let write_chunk t i s =
  check_index t i;
  if not (Dtype.equal (Slab.dtype s) t.dtype) then
    invalid_arg
      (Printf.sprintf "Zarrz.Arr: slab of %s, array of %s"
         (Dtype.name (Slab.dtype s))
         (Dtype.name t.dtype));
  if not (ia_eq (Slab.shape s) t.chunk_shape) then
    invalid_arg "Zarrz.Arr: slab is not one whole chunk";
  store_set t.store ~key:(chunk_key t i) (Codec.encode_chunk t.chain s)

(* Boxes.

   [intersect] works in array coordinates. Both boxes are half open, so
   an empty overlap in any dimension makes the whole intersection
   empty. *)

let intersect ~start ~shape ~origin ~extent =
  let n = Array.length start in
  let istart = Array.make n 0 and ishape = Array.make n 0 in
  let empty = ref false in
  for d = 0 to n - 1 do
    let lo = if start.(d) > origin.(d) then start.(d) else origin.(d) in
    let a = start.(d) + shape.(d) and b = origin.(d) + extent.(d) in
    let hi = if a < b then a else b in
    istart.(d) <- lo;
    ishape.(d) <- (if hi > lo then hi - lo else 0);
    if hi <= lo then empty := true
  done;
  if !empty then None else Some (istart, ishape)

(* [rel ~base istart shape] moves the box starting at [istart] in array
   coordinates into the frame whose origin is [base], which is a chunk
   origin for a chunk local box and the subset start for a destination
   box. *)
let rel ~base istart shape =
  let n = Array.length istart in
  { Subset.start = Ia.init n (fun d -> istart.(d) - base.(d)); shape }

(* Reading *)

(* [size] is how absence is detected on the ranged path: a store that
   cannot answer it reports [None], which sends the caller down the
   whole fetch path where absence is handled again. *)
let partial_chunk t ~key repr sub =
  match t.store.Store.size ~key with
  | None -> None
  | Some n ->
      let missing () =
        Error.raise_
          (Error.Store (Printf.sprintf "%s: vanished during a ranged read" key))
      in
      let read r =
        match t.store.Store.get_range ~key r with
        | Some b -> b
        | None -> missing ()
      in
      let read_many rs =
        match t.store.Store.get_ranges ~key rs with
        | Some bs -> bs
        | None -> missing ()
      in
      let src = { Byte_source.size = (fun () -> n); read; read_many } in
      Codec.partial_decode t.chain repr src sub

(* The whole array subset assembler. Every chunk contributes the box
   where it meets the subset, expressed once in the chunk's frame and
   once in the subset's. A missing chunk contributes the fill value
   through a single slab built on first need, so a read of an entirely
   absent region allocates one chunk, not one per chunk. *)
let assemble t (sub : Subset.t) start shp =
  let out = Slab.create t.dtype sub.shape in
  let out_buf = Slab.bigstring out in
  let esz = t.elem_size in
  let repr = chunk_repr t in
  (* The gate is decided once. A ranged read costs a [size] request
     before it can start, which is wasted on a chain that cannot decode
     part of a chunk or a store that answers a range by fetching the
     object whole. *)
  let ranged = Codec.supports_partial t.chain && t.store.Store.ranged in
  let tmp = lazy (Base_bigstring.create (t.chunk_elements * esz)) in
  let filled = lazy (fill_chunk t) in
  Chunk_grid.chunks_overlapping t.grid ~start ~shape:shp (fun ci ->
      let origin = Chunk_grid.chunk_origin t.grid ci in
      match intersect ~start ~shape:shp ~origin ~extent:t.chunk_shape with
      | None -> ()
      | Some (istart, ishape) -> (
          let box = Ia.of_array ishape in
          let dst = rel ~base:start istart box in
          let src = rel ~base:origin istart box in
          let key = chunk_key t ci in
          let partial =
            if ranged then partial_chunk t ~key repr src else None
          in
          match partial with
          | Some s ->
              let want = Array.fold_left ( * ) 1 ishape in
              if Slab.num_elements s <> want then
                codec_err "partial decode of %s returned %d elements, wanted %d"
                  key (Slab.num_elements s) want;
              Subset.scatter ~elem_size:esz ~src:(Slab.bigstring s)
                ~dst:out_buf ~outer:sub.shape dst
          | None ->
              let chunk =
                match t.store.Store.get ~key with
                | Some b -> Codec.decode_chunk t.chain repr b
                | None -> Lazy.force filled
              in
              let mid = Lazy.force tmp in
              Subset.gather ~elem_size:esz ~src:(Slab.bigstring chunk)
                ~outer:t.chunk_shape_ia src ~dst:mid;
              Subset.scatter ~elem_size:esz ~src:mid ~dst:out_buf
                ~outer:sub.shape dst));
  out

(* [whole_chunk t start shp] is the grid index of the chunk the subset
   covers exactly, if there is one. Such a chunk decodes straight into
   the result, with no scratch buffer and no assembly. *)
let whole_chunk t start shp =
  let n = Array.length t.chunk_shape in
  let i = Array.make n 0 in
  let ok = ref true in
  for d = 0 to n - 1 do
    let c = t.chunk_shape.(d) in
    if shp.(d) <> c || start.(d) mod c <> 0 then ok := false
    else i.(d) <- start.(d) / c
  done;
  if !ok then Some i else None

let read t (sub : Subset.t) =
  Subset.validate ~outer:t.shape_ia sub;
  let start = Ia.to_array sub.start and shp = Ia.to_array sub.shape in
  match whole_chunk t start shp with
  | Some i -> read_chunk t i
  | None -> assemble t sub start shp

(* Writing *)

let write t (sub : Subset.t) s =
  Subset.validate ~outer:t.shape_ia sub;
  if not (Dtype.equal (Slab.dtype s) t.dtype) then
    invalid_arg
      (Printf.sprintf "Zarrz.Arr: slab of %s, array of %s"
         (Dtype.name (Slab.dtype s))
         (Dtype.name t.dtype));
  let start = Ia.to_array sub.start and shp = Ia.to_array sub.shape in
  if not (ia_eq (Slab.shape s) shp) then
    invalid_arg "Zarrz.Arr: slab shape is not the subset shape";
  let esz = t.elem_size in
  let src_buf = Slab.bigstring s in
  let tmp = lazy (Base_bigstring.create (t.chunk_elements * esz)) in
  Chunk_grid.chunks_overlapping t.grid ~start ~shape:shp (fun ci ->
      let origin = Chunk_grid.chunk_origin t.grid ci in
      match intersect ~start ~shape:shp ~origin ~extent:t.chunk_shape with
      | None -> ()
      | Some (istart, ishape) ->
          let box = Ia.of_array ishape in
          let from = rel ~base:start istart box in
          let covered = ref true in
          for d = 0 to Array.length ishape - 1 do
            if ishape.(d) <> t.chunk_shape.(d) then covered := false
          done;
          if !covered then begin
            (* The subset owns every element of the chunk, so nothing
               has to be read back first. *)
            let chunk = Slab.create t.dtype t.chunk_shape_ia in
            Subset.gather ~elem_size:esz ~src:src_buf ~outer:sub.shape from
              ~dst:(Slab.bigstring chunk);
            write_chunk t ci chunk
          end
          else begin
            (* Read modify write. The elements outside the subset, and
               the part of an edge chunk beyond the array, keep what the
               store had, or the fill value when it had nothing. *)
            let chunk = read_chunk t ci in
            let mid = Lazy.force tmp in
            Subset.gather ~elem_size:esz ~src:src_buf ~outer:sub.shape from
              ~dst:mid;
            Subset.scatter ~elem_size:esz ~src:mid
              ~dst:(Slab.bigstring chunk) ~outer:t.chunk_shape_ia
              (rel ~base:origin istart box);
            write_chunk t ci chunk
          end)
