(** {1 Zarr v3 -- Pure OCaml Implementation}

    Core library for Zarr v3.1 chunked, compressed N-dimensional arrays.
    Provides types, codecs, metadata handling, and array/group operations
    with no I/O dependencies.

    Optimised for the OxCaml compiler with bytes-backed chunk storage,
    [let mutable] loops, and zero-allocation inner paths. *)

(** {2 Data Types} *)

module Dtype = Zarr_dtype
module Fill = Zarr_fill

(** {2 Chunk Storage} *)

module Chunk_data = Chunk_data
module Chunk_grid = Chunk_grid
module Chunk_key = Chunk_key

(** {2 Slicing} *)

module Slice = Slice

(** {2 Codecs} *)

module Codec = Zarr_codec
module Codec_bytes = Codec_bytes
module Codec_transpose = Codec_transpose
module Codec_gzip = Codec_gzip
module Codec_zstd = Codec_zstd
module Codec_crc32c = Codec_crc32c
module Codec_sharding = Codec_sharding

(** Ensure the codec chain builder is initialised at link time. *)
(* Wire up the codec chain builder at library load time.
   This MUST be in zarr.ml because it's the wrapper module and is
   guaranteed to be initialized when any Zarr.* module is accessed. *)
let () =
  Zarr_codec.build_codec_ref := (fun spec dtype chunk_shape fill_value build_chain ->
    match (spec : Zarr_codec.codec_spec) with
    | Bytes { endian } ->
      let endian = match endian with Some e -> e | None -> Zarr_dtype.Little in
      Zarr_codec.ArrayToBytes (Codec_bytes.create endian)
    | Transpose { order } ->
      Zarr_codec.ArrayToArray (Codec_transpose.create order)
    | Gzip { level } ->
      Zarr_codec.BytesToBytes (Codec_gzip.create level)
    | Zstd { level; checksum = _ } ->
      Zarr_codec.BytesToBytes (Codec_zstd.create level)
    | Crc32c ->
      Zarr_codec.BytesToBytes (Codec_crc32c.create ())
    | Sharding { chunk_shape = inner_shape; codecs; index_codecs; index_location } ->
      (* Zarr v3.1 spec: inner chunk shape must evenly divide outer chunk shape *)
      let ndim = Array.length chunk_shape in
      if Array.length inner_shape <> ndim then
        raise (Failure "sharding: inner chunk shape dimensionality must match outer")
      else begin
        for i = 0 to ndim - 1 do
          if inner_shape.(i) <= 0 then
            raise (Failure (Printf.sprintf
              "sharding: inner chunk shape[%d] must be positive" i));
          if chunk_shape.(i) mod inner_shape.(i) <> 0 then
            raise (Failure (Printf.sprintf
              "sharding: inner chunk shape[%d]=%d must evenly divide outer[%d]=%d"
              i inner_shape.(i) i chunk_shape.(i)))
        done
      end;
      let num_inner = Codec_sharding.total_inner_chunks chunk_shape inner_shape in
      let inner_chain = build_chain codecs dtype inner_shape fill_value in
      let index_fv = Zarr_fill.default Zarr_dtype.Uint64 in
      let index_chain = build_chain index_codecs Zarr_dtype.Uint64 [|num_inner * 2|] index_fv in
      (match inner_chain, index_chain with
       | Ok ic, Ok xc ->
         Zarr_codec.ArrayToBytes (Codec_sharding.create
           ~outer_chunk_shape:chunk_shape ~inner_chunk_shape:inner_shape
           ~inner_chain:ic ~index_chain:xc ~index_location ~dtype ~fill_value)
       | Error e, _ -> raise (Failure (match e with `Codec_error m -> m | _ -> "codec error"))
       | _, Error e -> raise (Failure (match e with `Codec_error m -> m | _ -> "codec error")))
    | Extension { name; config } ->
      (match Zarr_codec.find_codec name with
       | Some builder ->
         (match builder config dtype chunk_shape with
          | Ok c -> c
          | Error (`Codec_error msg) -> raise (Failure ("extension '" ^ name ^ "': " ^ msg))
          | Error _ -> raise (Failure ("extension '" ^ name ^ "' error")))
       | None -> raise (Failure ("unknown extension codec: " ^ name))))

(** {2 Metadata} *)

module Metadata = Zarr_metadata

(** {2 Store} *)

module Store = Zarr_store

(** {2 Array and Group Operations} *)

module Array = Zarr_array
module Group = Zarr_group

(** {2 JSON Utilities} *)

module Json_util = Json_util

(** {2 Convenience Constructors} *)

(** Default codec chain: bytes in little-endian order. *)
let default_codecs = [Zarr_codec.Bytes { endian = Some Little }]

let bytes_codec ?(endian = Zarr_dtype.Little) () =
  Zarr_codec.Bytes { endian = Some endian }

let gzip_codec ?(level = 5) () = Zarr_codec.Gzip { level }

let zstd_codec ?(level = 3) ?(checksum = false) () =
  Zarr_codec.Zstd { level; checksum }

let crc32c_codec () = Zarr_codec.Crc32c

let transpose_codec order = Zarr_codec.Transpose { order }

let sharding_codec ~chunk_shape
    ?(codecs = [Zarr_codec.Bytes { endian = Some Little }])
    ?(index_codecs = [Zarr_codec.Bytes { endian = Some Little }; Crc32c])
    ?(index_location = Zarr_codec.End) () =
  Zarr_codec.Sharding { chunk_shape; codecs; index_codecs; index_location }

(** {2 Slice Constructors} *)

let idx i = Slice.Index i
let range s e = Slice.Range (s, e)
let range_from s = Slice.RangeFrom s
let range_to e = Slice.RangeTo e
let all = Slice.All
let stepped s e st = Slice.Stepped (s, e, st)
