(** {1 Zarr v3 -- Pure OCaml Implementation}

    Core library for Zarr v3.1 chunked, compressed N-dimensional arrays.

    This library provides types, codecs, metadata handling, and
    array/group operations with {b no I/O dependencies}.  Store
    implementations (memory, filesystem, etc.) are provided by companion
    packages ([zarr-sync], [zarr-eio]).

    {b Optimisations:} bytes-backed chunk storage (not Bigarray), OxCaml
    [let mutable] loops, [\[@inline\]] element access, contiguous
    [Bytes.blit] for slice operations, and doubling-copy fill replication.

    @see <https://zarr-specs.readthedocs.io/en/latest/v3/core/index.html>
    Zarr v3.1 Specification

    {2 Quick start}

    {[
      (* Create an array *)
      let store = Memory_store.create () in
      let arr = Memory_array.create store ~path:"data"
        ~shape:[|100; 100|] ~chunks:[|10; 10|] ~dtype:Zarr.Dtype.Float64
        () in

      (* Write a slice *)
      let data = Chunk_data.create_zero Zarr.Dtype.Float64 [|5; 5|] in
      Memory_array.set_slice arr [Zarr.range 0 5; Zarr.range 0 5] data;

      (* Read it back *)
      let result = Memory_array.get_slice arr [Zarr.all; Zarr.all] in
      ...
    ]} *)

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

(** {2 Metadata} *)

module Metadata = Zarr_metadata

(** {2 Store} *)

module Store = Zarr_store

(** {2 Array and Group Operations} *)

module Array = Zarr_array
module Group = Zarr_group

(** {2 JSON Utilities} *)

(** Internal JSON helpers for constructing and destructuring {!Jsont.json}
    values. Useful for building attribute objects. *)
module Json_util = Json_util

(** {2 Convenience Constructors}

    Shortcuts for building codec specifications. *)

val default_codecs : Zarr_codec.codec_spec list
(** Default codec chain: bytes in little-endian order. *)

val bytes_codec : ?endian:Zarr_dtype.endian -> unit -> Zarr_codec.codec_spec
(** [bytes_codec ()] is a bytes codec spec.  Default endianness is
    little-endian. *)

val gzip_codec : ?level:int -> unit -> Zarr_codec.codec_spec
(** [gzip_codec ()] is a gzip codec spec.  Default level is 5. *)

val zstd_codec : ?level:int -> ?checksum:bool -> unit -> Zarr_codec.codec_spec
(** [zstd_codec ()] is a zstd codec spec.  Default level 3, no
    checksum. *)

val crc32c_codec : unit -> Zarr_codec.codec_spec
(** [crc32c_codec ()] is a CRC32C checksum codec spec. *)

val transpose_codec : int array -> Zarr_codec.codec_spec
(** [transpose_codec order] is a transpose codec spec.  [order] maps
    new dimension [i] to old dimension [order.(i)]. *)

val sharding_codec :
  chunk_shape:int array ->
  ?codecs:Zarr_codec.codec_spec list ->
  ?index_codecs:Zarr_codec.codec_spec list ->
  ?index_location:Zarr_codec.index_location ->
  unit -> Zarr_codec.codec_spec
(** [sharding_codec ~chunk_shape ()] is a sharding codec spec.

    Defaults:
    - Inner codecs: [bytes(little)]
    - Index codecs: [bytes(little) + crc32c]
    - Index location: [End] *)

(** {2 Slice Constructors}

    Convenience wrappers for {!Slice.t} constructors. *)

val idx : int -> Slice.t
val range : int -> int -> Slice.t
val range_from : int -> Slice.t
val range_to : int -> Slice.t
val all : Slice.t
val stepped : int -> int -> int -> Slice.t
