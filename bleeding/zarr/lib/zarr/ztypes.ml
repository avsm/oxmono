(** Core type definitions for Zarr v3 *)

(** Data types supported by Zarr v3 *)
module Dtype = struct
  type t =
    | Bool
    | Int8
    | Int16
    | Int32
    | Int64
    | Uint8
    | Uint16
    | Uint32
    | Uint64
    | Float16
    | Float32
    | Float64
    | Complex64
    | Complex128
    | Raw of int  (** r8, r16, etc. - raw bytes with specified bit width *)
end

(** Byte order for multi-byte data types *)
module Endianness = struct
  type t =
    | Little
    | Big
end

(** Fill value representation *)
module Fill_value = struct
  type t =
    | Bool of bool
    | Int of int64
    | Uint of int64
    | Float of float
    | Complex of float * float
    | Raw of bytes
    | NaN
    | Infinity
    | NegInfinity
    | Hex of string  (** For custom NaN bit patterns like "0x7fc00000" *)
end

(** Separator character for chunk key encoding *)
module Separator = struct
  type t =
    | Slash
    | Dot
end

(** Index location for sharding codec *)
module Index_location = struct
  type t =
    | Start
    | End
end

(** Error types for Zarr operations *)
type error =
  [ `Invalid_metadata of string
  | `Unsupported_dtype of string
  | `Codec_error of string
  | `Store_error of string
  | `Not_found of string
  | `Invalid_slice of string
  | `Checksum_mismatch
  | `Invalid_chunk_coords of string
  | `Shape_mismatch of string
  ]

(** Chunk grid configuration *)
type chunk_grid_config = {
  chunk_shape : int array;
}

(** Chunk grid types *)
type chunk_grid =
  | Regular of chunk_grid_config

(** Chunk key encoding schemes *)
type chunk_key_encoding =
  | Default of { separator : Separator.t }
  | V2 of { separator : Separator.t }

(** Codec specifications - parsed from JSON *)
type codec_spec =
  | Bytes of { endian : Endianness.t option }  (** None means native endianness *)
  | Transpose of { order : int array }
  | Gzip of { level : int }
  | Zstd of { level : int; checksum : bool }
  | Crc32c
  | Sharding of {
      chunk_shape : int array;
      codecs : codec_spec list;
      index_codecs : codec_spec list;
      index_location : Index_location.t;
    }
  | Extension of { name : string; config : Yojson.Safe.t }

(** Array metadata as defined by Zarr v3 spec *)
type array_metadata = {
  zarr_format : int;
  node_type : [ `Array ];
  shape : int array;
  data_type : Dtype.t;
  chunk_grid : chunk_grid;
  chunk_key_encoding : chunk_key_encoding;
  fill_value : Fill_value.t;
  codecs : codec_spec list;
  dimension_names : string option array option;
  attributes : Yojson.Safe.t option;
}

(** Group metadata as defined by Zarr v3 spec *)
type group_metadata = {
  zarr_format : int;
  node_type : [ `Group ];
  attributes : Yojson.Safe.t option;
}

(** Slice specification for array indexing *)
type slice =
  | Index of int          (** Single index *)
  | Range of int * int    (** Start (inclusive) to stop (exclusive) *)
  | RangeFrom of int      (** From start to end of dimension *)
  | RangeTo of int        (** From beginning to stop (exclusive) *)
  | All                   (** Entire dimension *)
  | Stepped of int * int * int  (** Start, stop, step *)

(** Result type alias for convenience *)
type 'a result = ('a, error) Stdlib.result
