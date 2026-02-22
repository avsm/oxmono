(** Zarr v3 - Pure OCaml implementation

    This is the main entry point for the Zarr library, providing
    types, codecs, metadata handling, and array/group operations.
*)

(* Capture module references before include Ztypes shadows them *)
module Fill_value_impl = Fill_value
module Data_type_impl = Data_type

(** {1 Core Types} *)

(** Re-export core types *)
include Ztypes

(** Re-export Ztypes module for access to type constructors *)
module Ztypes = Ztypes

(** {1 Data Types} *)

module Data_type = Data_type_impl

(** {1 Fill Values} *)

module Fill_value = Fill_value_impl

(** {1 N-dimensional Arrays} *)

module Ndarray = Ndarray

(** {1 Indexing and Slicing} *)

module Indexing = Indexing

(** {1 Chunk Grid} *)

module Chunk_grid = Chunk_grid

(** {1 Chunk Key Encoding} *)

module Chunk_key = Chunk_key

(** {1 Codecs} *)

module Codec = Codec
module Codec_intf = Codec_intf
module Codec_registry = Codec_registry

(** Individual codec implementations *)
module Codecs = struct
  module Bytes_codec = Codecs.Bytes_codec
  module Transpose = Codecs.Transpose
  module Gzip = Codecs.Gzip
  module Zstd = Codecs.Zstd
  module Crc32c = Codecs.Crc32c
  module Sharding = Codecs.Sharding
end

(** {1 Metadata} *)

module Metadata = Metadata

(** {1 Store Interface} *)

module Store = Store

(** {1 Array Operations} *)

module Array = Array_repr

(** {1 Group Operations} *)

module Group = Group_repr

(** {1 Convenience Functions} *)

(** Default codec chain: just bytes in little endian *)
let default_codecs = [Bytes { endian = Some Endianness.Little }]

(** Create a bytes codec specification *)
let bytes_codec ?(endian = Endianness.Little) () = Bytes { endian = Some endian }

(** Create a gzip codec specification *)
let gzip_codec ?(level = 5) () = Gzip { level }

(** Create a zstd codec specification *)
let zstd_codec ?(level = 3) ?(checksum = false) () = Zstd { level; checksum }

(** Create a crc32c codec specification *)
let crc32c_codec () = Crc32c

(** Create a transpose codec specification *)
let transpose_codec order = Transpose { order }

(** Create a sharding codec specification *)
let sharding_codec ~chunk_shape ?(codecs = [Bytes { endian = Some Endianness.Little }])
    ?(index_codecs = [Bytes { endian = Some Endianness.Little }; Crc32c])
    ?(index_location = Index_location.End) () =
  Sharding { chunk_shape; codecs; index_codecs; index_location }

(** {1 Slice Constructors} *)

(** Create an index slice *)
let idx i = Index i

(** Create a range slice [start, stop) *)
let range start stop = Range (start, stop)

(** Create a range slice from start to end *)
let range_from start = RangeFrom start

(** Create a range slice from beginning to stop *)
let range_to stop = RangeTo stop

(** All elements in a dimension *)
let all = All

(** Create a stepped range slice *)
let stepped start stop step = Stepped (start, stop, step)
