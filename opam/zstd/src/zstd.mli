(** Zstandard - fast lossless compression algorithm

    Direct C bindings with OxCaml optimizations for unboxed/untagged values. *)

exception Error of string

val version : unit -> int * int * int
(** Returns (major, minor, patch) version tuple *)

val compress : level:int -> ?dict:string -> string -> string
(** [compress ~level ?dict s] compresses string [s] at compression [level] (0-22).
    Optional: use pre-defined dictionary content (see dictBuilder). *)

val decompress : int -> ?dict:string -> string -> string
(** [decompress orig_size ?dict s] decompresses string [s].

    @param orig_size specifies size of buffer for decompression
           (not less than original size of uncompressed [s])
    @param dict must be identical to the one used during compression,
           otherwise uncompressed data will be corrupted. *)

val get_decompressed_size : string -> int
(** [get_decompressed_size s] extracts the decompressed size from the frame header.
    @raise Error if the content size is unknown or there is a decoding error. *)

val compress_bound : int -> int
(** [compress_bound src_size] returns the maximum compressed size for data
    of size [src_size]. Use this to pre-allocate buffers for [compress_to]. *)

(** {1 Buffer-Passing API}

    These functions write directly to a provided buffer, avoiding allocation
    of result strings. Useful for repeated compression/decompression. *)

val compress_to : level:int -> ?dict:string -> string -> bytes -> int
(** [compress_to ~level ?dict src dst] compresses [src] into [dst] and returns
    the number of bytes written. [dst] must be at least [compress_bound (String.length src)]
    bytes long. *)

val decompress_to : int -> ?dict:string -> string -> bytes -> int
(** [decompress_to orig_size ?dict src dst] decompresses [src] into [dst] and returns
    the number of bytes written. [dst] must be at least [orig_size] bytes long. *)
