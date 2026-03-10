(** Blosc - high performance meta-compressor with shuffling

    Direct C bindings with OxCaml optimizations for unboxed/untagged values.

    Blosc is a meta-compressor that splits data into blocks, applies optional
    shuffle filters for better compression ratios, and compresses each block
    using a configurable compression algorithm (LZ4, LZ4HC, BloscLZ, Zstd,
    Snappy, Zlib). *)

exception Error of string

(** {1 Types} *)

(** Shuffle filter mode applied before compression. Shuffling rearranges
    bytes to improve compression ratio for typed data. *)
type shuffle =
  | NoShuffle    (** No shuffling *)
  | Shuffle      (** Byte shuffling (recommended for most data) *)
  | BitShuffle   (** Bit-level shuffling (best for small type sizes) *)

(** Available compression algorithms. *)
type compressor =
  | LZ4       (** LZ4: very fast compression/decompression *)
  | LZ4HC     (** LZ4HC: high compression variant of LZ4 *)
  | BloscLZ   (** BloscLZ: Blosc's default compressor *)
  | Zstd      (** Zstandard: high ratio with good speed *)
  | Snappy    (** Snappy: fast compression by Google *)
  | Zlib      (** Zlib: widely compatible, moderate speed *)

(** {1 Constants} *)

val max_overhead : int
(** Maximum overhead that Blosc adds to compressed data (currently 16 bytes).
    Used to calculate worst-case compressed buffer size. *)

(** {1 Compression} *)

val compress : cname:compressor -> clevel:int -> shuffle:shuffle ->
  typesize:int -> blocksize:int -> string -> string
(** [compress ~cname ~clevel ~shuffle ~typesize ~blocksize src] compresses
    string [src] using the specified parameters.

    @param cname compression algorithm to use
    @param clevel compression level (0-9, where 0 means no compression)
    @param shuffle shuffle filter to apply before compression
    @param typesize size of the data type in bytes (e.g., 4 for float32);
           used by the shuffle filter
    @param blocksize internal block size for splitting (0 = automatic)
    @raise Error if compression fails *)

val compress_to : cname:compressor -> clevel:int -> shuffle:shuffle ->
  typesize:int -> blocksize:int -> string -> bytes -> int
(** [compress_to ~cname ~clevel ~shuffle ~typesize ~blocksize src dst]
    compresses [src] into [dst] and returns the number of bytes written.
    [dst] must be at least [compress_bound (String.length src)] bytes long.

    @raise Error if compression fails *)

val compress_bound : int -> int
(** [compress_bound src_size] returns the maximum compressed size for data
    of size [src_size]. This is [src_size + max_overhead]. Use this to
    pre-allocate buffers for {!compress_to}. *)

(** {1 Decompression} *)

val decompress : string -> string
(** [decompress src] decompresses [src]. The decompressed size is extracted
    from the Blosc header embedded in the compressed data.

    @raise Error if the compressed data is invalid or decompression fails *)

val decompress_to : string -> bytes -> int
(** [decompress_to src dst] decompresses [src] into [dst] and returns the
    number of bytes written. [dst] must be at least
    [get_decompressed_size src] bytes long.

    @raise Error if decompression fails *)

(** {1 Inspection} *)

val get_decompressed_size : string -> int
(** [get_decompressed_size src] extracts the original (uncompressed) size
    from the Blosc header of compressed data [src].

    @raise Error if the compressed data is invalid (nbytes=0) *)

val get_complib : string -> string
(** [get_complib src] returns the name of the compression library used
    to compress [src], as read from the Blosc header. *)

(** {1 Helpers} *)

val shuffle_to_int : shuffle -> int
(** Convert a shuffle mode to its integer representation. *)

val compressor_to_string : compressor -> string
(** Convert a compressor to its string name (e.g., [LZ4] becomes ["lz4"]). *)
