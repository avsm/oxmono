(** {1 Blosc Codec for Zarr}

    High-performance meta-compressor with shuffling support.
    Blosc splits data into blocks, applies byte/bit shuffling to improve
    compressibility, and compresses each block with an internal compressor.

    This codec auto-registers with the Zarr codec registry at link time,
    so linking [zarr-blosc] is sufficient to enable blosc support in Zarr
    arrays. *)

(** {2 Shuffle Modes} *)

(** Byte shuffle mode. Shuffling rearranges bytes within each element
    to group similar bytes together, improving compression. *)
type shuffle =
  | NoShuffle   (** No shuffling *)
  | Shuffle     (** Byte-level shuffle (groups MSB, LSB, etc.) *)
  | BitShuffle  (** Bit-level shuffle (groups individual bits) *)

val shuffle_of_string : string -> shuffle
(** [shuffle_of_string s] parses ["noshuffle"], ["shuffle"], or
    ["bitshuffle"].
    @raise Zarr.Codec.Codec_error on unknown shuffle mode. *)

val shuffle_to_string : shuffle -> string
(** [shuffle_to_string mode] returns the canonical string representation. *)

(** {2 Compressor Names} *)

(** Internal compressor used by blosc. *)
type compressor =
  | LZ4      (** LZ4 — fast compression *)
  | LZ4HC    (** LZ4 High Compression — slower, better ratio *)
  | BloscLZ  (** BloscLZ — blosc's default compressor *)
  | Zstd     (** Zstandard *)
  | Snappy   (** Google Snappy *)
  | Zlib     (** zlib / deflate *)

val compressor_of_string : string -> compressor
(** [compressor_of_string s] parses a compressor name.
    @raise Zarr.Codec.Codec_error on unknown compressor. *)

val compressor_to_string : compressor -> string
(** [compressor_to_string c] returns the canonical name. *)

(** {2 Compression / Decompression} *)

val compress :
  cname:compressor -> clevel:int -> shuffle:shuffle ->
  typesize:int -> blocksize:int -> bytes -> bytes
(** [compress ~cname ~clevel ~shuffle ~typesize ~blocksize input]
    compresses [input] using blosc.

    @param clevel Compression level (1–9).
    @param typesize Element size in bytes (used by shuffle).
    @param blocksize Internal block size (0 = auto). *)

val decompress : bytes -> bytes
(** [decompress input] decompresses blosc-compressed bytes.
    @raise Zarr.Codec.Codec_error on decompression failure. *)

(** {2 Zarr Integration} *)

val create :
  cname:compressor -> clevel:int -> shuffle:shuffle ->
  typesize:int -> blocksize:int -> Zarr.Codec.bytes_to_bytes
(** [create ~cname ~clevel ~shuffle ~typesize ~blocksize] creates a
    bytes-to-bytes codec for use in a Zarr codec chain. *)

val codec_spec :
  cname:compressor -> clevel:int -> shuffle:shuffle ->
  typesize:int -> blocksize:int -> Zarr.Codec.codec_spec
(** [codec_spec ~cname ~clevel ~shuffle ~typesize ~blocksize] creates a
    {!Zarr.Codec.codec_spec} that can be passed to array creation. *)

val register : unit -> unit
(** [register ()] registers the blosc codec with the Zarr codec registry.
    Called automatically at link time. *)
