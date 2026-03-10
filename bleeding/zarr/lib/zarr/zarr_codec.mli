(** {1 Zarr v3 Codec Interface and Pipeline}

    Codecs transform chunk data through a chain of transformations:

    + 0 or more {b array-to-array} codecs (e.g. transpose)
    + Exactly 1 {b array-to-bytes} codec (e.g. bytes, sharding)
    + 0 or more {b bytes-to-bytes} codecs (e.g. gzip, zstd, crc32c)

    This ordering is enforced by {!build_chain} and matches the
    Zarr v3.1 specification section "Codecs".

    {b Encoding} proceeds left-to-right through the chain:
    {v chunk -> a2a[0] -> ... -> a2a[n] -> a2b -> b2b[0] -> ... -> b2b[m] -> bytes v}

    {b Decoding} proceeds right-to-left:
    {v bytes -> b2b[m] -> ... -> b2b[0] -> a2b -> a2a[n] -> ... -> a2a[0] -> chunk v} *)

(** {2 Exceptions} *)

exception Codec_error of string
(** Raised when a codec chain fails to build, encode, or decode. *)

exception Checksum_mismatch
(** Raised when a checksum codec detects data corruption. *)

(** {2 Codec types} *)

(** Array-to-array codec: transforms chunk shape/layout without changing
    the representation domain (e.g. transpose).

    {b Zarr v3.1 spec:} "An array → array codec transforms an array
    to another array.  Both the decoded and encoded representations
    are arrays." *)
type array_to_array = {
  encode : Chunk_data.t -> Chunk_data.t;
  decode : Chunk_data.t -> Chunk_data.t;
  compute_output_shape : int array -> int array;
}

(** Array-to-bytes codec: serialises a typed chunk to raw bytes
    (e.g. bytes codec, sharding).

    {b Zarr v3.1 spec:} "An array → bytes codec transforms an array
    to a byte sequence.  Every codec chain must contain exactly one." *)
type array_to_bytes = {
  encode : Chunk_data.t -> bytes;
  decode : int array -> Zarr_dtype.t -> bytes -> Chunk_data.t;
}

(** Bytes-to-bytes codec: transforms raw bytes (e.g. compression,
    checksum).

    {b Zarr v3.1 spec:} "A bytes → bytes codec transforms a byte
    sequence to another byte sequence."

    [decode] raises {!Codec_error} or {!Checksum_mismatch} on failure.

    [compute_encoded_size] returns [None] for variable-size codecs
    (compression) and [Some n] for fixed-overhead codecs (CRC32C). *)
type bytes_to_bytes = {
  encode : bytes -> bytes;
  decode : bytes -> bytes;
  compute_encoded_size : int -> int option;
}

(** A validated codec chain: 0+ array-to-array, 1 array-to-bytes,
    0+ bytes-to-bytes. *)
type codec_chain = {
  array_to_array : array_to_array list;
  array_to_bytes : array_to_bytes;
  bytes_to_bytes : bytes_to_bytes list;
}

(** {2 Codec classification} *)

type codec_class =
  | ArrayToArray of array_to_array
  | ArrayToBytes of array_to_bytes
  | BytesToBytes of bytes_to_bytes

(** {2 Codec registry}

    Allows external packages (e.g. zarr-blosc) to register codec builders
    at link time so the core codec chain can construct them from metadata
    JSON.

    {b Zarr v3.1 spec:} "Implementations should be extensible to support
    new codecs." *)

type codec_builder =
  Jsont.json -> Zarr_dtype.t -> int array -> codec_class

val register_codec : string -> codec_builder -> unit
(** [register_codec name builder] registers an extension codec. *)

val find_codec : string -> codec_builder option
(** [find_codec name] looks up a registered codec builder. *)

val is_registered : string -> bool
(** [is_registered name] returns [true] if [name] is a registered codec. *)

(** {2 Codec specifications}

    Parsed from / serialised to JSON metadata.  These represent the
    persistent codec configuration stored in [zarr.json]. *)

type separator = Slash | Dot
(** Separator character for chunk key encoding. *)

type index_location = Start | End
(** Index location for the sharding codec. *)

(** Codec specification -- the JSON-level description before building.

    {b Core codecs:} [Bytes], [Transpose], [Gzip], [Crc32c],
    [Sharding] (sharding_indexed).

    {b Extension codecs:} [Zstd] (registered as extension), [Extension]
    for any codec registered via {!register_codec}. *)
type codec_spec =
  | Bytes of { endian : Zarr_dtype.endian option }
  | Transpose of { order : int array }
  | Gzip of { level : int }
  | Zstd of { level : int; checksum : bool }
  | Crc32c
  | Sharding of {
      chunk_shape : int array;
      codecs : codec_spec list;
      index_codecs : codec_spec list;
      index_location : index_location;
    }
  | Extension of { name : string; config : Jsont.json }

(** {2 Chain building} *)

val build_codec_ref :
  (codec_spec -> Zarr_dtype.t -> int array -> Zarr_fill.t ->
   (codec_spec list -> Zarr_dtype.t -> int array -> Zarr_fill.t -> codec_chain) ->
   codec_class) ref
(** Forward reference for building individual codecs.
    Set at library initialisation time by [zarr.ml]. *)

val build_chain : codec_spec list -> Zarr_dtype.t -> int array -> Zarr_fill.t -> codec_chain
(** [build_chain specs dtype chunk_shape fill_value] builds a validated
    codec chain from a list of codec specifications.

    Enforces the Zarr v3.1 ordering constraint:
    - 0+ array-to-array codecs
    - exactly 1 array-to-bytes codec
    - 0+ bytes-to-bytes codecs

    The [fill_value] is threaded through to the sharding codec, which
    needs it to correctly fill uninitialised inner chunks during decode
    (Zarr v3.1 spec: empty inner chunks use the array's fill value).

    @raise Codec_error if ordering is invalid or a codec fails to build. *)

val build_chain_default : codec_spec list -> Zarr_dtype.t -> int array -> codec_chain
(** [build_chain_default specs dtype chunk_shape] is
    [build_chain specs dtype chunk_shape (Zarr_fill.default dtype)].
    Convenience wrapper for callers that use the default (zero) fill
    value. *)

(** {2 Encode / Decode} *)

val encode : codec_chain -> Chunk_data.t -> bytes
(** [encode chain chunk] encodes chunk data through the full codec chain.

    The chunk passes through array-to-array codecs, then the
    array-to-bytes codec, then bytes-to-bytes codecs. *)

val decode : codec_chain -> int array -> Zarr_dtype.t -> bytes -> Chunk_data.t
(** [decode chain shape dtype bytes] decodes bytes through the codec
    chain in reverse order.

    @raise Codec_error if any codec in the chain fails.
    @raise Checksum_mismatch if a checksum codec detects corruption. *)

(** {2 JSON serialization} *)

val specs_of_json : Jsont.json list -> codec_spec list
(** [specs_of_json json_list] parses codec specifications from JSON.

    Recognises the following codec names:
    - ["bytes"]: array-to-bytes, config: [\{"endian": "little"|"big"\}]
    - ["transpose"]: array-to-array, config: [\{"order": [...]\}]
    - ["gzip"]: bytes-to-bytes, config: [\{"level": 0-9\}]
    - ["zstd"]: bytes-to-bytes, config: [\{"level": 1-22\}]
    - ["crc32c"]: bytes-to-bytes, no config
    - ["sharding_indexed"]: array-to-bytes, nested codecs
    - Registered extension codecs

    @raise Codec_error if an unsupported codec is encountered. *)

val specs_to_json : codec_spec list -> Jsont.json list
(** [specs_to_json specs] serialises codec specifications to JSON. *)

val spec_to_json : codec_spec -> Jsont.json
(** [spec_to_json spec] serialises one codec specification to JSON. *)
