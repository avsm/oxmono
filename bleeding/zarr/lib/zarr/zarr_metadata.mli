(** {1 Zarr v3 Metadata}

    Parsing and serialisation of [zarr.json] metadata documents for
    arrays and groups.

    See {{:https://zarr-specs.readthedocs.io/en/latest/v3/core/index.html}
    Zarr v3.1 spec, "Metadata" section}.

    Metadata is stored as JSON at key [<path>/zarr.json] for every node
    in the hierarchy.  Attributes are represented as generic {!Jsont.json}
    values.

    {2 Required fields (array)}

    - [zarr_format]: must be [3]
    - [node_type]: ["array"]
    - [shape]: array of non-negative integers
    - [data_type]: identifier string (e.g. ["int32"], ["float64"])
    - [chunk_grid]: e.g. [\{"name": "regular", ...\}]
    - [chunk_key_encoding]: e.g. [\{"name": "default", ...\}]
    - [fill_value]: type-dependent JSON value
    - [codecs]: array with at least one array-to-bytes codec

    {2 Optional fields}

    - [dimension_names]: array of strings/nulls
    - [attributes]: arbitrary JSON object *)

(** {2 Types} *)

(** Array metadata as parsed from [zarr.json]. *)
type array_metadata = {
  zarr_format : int;                   (** Must be [3]. *)
  shape : int array;                   (** Array dimensions. *)
  data_type : Zarr_dtype.t;            (** Element data type. *)
  chunk_grid : Chunk_grid.t;           (** Chunk grid configuration. *)
  chunk_key_encoding : Chunk_key.encoding;  (** How chunk coords map to storage keys. *)
  fill_value : Zarr_fill.t;            (** Default value for uninitialised regions. *)
  codecs : Zarr_codec.codec_spec list;  (** Codec pipeline specification. *)
  dimension_names : string option array option;  (** Optional per-dimension names. *)
  attributes : Jsont.json option;       (** User-defined attributes. *)
}

(** Group metadata as parsed from [zarr.json]. *)
type group_metadata = {
  zarr_format : int;                   (** Must be [3]. *)
  attributes : Jsont.json option;       (** User-defined attributes. *)
}

(** {2 Validation} *)

val validate_node_name : string -> (unit, string) result
(** [validate_node_name name] checks Zarr v3 node naming rules:
    - Must not be empty
    - Must not contain ['/']
    - Must not be ["."] or [".."]
    - Must not start with ["__"] (reserved prefix)

    {b Zarr v3.1 spec:} Node names should only contain ASCII [a-zA-Z0-9],
    [-], [_], [.].  Names should be NFKC-normalised and case-folded for
    portable comparison. *)

(** {2 Array Metadata} *)

val array_of_json : string -> (array_metadata, string) result
(** [array_of_json json_string] parses array metadata from a JSON string.

    Validates:
    - [zarr_format = 3]
    - [node_type = "array"]
    - Codec chain contains at least one array-to-bytes codec
    - All required fields are present and well-typed *)

val array_to_json : array_metadata -> string
(** [array_to_json m] serialises array metadata to a pretty-printed
    JSON string suitable for writing to [zarr.json]. *)

val create_array_metadata :
  shape:int array -> chunks:int array -> dtype:Zarr_dtype.t ->
  ?fill_value:Zarr_fill.t -> ?codecs:Zarr_codec.codec_spec list ->
  ?dimension_names:string option array ->
  ?attributes:Jsont.json -> unit -> array_metadata
(** [create_array_metadata ~shape ~chunks ~dtype ()] creates array metadata
    with sensible defaults:
    - [zarr_format = 3]
    - Default chunk key encoding with [/] separator
    - Bytes codec in little-endian order
    - Zero/false fill value for the data type *)

(** {2 Group Metadata} *)

val group_of_json : string -> (group_metadata, string) result
(** [group_of_json json_string] parses group metadata from a JSON string.
    Validates [zarr_format = 3] and [node_type = "group"]. *)

val group_to_json : group_metadata -> string
(** [group_to_json m] serialises group metadata to a pretty-printed
    JSON string. *)

val create_group_metadata : ?attributes:Jsont.json -> unit -> group_metadata
(** [create_group_metadata ()] creates group metadata with [zarr_format=3]. *)
