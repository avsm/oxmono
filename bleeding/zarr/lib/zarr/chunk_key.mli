(** {1 Chunk Key Encoding}

    Maps chunk grid coordinates to storage keys.

    See {{:https://zarr-specs.readthedocs.io/en/latest/v3/chunk-key-encodings/default/index.html}
    Zarr v3.1 spec, "Chunk Key Encodings"}.

    Two encodings are defined:

    - {b Default encoding:} prefix ["c"] + separator + indices,
      e.g. ["c/1/23/45"] or ["c.1.23.45"].
      For 0-dimensional arrays, the key is just ["c"].

    - {b V2 encoding:} indices only (no prefix), e.g. ["1/23/45"] or
      ["1.23.45"].  For 0-dimensional arrays, the key is ["0"].
      Intended only for migration from Zarr v2, not recommended for new
      arrays. *)

(** Chunk key encoding configuration. *)
type encoding =
  | Default of { separator : Zarr_codec.separator }
      (** Recommended encoding: ["c/<i>/<j>/..."] *)
  | V2 of { separator : Zarr_codec.separator }
      (** Legacy v2-compatible: ["<i>/<j>/..."] *)

(** {2 Key Operations} *)

val encode : encoding -> int array -> string
(** [encode encoding coords] converts chunk coordinates to a key string. *)

val decode : encoding -> string -> int array option
(** [decode encoding key] parses a key string to chunk coordinates.
    Returns [None] for malformed keys. *)

val full_path : string -> encoding -> int array -> string
(** [full_path array_path encoding coords] returns the complete storage
    key for a chunk, combining the array path and chunk key.

    {b Zarr v3.1 spec:} non-root array at path [P] has data prefix [P/],
    so chunk key is [P/<chunk_key>]. *)

val metadata_path : string -> string
(** [metadata_path path] returns the storage key for [zarr.json] at the
    given node path.

    {b Zarr v3.1 spec:} root metadata at [zarr.json], non-root at
    [P/zarr.json]. *)

(** {2 JSON Serialization}

    JSON format:
    {v {"name": "default"|"v2", "configuration": {"separator": "/"|"."}} v} *)

val of_json : Jsont.json -> (encoding, string) result
val to_json : encoding -> Jsont.json
