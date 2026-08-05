(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** YAML codec using Jsont type descriptions.

    This module provides YAML streaming encode/decode that interprets {!Jsont.t}
    type descriptions, allowing the same codec definitions to work for both JSON
    and YAML.

    {b Example:}
    {[
      (* Define a codec once using Jsont *)
      module Config = struct
        type t = { name : string; port : int }

        let make name port = { name; port }

        let jsont =
          Jsont.Object.map ~kind:"Config" make
          |> Jsont.Object.mem "name" Jsont.string ~enc:(fun c -> c.name)
          |> Jsont.Object.mem "port" Jsont.int ~enc:(fun c -> c.port)
          |> Jsont.Object.finish
      end

      (* Use the same codec for both JSON and YAML *)
      let from_json = Jsont_bytesrw.decode_string Config.jsont json_str
      let from_yaml = Yamlt.decode_string Config.jsont yaml_str
    ]}

    {2 Related Libraries}

    - [Jsont] - JSON codec library whose type descriptions this library
      interprets
    - [Yamlrw] - Pure OCaml YAML parser/emitter used for low-level YAML
      processing
    - [Bytesrw] - Byte-level I/O abstraction for streaming encode/decode

    See notes about {{!yaml_mapping}YAML to JSON mapping},
    {{!yaml_scalars}YAML scalar resolution}, and
    {{!null_handling}null value handling}. *)

open Bytesrw

(** {1:decode Decode} *)

val decode :
  ?layout:bool ->
  ?locs:bool ->
  ?file:Jsont.Textloc.fpath ->
  ?max_depth:int ->
  ?max_nodes:int ->
  'a Jsont.t ->
  Bytes.Reader.t ->
  ('a, string) result
(** [decode t r] decodes a value from YAML reader [r] according to type [t].
    - If [layout] is [true], style information is preserved in {!Jsont.Meta.t}
      values (for potential round-tripping). Defaults to [false].
    - If [locs] is [true], source locations are preserved in {!Jsont.Meta.t}
      values and error messages are precisely located. Defaults to [false].
    - [file] is the file path for error messages. Defaults to
      {!Jsont.Textloc.file_none}.
    - [max_depth] limits nesting depth to prevent stack overflow (billion laughs
      protection). Defaults to [100].
    - [max_nodes] limits total decoded nodes (billion laughs protection).
      Defaults to [10_000_000].

    The YAML input must contain exactly one document. Multi-document streams are
    not supported; use {!decode_all} for those. *)

val decode' :
  ?layout:bool ->
  ?locs:bool ->
  ?file:Jsont.Textloc.fpath ->
  ?max_depth:int ->
  ?max_nodes:int ->
  'a Jsont.t ->
  Bytes.Reader.t ->
  ('a, Jsont.Error.t) result
(** [decode'] is like {!val-decode} but preserves the error structure. *)

val decode_all :
  ?layout:bool ->
  ?locs:bool ->
  ?file:Jsont.Textloc.fpath ->
  ?max_depth:int ->
  ?max_nodes:int ->
  'a Jsont.t ->
  Bytes.Reader.t ->
  ('a, string) result Seq.t
(** [decode_all t r] decodes all documents from a multi-document YAML stream.
    Returns a sequence where each element is a result of decoding one document.
    Parameters are as in {!val-decode}. Use this for YAML streams containing
    multiple documents separated by [---]. *)

val decode_all' :
  ?layout:bool ->
  ?locs:bool ->
  ?file:Jsont.Textloc.fpath ->
  ?max_depth:int ->
  ?max_nodes:int ->
  'a Jsont.t ->
  Bytes.Reader.t ->
  ('a, Jsont.Error.t) result Seq.t
(** [decode_all'] is like {!val-decode_all} but preserves the error structure.
*)

val decode_string :
  ?layout:bool ->
  ?locs:bool ->
  ?file:Jsont.Textloc.fpath ->
  ?max_depth:int ->
  ?max_nodes:int ->
  'a Jsont.t ->
  string ->
  ('a, string) result
(** [decode_string t s] decodes a value from YAML string [s] according to type
    [t]. This is a convenience wrapper around {!val-decode}. *)

val decode_value : 'a Jsont.t -> Yamlrw.value -> ('a, string) result
(** [decode_value t v] decodes a value from a pre-parsed {!Yamlrw.value}
    according to type [t].

    This is useful when you have already parsed YAML into its JSON-compatible
    representation (e.g., when using {!Yamlrw.of_string}) and want to decode it
    using a Jsont codec without re-parsing the YAML text. *)

val decode_value' : 'a Jsont.t -> Yamlrw.value -> ('a, Jsont.Error.t) result
(** [decode_value'] is like {!val-decode_value} but preserves the error
    structure. *)

(** {1:encode Encode} *)

(** YAML output format. *)
type yaml_format =
  | Block  (** Block style (indented) - default. Clean, readable YAML. *)
  | Flow  (** Flow style (JSON-like). Compact, single-line collections. *)
  | Layout  (** Preserve layout from {!Jsont.Meta.t} when available. *)

val encode :
  ?buf:Stdlib.Bytes.t ->
  ?format:yaml_format ->
  ?indent:int ->
  ?explicit_doc:bool ->
  ?scalar_style:Yamlrw.Scalar_style.t ->
  'a Jsont.t ->
  'a ->
  eod:bool ->
  Bytes.Writer.t ->
  (unit, string) result
(** [encode t v w] encodes value [v] according to type [t] to YAML on [w].
    - If [buf] is specified, it is used as a buffer for output slices. Defaults
      to a buffer of length {!Bytesrw.Bytes.Writer.slice_length}[ w].
    - [format] controls the output style. Defaults to {!Block}.
    - [indent] is the indentation width in spaces. Defaults to [2].
    - [explicit_doc] if [true], emits explicit document markers ([---] and
      [...]). Defaults to [false].
    - [scalar_style] is the preferred style for string scalars. Defaults to
      [`Any] (auto-detect based on content).
    - [eod] indicates whether {!Bytesrw.Bytes.Slice.eod} should be written on
      [w] after encoding. *)

val encode' :
  ?buf:Stdlib.Bytes.t ->
  ?format:yaml_format ->
  ?indent:int ->
  ?explicit_doc:bool ->
  ?scalar_style:Yamlrw.Scalar_style.t ->
  'a Jsont.t ->
  'a ->
  eod:bool ->
  Bytes.Writer.t ->
  (unit, Jsont.Error.t) result
(** [encode'] is like {!val-encode} but preserves the error structure. *)

(** {1:recode Recode}

    The defaults in these functions are those of {!val-decode} and
    {!val-encode}, except if [layout] is [true], [format] defaults to {!Layout}
    and vice-versa. *)

val recode :
  ?layout:bool ->
  ?locs:bool ->
  ?file:Jsont.Textloc.fpath ->
  ?max_depth:int ->
  ?max_nodes:int ->
  ?buf:Stdlib.Bytes.t ->
  ?format:yaml_format ->
  ?indent:int ->
  ?explicit_doc:bool ->
  ?scalar_style:Yamlrw.Scalar_style.t ->
  'a Jsont.t ->
  Bytes.Reader.t ->
  Bytes.Writer.t ->
  eod:bool ->
  (unit, string) result
(** [recode t r w] is {!val-decode} followed by {!val-encode}. *)

(** {1:yaml_mapping YAML to JSON Mapping}

    YAML is a superset of JSON. This module maps YAML structures to the JSON
    data model that {!Jsont.t} describes:

    - YAML scalars map to JSON null, boolean, number, or string depending on
      content and the expected type
    - YAML sequences map to JSON arrays
    - YAML mappings map to JSON objects (keys must be strings)
    - YAML aliases are resolved during decoding
    - YAML tags are used to guide type resolution when present

    {b Limitations:}
    - Only string keys are supported in mappings (JSON object compatibility)
    - Anchors and aliases are resolved; the alias structure is not preserved
    - Multi-document streams require {!decode_all} *)

(** {1:yaml_scalars YAML Scalar Resolution}

    YAML scalars are resolved to JSON types as follows:

    {b Null:} [null], [Null], [NULL], [~], or empty string

    {b Boolean:} [true], [True], [TRUE], [false], [False], [FALSE], [yes],
    [Yes], [YES], [no], [No], [NO], [on], [On], [ON], [off], [Off], [OFF]

    {b Number:} Decimal integers, floats, hex ([0x...]), octal ([0o...]),
    infinity ([.inf], [-.inf]), NaN ([.nan])

    {b String:} Anything else, or explicitly quoted scalars

    When decoding against a specific {!Jsont.t} type, the expected type takes
    precedence over automatic resolution. For example, decoding ["yes"] against
    {!Jsont.string} yields the string ["yes"], not [true]. *)

(** {1:null_handling Null Value Handling}

    YAML null values are handled according to the expected type to provide
    friendly defaults while maintaining type safety:

    {b Collections (Arrays and Objects):}

    Null values decode as empty collections when the codec expects a collection
    type. This provides convenient defaults for optional collection fields in
    YAML:
    {[
      # YAML with null collection fields
      config:
        items: null      # Decodes as []
        settings: ~      # Decodes as {}
        tags:            # Missing value = null, decodes as []
    ]}

    For arrays, null decodes to an empty array. For objects, null decodes to an
    object with all fields set to their [dec_absent] defaults. If any required
    field lacks a default, decoding fails with a missing member error.

    This behavior makes yamlt more forgiving for schemas with many optional
    collection fields, where writing [field:] (which parses as null) is natural
    and semantically equivalent to [field: []].

    {b Numbers:}

    Null values decode to [Float.nan] when the codec expects a number.

    {b Primitive Types (Int, Bool, String):}

    Null values {e fail} when decoding into primitive scalar types ([int],
    [bool], [string]). Null typically indicates genuinely missing or incorrect
    data for these types, and silent conversion could clash with a manual
    setting of the default value (e.g. 0 and [null] for an integer would be
    indistinguishable).

    To accept null for primitive fields, explicitly use {!val:Jsont.option}:
    {[
      (* Accepts null, decodes as None *)
      Jsont.Object.mem "count" (Jsont.option Jsont.int) ~dec_absent:None
        (* Rejects null, requires a number *)
        Jsont.Object.mem "count" Jsont.int ~dec_absent:0
    ]} *)
