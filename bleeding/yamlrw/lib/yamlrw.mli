(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** {1 Yamlrw - A Pure OCaml YAML Parser and Emitter}

    Yamlrw is a pure OCaml implementation of YAML 1.1/1.2 parsing and emission.
    It provides both a high-level JSON-compatible interface and a lower-level
    streaming API for fine-grained control.

    {2 Quick Start}

    Parse a YAML string:
    {[
      let value = Yamlrw.of_string "name: Alice\nage: 30" in
      match value with
      | `O [("name", `String "Alice"); ("age", `Float 30.)] -> ...
      | _ -> ...
    ]}

    Serialize to YAML:
    {[
      let yaml = `O [("name", `String "Bob"); ("active", `Bool true)] in
      let s = Yamlrw.to_string yaml in
      (* "name: Bob\nactive: true\n" *)
    ]}

    Use the Util module for convenient access:
    {[
      let name = Yamlrw.Util.(get_string (get "name" value)) in
      let age = Yamlrw.Util.(get_int (get "age" value)) in
    ]}

    {2 Related Libraries}

    - [Yamlrw_unix] - Unix file I/O integration
    - [Yamlrw_eio] - Eio async file and flow operations
    - [Yamlt] - YAML codec using Jsont type descriptions (higher-level) *)

(** {2 Error Handling} *)

module Error = Error

exception Yamlrw_error of Error.t
(** Raised on parse or emit errors. *)

(** {2 Core Types} *)

type value =
  [ `Null  (** YAML null, ~, or empty values *)
  | `Bool of bool  (** YAML booleans (true, false, yes, no, on, off) *)
  | `Float of float  (** All YAML numbers (integers stored as floats) *)
  | `String of string  (** YAML strings *)
  | `A of value list  (** YAML sequences/arrays *)
  | `O of (string * value) list  (** YAML mappings/objects with string keys *)
  ]
(** JSON-compatible YAML representation. Use this for simple data interchange.

    This type is structurally equivalent to {!Value.t} and compatible with the
    ezjsonm representation. For additional operations, see {!Value} and {!Util}.
*)

type yaml =
  [ `Scalar of Scalar.t  (** YAML scalar value with style and metadata *)
  | `Alias of string  (** Alias reference to an anchored node *)
  | `A of yaml Sequence.t  (** YAML sequence with style and metadata *)
  | `O of (yaml, yaml) Mapping.t  (** YAML mapping with style and metadata *)
  ]
(** Full YAML representation preserving anchors, tags, and aliases.

    This type is structurally equivalent to {!Yaml.t}. Use this when you need
    access to YAML-specific features like anchors and aliases for node reuse,
    type tags for custom types, scalar styles (plain, quoted, literal, folded),
    and collection styles (block vs flow).

    For additional operations, see {!Yaml}, {!Scalar}, {!Sequence}, and
    {!Mapping}. *)

type document = {
  version : (int * int) option;
      (** Optional YAML version directive (e.g., (1, 2) for YAML 1.2) *)
  tags : (string * string) list;
      (** TAG directives mapping handles to prefixes *)
  root : yaml option;  (** Root content of the document *)
  implicit_start : bool;
      (** Whether the document start marker (---) is implicit *)
  implicit_end : bool;  (** Whether the document end marker (...) is implicit *)
}
(** A YAML document with directives and metadata.

    This type is structurally equivalent to {!Document.t}. A YAML stream can
    contain multiple documents, each separated by document markers.

    For additional operations, see {!Document}. *)

(** {2 Character Encoding} *)

module Encoding = Encoding

(** {2 Parsing} *)

type version = [ `V1_1 | `V1_2 ]
(** YAML specification version. *)

val default_max_alias_nodes : int
(** Default maximum nodes during alias expansion (10 million). *)

val default_max_alias_depth : int
(** Default maximum alias nesting depth (100). *)

val of_string :
  ?resolve_aliases:bool -> ?max_nodes:int -> ?max_depth:int -> string -> value
(** Parse a YAML string into a JSON-compatible value.

    @param resolve_aliases Whether to expand aliases (default: true)
    @param max_nodes Maximum nodes during alias expansion (default: 10M)
    @param max_depth Maximum alias nesting depth (default: 100)
    @raise Yamlrw_error on parse error or if multiple documents found *)

val yaml_of_string :
  ?resolve_aliases:bool -> ?max_nodes:int -> ?max_depth:int -> string -> yaml
(** Parse a YAML string preserving full YAML metadata (anchors, tags, etc).

    By default, aliases are NOT resolved, preserving the document structure.

    @param resolve_aliases Whether to expand aliases (default: false)
    @param max_nodes Maximum nodes during alias expansion (default: 10M)
    @param max_depth Maximum alias nesting depth (default: 100)
    @raise Yamlrw_error on parse error or if multiple documents found *)

val documents_of_string : string -> document list
(** Parse a multi-document YAML stream.

    Use this when your YAML input contains multiple documents separated by
    document markers (---).

    @raise Yamlrw_error on parse error *)

(** {2 Formatting Styles} *)

module Scalar_style = Scalar_style
module Layout_style = Layout_style

(** {2 Serialization} *)

val to_buffer :
  ?encoding:Encoding.t ->
  ?scalar_style:Scalar_style.t ->
  ?layout_style:Layout_style.t ->
  ?buffer:Buffer.t ->
  value ->
  Buffer.t
(** Serialize a value to a buffer.

    @param encoding Output encoding (default: UTF-8)
    @param scalar_style Preferred scalar style (default: Any)
    @param layout_style Preferred layout style (default: Any)
    @param buffer
      Optional buffer to append to (allocates new one if not provided)
    @return The buffer containing the serialized YAML *)

val to_string :
  ?encoding:Encoding.t ->
  ?scalar_style:Scalar_style.t ->
  ?layout_style:Layout_style.t ->
  value ->
  string
(** Serialize a value to a YAML string.

    @param encoding Output encoding (default: UTF-8)
    @param scalar_style Preferred scalar style (default: Any)
    @param layout_style Preferred layout style (default: Any) *)

val yaml_to_buffer :
  ?encoding:Encoding.t ->
  ?scalar_style:Scalar_style.t ->
  ?layout_style:Layout_style.t ->
  ?buffer:Buffer.t ->
  yaml ->
  Buffer.t
(** Serialize a full YAML value to a buffer.

    @param encoding Output encoding (default: UTF-8)
    @param scalar_style Preferred scalar style (default: Any)
    @param layout_style Preferred layout style (default: Any)
    @param buffer
      Optional buffer to append to (allocates new one if not provided)
    @return The buffer containing the serialized YAML *)

val yaml_to_string :
  ?encoding:Encoding.t ->
  ?scalar_style:Scalar_style.t ->
  ?layout_style:Layout_style.t ->
  yaml ->
  string
(** Serialize a full YAML value to a string.

    @param encoding Output encoding (default: UTF-8)
    @param scalar_style Preferred scalar style (default: Any)
    @param layout_style Preferred layout style (default: Any) *)

val documents_to_buffer :
  ?encoding:Encoding.t ->
  ?scalar_style:Scalar_style.t ->
  ?layout_style:Layout_style.t ->
  ?resolve_aliases:bool ->
  ?buffer:Buffer.t ->
  document list ->
  Buffer.t
(** Serialize multiple documents to a buffer.

    @param encoding Output encoding (default: UTF-8)
    @param scalar_style Preferred scalar style (default: Any)
    @param layout_style Preferred layout style (default: Any)
    @param resolve_aliases Whether to expand aliases (default: true)
    @param buffer
      Optional buffer to append to (allocates new one if not provided)
    @return The buffer containing the serialized YAML *)

val documents_to_string :
  ?encoding:Encoding.t ->
  ?scalar_style:Scalar_style.t ->
  ?layout_style:Layout_style.t ->
  ?resolve_aliases:bool ->
  document list ->
  string
(** Serialize multiple documents to a YAML stream.

    @param encoding Output encoding (default: UTF-8)
    @param scalar_style Preferred scalar style (default: Any)
    @param layout_style Preferred layout style (default: Any)
    @param resolve_aliases Whether to expand aliases (default: true) *)

(** {2 Buffer Parsing} *)

val of_buffer :
  ?resolve_aliases:bool -> ?max_nodes:int -> ?max_depth:int -> Buffer.t -> value
(** Parse YAML from a buffer into a JSON-compatible value.

    @param resolve_aliases Whether to expand aliases (default: true)
    @param max_nodes Maximum nodes during alias expansion (default: 10M)
    @param max_depth Maximum alias nesting depth (default: 100)
    @raise Yamlrw_error on parse error or if multiple documents found *)

val yaml_of_buffer :
  ?resolve_aliases:bool -> ?max_nodes:int -> ?max_depth:int -> Buffer.t -> yaml
(** Parse YAML from a buffer preserving full YAML metadata.

    @param resolve_aliases Whether to expand aliases (default: false)
    @param max_nodes Maximum nodes during alias expansion (default: 10M)
    @param max_depth Maximum alias nesting depth (default: 100)
    @raise Yamlrw_error on parse error or if multiple documents found *)

val documents_of_buffer : Buffer.t -> document list
(** Parse a multi-document YAML stream from a buffer.

    @raise Yamlrw_error on parse error *)

(** {2 Conversion} *)

val to_json :
  ?resolve_aliases:bool -> ?max_nodes:int -> ?max_depth:int -> yaml -> value
(** Convert full YAML to JSON-compatible value.

    @param resolve_aliases Whether to expand aliases (default: true)
    @param max_nodes Maximum nodes during alias expansion (default: 10M)
    @param max_depth Maximum alias nesting depth (default: 100)
    @raise Yamlrw_error if alias limits exceeded or complex keys found *)

val of_json : value -> yaml
(** Convert JSON-compatible value to full YAML representation. *)

(** {2 Pretty Printing & Equality} *)

val pp : Format.formatter -> value -> unit
(** Pretty-print a value. *)

val equal : value -> value -> bool
(** Test equality of two values. *)

(** {2 Util - Value Combinators}

    Combinators for working with {!type:value} values.

    This module provides constructors, accessors, and transformations for
    JSON-compatible YAML values. *)

module Util : sig
  type t = Value.t
  (** Alias for {!type:value}. *)

  (** {3 Type Error} *)

  exception Type_error of string * t
  (** Raised when a value has unexpected type.
      [Type_error (expected, actual_value)] *)

  (** {3 Constructors} *)

  val null : t
  (** The null value. *)

  val bool : bool -> t
  (** Create a boolean value. *)

  val int : int -> t
  (** Create an integer value (stored as float). *)

  val float : float -> t
  (** Create a float value. *)

  val string : string -> t
  (** Create a string value. *)

  val strings : string list -> t
  (** Create a list of strings. *)

  val list : t list -> t
  (** Create a list value. *)

  val obj : (string * t) list -> t
  (** Create an object value from key-value pairs. *)

  (** {3 Type Predicates} *)

  val is_null : t -> bool
  (** Check if value is null. *)

  val is_bool : t -> bool
  (** Check if value is a boolean. *)

  val is_number : t -> bool
  (** Check if value is a number. *)

  val is_string : t -> bool
  (** Check if value is a string. *)

  val is_list : t -> bool
  (** Check if value is a list. *)

  val is_obj : t -> bool
  (** Check if value is an object. *)

  (** {3 Safe Accessors}

      These return [None] if the value has the wrong type. *)

  val as_null : t -> unit option
  (** Get unit if value is null. *)

  val as_bool : t -> bool option
  (** Get boolean value. *)

  val as_float : t -> float option
  (** Get float value. *)

  val as_string : t -> string option
  (** Get string value. *)

  val as_list : t -> t list option
  (** Get list value. *)

  val as_obj : t -> (string * t) list option
  (** Get object as association list. *)

  val as_int : t -> int option
  (** Get integer value if float is an exact integer. *)

  (** {3 Unsafe Accessors}

      These raise {!Type_error} if the value has the wrong type. *)

  val get_null : t -> unit
  (** Get unit or raise {!Type_error}. *)

  val get_bool : t -> bool
  (** Get boolean or raise {!Type_error}. *)

  val get_float : t -> float
  (** Get float or raise {!Type_error}. *)

  val get_string : t -> string
  (** Get string or raise {!Type_error}. *)

  val get_list : t -> t list
  (** Get list or raise {!Type_error}. *)

  val get_obj : t -> (string * t) list
  (** Get object or raise {!Type_error}. *)

  val get_int : t -> int
  (** Get integer or raise {!Type_error}. *)

  (** {3 Object Operations} *)

  val mem : string -> t -> bool
  (** [mem key obj] checks if [key] exists in object [obj]. Returns [false] if
      [obj] is not an object. *)

  val find : string -> t -> t option
  (** [find key obj] looks up [key] in object [obj]. Returns [None] if key not
      found or if [obj] is not an object. *)

  val get : string -> t -> t
  (** [get key obj] looks up [key] in object [obj]. Raises [Not_found] if key
      not found. *)

  val keys : t -> string list
  (** Get all keys from an object.
      @raise Type_error if not an object *)

  val values : t -> t list
  (** Get all values from an object.
      @raise Type_error if not an object *)

  val update : string -> t -> t -> t
  (** [update key value obj] sets [key] to [value] in [obj]. Adds the key if it
      doesn't exist.
      @raise Type_error if [obj] is not an object *)

  val remove : string -> t -> t
  (** [remove key obj] removes [key] from [obj].
      @raise Type_error if [obj] is not an object *)

  val combine : t -> t -> t
  (** [combine obj1 obj2] merges two objects, with [obj2] values taking
      precedence.
      @raise Type_error if either argument is not an object *)

  (** {3 List Operations} *)

  val map : (t -> t) -> t -> t
  (** [map f lst] applies [f] to each element of list [lst].
      @raise Type_error if [lst] is not a list *)

  val mapi : (int -> t -> t) -> t -> t
  (** [mapi f lst] applies [f i x] to each element [x] at index [i].
      @raise Type_error if [lst] is not a list *)

  val filter : (t -> bool) -> t -> t
  (** [filter pred lst] keeps elements satisfying [pred].
      @raise Type_error if [lst] is not a list *)

  val fold : ('a -> t -> 'a) -> 'a -> t -> 'a
  (** [fold f init lst] folds [f] over list [lst].
      @raise Type_error if [lst] is not a list *)

  val nth : int -> t -> t option
  (** [nth n lst] gets element at index [n]. Returns [None] if [lst] is not a
      list or index out of bounds. *)

  val length : t -> int
  (** Get the length of a list or object. Returns 0 for other types. *)

  val flatten : t -> t
  (** Flatten a list of lists into a single list. Non-list elements are kept
      as-is.
      @raise Type_error if not a list *)

  (** {3 Path Operations} *)

  val get_path : string list -> t -> t option
  (** [get_path ["a"; "b"; "c"] obj] looks up nested path [obj.a.b.c]. Returns
      [None] if any key is not found. *)

  val get_path_exn : string list -> t -> t
  (** Like {!get_path} but raises [Not_found] if path not found. *)

  (** {3 Iteration} *)

  val iter_obj : (string -> t -> unit) -> t -> unit
  (** [iter_obj f obj] calls [f key value] for each pair in [obj].
      @raise Type_error if [obj] is not an object *)

  val iter_list : (t -> unit) -> t -> unit
  (** [iter_list f lst] calls [f] on each element of [lst].
      @raise Type_error if [lst] is not a list *)

  val fold_obj : ('a -> string -> t -> 'a) -> 'a -> t -> 'a
  (** [fold_obj f init obj] folds over object key-value pairs.
      @raise Type_error if [obj] is not an object *)

  (** {3 Mapping} *)

  val map_obj : (string -> t -> t) -> t -> t
  (** [map_obj f obj] maps [f key value] over each pair in [obj].
      @raise Type_error if [obj] is not an object *)

  val filter_obj : (string -> t -> bool) -> t -> t
  (** [filter_obj pred obj] keeps pairs satisfying [pred key value].
      @raise Type_error if [obj] is not an object *)

  (** {3 Conversion Helpers}

      Get values with optional defaults. If no default is provided and the type
      doesn't match, these raise {!Type_error}. *)

  val to_bool : ?default:bool -> t -> bool
  (** Get boolean or return default.
      @raise Type_error if type doesn't match and no default provided *)

  val to_int : ?default:int -> t -> int
  (** Get integer or return default.
      @raise Type_error if type doesn't match and no default provided *)

  val to_float : ?default:float -> t -> float
  (** Get float or return default.
      @raise Type_error if type doesn't match and no default provided *)

  val to_string : ?default:string -> t -> string
  (** Get string or return default.
      @raise Type_error if type doesn't match and no default provided *)

  val to_list : ?default:t list -> t -> t list
  (** Get list or return default.
      @raise Type_error if type doesn't match and no default provided *)
end

(** {2 Stream - Low-Level Event API}

    Low-level streaming API for event-based YAML processing.

    This is useful for:
    - Processing very large YAML files incrementally
    - Building custom YAML transformers
    - Fine-grained control over YAML emission *)

module Stream : sig
  (** {3 Event Types} *)

  type event = Event.t
  (** A parsing or emitting event. *)

  type position = Position.t
  (** A position in the source (line, column, byte offset). *)

  type event_result = {
    event : event;
    start_pos : position;
    end_pos : position;
  }
  (** Result of parsing an event with its source location. *)

  (** {3 Parsing} *)

  type parser
  (** A streaming YAML parser. *)

  val parser : string -> parser
  (** Create a parser from a string. *)

  val next : parser -> event_result option
  (** Get the next event from the parser. Returns [None] when parsing is
      complete. *)

  val iter : (event -> position -> position -> unit) -> parser -> unit
  (** [iter f parser] calls [f event start_pos end_pos] for each event. *)

  val fold : ('a -> event -> 'a) -> 'a -> parser -> 'a
  (** [fold f init parser] folds [f] over all events. *)

  (** {3 Emitting} *)

  type emitter
  (** A streaming YAML emitter. *)

  val emitter : ?len:int -> unit -> emitter
  (** Create a new emitter. *)

  val contents : emitter -> string
  (** Get the emitted YAML string. *)

  val emit : emitter -> event -> unit
  (** Emit an event.
      @raise Yamlrw_error if the event sequence is invalid *)

  (** {3 Event Emission Helpers} *)

  val stream_start : emitter -> Encoding.t -> unit
  (** Emit a stream start event. *)

  val stream_end : emitter -> unit
  (** Emit a stream end event. *)

  val document_start :
    emitter -> ?version:version -> ?implicit:bool -> unit -> unit
  (** Emit a document start event.
      @param version YAML version directive
      @param implicit Whether start marker is implicit (default: true) *)

  val document_end : emitter -> ?implicit:bool -> unit -> unit
  (** Emit a document end event.
      @param implicit Whether end marker is implicit (default: true) *)

  val scalar :
    emitter ->
    ?anchor:string ->
    ?tag:string ->
    ?style:Scalar_style.t ->
    string ->
    unit
  (** Emit a scalar value.
      @param anchor Optional anchor name
      @param tag Optional type tag
      @param style Scalar style (default: Any) *)

  val alias : emitter -> string -> unit
  (** Emit an alias reference. *)

  val sequence_start :
    emitter ->
    ?anchor:string ->
    ?tag:string ->
    ?style:Layout_style.t ->
    unit ->
    unit
  (** Emit a sequence start event.
      @param anchor Optional anchor name
      @param tag Optional type tag
      @param style Layout style (default: Any) *)

  val sequence_end : emitter -> unit
  (** Emit a sequence end event. *)

  val mapping_start :
    emitter ->
    ?anchor:string ->
    ?tag:string ->
    ?style:Layout_style.t ->
    unit ->
    unit
  (** Emit a mapping start event.
      @param anchor Optional anchor name
      @param tag Optional type tag
      @param style Layout style (default: Any) *)

  val mapping_end : emitter -> unit
  (** Emit a mapping end event. *)
end

(** {2 Internal Modules}

    These modules are exposed for advanced use cases requiring fine-grained
    control over parsing, emission, or data structures.

    For typical usage, prefer the top-level functions and {!Util}. *)

module Position = Position
(** Source position tracking. *)

module Span = Span
(** Source span (range of positions). *)

module Chomping = Chomping
(** Block scalar chomping modes. *)

module Tag = Tag
(** YAML type tags. *)

module Value = Value
(** JSON-compatible value type and operations. *)

module Scalar = Scalar
(** YAML scalar with metadata. *)

module Sequence = Sequence
(** YAML sequence with metadata. *)

module Mapping = Mapping
(** YAML mapping with metadata. *)

module Yaml = Yaml
(** Full YAML value type. *)

module Document = Document
(** YAML document with directives. *)

module Token = Token
(** Lexical tokens. *)

module Scanner = Scanner
(** Lexical scanner. *)

module Event = Event
(** Parser events. *)

module Parser = Parser
(** Event-based parser. *)

module Loader = Loader
(** Document loader. *)

module Emitter = Emitter
(** Event-based emitter. *)

module Input = Input
(** Input stream utilities. *)

module Serialize = Serialize
(** Buffer serialization utilities. *)
