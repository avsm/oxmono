(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** {1 Yamlrw - A Pure OCaml YAML Parser and Emitter} *)

(** {2 Error Handling} *)

module Error = Error

exception Yamlrw_error = Error.Yamlrw_error

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

(** Default maximum nodes during alias expansion (10 million). *)
let default_max_alias_nodes = Yaml.default_max_alias_nodes

(** Default maximum alias nesting depth (100). *)
let default_max_alias_depth = Yaml.default_max_alias_depth

(** Parse a YAML string into a JSON-compatible value.

    @param resolve_aliases Whether to expand aliases (default: true)
    @param max_nodes Maximum nodes during alias expansion (default: 10M)
    @param max_depth Maximum alias nesting depth (default: 100)
    @raise Yamlrw_error on parse error or if multiple documents found *)
let of_string ?(resolve_aliases = true) ?(max_nodes = default_max_alias_nodes)
    ?(max_depth = default_max_alias_depth) s : value =
  (Loader.value_of_string ~resolve_aliases ~max_nodes ~max_depth s :> value)

(** Parse a YAML string preserving full YAML metadata (anchors, tags, etc).

    By default, aliases are NOT resolved, preserving the document structure.

    @param resolve_aliases Whether to expand aliases (default: false)
    @param max_nodes Maximum nodes during alias expansion (default: 10M)
    @param max_depth Maximum alias nesting depth (default: 100)
    @raise Yamlrw_error on parse error or if multiple documents found *)
let yaml_of_string ?(resolve_aliases = false)
    ?(max_nodes = default_max_alias_nodes)
    ?(max_depth = default_max_alias_depth) s : yaml =
  (Loader.yaml_of_string ~resolve_aliases ~max_nodes ~max_depth s :> yaml)

(** Parse a multi-document YAML stream.

    Use this when your YAML input contains multiple documents separated by
    document markers (---).

    @raise Yamlrw_error on parse error *)
let documents_of_string s : document list =
  let docs = Loader.documents_of_string s in
  List.map
    (fun (d : Document.t) : document ->
      {
        version = d.version;
        tags = d.tags;
        root = (d.root :> yaml option);
        implicit_start = d.implicit_start;
        implicit_end = d.implicit_end;
      })
    docs

(** {2 Formatting Styles} *)

module Scalar_style = Scalar_style
module Layout_style = Layout_style

(** {2 Serialization} *)

let make_config ~encoding ~scalar_style ~layout_style =
  { Emitter.default_config with encoding; scalar_style; layout_style }

(** Serialize a value to a buffer.

    @param encoding Output encoding (default: UTF-8)
    @param scalar_style Preferred scalar style (default: Any)
    @param layout_style Preferred layout style (default: Any)
    @param buffer
      Optional buffer to append to (allocates new one if not provided)
    @return The buffer containing the serialized YAML *)
let to_buffer ?(encoding = `Utf8) ?(scalar_style = `Any) ?(layout_style = `Any)
    ?buffer (value : value) =
  let config = make_config ~encoding ~scalar_style ~layout_style in
  Serialize.value_to_buffer ~config ?buffer (value :> Value.t)

(** Serialize a value to a YAML string.

    @param encoding Output encoding (default: UTF-8)
    @param scalar_style Preferred scalar style (default: Any)
    @param layout_style Preferred layout style (default: Any) *)
let to_string ?(encoding = `Utf8) ?(scalar_style = `Any) ?(layout_style = `Any)
    (value : value) =
  Buffer.contents (to_buffer ~encoding ~scalar_style ~layout_style value)

(** Serialize a full YAML value to a buffer.

    @param encoding Output encoding (default: UTF-8)
    @param scalar_style Preferred scalar style (default: Any)
    @param layout_style Preferred layout style (default: Any)
    @param buffer
      Optional buffer to append to (allocates new one if not provided)
    @return The buffer containing the serialized YAML *)
let yaml_to_buffer ?(encoding = `Utf8) ?(scalar_style = `Any)
    ?(layout_style = `Any) ?buffer (yaml : yaml) =
  let config = make_config ~encoding ~scalar_style ~layout_style in
  Serialize.yaml_to_buffer ~config ?buffer (yaml :> Yaml.t)

(** Serialize a full YAML value to a string.

    @param encoding Output encoding (default: UTF-8)
    @param scalar_style Preferred scalar style (default: Any)
    @param layout_style Preferred layout style (default: Any) *)
let yaml_to_string ?(encoding = `Utf8) ?(scalar_style = `Any)
    ?(layout_style = `Any) (yaml : yaml) =
  Buffer.contents (yaml_to_buffer ~encoding ~scalar_style ~layout_style yaml)

(** Serialize multiple documents to a buffer.

    @param encoding Output encoding (default: UTF-8)
    @param scalar_style Preferred scalar style (default: Any)
    @param layout_style Preferred layout style (default: Any)
    @param resolve_aliases Whether to expand aliases (default: true)
    @param buffer
      Optional buffer to append to (allocates new one if not provided)
    @return The buffer containing the serialized YAML *)
let documents_to_buffer ?(encoding = `Utf8) ?(scalar_style = `Any)
    ?(layout_style = `Any) ?(resolve_aliases = true) ?buffer
    (documents : document list) =
  let config = make_config ~encoding ~scalar_style ~layout_style in
  let docs' =
    List.map
      (fun (d : document) : Document.t ->
        {
          Document.version = d.version;
          Document.tags = d.tags;
          Document.root = (d.root :> Yaml.t option);
          Document.implicit_start = d.implicit_start;
          Document.implicit_end = d.implicit_end;
        })
      documents
  in
  Serialize.documents_to_buffer ~config ~resolve_aliases ?buffer docs'

(** Serialize multiple documents to a YAML stream.

    @param encoding Output encoding (default: UTF-8)
    @param scalar_style Preferred scalar style (default: Any)
    @param layout_style Preferred layout style (default: Any)
    @param resolve_aliases Whether to expand aliases (default: true) *)
let documents_to_string ?(encoding = `Utf8) ?(scalar_style = `Any)
    ?(layout_style = `Any) ?(resolve_aliases = true) (documents : document list)
    =
  Buffer.contents
    (documents_to_buffer ~encoding ~scalar_style ~layout_style ~resolve_aliases
       documents)

(** {2 Buffer Parsing} *)

(** Parse YAML from a buffer into a JSON-compatible value.

    @param resolve_aliases Whether to expand aliases (default: true)
    @param max_nodes Maximum nodes during alias expansion (default: 10M)
    @param max_depth Maximum alias nesting depth (default: 100)
    @raise Yamlrw_error on parse error or if multiple documents found *)
let of_buffer ?(resolve_aliases = true) ?(max_nodes = default_max_alias_nodes)
    ?(max_depth = default_max_alias_depth) buffer : value =
  of_string ~resolve_aliases ~max_nodes ~max_depth (Buffer.contents buffer)

(** Parse YAML from a buffer preserving full YAML metadata.

    @param resolve_aliases Whether to expand aliases (default: false)
    @param max_nodes Maximum nodes during alias expansion (default: 10M)
    @param max_depth Maximum alias nesting depth (default: 100)
    @raise Yamlrw_error on parse error or if multiple documents found *)
let yaml_of_buffer ?(resolve_aliases = false)
    ?(max_nodes = default_max_alias_nodes)
    ?(max_depth = default_max_alias_depth) buffer : yaml =
  yaml_of_string ~resolve_aliases ~max_nodes ~max_depth (Buffer.contents buffer)

(** Parse a multi-document YAML stream from a buffer.

    @raise Yamlrw_error on parse error *)
let documents_of_buffer buffer : document list =
  documents_of_string (Buffer.contents buffer)

(** {2 Conversion} *)

(** Convert full YAML to JSON-compatible value.

    @param resolve_aliases Whether to expand aliases (default: true)
    @param max_nodes Maximum nodes during alias expansion (default: 10M)
    @param max_depth Maximum alias nesting depth (default: 100)
    @raise Yamlrw_error if alias limits exceeded or complex keys found *)
let to_json ?(resolve_aliases = true) ?(max_nodes = default_max_alias_nodes)
    ?(max_depth = default_max_alias_depth) (yaml : yaml) : value =
  (Yaml.to_value ~resolve_aliases_first:resolve_aliases ~max_nodes ~max_depth
     (yaml :> Yaml.t)
    :> value)

(** Convert JSON-compatible value to full YAML representation. *)
let of_json (value : value) : yaml = (Yaml.of_value (value :> Value.t) :> yaml)

(** {2 Pretty Printing & Equality} *)

(** Pretty-print a value. *)
let pp = Value.pp

(** Test equality of two values. *)
let equal = Value.equal

(** {2 Util - Value Combinators} *)

module Util = struct
  (** Combinators for working with {!type:value} values.

      This module provides constructors, accessors, and transformations for
      JSON-compatible YAML values. *)

  type t = Value.t

  (** {3 Type Error} *)

  exception Type_error of string * t
  (** Raised when a value has unexpected type.
      [Type_error (expected, actual_value)] *)

  let type_error expected v = raise (Type_error (expected, v))

  (** {3 Constructors} *)

  let null : t = `Null
  let bool b : t = `Bool b
  let int n : t = `Float (Float.of_int n)
  let float f : t = `Float f
  let string s : t = `String s
  let strings ss : t = `A (List.map (fun s -> `String s) ss)
  let list vs : t = `A vs
  let obj pairs : t = `O pairs

  (** {3 Type Predicates} *)

  let is_null = function `Null -> true | _ -> false
  let is_bool = function `Bool _ -> true | _ -> false
  let is_number = function `Float _ -> true | _ -> false
  let is_string = function `String _ -> true | _ -> false
  let is_list = function `A _ -> true | _ -> false
  let is_obj = function `O _ -> true | _ -> false

  (** {3 Safe Accessors} *)

  let as_null = function `Null -> Some () | _ -> None
  let as_bool = function `Bool b -> Some b | _ -> None
  let as_float = function `Float f -> Some f | _ -> None
  let as_string = function `String s -> Some s | _ -> None
  let as_list = function `A l -> Some l | _ -> None
  let as_obj = function `O o -> Some o | _ -> None

  let as_int = function
    | `Float f ->
        let i = Float.to_int f in
        if Float.equal (Float.of_int i) f then Some i else None
    | _ -> None

  (** {3 Unsafe Accessors} *)

  let get_null v = match v with `Null -> () | _ -> type_error "null" v
  let get_bool v = match v with `Bool b -> b | _ -> type_error "bool" v
  let get_float v = match v with `Float f -> f | _ -> type_error "float" v
  let get_string v = match v with `String s -> s | _ -> type_error "string" v
  let get_list v = match v with `A l -> l | _ -> type_error "list" v
  let get_obj v = match v with `O o -> o | _ -> type_error "object" v
  let get_int v = match as_int v with Some i -> i | None -> type_error "int" v

  (** {3 Object Operations} *)

  let mem key = function
    | `O pairs -> List.exists (fun (k, _) -> k = key) pairs
    | _ -> false

  let find key = function `O pairs -> List.assoc_opt key pairs | _ -> None
  let get key v = match find key v with Some v -> v | None -> raise Not_found

  let keys v =
    match v with `O pairs -> List.map fst pairs | _ -> type_error "object" v

  let values v =
    match v with `O pairs -> List.map snd pairs | _ -> type_error "object" v

  let update key value = function
    | `O pairs ->
        let rec go = function
          | [] -> [ (key, value) ]
          | (k, _) :: rest when k = key -> (key, value) :: rest
          | kv :: rest -> kv :: go rest
        in
        `O (go pairs)
    | v -> type_error "object" v

  let remove key = function
    | `O pairs -> `O (List.filter (fun (k, _) -> k <> key) pairs)
    | v -> type_error "object" v

  let combine v1 v2 =
    match (v1, v2) with
    | `O o1, `O o2 -> `O (o1 @ o2)
    | `O _, _ -> type_error "object" v2
    | _, _ -> type_error "object" v1

  (** {3 List Operations} *)

  let map f = function `A l -> `A (List.map f l) | v -> type_error "list" v
  let mapi f = function `A l -> `A (List.mapi f l) | v -> type_error "list" v

  let filter pred = function
    | `A l -> `A (List.filter pred l)
    | v -> type_error "list" v

  let fold f init = function
    | `A l -> List.fold_left f init l
    | v -> type_error "list" v

  let nth n = function `A l -> List.nth_opt l n | _ -> None
  let length = function `A l -> List.length l | `O o -> List.length o | _ -> 0

  let flatten = function
    | `A l -> `A (List.concat_map (function `A inner -> inner | v -> [ v ]) l)
    | v -> type_error "list" v

  (** {3 Path Operations} *)

  let rec get_path path v =
    match path with
    | [] -> Some v
    | key :: rest -> (
        match find key v with Some child -> get_path rest child | None -> None)

  let get_path_exn path v =
    match get_path path v with Some v -> v | None -> raise Not_found

  (** {3 Iteration} *)

  let iter_obj f = function
    | `O pairs -> List.iter (fun (k, v) -> f k v) pairs
    | v -> type_error "object" v

  let iter_list f = function `A l -> List.iter f l | v -> type_error "list" v

  let fold_obj f init = function
    | `O pairs -> List.fold_left (fun acc (k, v) -> f acc k v) init pairs
    | v -> type_error "object" v

  (** {3 Mapping} *)

  let map_obj f = function
    | `O pairs -> `O (List.map (fun (k, v) -> (k, f k v)) pairs)
    | v -> type_error "object" v

  let filter_obj pred = function
    | `O pairs -> `O (List.filter (fun (k, v) -> pred k v) pairs)
    | v -> type_error "object" v

  (** {3 Conversion Helpers} *)

  let to_bool ?default v =
    match (as_bool v, default) with
    | Some b, _ -> b
    | None, Some d -> d
    | None, None -> type_error "bool" v

  let to_int ?default v =
    match (as_int v, default) with
    | Some i, _ -> i
    | None, Some d -> d
    | None, None -> type_error "int" v

  let to_float ?default v =
    match (as_float v, default) with
    | Some f, _ -> f
    | None, Some d -> d
    | None, None -> type_error "float" v

  let to_string ?default v =
    match (as_string v, default) with
    | Some s, _ -> s
    | None, Some d -> d
    | None, None -> type_error "string" v

  let to_list ?default v =
    match (as_list v, default) with
    | Some l, _ -> l
    | None, Some d -> d
    | None, None -> type_error "list" v
end

(** {2 Stream - Low-Level Event API} *)

module Stream = struct
  (** Low-level streaming API for event-based YAML processing.

      This is useful for:
      - Processing very large YAML files incrementally
      - Building custom YAML transformers
      - Fine-grained control over YAML emission *)

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
  (** Result of parsing an event. *)

  (** {3 Parsing} *)

  type parser = Parser.t
  (** A streaming YAML parser. *)

  (** Create a parser from a string. *)
  let parser s = Parser.of_string s

  (** Get the next event from the parser. Returns [None] when parsing is
      complete. *)
  let next p =
    match Parser.next p with
    | Some { event; span } ->
        Some { event; start_pos = span.start; end_pos = span.stop }
    | None -> None

  (** Iterate over all events from the parser. *)
  let iter f p =
    let rec go () =
      match next p with
      | Some { event; start_pos; end_pos } ->
          f event start_pos end_pos;
          go ()
      | None -> ()
    in
    go ()

  (** Fold over all events from the parser. *)
  let fold f init p =
    let rec go acc =
      match Parser.next p with
      | Some { event; _ } -> go (f acc event)
      | None -> acc
    in
    go init

  (** {3 Emitting} *)

  type emitter = Emitter.t
  (** A streaming YAML emitter. *)

  (** Create a new emitter. *)
  let emitter ?len:_ () = Emitter.create ()

  (** Get the emitted YAML string. *)
  let contents e = Emitter.contents e

  (** Emit an event. *)
  let emit e ev = Emitter.emit e ev

  (** {3 Event Emission Helpers} *)

  let stream_start e enc =
    Emitter.emit e (Event.Stream_start { encoding = enc })

  let stream_end e = Emitter.emit e Event.Stream_end

  let document_start e ?version ?(implicit = true) () =
    let version =
      match version with
      | Some `V1_1 -> Some (1, 1)
      | Some `V1_2 -> Some (1, 2)
      | None -> None
    in
    Emitter.emit e (Event.Document_start { version; implicit })

  let document_end e ?(implicit = true) () =
    Emitter.emit e (Event.Document_end { implicit })

  let scalar e ?anchor ?tag ?(style = `Any) value =
    Emitter.emit e
      (Event.Scalar
         {
           anchor;
           tag;
           value;
           plain_implicit = true;
           quoted_implicit = true;
           style;
         })

  let alias e name = Emitter.emit e (Event.Alias { anchor = name })

  let sequence_start e ?anchor ?tag ?(style = `Any) () =
    Emitter.emit e
      (Event.Sequence_start { anchor; tag; implicit = true; style })

  let sequence_end e = Emitter.emit e Event.Sequence_end

  let mapping_start e ?anchor ?tag ?(style = `Any) () =
    Emitter.emit e (Event.Mapping_start { anchor; tag; implicit = true; style })

  let mapping_end e = Emitter.emit e Event.Mapping_end
end

(** {2 Internal Modules} *)

(** These modules are exposed for advanced use cases requiring fine-grained
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
