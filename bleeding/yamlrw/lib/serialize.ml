(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Serialize - high-level serialization to buffers and event streams

    This module provides functions to convert YAML values to events and strings.
    Both {!Emitter.t}-based and function-based emission APIs are provided. *)

(** {1 Internal Helpers} *)

(** Emit a YAML node using an emit function. This is the core implementation
    used by both Emitter.t and function-based APIs. *)
let rec emit_yaml_node_impl ~emit (yaml : Yaml.t) =
  match yaml with
  | `Scalar s ->
      emit
        (Event.Scalar
           {
             anchor = Scalar.anchor s;
             tag = Scalar.tag s;
             value = Scalar.value s;
             plain_implicit = Scalar.plain_implicit s;
             quoted_implicit = Scalar.quoted_implicit s;
             style = Scalar.style s;
           })
  | `Alias name -> emit (Event.Alias { anchor = name })
  | `A seq ->
      let members = Sequence.members seq in
      (* Force flow style for empty sequences *)
      let style = if members = [] then `Flow else Sequence.style seq in
      emit
        (Event.Sequence_start
           {
             anchor = Sequence.anchor seq;
             tag = Sequence.tag seq;
             implicit = Sequence.implicit seq;
             style;
           });
      List.iter (emit_yaml_node_impl ~emit) members;
      emit Event.Sequence_end
  | `O map ->
      let members = Mapping.members map in
      (* Force flow style for empty mappings *)
      let style = if members = [] then `Flow else Mapping.style map in
      emit
        (Event.Mapping_start
           {
             anchor = Mapping.anchor map;
             tag = Mapping.tag map;
             implicit = Mapping.implicit map;
             style;
           });
      List.iter
        (fun (k, v) ->
          emit_yaml_node_impl ~emit k;
          emit_yaml_node_impl ~emit v)
        members;
      emit Event.Mapping_end

(** Emit a Value node using an emit function. This is the core implementation
    used by both Emitter.t and function-based APIs. *)
let rec emit_value_node_impl ~emit ~config (value : Value.t) =
  match value with
  | `Null ->
      emit
        (Event.Scalar
           {
             anchor = None;
             tag = None;
             value = "null";
             plain_implicit = true;
             quoted_implicit = false;
             style = `Plain;
           })
  | `Bool b ->
      emit
        (Event.Scalar
           {
             anchor = None;
             tag = None;
             value = (if b then "true" else "false");
             plain_implicit = true;
             quoted_implicit = false;
             style = `Plain;
           })
  | `Float f ->
      let value =
        match Float.classify_float f with
        | FP_nan -> ".nan"
        | FP_infinite -> if f > 0.0 then ".inf" else "-.inf"
        | _ ->
            if Float.is_integer f && Float.abs f < 1e15 then
              Printf.sprintf "%.0f" f
            else Printf.sprintf "%g" f
      in
      emit
        (Event.Scalar
           {
             anchor = None;
             tag = None;
             value;
             plain_implicit = true;
             quoted_implicit = false;
             style = `Plain;
           })
  | `String s ->
      let style = Quoting.choose_style s in
      emit
        (Event.Scalar
           {
             anchor = None;
             tag = None;
             value = s;
             plain_implicit = style = `Plain;
             quoted_implicit = style <> `Plain;
             style;
           })
  | `A items ->
      (* Force flow style for empty sequences, otherwise use config *)
      let style =
        if items = [] || config.Emitter.layout_style = `Flow then `Flow
        else `Block
      in
      emit
        (Event.Sequence_start
           { anchor = None; tag = None; implicit = true; style });
      List.iter (emit_value_node_impl ~emit ~config) items;
      emit Event.Sequence_end
  | `O pairs ->
      (* Force flow style for empty mappings, otherwise use config *)
      let style =
        if pairs = [] || config.Emitter.layout_style = `Flow then `Flow
        else `Block
      in
      emit
        (Event.Mapping_start
           { anchor = None; tag = None; implicit = true; style });
      List.iter
        (fun (k, v) ->
          let style = Quoting.choose_style k in
          emit
            (Event.Scalar
               {
                 anchor = None;
                 tag = None;
                 value = k;
                 plain_implicit = style = `Plain;
                 quoted_implicit = style <> `Plain;
                 style;
               });
          emit_value_node_impl ~emit ~config v)
        pairs;
      emit Event.Mapping_end

(** Strip anchors from a YAML tree (used when resolving aliases for output) *)
let rec strip_anchors (yaml : Yaml.t) : Yaml.t =
  match yaml with
  | `Scalar s ->
      if Option.is_none (Scalar.anchor s) then yaml
      else
        `Scalar
          (Scalar.make ?tag:(Scalar.tag s)
             ~plain_implicit:(Scalar.plain_implicit s)
             ~quoted_implicit:(Scalar.quoted_implicit s) ~style:(Scalar.style s)
             (Scalar.value s))
  | `Alias _ -> yaml
  | `A seq ->
      `A
        (Sequence.make ?tag:(Sequence.tag seq) ~implicit:(Sequence.implicit seq)
           ~style:(Sequence.style seq)
           (List.map strip_anchors (Sequence.members seq)))
  | `O map ->
      `O
        (Mapping.make ?tag:(Mapping.tag map) ~implicit:(Mapping.implicit map)
           ~style:(Mapping.style map)
           (List.map
              (fun (k, v) -> (strip_anchors k, strip_anchors v))
              (Mapping.members map)))

(** Emit a document using an emit function *)
let emit_document_impl ?(resolve_aliases = true) ~emit doc =
  emit
    (Event.Document_start
       {
         version = Document.version doc;
         implicit = Document.implicit_start doc;
       });
  (match Document.root doc with
  | Some yaml ->
      let yaml =
        if resolve_aliases then yaml |> Yaml.resolve_aliases |> strip_anchors
        else yaml
      in
      emit_yaml_node_impl ~emit yaml
  | None ->
      emit
        (Event.Scalar
           {
             anchor = None;
             tag = None;
             value = "";
             plain_implicit = true;
             quoted_implicit = false;
             style = `Plain;
           }));
  emit (Event.Document_end { implicit = Document.implicit_end doc })

(** {1 Emitter.t-based API} *)

(** Emit a YAML node to an emitter *)
let emit_yaml_node t yaml = emit_yaml_node_impl ~emit:(Emitter.emit t) yaml

(** Emit a complete YAML document to an emitter *)
let emit_yaml t yaml =
  let config = Emitter.config t in
  Emitter.emit t (Event.Stream_start { encoding = config.encoding });
  Emitter.emit t (Event.Document_start { version = None; implicit = true });
  emit_yaml_node t yaml;
  Emitter.emit t (Event.Document_end { implicit = true });
  Emitter.emit t Event.Stream_end

(** Emit a Value node to an emitter *)
let emit_value_node t value =
  let config = Emitter.config t in
  emit_value_node_impl ~emit:(Emitter.emit t) ~config value

(** Emit a complete Value document to an emitter *)
let emit_value t value =
  let config = Emitter.config t in
  Emitter.emit t (Event.Stream_start { encoding = config.encoding });
  Emitter.emit t (Event.Document_start { version = None; implicit = true });
  emit_value_node t value;
  Emitter.emit t (Event.Document_end { implicit = true });
  Emitter.emit t Event.Stream_end

(** Emit a document to an emitter *)
let emit_document ?resolve_aliases t doc =
  emit_document_impl ?resolve_aliases ~emit:(Emitter.emit t) doc

(** {1 Buffer-based API} *)

(** Serialize a Value to a buffer.

    @param config Emitter configuration (default: {!Emitter.default_config})
    @param buffer Optional buffer to append to; creates new one if not provided
    @return The buffer containing serialized YAML *)
let value_to_buffer ?(config = Emitter.default_config) ?buffer value =
  let buf = Option.value buffer ~default:(Buffer.create 1024) in
  let t = Emitter.create ~config () in
  emit_value t value;
  Buffer.add_string buf (Emitter.contents t);
  buf

(** Serialize a Yaml.t to a buffer.

    @param config Emitter configuration (default: {!Emitter.default_config})
    @param buffer Optional buffer to append to; creates new one if not provided
    @return The buffer containing serialized YAML *)
let yaml_to_buffer ?(config = Emitter.default_config) ?buffer yaml =
  let buf = Option.value buffer ~default:(Buffer.create 1024) in
  let t = Emitter.create ~config () in
  emit_yaml t yaml;
  Buffer.add_string buf (Emitter.contents t);
  buf

(** Serialize documents to a buffer.

    @param config Emitter configuration (default: {!Emitter.default_config})
    @param resolve_aliases
      Whether to resolve aliases before emission (default: true)
    @param buffer Optional buffer to append to; creates new one if not provided
    @return The buffer containing serialized YAML *)
let documents_to_buffer ?(config = Emitter.default_config)
    ?(resolve_aliases = true) ?buffer documents =
  let buf = Option.value buffer ~default:(Buffer.create 1024) in
  let t = Emitter.create ~config () in
  Emitter.emit t (Event.Stream_start { encoding = config.encoding });
  List.iter (emit_document ~resolve_aliases t) documents;
  Emitter.emit t Event.Stream_end;
  Buffer.add_string buf (Emitter.contents t);
  buf

(** {1 String-based API} *)

(** Serialize a Value to a string.

    @param config Emitter configuration (default: {!Emitter.default_config}) *)
let value_to_string ?(config = Emitter.default_config) value =
  Buffer.contents (value_to_buffer ~config value)

(** Serialize a Yaml.t to a string.

    @param config Emitter configuration (default: {!Emitter.default_config}) *)
let yaml_to_string ?(config = Emitter.default_config) yaml =
  Buffer.contents (yaml_to_buffer ~config yaml)

(** Serialize documents to a string.

    @param config Emitter configuration (default: {!Emitter.default_config})
    @param resolve_aliases
      Whether to resolve aliases before emission (default: true) *)
let documents_to_string ?(config = Emitter.default_config)
    ?(resolve_aliases = true) documents =
  Buffer.contents (documents_to_buffer ~config ~resolve_aliases documents)

(** {1 Writer-based API}

    These functions write directly to a bytesrw [Bytes.Writer.t], enabling true
    streaming output without intermediate string allocation. Uses the emitter's
    native Writer support for efficiency. *)

(** Serialize a Value directly to a Bytes.Writer.

    @param config Emitter configuration (default: {!Emitter.default_config})
    @param eod Whether to write end-of-data after serialization (default: true)
*)
let value_to_writer ?(config = Emitter.default_config) ?(eod = true) writer
    value =
  let t = Emitter.of_writer ~config writer in
  emit_value t value;
  if eod then Emitter.flush t

(** Serialize a Yaml.t directly to a Bytes.Writer.

    @param config Emitter configuration (default: {!Emitter.default_config})
    @param eod Whether to write end-of-data after serialization (default: true)
*)
let yaml_to_writer ?(config = Emitter.default_config) ?(eod = true) writer yaml
    =
  let t = Emitter.of_writer ~config writer in
  emit_yaml t yaml;
  if eod then Emitter.flush t

(** Serialize documents directly to a Bytes.Writer.

    @param config Emitter configuration (default: {!Emitter.default_config})
    @param resolve_aliases
      Whether to resolve aliases before emission (default: true)
    @param eod Whether to write end-of-data after serialization (default: true)
*)
let documents_to_writer ?(config = Emitter.default_config)
    ?(resolve_aliases = true) ?(eod = true) writer documents =
  let t = Emitter.of_writer ~config writer in
  Emitter.emit t (Event.Stream_start { encoding = config.encoding });
  List.iter (emit_document ~resolve_aliases t) documents;
  Emitter.emit t Event.Stream_end;
  if eod then Emitter.flush t

(** {1 Function-based API}

    These functions accept an emit function [Event.t -> unit] instead of an
    {!Emitter.t}, allowing them to work with any event sink (e.g., streaming
    writers, custom processors). *)

(** Emit a YAML node using an emitter function *)
let emit_yaml_node_fn ~emitter yaml = emit_yaml_node_impl ~emit:emitter yaml

(** Emit a complete YAML stream using an emitter function *)
let emit_yaml_fn ~emitter ~config yaml =
  emitter (Event.Stream_start { encoding = config.Emitter.encoding });
  emitter (Event.Document_start { version = None; implicit = true });
  emit_yaml_node_fn ~emitter yaml;
  emitter (Event.Document_end { implicit = true });
  emitter Event.Stream_end

(** Emit a Value node using an emitter function *)
let emit_value_node_fn ~emitter ~config value =
  emit_value_node_impl ~emit:emitter ~config value

(** Emit a complete Value stream using an emitter function *)
let emit_value_fn ~emitter ~config value =
  emitter (Event.Stream_start { encoding = config.Emitter.encoding });
  emitter (Event.Document_start { version = None; implicit = true });
  emit_value_node_fn ~emitter ~config value;
  emitter (Event.Document_end { implicit = true });
  emitter Event.Stream_end

(** Emit a document using an emitter function *)
let emit_document_fn ?resolve_aliases ~emitter doc =
  emit_document_impl ?resolve_aliases ~emit:emitter doc

(** Emit multiple documents using an emitter function *)
let emit_documents ~emitter ~config ?(resolve_aliases = true) documents =
  emitter (Event.Stream_start { encoding = config.Emitter.encoding });
  List.iter (emit_document_fn ~resolve_aliases ~emitter) documents;
  emitter Event.Stream_end
