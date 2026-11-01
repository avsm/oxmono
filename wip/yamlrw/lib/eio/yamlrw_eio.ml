(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Yamlrw Eio - Streaming YAML parsing and emitting with Eio

    This module provides Eio-compatible streaming YAML parsing and emitting. It
    uses bytesrw adapters to convert Eio sources/sinks to the standard YAML
    scanner/parser/emitter, eliminating code duplication. *)

open Yamlrw

(** {1 Types} *)

type value = Value.t
type yaml = Yaml.t
type document = Document.t
type event = Event.t

(** {1 Reading from Eio Sources} *)

module Read = struct
  (** Parse YAML from Eio sources/flows *)

  (** Create a scanner from an Eio flow *)
  let scanner_of_flow flow =
    let reader = Bytesrw_eio.bytes_reader_of_flow flow in
    let input = Input.of_reader reader in
    Scanner.of_input input

  (** Create a parser from an Eio flow *)
  let parser_of_flow flow = Parser.of_scanner (scanner_of_flow flow)

  (** Parse a JSON-compatible value from an Eio flow.

      @param resolve_aliases Whether to expand aliases (default: true)
      @param max_nodes Maximum nodes during alias expansion (default: 10M)
      @param max_depth Maximum alias nesting depth (default: 100) *)
  let value ?(resolve_aliases = true)
      ?(max_nodes = Yaml.default_max_alias_nodes)
      ?(max_depth = Yaml.default_max_alias_depth) flow =
    let parser = parser_of_flow flow in
    Loader.value_of_parser ~resolve_aliases ~max_nodes ~max_depth (fun () ->
        Parser.next parser)

  (** Parse a full YAML value from an Eio flow.

      By default, aliases are NOT resolved, preserving the document structure.

      @param resolve_aliases Whether to expand aliases (default: false)
      @param max_nodes Maximum nodes during alias expansion (default: 10M)
      @param max_depth Maximum alias nesting depth (default: 100) *)
  let yaml ?(resolve_aliases = false)
      ?(max_nodes = Yaml.default_max_alias_nodes)
      ?(max_depth = Yaml.default_max_alias_depth) flow =
    let parser = parser_of_flow flow in
    Loader.yaml_of_parser ~resolve_aliases ~max_nodes ~max_depth (fun () ->
        Parser.next parser)

  (** Parse multiple YAML documents from an Eio flow. *)
  let documents flow =
    let parser = parser_of_flow flow in
    Loader.documents_of_parser (fun () -> Parser.next parser)

  (** {2 Event-Based Streaming} *)

  type event_reader = { parser : Parser.t }
  (** A streaming event reader backed by a flow *)

  (** Create an event reader from an Eio flow. This reads data incrementally as
      events are requested. *)
  let event_reader flow = { parser = parser_of_flow flow }

  (** Get the next event from an event reader. Returns [None] when parsing is
      complete. *)
  let next_event reader = Parser.next reader.parser

  (** Iterate over all events from a flow.

      @param f Called with each event and its source span *)
  let iter_events f flow =
    let parser = parser_of_flow flow in
    Parser.iter (fun ev -> f ev.event ev.span) parser

  (** Fold over all events from a flow. *)
  let fold_events f init flow =
    let parser = parser_of_flow flow in
    let rec loop acc =
      match Parser.next parser with
      | Some ev -> loop (f acc ev.event)
      | None -> acc
    in
    loop init

  (** Iterate over documents from a flow, calling [f] for each document. *)
  let iter_documents f flow =
    let parser = parser_of_flow flow in
    Loader.iter_documents_parser f (fun () -> Parser.next parser)

  (** Fold over documents from a flow. *)
  let fold_documents f init flow =
    let parser = parser_of_flow flow in
    Loader.fold_documents_parser f init (fun () -> Parser.next parser)
end

(** {1 Writing to Eio Sinks} *)

module Write = struct
  (** Emit YAML to Eio sinks/flows using true streaming output. *)

  (** Write a JSON-compatible value to an Eio flow.

      Uses the emitter's native Writer support for streaming output.

      @param encoding Output encoding (default: UTF-8)
      @param scalar_style Preferred scalar style (default: Any)
      @param layout_style Preferred layout style (default: Any) *)
  let value ?(encoding = `Utf8) ?(scalar_style = `Any) ?(layout_style = `Any)
      flow (v : value) =
    let config =
      { Emitter.default_config with encoding; scalar_style; layout_style }
    in
    let writer = Bytesrw_eio.bytes_writer_of_flow flow in
    Serialize.value_to_writer ~config writer v

  (** Write a full YAML value to an Eio flow.

      @param encoding Output encoding (default: UTF-8)
      @param scalar_style Preferred scalar style (default: Any)
      @param layout_style Preferred layout style (default: Any) *)
  let yaml ?(encoding = `Utf8) ?(scalar_style = `Any) ?(layout_style = `Any)
      flow (v : yaml) =
    let config =
      { Emitter.default_config with encoding; scalar_style; layout_style }
    in
    let writer = Bytesrw_eio.bytes_writer_of_flow flow in
    Serialize.yaml_to_writer ~config writer v

  (** Write multiple YAML documents to an Eio flow.

      @param encoding Output encoding (default: UTF-8)
      @param scalar_style Preferred scalar style (default: Any)
      @param layout_style Preferred layout style (default: Any)
      @param resolve_aliases Whether to expand aliases (default: true) *)
  let documents ?(encoding = `Utf8) ?(scalar_style = `Any)
      ?(layout_style = `Any) ?(resolve_aliases = true) flow docs =
    let config =
      { Emitter.default_config with encoding; scalar_style; layout_style }
    in
    let writer = Bytesrw_eio.bytes_writer_of_flow flow in
    Serialize.documents_to_writer ~config ~resolve_aliases writer docs

  (** {2 Event-Based Streaming} *)

  type event_writer = { emitter : Emitter.t }
  (** A streaming event writer that writes directly to a flow *)

  (** Create an event writer that writes directly to a flow. Events are written
      incrementally as they are emitted.

      @param encoding Output encoding (default: UTF-8)
      @param scalar_style Preferred scalar style (default: Any)
      @param layout_style Preferred layout style (default: Any) *)
  let event_writer ?(encoding = `Utf8) ?(scalar_style = `Any)
      ?(layout_style = `Any) flow =
    let config =
      { Emitter.default_config with encoding; scalar_style; layout_style }
    in
    let writer = Bytesrw_eio.bytes_writer_of_flow flow in
    { emitter = Emitter.of_writer ~config writer }

  (** Emit a single event to the writer. *)
  let emit ew ev = Emitter.emit ew.emitter ev

  (** Flush the writer by sending end-of-data. *)
  let flush ew = Emitter.flush ew.emitter

  (** Emit events from a list to a flow. *)
  let emit_all flow events =
    let ew = event_writer flow in
    List.iter (emit ew) events;
    flush ew
end

(** {1 Convenience Functions} *)

(** Read a value from a file path *)
let of_file ?(resolve_aliases = true)
    ?(max_nodes = Yaml.default_max_alias_nodes)
    ?(max_depth = Yaml.default_max_alias_depth) ~fs path =
  Eio.Path.with_open_in Eio.Path.(fs / path) @@ fun flow ->
  Read.value ~resolve_aliases ~max_nodes ~max_depth flow

(** Read full YAML from a file path *)
let yaml_of_file ?(resolve_aliases = false)
    ?(max_nodes = Yaml.default_max_alias_nodes)
    ?(max_depth = Yaml.default_max_alias_depth) ~fs path =
  Eio.Path.with_open_in Eio.Path.(fs / path) @@ fun flow ->
  Read.yaml ~resolve_aliases ~max_nodes ~max_depth flow

(** Read documents from a file path *)
let documents_of_file ~fs path =
  Eio.Path.with_open_in Eio.Path.(fs / path) @@ fun flow -> Read.documents flow

(** Write a value to a file path *)
let to_file ?(encoding = `Utf8) ?(scalar_style = `Any) ?(layout_style = `Any)
    ~fs path v =
  Eio.Path.with_open_out ~create:(`Or_truncate 0o644) Eio.Path.(fs / path)
  @@ fun flow -> Write.value ~encoding ~scalar_style ~layout_style flow v

(** Write full YAML to a file path *)
let yaml_to_file ?(encoding = `Utf8) ?(scalar_style = `Any)
    ?(layout_style = `Any) ~fs path v =
  Eio.Path.with_open_out ~create:(`Or_truncate 0o644) Eio.Path.(fs / path)
  @@ fun flow -> Write.yaml ~encoding ~scalar_style ~layout_style flow v

(** Write documents to a file path *)
let documents_to_file ?(encoding = `Utf8) ?(scalar_style = `Any)
    ?(layout_style = `Any) ?(resolve_aliases = true) ~fs path docs =
  Eio.Path.with_open_out ~create:(`Or_truncate 0o644) Eio.Path.(fs / path)
  @@ fun flow ->
  Write.documents ~encoding ~scalar_style ~layout_style ~resolve_aliases flow
    docs
