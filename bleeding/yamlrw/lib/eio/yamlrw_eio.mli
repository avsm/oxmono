(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Yamlrw Eio - Streaming YAML parsing and emitting with Eio

    This library provides Eio-based streaming support for YAML parsing and
    emitting. It uses bytesrw adapters that read/write directly to Eio flows,
    with bytesrw handling internal buffering.

    {2 Quick Start}

    Read YAML from a file:
    {[
      Eio_main.run @@ fun env ->
      let fs = Eio.Stdenv.fs env in
      let value = Yaml_eio.of_file ~fs "config.yaml" in
      ...
    ]}

    Write YAML to a flow:
    {[
      Eio_main.run @@ fun env ->
      let fs = Eio.Stdenv.fs env in
      Eio.Path.with_open_out Eio.Path.(fs / "output.yaml") @@ fun flow ->
      Yaml_eio.Write.value flow (`O [ ("name", `String "test") ])
    ]}

    Stream events incrementally:
    {[
      Eio_main.run @@ fun env ->
      let fs = Eio.Stdenv.fs env in
      Eio.Path.with_open_in Eio.Path.(fs / "data.yaml") @@ fun flow ->
      Yaml_eio.Read.iter_events
        (fun event span -> Format.printf "Event at %a@." Yamlrw.Span.pp span)
        flow
    ]}

    {2 Streaming Architecture}

    This library uses bytesrw for direct I/O with Eio flows:

    - {b Reading}: Data is read directly from the flow as the parser requests
      it. Bytesrw handles internal buffering.

    - {b Writing}: Output is written directly to the flow. Bytesrw handles
      chunking and buffering. *)

(** {1 Types} *)

type value = Yamlrw.Value.t
(** JSON-compatible YAML value *)

type yaml = Yamlrw.Yaml.t
(** Full YAML value with metadata *)

type document = Yamlrw.Document.t
(** YAML document with directives *)

type event = Yamlrw.Event.t
(** Parser/emitter event *)

(** {1 Reading from Eio Sources} *)

module Read : sig
  (** Parse YAML from Eio flows.

      All functions read data incrementally from the underlying flow, without
      loading the entire file into memory first. *)

  (** {2 High-Level Parsing} *)

  val value :
    ?resolve_aliases:bool ->
    ?max_nodes:int ->
    ?max_depth:int ->
    _ Eio.Flow.source ->
    value
  (** Parse a JSON-compatible value from an Eio flow.

      @param resolve_aliases Whether to expand aliases (default: true)
      @param max_nodes Maximum nodes during alias expansion (default: 10M)
      @param max_depth Maximum alias nesting depth (default: 100) *)

  val yaml :
    ?resolve_aliases:bool ->
    ?max_nodes:int ->
    ?max_depth:int ->
    _ Eio.Flow.source ->
    yaml
  (** Parse a full YAML value from an Eio flow.

      By default, aliases are NOT resolved, preserving the document structure.

      @param resolve_aliases Whether to expand aliases (default: false)
      @param max_nodes Maximum nodes during alias expansion (default: 10M)
      @param max_depth Maximum alias nesting depth (default: 100) *)

  val documents : _ Eio.Flow.source -> document list
  (** Parse multiple YAML documents from an Eio flow. *)

  (** {2 Event-Based Streaming} *)

  type event_reader
  (** A streaming event reader backed by a flow. Events are parsed incrementally
      as requested. *)

  val event_reader : _ Eio.Flow.source -> event_reader
  (** Create an event reader from an Eio flow. *)

  val next_event : event_reader -> Yamlrw.Event.spanned option
  (** Get the next event from an event reader. Returns [None] when parsing is
      complete. *)

  val iter_events :
    (event -> Yamlrw.Span.t -> unit) -> _ Eio.Flow.source -> unit
  (** Iterate over all events from a flow. *)

  val fold_events : ('a -> event -> 'a) -> 'a -> _ Eio.Flow.source -> 'a
  (** Fold over all events from a flow. *)

  val iter_documents : (document -> unit) -> _ Eio.Flow.source -> unit
  (** Iterate over documents from a flow, calling [f] for each document. *)

  val fold_documents : ('a -> document -> 'a) -> 'a -> _ Eio.Flow.source -> 'a
  (** Fold over documents from a flow. *)
end

(** {1 Writing to Eio Sinks} *)

module Write : sig
  (** Emit YAML to Eio flows.

      All functions write data directly to the underlying flow. *)

  (** {2 High-Level Emission} *)

  val value :
    ?encoding:Yamlrw.Encoding.t ->
    ?scalar_style:Yamlrw.Scalar_style.t ->
    ?layout_style:Yamlrw.Layout_style.t ->
    _ Eio.Flow.sink ->
    value ->
    unit
  (** Write a JSON-compatible value to an Eio flow.

      @param encoding Output encoding (default: UTF-8)
      @param scalar_style Preferred scalar style (default: Any)
      @param layout_style Preferred layout style (default: Any) *)

  val yaml :
    ?encoding:Yamlrw.Encoding.t ->
    ?scalar_style:Yamlrw.Scalar_style.t ->
    ?layout_style:Yamlrw.Layout_style.t ->
    _ Eio.Flow.sink ->
    yaml ->
    unit
  (** Write a full YAML value to an Eio flow.

      @param encoding Output encoding (default: UTF-8)
      @param scalar_style Preferred scalar style (default: Any)
      @param layout_style Preferred layout style (default: Any) *)

  val documents :
    ?encoding:Yamlrw.Encoding.t ->
    ?scalar_style:Yamlrw.Scalar_style.t ->
    ?layout_style:Yamlrw.Layout_style.t ->
    ?resolve_aliases:bool ->
    _ Eio.Flow.sink ->
    document list ->
    unit
  (** Write multiple YAML documents to an Eio flow.

      @param encoding Output encoding (default: UTF-8)
      @param scalar_style Preferred scalar style (default: Any)
      @param layout_style Preferred layout style (default: Any)
      @param resolve_aliases Whether to expand aliases (default: true) *)

  (** {2 Event-Based Streaming} *)

  type event_writer
  (** A streaming event writer backed by a flow. Events are written
      incrementally to the underlying flow. *)

  val event_writer :
    ?encoding:Yamlrw.Encoding.t ->
    ?scalar_style:Yamlrw.Scalar_style.t ->
    ?layout_style:Yamlrw.Layout_style.t ->
    _ Eio.Flow.sink ->
    event_writer
  (** Create an event writer that writes directly to a flow. Events are written
      incrementally as they are emitted.

      @param encoding Output encoding (default: UTF-8)
      @param scalar_style Preferred scalar style (default: Any)
      @param layout_style Preferred layout style (default: Any) *)

  val emit : event_writer -> event -> unit
  (** Emit a single event to the writer. *)

  val flush : event_writer -> unit
  (** Flush the writer by sending end-of-data. *)

  val emit_all : _ Eio.Flow.sink -> event list -> unit
  (** Emit events from a list to a flow. *)
end

(** {1 Convenience Functions} *)

val of_file :
  ?resolve_aliases:bool ->
  ?max_nodes:int ->
  ?max_depth:int ->
  fs:_ Eio.Path.t ->
  string ->
  value
(** Read a value from a file path.

    @param fs
      The filesystem path (e.g., [Eio.Stdenv.fs env] or [Eio.Stdenv.cwd env]) *)

val yaml_of_file :
  ?resolve_aliases:bool ->
  ?max_nodes:int ->
  ?max_depth:int ->
  fs:_ Eio.Path.t ->
  string ->
  yaml
(** Read full YAML from a file path.

    @param fs
      The filesystem path (e.g., [Eio.Stdenv.fs env] or [Eio.Stdenv.cwd env]) *)

val documents_of_file : fs:_ Eio.Path.t -> string -> document list
(** Read documents from a file path.

    @param fs
      The filesystem path (e.g., [Eio.Stdenv.fs env] or [Eio.Stdenv.cwd env]) *)

val to_file :
  ?encoding:Yamlrw.Encoding.t ->
  ?scalar_style:Yamlrw.Scalar_style.t ->
  ?layout_style:Yamlrw.Layout_style.t ->
  fs:_ Eio.Path.t ->
  string ->
  value ->
  unit
(** Write a value to a file path.

    @param fs
      The filesystem path (e.g., [Eio.Stdenv.fs env] or [Eio.Stdenv.cwd env]) *)

val yaml_to_file :
  ?encoding:Yamlrw.Encoding.t ->
  ?scalar_style:Yamlrw.Scalar_style.t ->
  ?layout_style:Yamlrw.Layout_style.t ->
  fs:_ Eio.Path.t ->
  string ->
  yaml ->
  unit
(** Write full YAML to a file path.

    @param fs
      The filesystem path (e.g., [Eio.Stdenv.fs env] or [Eio.Stdenv.cwd env]) *)

val documents_to_file :
  ?encoding:Yamlrw.Encoding.t ->
  ?scalar_style:Yamlrw.Scalar_style.t ->
  ?layout_style:Yamlrw.Layout_style.t ->
  ?resolve_aliases:bool ->
  fs:_ Eio.Path.t ->
  string ->
  document list ->
  unit
(** Write documents to a file path.

    @param fs
      The filesystem path (e.g., [Eio.Stdenv.fs env] or [Eio.Stdenv.cwd env]) *)
