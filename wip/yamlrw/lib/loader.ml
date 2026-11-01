(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Loader - converts parser events to YAML data structures *)

(** Stack frame for building nested structures *)
type frame =
  | Sequence_frame of {
      anchor : string option;
      tag : string option;
      implicit : bool;
      style : Layout_style.t;
      items : Yaml.t list;
    }
  | Mapping_frame of {
      anchor : string option;
      tag : string option;
      implicit : bool;
      style : Layout_style.t;
      pairs : (Yaml.t * Yaml.t) list;
      pending_key : Yaml.t option;
    }

type state = {
  mutable stack : frame list;
  mutable current : Yaml.t option;
  mutable documents : Document.t list;
  mutable doc_version : (int * int) option;
  mutable doc_implicit_start : bool;
}

let create_state () =
  {
    stack = [];
    current = None;
    documents = [];
    doc_version = None;
    doc_implicit_start = true;
  }

(** Process a single event *)
let rec process_event state (ev : Event.spanned) =
  match ev.event with
  | Event.Stream_start _ -> ()
  | Event.Stream_end -> ()
  | Event.Document_start { version; implicit } ->
      state.doc_version <- version;
      state.doc_implicit_start <- implicit
  | Event.Document_end { implicit } ->
      let doc =
        Document.make ?version:state.doc_version
          ~implicit_start:state.doc_implicit_start ~implicit_end:implicit
          state.current
      in
      state.documents <- doc :: state.documents;
      state.current <- None;
      state.doc_version <- None;
      state.doc_implicit_start <- true
  | Event.Alias { anchor } ->
      let node : Yaml.t = `Alias anchor in
      add_node state node
  | Event.Scalar { anchor; tag; value; plain_implicit; quoted_implicit; style }
    ->
      let scalar =
        Scalar.make ?anchor ?tag ~plain_implicit ~quoted_implicit ~style value
      in
      let node : Yaml.t = `Scalar scalar in
      add_node state node
  | Event.Sequence_start { anchor; tag; implicit; style } ->
      let frame = Sequence_frame { anchor; tag; implicit; style; items = [] } in
      state.stack <- frame :: state.stack
  | Event.Sequence_end -> (
      match state.stack with
      | Sequence_frame { anchor; tag; implicit; style; items } :: rest ->
          let seq =
            Sequence.make ?anchor ?tag ~implicit ~style (List.rev items)
          in
          let node : Yaml.t = `A seq in
          state.stack <- rest;
          add_node state node
      | _ -> Error.raise (Invalid_state "unexpected sequence end"))
  | Event.Mapping_start { anchor; tag; implicit; style } ->
      let frame =
        Mapping_frame
          { anchor; tag; implicit; style; pairs = []; pending_key = None }
      in
      state.stack <- frame :: state.stack
  | Event.Mapping_end -> (
      match state.stack with
      | Mapping_frame
          { anchor; tag; implicit; style; pairs; pending_key = None }
        :: rest ->
          let map =
            Mapping.make ?anchor ?tag ~implicit ~style (List.rev pairs)
          in
          let node : Yaml.t = `O map in
          state.stack <- rest;
          add_node state node
      | Mapping_frame { pending_key = Some _; _ } :: _ ->
          Error.raise (Invalid_state "mapping ended with pending key")
      | _ -> Error.raise (Invalid_state "unexpected mapping end"))

(** Add a node to current context *)
and add_node state node =
  match state.stack with
  | [] -> state.current <- Some node
  | Sequence_frame f :: rest ->
      state.stack <- Sequence_frame { f with items = node :: f.items } :: rest
  | Mapping_frame f :: rest -> (
      match f.pending_key with
      | None ->
          (* This is a key *)
          state.stack <-
            Mapping_frame { f with pending_key = Some node } :: rest
      | Some key ->
          (* This is a value *)
          state.stack <-
            Mapping_frame
              { f with pairs = (key, node) :: f.pairs; pending_key = None }
            :: rest)

(** Internal: parse all documents from a parser *)
let parse_all_documents parser =
  let state = create_state () in
  Parser.iter (process_event state) parser;
  List.rev state.documents

(** Internal: extract single document or raise *)
let single_document_or_error docs ~empty =
  match docs with
  | [] -> empty
  | [ doc ] -> doc
  | _ -> Error.raise Multiple_documents

(** Load single document as Value.

    @param resolve_aliases Whether to resolve aliases (default true)
    @param max_nodes Maximum nodes during alias expansion (default 10M)
    @param max_depth Maximum alias nesting depth (default 100) *)
let value_of_string ?(resolve_aliases = true)
    ?(max_nodes = Yaml.default_max_alias_nodes)
    ?(max_depth = Yaml.default_max_alias_depth) s =
  let docs = parse_all_documents (Parser.of_string s) in
  let doc = single_document_or_error docs ~empty:(Document.make None) in
  match Document.root doc with
  | None -> `Null
  | Some yaml ->
      Yaml.to_value ~resolve_aliases_first:resolve_aliases ~max_nodes ~max_depth
        yaml

(** Load single document as Yaml.

    @param resolve_aliases Whether to resolve aliases (default false for Yaml.t)
    @param max_nodes Maximum nodes during alias expansion (default 10M)
    @param max_depth Maximum alias nesting depth (default 100) *)
let yaml_of_string ?(resolve_aliases = false)
    ?(max_nodes = Yaml.default_max_alias_nodes)
    ?(max_depth = Yaml.default_max_alias_depth) s =
  let docs = parse_all_documents (Parser.of_string s) in
  let doc = single_document_or_error docs ~empty:(Document.make None) in
  match Document.root doc with
  | None -> `Scalar (Scalar.make "")
  | Some yaml ->
      if resolve_aliases then Yaml.resolve_aliases ~max_nodes ~max_depth yaml
      else yaml

(** Load all documents *)
let documents_of_string s = parse_all_documents (Parser.of_string s)

(** {2 Reader-based loading} *)

(** Load single document as Value from a Bytes.Reader.

    @param resolve_aliases Whether to resolve aliases (default true)
    @param max_nodes Maximum nodes during alias expansion (default 10M)
    @param max_depth Maximum alias nesting depth (default 100) *)
let value_of_reader ?(resolve_aliases = true)
    ?(max_nodes = Yaml.default_max_alias_nodes)
    ?(max_depth = Yaml.default_max_alias_depth) reader =
  let docs = parse_all_documents (Parser.of_reader reader) in
  let doc = single_document_or_error docs ~empty:(Document.make None) in
  match Document.root doc with
  | None -> `Null
  | Some yaml ->
      Yaml.to_value ~resolve_aliases_first:resolve_aliases ~max_nodes ~max_depth
        yaml

(** Load single document as Yaml from a Bytes.Reader.

    @param resolve_aliases Whether to resolve aliases (default false for Yaml.t)
    @param max_nodes Maximum nodes during alias expansion (default 10M)
    @param max_depth Maximum alias nesting depth (default 100) *)
let yaml_of_reader ?(resolve_aliases = false)
    ?(max_nodes = Yaml.default_max_alias_nodes)
    ?(max_depth = Yaml.default_max_alias_depth) reader =
  let docs = parse_all_documents (Parser.of_reader reader) in
  let doc = single_document_or_error docs ~empty:(Document.make None) in
  match Document.root doc with
  | None -> `Scalar (Scalar.make "")
  | Some yaml ->
      if resolve_aliases then Yaml.resolve_aliases ~max_nodes ~max_depth yaml
      else yaml

(** Load all documents from a Bytes.Reader *)
let documents_of_reader reader = parse_all_documents (Parser.of_reader reader)

(** {2 Parser-function based loading}

    These functions accept a [unit -> Event.spanned option] function instead of
    a [Parser.t], allowing them to work with any event source (e.g., streaming
    parsers). *)

(** Generic document loader using event source function *)
let load_generic_fn extract next_event =
  let state = create_state () in
  let rec loop () =
    match next_event () with
    | None -> None
    | Some ev -> (
        process_event state ev;
        match ev.event with
        | Event.Document_end _ -> (
            match state.documents with
            | doc :: _ ->
                state.documents <- [];
                Some (extract doc)
            | [] -> None)
        | Event.Stream_end -> None
        | _ -> loop ())
  in
  loop ()

(** Generic document loader - extracts common pattern from load_* functions *)
let load_generic extract parser =
  load_generic_fn extract (fun () -> Parser.next parser)

(** Load single Value from parser.

    @param resolve_aliases Whether to resolve aliases (default true)
    @param max_nodes Maximum nodes during alias expansion (default 10M)
    @param max_depth Maximum alias nesting depth (default 100) *)
let load_value ?(resolve_aliases = true)
    ?(max_nodes = Yaml.default_max_alias_nodes)
    ?(max_depth = Yaml.default_max_alias_depth) parser =
  load_generic
    (fun doc ->
      match Document.root doc with
      | None -> `Null
      | Some yaml ->
          Yaml.to_value ~resolve_aliases_first:resolve_aliases ~max_nodes
            ~max_depth yaml)
    parser

(** Load single Yaml from parser *)
let load_yaml parser =
  load_generic
    (fun doc ->
      Document.root doc |> Option.value ~default:(`Scalar (Scalar.make "")))
    parser

(** Load single Document from parser *)
let load_document parser = load_generic Fun.id parser

(** Iterate over documents *)
let iter_documents f parser =
  let rec loop () =
    match load_document parser with
    | None -> ()
    | Some doc ->
        f doc;
        loop ()
  in
  loop ()

(** Fold over documents *)
let fold_documents f init parser =
  let rec loop acc =
    match load_document parser with None -> acc | Some doc -> loop (f acc doc)
  in
  loop init

(** Load single Value from event source.

    @param resolve_aliases Whether to resolve aliases (default true)
    @param max_nodes Maximum nodes during alias expansion (default 10M)
    @param max_depth Maximum alias nesting depth (default 100) *)
let value_of_parser ?(resolve_aliases = true)
    ?(max_nodes = Yaml.default_max_alias_nodes)
    ?(max_depth = Yaml.default_max_alias_depth) next_event =
  match
    load_generic_fn
      (fun doc ->
        match Document.root doc with
        | None -> `Null
        | Some yaml ->
            Yaml.to_value ~resolve_aliases_first:resolve_aliases ~max_nodes
              ~max_depth yaml)
      next_event
  with
  | Some v -> v
  | None -> `Null

(** Load single Yaml from event source.

    @param resolve_aliases Whether to resolve aliases (default false)
    @param max_nodes Maximum nodes during alias expansion (default 10M)
    @param max_depth Maximum alias nesting depth (default 100) *)
let yaml_of_parser ?(resolve_aliases = false)
    ?(max_nodes = Yaml.default_max_alias_nodes)
    ?(max_depth = Yaml.default_max_alias_depth) next_event =
  match
    load_generic_fn
      (fun doc ->
        match Document.root doc with
        | None -> `Scalar (Scalar.make "")
        | Some yaml ->
            if resolve_aliases then
              Yaml.resolve_aliases ~max_nodes ~max_depth yaml
            else yaml)
      next_event
  with
  | Some v -> v
  | None -> `Scalar (Scalar.make "")

(** Load single Document from event source *)
let document_of_parser next_event = load_generic_fn Fun.id next_event

(** Load all documents from event source *)
let documents_of_parser next_event =
  let state = create_state () in
  let rec loop () =
    match next_event () with
    | None -> List.rev state.documents
    | Some ev ->
        process_event state ev;
        loop ()
  in
  loop ()

(** Iterate over documents from event source *)
let iter_documents_parser f next_event =
  let rec loop () =
    match document_of_parser next_event with
    | None -> ()
    | Some doc ->
        f doc;
        loop ()
  in
  loop ()

(** Fold over documents from event source *)
let fold_documents_parser f init next_event =
  let rec loop acc =
    match document_of_parser next_event with
    | None -> acc
    | Some doc -> loop (f acc doc)
  in
  loop init
