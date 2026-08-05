(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** YAML parser - converts tokens to semantic events via state machine *)

(** Parser states *)
type state =
  | Stream_start
  | Implicit_document_start
  | Document_content
  | Document_content_done
    (* After parsing a node, check for unexpected content *)
  | Document_end
  | Block_sequence_first_entry
  | Block_sequence_entry
  | Indentless_sequence_entry
  | Block_mapping_first_key
  | Block_mapping_key
  | Block_mapping_value
  | Flow_sequence_first_entry
  | Flow_sequence_entry
  | Flow_sequence_entry_mapping_key
  | Flow_sequence_entry_mapping_value
  | Flow_sequence_entry_mapping_end
  | Flow_mapping_first_key
  | Flow_mapping_key
  | Flow_mapping_value
  | End

type t = {
  scanner : Scanner.t;
  mutable state : state;
  mutable states : state list;  (** State stack *)
  mutable version : (int * int) option;
  mutable tag_directives : (string * string) list;
  mutable current_token : Token.spanned option;
  mutable finished : bool;
  mutable explicit_doc_end : bool;
      (** True if last doc ended with explicit ... *)
  mutable stream_start : bool;
      (** True if we haven't emitted any documents yet *)
}

let create scanner =
  {
    scanner;
    state = Stream_start;
    states = [];
    version = None;
    tag_directives = [ ("!", "!"); ("!!", "tag:yaml.org,2002:") ];
    current_token = None;
    finished = false;
    explicit_doc_end = false;
    stream_start = true;
  }

let of_string s = create (Scanner.of_string s)
let of_scanner = create
let of_input i = create (Scanner.of_input i)
let of_reader r = create (Scanner.of_reader r)

(** Get current token, fetching if needed *)
let current_token t =
  match t.current_token with
  | Some tok -> tok
  | None -> (
      let tok = Scanner.next t.scanner in
      t.current_token <- tok;
      match tok with Some tok -> tok | None -> Error.raise Unexpected_eof)

(** Peek at current token *)
let peek_token t =
  match t.current_token with
  | Some _ -> t.current_token
  | None ->
      t.current_token <- Scanner.next t.scanner;
      t.current_token

(** Skip current token *)
let skip_token t = t.current_token <- None

(** Check if current token matches predicate *)
let check t pred =
  match peek_token t with Some tok -> pred tok.token | None -> false

(** Push state onto stack *)
let push_state t s = t.states <- s :: t.states

(** Pop state from stack *)
let pop_state t =
  match t.states with
  | s :: rest ->
      t.states <- rest;
      s
  | [] -> End

(** Resolve a tag *)
let resolve_tag t ~handle ~suffix =
  if handle = "" then
    (* Verbatim tag - suffix is already the full URI *)
    suffix
  else
    match List.assoc_opt handle t.tag_directives with
    | Some prefix -> prefix ^ suffix
    | None when handle = "!" -> "!" ^ suffix
    | None -> Error.raise (Invalid_tag (handle ^ suffix))

(** Process directives at document start *)
let process_directives t =
  t.version <- None;
  t.tag_directives <- [ ("!", "!"); ("!!", "tag:yaml.org,2002:") ];

  while
    check t (function
      | Token.Version_directive _ | Token.Tag_directive _ -> true
      | _ -> false)
  do
    let tok = current_token t in
    skip_token t;
    match tok.token with
    | Token.Version_directive { major; minor } ->
        if t.version <> None then
          Error.raise_span tok.span
            (Invalid_yaml_version "duplicate YAML directive");
        t.version <- Some (major, minor)
    | Token.Tag_directive { handle; prefix } ->
        (* Skip empty tag directives (these are reserved/unknown directives that were ignored) *)
        if handle = "" && prefix = "" then () (* Ignore reserved directives *)
        else begin
          if
            List.mem_assoc handle t.tag_directives
            && handle <> "!" && handle <> "!!"
          then
            Error.raise_span tok.span
              (Invalid_tag_directive ("duplicate tag handle: " ^ handle));
          t.tag_directives <- (handle, prefix) :: t.tag_directives
        end
    | _ -> ()
  done

(** Parse anchor and/or tag properties *)
let parse_properties t =
  let anchor = ref None in
  let tag = ref None in

  while
    check t (function Token.Anchor _ | Token.Tag _ -> true | _ -> false)
  do
    let tok = current_token t in
    skip_token t;
    match tok.token with
    | Token.Anchor name ->
        if Option.is_some !anchor then
          Error.raise_span tok.span (Duplicate_anchor name);
        anchor := Some name
    | Token.Tag { handle; suffix } ->
        if Option.is_some !tag then
          Error.raise_span tok.span (Invalid_tag "duplicate tag");
        let resolved =
          if handle = "" && suffix = "" then None
          else if handle = "!" && suffix = "" then Some "!"
          else Some (resolve_tag t ~handle ~suffix)
        in
        tag := resolved
    | _ -> ()
  done;
  (!anchor, !tag)

(** Empty scalar event *)
let empty_scalar_event ~anchor ~tag span =
  ( Event.Scalar
      {
        anchor;
        tag;
        value = "";
        plain_implicit = tag = None;
        quoted_implicit = false;
        style = `Plain;
      },
    span )

(** Parse stream start *)
let parse_stream_start t =
  let tok = current_token t in
  skip_token t;
  match tok.token with
  | Token.Stream_start encoding ->
      t.state <- Implicit_document_start;
      (Event.Stream_start { encoding }, tok.span)
  | _ -> Error.raise_span tok.span (Unexpected_token "expected stream start")

(** Parse document start (implicit or explicit) *)
let parse_document_start t ~implicit =
  process_directives t;

  if not implicit then begin
    let tok = current_token t in
    match tok.token with
    | Token.Document_start -> skip_token t
    | _ -> Error.raise_span tok.span Expected_document_start
  end;

  let span =
    match peek_token t with
    | Some tok -> tok.span
    | None -> Span.point Position.initial
  in

  (* After first document, stream_start is false *)
  t.stream_start <- false;
  push_state t Document_end;
  t.state <- Document_content;
  (Event.Document_start { version = t.version; implicit }, span)

(** Parse document end *)
let parse_document_end t =
  let implicit =
    not (check t (function Token.Document_end -> true | _ -> false))
  in
  let span =
    match peek_token t with
    | Some tok -> tok.span
    | None -> Span.point Position.initial
  in

  if not implicit then skip_token t;

  (* Track if this document ended explicitly with ... *)
  t.explicit_doc_end <- not implicit;
  t.state <- Implicit_document_start;
  (Event.Document_end { implicit }, span)

(** Parse node in various contexts *)
let parse_node t ~block ~indentless =
  let tok = current_token t in
  match tok.token with
  | Token.Alias name ->
      skip_token t;
      t.state <- pop_state t;
      (Event.Alias { anchor = name }, tok.span)
  | Token.Anchor _ | Token.Tag _ -> (
      let anchor, tag = parse_properties t in
      let tok = current_token t in
      match tok.token with
      | Token.Block_entry when indentless ->
          t.state <- Indentless_sequence_entry;
          ( Event.Sequence_start
              { anchor; tag; implicit = tag = None; style = `Block },
            tok.span )
      | Token.Block_sequence_start when block ->
          t.state <- Block_sequence_first_entry;
          skip_token t;
          ( Event.Sequence_start
              { anchor; tag; implicit = tag = None; style = `Block },
            tok.span )
      | Token.Block_mapping_start when block ->
          t.state <- Block_mapping_first_key;
          skip_token t;
          ( Event.Mapping_start
              { anchor; tag; implicit = tag = None; style = `Block },
            tok.span )
      | Token.Flow_sequence_start ->
          t.state <- Flow_sequence_first_entry;
          skip_token t;
          ( Event.Sequence_start
              { anchor; tag; implicit = tag = None; style = `Flow },
            tok.span )
      | Token.Flow_mapping_start ->
          t.state <- Flow_mapping_first_key;
          skip_token t;
          ( Event.Mapping_start
              { anchor; tag; implicit = tag = None; style = `Flow },
            tok.span )
      | Token.Scalar { style; value } ->
          skip_token t;
          t.state <- pop_state t;
          let plain_implicit = tag = None && style = `Plain in
          let quoted_implicit = tag = None && style <> `Plain in
          ( Event.Scalar
              { anchor; tag; value; plain_implicit; quoted_implicit; style },
            tok.span )
      | _ ->
          (* Empty node *)
          t.state <- pop_state t;
          empty_scalar_event ~anchor ~tag tok.span)
  | Token.Block_sequence_start when block ->
      t.state <- Block_sequence_first_entry;
      skip_token t;
      ( Event.Sequence_start
          { anchor = None; tag = None; implicit = true; style = `Block },
        tok.span )
  | Token.Block_mapping_start when block ->
      t.state <- Block_mapping_first_key;
      skip_token t;
      ( Event.Mapping_start
          { anchor = None; tag = None; implicit = true; style = `Block },
        tok.span )
  | Token.Flow_sequence_start ->
      t.state <- Flow_sequence_first_entry;
      skip_token t;
      ( Event.Sequence_start
          { anchor = None; tag = None; implicit = true; style = `Flow },
        tok.span )
  | Token.Flow_mapping_start ->
      t.state <- Flow_mapping_first_key;
      skip_token t;
      ( Event.Mapping_start
          { anchor = None; tag = None; implicit = true; style = `Flow },
        tok.span )
  | Token.Block_entry when indentless ->
      t.state <- Indentless_sequence_entry;
      ( Event.Sequence_start
          { anchor = None; tag = None; implicit = true; style = `Block },
        tok.span )
  | Token.Scalar { style; value } ->
      skip_token t;
      t.state <- pop_state t;
      let plain_implicit = style = `Plain in
      let quoted_implicit = style <> `Plain in
      ( Event.Scalar
          {
            anchor = None;
            tag = None;
            value;
            plain_implicit;
            quoted_implicit;
            style;
          },
        tok.span )
  | _ ->
      (* Empty node *)
      t.state <- pop_state t;
      empty_scalar_event ~anchor:None ~tag:None tok.span

(** Parse block sequence entry *)
let parse_block_sequence_entry t =
  let tok = current_token t in
  match tok.token with
  | Token.Block_entry ->
      skip_token t;
      if
        check t (function
          | Token.Block_entry | Token.Block_end -> true
          | _ -> false)
      then begin
        t.state <- Block_sequence_entry;
        empty_scalar_event ~anchor:None ~tag:None tok.span
      end
      else begin
        push_state t Block_sequence_entry;
        parse_node t ~block:true ~indentless:false
      end
  | Token.Block_end ->
      skip_token t;
      t.state <- pop_state t;
      (Event.Sequence_end, tok.span)
  | _ -> Error.raise_span tok.span Expected_block_entry

(** Parse block mapping key *)
let parse_block_mapping_key t =
  let tok = current_token t in
  match tok.token with
  | Token.Key ->
      skip_token t;
      if
        check t (function
          | Token.Key | Token.Value | Token.Block_end -> true
          | _ -> false)
      then begin
        t.state <- Block_mapping_value;
        empty_scalar_event ~anchor:None ~tag:None tok.span
      end
      else begin
        push_state t Block_mapping_value;
        parse_node t ~block:true ~indentless:true
      end
  (* Handle value without explicit key - key is empty/null *)
  | Token.Value ->
      t.state <- Block_mapping_value;
      empty_scalar_event ~anchor:None ~tag:None tok.span
  | Token.Block_end ->
      skip_token t;
      t.state <- pop_state t;
      (Event.Mapping_end, tok.span)
  | _ -> Error.raise_span tok.span Expected_key

(** Parse block mapping value *)
let parse_block_mapping_value t =
  let tok = current_token t in
  match tok.token with
  | Token.Value ->
      skip_token t;
      if
        check t (function
          | Token.Key | Token.Value | Token.Block_end -> true
          | _ -> false)
      then begin
        t.state <- Block_mapping_key;
        empty_scalar_event ~anchor:None ~tag:None tok.span
      end
      else begin
        push_state t Block_mapping_key;
        parse_node t ~block:true ~indentless:true
      end
  | _ ->
      (* Implicit empty value *)
      t.state <- Block_mapping_key;
      empty_scalar_event ~anchor:None ~tag:None tok.span

(** Parse indentless sequence entry *)
let parse_indentless_sequence_entry t =
  let tok = current_token t in
  match tok.token with
  | Token.Block_entry ->
      skip_token t;
      if
        check t (function
          | Token.Block_entry | Token.Key | Token.Value | Token.Block_end ->
              true
          | _ -> false)
      then begin
        t.state <- Indentless_sequence_entry;
        empty_scalar_event ~anchor:None ~tag:None tok.span
      end
      else begin
        push_state t Indentless_sequence_entry;
        parse_node t ~block:true ~indentless:false
      end
  | _ ->
      t.state <- pop_state t;
      (Event.Sequence_end, tok.span)

(** Parse flow sequence *)
let rec parse_flow_sequence_entry t ~first =
  let tok = current_token t in
  match tok.token with
  | Token.Flow_sequence_end ->
      skip_token t;
      t.state <- pop_state t;
      (Event.Sequence_end, tok.span)
  | Token.Flow_entry when not first ->
      skip_token t;
      parse_flow_sequence_entry_internal t
  | _ when first -> parse_flow_sequence_entry_internal t
  | _ -> Error.raise_span tok.span Expected_sequence_end

and parse_flow_sequence_entry_internal t =
  let tok = current_token t in
  match tok.token with
  | Token.Flow_sequence_end ->
      (* Trailing comma case - don't emit empty scalar, just go back to sequence entry state *)
      skip_token t;
      t.state <- pop_state t;
      (Event.Sequence_end, tok.span)
  | Token.Flow_entry ->
      (* Double comma or comma after comma - invalid *)
      Error.raise_span tok.span
        (Unexpected_token "unexpected ',' in flow sequence")
  | Token.Key ->
      skip_token t;
      t.state <- Flow_sequence_entry_mapping_key;
      ( Event.Mapping_start
          { anchor = None; tag = None; implicit = true; style = `Flow },
        tok.span )
  | Token.Value ->
      (* Implicit empty key mapping: [ : value ] *)
      t.state <- Flow_sequence_entry_mapping_key;
      ( Event.Mapping_start
          { anchor = None; tag = None; implicit = true; style = `Flow },
        tok.span )
  | _ ->
      push_state t Flow_sequence_entry;
      parse_node t ~block:false ~indentless:false

(** Parse flow sequence entry mapping *)
let parse_flow_sequence_entry_mapping_key t =
  let tok = current_token t in
  if
    check t (function
      | Token.Value | Token.Flow_entry | Token.Flow_sequence_end -> true
      | _ -> false)
  then begin
    t.state <- Flow_sequence_entry_mapping_value;
    empty_scalar_event ~anchor:None ~tag:None tok.span
  end
  else begin
    push_state t Flow_sequence_entry_mapping_value;
    parse_node t ~block:false ~indentless:false
  end

let parse_flow_sequence_entry_mapping_value t =
  let tok = current_token t in
  match tok.token with
  | Token.Value ->
      skip_token t;
      if
        check t (function
          | Token.Flow_entry | Token.Flow_sequence_end -> true
          | _ -> false)
      then begin
        t.state <- Flow_sequence_entry_mapping_end;
        empty_scalar_event ~anchor:None ~tag:None tok.span
      end
      else begin
        push_state t Flow_sequence_entry_mapping_end;
        parse_node t ~block:false ~indentless:false
      end
  | _ ->
      t.state <- Flow_sequence_entry_mapping_end;
      empty_scalar_event ~anchor:None ~tag:None tok.span

let parse_flow_sequence_entry_mapping_end t =
  let tok = current_token t in
  t.state <- Flow_sequence_entry;
  (Event.Mapping_end, tok.span)

(** Parse flow mapping *)
let rec parse_flow_mapping_key t ~first =
  let tok = current_token t in
  match tok.token with
  | Token.Flow_mapping_end ->
      skip_token t;
      t.state <- pop_state t;
      (Event.Mapping_end, tok.span)
  | Token.Flow_entry when not first ->
      skip_token t;
      parse_flow_mapping_key_internal t
  | _ when first -> parse_flow_mapping_key_internal t
  | _ -> Error.raise_span tok.span Expected_mapping_end

and parse_flow_mapping_key_internal t =
  let tok = current_token t in
  match tok.token with
  | Token.Flow_mapping_end ->
      (* Trailing comma case - don't emit empty scalar, just return to key state *)
      skip_token t;
      t.state <- pop_state t;
      (Event.Mapping_end, tok.span)
  | Token.Flow_entry ->
      (* Double comma or comma after comma - invalid *)
      Error.raise_span tok.span
        (Unexpected_token "unexpected ',' in flow mapping")
  | Token.Key ->
      skip_token t;
      if
        check t (function
          | Token.Value | Token.Flow_entry | Token.Flow_mapping_end -> true
          | _ -> false)
      then begin
        t.state <- Flow_mapping_value;
        empty_scalar_event ~anchor:None ~tag:None tok.span
      end
      else begin
        push_state t Flow_mapping_value;
        parse_node t ~block:false ~indentless:false
      end
  | _ ->
      push_state t Flow_mapping_value;
      parse_node t ~block:false ~indentless:false

let parse_flow_mapping_value t ~empty =
  let tok = current_token t in
  if empty then begin
    t.state <- Flow_mapping_key;
    empty_scalar_event ~anchor:None ~tag:None tok.span
  end
  else
    match tok.token with
    | Token.Value ->
        skip_token t;
        if
          check t (function
            | Token.Flow_entry | Token.Flow_mapping_end -> true
            | _ -> false)
        then begin
          t.state <- Flow_mapping_key;
          empty_scalar_event ~anchor:None ~tag:None tok.span
        end
        else begin
          push_state t Flow_mapping_key;
          parse_node t ~block:false ~indentless:false
        end
    | _ ->
        t.state <- Flow_mapping_key;
        empty_scalar_event ~anchor:None ~tag:None tok.span

(** Main state machine dispatcher *)
let rec parse t =
  match t.state with
  | Stream_start -> parse_stream_start t
  | Implicit_document_start -> (
      (* Skip any document end markers before checking what's next *)
      while check t (function Token.Document_end -> true | _ -> false) do
        t.explicit_doc_end <- true;
        (* Seeing ... counts as explicit end *)
        skip_token t
      done;

      let tok = current_token t in
      match tok.token with
      | Token.Stream_end ->
          skip_token t;
          t.state <- End;
          t.finished <- true;
          (Event.Stream_end, tok.span)
      | Token.Version_directive _ | Token.Tag_directive _ ->
          (* Directives are only allowed at stream start or after explicit ... (MUS6/01) *)
          if (not t.stream_start) && not t.explicit_doc_end then
            Error.raise_span tok.span
              (Invalid_directive
                 "directives require explicit document end '...' before them");
          parse_document_start t ~implicit:false
      | Token.Document_start -> parse_document_start t ~implicit:false
      (* These tokens are invalid at document start - they indicate leftover junk *)
      | Token.Flow_sequence_end | Token.Flow_mapping_end | Token.Flow_entry
      | Token.Block_end | Token.Value ->
          Error.raise_span tok.span
            (Unexpected_token "unexpected token at document start")
      | _ -> parse_document_start t ~implicit:true)
  | Document_content ->
      if
        check t (function
          | Token.Version_directive _ | Token.Tag_directive _
          | Token.Document_start | Token.Document_end | Token.Stream_end ->
              true
          | _ -> false)
      then begin
        let tok = current_token t in
        t.state <- pop_state t;
        empty_scalar_event ~anchor:None ~tag:None tok.span
      end
      else begin
        (* Push Document_content_done so we return there after parsing the node.
           This allows us to check for unexpected content after the node. *)
        push_state t Document_content_done;
        parse_node t ~block:true ~indentless:false
      end
  | Document_content_done ->
      (* After parsing a node in document content, check for unexpected content *)
      if
        check t (function
          | Token.Version_directive _ | Token.Tag_directive _
          | Token.Document_start | Token.Document_end | Token.Stream_end ->
              true
          | _ -> false)
      then begin
        (* Valid document boundary - continue to Document_end *)
        t.state <- pop_state t;
        parse t (* Continue to emit the next event *)
      end
      else begin
        (* Unexpected content after document value - this is an error (KS4U, BS4K) *)
        let tok = current_token t in
        Error.raise_span tok.span
          (Unexpected_token "content not allowed after document value")
      end
  | Document_end -> parse_document_end t
  | Block_sequence_first_entry ->
      t.state <- Block_sequence_entry;
      parse_block_sequence_entry t
  | Block_sequence_entry -> parse_block_sequence_entry t
  | Indentless_sequence_entry -> parse_indentless_sequence_entry t
  | Block_mapping_first_key ->
      t.state <- Block_mapping_key;
      parse_block_mapping_key t
  | Block_mapping_key -> parse_block_mapping_key t
  | Block_mapping_value -> parse_block_mapping_value t
  | Flow_sequence_first_entry -> parse_flow_sequence_entry t ~first:true
  | Flow_sequence_entry -> parse_flow_sequence_entry t ~first:false
  | Flow_sequence_entry_mapping_key -> parse_flow_sequence_entry_mapping_key t
  | Flow_sequence_entry_mapping_value ->
      parse_flow_sequence_entry_mapping_value t
  | Flow_sequence_entry_mapping_end -> parse_flow_sequence_entry_mapping_end t
  | Flow_mapping_first_key -> parse_flow_mapping_key t ~first:true
  | Flow_mapping_key -> parse_flow_mapping_key t ~first:false
  | Flow_mapping_value -> parse_flow_mapping_value t ~empty:false
  | End ->
      let span = Span.point Position.initial in
      t.finished <- true;
      (Event.Stream_end, span)

(** Get next event *)
let next t =
  if t.finished then None
  else
    let event, span = parse t in
    Some { Event.event; span }

(** Iterate over all events *)
let iter f t =
  let rec loop () =
    match next t with
    | None -> ()
    | Some ev ->
        f ev;
        loop ()
  in
  loop ()

(** Fold over all events *)
let fold f init t =
  let rec loop acc =
    match next t with None -> acc | Some ev -> loop (f acc ev)
  in
  loop init

(** Convert to list *)
let to_list t = fold (fun acc ev -> ev :: acc) [] t |> List.rev
