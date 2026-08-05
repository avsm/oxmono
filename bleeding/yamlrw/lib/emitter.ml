(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Emitter - converts YAML data structures to string output

    The emitter can write to either a Buffer (default) or directly to a bytesrw
    Bytes.Writer for streaming output. *)

type config = {
  encoding : Encoding.t;
  scalar_style : Scalar_style.t;
  layout_style : Layout_style.t;
  indent : int;
  width : int;
  canonical : bool;
}

let default_config =
  {
    encoding = `Utf8;
    scalar_style = `Any;
    layout_style = `Any;
    indent = 2;
    width = 80;
    canonical = false;
  }

type state =
  | Initial
  | Stream_started
  | Document_started
  | In_block_sequence of int (* indent level *)
  | In_block_mapping_key of int
  | In_block_mapping_value of int
  | In_block_mapping_first_key of
      int (* first key after "- ", no indent needed *)
  | In_flow_sequence
  | In_flow_mapping_key
  | In_flow_mapping_value
  | Document_ended
  | Stream_ended

(** Output sink - either a Buffer or a bytesrw Writer *)
type sink = Buffer_sink of Buffer.t | Writer_sink of Bytesrw.Bytes.Writer.t

type t = {
  config : config;
  sink : sink;
  mutable state : state;
  mutable states : state list;
  mutable indent : int;
  mutable flow_level : int;
  mutable need_separator : bool;
}

let create ?(config = default_config) () =
  {
    config;
    sink = Buffer_sink (Buffer.create 1024);
    state = Initial;
    states = [];
    indent = 0;
    flow_level = 0;
    need_separator = false;
  }

(** Create an emitter that writes directly to a Bytes.Writer *)
let of_writer ?(config = default_config) writer =
  {
    config;
    sink = Writer_sink writer;
    state = Initial;
    states = [];
    indent = 0;
    flow_level = 0;
    need_separator = false;
  }

let contents t =
  match t.sink with
  | Buffer_sink buf -> Buffer.contents buf
  | Writer_sink _ -> "" (* No accumulated content for writer sink *)

let reset t =
  (match t.sink with
  | Buffer_sink buf -> Buffer.clear buf
  | Writer_sink _ -> ());
  t.state <- Initial;
  t.states <- [];
  t.indent <- 0;
  t.flow_level <- 0;
  t.need_separator <- false

(** Output helpers - write to appropriate sink *)

let write t s =
  match t.sink with
  | Buffer_sink buf -> Buffer.add_string buf s
  | Writer_sink w -> Bytesrw.Bytes.Writer.write_string w s

let write_char t c =
  match t.sink with
  | Buffer_sink buf -> Buffer.add_char buf c
  | Writer_sink w ->
      let b = Bytes.make 1 c in
      Bytesrw.Bytes.Writer.write_bytes w b

let write_indent t =
  if t.indent <= 8 then
    for _ = 1 to t.indent do
      write_char t ' '
    done
  else write t (String.make t.indent ' ')

let write_newline t = write_char t '\n'

let push_state t s =
  t.states <- t.state :: t.states;
  t.state <- s

let pop_state t =
  match t.states with
  | s :: rest ->
      t.state <- s;
      t.states <- rest
  | [] -> t.state <- Stream_ended

(** Escape a string for double-quoted output. Uses a buffer to batch writes
    instead of character-by-character. *)
let escape_double_quoted value =
  let len = String.length value in
  (* Check if any escaping is needed *)
  let needs_escape = ref false in
  for i = 0 to len - 1 do
    match value.[i] with
    | '"' | '\\' | '\n' | '\r' | '\t' -> needs_escape := true
    | c when c < ' ' -> needs_escape := true
    | _ -> ()
  done;
  if not !needs_escape then value
  else begin
    let buf = Buffer.create (len + (len / 4)) in
    for i = 0 to len - 1 do
      match value.[i] with
      | '"' -> Buffer.add_string buf "\\\""
      | '\\' -> Buffer.add_string buf "\\\\"
      | '\n' -> Buffer.add_string buf "\\n"
      | '\r' -> Buffer.add_string buf "\\r"
      | '\t' -> Buffer.add_string buf "\\t"
      | c when c < ' ' ->
          Buffer.add_string buf (Printf.sprintf "\\x%02x" (Char.code c))
      | c -> Buffer.add_char buf c
    done;
    Buffer.contents buf
  end

(** Escape a string for single-quoted output. *)
let escape_single_quoted value =
  if not (String.contains value '\'') then value
  else begin
    let len = String.length value in
    let buf = Buffer.create (len + (len / 8)) in
    for i = 0 to len - 1 do
      let c = value.[i] in
      if c = '\'' then Buffer.add_string buf "''" else Buffer.add_char buf c
    done;
    Buffer.contents buf
  end

(** Write indentation for block scalar content.
    Block scalar content must be indented by at least 1 space more than the
    containing structure. We use config.indent spaces, ensuring at least 1. *)
let write_block_scalar_indent t =
  let content_indent = max 1 t.config.indent in
  for _ = 1 to t.indent + content_indent do
    write_char t ' '
  done

(** Write scalar with appropriate quoting.
    Returns true if the scalar ends with a newline (block scalars), false otherwise.
    Callers should check this to avoid double newlines. *)
let write_scalar t ?(style = `Any) value =
  match match style with `Any -> Quoting.choose_style value | s -> s with
  | `Plain | `Any ->
      write t value;
      false
  | `Single_quoted ->
      write_char t '\'';
      write t (escape_single_quoted value);
      write_char t '\'';
      false
  | `Double_quoted ->
      write_char t '"';
      write t (escape_double_quoted value);
      write_char t '"';
      false
  | `Literal ->
      write t "|";
      write_newline t;
      let lines = String.split_on_char '\n' value in
      let rec write_lines = function
        | [] -> ()
        | [ last ] ->
            write_block_scalar_indent t;
            write t last
            (* No trailing newline - caller will add it *)
        | line :: rest ->
            write_block_scalar_indent t;
            write t line;
            write_newline t;
            write_lines rest
      in
      write_lines lines;
      true (* Block scalar ends with content on last line, needs newline from caller *)
  | `Folded ->
      write t ">";
      write_newline t;
      let lines = String.split_on_char '\n' value in
      let rec write_lines = function
        | [] -> ()
        | [ last ] ->
            write_block_scalar_indent t;
            write t last
            (* No trailing newline - caller will add it *)
        | line :: rest ->
            write_block_scalar_indent t;
            write t line;
            write_newline t;
            write_lines rest
      in
      write_lines lines;
      true (* Block scalar ends with content on last line, needs newline from caller *)

(** Write anchor if present *)
let write_anchor t anchor =
  match anchor with
  | Some name ->
      write_char t '&';
      write t name;
      write_char t ' '
  | None -> ()

(** Write tag if present and not implicit *)
let write_tag t ~implicit tag =
  if not implicit then
    match tag with
    | Some tag_str ->
        write_char t '!';
        write t tag_str;
        write_char t ' '
    | None -> ()

(** Emit events *)

let emit t (ev : Event.t) =
  match ev with
  | Event.Stream_start _ -> t.state <- Stream_started
  | Event.Stream_end -> t.state <- Stream_ended
  | Event.Document_start { version; implicit } ->
      if not implicit then begin
        (match version with
        | Some (maj, min) -> write t (Printf.sprintf "%%YAML %d.%d\n" maj min)
        | None -> ());
        write t "---";
        write_newline t
      end;
      t.state <- Document_started
  | Event.Document_end { implicit } ->
      if not implicit then begin
        write t "...";
        write_newline t
      end;
      t.state <- Document_ended
  | Event.Alias { anchor } ->
      if t.flow_level > 0 then begin
        if t.need_separator then write t ", ";
        t.need_separator <- true;
        write_char t '*';
        write t anchor
      end
      else begin
        match t.state with
        | In_block_sequence _ ->
            write_indent t;
            write t "- *";
            write t anchor;
            write_newline t
        | In_block_mapping_key _ ->
            write_indent t;
            write_char t '*';
            write t anchor;
            write t ": ";
            t.state <- In_block_mapping_value t.indent
        | In_block_mapping_value indent ->
            write_char t '*';
            write t anchor;
            write_newline t;
            t.state <- In_block_mapping_key indent
        | _ ->
            write_char t '*';
            write t anchor;
            write_newline t
      end
  | Event.Scalar { anchor; tag; value; plain_implicit; style; _ } ->
      if t.flow_level > 0 then begin
        match t.state with
        | In_flow_mapping_key ->
            if t.need_separator then write t ", ";
            write_anchor t anchor;
            write_tag t ~implicit:plain_implicit tag;
            let (_ : bool) = write_scalar t ~style value in
            write t ": ";
            t.need_separator <- false;
            t.state <- In_flow_mapping_value
        | In_flow_mapping_value ->
            if t.need_separator then begin
              (* We just finished a nested structure (array/mapping),
                  so this scalar is the next key, not a value *)
              write t ", ";
              write_anchor t anchor;
              write_tag t ~implicit:plain_implicit tag;
              let (_ : bool) = write_scalar t ~style value in
              write t ": ";
              t.need_separator <- false;
              t.state <- In_flow_mapping_value
            end
            else begin
              (* Normal value scalar *)
              write_anchor t anchor;
              write_tag t ~implicit:plain_implicit tag;
              let (_ : bool) = write_scalar t ~style value in
              t.need_separator <- true;
              t.state <- In_flow_mapping_key
            end
        | _ ->
            if t.need_separator then write t ", ";
            t.need_separator <- true;
            write_anchor t anchor;
            write_tag t ~implicit:plain_implicit tag;
            let (_ : bool) = write_scalar t ~style value in
            ()
      end
      else begin
        match t.state with
        | In_block_sequence _ ->
            write_indent t;
            write t "- ";
            write_anchor t anchor;
            write_tag t ~implicit:plain_implicit tag;
            let (_ : bool) = write_scalar t ~style value in
            write_newline t
        | In_block_mapping_key indent ->
            write_indent t;
            write_anchor t anchor;
            write_tag t ~implicit:plain_implicit tag;
            let (_ : bool) = write_scalar t ~style value in
            write_char t ':';
            t.state <- In_block_mapping_value indent
        | In_block_mapping_first_key indent ->
            (* First key after "- ", no indent needed *)
            write_anchor t anchor;
            write_tag t ~implicit:plain_implicit tag;
            let (_ : bool) = write_scalar t ~style value in
            write_char t ':';
            t.state <- In_block_mapping_value indent
        | In_block_mapping_value indent ->
            write_char t ' ';
            write_anchor t anchor;
            write_tag t ~implicit:plain_implicit tag;
            let (_ : bool) = write_scalar t ~style value in
            write_newline t;
            t.state <- In_block_mapping_key indent
        | _ ->
            write_anchor t anchor;
            write_tag t ~implicit:plain_implicit tag;
            let (_ : bool) = write_scalar t ~style value in
            write_newline t
      end
  | Event.Sequence_start { anchor; tag; implicit; style } ->
      let use_flow = style = `Flow || t.flow_level > 0 in
      if t.flow_level > 0 then begin
        match t.state with
        | In_flow_mapping_key ->
            if t.need_separator then write t ", ";
            write_anchor t anchor;
            write_tag t ~implicit tag;
            write_char t '[';
            t.flow_level <- t.flow_level + 1;
            t.need_separator <- false;
            push_state t In_flow_mapping_value;
            (* After ] we'll be in value position but sequence handles it *)
            t.state <- In_flow_sequence
        | In_flow_mapping_value ->
            write_anchor t anchor;
            write_tag t ~implicit tag;
            write_char t '[';
            t.flow_level <- t.flow_level + 1;
            t.need_separator <- false;
            push_state t In_flow_mapping_key;
            t.state <- In_flow_sequence
        | _ ->
            if t.need_separator then write t ", ";
            write_anchor t anchor;
            write_tag t ~implicit tag;
            write_char t '[';
            t.flow_level <- t.flow_level + 1;
            t.need_separator <- false;
            push_state t In_flow_sequence
      end
      else begin
        match t.state with
        | In_block_sequence _ ->
            write_indent t;
            write t "- ";
            write_anchor t anchor;
            write_tag t ~implicit tag;
            if use_flow then begin
              write_char t '[';
              t.flow_level <- t.flow_level + 1;
              t.need_separator <- false;
              push_state t In_flow_sequence
            end
            else begin
              write_newline t;
              push_state t (In_block_sequence t.indent);
              t.indent <- t.indent + t.config.indent
            end
        | In_block_mapping_key indent ->
            write_indent t;
            write_anchor t anchor;
            write_tag t ~implicit tag;
            write t ":";
            write_newline t;
            push_state t (In_block_mapping_key indent);
            t.indent <- t.indent + t.config.indent;
            t.state <- In_block_sequence t.indent
        | In_block_mapping_first_key indent ->
            (* First key after "- " with sequence value - no indent *)
            write_anchor t anchor;
            write_tag t ~implicit tag;
            write t ":";
            write_newline t;
            push_state t (In_block_mapping_key indent);
            t.indent <- t.indent + t.config.indent;
            t.state <- In_block_sequence t.indent
        | In_block_mapping_value indent ->
            write_anchor t anchor;
            write_tag t ~implicit tag;
            if use_flow then begin
              write_char t ' ';
              write_char t '[';
              t.flow_level <- t.flow_level + 1;
              t.need_separator <- false;
              (* Save key state to return to after flow sequence *)
              t.state <- In_block_mapping_key indent;
              push_state t In_flow_sequence
            end
            else begin
              write_newline t;
              (* Save key state to return to after nested sequence *)
              t.state <- In_block_mapping_key indent;
              push_state t (In_block_sequence (t.indent + t.config.indent));
              t.indent <- t.indent + t.config.indent
            end
        | _ ->
            write_anchor t anchor;
            write_tag t ~implicit tag;
            if use_flow then begin
              write_char t '[';
              t.flow_level <- t.flow_level + 1;
              t.need_separator <- false;
              push_state t In_flow_sequence
            end
            else begin
              push_state t (In_block_sequence t.indent);
              t.state <- In_block_sequence t.indent
            end
      end
  | Event.Sequence_end ->
      if t.flow_level > 0 then begin
        write_char t ']';
        t.flow_level <- t.flow_level - 1;
        t.need_separator <- true;
        pop_state t;
        (* Write newline if returning to block context *)
        match t.state with
        | In_block_mapping_key _ | In_block_sequence _ -> write_newline t
        | _ -> ()
      end
      else begin
        t.indent <- t.indent - t.config.indent;
        pop_state t
      end
  | Event.Mapping_start { anchor; tag; implicit; style } ->
      let use_flow = style = `Flow || t.flow_level > 0 in
      if t.flow_level > 0 then begin
        match t.state with
        | In_flow_mapping_key ->
            if t.need_separator then write t ", ";
            write_anchor t anchor;
            write_tag t ~implicit tag;
            write_char t '{';
            t.flow_level <- t.flow_level + 1;
            t.need_separator <- false;
            push_state t In_flow_mapping_value;
            t.state <- In_flow_mapping_key
        | In_flow_mapping_value ->
            write_anchor t anchor;
            write_tag t ~implicit tag;
            write_char t '{';
            t.flow_level <- t.flow_level + 1;
            t.need_separator <- false;
            push_state t In_flow_mapping_key;
            t.state <- In_flow_mapping_key
        | _ ->
            if t.need_separator then write t ", ";
            write_anchor t anchor;
            write_tag t ~implicit tag;
            write_char t '{';
            t.flow_level <- t.flow_level + 1;
            t.need_separator <- false;
            push_state t In_flow_mapping_key
      end
      else begin
        match t.state with
        | In_block_sequence _ ->
            write_indent t;
            write t "- ";
            write_anchor t anchor;
            write_tag t ~implicit tag;
            if use_flow then begin
              write_char t '{';
              t.flow_level <- t.flow_level + 1;
              t.need_separator <- false;
              push_state t In_flow_mapping_key
            end
            else begin
              (* Don't write newline - first key goes on same line as "- " *)
              push_state t (In_block_sequence t.indent);
              t.indent <- t.indent + t.config.indent;
              t.state <- In_block_mapping_first_key t.indent
            end
        | In_block_mapping_key indent ->
            write_indent t;
            write_anchor t anchor;
            write_tag t ~implicit tag;
            write t ":";
            write_newline t;
            push_state t (In_block_mapping_key indent);
            t.indent <- t.indent + t.config.indent;
            t.state <- In_block_mapping_key t.indent
        | In_block_mapping_first_key indent ->
            (* First key after "- " with mapping value - no indent *)
            write_anchor t anchor;
            write_tag t ~implicit tag;
            write t ":";
            write_newline t;
            push_state t (In_block_mapping_key indent);
            t.indent <- t.indent + t.config.indent;
            t.state <- In_block_mapping_key t.indent
        | In_block_mapping_value indent ->
            write_anchor t anchor;
            write_tag t ~implicit tag;
            if use_flow then begin
              write_char t ' ';
              write_char t '{';
              t.flow_level <- t.flow_level + 1;
              t.need_separator <- false;
              (* Save key state to return to after flow mapping *)
              t.state <- In_block_mapping_key indent;
              push_state t In_flow_mapping_key
            end
            else begin
              write_newline t;
              (* Save key state to return to after nested mapping *)
              t.state <- In_block_mapping_key indent;
              push_state t (In_block_mapping_key (t.indent + t.config.indent));
              t.indent <- t.indent + t.config.indent
            end
        | _ ->
            write_anchor t anchor;
            write_tag t ~implicit tag;
            if use_flow then begin
              write_char t '{';
              t.flow_level <- t.flow_level + 1;
              t.need_separator <- false;
              push_state t In_flow_mapping_key
            end
            else begin
              push_state t (In_block_mapping_key t.indent);
              t.state <- In_block_mapping_key t.indent
            end
      end
  | Event.Mapping_end ->
      if t.flow_level > 0 then begin
        write_char t '}';
        t.flow_level <- t.flow_level - 1;
        t.need_separator <- true;
        pop_state t;
        (* Write newline if returning to block context *)
        match t.state with
        | In_block_mapping_key _ | In_block_sequence _ -> write_newline t
        | _ -> ()
      end
      else begin
        t.indent <- t.indent - t.config.indent;
        pop_state t
      end

(** Access to the underlying buffer for advanced use. Returns None if emitter is
    writing to a Writer instead of Buffer. *)
let buffer t =
  match t.sink with Buffer_sink buf -> Some buf | Writer_sink _ -> None

(** Get config *)
let config t = t.config

(** Check if emitter is writing to a Writer *)
let is_streaming t =
  match t.sink with Writer_sink _ -> true | Buffer_sink _ -> false

(** Flush the writer sink (no-op for buffer sink) *)
let flush t =
  match t.sink with
  | Writer_sink w -> Bytesrw.Bytes.Writer.write_eod w
  | Buffer_sink _ -> ()
