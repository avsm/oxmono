(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** YAML tokenizer/scanner with lookahead for ambiguity resolution *)

type simple_key = {
  sk_possible : bool;
  sk_required : bool;
  sk_token_number : int;
  sk_position : Position.t;
}
(** Simple key tracking for mapping key disambiguation *)

type indent = { indent : int; needs_block_end : bool }
(** Indent level tracking *)

type t = {
  input : Input.t;
  tokens : Token.spanned Queue.t;
  mutable token_number : int;
  mutable tokens_taken : int;
  mutable stream_started : bool;
  mutable stream_ended : bool;
  mutable indent_stack : indent list;  (** Stack of indentation levels *)
  mutable flow_level : int;  (** Nesting depth in \[\] or \{\} *)
  mutable flow_indent : int;
      (** Column where outermost flow collection started *)
  mutable simple_keys : simple_key option list;
      (** Per flow-level simple key tracking *)
  mutable allow_simple_key : bool;
  mutable leading_whitespace : bool;
      (** True when at start of line (only whitespace seen) *)
  mutable document_has_content : bool;
      (** True if we've emitted content tokens in current document *)
  mutable adjacent_value_allowed_at : Position.t option;
      (** Position where adjacent : is allowed *)
  mutable flow_mapping_stack : bool list;
      (** Stack of whether each flow level is a mapping *)
}

let create input =
  {
    input;
    tokens = Queue.create ();
    token_number = 0;
    tokens_taken = 0;
    stream_started = false;
    stream_ended = false;
    indent_stack = [];
    flow_level = 0;
    flow_indent = 0;
    simple_keys = [ None ];
    (* One entry for the base level *)
    allow_simple_key = true;
    leading_whitespace = true;
    (* Start at beginning of stream *)
    document_has_content = false;
    adjacent_value_allowed_at = None;
    flow_mapping_stack = [];
  }

let of_string s = create (Input.of_string s)
let of_input = create
let of_reader r = create (Input.of_reader r)
let position t = Input.position t.input

(** Add a token to the queue *)
let emit t span token =
  Queue.add { Token.token; span } t.tokens;
  t.token_number <- t.token_number + 1

(** Get current column (1-indexed) *)
let column t = (Input.position t.input).column

(** Get current indent level *)
let current_indent t =
  match t.indent_stack with [] -> -1 | { indent; _ } :: _ -> indent

(** Skip whitespace to end of line, checking for valid comments. Returns true if
    any whitespace (including tabs) was found before a comment. *)
let skip_whitespace_and_comment t =
  let has_whitespace = ref false in
  (* Skip blanks (spaces and tabs) *)
  while Input.next_is_blank t.input do
    has_whitespace := true;
    ignore (Input.next t.input)
  done;
  (* Check for comment *)
  if Input.next_is (( = ) '#') t.input then begin
    (* Validate: comment must be preceded by whitespace or be at start of line *)
    if not !has_whitespace then begin
      (* Check if we're at the start of input or after whitespace (blank or line break) *)
      match Input.peek_back t.input with
      | None -> () (* Start of input - OK *)
      | Some c when Input.is_whitespace c -> () (* After whitespace - OK *)
      | _ ->
          (* Comment not preceded by whitespace - ERROR *)
          Error.raise_at (Input.mark t.input) Invalid_comment
    end;
    (* Skip to end of line *)
    while (not (Input.is_eof t.input)) && not (Input.next_is_break t.input) do
      ignore (Input.next t.input)
    done
  end

(** Skip blanks (spaces/tabs) and return (found_tabs, found_spaces) *)
let skip_blanks_check_tabs t =
  let found_tab = ref false in
  let found_space = ref false in
  while Input.next_is_blank t.input do
    (match Input.peek t.input with
    | Some '\t' -> found_tab := true
    | Some ' ' -> found_space := true
    | _ -> ());
    ignore (Input.next t.input)
  done;
  (!found_tab, !found_space)

(** Skip whitespace and comments, return true if at newline *)
let rec skip_to_next_token t =
  (* Check for tabs used as indentation in block context *)
  (match Input.peek t.input with
  | Some '\t'
    when t.flow_level = 0 && t.leading_whitespace
         && column t - 1 < current_indent t ->
      (* Tab found in indentation zone - this is invalid *)
      (* Skip to end of line to check if line has content *)
      let start_pos = Input.mark t.input in
      while Input.next_is_blank t.input do
        ignore (Input.next t.input)
      done;
      (* If we have content on this line with a tab, raise error *)
      if (not (Input.next_is_break t.input)) && not (Input.is_eof t.input) then
        Error.raise_at start_pos Tab_in_indentation
  | _ -> ());

  (* Skip blanks and validate comments *)
  skip_whitespace_and_comment t;
  (* Skip line break in block context *)
  if t.flow_level = 0 && Input.next_is_break t.input then begin
    Input.consume_break t.input;
    t.allow_simple_key <- true;
    t.leading_whitespace <- true;
    skip_to_next_token t
  end
  else if t.flow_level > 0 && Input.next_is_whitespace t.input then begin
    (* In flow context, skip all whitespace including line breaks *)
    if Input.next_is_break t.input then begin
      Input.consume_break t.input;
      (* Allow simple keys after line breaks in flow context *)
      t.allow_simple_key <- true;
      (* After line break in flow, check for tabs at start of line (Y79Y/03)
         Tabs are not allowed as indentation - if tab is first char and results
         in a column less than flow_indent, it's an error *)
      if Input.next_is (( = ) '\t') t.input then begin
        (* Tab at start of line in flow context - skip tabs and check position *)
        let start_mark = Input.mark t.input in
        while Input.next_is (( = ) '\t') t.input do
          ignore (Input.next t.input)
        done;
        (* If only tabs were used (no spaces) and column < flow_indent, error *)
        if
          (not (Input.next_is_break t.input))
          && (not (Input.is_eof t.input))
          && column t < t.flow_indent
        then Error.raise_at start_mark Invalid_flow_indentation
      end;
      skip_to_next_token t
    end
    else begin
      ignore (Input.next t.input);
      skip_to_next_token t
    end
  end

(** Roll the indentation level *)
let roll_indent t col =
  if t.flow_level = 0 && col > current_indent t then begin
    t.indent_stack <- { indent = col; needs_block_end = true } :: t.indent_stack;
    true
  end
  else false

(** Unroll indentation to given column *)
let unroll_indent t col =
  while
    t.flow_level = 0
    &&
    match t.indent_stack with
    | { indent; needs_block_end = true; _ } :: _ when indent > col -> true
    | _ -> false
  do
    match t.indent_stack with
    | { indent = _; needs_block_end = true; _ } :: rest ->
        let pos = Input.position t.input in
        let span = Span.point pos in
        emit t span Token.Block_end;
        t.indent_stack <- rest
    | _ -> ()
  done

(** Save a potential simple key *)
let save_simple_key t =
  if t.allow_simple_key then begin
    (* A simple key is required only if we're in a block context,
       at the current indentation level, AND the current indent needs a block end.
       This matches saphyr's logic and prevents false positives for values. *)
    let required =
      t.flow_level = 0
      &&
      match t.indent_stack with
      | { indent; needs_block_end = true; _ } :: _ -> indent = column t
      | _ -> false
    in
    let sk =
      {
        sk_possible = true;
        sk_required = required;
        sk_token_number = t.token_number;
        sk_position = Input.position t.input;
      }
    in
    (* Remove any existing simple key at current level *)
    t.simple_keys <-
      (match t.simple_keys with
      | _ :: rest -> Some sk :: rest
      | [] -> [ Some sk ])
  end

(** Remove simple key at current level *)
let remove_simple_key t =
  match t.simple_keys with
  | Some sk :: _rest when sk.sk_required ->
      Error.raise_at sk.sk_position Expected_key
  | _ :: rest -> t.simple_keys <- None :: rest
  | [] -> ()

(** Stale simple keys that span too many tokens *)
let stale_simple_keys t =
  t.simple_keys <-
    List.map
      (fun sk_opt ->
        match sk_opt with
        | Some sk
          when sk.sk_possible
               && (Input.position t.input).line > sk.sk_position.line
               && t.flow_level = 0 ->
            if sk.sk_required then Error.raise_at sk.sk_position Expected_key;
            None
        | _ -> sk_opt)
      t.simple_keys

(** Read anchor or alias name *)
let scan_anchor_alias t =
  let start = Input.mark t.input in
  let buf = Buffer.create 16 in
  (* Per YAML 1.2 spec: anchor names can contain any character that is NOT:
     - Whitespace (space, tab, line breaks)
     - Flow indicators: []{}
     - Comma (,)
     This matches the saphyr implementation: is_yaml_non_space && !is_flow *)
  while
    match Input.peek t.input with
    | Some c
      when (not (Input.is_whitespace c))
           && (not (Input.is_flow_indicator c))
           && c <> '\x00' ->
        Buffer.add_char buf c;
        ignore (Input.next t.input);
        true
    | _ -> false
  do
    ()
  done;
  let name = Buffer.contents buf in
  if String.length name = 0 then
    Error.raise_at start (Invalid_anchor "empty anchor name");
  (name, Span.make ~start ~stop:(Input.mark t.input))

(** Scan tag handle *)
let scan_tag_handle t =
  let start = Input.mark t.input in
  let buf = Buffer.create 16 in
  (* Expect ! *)
  (match Input.peek t.input with
  | Some '!' ->
      Buffer.add_char buf '!';
      ignore (Input.next t.input)
  | _ -> Error.raise_at start (Invalid_tag "expected '!'"));
  (* Read word chars *)
  while
    match Input.peek t.input with
    | Some c when Input.is_alnum c || c = '-' ->
        Buffer.add_char buf c;
        ignore (Input.next t.input);
        true
    | _ -> false
  do
    ()
  done;
  (* Check for secondary ! *)
  (match Input.peek t.input with
  | Some '!' ->
      Buffer.add_char buf '!';
      ignore (Input.next t.input)
  | _ -> ());
  Buffer.contents buf

(** Scan tag suffix (after handle) *)
let scan_tag_suffix t =
  let is_hex_digit c =
    (c >= '0' && c <= '9') || (c >= 'A' && c <= 'F') || (c >= 'a' && c <= 'f')
  in
  let hex_val c =
    match c with
    | '0' .. '9' -> Char.code c - Char.code '0'
    | 'A' .. 'F' -> Char.code c - Char.code 'A' + 10
    | 'a' .. 'f' -> Char.code c - Char.code 'a' + 10
    | _ -> 0
  in
  let buf = Buffer.create 32 in
  while
    match Input.peek t.input with
    | Some '%' -> (
        (* Percent-encoded character *)
        ignore (Input.next t.input);
        match (Input.peek t.input, Input.peek_nth t.input 1) with
        | Some c1, Some c2 when is_hex_digit c1 && is_hex_digit c2 ->
            ignore (Input.next t.input);
            ignore (Input.next t.input);
            let code = (hex_val c1 * 16) + hex_val c2 in
            Buffer.add_char buf (Char.chr code);
            true
        | _ ->
            (* Invalid percent encoding - keep the % *)
            Buffer.add_char buf '%';
            true)
    | Some c
      when (not (Input.is_whitespace c)) && not (Input.is_flow_indicator c) ->
        Buffer.add_char buf c;
        ignore (Input.next t.input);
        true
    | _ -> false
  do
    ()
  done;
  Buffer.contents buf

(** Scan a tag *)
let scan_tag t =
  let start = Input.mark t.input in
  ignore (Input.next t.input);
  (* consume ! *)
  let handle, suffix =
    match Input.peek t.input with
    | Some '<' ->
        (* Verbatim tag: !<...> - handle is empty, suffix is full URI *)
        ignore (Input.next t.input);
        let buf = Buffer.create 32 in
        while
          match Input.peek t.input with
          | Some '>' -> false
          | Some c ->
              Buffer.add_char buf c;
              ignore (Input.next t.input);
              true
          | None ->
              Error.raise_at (Input.mark t.input)
                (Invalid_tag "unclosed verbatim tag")
        do
          ()
        done;
        ignore (Input.next t.input);
        (* consume > *)
        ("", Buffer.contents buf)
    | Some c when Input.is_whitespace c || Input.is_flow_indicator c ->
        (* Non-specific tag: ! *)
        ("!", "")
    | Some '!' ->
        (* Secondary handle: !! *)
        ignore (Input.next t.input);
        (* consume second ! *)
        let suffix = scan_tag_suffix t in
        ("!!", suffix)
    | _ -> (
        (* Primary handle or just suffix: !foo or !e!foo *)
        (* Read alphanumeric characters *)
        let buf = Buffer.create 16 in
        while
          match Input.peek t.input with
          | Some c when Input.is_alnum c || c = '-' ->
              Buffer.add_char buf c;
              ignore (Input.next t.input);
              true
          | _ -> false
        do
          ()
        done;
        (* Check if next character is ! - if so, this is a named handle *)
        match Input.peek t.input with
        | Some '!' ->
            (* Named handle like !e! *)
            ignore (Input.next t.input);
            let handle_name = Buffer.contents buf in
            let suffix = scan_tag_suffix t in
            ("!" ^ handle_name ^ "!", suffix)
        | _ ->
            (* Just ! followed by suffix *)
            ("!", Buffer.contents buf ^ scan_tag_suffix t))
  in
  (* Validate that tag is followed by whitespace, break, or (in flow) flow indicator *)
  (match Input.peek t.input with
  | None -> () (* EOF is ok *)
  | Some c when Input.is_whitespace c || Input.is_break c -> ()
  | Some c when t.flow_level > 0 && Input.is_flow_indicator c -> ()
  | _ ->
      Error.raise_at start
        (Invalid_tag "expected whitespace or line break after tag"));
  let span = Span.make ~start ~stop:(Input.mark t.input) in
  (handle, suffix, span)

(** Scan single-quoted scalar *)
let scan_single_quoted t =
  let start = Input.mark t.input in
  ignore (Input.next t.input);
  (* consume opening single-quote *)
  let buf = Buffer.create 64 in
  let whitespace = Buffer.create 16 in
  (* Track trailing whitespace *)

  let flush_whitespace () =
    if Buffer.length whitespace > 0 then begin
      Buffer.add_buffer buf whitespace;
      Buffer.clear whitespace
    end
  in

  let rec loop () =
    match Input.peek t.input with
    | None -> Error.raise_at start Unclosed_single_quote
    | Some '\'' -> (
        ignore (Input.next t.input);
        (* Check for escaped quote ('') *)
        match Input.peek t.input with
        | Some '\'' ->
            flush_whitespace ();
            Buffer.add_char buf '\'';
            ignore (Input.next t.input);
            loop ()
        | _ ->
            (* End of string - flush any trailing whitespace *)
            flush_whitespace ())
    | Some ' ' | Some '\t' ->
        (* Track whitespace - don't add to buf yet *)
        Buffer.add_char whitespace (Option.get (Input.peek t.input));
        ignore (Input.next t.input);
        loop ()
    | Some '\n' | Some '\r' ->
        (* Discard trailing whitespace before line break *)
        Buffer.clear whitespace;
        Input.consume_break t.input;
        (* Skip leading whitespace on next line *)
        while Input.next_is_blank t.input do
          ignore (Input.next t.input)
        done;
        (* Check for document boundary *)
        if Input.at_document_boundary t.input then
          Error.raise_at start Unclosed_single_quote;
        (* Check indentation: continuation must be > block indent (QB6E, DK95) *)
        let col = column t in
        let indent = current_indent t in
        if
          (not (Input.is_eof t.input))
          && (not (Input.next_is_break t.input))
          && col <= indent && indent >= 0
        then
          Error.raise_at (Input.mark t.input)
            (Invalid_quoted_scalar_indentation
               "invalid indentation in quoted scalar");
        (* Count empty lines (consecutive line breaks) *)
        let empty_lines = ref 0 in
        while Input.next_is_break t.input do
          incr empty_lines;
          Input.consume_break t.input;
          while Input.next_is_blank t.input do
            ignore (Input.next t.input)
          done;
          if Input.at_document_boundary t.input then
            Error.raise_at start Unclosed_single_quote;
          (* Check indentation after each empty line too *)
          let col = column t in
          let indent = current_indent t in
          if
            (not (Input.is_eof t.input))
            && (not (Input.next_is_break t.input))
            && col <= indent && indent >= 0
          then
            Error.raise_at (Input.mark t.input)
              (Invalid_quoted_scalar_indentation
                 "invalid indentation in quoted scalar")
        done;
        (* Apply folding rules *)
        if !empty_lines > 0 then begin
          (* Empty lines: preserve as newlines *)
          for _ = 1 to !empty_lines do
            Buffer.add_char buf '\n'
          done
        end
        else
          (* Single break: fold to space (even at start of string) *)
          Buffer.add_char buf ' ';
        loop ()
    | Some c ->
        flush_whitespace ();
        Buffer.add_char buf c;
        ignore (Input.next t.input);
        loop ()
  in
  loop ();
  let span = Span.make ~start ~stop:(Input.mark t.input) in
  (Buffer.contents buf, span)

(** Decode hex escape of given length *)
let decode_hex t len =
  let start = Input.mark t.input in
  let buf = Buffer.create len in
  for _ = 1 to len do
    match Input.peek t.input with
    | Some c when Input.is_hex c ->
        Buffer.add_char buf c;
        ignore (Input.next t.input)
    | _ -> Error.raise_at start (Invalid_hex_escape (Buffer.contents buf))
  done;
  let code = int_of_string ("0x" ^ Buffer.contents buf) in
  (* Validate Unicode scalar value (0x0000-0x10FFFF, excluding surrogates) *)
  if not (Uchar.is_valid code) then
    Error.raise_at start (Invalid_unicode_escape (Buffer.contents buf));
  if code <= 0x7F then String.make 1 (Char.chr code)
  else if code <= 0x7FF then
    let b1 = 0xC0 lor (code lsr 6) in
    let b2 = 0x80 lor (code land 0x3F) in
    String.init 2 (fun i -> Char.chr (if i = 0 then b1 else b2))
  else if code <= 0xFFFF then
    let b1 = 0xE0 lor (code lsr 12) in
    let b2 = 0x80 lor ((code lsr 6) land 0x3F) in
    let b3 = 0x80 lor (code land 0x3F) in
    String.init 3 (fun i ->
        Char.chr (match i with 0 -> b1 | 1 -> b2 | _ -> b3))
  else
    let b1 = 0xF0 lor (code lsr 18) in
    let b2 = 0x80 lor ((code lsr 12) land 0x3F) in
    let b3 = 0x80 lor ((code lsr 6) land 0x3F) in
    let b4 = 0x80 lor (code land 0x3F) in
    String.init 4 (fun i ->
        Char.chr (match i with 0 -> b1 | 1 -> b2 | 2 -> b3 | _ -> b4))

(** Scan double-quoted scalar *)
let scan_double_quoted t =
  let start = Input.mark t.input in
  ignore (Input.next t.input);
  (* consume opening double-quote *)
  let buf = Buffer.create 64 in
  let whitespace = Buffer.create 16 in
  (* Track pending whitespace *)

  let flush_whitespace () =
    if Buffer.length whitespace > 0 then begin
      Buffer.add_buffer buf whitespace;
      Buffer.clear whitespace
    end
  in

  let rec loop () =
    match Input.peek t.input with
    | None -> Error.raise_at start Unclosed_double_quote
    | Some '"' ->
        (* Flush trailing whitespace before closing quote to preserve it *)
        flush_whitespace ();
        ignore (Input.next t.input)
    | (Some ' ' | Some '\t') as c_opt ->
        (* Track whitespace - don't add to buf yet *)
        let c = match c_opt with Some c -> c | None -> assert false in
        Buffer.add_char whitespace c;
        ignore (Input.next t.input);
        loop ()
    | Some '\\' ->
        (* Escape sequence - this is non-whitespace content *)
        flush_whitespace ();
        (* Commit any pending whitespace *)
        ignore (Input.next t.input);
        (match Input.peek t.input with
        | None -> Error.raise_at start (Invalid_escape_sequence "\\<EOF>")
        | Some '0' ->
            Buffer.add_char buf '\x00';
            ignore (Input.next t.input)
        | Some 'a' ->
            Buffer.add_char buf '\x07';
            ignore (Input.next t.input)
        | Some 'b' ->
            Buffer.add_char buf '\x08';
            ignore (Input.next t.input)
        | Some 't' | Some '\t' ->
            Buffer.add_char buf '\t';
            ignore (Input.next t.input)
        | Some 'n' ->
            Buffer.add_char buf '\n';
            ignore (Input.next t.input)
        | Some 'v' ->
            Buffer.add_char buf '\x0B';
            ignore (Input.next t.input)
        | Some 'f' ->
            Buffer.add_char buf '\x0C';
            ignore (Input.next t.input)
        | Some 'r' ->
            Buffer.add_char buf '\r';
            ignore (Input.next t.input)
        | Some 'e' ->
            Buffer.add_char buf '\x1B';
            ignore (Input.next t.input)
        | Some ' ' ->
            Buffer.add_char buf ' ';
            ignore (Input.next t.input)
        | Some '"' ->
            Buffer.add_char buf '"';
            ignore (Input.next t.input)
        | Some '/' ->
            Buffer.add_char buf '/';
            ignore (Input.next t.input)
        | Some '\\' ->
            Buffer.add_char buf '\\';
            ignore (Input.next t.input)
        | Some 'N' ->
            Buffer.add_string buf "\xC2\x85";
            ignore (Input.next t.input) (* NEL *)
        | Some '_' ->
            Buffer.add_string buf "\xC2\xA0";
            ignore (Input.next t.input) (* NBSP *)
        | Some 'L' ->
            Buffer.add_string buf "\xE2\x80\xA8";
            ignore (Input.next t.input) (* LS *)
        | Some 'P' ->
            Buffer.add_string buf "\xE2\x80\xA9";
            ignore (Input.next t.input) (* PS *)
        | Some 'x' ->
            ignore (Input.next t.input);
            Buffer.add_string buf (decode_hex t 2)
        | Some 'u' ->
            ignore (Input.next t.input);
            Buffer.add_string buf (decode_hex t 4)
        | Some 'U' ->
            ignore (Input.next t.input);
            Buffer.add_string buf (decode_hex t 8)
        | Some '\n' | Some '\r' ->
            (* Line continuation escape *)
            Input.consume_break t.input;
            while Input.next_is_blank t.input do
              ignore (Input.next t.input)
            done
        | Some c ->
            Error.raise_at (Input.mark t.input)
              (Invalid_escape_sequence (Printf.sprintf "\\%c" c)));
        loop ()
    | Some '\n' | Some '\r' ->
        (* Line break: discard any pending trailing whitespace *)
        Buffer.clear whitespace;
        Input.consume_break t.input;
        (* Count consecutive line breaks (empty lines) *)
        let empty_lines = ref 0 in
        let continue = ref true in
        let started_with_tab = ref false in
        while !continue do
          (* Track if we start with a tab (for DK95/01 check) *)
          if Input.next_is (( = ) '\t') t.input then started_with_tab := true;
          (* Skip blanks (spaces/tabs) on the line *)
          while Input.next_is_blank t.input do
            ignore (Input.next t.input)
          done;
          (* Check if we hit another line break (empty line) *)
          if Input.next_is_break t.input then begin
            Input.consume_break t.input;
            incr empty_lines;
            started_with_tab := false (* Reset for next line *)
          end
          else continue := false
        done;
        (* Check for document boundary - this terminates the quoted string *)
        if Input.at_document_boundary t.input then
          Error.raise_at start Unclosed_double_quote;
        (* Check indentation: continuation must be > block indent (QB6E, DK95)
           Note: must be strictly greater than block indent, not just equal *)
        let col = column t in
        let indent = current_indent t in
        let start_col = start.column in
        (* DK95/01: if continuation started with tabs and column < start column, error *)
        if (not (Input.is_eof t.input)) && !started_with_tab && col < start_col
        then
          Error.raise_at (Input.mark t.input)
            (Invalid_quoted_scalar_indentation
               "invalid indentation in quoted scalar");
        if (not (Input.is_eof t.input)) && col <= indent && indent >= 0 then
          Error.raise_at (Input.mark t.input)
            (Invalid_quoted_scalar_indentation
               "invalid indentation in quoted scalar");
        (* Per YAML spec: single break = space, break + empty lines = newlines *)
        if !empty_lines > 0 then begin
          (* Empty lines: output N newlines where N = number of empty lines *)
          for _ = 1 to !empty_lines do
            Buffer.add_char buf '\n'
          done
        end
        else
          (* Single break folds to space *)
          Buffer.add_char buf ' ';
        loop ()
    | Some c ->
        (* Non-whitespace character *)
        flush_whitespace ();
        (* Commit any pending whitespace *)
        Buffer.add_char buf c;
        ignore (Input.next t.input);
        loop ()
  in
  loop ();
  let span = Span.make ~start ~stop:(Input.mark t.input) in
  (Buffer.contents buf, span)

(** Check if character can appear in plain scalar at this position *)
let can_continue_plain t c ~in_flow =
  match c with
  | ':' -> (
      (* : is OK if not followed by whitespace or flow indicator *)
      match Input.peek_nth t.input 1 with
      | None -> true
      | Some c2 when Input.is_whitespace c2 -> false
      | Some c2 when in_flow && Input.is_flow_indicator c2 -> false
      | _ -> true)
  | '#' -> (
      (* # is a comment indicator only if preceded by whitespace *)
      (* Check the previous character to determine if this is a comment *)
      match Input.peek_back t.input with
      | None -> true (* At start - can't be comment indicator, allow it *)
      | Some c when Input.is_whitespace c ->
          false (* Preceded by whitespace - comment *)
      | Some c when Input.is_break c -> false (* At start of line - comment *)
      | _ -> true (* Not preceded by whitespace - part of scalar *))
  | c when in_flow && Input.is_flow_indicator c -> false
  | _ when Input.is_break c -> false
  | _ -> true

(** Scan plain scalar *)
let scan_plain_scalar t =
  let start = Input.mark t.input in
  let in_flow = t.flow_level > 0 in
  let indent = current_indent t in
  (* In flow context, scalars must be indented more than the current block indent.
     This ensures that content at block indent or less ends the flow context. *)
  if in_flow && column t - 1 < indent then
    Error.raise_at start Invalid_flow_indentation;
  let buf = Buffer.create 64 in
  let spaces = Buffer.create 16 in
  let whitespace = Buffer.create 16 in
  (* Track whitespace within a line *)
  let leading_blanks = ref false in

  let rec scan_line () =
    match Input.peek t.input with
    | None -> ()
    | Some c when Input.is_blank c && can_continue_plain t c ~in_flow ->
        (* Blank character within a line - save to whitespace buffer *)
        Buffer.add_char whitespace c;
        ignore (Input.next t.input);
        scan_line ()
    | Some c when can_continue_plain t c ~in_flow ->
        (* Non-blank character - process any pending breaks/whitespace first *)
        begin
          if Buffer.length spaces > 0 then begin
            if !leading_blanks then begin
              (* Fold line break *)
              if Buffer.contents spaces = "\n" then Buffer.add_char buf ' '
              else begin
                (* Multiple breaks - preserve all but first *)
                let s = Buffer.contents spaces in
                Buffer.add_substring buf s 1 (String.length s - 1)
              end
            end
            else Buffer.add_buffer buf spaces;
            Buffer.clear spaces
          end;
          (* Add any pending whitespace from within the line *)
          if Buffer.length whitespace > 0 then begin
            Buffer.add_buffer buf whitespace;
            Buffer.clear whitespace
          end;
          (* Add the character *)
          Buffer.add_char buf c;
          ignore (Input.next t.input);
          leading_blanks := false;
          scan_line ()
        end
    | _ -> ()
  in

  let rec scan_lines () =
    scan_line ();
    (* Check for line continuation *)
    if Input.next_is_break t.input then begin
      (* Discard any trailing whitespace from the current line *)
      Buffer.clear whitespace;
      (* Save the line break *)
      if !leading_blanks then begin
        (* We already had a break - this is an additional break (empty line) *)
        Buffer.add_char spaces '\n'
      end
      else begin
        (* First line break *)
        Buffer.clear spaces;
        Buffer.add_char spaces '\n';
        leading_blanks := true
      end;
      Input.consume_break t.input;
      (* Note: We do NOT set allow_simple_key here during plain scalar scanning.
         Setting it here would incorrectly allow ':' that appears on a continuation
         line to become a mapping indicator. The flag will be set properly after
         the scalar ends and skip_to_next_token processes line breaks. *)
      (* Skip leading blanks on the next line *)
      while Input.next_is_blank t.input do
        ignore (Input.next t.input)
      done;
      let col = (Input.position t.input).column in
      (* Check indentation - stop if we're at or before the containing block's indent *)
      (* However, allow empty lines (line breaks) to continue even if dedented *)
      if Input.next_is_break t.input then
        scan_lines () (* Empty line - continue *)
      else if (not in_flow) && col <= indent then ()
        (* Stop - dedented or at parent level in block context *)
      else if Input.at_document_boundary t.input then ()
        (* Stop - document boundary *)
      else scan_lines ()
    end
  in

  scan_lines ();
  let value = Buffer.contents buf in
  (* Trim trailing whitespace (spaces and tabs) *)
  let value =
    let len = String.length value in
    let rec find_end i =
      if i < 0 then 0
      else match value.[i] with ' ' | '\t' -> find_end (i - 1) | _ -> i + 1
    in
    let end_pos = find_end (len - 1) in
    String.sub value 0 end_pos
  in
  let span = Span.make ~start ~stop:(Input.mark t.input) in
  (* Return value, span, and whether we ended with leading blanks (crossed a line break) *)
  (value, span, !leading_blanks)

(** Scan block scalar (literal | or folded >) *)
let scan_block_scalar t literal =
  let start = Input.mark t.input in
  ignore (Input.next t.input);

  (* consume | or > *)

  (* Parse header: optional indentation indicator and chomping *)
  let explicit_indent = ref None in
  let chomping = ref Chomping.Clip in

  (* First character of header *)
  (match Input.peek t.input with
  | Some c when Input.is_digit c && c <> '0' ->
      explicit_indent := Some (Char.code c - Char.code '0');
      ignore (Input.next t.input)
  | Some '-' ->
      chomping := Chomping.Strip;
      ignore (Input.next t.input)
  | Some '+' ->
      chomping := Chomping.Keep;
      ignore (Input.next t.input)
  | _ -> ());

  (* Second character of header *)
  (match Input.peek t.input with
  | Some c when Input.is_digit c && c <> '0' && !explicit_indent = None ->
      explicit_indent := Some (Char.code c - Char.code '0');
      ignore (Input.next t.input)
  | Some '-' when !chomping = Chomping.Clip ->
      chomping := Chomping.Strip;
      ignore (Input.next t.input)
  | Some '+' when !chomping = Chomping.Clip ->
      chomping := Chomping.Keep;
      ignore (Input.next t.input)
  | _ -> ());

  (* Skip whitespace and optional comment *)
  skip_whitespace_and_comment t;

  (* Consume line break *)
  if Input.next_is_break t.input then Input.consume_break t.input
  else if not (Input.is_eof t.input) then
    Error.raise_at (Input.mark t.input)
      (Invalid_block_scalar_header "expected newline after header");

  let base_indent = current_indent t in
  (* base_indent is the indent level from the stack, -1 if empty.
     It's used directly for comparisons in implicit indent case. *)
  let content_indent =
    ref
      (match !explicit_indent with
      | Some n ->
          (* Explicit indent: base_indent is 1-indexed column, convert to 0-indexed.
           content_indent = (base_indent - 1) + n, but at least n for document level. *)
          let base_level = max 0 (base_indent - 1) in
          base_level + n
      | None -> 0 (* Will be determined by first non-empty line *))
  in

  let buf = Buffer.create 256 in
  let trailing_breaks = Buffer.create 16 in
  let leading_blank = ref false in
  (* Was the previous line "more indented"? *)
  let max_empty_line_indent = ref 0 in
  (* Track max indent of empty lines before first content *)

  (* Skip to content indentation, skipping empty lines.
     Returns the number of spaces actually skipped (important for detecting dedentation). *)
  let rec skip_to_content_indent () =
    if !content_indent > 0 then begin
      (* Explicit indent - skip up to content_indent spaces *)
      let spaces_skipped = ref 0 in
      while
        !spaces_skipped < !content_indent && Input.next_is (( = ) ' ') t.input
      do
        incr spaces_skipped;
        ignore (Input.next t.input)
      done;

      (* Check if this line is empty (only spaces/tabs until break/eof) *)
      if Input.next_is_break t.input then begin
        (* Empty line - record the break and continue *)
        Buffer.add_char trailing_breaks '\n';
        Input.consume_break t.input;
        skip_to_content_indent ()
      end
      else if !spaces_skipped < !content_indent then begin
        (* Line starts with fewer spaces than content_indent - dedented *)
        !spaces_skipped
      end
      else if Input.next_is_blank t.input then begin
        (* Line has spaces/tabs beyond content_indent - could be whitespace content or empty line.
           For literal scalars, whitespace-only lines ARE content (not empty).
           For folded scalars, whitespace-only lines that are "more indented" are preserved. *)
        if literal then
          (* Literal: whitespace beyond content_indent is content, let read_lines handle it *)
          !content_indent
        else begin
          (* Folded: check if rest is only blanks *)
          let idx = ref 0 in
          while
            match Input.peek_nth t.input !idx with
            | Some c when Input.is_blank c ->
                incr idx;
                true
            | _ -> false
          do
            ()
          done;
          match Input.peek_nth t.input !idx with
          | None | Some '\n' | Some '\r' ->
              (* Empty/whitespace-only line in folded - skip spaces *)
              while Input.next_is_blank t.input do
                ignore (Input.next t.input)
              done;
              Buffer.add_char trailing_breaks '\n';
              Input.consume_break t.input;
              skip_to_content_indent ()
          | _ ->
              (* Has non-whitespace content *)
              !content_indent
        end
      end
      else !content_indent
    end
    else begin
      (* Implicit indent - skip empty lines without consuming spaces.
         Note: Only SPACES count as indentation. Tabs are content, not indentation.
         So we only check for spaces when determining if a line is "empty". *)
      if Input.next_is_break t.input then begin
        Buffer.add_char trailing_breaks '\n';
        Input.consume_break t.input;
        skip_to_content_indent ()
      end
      else if Input.next_is (( = ) ' ') t.input then begin
        (* Check if line is empty (only spaces before break) *)
        let idx = ref 0 in
        while
          match Input.peek_nth t.input !idx with
          | Some ' ' ->
              incr idx;
              true
          | _ -> false
        do
          ()
        done;
        match Input.peek_nth t.input !idx with
        | None | Some '\n' | Some '\r' ->
            (* Line has only spaces - empty line *)
            (* Track max indent of empty lines for later validation *)
            if !idx > !max_empty_line_indent then max_empty_line_indent := !idx;
            while Input.next_is (( = ) ' ') t.input do
              ignore (Input.next t.input)
            done;
            Buffer.add_char trailing_breaks '\n';
            Input.consume_break t.input;
            skip_to_content_indent ()
        | _ ->
            (* Has content (including tabs which are content, not indentation) *)
            0
      end
      else if Input.next_is (( = ) '\t') t.input then begin
        (* Tab at start of line in implicit indent mode - this is an error (Y79Y)
           because tabs cannot be used as indentation in YAML *)
        Error.raise_at (Input.mark t.input) Tab_in_indentation
      end
      else
        (* Not at break or space - other content character *)
        0
    end
  in

  (* Read content *)
  let rec read_lines () =
    let spaces_skipped = skip_to_content_indent () in

    (* Check if we're at content *)
    if Input.is_eof t.input then ()
    else if Input.at_document_boundary t.input then ()
    else begin
      (* Count additional leading spaces beyond what was skipped *)
      let extra_spaces = ref 0 in
      while Input.next_is (( = ) ' ') t.input do
        incr extra_spaces;
        ignore (Input.next t.input)
      done;

      (* Calculate actual line indentation *)
      let line_indent = spaces_skipped + !extra_spaces in

      (* Determine content indent from first content line (implicit case) *)
      let first_line = !content_indent = 0 in
      (* base_indent is 1-indexed column, convert to 0-indexed for comparison with line_indent.
         If base_indent = -1 (empty stack), then base_level = -1 means col 0 is valid. *)
      let base_level = base_indent - 1 in
      let should_process =
        if !content_indent = 0 then begin
          (* For implicit indent, content must be more indented than base_level. *)
          if line_indent <= base_level then false
            (* No content - first line not indented enough *)
          else begin
            (* Validate: first content line must be indented at least as much as
               the maximum indent seen on empty lines before it (5LLU, S98Z, W9L4) *)
            if line_indent < !max_empty_line_indent && line_indent > base_level
            then
              Error.raise_at (Input.mark t.input)
                (Invalid_block_scalar_header
                   "wrongly indented line in block scalar");
            content_indent := line_indent;
            true
          end
        end
        else if line_indent < !content_indent then false
          (* Dedented - done with content *)
        else true
      in

      if should_process then begin
        (* Check if current line is "more indented" (has extra indent or starts with whitespace).
           For folded scalars, lines that start with any whitespace (space or tab) after the
           content indentation are "more indented" and preserve breaks.
           Note: we check Input.next_is_blank BEFORE reading content to see if content starts with whitespace. *)
        let trailing_blank =
          line_indent > !content_indent || Input.next_is_blank t.input
        in

        (* Add trailing breaks to buffer *)
        if Buffer.length buf > 0 then begin
          if Buffer.length trailing_breaks > 0 then begin
            if literal then Buffer.add_buffer buf trailing_breaks
            else begin
              (* Folded scalar: fold only if both previous and current lines are not more-indented *)
              if (not !leading_blank) && not trailing_blank then begin
                let breaks = Buffer.contents trailing_breaks in
                if String.length breaks = 1 then Buffer.add_char buf ' '
                else Buffer.add_substring buf breaks 1 (String.length breaks - 1)
              end
              else begin
                (* Preserve breaks for more-indented lines *)
                Buffer.add_buffer buf trailing_breaks
              end
            end
          end
          else if not literal then Buffer.add_char buf ' '
        end
        else Buffer.add_buffer buf trailing_breaks;
        Buffer.clear trailing_breaks;

        (* Add extra indentation for literal or more-indented folded lines *)
        (* On the first line (when determining content_indent), we've already consumed all spaces,
           so we should NOT add any back. On subsequent lines, we add only the spaces beyond content_indent. *)
        if (not first_line) && (literal || (!extra_spaces > 0 && not literal))
        then begin
          for _ = 1 to !extra_spaces do
            Buffer.add_char buf ' '
          done
        end;

        (* Read line content *)
        while
          (not (Input.is_eof t.input)) && not (Input.next_is_break t.input)
        do
          Buffer.add_char buf (Input.next_exn t.input)
        done;

        (* Record trailing break *)
        if Input.next_is_break t.input then begin
          Buffer.add_char trailing_breaks '\n';
          Input.consume_break t.input
        end;

        (* Update leading_blank for next iteration *)
        leading_blank := trailing_blank;

        read_lines ()
      end
    end
  in

  read_lines ();

  (* Apply chomping *)
  let value =
    let content = Buffer.contents buf in
    match !chomping with
    | Chomping.Strip -> content
    | Chomping.Clip ->
        if String.length content > 0 then content ^ "\n" else content
    | Chomping.Keep -> content ^ Buffer.contents trailing_breaks
  in

  let span = Span.make ~start ~stop:(Input.mark t.input) in
  let style = if literal then `Literal else `Folded in
  (value, style, span)

(** Scan directive (after %) *)
let scan_directive t =
  let start = Input.mark t.input in
  ignore (Input.next t.input);

  (* consume % *)

  (* Read directive name *)
  let name_buf = Buffer.create 16 in
  while
    match Input.peek t.input with
    | Some c when Input.is_alnum c || c = '-' ->
        Buffer.add_char name_buf c;
        ignore (Input.next t.input);
        true
    | _ -> false
  do
    ()
  done;
  let name = Buffer.contents name_buf in

  (* Skip blanks *)
  while Input.next_is_blank t.input do
    ignore (Input.next t.input)
  done;

  match name with
  | "YAML" ->
      (* Version directive: %YAML 1.2 *)
      let major = ref 0 in
      let minor = ref 0 in
      (* Read major version *)
      while Input.next_is_digit t.input do
        major :=
          (!major * 10) + (Char.code (Input.next_exn t.input) - Char.code '0')
      done;
      (* Expect . *)
      (match Input.peek t.input with
      | Some '.' -> ignore (Input.next t.input)
      | _ ->
          Error.raise_at (Input.mark t.input)
            (Invalid_yaml_version "expected '.'"));
      (* Read minor version *)
      while Input.next_is_digit t.input do
        minor :=
          (!minor * 10) + (Char.code (Input.next_exn t.input) - Char.code '0')
      done;
      (* Validate: only whitespace and comments allowed before line break (MUS6) *)
      skip_whitespace_and_comment t;
      if (not (Input.next_is_break t.input)) && not (Input.is_eof t.input) then
        Error.raise_at (Input.mark t.input)
          (Invalid_directive "expected comment or line break after version");
      let span = Span.make ~start ~stop:(Input.mark t.input) in
      (Token.Version_directive { major = !major; minor = !minor }, span)
  | "TAG" ->
      (* Tag directive: %TAG !foo! tag:example.com,2000: *)
      let handle = scan_tag_handle t in
      (* Skip blanks *)
      while Input.next_is_blank t.input do
        ignore (Input.next t.input)
      done;
      (* Read prefix *)
      let prefix_buf = Buffer.create 32 in
      while
        match Input.peek t.input with
        | Some c when not (Input.is_whitespace c) ->
            Buffer.add_char prefix_buf c;
            ignore (Input.next t.input);
            true
        | _ -> false
      do
        ()
      done;
      let prefix = Buffer.contents prefix_buf in
      let span = Span.make ~start ~stop:(Input.mark t.input) in
      (Token.Tag_directive { handle; prefix }, span)
  | _ ->
      (* Reserved/Unknown directive - skip to end of line and ignore *)
      (* Per YAML spec, reserved directives should be ignored with a warning *)
      while (not (Input.is_eof t.input)) && not (Input.next_is_break t.input) do
        ignore (Input.next t.input)
      done;
      let span = Span.make ~start ~stop:(Input.mark t.input) in
      (* Return an empty tag directive token to indicate directive was processed but ignored *)
      (Token.Tag_directive { handle = ""; prefix = "" }, span)

(** Fetch the next token(s) into the queue *)
let rec fetch_next_token t =
  skip_to_next_token t;
  stale_simple_keys t;
  let col = column t in
  (* Unroll indents that are deeper than current column.
     Note: we use col, not col-1, to allow entries at the same level. *)
  unroll_indent t col;

  (* We're about to process actual content, not leading whitespace *)
  t.leading_whitespace <- false;

  if Input.is_eof t.input then fetch_stream_end t
  else if Input.at_document_boundary t.input then fetch_document_indicator t
  else begin
    match Input.peek t.input with
    | None -> fetch_stream_end t
    | Some '%' when (Input.position t.input).column = 1 -> fetch_directive t
    | Some '[' -> fetch_flow_collection_start t Token.Flow_sequence_start
    | Some '{' -> fetch_flow_collection_start t Token.Flow_mapping_start
    | Some ']' -> fetch_flow_collection_end t Token.Flow_sequence_end
    | Some '}' -> fetch_flow_collection_end t Token.Flow_mapping_end
    | Some ',' -> fetch_flow_entry t
    | Some '-' when t.flow_level = 0 && check_block_entry t ->
        fetch_block_entry t
    | Some '?' when check_key t -> fetch_key t
    | Some ':' when check_value t -> fetch_value t
    | Some '*' -> fetch_alias t
    | Some '&' -> fetch_anchor t
    | Some '!' -> fetch_tag t
    | Some '|' when t.flow_level = 0 -> fetch_block_scalar t true
    | Some '>' when t.flow_level = 0 -> fetch_block_scalar t false
    | Some '\'' -> fetch_single_quoted t
    | Some '"' -> fetch_double_quoted t
    | Some '-' when can_start_plain t -> fetch_plain_scalar t
    | Some '?' when can_start_plain t -> fetch_plain_scalar t
    | Some ':' when can_start_plain t -> fetch_plain_scalar t
    | Some c when can_start_plain_char c t -> fetch_plain_scalar t
    | Some c -> Error.raise_at (Input.mark t.input) (Unexpected_character c)
  end

and fetch_stream_end t =
  if not t.stream_ended then begin
    unroll_indent t (-1);
    remove_simple_key t;
    t.allow_simple_key <- false;
    t.stream_ended <- true;
    let span = Span.point (Input.mark t.input) in
    emit t span Token.Stream_end
  end

and fetch_document_indicator t =
  unroll_indent t (-1);
  remove_simple_key t;
  t.allow_simple_key <- false;
  let start = Input.mark t.input in
  let indicator = Input.peek_string t.input 3 in
  Input.skip t.input 3;
  let span = Span.make ~start ~stop:(Input.mark t.input) in
  let token =
    if indicator = "---" then Token.Document_start else Token.Document_end
  in
  (* Reset document content flag after document end marker *)
  if indicator = "..." then begin
    t.document_has_content <- false;
    (* After document end marker, skip whitespace and check for end of line or comment *)
    while Input.next_is_blank t.input do
      ignore (Input.next t.input)
    done;
    match Input.peek t.input with
    | None -> () (* EOF is ok *)
    | Some c when Input.is_break c -> ()
    | Some '#' -> () (* Comment is ok *)
    | _ ->
        Error.raise_at start
          (Invalid_directive
             "content not allowed after document end marker on same line")
  end;
  emit t span token

and fetch_directive t =
  (* Directives can only appear:
     1. At stream start (before any document content)
     2. After a document end marker (...)
     If we've emitted content in the current document, we need a document end marker first *)
  if t.document_has_content then
    Error.raise_at (Input.mark t.input)
      (Unexpected_token
         "directives must be separated from document content by document end \
          marker (...)");
  unroll_indent t (-1);
  remove_simple_key t;
  t.allow_simple_key <- false;
  let token, span = scan_directive t in
  emit t span token

and fetch_flow_collection_start t token_type =
  save_simple_key t;
  (* Record indent of outermost flow collection *)
  if t.flow_level = 0 then t.flow_indent <- column t;
  t.flow_level <- t.flow_level + 1;
  (* Track whether this is a mapping or sequence *)
  let is_mapping = token_type = Token.Flow_mapping_start in
  t.flow_mapping_stack <- is_mapping :: t.flow_mapping_stack;
  t.allow_simple_key <- true;
  t.simple_keys <- None :: t.simple_keys;
  t.document_has_content <- true;
  let start = Input.mark t.input in
  ignore (Input.next t.input);
  let span = Span.make ~start ~stop:(Input.mark t.input) in
  emit t span token_type

and fetch_flow_collection_end t token_type =
  remove_simple_key t;
  t.flow_level <- t.flow_level - 1;
  t.flow_mapping_stack <-
    (match t.flow_mapping_stack with _ :: rest -> rest | [] -> []);
  t.simple_keys <- (match t.simple_keys with _ :: rest -> rest | [] -> []);
  t.allow_simple_key <- false;
  let start = Input.mark t.input in
  ignore (Input.next t.input);
  (* Allow adjacent values after flow collection ends *)
  if t.flow_level > 0 then
    t.adjacent_value_allowed_at <- Some (Input.position t.input);
  let span = Span.make ~start ~stop:(Input.mark t.input) in
  emit t span token_type

and fetch_flow_entry t =
  remove_simple_key t;
  t.allow_simple_key <- true;
  let start = Input.mark t.input in
  ignore (Input.next t.input);
  let span = Span.make ~start ~stop:(Input.mark t.input) in
  emit t span Token.Flow_entry

and check_block_entry t =
  (* - followed by whitespace or EOF *)
  match Input.peek_nth t.input 1 with
  | None -> true
  | Some c -> Input.is_whitespace c

and fetch_block_entry t =
  if t.flow_level = 0 then begin
    (* Block entries require allow_simple_key to be true.
       This prevents block sequences on the same line as a mapping value,
       e.g., "key: - a" is invalid. *)
    if not t.allow_simple_key then
      Error.raise_at (Input.mark t.input) Block_sequence_disallowed;
    let col = column t in
    if roll_indent t col then begin
      let span = Span.point (Input.mark t.input) in
      emit t span Token.Block_sequence_start
    end
  end;
  remove_simple_key t;
  t.allow_simple_key <- true;
  t.document_has_content <- true;
  let start = Input.mark t.input in
  ignore (Input.next t.input);

  (* Check for tabs after - : pattern like -\t- is invalid *)
  let found_tabs, _found_spaces = skip_blanks_check_tabs t in
  if found_tabs then begin
    (* If we found tabs and next char is - followed by whitespace, error *)
    match Input.peek t.input with
    | Some '-' -> (
        match Input.peek_nth t.input 1 with
        | None -> Error.raise_at start Tab_in_indentation
        | Some c when Input.is_whitespace c ->
            Error.raise_at start Tab_in_indentation
        | Some _ -> ())
    | _ -> ()
  end;

  let span = Span.make ~start ~stop:(Input.mark t.input) in
  emit t span Token.Block_entry

and check_key t =
  (* ? followed by whitespace or flow indicator in both block and flow *)
  match Input.peek_nth t.input 1 with
  | None -> true
  | Some c ->
      Input.is_whitespace c || (t.flow_level > 0 && Input.is_flow_indicator c)

and fetch_key t =
  if t.flow_level = 0 then begin
    if not t.allow_simple_key then
      Error.raise_at (Input.mark t.input) Expected_key;
    let col = column t in
    if roll_indent t col then begin
      let span = Span.point (Input.mark t.input) in
      emit t span Token.Block_mapping_start
    end
  end;
  remove_simple_key t;
  t.allow_simple_key <- t.flow_level = 0;
  t.document_has_content <- true;
  let start = Input.mark t.input in
  ignore (Input.next t.input);

  (* Check for tabs after ? : pattern like ?\t- or ?\tkey is invalid *)
  let found_tabs, _found_spaces = skip_blanks_check_tabs t in
  if found_tabs && t.flow_level = 0 then begin
    (* In block context, tabs after ? are not allowed *)
    Error.raise_at start Tab_in_indentation
  end;

  let span = Span.make ~start ~stop:(Input.mark t.input) in
  emit t span Token.Key

and check_value t =
  (* : followed by whitespace in block, or whitespace/flow indicator in flow, or adjacent value *)
  match Input.peek_nth t.input 1 with
  | None -> true
  | Some c -> (
      Input.is_whitespace c
      || (t.flow_level > 0 && Input.is_flow_indicator c)
      ||
      (* Allow adjacent values in flow context at designated positions *)
      t.flow_level > 0
      &&
      match t.adjacent_value_allowed_at with
      | Some pos ->
          pos.Position.line = (Input.position t.input).Position.line
          && pos.Position.column = (Input.position t.input).Position.column
      | None -> false)

and fetch_value t =
  let start = Input.mark t.input in
  (* Check for simple key *)
  let used_simple_key =
    match t.simple_keys with
    | Some sk :: _ when sk.sk_possible ->
        (* In implicit flow mapping (inside a flow sequence), key and : must be on the same line.
           In explicit flow mapping { }, key and : can span lines. *)
        let is_implicit_flow_mapping =
          match t.flow_mapping_stack with
          | false :: _ ->
              true (* false = we're in a sequence, so any mapping is implicit *)
          | _ -> false
        in
        if
          is_implicit_flow_mapping
          && sk.sk_position.line < (Input.position t.input).line
        then Error.raise_at start Illegal_flow_key_line;
        (* Insert KEY token before the simple key value *)
        let key_span = Span.point sk.sk_position in
        let key_token = { Token.token = Token.Key; span = key_span } in
        (* We need to insert at the right position *)
        let tokens = Queue.to_seq t.tokens |> Array.of_seq in
        Queue.clear t.tokens;
        let insert_pos = sk.sk_token_number - t.tokens_taken in
        Array.iteri
          (fun i tok ->
            if i = insert_pos then Queue.add key_token t.tokens;
            Queue.add tok t.tokens)
          tokens;
        if insert_pos >= Array.length tokens then Queue.add key_token t.tokens;
        t.token_number <- t.token_number + 1;
        (* Roll indent for implicit block mapping *)
        if t.flow_level = 0 then begin
          let col = sk.sk_position.column in
          if roll_indent t col then begin
            let span = Span.point sk.sk_position in
            (* Insert block mapping start before key *)
            let bm_token = { Token.token = Token.Block_mapping_start; span } in
            let tokens = Queue.to_seq t.tokens |> Array.of_seq in
            Queue.clear t.tokens;
            Array.iteri
              (fun i tok ->
                if i = insert_pos then Queue.add bm_token t.tokens;
                Queue.add tok t.tokens)
              tokens;
            if insert_pos >= Array.length tokens then
              Queue.add bm_token t.tokens;
            t.token_number <- t.token_number + 1
          end
        end;
        t.simple_keys <- None :: List.tl t.simple_keys;
        true
    | _ ->
        (* No simple key - this is a complex value (or empty key) *)
        if t.flow_level = 0 then begin
          if not t.allow_simple_key then
            Error.raise_at (Input.mark t.input) Expected_key;
          let col = column t in
          if roll_indent t col then begin
            let span = Span.point (Input.mark t.input) in
            emit t span Token.Block_mapping_start
          end
          (* Note: We don't emit KEY here. Empty key handling is done by the parser,
             which emits empty scalar when it sees VALUE without preceding KEY. *)
        end;
        false
  in
  remove_simple_key t;
  (* In block context without simple key, allow simple keys for compact mappings like ": moon: white"
     In flow context or after using a simple key, disallow simple keys *)
  t.allow_simple_key <- (not used_simple_key) && t.flow_level = 0;
  t.document_has_content <- true;
  let start = Input.mark t.input in
  ignore (Input.next t.input);

  (* Check for tabs after : : patterns like :\t- or :\tkey: are invalid in block context (Y79Y/09)
     However, :\t bar (tab followed by space then content) is valid (6BCT) *)
  let found_tabs, found_spaces = skip_blanks_check_tabs t in
  if found_tabs && (not found_spaces) && t.flow_level = 0 then begin
    (* In block context, tabs-only after : followed by indicator or alphanumeric are not allowed *)
    match Input.peek t.input with
    | Some ('-' | '?') -> Error.raise_at start Tab_in_indentation
    | Some c
      when (c >= 'a' && c <= 'z')
           || (c >= 'A' && c <= 'Z')
           || (c >= '0' && c <= '9') ->
        (* Tab-only followed by alphanumeric - likely a key, which is invalid *)
        Error.raise_at start Tab_in_indentation
    | _ -> ()
  end;

  (* Skip any comment that may follow the colon and whitespace *)
  skip_whitespace_and_comment t;

  let span = Span.make ~start ~stop:(Input.mark t.input) in
  emit t span Token.Value

and fetch_anchor_or_alias t ~is_alias =
  save_simple_key t;
  t.allow_simple_key <- false;
  t.document_has_content <- true;
  let start = Input.mark t.input in
  ignore (Input.next t.input);
  (* consume * or & *)
  let name, span = scan_anchor_alias t in
  let span = Span.make ~start ~stop:span.stop in
  let token = if is_alias then Token.Alias name else Token.Anchor name in
  emit t span token

and fetch_alias t = fetch_anchor_or_alias t ~is_alias:true
and fetch_anchor t = fetch_anchor_or_alias t ~is_alias:false

and fetch_tag t =
  save_simple_key t;
  t.allow_simple_key <- false;
  t.document_has_content <- true;
  let handle, suffix, span = scan_tag t in
  emit t span (Token.Tag { handle; suffix })

and fetch_block_scalar t literal =
  remove_simple_key t;
  t.allow_simple_key <- true;
  t.document_has_content <- true;
  let value, style, span = scan_block_scalar t literal in
  emit t span (Token.Scalar { style; value })

and fetch_quoted t ~double =
  save_simple_key t;
  t.allow_simple_key <- false;
  t.document_has_content <- true;
  let value, span =
    if double then scan_double_quoted t else scan_single_quoted t
  in
  (* Allow adjacent values after quoted scalars in flow context (for JSON compatibility) *)
  skip_to_next_token t;
  if t.flow_level > 0 then
    t.adjacent_value_allowed_at <- Some (Input.position t.input);
  let style = if double then `Double_quoted else `Single_quoted in
  emit t span (Token.Scalar { style; value })

and fetch_single_quoted t = fetch_quoted t ~double:false
and fetch_double_quoted t = fetch_quoted t ~double:true

and can_start_plain t =
  (* Check if - ? : can start a plain scalar *)
  match Input.peek_nth t.input 1 with
  | None -> false
  | Some c ->
      (not (Input.is_whitespace c))
      && (t.flow_level = 0 || not (Input.is_flow_indicator c))

and can_start_plain_char c _t =
  (* Characters that can start a plain scalar *)
  if Input.is_whitespace c then false
  else if Input.is_indicator c then false
  else true

and fetch_plain_scalar t =
  save_simple_key t;
  t.allow_simple_key <- false;
  t.document_has_content <- true;
  let value, span, ended_with_linebreak = scan_plain_scalar t in
  (* If the plain scalar ended after crossing a line break (leading_blanks = true),
     allow simple keys. This is important because the scanner already consumed the
     line break and leading whitespace when checking for continuation. *)
  if ended_with_linebreak then t.allow_simple_key <- true;
  emit t span (Token.Scalar { style = `Plain; value })

(** Check if we need more tokens to resolve simple keys *)
let need_more_tokens t =
  if t.stream_ended then false
  else if Queue.is_empty t.tokens then true
  else
    (* Check if any simple key could affect the first queued token *)
    List.exists
      (function
        | Some sk when sk.sk_possible -> sk.sk_token_number >= t.tokens_taken
        | _ -> false)
      t.simple_keys

(** Ensure we have enough tokens to return one safely *)
let ensure_tokens t =
  if not t.stream_started then begin
    t.stream_started <- true;
    let span = Span.point (Input.position t.input) in
    let encoding, _ = Encoding.detect (Input.source t.input) in
    emit t span (Token.Stream_start encoding)
  end;
  while need_more_tokens t do
    fetch_next_token t
  done

(** Get next token *)
let next t =
  ensure_tokens t;
  if Queue.is_empty t.tokens then None
  else begin
    t.tokens_taken <- t.tokens_taken + 1;
    Some (Queue.pop t.tokens)
  end

(** Peek at next token *)
let peek t =
  ensure_tokens t;
  Queue.peek_opt t.tokens

(** Iterate over all tokens *)
let iter f t =
  let rec loop () =
    match next t with
    | None -> ()
    | Some tok ->
        f tok;
        loop ()
  in
  loop ()

(** Fold over all tokens *)
let fold f init t =
  let rec loop acc =
    match next t with None -> acc | Some tok -> loop (f acc tok)
  in
  loop init

(** Convert to list *)
let to_list t = fold (fun acc tok -> tok :: acc) [] t |> List.rev
