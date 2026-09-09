(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Bytesrw

(* Aliases for cleaner code *)
module V = Value

(* Lexer - streams bytes from a Bytes.Reader through a small lookahead window.

   The lexer never buffers the entire input. It maintains a sliding window [buf]
   of recently-read bytes; [buf_pos] points at the next byte to consume and
   [buf_len] is the number of valid bytes in [buf]. When lookahead needs more
   bytes than [buf_len - buf_pos] offers, [refill] compacts the window (moving
   unread bytes to the front) and reads one slice from the underlying
   [Bytes.Reader.t]. The window grows on demand up to [max_buf_size] for the
   rare bare key / number that exceeds the default [initial_buf_size].

   All token content (strings, numbers, bare keys, datetimes) is accumulated
   into per-token [Buffer.t] values as bytes are consumed, so token construction
   never depends on re-addressing an earlier [buf_pos]. The window size only
   needs to cover the longest lookahead required at any decision point. *)

type token =
  | Lbracket
  | Rbracket
  | Lbrace
  | Rbrace
  | Equals
  | Comma
  | Dot
  | Newline
  | Eof
  | Bare_key of string
  | Basic_string of string
  | Literal_string of string
  | Ml_basic_string of string (* Multiline basic string - not valid as key *)
  | Ml_literal_string of
      string (* Multiline literal string - not valid as key *)
  | Integer of
      int64 * string (* value, original string for key reconstruction *)
  | Float of float * string (* value, original string for key reconstruction *)
  | Datetime of string
  | Datetime_local of string
  | Date_local of string
  | Time_local of string

let initial_buf_size = 64
let max_buf_size = 65_536

type lexer = {
  reader : Bytes.Reader.t;
  source : Buffer.t;
      (* Every byte read, in the order it was read. The window below is capped
         at [max_buf_size] and forgets what it has passed, but a document has to
         be writable back as the bytes it arrived as, so they are kept here as
         [refill] takes them off the reader. One copy of the input, against an
         AST that is already several times its size. *)
  mutable buf : bytes;
  mutable buf_pos : int; (* next byte to consume in buf *)
  mutable buf_len : int; (* number of valid bytes in buf *)
  mutable eof : bool;
  mutable line : int;
  mutable col : int;
  mutable byte : int; (* absolute byte offset of the next byte to consume *)
  mutable line_byte : int;
      (* absolute byte offset of the current line's start *)
  file : string;
}

(* Grow the window to at least [cap] bytes, preserving valid content. *)
let grow_buf l cap =
  let new_size = ref (Bytes.length l.buf) in
  while !new_size < cap do
    new_size := !new_size * 2
  done;
  if !new_size > max_buf_size then new_size := max_buf_size;
  if !new_size > Bytes.length l.buf then begin
    let new_buf = Bytes.create !new_size in
    Bytes.blit l.buf 0 new_buf 0 l.buf_len;
    l.buf <- new_buf
  end

(* Compact the window (moving unread bytes to the front) and read one slice from
   the reader, appending to the window. *)
let refill l =
  if l.eof then ()
  else begin
    let remaining = l.buf_len - l.buf_pos in
    if remaining > 0 && l.buf_pos > 0 then
      Bytes.blit l.buf l.buf_pos l.buf 0 remaining;
    l.buf_pos <- 0;
    l.buf_len <- remaining;
    let slice = Bytes.Reader.read l.reader in
    if Bytes.Slice.is_eod slice then l.eof <- true
    else begin
      let src = Bytes.Slice.bytes slice in
      let off = Bytes.Slice.first slice in
      let len = Bytes.Slice.length slice in
      let space = Bytes.length l.buf - l.buf_len in
      if len > space then grow_buf l (l.buf_len + len);
      let space = Bytes.length l.buf - l.buf_len in
      let to_copy = min len space in
      Bytes.blit src off l.buf l.buf_len to_copy;
      Buffer.add_subbytes l.source src off to_copy;
      l.buf_len <- l.buf_len + to_copy;
      if to_copy < len then
        Bytes.Reader.push_back l.reader
          (Bytes.Slice.make src ~first:(off + to_copy) ~length:(len - to_copy))
    end
  end

(* Ensure at least [n] bytes are available in the window, if possible. When [n]
   exceeds the current window size, grow first so a single refill round can
   satisfy the request. The window is capped at [max_buf_size]: once a refill
   makes no progress against a full window, stop -- callers observe fewer than
   [n] bytes, exactly as at end of input, instead of spinning on a reader that
   still has bytes the window cannot hold. *)
let ensure_n l n =
  if n > Bytes.length l.buf then grow_buf l n;
  let progress = ref true in
  while !progress && l.buf_len - l.buf_pos < n && not l.eof do
    let before = l.buf_len - l.buf_pos in
    refill l;
    progress := l.buf_len - l.buf_pos > before
  done

(* Drop a leading UTF-8 byte order mark (EF BB BF).

   The Unicode Standard (16.0, s. 23.8, "Byte Order Mark") makes an initial
   U+FEFF an encoding signature rather than text, so it is removed before the
   first token is read; a Windows editor writes one as a matter of course. Only
   the very start is a signature -- a mark anywhere else is content, and U+FEFF
   is not a key, so it stays the error it was.

   Whether a document parses is a property of its bytes, and a reader owes no
   particular slice length, so the three mark bytes can arrive over three
   separate reads: a file behind a small buffer, a socket, a decompressor.
   Deciding on whatever the first read delivered would leave the tail of the
   mark in front of the first key, so [ensure_n] fills until the window holds
   three bytes or the input ends before the signature is looked for. A document
   shorter than three bytes cannot carry a mark, and the end-of-input exit
   covers it.

   The mark still counts toward [byte], so reported offsets stay those of the
   file as written, while [line_byte] moves past it so the first line begins at
   its first character. *)
let skip_bom l =
  ensure_n l 3;
  if
    l.buf_len - l.buf_pos >= 3
    && Bytes.get l.buf l.buf_pos = '\xEF'
    && Bytes.get l.buf (l.buf_pos + 1) = '\xBB'
    && Bytes.get l.buf (l.buf_pos + 2) = '\xBF'
  then begin
    l.buf_pos <- l.buf_pos + 3;
    l.byte <- l.byte + 3;
    l.line_byte <- l.byte
  end

(* Create lexer directly from Bytes.Reader. *)
let lexer_of_reader ?(file = "-") reader =
  let l =
    {
      reader;
      source = Buffer.create initial_buf_size;
      buf = Bytes.create initial_buf_size;
      buf_pos = 0;
      buf_len = 0;
      eof = false;
      line = 1;
      col = 1;
      byte = 0;
      line_byte = 0;
      file;
    }
  in
  skip_bom l;
  l

(* Create lexer from string (wraps as a reader so the streaming path is
   exercised in both APIs). *)
let lexer ?file s = lexer_of_reader ?file (Bytes.Reader.of_string s)

let is_eof l =
  if l.buf_pos < l.buf_len then false
  else if l.eof then true
  else begin
    ensure_n l 1;
    l.buf_pos >= l.buf_len
  end

let peek l =
  if l.buf_pos < l.buf_len then Some (Bytes.unsafe_get l.buf l.buf_pos)
  else if l.eof then None
  else begin
    ensure_n l 1;
    if l.buf_pos < l.buf_len then Some (Bytes.unsafe_get l.buf l.buf_pos)
    else None
  end

let peek2 l =
  if l.buf_pos + 1 < l.buf_len then
    Some (Bytes.unsafe_get l.buf (l.buf_pos + 1))
  else begin
    ensure_n l 2;
    if l.buf_pos + 1 < l.buf_len then
      Some (Bytes.unsafe_get l.buf (l.buf_pos + 1))
    else None
  end

let peek_n l n =
  ensure_n l n;
  if l.buf_len - l.buf_pos < n then None
  else Some (Bytes.sub_string l.buf l.buf_pos n)

(* Advance over a byte we know is present in the window; updates line/col and
   absolute byte tracking. *)
let advance l =
  if l.buf_pos < l.buf_len then begin
    let c = Bytes.unsafe_get l.buf l.buf_pos in
    l.buf_pos <- l.buf_pos + 1;
    l.byte <- l.byte + 1;
    if c = '\n' then begin
      l.line <- l.line + 1;
      l.col <- 1;
      l.line_byte <- l.byte
    end
    else l.col <- l.col + 1
  end
  else if not l.eof then begin
    ensure_n l 1;
    if l.buf_pos < l.buf_len then begin
      let c = Bytes.unsafe_get l.buf l.buf_pos in
      l.buf_pos <- l.buf_pos + 1;
      l.byte <- l.byte + 1;
      if c = '\n' then begin
        l.line <- l.line + 1;
        l.col <- 1;
        l.line_byte <- l.byte
      end
      else l.col <- l.col + 1
    end
  end

let advance_n l n =
  for _ = 1 to n do
    advance l
  done

let skip_whitespace l =
  let continue = ref true in
  while !continue do
    match peek l with
    | Some ' ' | Some '\t' -> advance l
    | _ -> continue := false
  done

(* Access the byte at [buf_pos + off]. Requires [ensure_n (off + 1)] to have
   been called. Returns [' '] as a stand-in at EOF for callers that only probe
   already-ensured positions. *)
let[@inline] window_char l off =
  if l.buf_pos + off < l.buf_len then Bytes.unsafe_get l.buf (l.buf_pos + off)
  else ' '

let[@inline] window_has l off = l.buf_pos + off < l.buf_len
let[@inline] current l = Bytes.unsafe_get l.buf l.buf_pos

(* Build a [Loc.Meta.t] from the lexer's current position. The location spans
   the single byte at [l.byte] (the next byte to consume); callers that know a
   token's extent can widen it with [Loc.span]. *)
let lexer_meta l =
  let loc =
    Loc.v ~file:l.file ~first_byte:l.byte ~last_byte:l.byte
      ~first_line_num:l.line ~first_line_byte:l.line_byte ~last_line_num:l.line
      ~last_line_byte:l.line_byte
  in
  Loc.Meta.v loc

(* Get expected byte length of UTF-8 char from first byte *)
let utf8_first_byte_len c =
  let code = Char.code c in
  if code < 0x80 then 1
  else if code < 0xC0 then 0 (* Invalid: continuation byte as start *)
  else if code < 0xE0 then 2
  else if code < 0xF0 then 3
  else if code < 0xF8 then 4
  else 0 (* Invalid: 5+ byte sequence *)

(* Validate UTF-8 at the current window position, returning the byte length of
   the encoded codepoint. Ensures enough bytes are in the window for the full
   sequence. *)
let validate_utf8_at_pos_bytes l =
  if is_eof l then Error.raise_lexer ~meta:(lexer_meta l) Unexpected_eof;
  let byte_len = utf8_first_byte_len (Bytes.unsafe_get l.buf l.buf_pos) in
  if byte_len = 0 then Error.raise_lexer ~meta:(lexer_meta l) Invalid_utf8;
  ensure_n l byte_len;
  if l.buf_len - l.buf_pos < byte_len then
    Error.raise_lexer ~meta:(lexer_meta l) Incomplete_utf8;
  (* The window must hold exactly one scalar and nothing else. Accepting it
     because *some* byte in it decoded let a malformed lead byte swallow what
     followed: in "\xF6\u00e9" the 0xF6 claims four bytes, the backslash in
     them decodes on its own, and the escape it introduced was never seen. *)
  let sub = Bytes.sub_string l.buf l.buf_pos byte_len in
  let d = String.get_utf_8_uchar sub 0 in
  if not (Uchar.utf_decode_is_valid d && Uchar.utf_decode_length d = byte_len)
  then Error.raise_lexer ~meta:(lexer_meta l) Invalid_utf8;
  byte_len

(* UTF-8 validation - validates and advances over a single UTF-8 character *)
let validate_utf8_char l =
  let byte_len = validate_utf8_at_pos_bytes l in
  for _ = 1 to byte_len do
    advance l
  done

(* Append the current UTF-8 character to [buf] and advance over it. *)
let utf8_add_to_buffer l buf =
  let byte_len = validate_utf8_at_pos_bytes l in
  Buffer.add_subbytes buf l.buf l.buf_pos byte_len;
  for _ = 1 to byte_len do
    advance l
  done

let handle_comment_cr l continue =
  if peek2 l = Some '\n' then continue := false
  else Error.raise_lexer ~meta:(lexer_meta l) Bare_carriage_return

let handle_comment_ascii l code =
  if code < 0x09 || (code > 0x09 && code < 0x20) || code = 0x7F then
    Error.raise_lexer ~meta:(lexer_meta l) (Control_character code);
  advance l

let handle_comment_char l continue =
  let c = current l in
  let code = Char.code c in
  if c = '\r' then handle_comment_cr l continue
  else if code >= 0x80 then validate_utf8_char l
  else handle_comment_ascii l code

let skip_comment l =
  if (not (is_eof l)) && current l = '#' then begin
    advance l;
    let continue = ref true in
    while !continue && (not (is_eof l)) && current l <> '\n' do
      handle_comment_char l continue
    done
  end

let skip_ws_and_comments l =
  let rec loop () =
    skip_whitespace l;
    if (not (is_eof l)) && current l = '#' then begin
      skip_comment l;
      loop ()
    end
  in
  loop ()

let is_bare_key_char c =
  (c >= 'A' && c <= 'Z')
  || (c >= 'a' && c <= 'z')
  || (c >= '0' && c <= '9')
  || c = '_' || c = '-'

let is_digit c = c >= '0' && c <= '9'
let is_oct_digit c = c >= '0' && c <= '7'
let is_bin_digit c = c = '0' || c = '1'

(* Callers always guard with [Ascii.is_hex_digit], so the non-digit branch is
   unreachable in practice; there is no lexer position to attach to its
   error. *)
let hex_value c =
  match Ascii.hex_value_int c with
  | -1 -> Error.raise_number ~meta:Loc.Meta.none Invalid_hex_digit
  | v -> v

(* Convert a Unicode code point to UTF-8.

   This helper is only called from the Tagged_json decoder, which is a JSON
   helper for the toml-test harness and not part of the main TOML parse flow.
   Invalid codepoints here indicate malformed test input, so we raise the
   regular Failure; the Tagged_json layer catches and reports it as a string
   error. *)
let utf8_of_codepoint codepoint =
  if codepoint < 0 || codepoint > 0x10FFFF then
    Fmt.failwith "Invalid Unicode codepoint: U+%X" codepoint;
  if codepoint >= 0xD800 && codepoint <= 0xDFFF then
    Fmt.failwith "Surrogate codepoint not allowed: U+%04X" codepoint;
  let buf = Buffer.create 4 in
  Buffer.add_utf_8_uchar buf (Uchar.of_int codepoint);
  Buffer.contents buf

(* Parse Unicode escape with error location from lexer *)
let utf8_of_unicode l codepoint =
  if codepoint < 0 || codepoint > 0x10FFFF then
    Error.raise_lexer ~meta:(lexer_meta l) (Invalid_unicode_codepoint codepoint);
  if codepoint >= 0xD800 && codepoint <= 0xDFFF then
    Error.raise_lexer ~meta:(lexer_meta l) (Surrogate_codepoint codepoint);
  let buf = Buffer.create 4 in
  Buffer.add_utf_8_uchar buf (Uchar.of_int codepoint);
  Buffer.contents buf

let parse_escape l =
  advance l;
  (* skip backslash *)
  if is_eof l then Error.raise_lexer ~meta:(lexer_meta l) Unexpected_eof;
  let c = current l in
  advance l;
  match c with
  | 'b' -> "\b"
  | 't' -> "\t"
  | 'n' -> "\n"
  | 'f' -> "\x0C"
  | 'r' -> "\r"
  | 'e' -> "\x1B" (* TOML 1.1 escape *)
  | '"' -> "\""
  | '\\' -> "\\"
  | 'x' ->
      (* \xHH - 2 hex digits *)
      ensure_n l 2;
      if l.buf_len - l.buf_pos < 2 then
        Error.raise_lexer ~meta:(lexer_meta l) (Incomplete_escape "\\x");
      let c1 = Bytes.unsafe_get l.buf l.buf_pos in
      let c2 = Bytes.unsafe_get l.buf (l.buf_pos + 1) in
      if not (Ascii.is_hex_digit c1 && Ascii.is_hex_digit c2) then
        Error.raise_lexer ~meta:(lexer_meta l) (Invalid_unicode_escape "\\x");
      let cp = (hex_value c1 * 16) + hex_value c2 in
      advance l;
      advance l;
      utf8_of_unicode l cp
  | 'u' ->
      (* \uHHHH - 4 hex digits *)
      ensure_n l 4;
      if l.buf_len - l.buf_pos < 4 then
        Error.raise_lexer ~meta:(lexer_meta l) (Incomplete_escape "\\u");
      let s = Bytes.sub_string l.buf l.buf_pos 4 in
      for i = 0 to 3 do
        if not (Ascii.is_hex_digit s.[i]) then
          Error.raise_lexer ~meta:(lexer_meta l) (Invalid_unicode_escape "\\u")
      done;
      let cp = int_of_string ("0x" ^ s) in
      advance_n l 4;
      utf8_of_unicode l cp
  | 'U' ->
      (* \UHHHHHHHH - 8 hex digits *)
      ensure_n l 8;
      if l.buf_len - l.buf_pos < 8 then
        Error.raise_lexer ~meta:(lexer_meta l) (Incomplete_escape "\\U");
      let s = Bytes.sub_string l.buf l.buf_pos 8 in
      for i = 0 to 7 do
        if not (Ascii.is_hex_digit s.[i]) then
          Error.raise_lexer ~meta:(lexer_meta l) (Invalid_unicode_escape "\\U")
      done;
      let cp = int_of_string ("0x" ^ s) in
      advance_n l 8;
      utf8_of_unicode l cp
  | _ -> Error.raise_lexer ~meta:(lexer_meta l) (Invalid_escape c)

let validate_string_char l c is_multiline =
  let code = Char.code c in
  (* Control characters other than tab (and LF/CR for multiline) are not
     allowed *)
  if code < 0x09 then
    Error.raise_lexer ~meta:(lexer_meta l) (Control_character code);
  if
    code > 0x09 && code < 0x20
    && not (is_multiline && (code = 0x0A || code = 0x0D))
  then Error.raise_lexer ~meta:(lexer_meta l) (Control_character code);
  if code = 0x7F then
    Error.raise_lexer ~meta:(lexer_meta l) (Control_character code)

(* Validate UTF-8 in string context and add bytes to buffer *)
let utf8_add_validated l buf = utf8_add_to_buffer l buf

let skip_opening_newline l =
  match peek l with
  | Some '\n' -> advance l
  | Some '\r' ->
      advance l;
      if peek l = Some '\n' then advance l
      else Error.raise_lexer ~meta:(lexer_meta l) Bare_carriage_return
  | _ -> ()

let skip_all_ws_newlines l =
  let rec loop () =
    match peek l with
    | Some ' ' | Some '\t' | Some '\n' ->
        advance l;
        loop ()
    | Some '\r' ->
        advance l;
        if peek l = Some '\n' then advance l;
        loop ()
    | _ -> ()
  in
  loop ()

let handle_multiline_quotes l buf quote_char cont =
  (* At most 5 quotes matter: 3 closing + up to 2 literal. A 6th quote of the
     same kind is an error, so peek up to 6 bytes to diagnose it. *)
  ensure_n l 6;
  let quote_count = ref 0 in
  let off = ref 0 in
  while !off < 6 && window_has l !off && window_char l !off = quote_char do
    incr quote_count;
    incr off
  done;
  if !quote_count >= 3 then begin
    let extra = min (!quote_count - 3) 2 in
    for _ = 1 to extra do
      Buffer.add_char buf quote_char
    done;
    advance_n l !quote_count;
    if !quote_count > 5 then
      Error.raise_lexer ~meta:(lexer_meta l) Too_many_quotes
  end
  else begin
    for _ = 1 to !quote_count do
      Buffer.add_char buf quote_char;
      advance l
    done;
    cont ()
  end

let handle_multiline_basic_backslash l buf loop =
  (* The backslash is at the window head. A backslash followed by whitespace is
     only legal as a line-ending backslash, so the whitespace run can be
     consumed eagerly: it either reaches a newline (line continuation) or hits a
     non-whitespace byte, which makes the backslash an invalid escape. Consuming
     as we scan keeps arbitrarily long runs inside the bounded window; a
     peek-ahead classification would need the whole run in view at once. *)
  ensure_n l 2;
  let next = if window_has l 1 then Some (window_char l 1) else None in
  match next with
  | Some (' ' | '\t' | '\n' | '\r') ->
      advance l;
      (* consume the backslash *)
      let rec skip_ws () =
        match peek l with
        | Some ' ' | Some '\t' ->
            advance l;
            skip_ws ()
        | _ -> ()
      in
      skip_ws ();
      (match peek l with
      | Some '\n' -> advance l
      | Some '\r' ->
          advance l;
          if peek l = Some '\n' then advance l
      | Some c -> Error.raise_lexer ~meta:(lexer_meta l) (Invalid_escape c)
      | None -> Error.raise_lexer ~meta:(lexer_meta l) Unexpected_eof);
      skip_all_ws_newlines l;
      loop ()
  | _ ->
      Buffer.add_string buf (parse_escape l);
      loop ()

let rec parse_multiline_basic_string l buf () =
  if is_eof l then Error.raise_lexer ~meta:(lexer_meta l) Unterminated_string;
  let c = current l in
  if c = '"' then
    handle_multiline_quotes l buf '"' (parse_multiline_basic_string l buf)
  else if c = '\\' then
    handle_multiline_basic_backslash l buf (parse_multiline_basic_string l buf)
  else begin
    if c = '\r' then begin
      advance l;
      if peek l = Some '\n' then (
        Buffer.add_char buf '\n';
        advance l)
      else Error.raise_lexer ~meta:(lexer_meta l) Bare_carriage_return
    end
    else if Char.code c >= 0x80 then utf8_add_validated l buf
    else begin
      validate_string_char l c true;
      Buffer.add_char buf c;
      advance l
    end;
    parse_multiline_basic_string l buf ()
  end

let rec parse_single_basic_string l buf () =
  if is_eof l then Error.raise_lexer ~meta:(lexer_meta l) Unterminated_string;
  let c = current l in
  if c = '"' then advance l
  else if c = '\\' then (
    Buffer.add_string buf (parse_escape l);
    parse_single_basic_string l buf ())
  else if c = '\n' || c = '\r' then
    Error.raise_lexer ~meta:(lexer_meta l) Newline_in_string
  else begin
    if Char.code c >= 0x80 then utf8_add_validated l buf
    else begin
      validate_string_char l c false;
      Buffer.add_char buf c;
      advance l
    end;
    parse_single_basic_string l buf ()
  end

(* Top-level dispatcher shared by basic and literal string parsing: skip the
   opening delimiter, check for triple-delimiter (multiline form), and hand off
   to the appropriate per-character parser. *)
let parse_quoted_string l ~triple_delim ~multiline_parser ~single_parser =
  advance l;
  let buf = Buffer.create 64 in
  let multiline =
    match peek_n l 2 with
    | Some d when d = triple_delim ->
        advance l;
        advance l;
        skip_opening_newline l;
        true
    | _ -> false
  in
  if multiline then multiline_parser l buf () else single_parser l buf ();
  (Buffer.contents buf, multiline)

let parse_basic_string l =
  parse_quoted_string l ~triple_delim:"\"\""
    ~multiline_parser:parse_multiline_basic_string
    ~single_parser:parse_single_basic_string

let validate_literal_ctrl l c code ~multiline =
  let is_ctrl =
    if multiline then
      code < 0x09
      || (code > 0x09 && code < 0x0A)
      || (code > 0x0D && code < 0x20)
      || code = 0x7F
    else code < 0x09 || (code > 0x09 && code < 0x20) || code = 0x7F
  in
  if is_ctrl && code <> 0x0A && code <> 0x0D then
    Error.raise_lexer ~meta:(lexer_meta l) (Control_character (Char.code c))

let rec parse_multiline_literal_string l buf () =
  if is_eof l then Error.raise_lexer ~meta:(lexer_meta l) Unterminated_string;
  let c = current l in
  if c = '\'' then
    handle_multiline_quotes l buf '\'' (parse_multiline_literal_string l buf)
  else begin
    if c = '\r' then begin
      advance l;
      if peek l = Some '\n' then (
        Buffer.add_char buf '\n';
        advance l)
      else Error.raise_lexer ~meta:(lexer_meta l) Bare_carriage_return
    end
    else if Char.code c >= 0x80 then utf8_add_validated l buf
    else begin
      validate_literal_ctrl l c (Char.code c) ~multiline:true;
      Buffer.add_char buf c;
      advance l
    end;
    parse_multiline_literal_string l buf ()
  end

let rec parse_single_literal_string l buf () =
  if is_eof l then Error.raise_lexer ~meta:(lexer_meta l) Unterminated_string;
  let c = current l in
  if c = '\'' then advance l
  else if c = '\n' || c = '\r' then
    Error.raise_lexer ~meta:(lexer_meta l) Newline_in_string
  else begin
    let code = Char.code c in
    if code >= 0x80 then utf8_add_validated l buf
    else begin
      validate_literal_ctrl l c code ~multiline:false;
      Buffer.add_char buf c;
      advance l
    end;
    parse_single_literal_string l buf ()
  end

let parse_literal_string l =
  parse_quoted_string l ~triple_delim:"''"
    ~multiline_parser:parse_multiline_literal_string
    ~single_parser:parse_single_literal_string

(* Read a prefixed integer (0x/0o/0b) into [orig] (full token with prefix) and
   [num] (digits only, no underscores). Caller has already placed the '0' and
   prefix char into [orig]. [invalid_digit] names the [number_error] variant
   produced when a character violates the radix. *)
let read_prefixed_int l orig num is_valid_digit invalid_digit =
  advance l;
  advance l;
  (* skip 0x/0o/0b; prefix chars were already recorded by the caller *)
  if peek l = Some '_' then
    Error.raise_number ~meta:(lexer_meta l) Leading_underscore;
  let rec read first =
    match peek l with
    | Some c when is_valid_digit c ->
        Buffer.add_char orig c;
        Buffer.add_char num c;
        advance l;
        read false
    | Some '_' ->
        if first then
          Error.raise_number ~meta:(lexer_meta l) Underscore_not_between_digits;
        Buffer.add_char orig '_';
        advance l;
        if peek l |> Option.map is_valid_digit |> Option.value ~default:false
        then read false
        else Error.raise_number ~meta:(lexer_meta l) Trailing_underscore
    | _ -> if first then Error.raise_number ~meta:(lexer_meta l) invalid_digit
  in
  read true

let read_decimal_int l buf =
  (* Read digit sequence with _ separators into [buf]. *)
  let rec read_int first =
    match peek l with
    | Some c when is_digit c ->
        Buffer.add_char buf c;
        advance l;
        read_int false
    | Some '_' ->
        if first then
          Error.raise_number ~meta:(lexer_meta l) Underscore_not_between_digits;
        Buffer.add_char buf '_';
        advance l;
        if peek l |> Option.map is_digit |> Option.value ~default:false then
          read_int false
        else Error.raise_number ~meta:(lexer_meta l) Trailing_underscore
    | _ -> if first then Error.raise_number ~meta:(lexer_meta l) Missing_digit
  in
  read_int

let reject_leading_zero l =
  if peek l = Some '0' then
    match peek2 l with
    | Some c when is_digit c ->
        Error.raise_number ~meta:(lexer_meta l) Leading_zero
    | Some '_' -> Error.raise_number ~meta:(lexer_meta l) Leading_zero
    | _ -> ()

let read_decimal_fraction l orig read_int is_float =
  match (peek l, peek2 l) with
  | Some '.', Some c when is_digit c ->
      is_float := true;
      Buffer.add_char orig '.';
      advance l;
      read_int false
  | Some '.', _ ->
      Error.raise_number ~meta:(lexer_meta l) Missing_digit_after_decimal
  | _ -> ()

let read_exponent_sign l orig =
  match peek l with
  | Some (('+' | '-') as s) ->
      Buffer.add_char orig s;
      advance l
  | _ -> ()

let read_decimal_exponent l orig read_int is_float =
  match peek l with
  | Some (('e' | 'E') as e) ->
      is_float := true;
      Buffer.add_char orig e;
      advance l;
      read_exponent_sign l orig;
      (match peek l with
      | Some '_' ->
          Error.raise_number ~meta:(lexer_meta l) Underscore_after_exponent
      | _ -> ());
      read_int true
  | _ -> ()

let decimal_token l ~is_float s =
  let s' = String.concat "" (String.split_on_char '_' s) in
  if is_float then
    try Float (float_of_string s', s)
    with Failure _ -> Error.fail (lexer_meta l) "number out of range"
  else
    try Integer (Int64.of_string s', s)
    with Failure _ -> Error.fail (lexer_meta l) "integer out of range"

(* [orig] receives the full decimal number (with any sign already prepended by
   the caller). *)
let parse_decimal_number l orig =
  reject_leading_zero l;
  let read_int = read_decimal_int l orig in
  (match peek l with
  | Some c when is_digit c -> read_int false
  | _ -> Error.raise_number ~meta:(lexer_meta l) Missing_digit_after_sign);
  let is_float = ref false in
  read_decimal_fraction l orig read_int is_float;
  read_decimal_exponent l orig read_int is_float;
  decimal_token l ~is_float:!is_float (Buffer.contents orig)

(* [Int64.of_string] reads a radix-prefixed literal as an unsigned 64-bit value
   and wraps anything above 2^63-1 into the negative half, so it reports out of
   range only above 2^64-1. A prefixed TOML integer carries no sign, so a
   negative result is exactly a value the signed 64-bit range cannot hold. *)
let int64_of_prefix l prefix digits =
  let out_of_range () = Error.fail (lexer_meta l) "integer out of range" in
  let v =
    try Int64.of_string (prefix ^ digits) with Failure _ -> out_of_range ()
  in
  if Int64.compare v 0L < 0 then out_of_range () else v

let parse_number l =
  let orig = Buffer.create 16 in
  let neg =
    match peek l with
    | Some '-' ->
        Buffer.add_char orig '-';
        advance l;
        true
    | Some '+' ->
        Buffer.add_char orig '+';
        advance l;
        false
    | _ -> false
  in
  match peek_n l 3 with
  | Some "inf" ->
      advance_n l 3;
      Buffer.add_string orig "inf";
      let s = Buffer.contents orig in
      Float ((if neg then Float.neg_infinity else Float.infinity), s)
  | Some "nan" ->
      advance_n l 3;
      Buffer.add_string orig "nan";
      let s = Buffer.contents orig in
      Float (Float.nan, s)
  | _ -> (
      match (peek l, peek2 l) with
      | Some '0', Some 'x' when not neg ->
          Buffer.add_string orig "0x";
          let num = Buffer.create 16 in
          read_prefixed_int l orig num Ascii.is_hex_digit
            Error.Invalid_hex_digit;
          let s = Buffer.contents num in
          let o = Buffer.contents orig in
          Integer (int64_of_prefix l "0x" s, o)
      | Some '0', Some 'o' when not neg ->
          Buffer.add_string orig "0o";
          let num = Buffer.create 16 in
          read_prefixed_int l orig num is_oct_digit Error.Invalid_octal_digit;
          let s = Buffer.contents num in
          let o = Buffer.contents orig in
          Integer (int64_of_prefix l "0o" s, o)
      | Some '0', Some 'b' when not neg ->
          Buffer.add_string orig "0b";
          let num = Buffer.create 16 in
          read_prefixed_int l orig num is_bin_digit Error.Invalid_binary_digit;
          let s = Buffer.contents num in
          let o = Buffer.contents orig in
          Integer (int64_of_prefix l "0b" s, o)
      | _ -> parse_decimal_number l orig)

(* Check if we're looking at a datetime/date/time. These probe the window using
   offsets from the current [buf_pos]; they never consume bytes and rely on
   [ensure_n] to bring enough bytes into view. *)
let is_time_prefix l =
  ensure_n l 5;
  l.buf_len - l.buf_pos >= 5
  && is_digit (window_char l 0)
  && is_digit (window_char l 1)
  && window_char l 2 = ':'
  && is_digit (window_char l 3)
  && is_digit (window_char l 4)

let is_date_prefix l =
  ensure_n l 10;
  l.buf_len - l.buf_pos >= 10
  && is_digit (window_char l 0)
  && is_digit (window_char l 1)
  && is_digit (window_char l 2)
  && is_digit (window_char l 3)
  && window_char l 4 = '-'
  && is_digit (window_char l 5)
  && is_digit (window_char l 6)
  && window_char l 7 = '-'
  && is_digit (window_char l 8)
  && is_digit (window_char l 9)

let is_date_delimiter = function
  | 'T' | 't' | '\n' | '\r' | '#' | ',' | ']' | '}' -> true
  | _ -> false

let date_suffix_after_space l =
  (* After whitespace, look for '=' to tell key apart from datetime. Grow the
     window until we find a decisive byte. *)
  let off = ref 11 in
  let done_ = ref false in
  let is_eq = ref false in
  while not !done_ do
    ensure_n l (!off + 1);
    if not (window_has l !off) then done_ := true
    else
      match window_char l !off with
      | ' ' | '\t' -> incr off
      | '=' ->
          is_eq := true;
          done_ := true
      | _ -> done_ := true
  done;
  if !is_eq then `Other else `Date

let check_date_suffix l =
  (* Look past the YYYY-MM-DD prefix (offset 10 from buf_pos) to decide whether
     we're looking at a date value or a bare key. *)
  ensure_n l 11;
  if not (window_has l 10) then `Date
  else
    let next = window_char l 10 in
    if is_date_delimiter next then `Date
    else if next = ' ' || next = '\t' then date_suffix_after_space l
    else if is_bare_key_char next then `Other
    else `Date

let looks_like_datetime l =
  if is_date_prefix l then check_date_suffix l
  else if is_time_prefix l then `Time
  else `Other

(* Date/time validation. [meta] points at the token being validated so range
   errors report against the offending date/time/offset. *)
let validate_date ~meta year month day =
  if month < 1 || month > 12 then Error.raise_datetime ~meta (Month month);
  if day < 1 then Error.raise_datetime ~meta (Day (day, 0));
  let days_in_month = [| 0; 31; 28; 31; 30; 31; 30; 31; 31; 30; 31; 30; 31 |] in
  let is_leap = (year mod 4 = 0 && year mod 100 <> 0) || year mod 400 = 0 in
  let max_days = if month = 2 && is_leap then 29 else days_in_month.(month) in
  if day > max_days then Error.raise_datetime ~meta (Day (day, month))

let validate_time ~meta hour minute second =
  if hour < 0 || hour > 23 then Error.raise_datetime ~meta (Hour hour);
  if minute < 0 || minute > 59 then Error.raise_datetime ~meta (Minute minute);
  if second < 0 || second > 60 then (* 60 for leap second *)
    Error.raise_datetime ~meta (Second second)

let validate_offset ~meta hour minute =
  if hour < 0 || hour > 23 then
    Error.raise_datetime ~meta (Timezone_offset_hour hour);
  if minute < 0 || minute > 59 then
    Error.raise_datetime ~meta (Timezone_offset_minute minute)

let read_2_digits l buf secondary_buf fmt_desc =
  for _ = 1 to 2 do
    match peek l with
    | Some c when is_digit c ->
        Buffer.add_char buf c;
        Buffer.add_char secondary_buf c;
        advance l
    | _ -> Error.raise_datetime ~meta:(lexer_meta l) (Format fmt_desc)
  done

let read_optional_seconds_frac l buf second_buf =
  match peek l with
  | Some ':' -> (
      Buffer.add_char buf ':';
      advance l;
      read_2_digits l buf second_buf "time";
      (* Optional fractional seconds *)
      match peek l with
      | Some '.' ->
          Buffer.add_char buf '.';
          advance l;
          if not (peek l |> Option.map is_digit |> Option.value ~default:false)
          then
            Error.raise_number ~meta:(lexer_meta l) Missing_digit_after_decimal;
          while peek l |> Option.map is_digit |> Option.value ~default:false do
            Buffer.add_char buf (Option.get (peek l));
            advance l
          done
      | _ -> ())
  | _ ->
      (* No seconds - add :00 for normalization *)
      Buffer.add_string buf ":00";
      Buffer.add_string second_buf "00"

let read_date_part l buf =
  (* Read YYYY-MM-DD into buf, validate date components. *)
  let start_meta = lexer_meta l in
  let year_buf = Buffer.create 4 in
  let month_buf = Buffer.create 2 in
  let day_buf = Buffer.create 2 in
  for _ = 1 to 4 do
    match peek l with
    | Some c when is_digit c ->
        Buffer.add_char buf c;
        Buffer.add_char year_buf c;
        advance l
    | _ -> Error.raise_datetime ~meta:(lexer_meta l) (Format "date")
  done;
  if peek l <> Some '-' then
    Error.raise_datetime ~meta:(lexer_meta l) (Format "date");
  Buffer.add_char buf '-';
  advance l;
  read_2_digits l buf month_buf "date";
  if peek l <> Some '-' then
    Error.raise_datetime ~meta:(lexer_meta l) (Format "date");
  Buffer.add_char buf '-';
  advance l;
  read_2_digits l buf day_buf "date";
  let year = int_of_string (Buffer.contents year_buf) in
  let month = int_of_string (Buffer.contents month_buf) in
  let day = int_of_string (Buffer.contents day_buf) in
  validate_date ~meta:start_meta year month day

let read_tz_offset l buf =
  (* Parse +HH:MM or -HH:MM offset, validate and append to buf *)
  let start_meta = lexer_meta l in
  let sign = current l in
  let off_hour_buf = Buffer.create 2 in
  let off_min_buf = Buffer.create 2 in
  Buffer.add_char buf sign;
  advance l;
  read_2_digits l buf off_hour_buf "timezone";
  if peek l <> Some ':' then
    Error.raise_datetime ~meta:(lexer_meta l) (Format "timezone");
  Buffer.add_char buf ':';
  advance l;
  read_2_digits l buf off_min_buf "timezone";
  let off_hour = int_of_string (Buffer.contents off_hour_buf) in
  let off_min = int_of_string (Buffer.contents off_min_buf) in
  validate_offset ~meta:start_meta off_hour off_min

let read_time_and_offset l buf =
  (* Read HH:MM[:SS[.frac]] and optional offset, return datetime token *)
  let time_meta = lexer_meta l in
  let hour_buf = Buffer.create 2 in
  let minute_buf = Buffer.create 2 in
  let second_buf = Buffer.create 2 in
  Buffer.add_char buf 'T';
  (* normalize to uppercase T *)
  read_2_digits l buf hour_buf "time";
  if peek l <> Some ':' then
    Error.raise_datetime ~meta:(lexer_meta l) (Format "time");
  Buffer.add_char buf ':';
  advance l;
  read_2_digits l buf minute_buf "time";
  read_optional_seconds_frac l buf second_buf;
  let hour = int_of_string (Buffer.contents hour_buf) in
  let minute = int_of_string (Buffer.contents minute_buf) in
  let second =
    if Buffer.length second_buf > 0 then
      int_of_string (Buffer.contents second_buf)
    else 0
  in
  validate_time ~meta:time_meta hour minute second;
  match peek l with
  | Some 'Z' | Some 'z' ->
      Buffer.add_char buf 'Z';
      advance l;
      Datetime (Buffer.contents buf)
  | Some '+' | Some '-' ->
      read_tz_offset l buf;
      Datetime (Buffer.contents buf)
  | _ -> Datetime_local (Buffer.contents buf)

let parse_datetime l =
  let buf = Buffer.create 32 in
  read_date_part l buf;
  match peek l with
  | Some 'T' | Some 't' ->
      advance l;
      read_time_and_offset l buf
  | Some ' ' ->
      (* Space could be followed by time (datetime with space separator) or
         could be end of date (local date followed by comment/value). Peek at
         the byte after the space before consuming it, so we never need to
         rewind in the streaming lexer. *)
      begin match peek2 l with
      | Some c when is_digit c ->
          advance l;
          read_time_and_offset l buf
      | _ -> Date_local (Buffer.contents buf)
      end
  | _ -> Date_local (Buffer.contents buf)

let parse_time l =
  let buf = Buffer.create 16 in
  let time_meta = lexer_meta l in
  let hour_buf = Buffer.create 2 in
  let minute_buf = Buffer.create 2 in
  let second_buf = Buffer.create 2 in
  (* Read HH:MM *)
  read_2_digits l buf hour_buf "time";
  if peek l <> Some ':' then
    Error.raise_datetime ~meta:(lexer_meta l) (Format "time");
  Buffer.add_char buf ':';
  advance l;
  read_2_digits l buf minute_buf "time";
  read_optional_seconds_frac l buf second_buf;
  (* Validate time *)
  let hour = int_of_string (Buffer.contents hour_buf) in
  let minute = int_of_string (Buffer.contents minute_buf) in
  let second =
    if Buffer.length second_buf > 0 then
      int_of_string (Buffer.contents second_buf)
    else 0
  in
  validate_time ~meta:time_meta hour minute second;
  Time_local (Buffer.contents buf)

(* The following classifiers look ahead into the lookahead window, growing it as
   needed, to decide whether a token is a bare key or a number/date. They never
   consume input. Offsets are relative to [buf_pos]. *)

let ws_followed_by_eq l off =
  let rec skip o =
    ensure_n l (o + 1);
    if not (window_has l o) then false
    else
      match window_char l o with
      | ' ' | '\t' -> skip (o + 1)
      | '=' -> true
      | _ -> false
  in
  skip off

let signed_digit_is_key_context l =
  (* Determine if a signed-digit sequence like -01 is a key rather than a
     number. The sign is at offset 0; scanning starts at offset 1. *)
  let rec scan_ahead o =
    ensure_n l (o + 1);
    if not (window_has l o) then false
    else
      let c = window_char l o in
      if is_digit c || c = '_' then scan_ahead (o + 1)
      else if c = ' ' || c = '\t' then ws_followed_by_eq l (o + 1)
      else if c = '=' then true
      else if c = '.' then begin
        ensure_n l (o + 2);
        window_has l (o + 1)
        &&
        let next = window_char l (o + 1) in
        (not (is_digit next)) && is_bare_key_char next
      end
      else if c = 'e' || c = 'E' then false
      else is_bare_key_char c
  in
  scan_ahead 1

let after_e_or_capital_e l ~o has_dash =
  ensure_n l (o + 2);
  if not (window_has l (o + 1)) then true
  else
    let next = window_char l (o + 1) in
    if is_digit next then has_dash
    else if next = '+' || next = '-' then begin
      ensure_n l (o + 3);
      if window_has l (o + 2) && is_digit (window_char l (o + 2)) then has_dash
      else true
    end
    else true

let after_dash_in_digit l ~o ~scan has_dash =
  ensure_n l (o + 2);
  if not (window_has l (o + 1)) then has_dash
  else
    let next = window_char l (o + 1) in
    if is_digit next then scan (o + 1) true
    else is_bare_key_char next || has_dash

let digit_is_bare_key l =
  (* Scan from current buf_pos to determine if a digit-led token is a bare key
     vs number. *)
  let rec scan o has_dash =
    ensure_n l (o + 1);
    if not (window_has l o) then has_dash
    else
      match window_char l o with
      | c when is_digit c || c = '_' || c = '.' -> scan (o + 1) has_dash
      | '-' -> after_dash_in_digit l ~o ~scan has_dash
      | 'e' | 'E' -> after_e_or_capital_e l ~o has_dash
      | c -> is_bare_key_char c || has_dash
  in
  ensure_n l 2;
  let has_leading_zero =
    window_char l 0 = '0' && window_has l 1 && is_digit (window_char l 1)
  in
  has_leading_zero || scan 0 false

(* Consume a run of bare-key characters, accumulating into [buf]. *)
let read_bare_key_into l buf =
  let continue = ref true in
  while !continue do
    match peek l with
    | Some c when is_bare_key_char c ->
        Buffer.add_char buf c;
        advance l
    | _ -> continue := false
  done

let lex_signed_token l =
  let sign = current l in
  match peek2 l with
  | Some d when is_digit d ->
      if signed_digit_is_key_context l then begin
        let buf = Buffer.create 16 in
        read_bare_key_into l buf;
        Bare_key (Buffer.contents buf)
      end
      else parse_number l
  | Some 'i' ->
      let s = Fmt.str "%cinf" sign in
      if peek_n l 4 = Some s then begin
        advance_n l 4;
        if sign = '-' then Float (Float.neg_infinity, s)
        else Float (Float.infinity, s)
      end
      else if sign = '-' then begin
        let buf = Buffer.create 16 in
        read_bare_key_into l buf;
        Bare_key (Buffer.contents buf)
      end
      else Error.raise_lexer ~meta:(lexer_meta l) (Unexpected_character sign)
  | Some 'n' ->
      let s = Fmt.str "%cnan" sign in
      if peek_n l 4 = Some s then begin
        advance_n l 4;
        Float (Float.nan, s)
      end
      else if sign = '-' then begin
        let buf = Buffer.create 16 in
        read_bare_key_into l buf;
        Bare_key (Buffer.contents buf)
      end
      else Error.raise_lexer ~meta:(lexer_meta l) (Unexpected_character sign)
  | _ when sign = '-' ->
      let buf = Buffer.create 16 in
      read_bare_key_into l buf;
      Bare_key (Buffer.contents buf)
  | _ -> Error.raise_lexer ~meta:(lexer_meta l) (Unexpected_character sign)

let lex_digit_token l =
  match looks_like_datetime l with
  | `Date -> parse_datetime l
  | `Time -> parse_time l
  | `Other ->
      ensure_n l 2;
      let is_prefixed_number =
        window_has l 1
        && window_char l 0 = '0'
        &&
        let c1 = window_char l 1 in
        c1 = 'x' || c1 = 'X' || c1 = 'o' || c1 = 'O' || c1 = 'b' || c1 = 'B'
      in
      if is_prefixed_number then parse_number l
      else if digit_is_bare_key l then begin
        let buf = Buffer.create 16 in
        read_bare_key_into l buf;
        Bare_key (Buffer.contents buf)
      end
      else parse_number l

(* Read a single token. Whitespace and comments must be skipped by the caller;
   the returned token's span is exactly the bytes consumed by this call. *)
let next_token l =
  if is_eof l then Eof
  else begin
    let c = current l in
    match c with
    | '[' ->
        advance l;
        Lbracket
    | ']' ->
        advance l;
        Rbracket
    | '{' ->
        advance l;
        Lbrace
    | '}' ->
        advance l;
        Rbrace
    | '=' ->
        advance l;
        Equals
    | ',' ->
        advance l;
        Comma
    | '.' ->
        advance l;
        Dot
    | '\n' ->
        advance l;
        Newline
    | '\r' ->
        advance l;
        if peek l = Some '\n' then begin
          advance l;
          Newline
        end
        else Error.raise_lexer ~meta:(lexer_meta l) Bare_carriage_return
    | '"' ->
        let s, multiline = parse_basic_string l in
        if multiline then Ml_basic_string s else Basic_string s
    | '\'' ->
        let s, multiline = parse_literal_string l in
        if multiline then Ml_literal_string s else Literal_string s
    | '+' | '-' -> lex_signed_token l
    | c when is_digit c -> lex_digit_token l
    | c when c = 't' || c = 'f' || c = 'i' || c = 'n' ->
        let buf = Buffer.create 16 in
        read_bare_key_into l buf;
        Bare_key (Buffer.contents buf)
    | c when is_bare_key_char c ->
        let buf = Buffer.create 16 in
        read_bare_key_into l buf;
        Bare_key (Buffer.contents buf)
    | c ->
        let code = Char.code c in
        if code < 0x20 || code = 0x7F then
          Error.raise_lexer ~meta:(lexer_meta l) (Control_character code)
        else Error.raise_lexer ~meta:(lexer_meta l) (Unexpected_character c)
  end

(* Parser *)

type t = {
  lexer : lexer;
  mutable current : token;
  mutable current_meta : Loc.Meta.t;
  mutable peeked : bool;
  max_depth : int;
  max_nodes : int;
  mutable depth : int;
  mutable nodes : int;
  mutable document_meta : Loc.Meta.t;
      (* Meta covering the document so far, used by whole-document limit errors.
         Starts at [Loc.Meta.none] until the first token is peeked, then widens
         to include each consumed token. *)
}

let default_max_depth = 100
let default_max_nodes = 10_000_000

let v ?(max_depth = default_max_depth) ?(max_nodes = default_max_nodes) lexer =
  {
    lexer;
    current = Eof;
    current_meta = Loc.Meta.none;
    peeked = false;
    max_depth;
    max_nodes;
    depth = 0;
    nodes = 0;
    document_meta = Loc.Meta.none;
  }

let limits_enter p =
  p.depth <- p.depth + 1;
  if p.depth > p.max_depth then
    Loc.Error.failf p.document_meta "max depth exceeded (limit: %d)" p.max_depth

let limits_leave p = p.depth <- p.depth - 1

let limits_bump_node p =
  p.nodes <- p.nodes + 1;
  if p.nodes > p.max_nodes then
    Loc.Error.failf p.document_meta "max nodes exceeded (limit: %d)" p.max_nodes

(* Span a [Loc.Meta.t] from the lexer's pre-token position to its current
   post-token position. *)
let token_meta l ~start_byte ~start_line ~start_line_byte =
  let last_byte =
    if Loc.compare_byte_pos l.byte start_byte > 0 then l.byte - 1
    else start_byte
  in
  let loc =
    Loc.v ~file:l.file ~first_byte:start_byte ~last_byte
      ~first_line_num:start_line ~first_line_byte:start_line_byte
      ~last_line_num:l.line ~last_line_byte:l.line_byte
  in
  Loc.Meta.v loc

(* Union two metas into a single source span. If either is [Loc.Meta.none],
   returns the other. Whitespace captured in [first] is preserved. *)
let span_meta first last =
  if Loc.Meta.is_none first then last
  else if Loc.Meta.is_none last then first
  else
    let loc = Loc.span (Loc.Meta.loc first) (Loc.Meta.loc last) in
    Loc.Meta.with_loc first loc

let string_of_token = function
  | Lbracket -> "["
  | Rbracket -> "]"
  | Lbrace -> "{"
  | Rbrace -> "}"
  | Equals -> "="
  | Comma -> ","
  | Dot -> "."
  | Newline -> "newline"
  | Eof -> "end of input"
  | Bare_key s -> Fmt.str "bare key %S" s
  | Basic_string s -> Fmt.str "basic string %S" s
  | Literal_string s -> Fmt.str "literal string %S" s
  | Ml_basic_string _ -> "multiline basic string"
  | Ml_literal_string _ -> "multiline literal string"
  | Integer (_, s) -> Fmt.str "integer %s" s
  | Float (_, s) -> Fmt.str "float %s" s
  | Datetime s -> Fmt.str "datetime %s" s
  | Datetime_local s -> Fmt.str "local datetime %s" s
  | Date_local s -> Fmt.str "local date %s" s
  | Time_local s -> Fmt.str "local time %s" s

let peek_token p =
  if not p.peeked then begin
    skip_ws_and_comments p.lexer;
    let start_byte = p.lexer.byte in
    let start_line = p.lexer.line in
    let start_line_byte = p.lexer.line_byte in
    let tok = next_token p.lexer in
    p.current <- tok;
    p.current_meta <-
      token_meta p.lexer ~start_byte ~start_line ~start_line_byte;
    p.document_meta <- span_meta p.document_meta p.current_meta;
    p.peeked <- true
  end;
  p.current

let consume_token p =
  let tok = peek_token p in
  p.peeked <- false;
  tok

(* Check if next raw character (without skipping whitespace) matches *)
let next_raw_char_is p c = peek p.lexer = Some c

let expect_token p expected =
  let tok = consume_token p in
  if tok <> expected then
    let what =
      match expected with
      | Equals -> "="
      | Rbracket -> "]"
      | Rbrace -> "}"
      | Newline -> "newline"
      | _ -> "token"
    in
    Error.raise_syntax ~meta:p.current_meta (Expected what)

let skip_newlines p =
  while peek_token p = Newline do
    ignore (consume_token p)
  done

(* Parse a single key segment (bare, basic string, literal string, or integer).
   Returns a list of [V.name] values: one key usually, several when a
   float-shaped token has an embedded dot. All segments from one token share
   the token's meta. *)
(* Note: Float is handled specially in parse_dotted_key *)
let parse_key_segment p =
  let tok = peek_token p in
  let meta = p.current_meta in
  let tag s = (s, meta) in
  match tok with
  | Bare_key s ->
      ignore (consume_token p);
      [ tag s ]
  | Basic_string s ->
      ignore (consume_token p);
      [ tag s ]
  | Literal_string s ->
      ignore (consume_token p);
      [ tag s ]
  | Integer (_i, orig_str) ->
      ignore (consume_token p);
      [ tag orig_str ]
  | Float (f, orig_str) ->
      (* Float in key context - use original string to preserve exact key
         parts *)
      ignore (consume_token p);
      if Float.is_nan f then [ tag "nan" ]
      else if f = Float.infinity then [ tag "inf" ]
      else if f = Float.neg_infinity then [ tag "-inf" ]
      else begin
        (* Remove underscores from original string and split on dot *)
        let s = String.concat "" (String.split_on_char '_' orig_str) in
        if String.contains s 'e' || String.contains s 'E' then [ tag s ]
        else if String.contains s '.' then
          List.map tag (String.split_on_char '.' s)
        else [ tag s ]
      end
  | Date_local s ->
      ignore (consume_token p);
      [ tag s ]
  | Datetime s ->
      ignore (consume_token p);
      [ tag s ]
  | Datetime_local s ->
      ignore (consume_token p);
      [ tag s ]
  | Time_local s ->
      ignore (consume_token p);
      [ tag s ]
  | Ml_basic_string _ -> Error.raise_semantic ~meta:p.current_meta Multiline_key
  | Ml_literal_string _ ->
      Error.raise_semantic ~meta:p.current_meta Multiline_key
  | _ -> Error.raise_syntax ~meta:p.current_meta (Expected "key")

(* Parse a dotted key - returns list of names. *)
(* An [n]-segment dotted key nests [n - 1] tables, exactly what the same
   structure costs written as inline tables. Counting as the key is read
   refuses an absurd one before its list is built. *)
let parse_dotted_key p =
  let count = ref 0 in
  let add keys =
    count := !count + List.length keys;
    if p.depth + !count - 1 > p.max_depth then
      Loc.Error.failf p.document_meta "max depth exceeded (limit: %d)"
        p.max_depth
  in
  let first_keys = parse_key_segment p in
  add first_keys;
  let rec loop acc =
    match peek_token p with
    | Dot ->
        ignore (consume_token p);
        let keys = parse_key_segment p in
        add keys;
        loop (List.rev_append keys acc)
    | _ -> List.rev acc
  in
  let rest = loop [] in
  first_keys @ rest

(* The key's own nesting holds while its value is read, so what the value nests
   inside it counts against the same limit. *)
let with_key_depth p keys f =
  let n = List.length keys - 1 in
  p.depth <- p.depth + n;
  let v = f () in
  p.depth <- p.depth - n;
  v

let is_digit_char c = c >= '0' && c <= '9'

let validate_number_edge_underscores ~meta str len =
  if len > 0 && str.[0] = '_' then Error.raise_number ~meta Leading_underscore;
  if len > 0 && str.[len - 1] = '_' then
    Error.raise_number ~meta Trailing_underscore

let underscore_between_digits ~has_hex_prefix prev next =
  match (prev, next) with
  | Some p, Some n
    when has_hex_prefix && Ascii.is_hex_digit p && Ascii.is_hex_digit n ->
      true
  | Some p, Some n when is_digit_char p && is_digit_char n -> true
  | _ -> false

let validate_number_underscore_at ~meta str ~has_hex_prefix i =
  if str.[i] = '_' && str.[i + 1] = '_' then
    Error.raise_number ~meta Double_underscore;
  if str.[i] = '_' then
    let prev = if i > 0 then Some str.[i - 1] else None in
    let next = Some str.[i + 1] in
    if not (underscore_between_digits ~has_hex_prefix prev next) then
      Error.raise_number ~meta Underscore_not_between_digits

let validate_number_underscores ~meta str =
  let len = String.length str in
  validate_number_edge_underscores ~meta str len;
  let has_hex_prefix =
    len > 2 && str.[0] = '0' && (str.[1] = 'x' || str.[1] = 'X')
  in
  for i = 0 to len - 2 do
    validate_number_underscore_at ~meta str ~has_hex_prefix i
  done

let validate_bare_number_prefix ~meta original s len =
  if len = 0 then Error.raise_syntax ~meta (Unexpected_bare_key original);
  let c0 = s.[0] in
  if not (is_digit_char c0) then
    Error.raise_syntax ~meta (Unexpected_bare_key original);
  if len > 1 && c0 = '0' && is_digit_char s.[1] then
    Error.raise_number ~meta Leading_zero

let bare_number_is_float s =
  String.contains s '.' || String.contains s 'e' || String.contains s 'E'

let bare_key_as_number ~meta s =
  validate_number_underscores ~meta s;
  let s_no_underscore = String.concat "" (String.split_on_char '_' s) in
  let len = String.length s_no_underscore in
  validate_bare_number_prefix ~meta s s_no_underscore len;
  try
    if bare_number_is_float s_no_underscore then
      V.Float (float_of_string s_no_underscore, meta)
    else V.Int (Int64.of_string s_no_underscore, meta)
  with Failure _ -> Error.raise_syntax ~meta (Unexpected_bare_key s)

(* The members of one inline table, under construction.

   [by_name] holds each member under its name, so a duplicate key, or a dotted
   key naming a member already read, is found without rescanning the members
   read so far. [order] records the names in reverse order of first mention:
   each name is pushed once, when the document first names it, so reversing the
   list is the order the document reads in.

   A member is either the value its key was given directly or the table a
   dotted key opened under it, and which of the two it is decides what a later
   entry naming it again may do. Those sub-tables accumulate exactly the same
   way, so a member re-opened by a later dotted key is extended in place rather
   than rebuilt from its old members, and the width of an inline table costs
   time linear in its members at every level. *)
type inline_table = {
  by_name : (string, inline_member) Hashtbl.t;
  mutable order : string list;
}

and inline_member = { name : V.name; body : inline_body }

and inline_body =
  | Direct of V.t  (** the value the key was given directly *)
  | Dotted of Loc.Meta.t * inline_table  (** what dotted keys opened under it *)

let inline_table () = { by_name = Hashtbl.create 16; order = [] }

let add_inline_member t name member =
  Hashtbl.replace t.by_name name member;
  t.order <- name :: t.order

let skip_ws _p =
  (* Skip whitespace in token stream - handled by lexer but needed for
     lookahead *)
  ()

(* The meta the table under the head of [keys] carries: the span from that key
   to whichever of the next key and the value extends farthest right, taken
   down the whole chain. [keys] holds at least two elements; the value's own
   meta answers for a shorter one. *)
let rec dotted_table_meta keys value =
  match keys with
  | (_, k_meta) :: ((_, child_meta) :: _ as rest) ->
      let inner =
        match rest with
        | [ _ ] -> V.meta value
        | _ -> dotted_table_meta rest value
      in
      span_meta (span_meta k_meta child_meta) inner
  | _ -> V.meta value

(* Add [value] under the dotted key [keys] to a table a dotted key opened.
   Every clash below the inline table's own members is a plain conflict: the
   duplicate-key and cannot-extend refusals are about a member of the inline
   table itself and belong to [insert_inline_entry]. *)
let rec insert_dotted t keys value =
  match keys with
  | [] -> Error.raise_semantic ~meta:(V.meta value) Empty_key
  | [ ((name, _) as kn) ] -> (
      match Hashtbl.find_opt t.by_name name with
      | Some _ -> Error.raise_semantic ~meta:(V.meta value) Conflicting_keys
      | None -> add_inline_member t name { name = kn; body = Direct value })
  | ((name, _) as kn) :: rest -> (
      match Hashtbl.find_opt t.by_name name with
      | Some { body = Dotted (_, sub); _ } -> insert_dotted sub rest value
      | Some { body = Direct _; _ } ->
          Error.raise_semantic
            ~meta:(dotted_table_meta keys value)
            Conflicting_keys
      | None ->
          let sub = inline_table () in
          add_inline_member t name
            { name = kn; body = Dotted (dotted_table_meta keys value, sub) };
          insert_dotted sub rest value)

(* Add one entry of an inline table. A name the document gave a value directly
   can be given no second value ([Duplicate_key]) and cannot be opened by a
   dotted key ([Cannot_extend_inline_table]); both are reported at [meta], the
   entry that tried it. *)
let insert_inline_entry ~meta t keys value =
  match keys with
  | [] -> Error.raise_semantic ~meta:(V.meta value) Empty_key
  | [ ((name, _) as kn) ] -> (
      match Hashtbl.find_opt t.by_name name with
      | Some { body = Direct _; _ } ->
          Error.raise_semantic ~meta (Duplicate_key name)
      | Some { body = Dotted _; _ } ->
          Error.raise_semantic ~meta:(V.meta value) Conflicting_keys
      | None -> add_inline_member t name { name = kn; body = Direct value })
  | ((name, _) as kn) :: rest -> (
      match Hashtbl.find_opt t.by_name name with
      | Some { body = Direct _; _ } ->
          Error.raise_semantic ~meta (Cannot_extend_inline_table name)
      | Some { body = Dotted (_, sub); _ } -> insert_dotted sub rest value
      | None ->
          let sub = inline_table () in
          add_inline_member t name
            { name = kn; body = Dotted (dotted_table_meta keys value, sub) };
          insert_dotted sub rest value)

(* The members of [t] as a [Value.t] association list, in the order the
   document names them: [order] is newest first and holds each name once, so
   folding it while prepending reverses it back into source order. *)
let rec inline_members t =
  List.fold_left
    (fun members name ->
      let m = Hashtbl.find t.by_name name in
      (m.name, inline_member_value m) :: members)
    [] t.order

and inline_member_value m =
  match m.body with
  | Direct v -> v
  | Dotted (meta, sub) -> V.Table (inline_members sub, meta)

let rec parse_value p =
  limits_bump_node p;
  let tok = peek_token p in
  let meta = p.current_meta in
  match tok with
  | Basic_string s ->
      ignore (consume_token p);
      V.String (s, meta)
  | Literal_string s ->
      ignore (consume_token p);
      V.String (s, meta)
  | Ml_basic_string s ->
      ignore (consume_token p);
      V.String (s, meta)
  | Ml_literal_string s ->
      ignore (consume_token p);
      V.String (s, meta)
  | Integer (i, _) ->
      ignore (consume_token p);
      V.Int (i, meta)
  | Float (f, _) ->
      ignore (consume_token p);
      V.Float (f, meta)
  | Datetime s ->
      ignore (consume_token p);
      V.Datetime (s, meta)
  | Datetime_local s ->
      ignore (consume_token p);
      V.Datetime_local (s, meta)
  | Date_local s ->
      ignore (consume_token p);
      V.Date_local (s, meta)
  | Time_local s ->
      ignore (consume_token p);
      V.Time_local (s, meta)
  | Lbracket -> parse_array p
  | Lbrace -> parse_inline_table p
  | Bare_key s -> (
      ignore (consume_token p);
      match s with
      | "true" -> Bool (true, meta)
      | "false" -> Bool (false, meta)
      | "inf" -> Float (Float.infinity, meta)
      | "nan" -> Float (Float.nan, meta)
      | _ -> bare_key_as_number ~meta s)
  | _ -> Error.raise_syntax ~meta:p.current_meta (Expected "value")

and parse_array p =
  let start_meta = p.current_meta in
  ignore (consume_token p);
  (* [ *)
  limits_enter p;
  skip_newlines p;
  let rec loop acc =
    match peek_token p with
    | Rbracket ->
        let end_meta = p.current_meta in
        ignore (consume_token p);
        limits_leave p;
        V.Array (List.rev acc, span_meta start_meta end_meta)
    | _ -> (
        let v = parse_value p in
        skip_newlines p;
        match peek_token p with
        | Comma ->
            ignore (consume_token p);
            skip_newlines p;
            loop (v :: acc)
        | Rbracket ->
            let end_meta = p.current_meta in
            ignore (consume_token p);
            limits_leave p;
            V.Array (List.rev (v :: acc), span_meta start_meta end_meta)
        | _ ->
            Error.raise_syntax ~meta:p.current_meta
              (Expected "',' or ']' in array"))
  in
  loop []

and parse_inline_entry p acc =
  let keys = parse_dotted_key p in
  skip_ws p;
  expect_token p Equals;
  skip_ws p;
  let v = with_key_depth p keys (fun () -> parse_value p) in
  insert_inline_entry ~meta:p.current_meta acc keys v

and parse_inline_table p =
  let start_meta = p.current_meta in
  ignore (consume_token p);
  (* { *)
  limits_enter p;
  skip_newlines p;
  let acc = inline_table () in
  let rec loop () =
    match peek_token p with
    | Rbrace ->
        let end_meta = p.current_meta in
        ignore (consume_token p);
        limits_leave p;
        V.Table (inline_members acc, span_meta start_meta end_meta)
    | _ -> (
        parse_inline_entry p acc;
        skip_newlines p;
        match peek_token p with
        | Comma ->
            ignore (consume_token p);
            skip_newlines p;
            loop ()
        | Rbrace ->
            let end_meta = p.current_meta in
            ignore (consume_token p);
            limits_leave p;
            V.Table (inline_members acc, span_meta start_meta end_meta)
        | _ ->
            Error.raise_syntax ~meta:p.current_meta
              (Expected "',' or '}' in inline table"))
  in
  loop ()

(* The [validate_*_string] helpers re-validate dates/times encoded as strings
   inside parsed TOML values, from the Tagged_json encoder path (TOML -> tagged
   JSON, used by the toml-test harness). Callers pass the source [?meta] of the
   value carrying the string so range errors can point back at the offending
   literal; it defaults to [Loc.Meta.none] for the rare call site without lexer
   context. *)
let validate_datetime_string ?(meta = Loc.Meta.none) s =
  if String.length s >= 10 then begin
    let year = int_of_string (String.sub s 0 4) in
    let month = int_of_string (String.sub s 5 2) in
    let day = int_of_string (String.sub s 8 2) in
    validate_date ~meta year month day;
    if String.length s >= 16 then begin
      let time_start =
        if s.[10] = 'T' || s.[10] = 't' || s.[10] = ' ' then 11 else 10
      in
      let hour = int_of_string (String.sub s time_start 2) in
      let minute = int_of_string (String.sub s (time_start + 3) 2) in
      let second =
        if String.length s >= time_start + 8 && s.[time_start + 5] = ':' then
          int_of_string (String.sub s (time_start + 6) 2)
        else 0
      in
      validate_time ~meta hour minute second
    end
  end

let validate_date_string ?(meta = Loc.Meta.none) s =
  if String.length s >= 10 then begin
    let year = int_of_string (String.sub s 0 4) in
    let month = int_of_string (String.sub s 5 2) in
    let day = int_of_string (String.sub s 8 2) in
    validate_date ~meta year month day
  end

let validate_time_string ?(meta = Loc.Meta.none) s =
  if String.length s >= 5 then begin
    let hour = int_of_string (String.sub s 0 2) in
    let minute = int_of_string (String.sub s 3 2) in
    let second =
      if String.length s >= 8 && s.[5] = ':' then
        int_of_string (String.sub s 6 2)
      else 0
    in
    validate_time ~meta hour minute second
  end

(* Table management for the parser.

   A [table_state] accumulates the key-value pairs and subtables seen across
   multiple [section] / [section.sub] / dotted-key assignments, then is
   finalised into a [Value.t] by [toml_of_table_state]. We track source metas
   alongside the data so the finalised [Value.t] nodes carry real locations:

   - [values] stores keys as [V.name] so each key's source meta is preserved,
   and [value_names] indexes those names so a duplicate key is caught without
   rescanning them. - [subtable_metas] records the meta of the key under which each subtable was
   first introduced (the subtable itself has no "key" separate from its parent's
   map entry). - [header_meta] covers the [section] / [[array-of-tables]] header
   text for top-level tables; [Loc.Meta.none] for the root and for inline tables
   built via [{ k = v }] (whose span is computed separately). - [last_meta] is
   widened each time a value or subtable is added so the finalised [Table]
   node's meta spans from the header (or first member) to the last byte of the
   last member. *)
type table_state = {
  mutable values : (V.name * V.t) list;
  value_names : (string, unit) Hashtbl.t;
  subtables : (string, table_state) Hashtbl.t;
  subtable_metas : (string, Loc.Meta.t) Hashtbl.t;
  mutable is_array : bool;
  is_inline : bool;
  mutable defined : bool;
      (* Has this table been explicitly defined with [table]? *)
  mutable closed : bool; (* Closed to extension via dotted keys from parent *)
  mutable array_elements : table_state list; (* For arrays of tables *)
  mutable header_meta : Loc.Meta.t;
  mutable last_meta : Loc.Meta.t;
}

let table_state () =
  {
    values = [];
    value_names = Hashtbl.create 16;
    subtables = Hashtbl.create 16;
    subtable_metas = Hashtbl.create 16;
    is_array = false;
    is_inline = false;
    defined = false;
    closed = false;
    array_elements = [];
    header_meta = Loc.Meta.none;
    last_meta = Loc.Meta.none;
  }

(* Widen [state.last_meta] to cover [m]; [Loc.Meta.none] contributions are
   ignored. *)
let widen_last state m =
  if not (Loc.Meta.is_none m) then
    state.last_meta <- span_meta state.last_meta m

(* Record the meta of the key that introduced [k] as a subtable in
   [state.subtable_metas] (first-seen wins). *)
let remember_subtable_key state k meta =
  if not (Hashtbl.mem state.subtable_metas k) then
    Hashtbl.add state.subtable_metas k meta

let rec ensure_table ~meta state keys create_intermediate =
  match keys with
  | [] -> state
  | [ k ] -> (
      (* Check if key exists as a value *)
      if Hashtbl.mem state.value_names k then
        Error.raise_semantic ~meta (Cannot_use_value_as_table k);
      match Hashtbl.find_opt state.subtables k with
      | Some sub -> sub
      | None ->
          let sub = table_state () in
          Hashtbl.add state.subtables k sub;
          remember_subtable_key state k meta;
          sub)
  | k :: rest ->
      (* Check if key exists as a value *)
      if Hashtbl.mem state.value_names k then
        Error.raise_semantic ~meta (Cannot_use_value_as_table k);
      let sub =
        match Hashtbl.find_opt state.subtables k with
        | Some sub -> sub
        | None ->
            let sub = table_state () in
            Hashtbl.add state.subtables k sub;
            remember_subtable_key state k meta;
            sub
      in
      if create_intermediate && not sub.defined then sub.defined <- false;
      (* Mark as implicitly defined *)
      ensure_table ~meta sub rest create_intermediate

(* Like ensure_table but marks tables as defined (for dotted keys). Dotted keys
   mark tables as "defined" (can't re-define with [table]) but not "closed". *)
let validate_dotted_subtable ~meta k sub =
  if sub.is_array then
    Error.raise_semantic ~meta (Cannot_extend_array_of_tables k);
  if sub.closed then Error.raise_semantic ~meta (Cannot_extend_closed_table k);
  if sub.is_inline then
    Error.raise_semantic ~meta (Cannot_extend_inline_table k)

let new_dotted_subtable state k meta =
  let sub = table_state () in
  sub.defined <- true;
  Hashtbl.add state.subtables k sub;
  remember_subtable_key state k meta;
  sub

let dotted_subtable ~meta state k =
  if Hashtbl.mem state.value_names k then
    Error.raise_semantic ~meta (Cannot_use_value_as_table k);
  match Hashtbl.find_opt state.subtables k with
  | Some sub ->
      validate_dotted_subtable ~meta k sub;
      sub.defined <- true;
      sub
  | None -> new_dotted_subtable state k meta

let rec table_for_dotted_key ~meta state keys =
  match keys with
  | [] -> state
  | [ k ] -> dotted_subtable ~meta state k
  | k :: rest ->
      let sub = dotted_subtable ~meta state k in
      table_for_dotted_key ~meta sub rest

(* Where a member starts in the document: its key, or the [[section]] header
   that names it, whichever comes first. A member the parser did not locate has
   no place and sorts before the rest, which is where it already was. *)
let member_pos ((_, km), v) =
  let key = Loc.first_byte (Loc.Meta.loc km) in
  let value = Loc.first_byte (Loc.Meta.loc (V.meta v)) in
  if Loc.compare_byte_pos key value <= 0 then key else value

let meta_of_table_array k_meta elts =
  match elts with
  | [] -> k_meta
  | _ :: _ -> List.fold_left (fun m e -> span_meta m (V.meta e)) k_meta elts

let array_elements state = List.rev state.array_elements

let rec value_of_subtable k_meta sub =
  if sub.is_array then
    let elts = List.map toml_of_table_state (array_elements sub) in
    V.Array (elts, meta_of_table_array k_meta elts)
  else toml_of_table_state sub

and subtable_member state k sub =
  let k_meta =
    match Hashtbl.find_opt state.subtable_metas k with
    | Some m -> m
    | None -> Loc.Meta.none
  in
  ((k, k_meta), value_of_subtable k_meta sub)

(* Finalise a [table_state] into a [Value.t] node. The top-level Table's meta
   spans from the header (or first value) to the last byte of the last member.
   Subtables and array-of-tables elements are recursively finalised with their
   own spans. *)
and toml_of_table_state state =
  let subtable_values =
    Hashtbl.fold
      (fun k sub acc -> subtable_member state k sub :: acc)
      state.subtables []
  in
  let values = List.rev state.values in
  (* Direct values arrive in the order the document gave them and subtables in
     whatever order the hash table iterates, so the two are put back in document
     order by where each member starts. A table written [[a]] then [[b]] comes
     back as [a] then [b], and a dotted key between two assignments stays
     between them. *)
  let members =
    List.stable_sort
      (fun x y -> Loc.compare_byte_pos (member_pos x) (member_pos y))
      (values @ subtable_values)
  in
  let meta =
    let m = span_meta state.header_meta state.last_meta in
    List.fold_left
      (fun m ((_, km), v) -> span_meta (span_meta m km) (V.meta v))
      m members
  in
  V.Table (members, meta)

(* Hand every node of [v] the document it was read from, so that writing it back
   is a copy rather than a re-render. The whole document is one string shared by
   every node, so this costs a word per node; it is done in one pass at the end
   because the last of those bytes is not read until the parse is over. A node
   the parser did not locate is left alone: there are no bytes of its own to
   point at. *)
let rec in_source src v =
  let meta m = if Loc.Meta.is_none m then m else Loc.Meta.with_text m src in
  match v with
  | V.String (x, m) -> V.String (x, meta m)
  | V.Int (x, m) -> V.Int (x, meta m)
  | V.Float (x, m) -> V.Float (x, meta m)
  | V.Bool (x, m) -> V.Bool (x, meta m)
  | V.Datetime (x, m) -> V.Datetime (x, meta m)
  | V.Datetime_local (x, m) -> V.Datetime_local (x, meta m)
  | V.Date_local (x, m) -> V.Date_local (x, meta m)
  | V.Time_local (x, m) -> V.Time_local (x, meta m)
  | V.Array (items, m) -> V.Array (List.map (in_source src) items, meta m)
  | V.Table (members, m) ->
      let member ((k, km), v) = ((k, meta km), in_source src v) in
      V.Table (List.map member members, meta m)

(* The root additionally keeps the document when the parse located nothing in
   it: an empty file, or one that is only comments, is still a document and
   still has to come back as itself. *)
let in_document src v =
  match in_source src v with
  | V.Table (members, m) when Loc.Meta.is_none m ->
      V.Table (members, Loc.Meta.with_text m src)
  | v -> v

let rec key_path_has_prefix keys prefix =
  match (keys, prefix) with
  | _, [] -> true
  | [], _ -> false
  | k :: krest, p :: prest -> k = p && key_path_has_prefix krest prest

let rec remove_key_prefix keys prefix =
  match (keys, prefix) with
  | ks, [] -> ks
  | [], _ -> []
  | _ :: krest, _ :: prest -> remove_key_prefix krest prest

let check_array_table_conflict ~meta array_table keys =
  let name = String.concat "." keys in
  if array_table.defined && not array_table.is_array then
    Error.raise_semantic ~meta (Cannot_convert_table_to_array name);
  if
    (array_table.values <> [] || Hashtbl.length array_table.subtables > 0)
    && not array_table.is_array
  then Error.raise_semantic ~meta (Table_has_content name)

let define_table_header ~meta table keys =
  let name = String.concat "." keys in
  if table.is_array then
    Error.raise_semantic ~meta (Cannot_convert_array_to_table name);
  if table.defined then Error.raise_semantic ~meta (Table_already_defined name);
  table.defined <- true;
  table.closed <- true

let add_value_to_table ~meta ~key_meta tbl key v =
  if Hashtbl.mem tbl.value_names key then
    Error.raise_semantic ~meta (Duplicate_key key);
  (match Hashtbl.find_opt tbl.subtables key with
  | Some sub ->
      if sub.is_array then
        Error.raise_semantic ~meta (Cannot_redefine_array_as_value key)
      else Error.raise_semantic ~meta (Cannot_redefine_table_as_value key)
  | None -> ());
  tbl.values <- ((key, key_meta), v) :: tbl.values;
  Hashtbl.replace tbl.value_names key ();
  widen_last tbl key_meta;
  widen_last tbl (V.meta v)

type doc_state = {
  root : table_state;
  mutable current_table : table_state;
  array_context_stack : (string list * table_state * table_state) list ref;
}

let doc_state_of root =
  { root; current_table = root; array_context_stack = ref [] }

let array_context ds keys =
  let rec find stack =
    match stack with
    | [] -> None
    | (path, parent, container) :: rest ->
        if keys = path then Some (`Sibling (path, parent, container))
        else if
          key_path_has_prefix keys path && List.length keys > List.length path
        then
          let current_entry = List.hd container.array_elements in
          Some (`Nested (path, current_entry))
        else find rest
  in
  find !(ds.array_context_stack)

let rec pop_invalid_contexts ds keys =
  match !(ds.array_context_stack) with
  | [] -> ()
  | (path, _, _) :: rest ->
      if not (key_path_has_prefix keys path) then begin
        ds.array_context_stack := rest;
        pop_invalid_contexts ds keys
      end

let fresh_entry_with_header header_meta =
  let entry = table_state () in
  entry.header_meta <- header_meta;
  entry.last_meta <- header_meta;
  entry

let handle_array_of_tables ~meta ds keys =
  pop_invalid_contexts ds keys;
  match array_context ds keys with
  | Some (`Sibling (_path, _parent, container)) ->
      let new_entry = fresh_entry_with_header meta in
      container.array_elements <- new_entry :: container.array_elements;
      ds.current_table <- new_entry
  | Some (`Nested (parent_path, parent_entry)) ->
      let relative_keys = remove_key_prefix keys parent_path in
      let array_table = ensure_table ~meta parent_entry relative_keys true in
      check_array_table_conflict ~meta array_table keys;
      array_table.is_array <- true;
      let new_entry = fresh_entry_with_header meta in
      array_table.array_elements <- new_entry :: array_table.array_elements;
      ds.current_table <- new_entry;
      ds.array_context_stack :=
        (keys, parent_entry, array_table) :: !(ds.array_context_stack)
  | None ->
      let array_table = ensure_table ~meta ds.root keys true in
      check_array_table_conflict ~meta array_table keys;
      array_table.is_array <- true;
      let entry = fresh_entry_with_header meta in
      array_table.array_elements <- entry :: array_table.array_elements;
      ds.current_table <- entry;
      ds.array_context_stack :=
        (keys, ds.root, array_table) :: !(ds.array_context_stack)

(* Mark [table]'s header_meta/last_meta with the [section] header's meta,
   widening in case a prior implicit definition already set them. *)
let set_header_meta table meta =
  table.header_meta <- span_meta table.header_meta meta;
  widen_last table meta

let handle_table_header ~meta ds keys =
  pop_invalid_contexts ds keys;
  let name () = String.concat "." keys in
  match array_context ds keys with
  | Some (`Nested (parent_path, parent_entry)) ->
      let relative_keys = remove_key_prefix keys parent_path in
      let table =
        if relative_keys <> [] then
          ensure_table ~meta parent_entry relative_keys true
        else ensure_table ~meta ds.root keys true
      in
      define_table_header ~meta table keys;
      set_header_meta table meta;
      ds.current_table <- table
  | Some (`Sibling (_, _, container)) ->
      if container.is_array then
        Error.raise_semantic ~meta (Cannot_convert_array_to_table (name ()));
      let table = ensure_table ~meta ds.root keys true in
      if table.defined then
        Error.raise_semantic ~meta (Table_already_defined (name ()));
      table.defined <- true;
      table.closed <- true;
      set_header_meta table meta;
      ds.current_table <- table
  | None ->
      let table = ensure_table ~meta ds.root keys true in
      define_table_header ~meta table keys;
      set_header_meta table meta;
      ds.current_table <- table;
      if
        not
          (List.exists
             (fun (p, _, _) -> key_path_has_prefix keys p)
             !(ds.array_context_stack))
      then ds.array_context_stack := []

(* Skip whitespace + comments and the trailing newline (or accept EOF). *)
let skip_to_newline parser =
  skip_ws_and_comments parser.lexer;
  match peek_token parser with
  | Newline -> ignore (consume_token parser)
  | Eof -> ()
  | _ ->
      Error.raise_syntax ~meta:parser.current_meta
        (Expected "newline after value")

(* Handle [...] / [[...]] header. Caller has consumed the leading [.

   The header a table is named by is the whole of [[a.b]], closing bracket
   included, and not the [ the caller stopped at. A table with members spans
   past it either way, but one with none has only its header to stand on: given
   the bracket alone, [a] would say it lives at one byte of a three-byte line,
   and the bytes of the next table would be nobody's. *)
let handle_table_or_aot_header parser ds ~header_meta =
  let is_adjacent_bracket = next_raw_char_is parser '[' in
  let through_bracket () = span_meta header_meta parser.current_meta in
  match peek_token parser with
  | Lbracket when not is_adjacent_bracket ->
      Error.raise_syntax ~meta:parser.current_meta Invalid_table_header
  | Lbracket ->
      ignore (consume_token parser);
      let keys = List.map fst (parse_dotted_key parser) in
      expect_token parser Rbracket;
      if not (next_raw_char_is parser ']') then
        Error.raise_syntax ~meta:parser.current_meta
          Invalid_array_of_tables_header;
      expect_token parser Rbracket;
      let header_meta = through_bracket () in
      skip_to_newline parser;
      handle_array_of_tables ~meta:header_meta ds keys
  | _ ->
      let keys = List.map fst (parse_dotted_key parser) in
      expect_token parser Rbracket;
      let header_meta = through_bracket () in
      skip_to_newline parser;
      handle_table_header ~meta:header_meta ds keys

(* Handle a single `key = value` (or dotted key) entry. *)
let handle_keyval_entry parser ds =
  let key_meta = parser.current_meta in
  let keys = parse_dotted_key parser in
  expect_token parser Equals;
  let value = with_key_depth parser keys (fun () -> parse_value parser) in
  skip_to_newline parser;
  match keys with
  | [] -> Error.raise_semantic ~meta:key_meta Empty_key
  | [ (k, km) ] ->
      add_value_to_table ~meta:key_meta ~key_meta:km ds.current_table k value
  | _ ->
      let parent_keys = List.rev (List.tl (List.rev keys)) in
      let final_key, final_km = List.hd (List.rev keys) in
      let parent =
        table_for_dotted_key ~meta:key_meta ds.current_table
          (List.map fst parent_keys)
      in
      add_value_to_table ~meta:key_meta ~key_meta:final_km parent final_key
        value

(* Main parser function *)
let parse_toml_of_lexer ?max_depth ?max_nodes lexer =
  let parser = v ?max_depth ?max_nodes lexer in
  let ds = doc_state_of (table_state ()) in
  let rec parse_document () =
    skip_newlines parser;
    match peek_token parser with
    | Eof -> ()
    | Lbracket ->
        let header_meta = parser.current_meta in
        ignore (consume_token parser);
        handle_table_or_aot_header parser ds ~header_meta;
        parse_document ()
    | Bare_key _ | Basic_string _ | Literal_string _ | Integer _ | Float _
    | Date_local _ | Datetime _ | Datetime_local _ | Time_local _ ->
        handle_keyval_entry parser ds;
        parse_document ()
    | _ ->
        let tok = peek_token parser in
        Error.raise_syntax ~meta:parser.current_meta
          (Unexpected_token (string_of_token tok))
  in
  parse_document ();
  in_document (Buffer.contents lexer.source) (toml_of_table_state ds.root)

(* Parse TOML from string - creates lexer internally *)
let parse_toml ?max_depth ?max_nodes input =
  let lexer = lexer input in
  parse_toml_of_lexer ?max_depth ?max_nodes lexer

(* Parse TOML directly from Bytes.Reader - no intermediate string *)
let parse_toml_of_reader ?file ?max_depth ?max_nodes r =
  let lexer = lexer_of_reader ?file r in
  parse_toml_of_lexer ?max_depth ?max_nodes lexer

let normalize_exp_notation s =
  let buf = Buffer.create (String.length s + 1) in
  let i = ref 0 in
  while !i < String.length s do
    let c = s.[!i] in
    if c = 'E' || c = 'e' then begin
      Buffer.add_char buf 'e';
      if !i + 1 < String.length s then begin
        let next = s.[!i + 1] in
        if next >= '0' && next <= '9' then Buffer.add_char buf '+'
      end
    end
    else Buffer.add_char buf c;
    incr i
  done;
  Buffer.contents buf

let shortest_exp_notation f init_prec =
  let rec try_exp prec =
    if prec > 17 then Fmt.kstr normalize_exp_notation "%.17e" f
    else
      let s = Fmt.kstr normalize_exp_notation "%.*e" prec f in
      if float_of_string s = f then s else try_exp (prec + 1)
  in
  try_exp init_prec

(* Trim trailing zeros after the decimal point of [s], but keep at least one
   fractional digit. *)
let trim_trailing_zeros s =
  let len = String.length s in
  let dot_pos = try String.index s '.' with Not_found -> len in
  let rec last_nonzero i =
    if i <= dot_pos then dot_pos + 2
    else if s.[i] <> '0' then i + 1
    else last_nonzero (i - 1)
  in
  String.sub s 0 (min len (last_nonzero (len - 1)))

(* Make sure [s] has a fractional part (".0" suffix), so it round-trips as a
   float and not as an integer. *)
let ensure_fractional_part s =
  if not (String.contains s '.') then s ^ ".0"
  else if s.[String.length s - 1] = '.' then s ^ "0"
  else s

let shortest_decimal_notation f =
  let rec try_decimal prec =
    if prec > 17 then None
    else
      let s = Fmt.str "%.*f" prec f in
      let s = ensure_fractional_part (trim_trailing_zeros s) in
      if float_of_string s = f then Some s else try_decimal (prec + 1)
  in
  try_decimal 1

let shortest_g_notation f =
  let rec try_g prec =
    if prec > 17 then Fmt.str "%.17g" f
    else
      let s = Fmt.str "%.*g" prec f in
      if float_of_string s = f then s else try_g (prec + 1)
  in
  try_g 1

let integer_float_str f =
  let abs_f = Float.abs f in
  if abs_f = 9007199254740991.0 then Fmt.str "%.0f" f
  else if abs_f >= 1e6 then shortest_exp_notation f 0
  else if abs_f >= 2.0 then Fmt.str "%.1f" f
  else Fmt.str "%.0f" f

let fractional_float_str f =
  let abs_f = Float.abs f in
  if abs_f >= 1e10 || (abs_f < 1e-4 && abs_f > 0.0) then
    shortest_exp_notation f 1
  else
    match shortest_decimal_notation f with
    | Some d -> d
    | None -> shortest_g_notation f

let float_to_tagged_json_str f =
  if Float.is_nan f then "nan"
  else if f = Float.infinity then "inf"
  else if f = Float.neg_infinity then "-inf"
  else if f = 0.0 then if 1.0 /. f = Float.neg_infinity then "-0" else "0"
  else if Float.is_integer f then integer_float_str f
  else fractional_float_str f

let json_encode_string s =
  let buf = Buffer.create (String.length s + 2) in
  let ppf = Fmt.with_buffer buf in
  Fmt.pf ppf "\"";
  String.iter
    (fun c ->
      match c with
      | '"' -> Fmt.pf ppf "\\\""
      | '\\' -> Fmt.pf ppf "\\\\"
      | '\n' -> Fmt.pf ppf "\\n"
      | '\r' -> Fmt.pf ppf "\\r"
      | '\t' -> Fmt.pf ppf "\\t"
      | '\b' -> Fmt.pf ppf "\\b" (* backspace *)
      | c when Char.code c = 0x0C -> Fmt.pf ppf "\\f" (* formfeed *)
      | c when Char.code c < 0x20 -> Fmt.pf ppf "\\u%04x" (Char.code c)
      | c -> Fmt.pf ppf "%c" c)
    s;
  Fmt.pf ppf "\"%!";
  Buffer.contents buf

(* Convert TOML to tagged JSON for toml-test compatibility *)
let rec tagged_of_toml_json value =
  match value with
  | V.String (s, _) ->
      Fmt.str "{\"type\":\"string\",\"value\":%s}" (json_encode_string s)
  | V.Int (i, _) -> Fmt.str "{\"type\":\"integer\",\"value\":\"%Ld\"}" i
  | V.Float (f, _) ->
      Fmt.str "{\"type\":\"float\",\"value\":\"%s\"}"
        (float_to_tagged_json_str f)
  | V.Bool (b, _) ->
      Fmt.str "{\"type\":\"bool\",\"value\":\"%s\"}"
        (if b then "true" else "false")
  | V.Datetime (s, meta) ->
      validate_datetime_string ~meta s;
      Fmt.str "{\"type\":\"datetime\",\"value\":\"%s\"}" s
  | V.Datetime_local (s, meta) ->
      validate_datetime_string ~meta s;
      Fmt.str "{\"type\":\"datetime-local\",\"value\":\"%s\"}" s
  | V.Date_local (s, meta) ->
      validate_date_string ~meta s;
      Fmt.str "{\"type\":\"date-local\",\"value\":\"%s\"}" s
  | V.Time_local (s, meta) ->
      validate_time_string ~meta s;
      Fmt.str "{\"type\":\"time-local\",\"value\":\"%s\"}" s
  | V.Array (items, _) ->
      let json_items = List.map tagged_of_toml_json items in
      Fmt.str "[%s]" (String.concat "," json_items)
  | V.Table (pairs, _) ->
      let json_pairs =
        List.map
          (fun ((k, _), v) ->
            Fmt.str "%s:%s" (json_encode_string k) (tagged_of_toml_json v))
          pairs
      in
      Fmt.str "{%s}" (String.concat "," json_pairs)

(* Tagged-JSON -> TOML bridge for the toml-test harness. The JSON reader below
   ([json_reader]) is a minimal character-level decoder that does not track byte
   positions, so every value produced on this path carries [V.Meta.none]. This
   is intentional: the tagged-JSON input is not the TOML source document and
   there are no source positions to propagate. If byte-positioned tagged JSON is
   ever needed, switch the reader to an ocaml-json parser (already Meta-aware)
   and plumb each JSON node's [Meta.t] through the bridge. *)
let toml_of_tagged_value value =
  let m = V.Meta.none in
  match value with
  | V.Table
      ([ (("type", _), V.String (typ, _)); (("value", _), V.String (v, _)) ], _)
  | V.Table
      ([ (("value", _), V.String (v, _)); (("type", _), V.String (typ, _)) ], _)
    -> (
      match typ with
      | "string" -> V.String (v, m)
      | "integer" -> V.Int (Int64.of_string v, m)
      | "float" -> (
          match v with
          | "inf" -> V.Float (Float.infinity, m)
          | "-inf" -> V.Float (Float.neg_infinity, m)
          | "nan" -> V.Float (Float.nan, m)
          | _ -> V.Float (float_of_string v, m))
      | "bool" -> V.Bool (v = "true", m)
      | "datetime" -> V.Datetime (v, m)
      | "datetime-local" -> V.Datetime_local (v, m)
      | "date-local" -> V.Date_local (v, m)
      | "time-local" -> V.Time_local (v, m)
      | _ -> Fmt.failwith "Unknown type: %s" typ)
  | _ -> value

type json_reader = { s : string; len : int; pos : int ref }

let json_reader_of s = { s; len = String.length s; pos = ref 0 }

let jr_skip_ws r =
  while
    !(r.pos) < r.len
    && (r.s.[!(r.pos)] = ' '
       || r.s.[!(r.pos)] = '\t'
       || r.s.[!(r.pos)] = '\n'
       || r.s.[!(r.pos)] = '\r')
  do
    incr r.pos
  done

let jr_expect r c =
  jr_skip_ws r;
  if !(r.pos) >= r.len || r.s.[!(r.pos)] <> c then
    Fmt.failwith "Expected '%c' at position %d" c !(r.pos);
  incr r.pos

let jr_peek r =
  jr_skip_ws r;
  if !(r.pos) >= r.len then None else Some r.s.[!(r.pos)]

let jr_read_escape r buf =
  incr r.pos;
  if !(r.pos) >= r.len then failwith "Unexpected end in string escape";
  match r.s.[!(r.pos)] with
  | '"' ->
      Buffer.add_char buf '"';
      incr r.pos
  | '\\' ->
      Buffer.add_char buf '\\';
      incr r.pos
  | '/' ->
      Buffer.add_char buf '/';
      incr r.pos
  | 'n' ->
      Buffer.add_char buf '\n';
      incr r.pos
  | 'r' ->
      Buffer.add_char buf '\r';
      incr r.pos
  | 't' ->
      Buffer.add_char buf '\t';
      incr r.pos
  | 'b' ->
      Buffer.add_char buf '\b';
      incr r.pos
  | 'f' ->
      Buffer.add_char buf (Char.chr 0x0C);
      incr r.pos
  | 'u' ->
      incr r.pos;
      if !(r.pos) + 3 >= r.len then failwith "Invalid unicode escape";
      let hex = String.sub r.s !(r.pos) 4 in
      let cp = int_of_string ("0x" ^ hex) in
      Buffer.add_string buf (utf8_of_codepoint cp);
      r.pos := !(r.pos) + 4
  | c -> Fmt.failwith "Invalid escape: \\%c" c

let jr_read_string r =
  jr_skip_ws r;
  jr_expect r '"';
  let buf = Buffer.create 64 in
  while !(r.pos) < r.len && r.s.[!(r.pos)] <> '"' do
    if r.s.[!(r.pos)] = '\\' then jr_read_escape r buf
    else begin
      Buffer.add_char buf r.s.[!(r.pos)];
      incr r.pos
    end
  done;
  jr_expect r '"';
  Buffer.contents buf

let rec jr_read_value r =
  jr_skip_ws r;
  match jr_peek r with
  | Some '{' -> jr_read_object r
  | Some '[' -> jr_read_array r
  | Some '"' -> V.String (jr_read_string r, V.Meta.none)
  | _ -> failwith "Expected value"

and jr_read_object r =
  jr_expect r '{';
  jr_skip_ws r;
  if jr_peek r = Some '}' then begin
    incr r.pos;
    V.Table ([], V.Meta.none)
  end
  else begin
    let pairs = ref [] in
    let first = ref true in
    while jr_peek r <> Some '}' do
      if not !first then jr_expect r ',';
      first := false;
      jr_skip_ws r;
      let key = jr_read_string r in
      jr_expect r ':';
      let value = jr_read_value r in
      pairs := ((key, V.Meta.none), toml_of_tagged_value value) :: !pairs
    done;
    jr_expect r '}';
    V.Table (List.rev !pairs, V.Meta.none)
  end

and jr_read_array r =
  jr_expect r '[';
  jr_skip_ws r;
  if jr_peek r = Some ']' then begin
    incr r.pos;
    V.Array ([], V.Meta.none)
  end
  else begin
    let items = ref [] in
    let first = ref true in
    while jr_peek r <> Some ']' do
      if not !first then jr_expect r ',';
      first := false;
      items := toml_of_tagged_value (jr_read_value r) :: !items
    done;
    jr_expect r ']';
    V.Array (List.rev !items, V.Meta.none)
  end

(* Tagged JSON to TOML for encoder *)
let decode_tagged_json_string s = jr_read_value (json_reader_of s)

(* ============================================ Streaming TOML Encoder
   ============================================ *)

(* Internal encoder shape derived from the public [?indent] / [?preserve]
   parameters. See [to_string] in [toml.mli] for the user-visible contract. -
   [`Minify]: [indent = None] and [preserve = false]. Every nested table becomes
   inline; arrays and inline tables use no whitespace around separators; no
   trailing newlines. - [`Indent]: [indent = Some _] and [preserve = false].
   Pretty-printed with [[section]] headers for nested tables and
   [[array-of-tables]] headers. The integer [n] would control per-level
   indentation of section contents; TOML sections are conventionally flush-left,
   so the encoder currently ignores [n] and emits the canonical sectioned form.
   - [`Layout]: [preserve = true]. Write a parsed document back as the bytes it
   was read from, falling back to [`Indent] for a value that has none. *)
let format_of ~indent ~preserve =
  if preserve then `Layout
  else match indent with None -> `Minify | Some _ -> `Indent

let write_toml_string w s =
  (* Check if we need to escape *)
  let needs_escape =
    String.exists
      (fun c ->
        let code = Char.code c in
        c = '"' || c = '\\' || c = '\n' || c = '\r' || c = '\t' || code < 0x20
        || code = 0x7F)
      s
  in
  if needs_escape then begin
    Bytes.Writer.write_string w "\"";
    String.iter
      (fun c ->
        match c with
        | '"' -> Bytes.Writer.write_string w "\\\""
        | '\\' -> Bytes.Writer.write_string w "\\\\"
        | '\n' -> Bytes.Writer.write_string w "\\n"
        | '\r' -> Bytes.Writer.write_string w "\\r"
        | '\t' -> Bytes.Writer.write_string w "\\t"
        | '\b' -> Bytes.Writer.write_string w "\\b"
        | c when Char.code c = 0x0C -> Bytes.Writer.write_string w "\\f"
        | c when Char.code c < 0x20 || Char.code c = 0x7F ->
            Fmt.kstr (Bytes.Writer.write_string w) "\\u%04X" (Char.code c)
        | c ->
            let b = Bytes.create 1 in
            Bytes.set b 0 c;
            Bytes.Writer.write_bytes w b)
      s;
    Bytes.Writer.write_string w "\""
  end
  else begin
    Bytes.Writer.write_string w "\"";
    Bytes.Writer.write_string w s;
    Bytes.Writer.write_string w "\""
  end

let write_toml_key w k =
  (* Check if it can be a bare key *)
  let is_bare = String.length k > 0 && String.for_all is_bare_key_char k in
  if is_bare then Bytes.Writer.write_string w k else write_toml_string w k

let write_toml_float w f =
  if Float.is_nan f then Bytes.Writer.write_string w "nan"
  else if f = Float.infinity then Bytes.Writer.write_string w "inf"
  else if f = Float.neg_infinity then Bytes.Writer.write_string w "-inf"
  else
    let s = Fmt.str "%.17g" f in
    let s =
      if String.contains s '.' || String.contains s 'e' || String.contains s 'E'
      then s
      else s ^ ".0"
    in
    Bytes.Writer.write_string w s

let rec write_toml_array w ~format sep items =
  Bytes.Writer.write_string w "[";
  List.iteri
    (fun i item ->
      if i > 0 then Bytes.Writer.write_string w sep;
      write_toml_value w ~format ~inline:true item)
    items;
  Bytes.Writer.write_string w "]"

and write_inline_toml_table w ~format sep kv_sep pairs =
  Bytes.Writer.write_string w "{";
  List.iteri
    (fun i ((k, _), v) ->
      if i > 0 then Bytes.Writer.write_string w sep;
      write_toml_key w k;
      Bytes.Writer.write_string w kv_sep;
      write_toml_value w ~format ~inline:true v)
    pairs;
  Bytes.Writer.write_string w "}"

and write_toml_value w ~format ?(inline = false) value =
  (* Separator between array / inline-table members. [`Minify] packs them tight;
     the other formats keep a space after the comma for readability. *)
  let sep = match format with `Minify -> "," | `Indent | `Layout -> ", " in
  let kv_sep =
    match format with `Minify -> "=" | `Indent | `Layout -> " = "
  in
  match value with
  | V.String (s, _) -> write_toml_string w s
  | V.Int (i, _) -> Bytes.Writer.write_string w (Int64.to_string i)
  | V.Float (f, _) -> write_toml_float w f
  | V.Bool (b, _) -> Bytes.Writer.write_string w (if b then "true" else "false")
  | V.Datetime (s, _)
  | V.Datetime_local (s, _)
  | V.Date_local (s, _)
  | V.Time_local (s, _) ->
      Bytes.Writer.write_string w s
  | V.Array (items, _) -> write_toml_array w ~format sep items
  | V.Table (pairs, _) when inline ->
      write_inline_toml_table w ~format sep kv_sep pairs
  | V.Table (_, meta) -> Error.raise_encode ~meta Cannot_encode_inline_table

let is_pure_table_array items =
  items <> [] && List.for_all (function V.Table _ -> true | _ -> false) items

let write_path w path =
  List.iteri
    (fun i k ->
      if i > 0 then Bytes.Writer.write_string w ".";
      write_toml_key w k)
    path

let rec encode_at_path w ~format has_content path value =
  match value with
  | V.Table (pairs, _) ->
      let simple, nested =
        List.partition
          (fun (_, v) ->
            match v with
            | V.Table _ -> false
            | V.Array (items, _) -> not (is_pure_table_array items)
            | _ -> true)
          pairs
      in
      List.iter
        (fun ((k, _), v) ->
          write_toml_key w k;
          Bytes.Writer.write_string w " = ";
          write_toml_value w ~format ~inline:true v;
          Bytes.Writer.write_string w "\n";
          has_content := true)
        simple;
      encode_nested_pairs w ~format has_content path nested
  | _ -> Error.raise_encode ~meta:(V.meta value) Not_a_table

and encode_nested_pairs w ~format has_content path nested =
  List.iter
    (fun ((k, _), v) ->
      let new_path = path @ [ k ] in
      match v with
      | V.Table _ ->
          if !has_content then Bytes.Writer.write_string w "\n";
          Bytes.Writer.write_string w "[";
          write_path w new_path;
          Bytes.Writer.write_string w "]\n";
          has_content := true;
          encode_at_path w ~format has_content new_path v
      | V.Array (items, _) when is_pure_table_array items ->
          encode_table_array w ~format has_content new_path items
      | _ ->
          write_toml_key w k;
          Bytes.Writer.write_string w " = ";
          write_toml_value w ~format ~inline:true v;
          Bytes.Writer.write_string w "\n";
          has_content := true)
    nested

and encode_table_array w ~format has_content new_path items =
  List.iter
    (fun item ->
      match item with
      | V.Table _ ->
          if !has_content then Bytes.Writer.write_string w "\n";
          Bytes.Writer.write_string w "[[";
          write_path w new_path;
          Bytes.Writer.write_string w "]]\n";
          has_content := true;
          encode_at_path w ~format has_content new_path item
      | _ -> assert false)
    items

(* Minified encoder: flat [k=v] pairs at the top, nested tables rendered as
   inline tables. Arrays are tight ([v1,v2,v3]). No [[section]] headers, no
   trailing newline, [k=v] is flush. The output is a valid TOML document (the
   root cannot be an inline [{...}] under TOML 1.1). *)
let encode_minified w value =
  match value with
  | V.Table (pairs, _) ->
      List.iteri
        (fun i ((k, _), v) ->
          if i > 0 then Bytes.Writer.write_string w "\n";
          write_toml_key w k;
          Bytes.Writer.write_string w "=";
          write_toml_value w ~format:`Minify ~inline:true v)
        pairs
  | _ -> Error.raise_encode ~meta:(V.meta value) Not_a_table

(* ---- Layout preservation --------------------------------------------------

   Writing a parsed document back is copying, not rendering. Every node records
   the byte range it was read from and the document those bytes belong to, so a
   subtree nothing has touched is written as the source text under its own
   range. That is what carries the layout across: the comments, the column an
   author aligned [=] to, the blank line between two tables, which of TOML's
   three ways of writing a table the file used, and the spelling of every
   scalar -- [0x1F] against [31], ['a\b'] against ["a\\b"] -- none of which the
   value itself remembers.

   What a caller put there is rendered instead. A node with a range but no
   source text stands in for the bytes at that range: its neighbours are still
   copied and it alone is written afresh, which is the in-place edit
   {!Cursor.set} makes. A document that has parted from its bytes -- a member
   added or removed, a key renamed, a value built in OCaml -- has nowhere to
   splice into and is written in the [`Indent] form throughout. *)

(* The bytes [m]'s node occupies. [None] for a node the parser never located,
   which is any node a caller made. *)
let range_of m =
  let l = Loc.Meta.loc m in
  if Loc.is_none l || Loc.is_empty l then None
  else Some (Loc.first_byte l, Loc.last_byte l)

let byte_is src i c =
  Loc.compare_byte_pos i 0 >= 0
  && Loc.compare_byte_pos i (String.length src) < 0
  && src.[i] = c

(* Past the [#] comment at [i]: it ends on the newline, which the caller then
   steps over as whitespace. *)
let comment_end src ~stop i =
  let i = ref i in
  while Loc.compare_byte_pos !i stop <= 0 && src.[!i] <> '\n' do
    incr i
  done;
  !i

(* Past the run of layout at [i]: whitespace, comments, and the commas a
   container puts between its members. That is everything the document holds
   between one member and the next. *)
let layout_end src ~stop i =
  let i = ref i and done_ = ref false in
  while (not !done_) && Loc.compare_byte_pos !i stop <= 0 do
    match src.[!i] with
    | ' ' | '\t' | '\n' | '\r' | ',' -> incr i
    | '#' -> i := comment_end src ~stop !i
    | _ -> done_ := true
  done;
  !i

(* Past the quoted key at [i], and [None] where it does not close by [stop]. *)
let quoted_key_end src ~stop i =
  let quote = src.[i] in
  let j = ref (i + 1) and closed = ref false in
  while (not !closed) && Loc.compare_byte_pos !j stop <= 0 do
    if src.[!j] = '\\' && quote = '"' then j := !j + 2
    else if src.[!j] = quote then begin
      incr j;
      closed := true
    end
    else incr j
  done;
  if !closed then Some !j else None

(* Past the key at [i], bare or quoted, and [None] where there is no key
   there. *)
let key_end src ~stop i =
  match src.[i] with
  | '"' | '\'' -> quoted_key_end src ~stop i
  | c when is_bare_key_char c ->
      let j = ref i in
      while Loc.compare_byte_pos !j stop <= 0 && is_bare_key_char src.[!j] do
        incr j
      done;
      Some !j
  | _ -> None

let blanks_end src ~stop i =
  let i = ref i and done_ = ref false in
  while (not !done_) && Loc.compare_byte_pos !i stop <= 0 do
    match src.[!i] with ' ' | '\t' -> incr i | _ -> done_ := true
  done;
  !i

(* Whether [src.(i .. stop)] is a run of keys each followed by a dot, which is
   how a member of a table written with dotted keys names its way down from the
   root. Two members written [a.b = 1] and [a.c = 2] both belong to table [a],
   whose range starts at [b]: the [a.] before [c] is the second member's own
   spelling, restated, and it lies in the gap in front of it. *)
let rec is_key_prefix src ~stop i =
  Loc.compare_byte_pos i stop > 0
  ||
  match key_end src ~stop i with
  | None -> false
  | Some j ->
      let j = blanks_end src ~stop j in
      byte_is src j '.'
      && Loc.compare_byte_pos j stop <= 0
      && is_key_prefix src ~stop (blanks_end src ~stop (j + 1))

(* Whether the gap [first .. last] between two members holds nothing that was a
   member. [~keys] is for a table, where a gap may end in the dotted-key prefix
   of the member that follows it. *)
let is_gap src ~keys first last =
  let i = layout_end src ~stop:last first in
  Loc.compare_byte_pos i last > 0 || (keys && is_key_prefix src ~stop:last i)

(* Past the [table] or [[table]] header at [i], the quoted keys in it included.
   A header that does not close by [stop] is all there is of the container: an
   empty [a] spans its header and nothing else. *)
let header_end src ~stop i =
  let double =
    byte_is src (i + 1) '[' && Loc.compare_byte_pos (i + 1) stop <= 0
  in
  let j = ref (if double then i + 2 else i + 1) in
  let closed = ref false and bad = ref false in
  while (not !closed) && (not !bad) && Loc.compare_byte_pos !j stop <= 0 do
    match src.[!j] with
    | '"' | '\'' -> (
        match quoted_key_end src ~stop !j with
        | Some k -> j := k
        | None -> bad := true)
    | ']' ->
        incr j;
        if not double then closed := true
        else if byte_is src !j ']' && Loc.compare_byte_pos !j stop <= 0 then begin
          incr j;
          closed := true
        end
        else bad := true
    | _ -> incr j
  done;
  if !closed then !j else stop + 1

(* Whether the members tile [first .. last], in the order the container holds
   them, with nothing but layout in the gaps. Anything else means the container
   and its bytes have parted: a member the value no longer has still lies in
   the gap where it was, and copying that gap would put it back. *)
let tiles src ~keys ~(first : int) ~(last : int) spans =
  let stop = min last (String.length src - 1) in
  let rec loop (next : int) = function
    | [] -> is_gap src ~keys:false next stop
    | (f, l) :: rest ->
        Loc.compare_byte_pos f next >= 0
        && Loc.compare_byte_pos l f >= 0
        && Loc.compare_byte_pos l stop <= 0
        && is_gap src ~keys next (f - 1)
        && loop (l + 1) rest
  in
  loop (max first 0) spans

let rec spans_of f acc = function
  | [] -> Some (List.rev acc)
  | x :: rest -> (
      match f x with None -> None | Some s -> spans_of f (s :: acc) rest)

(* Where a table member sits: from its key, where that is spelled beside the
   value, to the end of the value. A member under a [section] header has its
   key spelled inside the header line and so inside the value's own range. *)
let member_span ((_, km), v) =
  match (range_of km, range_of (V.meta v)) with
  | None, _ | _, None -> None
  | Some (kfirst, _), Some (vfirst, vlast) ->
      if Loc.compare_byte_pos kfirst vfirst < 0 then Some (kfirst, vlast)
      else Some (vfirst, vlast)

(* Whether the container opens where its first member does, which says it has
   no brackets of its own: an array of tables is written as its elements and a
   table under a [section] header is written under that header. *)
let opens_at first = function
  | (f, _) :: _ -> Loc.compare_byte_pos f first = 0
  | [] -> false

(* Whether [v] is still tiled by what it holds, all the way down: every node
   located, and every container's range accounted for by its members and the
   layout between them. A document that fails this has parted from its bytes
   and has nowhere to splice an edit into, so it is written afresh.

   [~headed] says the container is named by a [section] header inside its own
   range, which is what a table reached through such a header is, and what
   every element of an array of tables is. The header is then the container's
   own frame rather than a gap: for anything else, and for the document root,
   there is no frame and the first member opens the range. *)
let rec is_spliceable ~headed src v =
  match range_of (V.meta v) with
  | None -> false
  | Some (first, last) -> (
      match v with
      | V.Array (items, _) -> array_spliceable src ~first ~last items
      | V.Table (members, _) ->
          table_spliceable src ~headed ~first ~last members
      | _ -> true)

and array_spliceable src ~first ~last items =
  match spans_of (fun v -> range_of (V.meta v)) [] items with
  | None -> false
  | Some spans ->
      let headed = opens_at first spans in
      (if headed then tiles src ~keys:false ~first ~last spans
       else
         byte_is src first '[' && byte_is src last ']'
         && Loc.compare_byte_pos last first > 0
         && tiles src ~keys:false ~first:(first + 1) ~last:(last - 1) spans)
      && List.for_all (is_spliceable ~headed src) items

and table_spliceable src ~headed ~first ~last members =
  match spans_of member_span [] members with
  | None -> false
  | Some spans ->
      (if byte_is src first '{' then
         byte_is src last '}'
         && Loc.compare_byte_pos last first > 0
         && tiles src ~keys:true ~first:(first + 1) ~last:(last - 1) spans
       else
         let body =
           if headed && byte_is src first '[' && not (opens_at first spans) then
             header_end src ~stop:last first
           else first
         in
         tiles src ~keys:true ~first:body ~last spans)
      && List.for_all (member_spliceable src) members

and member_spliceable src ((_, km), v) =
  let headed =
    match (range_of km, range_of (V.meta v)) with
    | Some (kfirst, _), Some (vfirst, _) ->
        Loc.compare_byte_pos kfirst vfirst >= 0
    | _ -> false
  in
  is_spliceable ~headed src v

(* Whether [v] is still the bytes it was read from, all the way down. A node
   that has kept its source text has kept its own bytes; one that has dropped
   it is a replacement standing at the same place. *)
let rec is_unchanged v =
  Option.is_some (Loc.Meta.text (V.meta v))
  &&
  match v with
  | V.Array (items, _) -> List.for_all is_unchanged items
  | V.Table (members, _) ->
      List.for_all
        (fun ((_, km), cv) ->
          Option.is_some (Loc.Meta.text km) && is_unchanged cv)
        members
  | _ -> true

(* Copy [src.(first .. last)], an empty range writing nothing. *)
let write_range w src first last =
  let first = max first 0 and last = min last (String.length src - 1) in
  if last >= first then
    Bytes.Writer.write_string w (String.sub src first (last - first + 1))

(* One stretch of a container's range: where it sits and what goes there. The
   bytes between two pieces are copied across, and that is where a container's
   own punctuation lives -- the [=] of an assignment, the comma and brackets of
   an array, the braces of an inline table, the [[section]] line above a table
   -- so none of it is ever rebuilt from the value. *)
type piece = { first : int; last : int; write : Bytes.Writer.t -> unit }

(* Write the pieces in source order, copying the bytes between them, before them
   and after them. *)
let write_pieces w src ~first ~last pieces =
  let pieces =
    List.sort (fun a b -> Loc.compare_byte_pos a.first b.first) pieces
  in
  let next = ref first in
  List.iter
    (fun p ->
      write_range w src !next (p.first - 1);
      p.write w;
      next := p.last + 1)
    pieces;
  write_range w src !next last

let rec write_preserved ~src w v =
  match range_of (V.meta v) with
  | Some (first, last) when is_unchanged v -> write_range w src first last
  | Some (first, last) -> (
      match v with
      | V.Array (items, _) ->
          write_pieces w src ~first ~last (List.map (element_piece ~src) items)
      | V.Table (members, _) ->
          write_pieces w src ~first ~last
            (List.concat_map (member_pieces ~src) members)
      | _ -> write_toml_value w ~format:`Indent ~inline:true v)
  | None -> write_toml_value w ~format:`Indent ~inline:true v

and element_piece ~src v =
  let first, last = Option.get (range_of (V.meta v)) in
  { first; last; write = (fun w -> write_preserved ~src w v) }

(* A table member is its key and its value, except under a [[section]] header,
   where the key is spelled inside the header line and so falls within the
   value's own range rather than beside it. *)
and member_pieces ~src ((k, km), v) =
  let vfirst, vlast = Option.get (range_of (V.meta v)) in
  let value =
    {
      first = vfirst;
      last = vlast;
      write = (fun w -> write_preserved ~src w v);
    }
  in
  match range_of km with
  | Some (kfirst, _) when Loc.compare_byte_pos kfirst vfirst >= 0 -> [ value ]
  | Some (kfirst, klast) ->
      let write w =
        if Option.is_some (Loc.Meta.text km) then write_range w src kfirst klast
        else write_toml_key w k
      in
      [ { first = kfirst; last = klast; write }; value ]
  | None -> [ value ]

(* Write [value] as the document it was read from. A document the parser found
   nothing in is still a document -- an empty file, or one that is only
   comments -- and is written as it stands. *)
let encode_preserved w value =
  let m = V.meta value in
  match (Loc.Meta.text m, range_of m) with
  | Some src, None -> Bytes.Writer.write_string w src
  | Some src, Some (first, last) when is_spliceable ~headed:false src value ->
      write_range w src 0 (first - 1);
      write_preserved ~src w value;
      write_range w src (last + 1) (String.length src - 1)
  | _ ->
      let has_content = ref false in
      encode_at_path w ~format:`Indent has_content [] value

(* Streaming TOML encoder dispatched on the internal [format] discriminator.
   [`Indent] is the pretty-printed output with [section] headers, [`Minify]
   produces one-line inline TOML, and [`Layout] writes a parsed document back as
   the bytes it was read from. *)
let encode_to_writer ~format w value =
  match format with
  | `Minify -> encode_minified w value
  | `Layout -> encode_preserved w value
  | `Indent ->
      let has_content = ref false in
      encode_at_path w ~format has_content [] value

(* ============================================ Public Interface - Parsing
   ============================================ *)

let of_string input = try Ok (parse_toml input) with Loc.Error e -> Error e

let of_reader ?file r =
  try Ok (parse_toml_of_reader ?file r) with Loc.Error e -> Error e

let parse = parse_toml

let parse_reader ?file ?max_depth ?max_nodes r =
  parse_toml_of_reader ?file ?max_depth ?max_nodes r

(* ============================================ Public Interface - Encoding
   ============================================ *)

let to_writer ?indent ?(preserve = false) w value =
  let format = format_of ~indent ~preserve in
  encode_to_writer ~format w value

let to_string ?indent ?(preserve = false) value =
  let format = format_of ~indent ~preserve in
  let buf = Buffer.create 256 in
  let w = Bytes.Writer.of_buffer buf in
  encode_to_writer ~format w value;
  Buffer.contents buf

(* ============================================ Tagged JSON Module
   ============================================ *)

module Tagged_json = struct
  let encode = tagged_of_toml_json
  let decode = decode_tagged_json_string
  let float_to_tagged_json_str = float_to_tagged_json_str

  let decode_and_encode_toml json_str =
    try
      let toml = decode_tagged_json_string json_str in
      Ok (to_string toml)
    with
    | Failure msg -> Error msg
    | e -> Error (Printexc.to_string e)
end
