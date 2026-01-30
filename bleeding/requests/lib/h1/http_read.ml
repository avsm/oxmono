(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** HTTP response parsing using Eio.Buf_read combinators

    This module provides efficient HTTP/1.1 response parsing using Eio's
    buffered read API with parser combinators for clean, composable parsing. *)

let src = Logs.Src.create "requests.http_read" ~doc:"HTTP response parsing"
module Log = (val Logs.src_log src : Logs.LOG)

module Read = Eio.Buf_read

(** Import limits from Response_limits module. *)
type limits = Response_limits.t

(** {1 Character Predicates} *)

(** HTTP version characters: letters, digits, slash, dot *)
let is_version_char = function
  | 'A'..'Z' | 'a'..'z' | '0'..'9' | '/' | '.' -> true
  | _ -> false

(** HTTP status code digits *)
let is_digit = function
  | '0'..'9' -> true
  | _ -> false

(** RFC 9110 token characters for header names *)
let is_token_char = function
  | 'A'..'Z' | 'a'..'z' | '0'..'9' -> true
  | '!' | '#' | '$' | '%' | '&' | '\'' | '*' | '+' | '-' | '.' -> true
  | '^' | '_' | '`' | '|' | '~' -> true
  | _ -> false

(** Hex digits for chunk size *)
let is_hex_digit = function
  | '0'..'9' | 'a'..'f' | 'A'..'F' -> true
  | _ -> false

(** Optional whitespace *)
let is_ows = function
  | ' ' | '\t' -> true
  | _ -> false

(** {1 Security Validation}

    Per RFC 9112 Section 2.2: bare CR MUST be rejected to prevent
    HTTP request smuggling attacks. *)

(** Maximum chunk size hex digits (16 hex digits = 64-bit max) *)
let max_chunk_size_hex_digits = 16

(** Validate that a string contains no bare CR characters.
    A bare CR is a CR not followed by LF, which can be used for
    HTTP request smuggling attacks.
    @raise Error.t if bare CR is found. *)
let validate_no_bare_cr ~context s =
  let len = String.length s in
  for i = 0 to len - 1 do
    if s.[i] = '\r' then begin
      if i + 1 >= len || s.[i + 1] <> '\n' then
        raise (Error.invalid_requestf "Bare CR in %s (potential HTTP smuggling attack)" context)
    end
  done

(** {1 Low-level Parsers} *)

let sp = Read.char ' '

let http_version r =
  Read.take_while is_version_char r

let status_code r =
  let code_str = Read.take_while is_digit r in
  if String.length code_str <> 3 then
    raise (Error.invalid_requestf "Invalid status code: %s" code_str);
  try int_of_string code_str
  with _ ->
    raise (Error.invalid_requestf "Invalid status code: %s" code_str)

let reason_phrase r =
  Read.line r

(** {1 HTTP Version Type}

    Per Recommendation #26: Expose HTTP version used for the response *)

type http_version =
  | HTTP_1_0
  | HTTP_1_1

let http_version_of_string = function
  | "HTTP/1.0" -> HTTP_1_0
  | "HTTP/1.1" -> HTTP_1_1
  | v -> raise (Error.invalid_requestf "Invalid HTTP version: %s" v)

let http_version_to_string = function
  | HTTP_1_0 -> "HTTP/1.0"
  | HTTP_1_1 -> "HTTP/1.1"

(** {1 Status Line Parser} *)

let status_line r =
  let version_str = http_version r in
  (* Parse and validate HTTP version *)
  let version = http_version_of_string version_str in
  sp r;
  let code = status_code r in
  sp r;
  let reason = reason_phrase r in
  (* RFC 9112 Section 2.2: Validate no bare CR in reason phrase *)
  validate_no_bare_cr ~context:"reason phrase" reason;
  Log.debug (fun m -> m "Parsed status line: %s %d" version_str code);
  (version, code)

(** {1 Header Parsing} *)

(** Parse a single header line. Returns ("", "") for empty line (end of headers).
    Handles obs-fold (RFC 9112 Section 5.2): continuation lines starting with
    whitespace are merged into the previous header value with a single space.
    Per RFC 9112 Section 2.2: validates that no bare CR characters are present. *)
let header_line r =
  let name = Read.take_while is_token_char r in
  if name = "" then begin
    (* Empty line - end of headers. Consume the CRLF. *)
    let line = Read.line r in
    if line <> "" then
      raise (Error.err (Error.Invalid_request {
        reason = "Expected empty line but got: " ^ line
      }));
    ("", "")
  end else begin
    Read.char ':' r;
    Read.skip_while is_ows r;
    let value = Read.line r in
    (* RFC 9112 Section 2.2: Validate no bare CR in header value *)
    validate_no_bare_cr ~context:"header value" value;
    (* RFC 9112 Section 5.2: Handle obs-fold (obsolete line folding)
       A recipient of an obs-fold MUST replace each obs-fold with one or more
       SP octets prior to interpreting the field value. *)
    let rec collect_obs_fold acc =
      match Read.peek_char r with
      | Some (' ' | '\t') ->
          (* obs-fold: continuation line starts with whitespace *)
          Log.debug (fun m -> m "Handling obs-fold continuation for header %s" name);
          Read.skip_while is_ows r;
          let continuation = Read.line r in
          (* Validate continuation for bare CR *)
          validate_no_bare_cr ~context:"header continuation" continuation;
          (* Replace obs-fold with single space and continue *)
          collect_obs_fold (acc ^ " " ^ String.trim continuation)
      | _ -> acc
    in
    let full_value = collect_obs_fold value in
    (String.lowercase_ascii name, String.trim full_value)
  end

(** Parse all headers with size and count limits *)
let headers ~limits r =
  let max_count = Response_limits.max_header_count limits in
  let max_size = Response_limits.max_header_size limits in
  let rec loop acc count =
    (* Check header count limit *)
    if count >= max_count then
      raise (Error.err (Error.Headers_too_large {
        limit = max_count;
        actual = count + 1
      }));

    let (name, value) = header_line r in

    if name = "" then begin
      (* End of headers *)
      Log.debug (fun m -> m "Parsed %d headers" count);
      Headers.of_list (List.rev acc)
    end else begin
      (* Check header line size limit *)
      let line_len = String.length name + String.length value + 2 in
      if line_len > max_size then
        raise (Error.err (Error.Headers_too_large {
          limit = max_size;
          actual = line_len
        }));

      loop ((name, value) :: acc) (count + 1)
    end
  in
  loop [] 0

(** {1 Body Parsing} *)

(** Read body until connection close (close-delimited message).
    Per RFC 9112 Section 6.3 item 8: When no Transfer-Encoding or Content-Length
    is present, the body length is determined by reading until connection close. *)
let close_delimited_body ~limits r =
  let max_body = Response_limits.max_response_body_size limits in
  Log.debug (fun m -> m "Reading close-delimited body (until EOF)");

  let buf = Buffer.create 8192 in
  let bytes_read = ref 0L in

  let rec read_until_eof () =
    (* Check size limit *)
    if !bytes_read > max_body then
      raise (Error.err (Error.Body_too_large {
        limit = max_body;
        actual = Some !bytes_read
      }));

    (* Try to read a chunk - at_end_of_input returns true when EOF reached *)
    if Read.at_end_of_input r then
      Buffer.contents buf
    else begin
      (* Read up to 8KB at a time *)
      let chunk = Read.take_while (fun _ -> true) r in
      let chunk_len = String.length chunk in
      if chunk_len > 0 then begin
        Buffer.add_string buf chunk;
        bytes_read := Int64.add !bytes_read (Int64.of_int chunk_len);
        read_until_eof ()
      end else
        (* No more data available *)
        Buffer.contents buf
    end
  in
  read_until_eof ()

(** Read a fixed-length body with size limit checking *)
let fixed_body ~limits ~length r =
  let max_body = Response_limits.max_response_body_size limits in
  (* Check size limit before allocating *)
  if length > max_body then
    raise (Error.err (Error.Body_too_large {
      limit = max_body;
      actual = Some length
    }));

  Log.debug (fun m -> m "Reading fixed-length body: %Ld bytes" length);

  let len_int = Int64.to_int length in
  let buf = Buffer.create len_int in
  let bytes_read = ref 0L in

  let rec read_n remaining =
    if remaining > 0L then begin
      let to_read = min 8192 (Int64.to_int remaining) in
      let chunk = Read.take to_read r in
      let chunk_len = String.length chunk in

      if chunk_len = 0 then
        (* Connection closed prematurely - Content-Length mismatch *)
        raise (Error.err (Error.Content_length_mismatch {
          expected = length;
          actual = !bytes_read
        }))
      else begin
        Buffer.add_string buf chunk;
        bytes_read := Int64.add !bytes_read (Int64.of_int chunk_len);
        read_n (Int64.sub remaining (Int64.of_int chunk_len))
      end
    end
  in
  read_n length;
  Buffer.contents buf

(** Parse chunk size line (hex size with optional extensions).
    Per RFC 9112 Section 7.1: protect against chunk size overflow attacks. *)
let chunk_size r =
  let hex_str = Read.take_while is_hex_digit r in
  if hex_str = "" then
    raise (Error.err (Error.Invalid_request {
      reason = "Empty chunk size"
    }));
  (* Protect against overflow: limit hex digits to prevent parsing huge numbers.
     16 hex digits = 64-bit max, which is way more than any reasonable chunk. *)
  if String.length hex_str > max_chunk_size_hex_digits then
    raise (Error.invalid_requestf "Chunk size too large (%d hex digits, max %d)"
      (String.length hex_str) max_chunk_size_hex_digits);
  (* Skip any chunk extensions (after semicolon) - validate for bare CR *)
  let extensions = Read.take_while (fun c -> c <> '\r' && c <> '\n') r in
  validate_no_bare_cr ~context:"chunk extension" extensions;
  let _ = Read.line r in  (* Consume CRLF *)
  try int_of_string ("0x" ^ hex_str)
  with _ ->
    raise (Error.invalid_requestf "Invalid chunk size: %s" hex_str)

(** {1 Trailer Header Parsing}

    Per RFC 9112 Section 7.1.2: Trailer section can contain headers after the
    final chunk. Certain headers MUST NOT be in trailers (hop-by-hop, content-*, etc.). *)

(** Headers that MUST NOT appear in trailers per RFC 9110 Section 6.5.1 *)
let forbidden_trailer_headers = [
  "transfer-encoding"; "content-length"; "host"; "content-encoding";
  "content-type"; "content-range"; "trailer"
]

(** Parse trailer headers after final chunk.
    Returns parsed headers. Forbidden trailer headers are logged and ignored. *)
let parse_trailers ~limits r =
  let max_count = Response_limits.max_header_count limits in
  let max_size = Response_limits.max_header_size limits in
  let rec loop acc count =
    if count >= max_count then begin
      Log.warn (fun m -> m "Trailer count limit reached (%d), skipping remaining" max_count);
      Headers.of_list (List.rev acc)
    end else begin
      let line = Read.line r in
      if line = "" then
        (* End of trailers *)
        Headers.of_list (List.rev acc)
      else
        (* Parse trailer line *)
        match String.index_opt line ':' with
        | None ->
            Log.warn (fun m -> m "Invalid trailer line (no colon): %s" line);
            loop acc count
        | Some colon_idx ->
            let name = String.sub line 0 colon_idx |> String.trim |> String.lowercase_ascii in
            let value = String.sub line (colon_idx + 1) (String.length line - colon_idx - 1) |> String.trim in
            (* Check header size *)
            let line_len = String.length name + String.length value + 2 in
            if line_len > max_size then begin
              Log.warn (fun m -> m "Trailer header too large (%d > %d), skipping: %s" line_len max_size name);
              loop acc count
            end else if List.mem name forbidden_trailer_headers then begin
              Log.warn (fun m -> m "Forbidden header in trailers, ignoring: %s" name);
              loop acc count
            end else
              loop ((name, value) :: acc) (count + 1)
    end
  in
  loop [] 0

(** Skip trailer headers after final chunk (legacy compatibility) *)
let skip_trailers r =
  let rec loop () =
    let line = Read.line r in
    if line <> "" then loop ()
  in
  loop ()

(** Read a chunked transfer-encoded body with size limit checking *)
let chunked_body ~limits r =
  Log.debug (fun m -> m "Reading chunked body");
  let max_body = Response_limits.max_response_body_size limits in
  let buf = Buffer.create 4096 in
  let total_size = ref 0L in

  let rec read_chunks () =
    let size = chunk_size r in

    if size = 0 then begin
      (* Final chunk - skip trailers *)
      skip_trailers r;
      Log.debug (fun m -> m "Chunked body complete: %Ld bytes" !total_size);
      Buffer.contents buf
    end else begin
      (* Check size limit before reading chunk *)
      let new_total = Int64.add !total_size (Int64.of_int size) in
      if new_total > max_body then
        raise (Error.err (Error.Body_too_large {
          limit = max_body;
          actual = Some new_total
        }));

      let chunk = Read.take size r in
      Buffer.add_string buf chunk;
      total_size := new_total;
      let _ = Read.line r in  (* Consume trailing CRLF *)
      read_chunks ()
    end
  in
  read_chunks ()

(** {1 Streaming Body Sources} *)

(** A flow source that reads from a Buf_read with a fixed length limit *)
module Fixed_body_source = struct
  type t = {
    buf_read : Read.t;
    mutable remaining : int64;
  }

  let single_read t dst =
    if t.remaining <= 0L then raise End_of_file;

    let to_read = min (Cstruct.length dst) (Int64.to_int (min t.remaining 8192L)) in

    (* Ensure data is available *)
    Read.ensure t.buf_read to_read;
    let src = Read.peek t.buf_read in
    let actual = min to_read (Cstruct.length src) in

    Cstruct.blit src 0 dst 0 actual;
    Read.consume t.buf_read actual;
    t.remaining <- Int64.sub t.remaining (Int64.of_int actual);
    actual

  let read_methods = []
end

let fixed_body_stream ~limits ~length buf_read =
  let max_body = Response_limits.max_response_body_size limits in
  (* Check size limit *)
  if length > max_body then
    raise (Error.err (Error.Body_too_large {
      limit = max_body;
      actual = Some length
    }));

  let t = { Fixed_body_source.buf_read; remaining = length } in
  let ops = Eio.Flow.Pi.source (module Fixed_body_source) in
  Eio.Resource.T (t, ops)

(** A flow source that reads chunked transfer encoding from a Buf_read *)
module Chunked_body_source = struct
  type state =
    | Reading_size
    | Reading_chunk of int
    | Reading_chunk_end
    | Done

  type t = {
    buf_read : Read.t;
    mutable state : state;
    mutable total_read : int64;
    max_body_size : int64;
  }

  let read_chunk_size t =
    let hex_str = Read.take_while is_hex_digit t.buf_read in
    if hex_str = "" then 0
    else begin
      (* Protect against overflow: limit hex digits *)
      if String.length hex_str > max_chunk_size_hex_digits then
        raise (Error.err (Error.Invalid_request {
          reason = Printf.sprintf "Chunk size too large (%d hex digits)"
            (String.length hex_str)
        }));
      (* Skip extensions and CRLF - validate for bare CR *)
      let extensions = Read.take_while (fun c -> c <> '\r' && c <> '\n') t.buf_read in
      validate_no_bare_cr ~context:"chunk extension" extensions;
      let _ = Read.line t.buf_read in
      try int_of_string ("0x" ^ hex_str)
      with _ -> 0
    end

  let single_read t dst =
    let rec aux () =
      match t.state with
      | Done -> raise End_of_file
      | Reading_size ->
          let size = read_chunk_size t in
          if size = 0 then begin
            (* Skip trailers *)
            let rec skip () =
              let line = Read.line t.buf_read in
              if line <> "" then skip ()
            in
            skip ();
            t.state <- Done;
            raise End_of_file
          end else begin
            (* Check size limit *)
            let new_total = Int64.add t.total_read (Int64.of_int size) in
            if new_total > t.max_body_size then
              raise (Error.err (Error.Body_too_large {
                limit = t.max_body_size;
                actual = Some new_total
              }));
            t.state <- Reading_chunk size;
            aux ()
          end
      | Reading_chunk remaining ->
          let to_read = min (Cstruct.length dst) remaining in
          Read.ensure t.buf_read to_read;
          let src = Read.peek t.buf_read in
          let actual = min to_read (Cstruct.length src) in
          Cstruct.blit src 0 dst 0 actual;
          Read.consume t.buf_read actual;
          t.total_read <- Int64.add t.total_read (Int64.of_int actual);
          let new_remaining = remaining - actual in
          if new_remaining = 0 then
            t.state <- Reading_chunk_end
          else
            t.state <- Reading_chunk new_remaining;
          actual
      | Reading_chunk_end ->
          let _ = Read.line t.buf_read in  (* Consume trailing CRLF *)
          t.state <- Reading_size;
          aux ()
    in
    aux ()

  let read_methods = []
end

let chunked_body_stream ~limits buf_read =
  let t = {
    Chunked_body_source.buf_read;
    state = Reading_size;
    total_read = 0L;
    max_body_size = Response_limits.max_response_body_size limits;
  } in
  let ops = Eio.Flow.Pi.source (module Chunked_body_source) in
  Eio.Resource.T (t, ops)

(** A flow source that reads until connection close (close-delimited).
    Per RFC 9112 Section 6.3 item 8: When no Transfer-Encoding or Content-Length
    is present, the body length is determined by reading until connection close. *)
module Close_delimited_source = struct
  type t = {
    buf_read : Read.t;
    mutable total_read : int64;
    max_body_size : int64;
    mutable eof : bool;
  }

  let single_read t dst =
    if t.eof then raise End_of_file;

    (* Check size limit *)
    if t.total_read > t.max_body_size then
      raise (Error.err (Error.Body_too_large {
        limit = t.max_body_size;
        actual = Some t.total_read
      }));

    if Read.at_end_of_input t.buf_read then begin
      t.eof <- true;
      raise End_of_file
    end;

    let to_read = min (Cstruct.length dst) 8192 in
    (* Try to ensure data is available, but don't fail on EOF *)
    (try Read.ensure t.buf_read 1 with End_of_file ->
      t.eof <- true;
      raise End_of_file);

    let src = Read.peek t.buf_read in
    let available = Cstruct.length src in
    if available = 0 then begin
      t.eof <- true;
      raise End_of_file
    end;

    let actual = min to_read available in
    Cstruct.blit src 0 dst 0 actual;
    Read.consume t.buf_read actual;
    t.total_read <- Int64.add t.total_read (Int64.of_int actual);
    actual

  let read_methods = []
end

let close_delimited_body_stream ~limits buf_read =
  let t = {
    Close_delimited_source.buf_read;
    total_read = 0L;
    max_body_size = Response_limits.max_response_body_size limits;
    eof = false;
  } in
  let ops = Eio.Flow.Pi.source (module Close_delimited_source) in
  Eio.Resource.T (t, ops)

(** {1 High-level Response Parsing} *)

(** Check if response should have no body per
    {{:https://datatracker.ietf.org/doc/html/rfc9110#section-6.4.1}RFC 9110 Section 6.4.1}:
    {ul
    {- Any response to a HEAD request}
    {- 2xx (Successful) response to a CONNECT request (switches to tunnel mode)}
    {- Any 1xx (Informational) response}
    {- 204 (No Content) response}
    {- 304 (Not Modified) response}} *)
let response_has_no_body ~method_ ~status =
  match method_, status with
  | Some `HEAD, _ -> true
  | Some `CONNECT, s when s >= 200 && s < 300 -> true
  | _, s when s >= 100 && s < 200 -> true
  | _, 204 | _, 304 -> true
  | _ -> false

(** {1 Transfer-Encoding Validation}

    Per RFC 9112 Section 6.1: Transfer-Encoding is a list of transfer codings.
    If "chunked" is present, it MUST be the final encoding. The encodings are
    applied in order, so we must reject unknown encodings that appear before chunked.

    Per RFC 9112 Section 6.1: A server MUST NOT send Transfer-Encoding in:
    - A response to a HEAD request
    - Any 1xx (Informational) response
    - A 204 (No Content) response
    - A 304 (Not Modified) response *)

(** Validate that Transfer-Encoding is not present in responses that MUST NOT have it.
    Per RFC 9112 Section 6.1: These responses must not include Transfer-Encoding.
    If present, this is a protocol violation but we log and continue.
    @return true if Transfer-Encoding is present (violation), false otherwise *)
let validate_no_transfer_encoding ~method_ ~status transfer_encoding =
  let should_not_have_te =
    match method_, status with
    | Some `HEAD, _ -> true   (* HEAD responses must not have TE *)
    | _, s when s >= 100 && s < 200 -> true  (* 1xx responses *)
    | _, 204 -> true          (* 204 No Content *)
    | _, 304 -> true          (* 304 Not Modified *)
    | _ -> false
  in
  match transfer_encoding, should_not_have_te with
  | Some te, true ->
      Log.warn (fun m -> m "RFC 9112 violation: Transfer-Encoding '%s' in %s response \
        (status %d) - ignoring per spec" te
        (match method_ with Some `HEAD -> "HEAD" | _ -> "bodiless")
        status);
      true
  | _ -> false

(** Parse Transfer-Encoding header into list of codings.
    Returns list in order (first coding is outermost) *)
let parse_transfer_encoding = function
  | None -> []
  | Some te ->
      String.split_on_char ',' te
      |> List.map (fun s -> String.trim (String.lowercase_ascii s))
      |> List.filter (fun s -> s <> "")

(** Validate Transfer-Encoding per RFC 9112 Section 6.1.
    Returns [`Chunked] if chunked encoding should be used, [`None] if no body,
    or raises an error for invalid encodings.
    @raise Error.t if chunked is not final or unknown encodings precede chunked *)
let validate_transfer_encoding encodings =
  match encodings with
  | [] -> `None
  | codings ->
      (* Find position of chunked if present *)
      let chunked_idx =
        List.mapi (fun i c -> (i, c)) codings
        |> List.find_map (fun (i, c) -> if c = "chunked" then Some i else None)
      in
      match chunked_idx with
      | None ->
          (* No chunked encoding - check if we support any of these *)
          Log.warn (fun m -> m "Transfer-Encoding without chunked: %s (not supported)"
            (String.concat ", " codings));
          `Unsupported codings
      | Some idx ->
          (* Per RFC 9112 Section 6.1: chunked MUST be the final transfer coding *)
          if idx <> List.length codings - 1 then begin
            Log.err (fun m -> m "Transfer-Encoding: chunked is not final (RFC 9112 violation)");
            raise (Error.err (Error.Invalid_request {
              reason = "Transfer-Encoding: chunked must be the final encoding"
            }))
          end;
          (* Check encodings before chunked - we only support identity *)
          let before_chunked = List.filteri (fun i _ -> i < idx) codings in
          List.iter (fun enc ->
            match enc with
            | "identity" -> () (* identity is a no-op *)
            | other ->
                Log.warn (fun m -> m "Unsupported encoding '%s' before chunked (treating as identity)" other)
          ) before_chunked;
          `Chunked

(** Helper to check if transfer-encoding indicates chunked *)
let is_chunked_encoding transfer_encoding =
  match validate_transfer_encoding (parse_transfer_encoding transfer_encoding) with
  | `Chunked -> true
  | `None | `Unsupported _ -> false

(** Safely parse Content-Length header, returning None for invalid values.
    Per RFC 9110 Section 8.6: Content-Length must be >= 0.
    @raise Error.t if Content-Length is invalid or negative. *)
let parse_content_length = function
  | None -> None
  | Some s ->
      try
        let len = Int64.of_string s in
        (* Per RFC 9110 Section 8.6: Content-Length MUST be >= 0 *)
        if len < 0L then begin
          Log.warn (fun m -> m "Negative Content-Length rejected: %s" s);
          raise (Error.invalid_requestf "Content-Length cannot be negative: %s" s)
        end;
        Some len
      with Failure _ ->
        Log.warn (fun m -> m "Invalid Content-Length header value: %s" s);
        raise (Error.invalid_requestf "Invalid Content-Length header: %s" s)

(** Parse complete response (status + headers + body) to string.
    Per {{:https://datatracker.ietf.org/doc/html/rfc9112#section-6}RFC 9112 Section 6}}. *)
let response ~limits ?method_ r =
  let version, status = status_line r in
  let hdrs = headers ~limits r in

  (* Per RFC 9112 Section 6.1: Validate Transfer-Encoding not present in bodiless responses *)
  let transfer_encoding = Headers.get `Transfer_encoding hdrs in
  let _ = validate_no_transfer_encoding ~method_ ~status transfer_encoding in

  (* Per RFC 9110 Section 6.4.1: Certain responses MUST NOT have a body *)
  if response_has_no_body ~method_ ~status then (
    Log.debug (fun m -> m "Response has no body (HEAD, CONNECT 2xx, 1xx, 204, or 304)");
    (version, status, hdrs, "")
  ) else
    (* Determine how to read body based on headers.
       Per RFC 9112 Section 6.3: Transfer-Encoding takes precedence over Content-Length *)
    let content_length = parse_content_length (Headers.get `Content_length hdrs) in
    let body = match is_chunked_encoding transfer_encoding, content_length with
      | true, Some _ ->
          (* Both headers present - potential HTTP request smuggling indicator *)
          Log.warn (fun m -> m "Both Transfer-Encoding and Content-Length present - \
            ignoring Content-Length per RFC 9112 (potential attack indicator)");
          chunked_body ~limits r
      | true, None ->
          Log.debug (fun m -> m "Reading chunked response body");
          chunked_body ~limits r
      | false, Some len ->
          Log.debug (fun m -> m "Reading fixed-length response body (%Ld bytes)" len);
          fixed_body ~limits ~length:len r
      | false, None ->
          (match transfer_encoding with
           | Some te ->
               Log.warn (fun m -> m "Unsupported transfer-encoding: %s, assuming no body" te);
               ""
           | None ->
               (* RFC 9112 Section 6.3 item 8: If no Transfer-Encoding or Content-Length,
                  the body length is determined by reading until connection close.
                  This is common for HTTP/1.0 responses. *)
               Log.debug (fun m -> m "No length indicators, reading until connection close");
               close_delimited_body ~limits r)
    in
    (version, status, hdrs, body)

(** Response with streaming body *)
type stream_response = {
  http_version : http_version;
  status : int;
  headers : Headers.t;
  body : [ `String of string
         | `Stream of Eio.Flow.source_ty Eio.Resource.t
         | `None ]
}

let response_stream ~limits ?method_ r =
  let (version, status) = status_line r in
  let hdrs = headers ~limits r in

  (* Per RFC 9112 Section 6.1: Validate Transfer-Encoding not present in bodiless responses *)
  let transfer_encoding = Headers.get `Transfer_encoding hdrs in
  let _ = validate_no_transfer_encoding ~method_ ~status transfer_encoding in

  (* Determine body type *)
  let content_length = parse_content_length (Headers.get `Content_length hdrs) in

  (* Per RFC 9112 Section 6.3: When both Transfer-Encoding and Content-Length
     are present, Transfer-Encoding takes precedence. The presence of both
     headers is a potential HTTP request smuggling attack indicator. *)
  let body = match is_chunked_encoding transfer_encoding, content_length with
    | true, Some _ ->
        (* Both headers present - log warning per RFC 9112 Section 6.3 *)
        Log.warn (fun m -> m "Both Transfer-Encoding and Content-Length present - \
          ignoring Content-Length per RFC 9112 (potential attack indicator)");
        `Stream (chunked_body_stream ~limits r)
    | true, None ->
        Log.debug (fun m -> m "Creating chunked body stream");
        `Stream (chunked_body_stream ~limits r)
    | false, Some len ->
        Log.debug (fun m -> m "Creating fixed-length body stream (%Ld bytes)" len);
        `Stream (fixed_body_stream ~limits ~length:len r)
    | false, None ->
        (match transfer_encoding with
         | Some te ->
             Log.warn (fun m -> m "Unsupported transfer-encoding: %s, assuming no body" te);
             `None
         | None ->
             (* RFC 9112 Section 6.3 item 8: If no Transfer-Encoding or Content-Length,
                the body length is determined by reading until connection close. *)
             Log.debug (fun m -> m "Creating close-delimited body stream");
             `Stream (close_delimited_body_stream ~limits r))
  in

  { http_version = version; status; headers = hdrs; body }

(** {1 Convenience Functions} *)

let of_flow ?initial_size ~max_size flow =
  Read.of_flow ?initial_size ~max_size flow
