(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Low-level HTTP/1.1 client over raw TCP connections for connection pooling

    This module orchestrates [Http_write] for request serialization and
    [Http_read] for response parsing, leveraging Eio's Buf_write and Buf_read
    for efficient I/O.

    Types are imported from domain-specific modules ({!Response_limits},
    {!Expect_continue}) and re-exported for API convenience. *)

let src = Logs.Src.create "requests.http_client" ~doc:"Low-level HTTP client"
module Log = (val Logs.src_log src : Logs.LOG)

(** {1 Types}

    Re-exported from domain-specific modules for API convenience. *)

type limits = Response_limits.t
let default_limits = Response_limits.default

type expect_100_config = Expect_continue.t
let default_expect_100_config = Expect_continue.default

(** {1 Decompression Support} *)

(** Generic decompression helper that handles common setup and result handling.
    The [uncompress] function receives refill/flush callbacks and input/output buffers. *)
let decompress_with ~name ~uncompress data =
  Log.debug (fun m -> m "Decompressing %s data (%d bytes)" name (String.length data));
  let i = De.bigstring_create De.io_buffer_size in
  let o = De.bigstring_create De.io_buffer_size in
  let r = Buffer.create (String.length data * 2) in
  let p = ref 0 in
  let refill buf =
    let len = min (String.length data - !p) De.io_buffer_size in
    Bigstringaf.blit_from_string data ~src_off:!p buf ~dst_off:0 ~len;
    p := !p + len;
    len
  in
  let flush buf len =
    Buffer.add_string r (Bigstringaf.substring buf ~off:0 ~len)
  in
  match uncompress ~refill ~flush i o with
  | Ok _ ->
      let result = Buffer.contents r in
      Log.debug (fun m -> m "%s decompression succeeded: %d -> %d bytes"
        name (String.length data) (String.length result));
      Some result
  | Error (`Msg e) ->
      Log.warn (fun m -> m "%s decompression failed: %s" name e);
      None

(** Decompress gzip-encoded data. Returns [Some decompressed] on success, [None] on failure. *)
let decompress_gzip data =
  decompress_with ~name:"gzip" data
    ~uncompress:(fun ~refill ~flush i o -> Gz.Higher.uncompress ~refill ~flush i o)

(** Decompress deflate-encoded data (raw DEFLATE, RFC 1951). Returns [Some decompressed] on success, [None] on failure. *)
let decompress_deflate data =
  let w = De.make_window ~bits:15 in
  decompress_with ~name:"deflate" data
    ~uncompress:(fun ~refill ~flush i o -> De.Higher.uncompress ~w ~refill ~flush i o)

(** Decompress zlib-encoded data (DEFLATE with zlib header, RFC 1950). Returns [Some decompressed] on success, [None] on failure. *)
let decompress_zlib data =
  let allocate bits = De.make_window ~bits in
  decompress_with ~name:"zlib" data
    ~uncompress:(fun ~refill ~flush i o -> Zl.Higher.uncompress ~allocate ~refill ~flush i o)

(** {1 Decompression Bomb Prevention}

    Per Recommendation #25: Check decompressed size and ratio limits *)

let check_decompression_limits ~limits ~compressed_size decompressed =
  let decompressed_size = Int64.of_int (String.length decompressed) in
  let compressed_size_i64 = Int64.of_int compressed_size in
  let max_decompressed = Response_limits.max_decompressed_size limits in

  (* Check absolute size *)
  if decompressed_size > max_decompressed then begin
    let ratio = Int64.to_float decompressed_size /. Int64.to_float compressed_size_i64 in
    raise (Error.err (Error.Decompression_bomb {
      limit = max_decompressed;
      ratio
    }))
  end;

  (* Check ratio - only if compressed size is > 0 to avoid division by zero *)
  if compressed_size > 0 then begin
    let ratio = Int64.to_float decompressed_size /. Int64.to_float compressed_size_i64 in
    if ratio > Response_limits.max_compression_ratio limits then
      raise (Error.err (Error.Decompression_bomb {
        limit = max_decompressed;
        ratio
      }))
  end;

  decompressed

(** Decompress body based on Content-Encoding header with limits *)
let decompress_body ~limits ~content_encoding body =
  let encoding = String.lowercase_ascii (String.trim content_encoding) in
  let compressed_size = String.length body in
  match encoding with
  | "gzip" | "x-gzip" ->
      (match decompress_gzip body with
       | Some decompressed -> check_decompression_limits ~limits ~compressed_size decompressed
       | None -> body)  (* Fall back to raw body on error *)
  | "deflate" ->
      (* "deflate" in HTTP can mean either raw DEFLATE or zlib-wrapped.
         Many servers send zlib-wrapped data despite the spec. Try zlib first,
         then fall back to raw deflate. *)
      (match decompress_zlib body with
       | Some decompressed -> check_decompression_limits ~limits ~compressed_size decompressed
       | None ->
           match decompress_deflate body with
           | Some decompressed -> check_decompression_limits ~limits ~compressed_size decompressed
           | None -> body)
  | "identity" | "" -> body
  | other ->
      Log.warn (fun m -> m "Unknown Content-Encoding '%s', returning raw body" other);
      body

(** {1 Request Execution} *)

(** Write request body to flow, handling empty, chunked, and fixed-length bodies *)
let write_body_to_flow ~sw flow body =
  Http_write.write_and_flush flow (fun w ->
    if Body.Private.is_empty body then
      ()
    else if Body.Private.is_chunked body then
      Body.Private.write_chunked ~sw w body
    else
      Body.Private.write ~sw w body
  )

(** Apply auto-decompression to response if enabled *)
let maybe_decompress ~limits ~auto_decompress (status, resp_headers, body_str) =
  match auto_decompress, Headers.get `Content_encoding resp_headers with
  | true, Some encoding ->
      let body_str = decompress_body ~limits ~content_encoding:encoding body_str in
      let resp_headers = Headers.remove `Content_encoding resp_headers in
      (status, resp_headers, body_str)
  | _ ->
      (status, resp_headers, body_str)

(** Make HTTP request over a pooled connection using Buf_write/Buf_read *)
let make_request ?(limits=default_limits) ~sw ~method_ ~uri ~headers ~body flow =
  Log.debug (fun m -> m "Making %s request to %s" (Method.to_string method_) (Uri.to_string uri));

  (* Write request using Buf_write - use write_and_flush to avoid nested switch issues *)
  Http_write.write_and_flush flow (fun w ->
    Http_write.request w ~sw ~method_ ~uri ~headers ~body
  );

  (* Read response using Buf_read *)
  let buf_read = Http_read.of_flow flow ~max_size:max_int in
  let (_version, status, headers, body) = Http_read.response ~limits ~method_ buf_read in
  (status, headers, body)

(** Make HTTP request with optional auto-decompression *)
let make_request_decompress ?(limits=default_limits) ~sw ~method_ ~uri ~headers ~body ~auto_decompress flow =
  make_request ~limits ~sw ~method_ ~uri ~headers ~body flow
  |> maybe_decompress ~limits ~auto_decompress

(** {1 HTTP 100-Continue Protocol Implementation}

    Per Recommendation #7: HTTP 100-Continue Support for Large Uploads.
    RFC 9110 Section 10.1.1 (Expect) and Section 15.2.1 (100 Continue)

    The 100-continue protocol allows:
    1. Client sends headers with [Expect: 100-continue]
    2. Server responds with 100 Continue (proceed) or error (4xx/5xx)
    3. Client sends body only if 100 Continue received
    4. Server sends final response

    This saves bandwidth when server would reject based on headers alone. *)

(** Result of waiting for 100-continue response *)
type expect_100_result =
  | Continue              (** Server sent 100 Continue - proceed with body *)
  | Rejected of int * Headers.t * string  (** Server rejected - status, headers, body *)
  | Timeout               (** Timeout expired - proceed with body anyway *)

(** Wait for 100 Continue or error response with timeout.
    Returns Continue, Rejected, or Timeout. *)
let wait_for_100_continue ~limits ~timeout:_ flow =
  Log.debug (fun m -> m "Waiting for 100 Continue response");

  let buf_read = Http_read.of_flow flow ~max_size:max_int in

  try
    let (_version, status) = Http_read.status_line buf_read in

    Log.debug (fun m -> m "Received response status %d while waiting for 100 Continue" status);

    if status = 100 then begin
      (* 100 Continue - read any headers (usually none) and return Continue *)
      let _ = Http_read.headers ~limits buf_read in
      Log.info (fun m -> m "Received 100 Continue, proceeding with body");
      Continue
    end else begin
      (* Error response - server rejected based on headers *)
      Log.info (fun m -> m "Server rejected request with status %d before body sent" status);
      let resp_headers = Http_read.headers ~limits buf_read in
      let transfer_encoding = Headers.get `Transfer_encoding resp_headers in
      let content_length = Headers.get `Content_length resp_headers |> Option.map Int64.of_string in
      let body_str = match transfer_encoding, content_length with
        | Some te, _ when String.lowercase_ascii te |> String.trim = "chunked" ->
            Http_read.chunked_body ~limits buf_read
        | _, Some len -> Http_read.fixed_body ~limits ~length:len buf_read
        | _ -> ""
      in
      Rejected (status, resp_headers, body_str)
    end
  with
  | Eio.Buf_read.Buffer_limit_exceeded ->
      Log.warn (fun m -> m "Buffer limit exceeded waiting for 100 Continue");
      Timeout
  | End_of_file ->
      Log.warn (fun m -> m "Connection closed waiting for 100 Continue");
      Timeout

(** Make HTTP request with 100-continue support for large bodies.

    If the body exceeds the threshold and 100-continue is enabled:
    1. Sends headers with Expect: 100-continue
    2. Waits for server response (100 Continue or error)
    3. Sends body only if 100 Continue received
    4. Otherwise returns the error response without sending body

    Per RFC 9110:
    - If timeout expires, client should send body anyway
    - 417 Expectation Failed means server doesn't support Expect header
    - Any error response (4xx/5xx) should be returned without sending body *)
let make_request_100_continue
    ?(limits=default_limits)
    ?(expect_100=default_expect_100_config)
    ~clock
    ~sw
    ~method_
    ~uri
    ~headers
    ~body
    flow =
  let body_len = Body.content_length body |> Option.value ~default:0L in

  (* Determine if we should use 100-continue *)
  let use_100_continue =
    Expect_continue.enabled expect_100 &&
    body_len >= Expect_continue.threshold expect_100 &&
    body_len > 0L &&
    not (Headers.mem `Expect headers)  (* Don't override explicit Expect header *)
  in

  if not use_100_continue then begin
    (* Standard request without 100-continue *)
    Log.debug (fun m -> m "100-continue not used (body_len=%Ld, threshold=%Ld, enabled=%b)"
      body_len (Expect_continue.threshold expect_100) (Expect_continue.enabled expect_100));
    make_request ~limits ~sw ~method_ ~uri ~headers ~body flow
  end else begin
    Log.info (fun m -> m "Using 100-continue for large body (%Ld bytes)" body_len);

    (* Add Expect: 100-continue header and Content-Type if present *)
    let headers_with_expect = Headers.expect_100_continue headers in
    let headers_with_expect = match Body.content_type body with
      | Some mime -> Headers.add `Content_type (Mime.to_string mime) headers_with_expect
      | None -> headers_with_expect
    in

    (* Send headers only using Buf_write *)
    Http_write.write_and_flush flow (fun w ->
      Http_write.request_headers_only w ~method_ ~uri
        ~headers:headers_with_expect ~content_length:(Some body_len)
    );

    (* Wait for 100 Continue or error response with timeout *)
    let result =
      try
        Eio.Time.with_timeout_exn clock (Expect_continue.timeout expect_100) (fun () ->
          wait_for_100_continue ~limits ~timeout:(Expect_continue.timeout expect_100) flow
        )
      with Eio.Time.Timeout ->
        Log.debug (fun m -> m "100-continue timeout expired, sending body anyway");
        Timeout
    in

    match result with
    | Continue ->
        (* Server said continue - send body and read final response *)
        Log.debug (fun m -> m "Sending body after 100 Continue");
        write_body_to_flow ~sw flow body;

        (* Read final response *)
        let buf_read = Http_read.of_flow flow ~max_size:max_int in
        let (_version, status, headers, body) = Http_read.response ~limits ~method_ buf_read in
        (status, headers, body)

    | Rejected (status, resp_headers, resp_body_str) ->
        (* RFC 9110 Section 10.1.1: If we receive 417 Expectation Failed, retry
           without the 100-continue expectation *)
        if status = 417 then begin
          Log.info (fun m -> m "Received 417 Expectation Failed, retrying without Expect header");
          (* Make a fresh request without Expect: 100-continue *)
          make_request ~limits ~sw ~method_ ~uri ~headers ~body flow
        end else begin
          (* Server rejected with non-417 error - return error response without sending body *)
          Log.info (fun m -> m "Request rejected with status %d, body not sent (saved %Ld bytes)"
            status body_len);
          (status, resp_headers, resp_body_str)
        end

    | Timeout ->
        (* Timeout expired - send body anyway per RFC 9110 *)
        Log.debug (fun m -> m "Sending body after timeout");
        write_body_to_flow ~sw flow body;

        (* Read response *)
        let buf_read = Http_read.of_flow flow ~max_size:max_int in
        let (_version, status, headers, body) = Http_read.response ~limits ~method_ buf_read in
        (status, headers, body)
  end

(** Make HTTP request with 100-continue support and optional auto-decompression *)
let make_request_100_continue_decompress
    ?(limits=default_limits)
    ?(expect_100=default_expect_100_config)
    ~clock ~sw ~method_ ~uri ~headers ~body ~auto_decompress flow =
  make_request_100_continue ~limits ~expect_100 ~clock ~sw ~method_ ~uri ~headers ~body flow
  |> maybe_decompress ~limits ~auto_decompress
