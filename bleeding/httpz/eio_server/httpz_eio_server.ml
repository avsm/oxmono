(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Eio integration for httpz - response writing and connection handling.
    Routing comes from {!Httpz_route}. *)

open Base

module I16 = Stdlib_stable.Int16_u
let[@inline] i16 x = I16.of_int x
let[@inline] to_int x = I16.to_int x

module F64 = Stdlib_upstream_compatible.Float_u

type request_info = {
  remote_addr : string;
  meth : Httpz.Method.t;
  target : string;
  path : string;
  host : string or_null;
  user_agent : string or_null;
  referer : string or_null;
  accept : string or_null;
  forwarded_for : string or_null;
  forwarded_proto : string or_null;
  request_headers : (string * string) list;
  status : Httpz.Res.status;
  response_content_type : string or_null;
  cache_status : string or_null;
  timestamp : float#;
  response_body_size : int;
  duration_us : int;
}

(** {1 Response Writing} *)

(* [Res]'s offsets are signed [int16#], so the header block must stay below
   32768 bytes however large [write_buf] is: past that an offset wraps
   negative and the unchecked stores write outside the buffer. Handler-supplied
   headers are unbounded (a long Set-Cookie list, say), so the limit is
   enforced here rather than assumed. Leaves room for the status line,
   Content-Length, Connection and the terminating CRLF appended afterwards. *)
exception Headers_too_large

let max_header_block = 30000

(** Write response headers to buffer using typed Header_name.t.
    Returns header length.
    @raise Headers_too_large if the caller's headers exceed the writable
    range of an [int16#] offset. *)
let rec write_headers_loop buf off (headers : Httpz_route.resp_header list) =
  match headers with
  | [] -> off
  | (name, value) :: rest ->
      if to_int off + String.length value + 64 > max_header_block
      then Stdlib.raise_notrace Headers_too_large;
      let off = Httpz.Res.write_header_name buf ~off name value in
      write_headers_loop buf off rest

type content_length_mode =
  | Known of int              (* Content-Length: N *)
  | Chunked                   (* Transfer-Encoding: chunked *)
  | Omit                      (* No content-length header (valid for HEAD per RFC 7230) *)

let write_response_headers buf ~off ~keep_alive ~content_length_mode version ~status
    ~(headers : Httpz_route.resp_header list) =
  let off = Httpz.Res.write_status_line buf ~off status version in
  (* RFC 7231 s7.1.1.2: an origin server MUST send Date, except that it MAY be
     omitted for 1xx and 5xx, and MUST be omitted with no usable clock. Sending
     it unconditionally satisfies all three. [write_http_date] is a table-driven
     write of 29 bytes and allocates nothing, so the cost here is the
     [gettimeofday] vDSO call rather than the formatting; a one-second cache
     would not avoid that call and so is not worth the shared state. *)
  let off =
    Httpz.Date.write_date_header buf ~off (F64.of_float (Unix.gettimeofday ()))
  in
  let off = write_headers_loop buf off headers in
  let off = match content_length_mode with
    | Known len -> Httpz.Res.write_content_length buf ~off len
    | Chunked -> Httpz.Res.write_header_name buf ~off Httpz.Header_name.Transfer_encoding "chunked"
    | Omit -> off  (* No content-length header for HEAD responses where we don't know the size *)
  in
  let off = Httpz.Res.write_connection buf ~off ~keep_alive in
  let off = Httpz.Res.write_crlf buf ~off in
  to_int off

(** {1 Connection State} *)

(** Response buffer size - 64KB for headers *)
let response_buffer_size = 65536

type 'a conn = {
  flow : 'a Eio.Net.stream_socket;
  read_buf : bytes;     (* For httpz parser *)
  write_buf : bytes;    (* For httpz response writer *)
  read_cs : Cstruct.t;  (* For Eio reading - separate buffer, we blit to read_buf *)
  mutable read_len : int;
  mutable keep_alive : bool;
  (* Logging capture — set by respond wrapper, read after dispatch *)
  mutable logged_status : Httpz.Res.status;
  mutable logged_resp_ct : string or_null;
  mutable logged_body_size : int;
  mutable logged_cache_status : string or_null;
}

let create_conn flow =
  {
    flow;
    read_buf = Bytes.create Httpz.buffer_size;
    write_buf = Bytes.create response_buffer_size;
    read_cs = Cstruct.create Httpz.buffer_size;
    read_len = 0;
    keep_alive = true;
    logged_status = Httpz.Res.Success;
    logged_resp_ct = Null;
    logged_body_size = 0;
    logged_cache_status = Null;
  }

(** {1 Response Writing with Eio} *)

(** Create a respond function that writes directly to the connection.

    For HEAD requests per {{:https://datatracker.ietf.org/doc/html/rfc7230#section-3.3.2}
    RFC 7230 Section 3.3.2}: "A server MAY send a Content-Length header field in a
    response to a HEAD request; a server MUST NOT send Content-Length in such a
    response unless its field-value equals the decimal number of octets that would
    have been sent in the payload body of a response if the same request had used
    the GET method."

    When body is Empty for HEAD (meaning we skipped generation), we omit Content-Length
    since we don't know the actual GET body size. When body is String/Bigstring for HEAD,
    we include Content-Length since we have the actual body. *)
let make_respond conn ~is_head ~keep_alive version ~status ~headers body =
  let buf = conn.write_buf in
  match body with
  | Httpz_route.Empty ->
      (* For HEAD with Empty body, we don't know the real content length, so omit it.
         Per RFC 7230 3.3.2, Content-Length MAY be omitted for HEAD responses. *)
      let content_length_mode = if is_head then Omit else Known 0 in
      let header_len = write_response_headers buf ~off:(i16 0) ~keep_alive
        ~content_length_mode version ~status ~headers in
      Eio.Flow.write conn.flow [Cstruct.of_bytes conn.write_buf ~off:0 ~len:header_len]

  | Httpz_route.String body_str ->
      (* We have the actual body, so Content-Length is accurate even for HEAD *)
      let header_len = write_response_headers buf ~off:(i16 0) ~keep_alive
        ~content_length_mode:(Known (String.length body_str)) version ~status ~headers in
      if is_head then
        (* HEAD: send headers with correct Content-Length but no body *)
        Eio.Flow.write conn.flow [Cstruct.of_bytes conn.write_buf ~off:0 ~len:header_len]
      else
        Eio.Flow.write conn.flow [
          Cstruct.of_bytes conn.write_buf ~off:0 ~len:header_len;
          Cstruct.of_string body_str;
        ]

  | Httpz_route.Bigstring { buf = body_buf; off; len } ->
      (* We have the actual body, so Content-Length is accurate even for HEAD *)
      let header_len = write_response_headers buf ~off:(i16 0) ~keep_alive
        ~content_length_mode:(Known len) version ~status ~headers in
      if is_head then
        Eio.Flow.write conn.flow [Cstruct.of_bytes conn.write_buf ~off:0 ~len:header_len]
      else
        Eio.Flow.write conn.flow [
          Cstruct.of_bytes conn.write_buf ~off:0 ~len:header_len;
          Cstruct.of_bigarray body_buf ~off ~len;
        ]

  | Httpz_route.Stream { length; iter } ->
      (* For streams: use known length if available, omit for HEAD without length, chunked otherwise *)
      let content_length_mode = match length, is_head with
        | Some len, _ -> Known len
        | None, true -> Omit
        | None, false -> Chunked
      in
      let header_len = write_response_headers buf ~off:(i16 0) ~keep_alive
        ~content_length_mode version ~status ~headers in
      Eio.Flow.write conn.flow [Cstruct.of_bytes conn.write_buf ~off:0 ~len:header_len];
      if is_head then
        (* HEAD: headers only, skip streaming body *)
        ()
      else begin
        (* Stream chunks - if no content-length, use chunked encoding *)
        match length with
        | Some _ ->
            (* Known length - just write chunks directly *)
            iter (fun chunk -> Eio.Flow.write conn.flow [Cstruct.of_string chunk])
        | None ->
            (* Chunked encoding *)
            iter (fun chunk ->
              let len = String.length chunk in
              if len > 0 then begin
                let hex = Printf.sprintf "%x\r\n" len in
                Eio.Flow.write conn.flow [
                  Cstruct.of_string hex;
                  Cstruct.of_string chunk;
                  Cstruct.of_string "\r\n";
                ]
              end);
            (* Final chunk *)
            Eio.Flow.write conn.flow [Cstruct.of_string "0\r\n\r\n"]
      end

  | Httpz_route.Stream_bigstring { length; iter } ->
      (* As [Stream], but each chunk is written straight out of the producer's
         bigstring. [Cstruct.of_bigarray] wraps without copying, so nothing
         reaches the OCaml heap. *)
      let content_length_mode = match length, is_head with
        | Some len, _ -> Known len
        | None, true -> Omit
        | None, false -> Chunked
      in
      let header_len = write_response_headers buf ~off:(i16 0) ~keep_alive
        ~content_length_mode version ~status ~headers in
      Eio.Flow.write conn.flow [Cstruct.of_bytes conn.write_buf ~off:0 ~len:header_len];
      if is_head then ()
      else begin
        match length with
        | Some _ ->
            iter (fun bs ~off ~len ->
              Eio.Flow.write conn.flow [Cstruct.of_bigarray bs ~off ~len])
        | None ->
            iter (fun bs ~off ~len ->
              if len > 0 then begin
                let hex = Printf.sprintf "%x\r\n" len in
                Eio.Flow.write conn.flow [
                  Cstruct.of_string hex;
                  Cstruct.of_bigarray bs ~off ~len;
                  Cstruct.of_string "\r\n";
                ]
              end);
            Eio.Flow.write conn.flow [Cstruct.of_string "0\r\n\r\n"]
      end

(** Send error response *)
let send_error conn status message version =
  let buf = conn.write_buf in
  let off = Httpz.Res.write_status_line buf ~off:(i16 0) status version in
  let off = Httpz.Res.write_header_name buf ~off Httpz.Header_name.Content_type "text/plain" in
  let off = Httpz.Res.write_content_length buf ~off (String.length message) in
  let off = Httpz.Res.write_connection buf ~off ~keep_alive:conn.keep_alive in
  let off = Httpz.Res.write_crlf buf ~off in
  let header_len = to_int off in
  Eio.Flow.write conn.flow
    [
      Cstruct.of_bytes conn.write_buf ~off:0 ~len:header_len;
      Cstruct.of_string message;
    ]

(** {1 Buffer Operations} *)

(** Read more data into buffer *)
let read_more conn =
  if conn.read_len >= Httpz.buffer_size then `Buffer_full
  else
    let available = Httpz.buffer_size - conn.read_len in
    let cs = Cstruct.sub conn.read_cs conn.read_len available in
    match Eio.Flow.single_read conn.flow cs with
    | n ->
        (* Blit from cstruct to bytes so httpz parser can access it *)
        Cstruct.blit_to_bytes cs 0 conn.read_buf conn.read_len n;
        conn.read_len <- conn.read_len + n;
        `Ok n
    | exception End_of_file -> `Eof

(** Shift buffer contents to remove processed data *)
let shift_buffer conn consumed =
  if consumed > 0 && consumed < conn.read_len then begin
    Bytes.blit ~src:conn.read_buf ~src_pos:consumed ~dst:conn.read_buf ~dst_pos:0 ~len:(conn.read_len - consumed);
    conn.read_len <- conn.read_len - consumed
  end
  else if consumed >= conn.read_len then conn.read_len <- 0

(** {1 Request Handling} *)

(** Handle one request. Returns `Continue, `Close, or `Need_more *)
let handle_request conn ~addr_str ~routes ~on_request =
  (* Create string view of bytes for parsing *)
  let buf = conn.read_buf in
  let len = conn.read_len in
  let len16 = i16 len in
  let #(status, req, headers) =
    Httpz.parse buf ~len:len16 ~limits:Httpz.default_limits
  in
  let body_off = to_int req.#body_off in
  let version = req.#version in
  match status with
  | Httpz.Buf_read.Complete ->
      let timestamp = F64.of_float (Unix.gettimeofday ()) in
      let meth = req.#meth in
      (* The parser had to split the target to validate it, so the path and
         query come off [req] rather than from a second scan. *)
      let path_span = req.#path in
      let path_str = Httpz.Span.to_string buf path_span in
      let target_str = Httpz.Span.to_string buf req.#target in
      (* Extract request headers for logging *)
      let get_hdr name =
        match Httpz.Header.find headers name with
        | Some hdr -> This (Httpz.Span.to_string buf hdr.Httpz.Header.value)
        | None -> Null
      in
      let host = get_hdr Httpz.Header_name.Host in
      let user_agent = get_hdr Httpz.Header_name.User_agent in
      let referer = get_hdr Httpz.Header_name.Referer in
      let accept = get_hdr Httpz.Header_name.Accept in
      let forwarded_for = get_hdr Httpz.Header_name.X_forwarded_for in
      let forwarded_proto = get_hdr Httpz.Header_name.X_forwarded_proto in
      let request_headers = Httpz.Header.to_string_pairs_local buf headers in
      (* Update keep_alive before dispatch *)
      conn.keep_alive <- req.#keep_alive;
      (* Check if this is a HEAD request for body suppression *)
      let is_head = phys_equal meth Httpz.Method.Head in
      (* Reset per-request logging state *)
      conn.logged_resp_ct <- Null;
      conn.logged_cache_status <- Null;
      conn.logged_body_size <- 0;
      (* Create respond function that captures metadata via conn *)
      let respond ~status ~headers body =
        conn.logged_status <- status;
        (* Scan response headers for Content_type and X_cache *)
        (* Copy a local string to global *)
        let copy_string (local_ s : string) : string =
          let len = String.length s in
          let dst = Bytes.create len in
          for i = 0 to len - 1 do
            Bytes.unsafe_set dst i (String.unsafe_get s i)
          done;
          Bytes.unsafe_to_string ~no_mutation_while_string_reachable:dst
        in
        let rec scan (local_ hs : Httpz_route.resp_header list) =
          match hs with
          | [] -> ()
          | (name, value) :: rest ->
            if phys_equal name Httpz.Header_name.Content_type then
              conn.logged_resp_ct <- This (copy_string value)
            else if phys_equal name Httpz.Header_name.X_cache then
              conn.logged_cache_status <- This (copy_string value);
            scan rest
        in
        scan headers;
        conn.logged_body_size <- (match body with
          | Httpz_route.String s -> String.length s
          | Bigstring { len; _ } -> len
          | Stream { length; _ } -> Option.value length ~default:0
          | Stream_bigstring { length; _ } -> Option.value length ~default:0
          | Empty -> 0);
        make_respond conn ~is_head ~keep_alive:conn.keep_alive version ~status ~headers body
      in
      (* Send 100 Continue if client expects it (RFC 7231 Section 5.1.1) *)
      if req.#expect_continue then begin
        let cont = "HTTP/1.1 100 Continue\r\n\r\n" in
        Eio.Flow.write conn.flow [Cstruct.of_string cont]
      end;
      (* Read complete body before dispatch.
         For Content-Length bodies: ensure all bytes are in buffer.
         For chunked bodies: dechunk into the buffer. *)
      let body_off_final = ref (to_int req.#body_off) in
      let body_len_final = ref 0 in
      if req.#is_chunked then begin
        (* Dechunk into an accumulator buffer *)
        let body_acc = Buffer.create 4096 in
        let chunk_off = ref (to_int req.#body_off) in
        let finished = ref false in
        while not !finished do
          let #(cstatus, chunk) =
            Httpz.Chunk.parse buf ~off:(i16 !chunk_off) ~len:(i16 conn.read_len)
          in
          match cstatus with
          | Httpz.Chunk.Complete ->
            let doff = to_int chunk.#data_off in
            let dlen = to_int chunk.#data_len in
            Buffer.add_subbytes body_acc buf ~pos:doff ~len:dlen;
            chunk_off := to_int chunk.#next_off
          | Httpz.Chunk.Done ->
            finished := true
          | Httpz.Chunk.Partial ->
            (* Need more data from the socket *)
            begin match read_more conn with
            | `Ok _ -> ()
            | `Eof -> finished := true
            | `Buffer_full ->
              (* Shift consumed chunk data out to make room *)
              let consumed = !chunk_off in
              if consumed > 0 then begin
                shift_buffer conn consumed;
                chunk_off := 0
              end;
              begin match read_more conn with
              | `Ok _ -> ()
              | `Eof | `Buffer_full -> finished := true
              end
            end
          | Httpz.Chunk.Malformed | Httpz.Chunk.Chunk_too_large ->
            finished := true
        done;
        let body_str = Buffer.contents body_acc in
        let blen = String.length body_str in
        (* Write dechunked body into read_buf at body_off for zero-copy dispatch *)
        let boff = to_int req.#body_off in
        if boff + blen <= Bytes.length buf then begin
          Stdlib.Bytes.blit_string body_str 0 buf boff blen;
          conn.read_len <- boff + blen
        end;
        body_off_final := boff;
        body_len_final := blen
      end else begin
        (* Content-Length body: ensure all bytes are read into buffer *)
        let cl = Stdlib_upstream_compatible.Int64_u.to_int req.#content_length in
        if cl > 0 then begin
          let body_end = to_int req.#body_off + cl in
          while conn.read_len < body_end do
            match read_more conn with
            | `Ok _ -> ()
            | `Eof | `Buffer_full ->
              conn.read_len <- body_end (* force exit *)
          done;
          body_off_final := to_int req.#body_off;
          body_len_final := cl
        end
      end;
      let body_span = Httpz.Span.make ~off:(i16 !body_off_final) ~len:(i16 !body_len_final) in
      let body_content_length = Stdlib_upstream_compatible.Int64_u.of_int !body_len_final in
      (* Dispatch - respond is called directly by handler *)
      let matched =
        Httpz_route.dispatch buf ~meth ~path:path_span ~query:req.#query
          ~body:body_span ~content_length:body_content_length ~headers routes
          ~respond
      in
      if not matched then begin
        conn.logged_status <- Httpz.Res.Not_found;
        Httpz_route.not_found respond
      end;
      (* Compute duration and build request_info *)
      let t1 = F64.of_float (Unix.gettimeofday ()) in
      let duration_us = Float.to_int (F64.to_float (F64.mul (F64.sub t1 timestamp) (F64.of_float 1_000_000.0))) in
      on_request { remote_addr = addr_str; meth; target = target_str;
        path = path_str; host; user_agent; referer; accept;
        forwarded_for; forwarded_proto; request_headers;
        status = conn.logged_status;
        response_content_type = conn.logged_resp_ct;
        cache_status = conn.logged_cache_status;
        timestamp;
        response_body_size = conn.logged_body_size;
        duration_us };
      (* Calculate consumed bytes — body_span already accounts for
         dechunked/fully-read body placement *)
      let body_span_len = Httpz.Span.len body_span in
      let body_span_off = Httpz.Span.off body_span in
      let consumed =
        if body_span_len > 0 then body_span_off + body_span_len else body_off
      in
      shift_buffer conn consumed;
      if conn.keep_alive then `Continue else `Close
  | Httpz.Buf_read.Partial -> `Need_more
  | Httpz.Buf_read.Headers_too_large | Httpz.Buf_read.Content_length_overflow ->
      conn.keep_alive <- false;
      send_error conn Httpz.Res.Payload_too_large "Payload Too Large"
        Httpz.Version.Http_1_1;
      `Close
  | Httpz.Buf_read.Uri_too_long ->
      conn.keep_alive <- false;
      send_error conn Httpz.Res.Uri_too_long "URI Too Long" Httpz.Version.Http_1_1;
      `Close
  | Httpz.Buf_read.Bare_cr_detected | Httpz.Buf_read.Ambiguous_framing ->
      conn.keep_alive <- false;
      send_error conn Httpz.Res.Bad_request "Bad Request" Httpz.Version.Http_1_1;
      `Close
  | Httpz.Buf_read.Missing_host_header ->
      conn.keep_alive <- false;
      send_error conn Httpz.Res.Bad_request "Missing Host Header"
        Httpz.Version.Http_1_1;
      `Close
  | _ ->
      conn.keep_alive <- false;
      send_error conn Httpz.Res.Bad_request "Bad Request" Httpz.Version.Http_1_1;
      `Close

(** Send payload too large error and close connection *)
let send_payload_too_large conn =
  conn.keep_alive <- false;
  send_error conn Httpz.Res.Payload_too_large "Payload Too Large"
    Httpz.Version.Http_1_1

(** Handle connection loop *)
let handle_connection conn ~addr_str ~routes ~on_request =
  let handle_read_result ~continue = function
    | `Eof -> ()
    | `Buffer_full -> send_payload_too_large conn
    | `Ok _ -> continue ()
  in
  let rec loop () =
    if conn.read_len = 0 then
      handle_read_result ~continue:loop (read_more conn)
    else
      match handle_request conn ~addr_str ~routes ~on_request with
      | `Continue -> loop ()
      | `Close -> ()
      | `Need_more -> handle_read_result ~continue:loop (read_more conn)
  in
  loop ()

(** Handle a single client connection *)
(* "a.b.c.d:port" without going through [Format], which allocates ~3KB per
   call — negligible under keep-alive, but paid per request by
   [Connection: close] clients. IPv6 falls back to the pretty-printer. *)
let format_tcp_addr (ip : Eio.Net.Ipaddr.v4v6) port =
  let raw = (ip :> string) in
  if String.length raw <> 4
  then Stdlib.Format.asprintf "%a:%d" Eio.Net.Ipaddr.pp ip port
  else (
    let b = Buffer.create 21 in
    for i = 0 to 3 do
      if i > 0 then Buffer.add_char b '.';
      Buffer.add_string b (Int.to_string (Char.to_int (String.get raw i)))
    done;
    Buffer.add_char b ':';
    Buffer.add_string b (Int.to_string port);
    Buffer.contents b)
;;

(** Handle a single client connection *)
let handle_client ~routes ~on_request ~on_error flow addr =
  let addr_str = match addr with
    | `Tcp (ip, port) -> format_tcp_addr ip port
    | `Unix path -> path
    | _ -> "unknown"
  in
  let conn = create_conn flow in
  try handle_connection conn ~addr_str ~routes ~on_request
  with exn -> on_error exn

(** {1 Static File Serving} *)

module Static = struct
  module I64 = Stdlib_upstream_compatible.Int64_u

  type t = {
    root : Eio.Fs.dir_ty Eio.Path.t;
    index : string list;    (* directory index candidates *)
    max_inline : int;       (* bodies <= this are read into one bigstring *)
    chunk_size : int;       (* streaming chunk size for larger bodies *)
  }

  let default_index = [ "index.html" ]
  let default_max_inline = 1 lsl 20   (* 1 MiB *)
  let default_chunk_size = 1 lsl 16   (* 64 KiB *)

  (* Confine here rather than trust the caller: a confined and an unconfined
     capability have the same type, so [Eio.Stdenv.fs] would otherwise disable
     containment silently. Confining an already-confined path costs one fd. *)
  let create ~sw ?(index = default_index) ?(max_inline = default_max_inline)
      ?(chunk_size = default_chunk_size) root =
    let root = (Eio.Path.open_subtree ~sw root :> Eio.Fs.dir_ty Eio.Path.t) in
    { root; index; max_inline; chunk_size }

  (** {2 MIME types} *)

  (* Lowercased extension of a file name, or "" for none. A leading dot
     (".bashrc") is treated as no extension. *)
  let extension name =
    match String.rindex name '.' with
    | None -> ""
    | Some i ->
        if i = 0 || i = String.length name - 1 then ""
        else String.lowercase (String.sub name ~pos:(i + 1) ~len:(String.length name - i - 1))

  let mime_type name =
    match extension name with
    | "html" | "htm" -> "text/html"
    | "css" -> "text/css"
    | "js" | "mjs" -> "application/javascript"
    | "json" -> "application/json"
    | "txt" -> "text/plain"
    | "md" -> "text/markdown"
    | "xml" -> "application/xml"
    | "png" -> "image/png"
    | "jpg" | "jpeg" -> "image/jpeg"
    | "gif" -> "image/gif"
    | "svg" -> "image/svg+xml"
    | "ico" -> "image/x-icon"
    | "webp" -> "image/webp"
    | "pdf" -> "application/pdf"
    | "woff" -> "font/woff"
    | "woff2" -> "font/woff2"
    | "ttf" -> "font/ttf"
    | "ml" | "mli" -> "text/x-ocaml"
    | "c" | "h" -> "text/x-c"
    | "py" -> "text/x-python"
    | "sh" -> "text/x-shellscript"
    | "yaml" | "yml" -> "text/yaml"
    | "toml" -> "text/toml"
    | _ -> "application/octet-stream"

  (** {2 ETags} *)

  (* Opaque tag content (no quotes, no [W/] prefix): "<mtime-ms hex>-<size hex>" *)
  let etag_opaque ~(mtime : float) ~(size : int) =
    Printf.sprintf "%x-%x" (Float.to_int (mtime *. 1000.0)) size

  let etag_header opaque = Printf.sprintf "W/\"%s\"" opaque

  (* Weak comparison of our opaque tag against an If-None-Match field value.
     Uses [Httpz.Etag] so that "*", quoting, [W/] prefixes and comma-separated
     lists are handled per RFC 7232. *)
  let if_none_match_matches ~opaque value =
    (* [unsafe_of_string] is sound here because [parse_match_header] and the
       comparison below only read. *)
    let buf = Stdlib.Bytes.unsafe_of_string value in
    let span = Httpz.Span.make ~off:(i16 0) ~len:(i16 (String.length value)) in
    (* Scratch for this call's tags, in this frame. A module-level array would
       be shared by every connection, and two fibres parsing If-None-Match at
       once would overwrite each other's tags — enough to answer 304 from
       another request's validator. [Array.create] allocates on the heap
       whatever mode it is bound at, so the slots are written out;
       [parse_match_header] keeps at most as many tags as it is given. *)
    let e = Httpz.Etag.empty in
    let local_ tags = [| e; e; e; e; e; e; e; e; e; e; e; e; e; e; e; e |] in
    let #(cond, count) = Httpz.Etag.parse_match_header buf span tags in
    match cond with
    | Httpz.Etag.Any -> true
    | Httpz.Etag.Empty -> false
    | Httpz.Etag.Tags ->
      (* Compare against the buffer in place rather than materialising a
         string per tag. This is the weak comparison RFC 7232 s2.3.2 requires
         for If-None-Match. *)
      let n = to_int count in
      let mutable i = 0 in
      let mutable found = false in
      while (not found) && i < n do
        let tag = Array.get tags i in
        let sp = Httpz.Span.make ~off:tag.#off ~len:tag.#len in
        if Httpz.Span.equal buf sp opaque then found <- true else i <- i + 1
      done;
      found

  (** {2 Path handling} *)

  (* Percent-decode every path segment, or [Null] if one holds a malformed
     ['%']. A ['+'] is a query convention and stays a literal ['+'] here.
     ["%2f"] decodes to a ['/'] inside its segment and introduces no new
     separator, because the router splits the path before this runs;
     {!safe_segment} rejects the decoded segment that contains one.

     {!Httpz.parse} already refuses a target whose ['%'] is not a complete
     triplet, so [Null] is unreachable through the server's own parser. It is
     still an error rather than a literal ['%'], since a file server that
     guesses at a malformed path can open a file the client did not name. *)
  let rec decode_segments acc segments =
    match segments with
    | [] -> This (List.rev acc)
    | s :: rest ->
      (match Uriz.pct_decode s with
       | Null -> Null
       | This d -> decode_segments (d :: acc) rest)

  (* Resolve "." and ".." within the segment list. Popping past the root is
     clamped, so the result can never escape [root]. *)
  let normalize segments =
    let rec go acc = function
      | [] -> List.rev acc
      | "." :: rest -> go acc rest
      | ".." :: rest -> (match acc with [] -> go [] rest | _ :: tl -> go tl rest)
      | seg :: rest -> go (seg :: acc) rest
    in
    go [] segments

  (* Reject segments that could confuse the OS layer. *)
  let safe_segment s =
    (not (String.is_empty s)) && (not (String.mem s '\000')) && (not (String.mem s '/'))

  (* No path check is needed here: [t.root] is confined (see [create]), so a
     path escaping it fails at [open] or [stat] with [Permission_denied], which
     [stat_opt] reports as 404. *)

  (** {2 Response construction} *)

  let text_error (local_ respond) status message =
    respond ~status
      ~headers:[ (Httpz.Header_name.Content_type, "text/plain; charset=utf-8") ]
      (Httpz_route.String message)

  (* Send [len] bytes of [path] starting at [off].

     Small bodies are read into a single bigstring and handed to the response
     writer as [Httpz_route.Bigstring] (written straight from the bigarray, no
     intermediate string). Larger bodies are streamed in [chunk_size] pieces
     with a known Content-Length so that keep-alive framing is preserved.

     For HEAD we emit a zero-iteration stream with the correct length: the
     response writer then sends headers with an accurate Content-Length and no
     body, and the file is never read. *)
  (* Read [len] bytes at [off] into a fresh bigstring. Captures nothing local. *)
  let read_region path ~off ~len =
    Eio.Path.with_open_in path (fun f ->
      (* [pread_exact] fills all [len] bytes or raises, so the buffer is fully
         written before it can be observed and need not be zeroed. Do not
         relax it to a partial read without restoring the zero-fill: that
         would disclose uninitialised heap. *)
      let cs = Cstruct.create_unsafe len in
      Eio.File.pread_exact f ~file_offset:(Optint.Int63.of_int off) [ cs ];
      cs)

  (* Streaming body iterator. The file is opened inside [iter] so that this
     closure captures only global values (the response body escapes the
     handler's local region). *)
  (* Emits each chunk straight out of the read buffer, which is reused across
     chunks — hence [Stream_bigstring]'s requirement that the callback not
     retain it. Copying each chunk into a string would instead put a
     [chunk_size] block on the major heap per chunk. *)
  let stream_region t path ~off ~len emit =
    Eio.Path.with_open_in path (fun f ->
      let chunk = Cstruct.create_unsafe t.chunk_size in
      let pos = ref off in
      let remaining = ref len in
      while !remaining > 0 do
        let n = Int.min !remaining t.chunk_size in
        let sub = Cstruct.sub chunk 0 n in
        Eio.File.pread_exact f ~file_offset:(Optint.Int63.of_int !pos) [ sub ];
        emit chunk.Cstruct.buffer ~off:sub.Cstruct.off ~len:n;
        pos := !pos + n;
        remaining := !remaining - n
      done)

  let send_body t ~path ~off ~len ~is_head ~status ~(local_ headers) (local_ respond) =
    if is_head then
      respond ~status ~headers (Httpz_route.Stream { length = Some len; iter = (fun _ -> ()) })
    else if len = 0 then respond ~status ~headers (Httpz_route.String "")
    else if len <= t.max_inline then begin
      let cs = read_region path ~off ~len in
      respond ~status ~headers
        (Httpz_route.Bigstring { buf = cs.Cstruct.buffer; off = cs.Cstruct.off; len })
    end
    else
      respond ~status ~headers
        (Httpz_route.Stream_bigstring
           { length = Some len; iter = stream_region t path ~off ~len })

  (* Serve a regular file, honouring If-None-Match and Range. *)
  let serve_file t ~path ~name ~(st : Eio.File.Stat.t) ~is_head ~range ~if_none_match
      (local_ respond) =
    let size = Optint.Int63.to_int st.size in
    let mtime = st.mtime in
    let opaque = etag_opaque ~mtime ~size in
    let etag = etag_header opaque in
    let last_modified = Httpz.Date.format (F64.of_float mtime) in
    let content_type = mime_type name in
    let full () =
      send_body t ~path ~off:0 ~len:size ~is_head ~status:Httpz.Res.Success
        ~headers:
          [ (Httpz.Header_name.Content_type, content_type)
          ; (Httpz.Header_name.Etag, etag)
          ; (Httpz.Header_name.Last_modified, last_modified)
          ; (Httpz.Header_name.Accept_ranges, "bytes")
          ]
        respond
    in
    match if_none_match with
    | Some v when if_none_match_matches ~opaque v ->
        respond ~status:Httpz.Res.Not_modified
          ~headers:
            [ (Httpz.Header_name.Etag, etag)
            ; (Httpz.Header_name.Last_modified, last_modified)
            ; (Httpz.Header_name.Accept_ranges, "bytes")
            ]
          Httpz_route.Empty
    | _ ->
      (match range with
       | None -> (full () [@nontail])
       | Some spec ->
         let ranges = Array.create ~len:(to_int Httpz.Range.max_ranges) Httpz.Range.empty in
         let #(pstatus, count) = Httpz.Range.parse_string spec ranges in
         (match pstatus with
          | Httpz.Range.Invalid -> (full () [@nontail])   (* RFC 7233: ignore an unparsable Range *)
          | Httpz.Range.Valid ->
            let resolved =
              Array.create ~len:(to_int Httpz.Range.max_ranges) Httpz.Range.empty_resolved
            in
            let #(result, _n) =
              Httpz.Range.evaluate ranges ~count ~resource_length:(I64.of_int size) resolved
            in
            (match result with
             | Httpz.Range.Full_content -> (full () [@nontail])
             | Httpz.Range.Not_satisfiable ->
                 respond ~status:Httpz.Res.Range_not_satisfiable
                   ~headers:
                     [ (Httpz.Header_name.Content_range, Printf.sprintf "bytes */%d" size)
                     ; (Httpz.Header_name.Accept_ranges, "bytes")
                     ]
                   Httpz_route.Empty
             | Httpz.Range.Single_range | Httpz.Range.Multiple_ranges ->
               (* Only the first range is served; multipart/byteranges is not
                  implemented, which RFC 7233 permits. *)
               let r = Array.get resolved 0 in
               let start = I64.to_int r.#start in
               let end_ = I64.to_int r.#end_ in
               let len = end_ - start + 1 in
               send_body t ~path ~off:start ~len ~is_head ~status:Httpz.Res.Partial_content
                 ~headers:
                   [ (Httpz.Header_name.Content_type, content_type)
                   ; ( Httpz.Header_name.Content_range
                     , Printf.sprintf "bytes %d-%d/%d" start end_ size )
                   ; (Httpz.Header_name.Etag, etag)
                   ; (Httpz.Header_name.Last_modified, last_modified)
                   ; (Httpz.Header_name.Accept_ranges, "bytes")
                   ]
                 respond)))

  let stat_opt path =
    match Eio.Path.stat ~follow:true path with
    | st -> Some st
    | exception Eio.Io _ -> None
    | exception Unix.Unix_error _ -> None

  (* First directory index candidate that exists as a regular file. *)
  let rec first_index dir = function
    | [] -> None
    | name :: rest ->
        let p = Eio.Path.(dir / name) in
        (match stat_opt p with
         | Some ({ kind = `Regular_file; _ } as st : Eio.File.Stat.t) -> Some (p, name, st)
         | _ -> first_index dir rest)

  let handle t segments ~range ~if_none_match ctx (local_ respond) =
    let is_head = Httpz_route.is_head ctx in
    let decoded =
      match decode_segments [] segments with
      | Null -> Null
      | This ss -> if List.for_all ss ~f:safe_segment then This ss else Null
    in
    match decoded with
    | Null -> text_error respond Httpz.Res.Bad_request "Bad Request"
    | This ss ->
      let parts = normalize ss in
      let path = List.fold parts ~init:t.root ~f:(fun p s -> Eio.Path.(p / s)) in
      (match stat_opt path with
       | None -> text_error respond Httpz.Res.Not_found "Not Found"
       | Some st ->
         (match st.Eio.File.Stat.kind with
          | `Directory ->
            (match first_index path t.index with
             | None -> text_error respond Httpz.Res.Not_found "Not Found"
             | Some (ipath, iname, ist) ->
               serve_file t ~path:ipath ~name:iname ~st:ist ~is_head ~range
                 ~if_none_match respond)
          | `Regular_file ->
            let name = match List.last parts with None -> "" | Some n -> n in
            serve_file t ~path ~name ~st ~is_head ~range ~if_none_match respond
          | _ -> text_error respond Httpz.Res.Not_found "Not Found"))

  let route t =
    Httpz_route.get_h Httpz_route.tail
      Httpz_route.(Httpz.Header_name.Range +> (Httpz.Header_name.If_none_match +> h0))
      (fun segments (range, (if_none_match, ())) ctx (local_ respond) ->
        handle t segments ~range ~if_none_match ctx respond)

  let routes t = Httpz_route.of_list [ route t ]
end
