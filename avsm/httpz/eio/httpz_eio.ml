(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Eio integration for httpz - response writing and connection handling *)

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

(** Write response headers to buffer using typed Header_name.t.
    Returns header length. *)
let rec write_headers_loop buf off (headers : Httpz_server.Route.resp_header list) =
  match headers with
  | [] -> off
  | (name, value) :: rest ->
      let off = Httpz.Res.write_header_name buf ~off name value in
      write_headers_loop buf off rest

type content_length_mode =
  | Known of int              (* Content-Length: N *)
  | Chunked                   (* Transfer-Encoding: chunked *)
  | Omit                      (* No content-length header (valid for HEAD per RFC 7230) *)

let write_response_headers buf ~off ~keep_alive ~content_length_mode version ~status
    ~(headers : Httpz_server.Route.resp_header list) =
  let off = Httpz.Res.write_status_line buf ~off status version in
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
    This is the CPS callback passed to dispatch - no intermediate resp record.

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
  | Httpz_server.Route.Empty ->
      (* For HEAD with Empty body, we don't know the real content length, so omit it.
         Per RFC 7230 3.3.2, Content-Length MAY be omitted for HEAD responses. *)
      let content_length_mode = if is_head then Omit else Known 0 in
      let header_len = write_response_headers buf ~off:(i16 0) ~keep_alive
        ~content_length_mode version ~status ~headers in
      Eio.Flow.write conn.flow [Cstruct.of_bytes conn.write_buf ~off:0 ~len:header_len]

  | Httpz_server.Route.String body_str ->
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

  | Httpz_server.Route.Bigstring { buf = body_buf; off; len } ->
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

  | Httpz_server.Route.Stream { length; iter } ->
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
      (* Parse target once - used for both logging and dispatch *)
      let target = Httpz.Target.parse buf req.#target in
      let path_span = Httpz.Target.path target in
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
        let rec scan (local_ hs : Httpz_server.Route.resp_header list) =
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
          | Httpz_server.Route.String s -> String.length s
          | Bigstring { len; _ } -> len
          | Stream { length; _ } -> Option.value length ~default:0
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
      let matched = Httpz_server.Route.dispatch buf ~meth ~target ~body:body_span ~content_length:body_content_length ~headers routes ~respond in
      if not matched then begin
        conn.logged_status <- Httpz.Res.Not_found;
        Httpz_server.Route.not_found respond
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
let handle_client ~routes ~on_request ~on_error flow addr =
  let addr_str = match addr with
    | `Tcp (ip, port) ->
      Stdlib.Format.asprintf "%a:%d" Eio.Net.Ipaddr.pp ip port
    | `Unix path -> path
    | _ -> "unknown"
  in
  let conn = create_conn flow in
  try handle_connection conn ~addr_str ~routes ~on_request
  with exn -> on_error exn
