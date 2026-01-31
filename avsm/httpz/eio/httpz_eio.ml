(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Eio integration for httpz - response writing and connection handling *)

open Base

module I16 = Stdlib_stable.Int16_u

let[@inline] i16 x = I16.of_int x
let[@inline] to_int x = I16.to_int x

(** {1 Response Writing} *)

(** Write response headers to buffer using typed Header_name.t.
    Returns header length. *)
let rec write_headers_loop buf off (local_ headers : Httpz.Route.resp_header list) =
  match headers with
  | [] -> off
  | (name, value) :: rest ->
      let off = Httpz.Res.write_header_name buf ~off name value in
      write_headers_loop buf off rest

let write_response_headers buf ~off ~keep_alive ~content_length version ~status
    ~(local_ headers : Httpz.Route.resp_header list) =
  let off = Httpz.Res.write_status_line buf ~off status version in
  let off = write_headers_loop buf off headers in
  let off = match content_length with
    | Some len -> Httpz.Res.write_content_length buf ~off len
    | None -> Httpz.Res.write_header_name buf ~off Httpz.Header_name.Transfer_encoding "chunked"
  in
  let off = Httpz.Res.write_connection buf ~off ~keep_alive in
  let off = Httpz.Res.write_crlf buf ~off in
  to_int off

(** {1 Connection State} *)

(** Response buffer size - 64KB for headers *)
let response_buffer_size = 65536

type 'a conn = {
  flow : 'a Eio.Net.stream_socket;
  read_buf : bytes;
  write_buf : bytes;
  read_cs : Cstruct.t;   (* Cstruct view of read_buf for Eio *)
  write_cs : Cstruct.t;  (* Cstruct view of write_buf for Eio *)
  mutable read_len : int;
  mutable keep_alive : bool;
}

let create_conn flow =
  let read_buf = Bytes.create Httpz.buffer_size in
  let write_buf = Bytes.create response_buffer_size in
  {
    flow;
    read_buf;
    write_buf;
    read_cs = Cstruct.of_bytes read_buf;
    write_cs = Cstruct.of_bytes write_buf;
    read_len = 0;
    keep_alive = true;
  }

(** {1 Response Writing with Eio} *)

(** Create a respond function that writes directly to the connection.
    This is the CPS callback passed to dispatch - no intermediate resp record. *)
let make_respond conn ~keep_alive version ~status ~(local_ headers) body =
  let buf = conn.write_buf in
  match body with
  | Httpz.Route.Empty ->
      let header_len = write_response_headers buf ~off:(i16 0) ~keep_alive
        ~content_length:(Some 0) version ~status ~headers in
      Eio.Flow.write conn.flow [Cstruct.sub conn.write_cs 0 header_len]

  | Httpz.Route.String body_str ->
      let header_len = write_response_headers buf ~off:(i16 0) ~keep_alive
        ~content_length:(Some (String.length body_str)) version ~status ~headers in
      Eio.Flow.write conn.flow [
        Cstruct.sub conn.write_cs 0 header_len;
        Cstruct.of_string body_str;
      ]

  | Httpz.Route.Bigstring { buf = body_buf; off; len } ->
      let header_len = write_response_headers buf ~off:(i16 0) ~keep_alive
        ~content_length:(Some len) version ~status ~headers in
      Eio.Flow.write conn.flow [
        Cstruct.sub conn.write_cs 0 header_len;
        Cstruct.of_bigarray body_buf ~off ~len;
      ]

  | Httpz.Route.Stream { length; iter } ->
      let header_len = write_response_headers buf ~off:(i16 0) ~keep_alive
        ~content_length:length version ~status ~headers in
      Eio.Flow.write conn.flow [Cstruct.sub conn.write_cs 0 header_len];
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
      Cstruct.sub conn.write_cs 0 header_len;
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
let handle_request conn ~routes ~on_request =
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
      let target = req.#target in
      let meth = req.#meth in
      (* Get path for logging *)
      let parsed = Httpz.Target.parse buf target in
      let path_span = Httpz.Target.path parsed in
      let path_str = Httpz.Span.to_string buf path_span in
      (* Update keep_alive before dispatch *)
      conn.keep_alive <- req.#keep_alive;
      (* Create respond function and track status for logging *)
      let logged_status = ref Httpz.Res.Success in
      let respond ~status ~headers body =
        logged_status := status;
        make_respond conn ~keep_alive:conn.keep_alive version ~status ~headers body
      in
      (* Dispatch - respond is called directly by handler *)
      let matched = Httpz.Route.dispatch buf ~meth ~target ~headers routes ~respond in
      if not matched then begin
        logged_status := Httpz.Res.Not_found;
        Httpz.Route.not_found respond
      end;
      (* Call request callback for logging etc *)
      on_request ~meth ~path:path_str ~status:!logged_status;
      (* Calculate consumed bytes *)
      let body_span = Httpz.Req.body_span ~len:len16 req in
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
let handle_connection conn ~routes ~on_request =
  let handle_read_result ~continue = function
    | `Eof -> ()
    | `Buffer_full -> send_payload_too_large conn
    | `Ok _ -> continue ()
  in
  let rec loop () =
    if conn.read_len = 0 then
      handle_read_result ~continue:loop (read_more conn)
    else
      match handle_request conn ~routes ~on_request with
      | `Continue -> loop ()
      | `Close -> ()
      | `Need_more -> handle_read_result ~continue:loop (read_more conn)
  in
  loop ()

(** Handle a single client connection *)
let handle_client ~routes ~on_request ~on_error flow _addr =
  let conn = create_conn flow in
  try handle_connection conn ~routes ~on_request
  with exn -> on_error exn
