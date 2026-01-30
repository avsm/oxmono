(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** httpz + Eio server adapter for arod routes *)

open Base

let src = Logs.Src.create "arod.server" ~doc:"Arod server adapter"

module Log = (val Logs.src_log src : Logs.LOG)

(** {1 Buffer Helpers} *)

let i16 = Httpz.Buf_write.i16
let to_int = Httpz.Buf_write.to_int

(** Response buffer size - 64KB for headers *)
let response_buffer_size = 65536

(** {1 Request Conversion} *)

(** Parse query parameters from a query string *)
let parse_query_string query_str =
  if String.length query_str = 0 then []
  else
    let pairs = String.split query_str ~on:'&' in
    List.filter_map pairs ~f:(fun pair ->
        match String.lsplit2 pair ~on:'=' with
        | None -> Some (pair, "")
        | Some (key, value) -> Some (key, value))

(** Parse target into path and query parameters *)
let parse_target target =
  match String.lsplit2 target ~on:'?' with
  | None -> (target, [])
  | Some (path, query_str) -> (path, parse_query_string query_str)

(** Convert httpz headers to string pairs (works with local header list) *)
let convert_headers buf headers =
  let rec loop acc = function
    | [] -> acc
    | (h : Httpz.Header.t) :: rest ->
        let name =
          match h.name with
          | Httpz.Header_name.Other -> Httpz.Span.to_string buf h.name_span
          | other -> Httpz.Header_name.canonical other
        in
        let value = Httpz.Span.to_string buf h.value in
        loop ((name, value) :: acc) rest
  in
  loop [] headers

(** Convert httpz request to arod request (headers must already be converted) *)
let request_of_httpz buf (req : Httpz.Req.t) string_headers =
  let meth = req.#meth in
  let target = Httpz.Span.to_string buf req.#target in
  let path, query = parse_target target in
  Arod.Route.Request.create ~meth ~path ~query ~headers:string_headers

(** {1 Response Writing} *)

(** Write response headers to buffer, returns header length and body *)
let write_response buf ~off ~keep_alive version resp =
  let status = Arod.Route.Response.status resp in
  let off = Httpz.Res.write_status_line buf ~off status version in
  (* Write headers using a recursive loop to handle int16# *)
  let rec write_headers off = function
    | [] -> off
    | (name, value) :: rest ->
        let off = Httpz.Res.write_header buf ~off name value in
        write_headers off rest
  in
  let off = write_headers off (Arod.Route.Response.headers resp) in
  let body = Arod.Route.Response.body resp in
  let off = Httpz.Res.write_content_length buf ~off (String.length body) in
  let off = Httpz.Res.write_connection buf ~off ~keep_alive in
  let off = Httpz.Res.write_crlf buf ~off in
  (to_int off, body)

(** {1 Connection State} *)

type 'a conn_state = {
  flow : 'a Eio.Net.stream_socket;
  read_buf : Httpz.buffer;
  write_buf : Httpz.buffer;
  mutable read_len : int;
  mutable keep_alive : bool;
}

let create_conn flow =
  {
    flow;
    read_buf = Httpz.create_buffer ();
    write_buf =
      Bigarray.Array1.create Bigarray.char Bigarray.c_layout response_buffer_size;
    read_len = 0;
    keep_alive = true;
  }

(** {1 Memoization} *)

let default_memoized_paths =
  [ "/feeds/"; "/sitemap"; "/perma."; "/bushel/graph.json" ]

let should_memoize path memoized_paths =
  List.exists memoized_paths ~f:(fun prefix ->
      String.length path >= String.length prefix
      && String.equal (String.sub path ~pos:0 ~len:(String.length prefix)) prefix)

(** {1 Server Implementation} *)

(** Send error response *)
let send_error conn status message version =
  let buf = conn.write_buf in
  let off = Httpz.Res.write_status_line buf ~off:(i16 0) status version in
  let off =
    Httpz.Res.write_header_name buf ~off Httpz.Header_name.Content_type
      "text/plain"
  in
  let off = Httpz.Res.write_content_length buf ~off (String.length message) in
  let off = Httpz.Res.write_connection buf ~off ~keep_alive:conn.keep_alive in
  let off = Httpz.Res.write_crlf buf ~off in
  let header_len = to_int off in
  (* Write headers then body *)
  Eio.Flow.write conn.flow
    [
      Cstruct.of_bigarray conn.write_buf ~off:0 ~len:header_len;
      Cstruct.of_string message;
    ]

(** Read more data into buffer *)
let read_more conn =
  if conn.read_len >= Httpz.buffer_size then `Buffer_full
  else
    let available = Httpz.buffer_size - conn.read_len in
    let cs = Cstruct.of_bigarray conn.read_buf ~off:conn.read_len ~len:available in
    match Eio.Flow.single_read conn.flow cs with
    | n ->
        conn.read_len <- conn.read_len + n;
        `Ok n
    | exception End_of_file -> `Eof

(** Shift buffer contents to remove processed data *)
let shift_buffer conn consumed =
  if consumed > 0 && consumed < conn.read_len then begin
    for i = 0 to conn.read_len - consumed - 1 do
      Bigarray.Array1.set conn.read_buf i
        (Bigarray.Array1.get conn.read_buf (consumed + i))
    done;
    conn.read_len <- conn.read_len - consumed
  end
  else if consumed >= conn.read_len then conn.read_len <- 0

(** Handle one request *)
let handle_request conn ~routes ~memo_cache ~memoized_paths =
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
      (* Convert headers while in local scope, then build arod request *)
      let string_headers = convert_headers buf headers in
      let arod_req = request_of_httpz buf req string_headers in
      let path = Arod.Route.Request.path arod_req in
      (* Dispatch with optional memoization *)
      let dispatch_fn =
        match memo_cache with
        | Some cache when should_memoize path memoized_paths ->
            fun req ->
              Arod.Memo.memoize cache
                (fun r ->
                  match Arod.Route.Routes.dispatch routes r with
                  | Some resp -> resp
                  | None -> Arod.Route.Response.not_found)
                req
        | _ -> (
            fun req ->
              match Arod.Route.Routes.dispatch routes req with
              | Some resp -> resp
              | None -> Arod.Route.Response.not_found)
      in
      let resp = dispatch_fn arod_req in
      (* Log request *)
      Log.info (fun m ->
          m "%s %s - %s"
            (Httpz.Method.to_string (Arod.Route.Request.meth arod_req))
            path (Httpz.Res.status_to_string (Arod.Route.Response.status resp)));
      (* Write response *)
      conn.keep_alive <- req.#keep_alive;
      let header_len, body =
        write_response conn.write_buf ~off:(i16 0) ~keep_alive:conn.keep_alive
          version resp
      in
      Eio.Flow.write conn.flow
        [
          Cstruct.of_bigarray conn.write_buf ~off:0 ~len:header_len;
          Cstruct.of_string body;
        ];
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
let handle_connection conn ~routes ~memo_cache ~memoized_paths =
  let handle_read_result ~continue = function
    | `Eof -> ()
    | `Buffer_full -> send_payload_too_large conn
    | `Ok _ -> continue ()
  in
  let rec loop () =
    if conn.read_len = 0 then
      handle_read_result ~continue:loop (read_more conn)
    else
      match handle_request conn ~routes ~memo_cache ~memoized_paths with
      | `Continue -> loop ()
      | `Close -> ()
      | `Need_more -> handle_read_result ~continue:loop (read_more conn)
  in
  loop ()

(** Handle a single client connection *)
let handle_client ~routes ~memo_cache ~memoized_paths flow _addr =
  let conn = create_conn flow in
  try handle_connection conn ~routes ~memo_cache ~memoized_paths
  with exn ->
    Log.err (fun m -> m "Connection error: %s" (Exn.to_string exn))

(** {1 Public API} *)

let run ~sw ~net ~config ?memo_cache ?(memoized_paths = default_memoized_paths)
    routes =
  let addr =
    `Tcp (Eio.Net.Ipaddr.V4.any, config.Arod.Config.server.port)
  in
  let socket = Eio.Net.listen net ~sw ~backlog:128 ~reuse_addr:true addr in
  Log.app (fun m ->
      m "Listening on http://%s:%d" config.server.host config.server.port);
  (* Accept connections in a loop *)
  while true do
    Eio.Net.accept_fork socket ~sw
      ~on_error:(fun exn ->
        Log.err (fun m -> m "Accept error: %s" (Exn.to_string exn)))
      (fun flow addr -> handle_client ~routes ~memo_cache ~memoized_paths flow addr)
  done
