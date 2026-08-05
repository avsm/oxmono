(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** HTTP request serialization using Eio.Buf_write

    This module provides efficient HTTP/1.1 request serialization using Eio's
    buffered write API. It avoids intermediate string allocations by writing
    directly to the output buffer. *)

let src = Logs.Src.create "requests.http_write" ~doc:"HTTP request serialization"
module Log = (val Logs.src_log src : Logs.LOG)

module Write = Eio.Buf_write

(** {1 Low-level Writers} *)

let crlf w =
  Write.string w "\r\n"

let sp w =
  Write.char w ' '

(** {1 Request Line} *)

(** Build authority value (host:port) for CONNECT requests.
    Per RFC 9110 Section 9.3.6: CONNECT uses authority-form as request-target.
    The port is always included for CONNECT since it's establishing a tunnel. *)
let authority_value uri =
  let host = match Uri.host uri with
    | Some h -> h
    | None -> raise (Error.err (Error.Invalid_url {
        url = Uri.to_string uri;
        reason = "URI must have a host for CONNECT"
      }))
  in
  let port = match Uri.port uri with
    | Some p -> p
    | None ->
        (* Default to 443 for CONNECT (typically used for HTTPS tunneling) *)
        match Uri.scheme uri with
        | Some "https" -> 443
        | Some "http" -> 80
        | _ -> 443  (* Default to 443 for tunneling *)
  in
  host ^ ":" ^ string_of_int port

let request_line w ~method_ ~uri =
  (* RFC 9112 Section 3.2: Request target forms depend on method *)
  let request_target =
    if method_ = "CONNECT" then
      (* RFC 9112 Section 3.2.3: CONNECT uses authority-form (host:port) *)
      authority_value uri
    else
      let path = Uri.path uri in
      (* RFC 9112 Section 3.2.4: asterisk-form for server-wide OPTIONS requests.
         When path is "*", use asterisk-form instead of origin-form.
         Example: OPTIONS * HTTP/1.1 *)
      if path = "*" && method_ = "OPTIONS" then
        "*"
      else begin
        let path = if path = "" then "/" else path in
        let query = Uri.query uri in
        if query = [] then path
        else path ^ "?" ^ (Uri.encoded_of_query query)
      end
  in
  Write.string w method_;
  sp w;
  Write.string w request_target;
  Write.string w " HTTP/1.1";
  crlf w

(** {1 Header Writing} *)

let header w ~name ~value =
  Write.string w name;
  Write.string w ": ";
  Write.string w value;
  crlf w

let headers w hdrs =
  Headers.to_list hdrs
  |> List.iter (fun (name, value) -> header w ~name ~value);
  crlf w

(** Build Host header value from URI *)
let host_value uri =
  let host = match Uri.host uri with
    | Some h -> h
    | None -> raise (Error.err (Error.Invalid_url {
        url = Uri.to_string uri;
        reason = "URI must have a host"
      }))
  in
  (* RFC 7230: default ports should be omitted from Host header *)
  match Uri.port uri, Uri.scheme uri with
  | Some p, Some "https" when p <> 443 -> host ^ ":" ^ string_of_int p
  | Some p, Some "http" when p <> 80 -> host ^ ":" ^ string_of_int p
  | Some p, _ -> host ^ ":" ^ string_of_int p
  | None, _ -> host

let request_headers w ~method_ ~uri ~headers:hdrs ~content_length =
  (* Write request line *)
  request_line w ~method_ ~uri;

  (* Per RFC 9110 Section 7.2: Host header handling.
     For CONNECT requests (RFC 9110 Section 9.3.6), Host should be the authority (host:port).
     For other requests, Host should match the URI authority. *)
  let expected_host =
    if method_ = "CONNECT" then authority_value uri
    else host_value uri
  in
  let hdrs = match Headers.get `Host hdrs with
    | None ->
        (* Auto-add Host header from URI *)
        Headers.add `Host expected_host hdrs
    | Some provided_host ->
        (* Validate provided Host matches expected value *)
        if provided_host <> expected_host then
          Log.warn (fun m -> m "Host header '%s' does not match expected '%s' \
            (RFC 9110 Section 7.2)" provided_host expected_host);
        hdrs
  in

  (* Ensure Connection header for keep-alive *)
  let hdrs = if not (Headers.mem `Connection hdrs) then
    Headers.add `Connection "keep-alive" hdrs
  else hdrs in

  (* Add Content-Length if we have a body length *)
  let hdrs = match content_length with
    | Some len when len > 0L && not (Headers.mem `Content_length hdrs) ->
        Headers.add `Content_length (Int64.to_string len) hdrs
    | _ -> hdrs
  in

  (* Write all headers *)
  headers w hdrs

(** {1 Body Writing} *)

let body_string w s =
  if s <> "" then
    Write.string w s

(** Copy from a flow source to the writer, chunk by chunk *)
let body_stream w source =
  let buf = Cstruct.create 8192 in
  let rec copy () =
    match Eio.Flow.single_read source buf with
    | n ->
        Write.cstruct w (Cstruct.sub buf 0 n);
        copy ()
    | exception End_of_file -> ()
  in
  copy ()

(** Write body using chunked transfer encoding *)
let body_chunked w source =
  let buf = Cstruct.create 8192 in
  let rec copy () =
    match Eio.Flow.single_read source buf with
    | n ->
        (* Write chunk size in hex *)
        Write.printf w "%x" n;
        crlf w;
        (* Write chunk data *)
        Write.cstruct w (Cstruct.sub buf 0 n);
        crlf w;
        copy ()
    | exception End_of_file ->
        (* Write final chunk *)
        Write.string w "0";
        crlf w;
        crlf w
  in
  copy ()

(** {1 High-level Request Writing} *)

let request w ~sw ~method_ ~uri ~headers:hdrs ~body =
  let method_str = Method.to_string method_ in

  (* Get content type and length from body *)
  let content_type = Body.content_type body in
  let content_length = Body.content_length body in

  (* Add Content-Type header if body has one *)
  let hdrs = match content_type with
    | Some mime when not (Headers.mem `Content_type hdrs) ->
        Headers.add `Content_type (Mime.to_string mime) hdrs
    | _ -> hdrs
  in

  (* Determine if we need chunked encoding *)
  let use_chunked = Body.Private.is_chunked body in

  let hdrs = if use_chunked && not (Headers.mem `Transfer_encoding hdrs) then
    Headers.add `Transfer_encoding "chunked" hdrs
  else hdrs in

  (* Write request line and headers *)
  request_headers w ~method_:method_str ~uri ~headers:hdrs ~content_length;

  (* Write body *)
  if Body.Private.is_empty body then
    ()
  else if use_chunked then
    Body.Private.write_chunked ~sw w body
  else
    Body.Private.write ~sw w body

(** {1 Headers-Only Writing (for 100-continue)} *)

let request_headers_only w ~method_ ~uri ~headers:hdrs ~content_length =
  let method_str = Method.to_string method_ in
  request_headers w ~method_:method_str ~uri ~headers:hdrs ~content_length

(** {1 Convenience Wrappers} *)

let with_flow ?initial_size flow fn =
  Write.with_flow ?initial_size flow fn

(** Write and flush directly to flow without creating a nested switch.
    This is a simpler alternative to [with_flow] that avoids potential
    issues with nested switches in the Eio fiber system. *)
let write_and_flush ?(initial_size=0x1000) flow fn =
  (* Create a writer without attaching to a switch *)
  let w = Write.create initial_size in
  (* Execute the writing function *)
  fn w;
  (* Serialize to string and copy to flow *)
  let data = Write.serialize_to_string w in
  if String.length data > 0 then
    Eio.Flow.copy_string data flow

(** {1 Proxy Request Writing} *)

(** Write request line using absolute-URI form for proxy requests.
    Per RFC 9112 Section 3.2.2 *)
let request_line_absolute w ~method_ ~uri =
  Write.string w method_;
  sp w;
  (* Use full absolute URI - write directly to buffer for efficiency *)
  Huri.write w uri;
  Write.string w " HTTP/1.1";
  crlf w

(** Write request headers for proxy request with absolute-URI *)
let request_headers_proxy w ~method_ ~uri ~headers:hdrs ~content_length ~proxy_auth =
  (* Write request line with absolute URI *)
  request_line_absolute w ~method_ ~uri;

  (* Ensure Host header is present *)
  let hdrs = if not (Headers.mem `Host hdrs) then
    Headers.add `Host (host_value uri) hdrs
  else hdrs in

  (* Ensure Connection header for keep-alive *)
  let hdrs = if not (Headers.mem `Connection hdrs) then
    Headers.add `Connection "keep-alive" hdrs
  else hdrs in

  (* Add Content-Length if we have a body length *)
  let hdrs = match content_length with
    | Some len when len > 0L && not (Headers.mem `Content_length hdrs) ->
        Headers.add `Content_length (Int64.to_string len) hdrs
    | _ -> hdrs
  in

  (* Add Proxy-Authorization if configured *)
  let hdrs = match proxy_auth with
    | Some value -> Headers.add `Proxy_authorization value hdrs
    | None -> hdrs
  in

  (* Write all headers *)
  headers w hdrs

(** Write complete HTTP request via proxy using absolute-URI form *)
let request_via_proxy w ~sw ~method_ ~uri ~headers:hdrs ~body ~proxy_auth =
  let method_str = Method.to_string method_ in

  (* Get content type and length from body *)
  let content_type = Body.content_type body in
  let content_length = Body.content_length body in

  (* Add Content-Type header if body has one *)
  let hdrs = match content_type with
    | Some mime when not (Headers.mem `Content_type hdrs) ->
        Headers.add `Content_type (Mime.to_string mime) hdrs
    | _ -> hdrs
  in

  (* Determine if we need chunked encoding *)
  let use_chunked = Body.Private.is_chunked body in

  let hdrs = if use_chunked && not (Headers.mem `Transfer_encoding hdrs) then
    Headers.add `Transfer_encoding "chunked" hdrs
  else hdrs in

  (* Write request line and headers *)
  request_headers_proxy w ~method_:method_str ~uri ~headers:hdrs
    ~content_length ~proxy_auth;

  (* Write body *)
  if Body.Private.is_empty body then
    ()
  else if use_chunked then
    Body.Private.write_chunked ~sw w body
  else
    Body.Private.write ~sw w body
