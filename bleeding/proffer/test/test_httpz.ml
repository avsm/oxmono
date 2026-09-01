open Proffer
open Proffer.Route
module St = Httpz.Res
module M = Httpz.Method

let checks = ref 0

let check name b =
  incr checks;
  if not b then (
    prerr_endline ("FAIL: " ^ name);
    exit 1)

type env = { greet : string -> string }

let index = "<h1>index</h1>"

let html_escape s =
  let b = Buffer.create (String.length s) in
  String.iter
    (function
      | '&' -> Buffer.add_string b "&amp;"
      | '<' -> Buffer.add_string b "&lt;"
      | '>' -> Buffer.add_string b "&gt;"
      | '"' -> Buffer.add_string b "&quot;"
      | '\'' -> Buffer.add_string b "&#39;"
      | c -> Buffer.add_char b c)
    s;
  Buffer.contents b

(* What a client reads as a second response if a body overruns its declared
   length under a keep-alive head. *)
let forged = "HTTP/1.1 200 OK\r\nContent-Length: 5\r\n\r\nowned"
let huge_trailer_body_ran = Atomic.make false

let echo socket =
  let ping = Bytes.create 4 in
  let rec fill off =
    if off < Bytes.length ping then
      match
        Body.Socket.read socket ping ~off ~len:(Bytes.length ping - off)
      with
      | 0 -> failwith "handoff ended before ping"
      | n -> fill (off + n)
  in
  fill 0;
  if not (Bytes.equal ping (Bytes.of_string "ping")) then
    failwith "handoff received something other than ping";
  Body.Socket.write socket "pong"

let routes =
  [
    get root (fun _env _req respond -> Resp.html respond index);
    get (s "hello" / str) (fun who env _req respond ->
        Resp.html respond (env.greet (Req.globalize who)));
    get (s "cached") (fun _env _req respond ->
        Resp.html respond ~etag:(Etag.strong "v1") "<p>cached</p>");
    get (s "stream") (fun _env _req respond ->
        Resp.v respond ~headers:Headers.empty ~content_type:(This "text/plain")
          (Body.Stream
             {
               length = None;
               write =
                 (fun sink ->
                   Body.Sink.write sink "ab";
                   Body.Sink.write sink "cd");
               trailers = Headers.empty;
             }));
    get (s "known") (fun _env _req respond ->
        Resp.v respond ~headers:Headers.empty ~content_type:(This "text/plain")
          (Body.Stream
             {
               length = Some 4L;
               write =
                 (fun sink ->
                   Body.Sink.write sink "ab";
                   Body.Sink.write sink "cd");
               trailers = Headers.empty;
             }));
    get (s "trailers") (fun _env _req respond ->
        Resp.stream respond ~length:5L
          ~trailers:[ Resp.other "X-Checksum" "ok" ]
          "text/plain"
          (fun sink -> Body.Sink.write sink "hello"));
    get (s "huge-trailer") (fun _env _req respond ->
        Resp.stream respond
          ~trailers:[ Resp.other "X-Huge" (String.make 30000 'x') ]
          "text/plain" (fun _sink -> Atomic.set huge_trailer_body_ran true));
    route M.Connect (s "example.test:443") (fun _env _req respond ->
        Resp.tunnel respond echo);
    get (s "upgrade") (fun _env _req respond ->
        Resp.upgrade respond ~protocol:"proffer-echo" echo);
    get (s "upgrade-required") (fun _env _req respond ->
        Resp.v respond ~status:St.Upgrade_required
          ~headers:[ Resp.h Httpz.Header_name.Upgrade "proffer-echo" ]
          ~content_type:Null Body.Empty);
    get (s "upgrade-raise") (fun _env _req respond ->
        Resp.upgrade respond ~protocol:"proffer-echo" (fun _socket ->
            failwith "upgraded session failed"));
    get (s "logged") (fun _env _req respond ->
        Resp.v respond ~content_type:(This "text/html; charset=utf-8")
          ~headers:[ Resp.other "X-Cache" "hit" ]
          (Body.String "hi"));
    get (s "supplied-date") (fun _env _req respond ->
        Resp.text respond
          ~headers:
            [ Headers.h Httpz.Header_name.Date
                "Sun, 06 Nov 1994 08:49:37 GMT" ]
          "hello");
    get (s "no-content") (fun _env _req respond ->
        Resp.text respond ~status:St.No_content "forbidden");
    get (s "reset-content") (fun _env _req respond ->
        Resp.text respond ~status:St.Reset_content "forbidden");
    get (s "dup") (fun _env req respond ->
        Resp.text respond
          (match Req.header_other req "X-Dup" with
           | Some v -> Req.globalize v
           | None -> "none"));
    post (s "form") (fun _env req respond ->
        match Req.form_param req "who" with
        | Some who -> Resp.see_other respond ("/hello/" ^ who)
        | None -> Resp.bad_request respond ());
    post (s "upload") (fun _env req respond ->
        Resp.text respond (string_of_int (String.length (Req.body req))));
    get (s "boom") (fun _env _req _respond -> failwith "handler exploded");
    (* Declares three bytes and then tries to append a whole forged response,
       the shape an application takes when it proxies upstream bytes under a
       precomputed length. *)
    get
      (s "overrun")
      (fun _env _req respond ->
        Resp.v respond ~headers:Headers.empty ~content_type:(This "text/plain")
          (Body.Stream
             {
               length = Some 3L;
               write =
                 (fun sink ->
                   Body.Sink.write sink "ok\n";
                   Body.Sink.write sink forged);
               trailers = Headers.empty;
             }));
  ]

let compiled =
  (Site.with_fallback
       (fun _env _req respond ->
         Resp.html respond ~status:St.Not_found "<p>missing</p>")
       (Site.of_routes routes))

let env = { greet = (fun who -> "<p>hello " ^ html_escape who ^ "</p>") }

type response = {
  line : string;
  headers : (string * string) list;
  body : string;
  trailers : (string * string) list;
}

let split_header l =
  match String.index_opt l ':' with
  | None -> (String.lowercase_ascii l, "")
  | Some i ->
      let name = String.lowercase_ascii (String.sub l 0 i) in
      let value =
        String.trim (String.sub l (i + 1) (String.length l - i - 1))
      in
      (name, value)

(* Chunk sizes may carry extensions after a ';', which this server never
   sends, so the whole line is the size. *)
let read_chunked r =
  let buf = Buffer.create 64 in
  let rec go () =
    let n = int_of_string ("0x" ^ Eio.Buf_read.line r) in
    if n > 0 then begin
      Buffer.add_string buf (Eio.Buf_read.take n r);
      ignore (Eio.Buf_read.line r : string);
      go ()
    end
    else
      let rec fields acc =
        match Eio.Buf_read.line r with
        | "" -> List.rev acc
        | line -> fields (split_header line :: acc)
      in
      fields []
  in
  let trailers = go () in
  (Buffer.contents buf, trailers)

let read_head r =
  let line = Eio.Buf_read.line r in
  let rec fields acc =
    match Eio.Buf_read.line r with
    | "" -> List.rev acc
    | l -> fields (split_header l :: acc)
  in
  (line, fields [])

(* [read_response ~head r] reads one response. [head] says the request was a
   HEAD, whose response carries the length of a body it does not send. *)
let read_response ~head r =
  let line, headers = read_head r in
  let body, trailers =
    if head then ("", [])
    else
      match List.assoc_opt "transfer-encoding" headers with
      | Some "chunked" -> read_chunked r
      | _ -> (
          match List.assoc_opt "content-length" headers with
          | Some n -> (Eio.Buf_read.take (int_of_string n) r, [])
          (* No framing field at all. A body, if there is one, runs to the
             end of the connection, which the server announces by closing. *)
          | None ->
              if List.assoc_opt "connection" headers = Some "close" then
                (Eio.Buf_read.take_all r, [])
              else ("", []))
  in
  { line; headers; body; trailers }

let field resp name = List.assoc_opt name resp.headers

let get_req ?(headers = "") path =
  Printf.sprintf "GET %s HTTP/1.1\r\nHost: localhost\r\n%s\r\n" path headers

let loopback = Eio.Net.Ipaddr.V4.loopback
let on_error exn = prerr_endline (Printexc.to_string exn)

module Wrapped_flow = struct
  type counts = { mutable shutdowns : int; mutable closes : int }
  type t = { flow : Httpz_tls.flow; counts : counts }

  let read_methods = []
  let single_read t buf = Eio.Flow.single_read t.flow buf
  let single_write t bufs = Eio.Flow.single_write t.flow bufs
  let copy t ~src = Eio.Flow.Pi.simple_copy ~single_write t ~src

  let shutdown t cmd =
    t.counts.shutdowns <- t.counts.shutdowns + 1;
    Eio.Flow.shutdown t.flow cmd

  let close t =
    t.counts.closes <- t.counts.closes + 1;
    Eio.Resource.close t.flow
end

let wrapped_flow counts flow : Httpz_tls.flow =
  let handler =
    Eio.Resource.handler
      (Eio.Resource.H (Eio.Resource.Close, Wrapped_flow.close)
      :: Eio.Resource.bindings (Eio.Flow.Pi.two_way (module Wrapped_flow)))
  in
  Eio.Resource.T ({ Wrapped_flow.flow; counts }, handler)

(* Wait for [on_listening] so tests never guess or race the selected port. *)
let with_server ~net ~clock ~mono_clock ?config ?tls ?on_event
    ?(on_error = on_error) f =
  let bound, set_bound = Eio.Promise.create () in
  Eio.Fiber.first
    (fun () ->
      Proffer_httpz.run ~port:0 ?config ?tls
        ~on_listening:(Eio.Promise.resolve set_bound)
        ?on_event ~on_error
        object
          method net = net
          method clock = clock
          method mono_clock = mono_clock
        end
        ~env compiled)
    (fun () -> f (Eio.Promise.await bound))

let with_conn ~net addr f =
  Eio.Switch.run @@ fun sw -> f (Eio.Net.connect ~sw net addr)

let session ~net addr f =
  with_conn ~net addr @@ fun flow ->
  let r = Eio.Buf_read.of_flow flow ~max_size:65536 in
  let send s = Eio.Flow.copy_string s flow in
  f send (fun ~head -> read_response ~head r)

let request ~net addr s =
  session ~net addr (fun send recv ->
      send s;
      recv ~head:false)

(* Bound tests of the timeout implementation so a regression cannot hang. *)
let within ~clock name f =
  match Eio.Time.with_timeout clock 5. (fun () -> Ok (f ())) with
  | Ok v -> v
  | Error `Timeout ->
      prerr_endline ("FAIL: " ^ name ^ " (hung)");
      exit 1

let events = ref []

let tests ~clock ~net addr =
  let request = request ~net addr in
  let resp = request (get_req "/") in
  check "200 status line" (resp.line = "HTTP/1.1 200 OK");
  check "200 body" (resp.body = index);
  check "content length"
    (field resp "content-length" = Some (string_of_int (String.length index)));
  check "content type"
    (field resp "content-type" = Some "text/html; charset=utf-8");
  check "keep-alive announced" (field resp "connection" = Some "keep-alive");

  let resp = request (get_req "/hello/world") in
  check "captured segment" (resp.body = "<p>hello world</p>");

  let resp = request (get_req "/hello/a%20b") in
  check "decoded segment" (resp.body = "<p>hello a b</p>");
  let resp = request (get_req "/hello/%3Csvg%20onload%3D%22x%22%3E%26%27") in
  check "captured HTML is escaped"
    (resp.body = "<p>hello &lt;svg onload=&quot;x&quot;&gt;&amp;&#39;</p>");

  let resp =
    request "GET http://localhost HTTP/1.1\r\nHost: localhost\r\n\r\n"
  in
  check "empty absolute path routes as slash" (resp.body = index);
  let resp =
    request
      "GET http://localhost/hello/proxy?q=1 HTTP/1.1\r\nHost: localhost\r\n\r\n"
  in
  check "absolute-form path is routed" (resp.body = "<p>hello proxy</p>");

  let resp = request (get_req "/nowhere") in
  check "404 status line" (resp.line = "HTTP/1.1 404 Not Found");
  check "404 body" (resp.body = "<p>missing</p>");

  let resp =
    request
      "POST /form HTTP/1.1\r\n\
       Host: localhost\r\n\
       Content-Type: application/x-www-form-urlencoded\r\n\
       Content-Length: 8\r\n\
       \r\n\
       who=avsm"
  in
  check "303 status line" (resp.line = "HTTP/1.1 303 See Other");
  check "303 location" (field resp "location" = Some "/hello/avsm");
  let resp = request (get_req "/hello/avsm") in
  check "posted then fetched" (resp.body = "<p>hello avsm</p>");

  let resp = request (get_req "/cached") in
  check "etag sent" (field resp "etag" = Some "\"v1\"");
  let resp =
    request (get_req ~headers:"If-None-Match: \"v1\"\r\n" "/cached")
  in
  check "304 status line" (resp.line = "HTTP/1.1 304 Not Modified");
  check "304 body" (resp.body = "");
  check "304 has no length" (field resp "content-length" = None);
  check "304 keeps the etag" (field resp "etag" = Some "\"v1\"");

  let resp = request (get_req "/no-content") in
  check "204 status line" (resp.line = "HTTP/1.1 204 No Content");
  check "204 body is suppressed" (resp.body = "");
  check "204 has no length" (field resp "content-length" = None);
  check "204 is not chunked" (field resp "transfer-encoding" = None);

  let resp = request (get_req "/reset-content") in
  check "205 status line" (resp.line = "HTTP/1.1 205 Reset Content");
  check "205 body is suppressed" (resp.body = "");
  check "205 has zero length" (field resp "content-length" = Some "0");

  (* The GET that follows on the same connection is what proves the HEAD
     sent no body: a stray one would be read as the next status line. *)
  session ~net addr (fun send recv ->
      send "HEAD / HTTP/1.1\r\nHost: localhost\r\n\r\n";
      let resp = recv ~head:true in
      check "head status line" (resp.line = "HTTP/1.1 200 OK");
      check "head keeps the length"
        (field resp "content-length"
        = Some (string_of_int (String.length index)));
      send (get_req "/");
      let next = recv ~head:false in
      check "head sent no body" (next.line = "HTTP/1.1 200 OK");
      check "the connection is still framed" (next.body = index));

  let resp = request (get_req "/stream") in
  check "unknown stream is chunked"
    (field resp "transfer-encoding" = Some "chunked");
  check "stream has no length" (field resp "content-length" = None);
  check "stream body" (resp.body = "abcd");

  let resp = request (get_req "/trailers") in
  check "trailers force chunked framing"
    (field resp "transfer-encoding" = Some "chunked");
  check "a trailer response omits Content-Length"
    (field resp "content-length" = None);
  check "trailer names are declared" (field resp "trailer" = Some "X-Checksum");
  check "the trailer body is complete" (resp.body = "hello");
  check "trailers are sent after the body"
    (List.assoc_opt "x-checksum" resp.trailers = Some "ok");

  (* A trailer block is checked while a clean error response can still replace
     it. In particular, its streaming callback has not run and no 200 head has
     reached the connection. *)
  let resp = request (get_req "/huge-trailer") in
  check "an oversized trailer block is a clean 500"
    (resp.line = "HTTP/1.1 500 Internal Server Error");
  check "an oversized trailer block closes"
    (field resp "connection" = Some "close");
  check "an oversized trailer does not start its body"
    (not (Atomic.get huge_trailer_body_ran));

  let handoff request expected_line connection upgrade =
    with_conn ~net addr @@ fun flow ->
    Eio.Flow.copy_string (request ^ "ping") flow;
    let reader = Eio.Buf_read.of_flow flow ~max_size:65536 in
    let line, headers = read_head reader in
    check "handoff status line" (line = expected_line);
    check "handoff has no Content-Length"
      (List.assoc_opt "content-length" headers = None);
    check "handoff has no Transfer-Encoding"
      (List.assoc_opt "transfer-encoding" headers = None);
    check "handoff connection field"
      (List.assoc_opt "connection" headers = connection);
    check "handoff upgrade field" (List.assoc_opt "upgrade" headers = upgrade);
    check "already-buffered handoff bytes reach the application"
      (Eio.Buf_read.take 4 reader = "pong")
  in
  handoff "CONNECT example.test:443 HTTP/1.1\r\nHost: example.test:443\r\n\r\n"
    "HTTP/1.1 200 OK" None None;
  handoff
    "GET /upgrade HTTP/1.1\r\n\
     Host: localhost\r\n\
     Connection: Upgrade\r\n\
     Upgrade: proffer-echo\r\n\
     \r\n"
    "HTTP/1.1 101 Switching Protocols" (Some "Upgrade") (Some "proffer-echo");
  let resp = request "GET /upgrade HTTP/1.1\r\nHost: localhost\r\n\r\n" in
  check "an absent Upgrade offer is rejected before handoff"
    (resp.line = "HTTP/1.1 500 Internal Server Error");
  let resp =
    request
      "GET /upgrade HTTP/1.1\r\n\
       Host: localhost\r\n\
       Connection: Upgrade\r\n\
       Upgrade: different\r\n\
       \r\n"
  in
  check "a mismatched Upgrade offer is rejected before handoff"
    (resp.line = "HTTP/1.1 500 Internal Server Error");
  let resp =
    request
      "GET /upgrade HTTP/1.0\r\n\
       Connection: Upgrade\r\n\
       Upgrade: proffer-echo\r\n\
       \r\n"
  in
  check "HTTP/1.0 Upgrade is rejected before handoff"
    (resp.line = "HTTP/1.0 500 Internal Server Error");
  let resp = request (get_req "/upgrade-required") in
  check "426 advertises Upgrade"
    (resp.line = "HTTP/1.1 426 Upgrade Required"
    && field resp "upgrade" = Some "proffer-echo");
  check "426 names Upgrade as a connection option"
    (field resp "connection" = Some "Upgrade");
  let resp =
    request
      "GET /upgrade HTTP/1.1\r\n\
       Host: localhost\r\n\
       Connection: Upgrade, bad/value\r\n\
       Upgrade: proffer-echo\r\n\
       \r\n"
  in
  check "malformed Connection cannot authorize an upgrade"
    (resp.line = "HTTP/1.1 400 Bad Request");

  let resp =
    request
      "POST /form HTTP/1.1\r\n\
       Host: localhost\r\n\
       Content-Type: application/x-www-form-urlencoded\r\n\
       Transfer-Encoding: chunked\r\n\
       \r\n\
       3;part=one\r\n\
       who\r\n\
       5\r\n\
       =avsm\r\n\
       0\r\n\
       X-Trailer: yes\r\n\
       \r\n"
  in
  check "chunked request is decoded" (resp.line = "HTTP/1.1 303 See Other");
  check "chunked form reaches handler"
    (field resp "location" = Some "/hello/avsm");

  let resp =
    request
      "POST /form HTTP/1.1\r\nHost: localhost\r\n\
       Transfer-Encoding: chunked\r\n\r\nz\r\n"
  in
  check "malformed chunked request is 400"
    (resp.line = "HTTP/1.1 400 Bad Request");

  let resp =
    request
      "POST /form HTTP/1.1\r\nHost: localhost\r\n\
       Expect: fancy-feature\r\nContent-Length: 0\r\n\r\n"
  in
  check "unsupported expectation is 417"
    (resp.line = "HTTP/1.1 417 Expectation Failed");

  let resp = request "PURGE / HTTP/1.1\r\nHost: localhost\r\n\r\n" in
  check "unsupported method is 501"
    (resp.line = "HTTP/1.1 501 Not Implemented");

  (* Answered from the header block alone: none of those 40000 bytes is
     ever read, since there is nowhere to put them. *)
  let resp =
    request
      "POST /form HTTP/1.1\r\nHost: localhost\r\nContent-Length: 40000\r\n\r\n"
  in
  check "an oversized body is 413"
    (resp.line = "HTTP/1.1 413 Payload Too Large");

  (* When the peer has already sent the refused body, draining it before close
     prevents an unread-data reset from erasing the 413 on common TCP stacks. *)
  let resp =
    request
      ("POST /form HTTP/1.1\r\nHost: localhost\r\nContent-Length: 40000\r\n\r\n"
     ^ String.make 40000 'x')
  in
  check "a coalesced refused body still receives its 413"
    (resp.line = "HTTP/1.1 413 Payload Too Large");

  (* A refused request is logged with what the parse knew of it. No route
     ran, so it has no path, and the reply is the server's own rather than a
     handler's, so neither response field is set. Content-Length is consumed
     for framing and so is not among the fields. *)
  Eio.Time.sleep clock 0.05;
  (match !events with
  | e :: _ ->
      let names =
        List.map
          (fun (name, _) -> String.lowercase_ascii name)
          e.Proffer_httpz.request_headers
      in
      check "413 event status" (Status.code e.Proffer_httpz.status = 413);
      check "413 event keeps the request fields" (names = [ "host" ]);
      check "413 event has no path" (e.Proffer_httpz.path = "");
      check "413 event has no response fields"
        (e.Proffer_httpz.response_content_type = None
        && e.Proffer_httpz.cache_status = None)
  | [] -> check "an event was recorded" false);

  (* A body arriving coalesced with the head counts against the read window,
     not the header-size limit: the head alone is far under 16 KiB. *)
  let resp =
    request
      ("POST /upload HTTP/1.1\r\n\
        Host: localhost\r\n\
        Content-Length: 20000\r\n\
        \r\n" ^ String.make 20000 'x')
  in
  check "a coalesced 20k body is not a header overflow"
    (resp.line = "HTTP/1.1 200 OK");
  check "the coalesced body reaches the handler whole" (resp.body = "20000");

  (* HTTP/1.0 defaults to closing and has no chunked encoding. *)
  let resp = request "GET / HTTP/1.0\r\n\r\n" in
  check "1.0 status line" (resp.line = "HTTP/1.0 200 OK");
  check "1.0 body" (resp.body = index);
  check "1.0 closes without being asked"
    (field resp "connection" = Some "close");

  let resp = request "GET /stream HTTP/1.0\r\n\r\n" in
  check "1.0 gets no chunked encoding"
    (field resp "transfer-encoding" = None);
  check "1.0 stream declares no length"
    (field resp "content-length" = None);
  check "1.0 stream is delimited by the close" (resp.body = "abcd");

  let resp = request "GET /trailers HTTP/1.0\r\n\r\n" in
  check "1.0 rejects a response that requires trailers"
    (resp.line = "HTTP/1.0 500 Internal Server Error");
  check "the unrepresentable trailer response closes"
    (field resp "connection" = Some "close");

  let resp = request (get_req "/known") in
  check "known stream has a length" (field resp "content-length" = Some "4");
  check "known stream is not chunked"
    (field resp "transfer-encoding" = None);
  check "known stream body" (resp.body = "abcd");
  check "known stream keeps the connection"
    (field resp "connection" = Some "keep-alive");

  let resp = request (get_req "/form") in
  check "405 status line" (resp.line = "HTTP/1.1 405 Method Not Allowed");
  check "405 allow" (field resp "allow" = Some "POST");
  check "405 body" (resp.body = "Method Not Allowed\n");

  (* Check every response-derived field exposed to an access log. *)
  let resp =
    request
      (get_req ~headers:"Accept: text/html\r\nUser-Agent: t\r\n" "/logged?q=1")
  in
  check "logged body" (resp.body = "hi");
  Eio.Time.sleep clock 0.05;
  (match !events with
  | e :: _ ->
      let sent =
        List.map
          (fun (name, value) -> (String.lowercase_ascii name, value))
          e.Proffer_httpz.request_headers
      in
      check "event path" (e.Proffer_httpz.path = "/logged");
      check "event request headers arrive in order"
        (List.map fst sent = [ "host"; "accept"; "user-agent" ]);
      check "event request headers"
        (List.assoc_opt "accept" sent = Some "text/html");
      check "event response content type"
        (e.Proffer_httpz.response_content_type
        = Some "text/html; charset=utf-8");
      check "event cache status" (e.Proffer_httpz.cache_status = Some "hit")
  | [] -> check "an event was recorded" false);

  (* A handler reads a field through a first-match lookup, so the request has
     to reach it in arrival order. Answering with the last value sent would
     let a client override an Authorization or an If-None-Match by repeating
     it, and would differ from what the mock backend does. *)
  let resp =
    request (get_req ~headers:"X-Dup: first\r\nX-Dup: second\r\n" "/dup")
  in
  check "a repeated field reads as the first one sent" (resp.body = "first");

  (* An access log is a place credentials leak to. The handler sees the real
     values; the event copy must not. *)
  let resp =
    request
      (get_req
         ~headers:
           "Authorization: Basic c2VjcmV0\r\n\
            Proxy-Authorization: Bearer tok\r\n\
            Cookie: session=abc\r\n\
            Accept: text/html\r\n"
         "/logged?token=hunter2")
  in
  check "credentialed request still answers" (resp.body = "hi");
  Eio.Time.sleep clock 0.05;
  (match !events with
  | e :: _ ->
      let sent =
        List.map
          (fun (name, value) -> (String.lowercase_ascii name, value))
          e.Proffer_httpz.request_headers
      in
      check "authorization is redacted"
        (List.assoc_opt "authorization" sent = Some "<redacted>");
      check "proxy-authorization is redacted"
        (List.assoc_opt "proxy-authorization" sent = Some "<redacted>");
      check "cookie is redacted"
        (List.assoc_opt "cookie" sent = Some "<redacted>");
      check "other fields are untouched"
        (List.assoc_opt "accept" sent = Some "text/html");
      check "path drops the query" (e.Proffer_httpz.path = "/logged");
      check "target keeps the query"
        (e.Proffer_httpz.target = "/logged?token=hunter2")
  | [] -> check "an event was recorded" false);

  let resp = request (get_req "/supplied-date") in
  let dates = List.filter (fun (name, _) -> name = "date") resp.headers in
  check "an application Date remains singleton" (List.length dates = 1);
  check "an application Date is retained"
    (field resp "date" = Some "Sun, 06 Nov 1994 08:49:37 GMT");

  (* A head the parser refuses never reaches the site, so the status has to
     come from the parse failure and not a blanket 400. *)
  let long = get_req ("/" ^ String.make 9000 'a') in
  let resp = request long in
  check "414 status line" (resp.line = "HTTP/1.1 414 URI Too Long");
  check "414 closes" (field resp "connection" = Some "close");
  let large_head =
    get_req ~headers:("X-Pad: " ^ String.make 33000 'x' ^ "\r\n") "/"
  in
  let resp = request large_head in
  check "431 status line"
    (resp.line = "HTTP/1.1 431 Request Header Fields Too Large");
  check "431 closes" (field resp "connection" = Some "close");
  let resp =
    request
      "POST /upload HTTP/1.1\r\n\
       Host: localhost\r\n\
       Transfer-Encoding: gzip, chunked\r\n\
       \r\n"
  in
  check "501 status line" (resp.line = "HTTP/1.1 501 Not Implemented");
  check "501 closes" (field resp "connection" = Some "close");
  let resp =
    request
      "POST /upload HTTP/1.1\r\n\
       Host: localhost\r\n\
       Transfer-Encoding: identity\r\n\
       \r\n"
  in
  check "identity coding is 400" (resp.line = "HTTP/1.1 400 Bad Request");
  check "identity rejection closes" (field resp "connection" = Some "close");
  with_conn ~net addr @@ fun flow ->
  let reader = Eio.Buf_read.of_flow flow ~max_size:65536 in
  Eio.Flow.copy_string
    "POST /upload HTTP/1.1\r\n\
     Host: localhost\r\n\
     Transfer-Encoding: identity\r\n\
     \r\n\
     GET / HTTP/1.1\r\n\
     Host: localhost\r\n\
     \r\n"
    flow;
  let resp = read_response ~head:false reader in
  check "smuggling form is rejected" (resp.line = "HTTP/1.1 400 Bad Request");
  check "smuggling rejection closes promptly"
    (match
       Eio.Time.with_timeout clock 1. (fun () ->
           Ok (Eio.Buf_read.take_all reader))
     with
    | Ok "" -> true
    | Ok _ | Error `Timeout -> false);

  (* A rejection answers an HTTP/1.0 request with an HTTP/1.1 status line,
     which RFC 9112 2.5 permits because nothing 1.1-only is used and the
     connection closes. *)
  let resp = request "GET /\000 HTTP/1.0\r\n\r\n" in
  check "a 1.0 rejection uses a 1.1 status line"
    (resp.line = "HTTP/1.1 400 Bad Request");

  (* An event is recorded once its response has been written, so the count
     is only stable after the serving fibre has had a turn. *)
  Eio.Time.sleep clock 0.05;
  let before = List.length !events in
  let resp = request "@@@ not a request\r\n\r\n" in
  check "400 status line" (resp.line = "HTTP/1.1 400 Bad Request");
  check "400 closes" (field resp "connection" = Some "close");
  Eio.Time.sleep clock 0.05;
  check "an unparsable request logs nothing"
    (List.length !events = before);

  (* Two requests down one connection, the second sent only after the first
     is read, so a mistake in framing shows up as a hang or a bad line. *)
  session ~net addr (fun send recv ->
      send (get_req "/");
      let first = recv ~head:false in
      send (get_req "/hello/again");
      let second = recv ~head:false in
      check "keep-alive first" (first.body = index);
      check "keep-alive second" (second.body = "<p>hello again</p>"));

  Eio.Time.sleep clock 0.05;
  let event_count = List.length !events in
  check (Printf.sprintf "one event per parsed request (got %d)" event_count)
    (event_count = 41);
  match !events with
  | last :: _ ->
      check "event method" (Method.equal last.Proffer_httpz.meth M.Get);
      check "event target" (last.Proffer_httpz.target = "/hello/again");
      check "event status" (Status.code last.Proffer_httpz.status = 200);
      check "event body size" (last.Proffer_httpz.body_size = 18)
  | [] -> check "an event was recorded" false

(* Short enough that the whole section runs in well under a second, and one
   connection at a time so that the cap is reached by opening two. *)
let short_config =
  {
    Proffer_httpz.default_config with
    max_connections = 1;
    first_byte_timeout = 0.2;
    idle_timeout = 0.2;
    request_timeout = 0.2;
  }

let timeout_tests ~clock ~net addr =
  within ~clock "idle timeout" (fun () ->
      with_conn ~net addr (fun flow ->
          let r = Eio.Buf_read.of_flow flow ~max_size:4096 in
          check "an idle connection is closed with no reply"
            (Eio.Buf_read.take_all r = "")));

  within ~clock "request timeout" (fun () ->
      with_conn ~net addr (fun flow ->
          Eio.Flow.copy_string "GET / HTTP/1.1\r\nHost: localhost\r\n" flow;
          let r = Eio.Buf_read.of_flow flow ~max_size:4096 in
          let resp = read_response ~head:false r in
          check "a half-sent request is 408"
            (resp.line = "HTTP/1.1 408 Request Timeout");
          check "408 closes" (field resp "connection" = Some "close")));

  (* The cap is one, so the second connection is accepted only once the
     first has idled out, and its response cannot arrive any sooner. *)
  within ~clock "connection cap" (fun () ->
      with_conn ~net addr (fun _held ->
          Eio.Time.sleep clock 0.05;
          let t0 = Eio.Time.now clock in
          let resp = request ~net addr (get_req "/") in
          check "a queued connection is served once a slot frees"
            (resp.body = index);
          check "a queued connection waits for the slot"
            (Eio.Time.now clock -. t0 >= 0.1)))

(* A declared Content-Length is what the client frames the connection by, so
   the sink must refuse the first byte past it: bytes already sent cannot be
   retracted, and here they spell a whole second response. *)
let overrun_tests ~clock ~mono_clock ~net =
  let seen = ref [] in
  let on_error exn = seen := exn :: !seen in
  with_server ~net ~clock ~mono_clock ~on_error (fun addr ->
      within ~clock "declared length" (fun () ->
          with_conn ~net addr (fun flow ->
              Eio.Flow.copy_string (get_req "/overrun") flow;
              let r = Eio.Buf_read.of_flow flow ~max_size:65536 in
              let resp = read_response ~head:false r in
              check "an overrun stream declares its length"
                (field resp "content-length" = Some "3");
              check "an overrun stream sends only what it declared"
                (resp.body = "ok\n");
              check "no forged bytes follow the declared body"
                (Eio.Buf_read.take_all r = ""))));
  check "the overrun is reported to on_error"
    (match !seen with
    | Invalid_argument m :: _ ->
        String.length m > 0
        && String.starts_with ~prefix:"Proffer_httpz: streamed body declared" m
    | _ -> false)

(* Both callbacks run in the connection fibre, whose failure would fail the
   server switch and close the listening socket with it. *)
let callback_tests ~clock ~mono_clock ~net =
  let calls = ref 0 in
  let on_error _ =
    incr calls;
    failwith "on_error exploded"
  in
  with_server ~net ~clock ~mono_clock ~on_error (fun addr ->
      within ~clock "raising on_error" (fun () ->
          let resp = request ~net addr (get_req "/boom") in
          check "a failing handler is still 500"
            (resp.line = "HTTP/1.1 500 Internal Server Error");
          let resp = request ~net addr (get_req "/") in
          check "the listener survives a raising on_error" (resp.body = index)));
  check "the raising on_error was called" (!calls > 0);
  let seen = ref 0 in
  let on_error _ = incr seen in
  let on_event _ = failwith "on_event exploded" in
  with_server ~net ~clock ~mono_clock ~on_error ~on_event (fun addr ->
      within ~clock "raising on_event" (fun () ->
          let resp = request ~net addr (get_req "/") in
          check "a raising on_event leaves the response alone"
            (resp.body = index);
          (* The refusal path reports its event outside the handler guard,
             where a raise used to reach the server switch. *)
          let resp =
            request ~net addr
              "POST /upload HTTP/1.1\r\n\
               Host: localhost\r\n\
               Content-Length: 40000\r\n\
               \r\n"
          in
          check "a raising on_event leaves a refusal alone"
            (resp.line = "HTTP/1.1 413 Payload Too Large");
          let resp = request ~net addr (get_req "/") in
          check "the listener survives a raising on_event" (resp.body = index);
          (* An event is recorded once its response is written, so the count
             is only stable after the serving fibres have had a turn. *)
          Eio.Time.sleep clock 0.05;
          check "a raising on_event is reported to on_error" (!seen >= 3)))

let handoff_event_tests ~clock ~mono_clock ~net =
  let events = ref [] and errors = ref 0 in
  with_server ~net ~clock ~mono_clock
    ~on_event:(fun event ->
      events := Proffer_httpz.globalize_event event :: !events)
    ~on_error:(fun _ -> incr errors)
    (fun addr ->
      within ~clock "raising handoff telemetry" (fun () ->
          with_conn ~net addr (fun flow ->
              Eio.Flow.copy_string
                "GET /upgrade-raise HTTP/1.1\r\n\
                 Host: localhost\r\n\
                 Connection: Upgrade\r\n\
                 Upgrade: proffer-echo\r\n\
                 \r\n"
                flow;
              let reader = Eio.Buf_read.of_flow flow ~max_size:65536 in
              let line, _headers = read_head reader in
              check "a raising upgraded session still sends 101"
                (line = "HTTP/1.1 101 Switching Protocols"));
          Eio.Time.sleep clock 0.01;
          check "a raising upgraded session records its HTTP response once"
            (match !events with
            | [ event ] -> Status.code event.status = 101 && event.body_size = 0
            | _ -> false);
          check "a raising upgraded session is reported" (!errors = 1)))

let continue_tests ~clock ~mono_clock ~net =
  with_server ~net ~clock ~mono_clock (fun addr ->
      (* An oversized body is refused from the head alone, so inviting it
         would only earn a 413 for bytes nobody wanted. *)
      within ~clock "oversized expectation" (fun () ->
          let resp =
            request ~net addr
              "POST /upload HTTP/1.1\r\n\
               Host: localhost\r\n\
               Expect: 100-continue\r\n\
               Content-Length: 40000\r\n\
               \r\n"
          in
          check "an oversized expectation is refused without 100 Continue"
            (resp.line = "HTTP/1.1 413 Payload Too Large"));
      within ~clock "honoured expectation" (fun () ->
          with_conn ~net addr (fun flow ->
              Eio.Flow.copy_string
                "POST /upload HTTP/1.1\r\n\
                 Host: localhost\r\n\
                 Expect: 100-continue\r\n\
                 Content-Length: 4\r\n\
                 \r\n"
                flow;
              let r = Eio.Buf_read.of_flow flow ~max_size:65536 in
              check "a body that fits is invited"
                (Eio.Buf_read.line r = "HTTP/1.1 100 Continue");
              check "the interim response ends with a blank line"
                (Eio.Buf_read.line r = "");
              Eio.Flow.copy_string "pong" flow;
              let resp = read_response ~head:false r in
              check "the invited body reaches the handler"
                (resp.line = "HTTP/1.1 200 OK" && resp.body = "4"))))

(* A stopped server finishes what it is serving and then returns, rather than
   truncating in-flight writes the way cancellation does. *)
let stop_tests ~clock ~mono_clock ~net =
  let returned = ref false in
  let stop, set_stop = Eio.Promise.create () in
  let bound, set_bound = Eio.Promise.create () in
  within ~clock "graceful stop" (fun () ->
      Eio.Switch.run @@ fun sw ->
      Eio.Fiber.both
        (fun () ->
          Proffer_httpz.run ~sw ~port:0
            ~on_listening:(Eio.Promise.resolve set_bound)
            ~on_error ~stop
            object
              method net = net
              method clock = clock
              method mono_clock = mono_clock
            end
            ~env compiled;
          returned := true)
        (fun () ->
          let addr = Eio.Promise.await bound in
          let resp = request ~net addr (get_req "/") in
          check "a stoppable server serves" (resp.body = index);
          Eio.Promise.resolve set_stop ()));
  check "run returns once stop resolves" !returned

let tls_wrapper_test ~clock ~mono_clock ~net =
  let calls = ref 0 in
  let counts = { Wrapped_flow.shutdowns = 0; closes = 0 } in
  let tls raw =
    incr calls;
    wrapped_flow counts raw
  in
  with_server ~net ~clock ~mono_clock ~tls (fun addr ->
      let response = request ~net addr (get_req "/") in
      check "a wrapped connection is served" (response.body = index));
  check "the TLS wrapper runs once per connection" (!calls = 1);
  check "a wrapped connection receives graceful shutdown" (counts.shutdowns = 1);
  check "a wrapped connection is closed after serving" (counts.closes = 1)

let () =
  Eio_main.run @@ fun stdenv ->
  let net = Eio.Stdenv.net stdenv in
  let clock = Eio.Stdenv.clock stdenv in
  let mono_clock = Eio.Stdenv.mono_clock stdenv in
  let on_event e = events := Proffer_httpz.globalize_event e :: !events in
  with_server ~net ~clock ~mono_clock ~on_event (fun addr ->
      tests ~clock ~net addr);
  with_server ~net ~clock ~mono_clock ~config:short_config (fun addr ->
      timeout_tests ~clock ~net addr);
  tls_wrapper_test ~clock ~mono_clock ~net;
  overrun_tests ~clock ~mono_clock ~net;
  callback_tests ~clock ~mono_clock ~net;
  handoff_event_tests ~clock ~mono_clock ~net;
  continue_tests ~clock ~mono_clock ~net;
  stop_tests ~clock ~mono_clock ~net;
  Printf.printf "test_httpz: %d checks ok\n" !checks
