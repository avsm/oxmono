(* The httpz backend over a real socket: hand-written requests in, raw
   response bytes out. *)

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

let routes =
  [
    get nil (fun _env _req respond -> Resp.html respond index);
    get (s "hello" / str /? nil) (fun who env _req respond ->
        Resp.html respond (env.greet who));
    get (s "cached" /? nil) (fun _env _req respond ->
        Resp.html respond ~etag:(`Strong "v1") "<p>cached</p>");
    get (s "stream" /? nil) (fun _env _req respond ->
        Resp.v respond ~headers:Headers.empty ~content_type:"text/plain"
          (Body.Stream
             {
               length = None;
               write =
                 (fun sink ->
                   Body.Sink.write sink "ab";
                   Body.Sink.write sink "cd");
             }));
    get (s "known" /? nil) (fun _env _req respond ->
        Resp.v respond ~headers:Headers.empty ~content_type:"text/plain"
          (Body.Stream
             {
               length = Some 4L;
               write =
                 (fun sink ->
                   Body.Sink.write sink "ab";
                   Body.Sink.write sink "cd");
             }));
    get (s "logged" /? nil) (fun _env _req respond ->
        Resp.v respond ~content_type:"text/html; charset=utf-8"
          ~headers:[ Resp.other "X-Cache" "hit" ]
          (Body.String "hi"));
    get (s "dup" /? nil) (fun _env req respond ->
        Resp.text respond
          (Option.value ~default:"none"
             (Req.header_other req "X-Dup")));
    post (s "form" /? nil) (fun _env req respond ->
        match Req.form_param req "who" with
        | Some who -> Resp.see_other respond ("/hello/" ^ who)
        | None -> Resp.bad_request respond ());
  ]

let compiled =
  Compiled.compile
    (Site.with_fallback
       (fun _env _req respond ->
         Resp.html respond ~status:St.Not_found "<p>missing</p>")
       (Site.of_routes routes))

let env = { greet = (fun who -> "<p>hello " ^ who ^ "</p>") }

(** {1 Reading a response} *)

type response = {
  line : string;
  headers : (string * string) list;
  body : string;
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
    else ignore (Eio.Buf_read.line r : string)
  in
  go ();
  Buffer.contents buf

(* [read_response ~head r] reads one response. [head] says the request was a
   HEAD, whose response carries the length of a body it does not send. *)
let read_response ~head r =
  let line = Eio.Buf_read.line r in
  let rec fields acc =
    match Eio.Buf_read.line r with
    | "" -> List.rev acc
    | l -> fields (split_header l :: acc)
  in
  let headers = fields [] in
  let body =
    if head then ""
    else
      match List.assoc_opt "transfer-encoding" headers with
      | Some "chunked" -> read_chunked r
      | _ -> (
          match List.assoc_opt "content-length" headers with
          | Some n -> Eio.Buf_read.take (int_of_string n) r
          (* No framing field at all. A body, if there is one, runs to the
             end of the connection, which the server announces by closing. *)
          | None ->
              if List.assoc_opt "connection" headers = Some "close" then
                Eio.Buf_read.take_all r
              else "")
  in
  { line; headers; body }

let field resp name = List.assoc_opt name resp.headers

(** {1 Driving the server} *)

let get_req ?(headers = "") path =
  Printf.sprintf "GET %s HTTP/1.1\r\nHost: localhost\r\n%s\r\n" path headers

let loopback = Eio.Net.Ipaddr.V4.loopback
let on_error exn = prerr_endline (Printexc.to_string exn)

(* [with_server ~net ~clock f] runs [f addr] against a server on a port the
   kernel picks, and stops it when [f] returns. [on_listening] is what makes
   the address known, so no test has to guess a port or race the listen. *)
let with_server ~net ~clock ?config ?on_event f =
  let bound, set_bound = Eio.Promise.create () in
  Eio.Fiber.first
    (fun () ->
      Eio.Switch.run @@ fun sw ->
      Proffer_httpz.run ~sw ~net ~clock ~addr:(`Tcp (loopback, 0)) ?config
        ~on_listening:(Eio.Promise.resolve set_bound)
        ?on_event ~on_error ~env compiled)
    (fun () -> f (Eio.Promise.await bound))

let with_conn ~net addr f =
  Eio.Switch.run @@ fun sw -> f (Eio.Net.connect ~sw net addr)

(* [session ~net addr f] runs [f send recv] over one connection, so a test
   that sends twice is testing keep-alive. *)
let session ~net addr f =
  with_conn ~net addr @@ fun flow ->
  let r = Eio.Buf_read.of_flow flow ~max_size:65536 in
  let send s = Eio.Flow.copy_string s flow in
  f send (fun ~head -> read_response ~head r)

let request ~net addr s =
  session ~net addr (fun send recv ->
      send s;
      recv ~head:false)

(* [within ~clock name f] is [f ()], and fails the run if it has not finished
   in five seconds. A timeout that never fires would otherwise hang the test
   rather than report itself. *)
let within ~clock name f =
  match Eio.Time.with_timeout clock 5. (fun () -> Ok (f ())) with
  | Ok v -> v
  | Error `Timeout ->
      prerr_endline ("FAIL: " ^ name ^ " (hung)");
      exit 1

(** {1 The tests} *)

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

  let resp =
    request
      "POST /form HTTP/1.1\r\n\
       Host: localhost\r\n\
       Transfer-Encoding: chunked\r\n\
       \r\n\
       0\r\n\r\n"
  in
  check "chunked request is 411" (resp.line = "HTTP/1.1 411 Length Required");
  check "411 closes" (field resp "connection" = Some "close");

  (* Answered from the header block alone: none of those 40000 bytes is
     ever read, since there is nowhere to put them. *)
  let resp =
    request
      "POST /form HTTP/1.1\r\nHost: localhost\r\nContent-Length: 40000\r\n\r\n"
  in
  check "an oversized body is 413"
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

  (* A known length is sent as one, whatever the client's version. *)
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

  (* Everything an access log needs beyond the request line: the path without
     its query, the fields the request arrived with, and what the response
     says about its type and its cache. *)
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
  check "one event per request" (List.length !events = 21);
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

let () =
  Eio_main.run @@ fun stdenv ->
  let net = Eio.Stdenv.net stdenv in
  let clock = Eio.Stdenv.clock stdenv in
  let on_event e = events := e :: !events in
  with_server ~net ~clock ~on_event (fun addr -> tests ~clock ~net addr);
  with_server ~net ~clock ~config:short_config (fun addr ->
      timeout_tests ~clock ~net addr);
  Printf.printf "test_httpz: %d checks ok\n" !checks
