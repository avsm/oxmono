(* The httpz backend over a real socket: hand-written requests in, raw
   response bytes out. *)

open Proffer
open Proffer.Route

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
    get nil (fun _env _req -> Resp.html index);
    get (s "hello" / str /? nil) (fun who env _req -> Resp.html (env.greet who));
    get (s "cached" /? nil) (fun _env _req ->
        Resp.html ~etag:(`Strong "v1") "<p>cached</p>");
    get (s "stream" /? nil) (fun _env _req ->
        Resp.v ~content_type:"text/plain"
          (Body.Stream
             {
               length = None;
               write =
                 (fun sink ->
                   Body.Sink.write sink "ab";
                   Body.Sink.write sink "cd");
             }));
    post (s "form" /? nil) (fun _env req ->
        match Req.form_param req "who" with
        | Some who -> Resp.see_other ("/hello/" ^ who)
        | None -> Resp.bad_request ());
  ]

let compiled =
  Compiled.compile
    (Site.with_fallback
       (fun _env _req -> Resp.html ~status:`Not_found "<p>missing</p>")
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
          | None -> "")
  in
  { line; headers; body }

let field resp name = List.assoc_opt name resp.headers

(** {1 Driving the server} *)

let get_req ?(headers = "") path =
  Printf.sprintf "GET %s HTTP/1.1\r\nHost: localhost\r\n%s\r\n" path headers

let loopback = Eio.Net.Ipaddr.V4.loopback

(* A port nothing is listening on. The probe socket is closed with its
   switch, so the window between that and the server's own bind is one
   fibre switch wide, and [reuse_addr] covers the rest. *)
let free_port net =
  Eio.Switch.run @@ fun sw ->
  let sock =
    Eio.Net.listen net ~sw ~backlog:1 ~reuse_addr:true (`Tcp (loopback, 0))
  in
  match Eio.Net.listening_addr sock with
  | `Tcp (_, port) -> port
  | `Unix _ -> assert false

let connect ~clock ~net ~sw addr =
  let rec go n =
    match Eio.Net.connect ~sw net addr with
    | flow -> flow
    | exception Eio.Io _ when n > 0 ->
        Eio.Time.sleep clock 0.01;
        go (n - 1)
  in
  go 100

(* [session ~clock ~net addr f] runs [f send recv] over one connection, so a
   test that sends twice is testing keep-alive. *)
let session ~clock ~net addr f =
  Eio.Switch.run @@ fun sw ->
  let flow = connect ~clock ~net ~sw addr in
  let r = Eio.Buf_read.of_flow flow ~max_size:65536 in
  let send s = Eio.Flow.copy_string s flow in
  f send (fun ~head -> read_response ~head r)

let request ~clock ~net addr s =
  session ~clock ~net addr (fun send recv ->
      send s;
      recv ~head:false)

(** {1 The tests} *)

let events = ref []

let tests ~clock ~net addr =
  let request = request ~clock ~net addr in
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
  session ~clock ~net addr (fun send recv ->
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

  (* Two requests down one connection, the second sent only after the first
     is read, so a mistake in framing shows up as a hang or a bad line. *)
  session ~clock ~net addr (fun send recv ->
      send (get_req "/");
      let first = recv ~head:false in
      send (get_req "/hello/again");
      let second = recv ~head:false in
      check "keep-alive first" (first.body = index);
      check "keep-alive second" (second.body = "<p>hello again</p>"));

  (* The last event is recorded after its response reaches the client, so
     give the serving fibre a moment to run before counting. *)
  Eio.Time.sleep clock 0.05;
  check "one event per request" (List.length !events = 15);
  match !events with
  | last :: _ ->
      check "event method" (Method.equal last.Proffer_httpz.Log.meth `GET);
      check "event target" (last.Proffer_httpz.Log.target = "/hello/again");
      check "event status" (Status.code last.Proffer_httpz.Log.status = 200);
      check "event body size" (last.Proffer_httpz.Log.body_size = 18)
  | [] -> check "an event was recorded" false

let () =
  Eio_main.run @@ fun stdenv ->
  let net = Eio.Stdenv.net stdenv in
  let clock = Eio.Stdenv.clock stdenv in
  let addr = `Tcp (loopback, free_port net) in
  let on_error exn = prerr_endline (Printexc.to_string exn) in
  let on_event e = events := e :: !events in
  Eio.Fiber.first
    (fun () ->
      Eio.Switch.run @@ fun sw ->
      Proffer_httpz.run ~sw ~net ~addr ~on_event ~on_error ~env compiled)
    (fun () -> tests ~clock ~net addr);
  Printf.printf "test_httpz: %d checks ok\n" !checks
