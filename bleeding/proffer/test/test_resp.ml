open Proffer
module H = Httpz.Header_name
module St = Httpz.Res
module M = Httpz.Method

let checks = ref 0

let check name b =
  incr checks;
  if not b then (
    prerr_endline ("FAIL: " ^ name);
    exit 1)

let describe f = Proffer_mock.describe f
let header r n = Proffer_mock.header r n
let code r = Status.code (Proffer_mock.status r)

(* Invalid response fields become a reported 500 rather than reaching the
   wire. *)
let refused f =
  let seen = ref None in
  let r = Proffer_mock.describe ~on_error:(fun e -> seen := Some e) f in
  Status.code (Proffer_mock.status r) = 500
  && match !seen with Some (Invalid_argument _) -> true | _ -> false

let accepted f =
  let seen = ref None in
  let r = Proffer_mock.describe ~on_error:(fun e -> seen := Some e) f in
  Status.code (Proffer_mock.status r) <> 500 && !seen = None

(* The block's strings are local, so the list is walked rather than searched
   with [List.mem_assoc]. *)
let rec mem_assoc_local name (l : (string * string) list @ local) =
  match l with
  | [] -> false
  | (k, _) :: tl -> String.equal k name || mem_assoc_local name tl

let () =
  let r =
    Proffer_mock.describe
      ~on_error:(fun _ -> failwith "on_error exploded")
      (fun _respond -> failwith "handler exploded")
  in
  check "a raising generic on_error callback is contained"
    (Status.code (Proffer_mock.status r) = 500)

let () =
  let req = Req.v ~meth:M.Get ~target:"/" () in
  let reported = ref 0 in
  check "a failing fallback writer is contained"
    (match
       Backend.run
         ~on_error:(fun _ -> incr reported)
         req
         (fun _respond -> failwith "handler")
         (fun _outcome -> failwith "writer")
     with
    | () -> !reported >= 2
    | exception _ -> false);
  let reported = ref None in
  let r =
    Proffer_mock.describe
      ~on_error:(fun exn -> reported := Some exn)
      (fun respond ->
        Resp.stream respond ~status:St.Accepted
          ~headers:[ Resp.other "X-Selected" "yes" ]
          "text/plain"
          (fun sink ->
            Body.Sink.write sink "prefix";
            failwith "stream"))
  in
  check "a mock retains the selected response when its stream fails"
    (code r = 202
    && Proffer_mock.header_other r "X-Selected" = Some "yes"
    && Proffer_mock.body r = "prefix"
    && Option.is_some !reported);
  let invalid_range = ref false in
  let r =
    Proffer_mock.describe (fun respond ->
        Resp.stream respond "application/octet-stream" (fun sink ->
            match
              Body.Sink.write_sub sink (Bytes.of_string "abc") ~off:2 ~len:2
            with
            | () -> ()
            | exception Invalid_argument _ -> invalid_range := true))
  in
  check "sink write_sub checks bounds before its backend callback"
    (!invalid_range && code r = 200)

let () =
  let cc = Cache_control.to_string in
  check "no_store" (cc Cache_control.no_store = "no-store");
  check "private" (cc (Cache_control.private' ()) = "private");
  check "private with max-age"
    (cc (Cache_control.private' ~max_age:(`Hours 1) ())
    = "private, max-age=3600");
  check "public days"
    (cc (Cache_control.public ~max_age:(`Days 365) ~immutable:true ())
    = "public, max-age=31536000, immutable");
  check "public secs"
    (cc (Cache_control.public ~max_age:(`Secs 30) ()) = "public, max-age=30");
  check "public everything"
    (cc
       (Cache_control.public ~max_age:(`Secs 60) ~s_maxage:120
          ~stale_while_revalidate:30 ~must_revalidate:true ~immutable:true ())
    = "public, max-age=60, s-maxage=120, stale-while-revalidate=30, \
       must-revalidate, immutable");
  check "a negative private duration is rejected"
    (match Cache_control.private' ~max_age:(`Secs (-1)) () with
    | _ -> false
    | exception Invalid_argument _ -> true);
  check "a negative shared duration is rejected"
    (match Cache_control.public ~max_age:(`Secs 1) ~s_maxage:(-1) () with
    | _ -> false
    | exception Invalid_argument _ -> true)

let () =
  check "strong etag" (Etag.to_string (Etag.strong "abc") = "\"abc\"");
  check "weak etag" (Etag.to_string (Etag.weak "abc") = "W/\"abc\"");
  check "weak_equal ignores strength"
    (Etag.weak_equal (Etag.weak "abc") (Etag.strong "abc"));
  check "weak_equal compares the value"
    (not (Etag.weak_equal (Etag.weak "abc") (Etag.weak "abd")));
  (* Entity-tags cache their rendered wire value. *)
  let t = Etag.strong "abc" in
  check "the wire form is rendered once, not per call"
    (Etag.to_string t == Etag.to_string t)

(* [Etag.t] is abstract, so it carries no kind unless its signature declares
   one, and without a kind it cannot cross into a portable handler. A static
   asset builds its tag at the top level and serves it from one, so this
   closure has that shape and fails to compile if the kind is dropped. *)
let module_level_etag = Etag.strong "static"

let () =
  let through_handler : (unit -> string) @ portable =
   fun () -> Etag.to_string module_level_etag
  in
  check "an entity-tag crosses into a portable handler"
    (through_handler () = "\"static\"")

let () =
  let r = describe (fun respond -> Resp.see_other respond "/contact/avsm") in
  check "see_other status" (code r = 303);
  check "see_other location"
    (header r H.Location = Some "/contact/avsm");
  check "see_other has no body" (String.equal (Proffer_mock.body r) "");
  let r = describe (fun respond -> Resp.redirect respond "/here") in
  check "redirect is 302" (code r = 302);
  let r =
    describe (fun respond -> Resp.redirect respond ~permanent:true "/here")
  in
  check "permanent redirect is 301" (code r = 301);
  check "permanent redirect location"
    (header r H.Location = Some "/here")

let () =
  let r = describe (fun respond -> Resp.html respond "<p>hi</p>") in
  check "html content type"
    (header r H.Content_type = Some "text/html; charset=utf-8");
  check "html is 200 by default" (code r = 200);
  check "html body reaches the wire"
    (String.equal (Proffer_mock.body r) "<p>hi</p>");
  let r =
    describe (fun respond -> Resp.text respond ~status:St.Conflict "taken")
  in
  check "text status" (code r = 409);
  check "text content type"
    (header r H.Content_type = Some "text/plain; charset=utf-8");
  let r = describe (fun respond -> Resp.media respond "image/png" "\137PNG") in
  check "media content type"
    (header r H.Content_type = Some "image/png");
  check "not_found is 404"
    (code (describe (fun respond -> Resp.not_found respond ())) = 404);
  check "bad_request is 400"
    (code (describe (fun respond -> Resp.bad_request respond ())) = 400)

let () =
  let contentless status expected_length =
    let generated = ref false in
    let r =
      describe (fun respond ->
          Resp.v respond ~status ~headers:Headers.empty ~content_type:Null
            (Body.Delayed
               {
                 length = Some 9L;
                 gen =
                   (fun () ->
                     generated := true;
                     "forbidden");
               }))
    in
    check "a contentless status suppresses its body"
      (String.equal (Proffer_mock.body r) "");
    check "a contentless status does not generate its body" (not !generated);
    check "a contentless status has the right declared length"
      (Proffer_mock.content_length r = expected_length)
  in
  contentless St.No_content None;
  contentless St.Reset_content (Some 0L);
  contentless St.Not_modified (Some 9L);
  let r =
    describe (fun respond ->
        Resp.v respond ~status:St.Not_modified
          ~headers:
            [
              Resp.h H.Vary "Accept";
              Resp.h H.Content_location "/cached";
              Resp.other "X-Extra" "drop" ]
          ~content_type:(This "text/plain") (Body.String "metadata"))
  in
  check "an explicit 304 keeps revalidation fields"
    (header r H.Vary = Some "Accept"
    && header r H.Content_location = Some "/cached");
  check "an explicit 304 drops representation and extension fields"
    (header r H.Content_type = None
    && Proffer_mock.header_other r "X-Extra" = None)

let () =
  let hi = describe (fun respond -> Resp.html respond "hi") in
  check "a known name renders its canonical spelling"
    (mem_assoc_local "Content-Type" (Headers.to_list (Proffer_mock.headers hi)));
  let r =
    describe (fun respond -> Resp.html respond ~etag:(Etag.strong "v1") "hi")
  in
  check "the etag is quoted on the wire"
    (header r H.Etag = Some "\"v1\"");
  let r =
    describe (fun respond ->
        Resp.v respond ~content_type:Null ~headers:Headers.empty
          ~last_modified:0.
          ~cache:Cache_control.no_store Body.Empty)
  in
  check "last_modified renders its field"
    (header r H.Last_modified
    = Some "Thu, 01 Jan 1970 00:00:00 GMT");
  check "cache renders its field"
    (header r H.Cache_control = Some "no-store")

let () =
  check "a header value with CRLF is rejected"
    (refused (fun respond ->
         Resp.see_other respond "/next\r\nSet-Cookie: x"));
  check "a lone LF is rejected"
    (refused (fun respond ->
         Resp.v respond ~content_type:Null
           ~headers:[ Resp.other "X-A" "a\nb" ]
           Body.Empty));
  check "a NUL is rejected"
    (refused (fun respond ->
         Resp.v respond ~content_type:Null
           ~headers:[ Resp.other "X-A" "a\000b" ]
           Body.Empty));
  check "another control byte is rejected"
    (refused (fun respond ->
         Resp.v respond ~content_type:Null
           ~headers:[ Resp.other "X-A" "a\001b" ]
           Body.Empty));
  check "DEL is rejected"
    (refused (fun respond ->
         Resp.v respond ~content_type:Null
           ~headers:[ Resp.other "X-A" "a\127b" ]
           Body.Empty));
  check "a header name that is not a token is rejected"
    (refused (fun respond ->
         Resp.v respond ~content_type:Null
           ~headers:[ Resp.other "X Bad" "1" ]
           Body.Empty));
  check "an empty header name is rejected"
    (refused (fun respond ->
         Resp.v respond ~content_type:Null
           ~headers:[ Resp.other "" "1" ] Body.Empty));
  List.iter
    (fun name ->
      check
        ("an application " ^ name ^ " is rejected")
        (refused (fun respond ->
             Resp.text respond ~headers:[ Resp.other name "invalid" ] "hello")))
    [ "Content-Length"; "Transfer-Encoding"; "Connection"; "Trailer" ];
  check "Upgrade is rejected on an ordinary response"
    (refused (fun respond ->
         Resp.text respond ~headers:[ Resp.h H.Upgrade "h2c" ] "hello"));
  check "Upgrade is accepted where 426 requires it"
    (accepted (fun respond ->
         Resp.v respond ~status:St.Upgrade_required
           ~headers:[ Resp.h H.Upgrade "h2c" ] ~content_type:Null Body.Empty));
  List.iter
    (fun value ->
      check "a malformed Upgrade offer on 426 is rejected"
        (refused (fun respond ->
             Resp.v respond ~status:St.Upgrade_required
               ~headers:[ Resp.h H.Upgrade value ]
               ~content_type:Null Body.Empty)))
    [ ""; ",h2c"; "h2c,"; "h2c,,websocket"; "h2c/v/2"; "bad offer" ];
  check "100 cannot be returned as a final response"
    (refused (fun respond ->
         Resp.v respond ~status:St.Continue ~headers:Headers.empty
           ~content_type:Null Body.Empty));
  check "a bare 101 cannot be returned as a final response"
    (refused (fun respond ->
         Resp.v respond ~status:St.Switching_protocols ~headers:Headers.empty
           ~content_type:Null Body.Empty));
  check "a content type with a newline is rejected"
    (refused (fun respond ->
         Resp.media respond "text/plain\r\nX-A: b" "hi"));
  check "an etag holding a quote is rejected"
    (refused (fun respond ->
         Resp.html respond ~etag:(Etag.strong "a\"b") "hi"));
  check "a weak etag holding a CR is rejected"
    (refused (fun respond -> Resp.html respond ~etag:(Etag.weak "a\rb") "hi"));
  check "an etag holding a space is rejected"
    (refused (fun respond -> Resp.html respond ~etag:(Etag.weak "a b") "hi"));
  check "an etag holding DEL is rejected"
    (refused (fun respond ->
         Resp.html respond ~etag:(Etag.weak "a\127b") "hi"));
  check "a last_modified that is not a number is rejected"
    (refused (fun respond ->
         Resp.v respond ~content_type:Null ~headers:Headers.empty
           ~last_modified:Float.nan
           Body.Empty));
  check "an infinite last_modified is rejected"
    (refused (fun respond ->
         Resp.v respond ~content_type:Null ~headers:Headers.empty
           ~last_modified:Float.infinity
           Body.Empty));
  check "a last_modified past year 9999 is rejected"
    (refused (fun respond ->
         Resp.v respond ~content_type:Null ~headers:Headers.empty
           ~last_modified:1e30 Body.Empty));
  check "a negative delayed length is rejected"
    (refused (fun respond ->
         Resp.v respond ~content_type:Null ~headers:Headers.empty
           (Body.Delayed { length = Some (-1L); gen = (fun () -> "") })));
  check "a negative stream length is rejected"
    (refused (fun respond ->
         Resp.v respond ~content_type:Null ~headers:Headers.empty
           (Body.Stream
              { length = Some (-1L); write = (fun _ -> ());
                trailers = Headers.empty })));
  check "an ordinary response is built"
    (accepted (fun respond ->
         Resp.v respond ~content_type:Null
           ~headers:[ Resp.other "X-Frame-Options" "DENY" ]
           ~etag:(Etag.strong "v1") ~last_modified:0. Body.Empty))

let () =
  let r =
    describe (fun respond ->
        Resp.stream respond ~length:5L
          ~trailers:[ Resp.other "X-Checksum" "ok" ]
          "text/plain"
          (fun sink -> Body.Sink.write sink "hello"))
  in
  check "a stream declares its trailer names"
    (Proffer_mock.header r H.Trailer = Some "X-Checksum");
  check "the mock still collects a trailer-bearing stream"
    (Proffer_mock.body r = "hello");
  let dropped ?(meth = M.Get) status =
    Proffer_mock.describe ~meth (fun respond ->
        Resp.stream respond ~status
          ~trailers:[ Resp.other "X-Checksum" "ok" ]
          "text/plain"
          (fun sink -> Body.Sink.write sink "hello"))
  in
  List.iter
    (fun (name, r) ->
      check (name ^ " drops Trailer with its body") (header r H.Trailer = None))
    [
      ("HEAD", dropped ~meth:M.Head St.Success);
      ("204", dropped St.No_content);
      ("205", dropped St.Reset_content);
      ("304", dropped St.Not_modified);
    ];
  check "a framing trailer is rejected"
    (refused (fun respond ->
         Resp.stream respond
           ~trailers:[ Resp.other "Content-Length" "5" ]
           "text/plain"
           (fun sink -> Body.Sink.write sink "hello")));
  check "a Set-Cookie trailer is rejected"
    (refused (fun respond ->
         Resp.stream respond
           ~trailers:[ Resp.other "Set-Cookie" "a=1" ]
           "text/plain"
           (fun sink -> Body.Sink.write sink "hello")));
  check "an application-supplied Trailer field cannot conflict"
    (refused (fun respond ->
         Resp.stream respond
           ~headers:[ Resp.other "Trailer" "X-Other" ]
           ~trailers:[ Resp.other "X-Checksum" "ok" ]
           "text/plain"
           (fun _ -> ())));
  let upgraded =
    Proffer_mock.describe ~connection_upgrade:true
      ~headers:[ ("Upgrade", "proffer-echo") ]
      (fun respond ->
        Resp.upgrade respond ~protocol:"proffer-echo" (fun _ -> ()))
  in
  check "upgrade builds status 101" (code upgraded = 101);
  check "upgrade selects its protocol"
    (header upgraded H.Upgrade = Some "proffer-echo");
  let versioned =
    Proffer_mock.describe ~connection_upgrade:true
      ~headers:[ ("Upgrade", "other, proffer-echo/1") ]
      (fun respond ->
        Resp.upgrade respond ~protocol:"proffer-echo/1" (fun _ -> ()))
  in
  check "an upgrade protocol may carry a version"
    (header versioned H.Upgrade = Some "proffer-echo/1");
  let folded_name =
    Proffer_mock.describe ~connection_upgrade:true
      ~headers:[ ("Upgrade", "PROFFER-ECHO/1") ]
      (fun respond ->
        Resp.upgrade respond ~protocol:"proffer-echo/1" (fun _ -> ()))
  in
  check "upgrade protocol names compare without case" (code folded_name = 101);
  let rejected_upgrade ?version ?(connection_upgrade = false) ?(headers = []) ()
      =
    let called = ref false in
    let r =
      Proffer_mock.describe ?version ~connection_upgrade ~headers
        (fun respond ->
          Resp.upgrade respond ~protocol:"proffer-echo" (fun _ ->
              called := true))
    in
    code r = 500 && not !called
  in
  check "upgrade requires Connection: upgrade"
    (rejected_upgrade ~headers:[ ("Upgrade", "proffer-echo") ] ());
  check "upgrade requires an offered matching protocol"
    (rejected_upgrade ~connection_upgrade:true
       ~headers:[ ("Upgrade", "different") ]
       ());
  check "a matching member cannot hide malformed Upgrade syntax"
    (rejected_upgrade ~connection_upgrade:true
       ~headers:[ ("Upgrade", "proffer-echo, bad/value/more") ]
       ());
  check "upgrade protocol versions compare with case"
    (rejected_upgrade ~connection_upgrade:true
       ~headers:[ ("Upgrade", "proffer-echo/V1") ]
       ());
  check "upgrade requires HTTP/1.1"
    (rejected_upgrade ~version:Httpz.Version.Http_1_0 ~connection_upgrade:true
       ~headers:[ ("Upgrade", "proffer-echo") ]
       ());
  List.iter
    (fun protocol ->
      check "an invalid upgrade protocol is rejected"
        (refused (fun respond -> Resp.upgrade respond ~protocol (fun _ -> ()))))
    [ "bad protocol"; "/bad"; "bad/"; "bad/v/2" ];
  check "an upgrade protocol cannot conflict with an Upgrade field"
    (refused (fun respond ->
         Resp.upgrade respond
           ~headers:[ Resp.h H.Upgrade "other" ]
           ~protocol:"proffer-echo"
           (fun _ -> ())));
  let tunnel =
    Proffer_mock.describe ~meth:M.Connect ~target:"example.test:443"
      (fun respond -> Resp.tunnel respond (fun _ -> ()))
  in
  check "CONNECT can build a tunnel response" (code tunnel = 200);
  check "a tunnel status must be successful"
    (refused (fun respond ->
         Resp.tunnel respond ~status:St.Bad_gateway (fun _ -> ())))

let () =
  let seen = ref None in
  let r =
    Proffer_mock.describe
      ~on_error:(fun exn -> seen := Some exn)
      (fun respond ->
        Resp.v respond ~content_type:Null ~headers:Headers.empty
          (Body.Delayed { length = Some 1L; gen = (fun () -> "wrong") }))
  in
  check "a delayed length mismatch is a 500" (code r = 500);
  check "a delayed length mismatch is reported"
    (match !seen with Some (Invalid_argument _) -> true | _ -> false)

let () =
  let r =
    describe (fun respond ->
        Resp.stream respond ~length:3L "text/plain" (fun sink ->
            Body.Sink.write sink "four"))
  in
  check "the mock preserves a stream's declared length"
    (Proffer_mock.content_length r = Some 3L);
  check "the mock leaves a length mismatch observable"
    (String.length (Proffer_mock.body r) = 4)

let () =
  let seen = ref None in
  let r =
    Proffer_mock.describe
      ~on_error:(fun e -> seen := Some e)
      (fun _respond -> ())
  in
  check "a handler that never responds is a 500"
    (Status.code (Proffer_mock.status r) = 500);
  check "and is reported"
    (match !seen with Some (Invalid_argument _) -> true | _ -> false);
  let seen = ref None in
  let r =
    Proffer_mock.describe
      ~on_error:(fun e -> seen := Some e)
      (fun respond ->
        Resp.text respond "first";
        Resp.text respond "second")
  in
  check "a second response is dropped"
    (String.equal (Proffer_mock.body r) "first");
  check "and the second is reported"
    (match !seen with Some (Invalid_argument _) -> true | _ -> false)

let () =
  let cases : (Status.t * int * string) list =
    [
      (St.Accepted, 202, "Accepted");
      (St.Temporary_redirect, 307, "Temporary Redirect");
      (St.Permanent_redirect, 308, "Permanent Redirect");
      (St.Unauthorized, 401, "Unauthorized");
      (St.Not_acceptable, 406, "Not Acceptable");
      (St.Proxy_authentication_required, 407, "Proxy Authentication Required");
      (St.Request_timeout, 408, "Request Timeout");
      (St.Gone, 410, "Gone");
      (St.Precondition_failed, 412, "Precondition Failed");
      (* httpz spells 422 with RFC 4918's phrase, not RFC 9110's
         "Unprocessable Content". The phrase is advisory and no client parses
         it, but it is what goes on the wire now. *)
      (St.Unprocessable_entity, 422, "Unprocessable Entity");
      (St.Too_many_requests, 429, "Too Many Requests");
      (St.Request_header_fields_too_large, 431, "Request Header Fields Too Large");
      (St.Bad_gateway, 502, "Bad Gateway");
      (St.Service_unavailable, 503, "Service Unavailable");
      (St.Gateway_timeout, 504, "Gateway Timeout");
    ]
  in
  List.iter
    (fun (s, c, r) ->
      check
        (Printf.sprintf "status %d" c)
        (Status.code s = c && String.equal (Status.reason s) r))
    cases

let () =
  check "status reason"
    (String.equal
       (Status.reason St.Method_not_allowed)
       "Method Not Allowed");
  check "a status names its code" (Status.code St.Not_found = 404);
  check "and reads back from it"
    (Status.of_code 404 = Some St.Not_found);
  check "a code httpz does not name has no status"
    (Status.of_code 799 = None);
  check "method spelling" (Method.to_string M.Patch = "PATCH");
  check "methods compare by constructor"
    (Method.equal M.Get M.Get
    && not (Method.equal M.Get M.Post))

let () =
  let at t =
    header
      (describe (fun respond ->
           Resp.v respond ~headers:Headers.empty ~last_modified:t
             ~content_type:(This "text/plain") Body.Empty))
      H.Last_modified
  in
  check "epoch renders" (at 0. = Some "Thu, 01 Jan 1970 00:00:00 GMT");
  check "rfc example renders"
    (at 784111777. = Some "Sun, 06 Nov 1994 08:49:37 GMT");
  check "leap day renders"
    (at 951782400. = Some "Tue, 29 Feb 2000 00:00:00 GMT");
  check "the last representable second renders"
    (at 253402300799. = Some "Fri, 31 Dec 9999 23:59:59 GMT");
  check "the first representable second renders"
    (at (-62135596800.) = Some "Mon, 01 Jan 0001 00:00:00 GMT")

let () =
  check "a method is its constructor" (Method.equal M.Get M.Get);
  check "and differs from another" (not (Method.equal M.Get M.Post))

(* Typed response arguments may not duplicate their corresponding field. *)
let () =
  check "headers may not repeat content_type"
    (refused (fun respond ->
         Resp.v respond
           ~headers:[ Resp.h H.Content_type "text/plain" ]
           ~content_type:(This "text/html") Body.Empty));
  check "headers may not repeat cache"
    (refused (fun respond ->
         Resp.v respond ~content_type:Null
           ~headers:[ Resp.h H.Cache_control "no-cache" ]
           ~cache:Cache_control.no_store Body.Empty));
  check "headers may not repeat etag"
    (refused (fun respond ->
         Resp.v respond ~content_type:Null
           ~headers:[ Resp.h H.Etag "\"raw\"" ]
           ~etag:(Etag.strong "v1") Body.Empty));
  check "headers may not repeat last_modified"
    (refused (fun respond ->
         Resp.v respond ~content_type:Null
           ~headers:
             [ Resp.h H.Last_modified "Thu, 01 Jan 1970 00:00:00 GMT" ]
           ~last_modified:784111777. Body.Empty));
  check "a header the arguments do not set is kept"
    (let r =
       describe (fun respond ->
           Resp.v respond
             ~headers:[ Resp.h H.Vary "Accept" ]
             ~content_type:(This "text/html") ~etag:(Etag.strong "v1")
             Body.Empty)
     in
     header r H.Vary = Some "Accept"
     && header r H.Content_type = Some "text/html"
     && header r H.Etag = Some "\"v1\"")

(* A 206 that does not say which bytes it carries is stored by a cache as the
   whole representation, so the field is required and its syntax checked. *)
let partial ?ct headers =
  fun respond ->
   Resp.v respond ~status:St.Partial_content ~headers
     ~content_type:(match ct with Some ct -> This ct | None -> This "text/plain")
     (Body.String "bytes")

let () =
  check "206 without a Content-Range is refused"
    (refused (partial Headers.empty));
  List.iter
    (fun value ->
      check
        ("206 accepts Content-Range " ^ value)
        (accepted (partial [ Resp.h H.Content_range value ])))
    [
      "bytes 0-499/1234";
      "bytes 0-499/*";
      "bytes 0-0/1";
      "bytes 500-1233/1234";
      "BYTES 000-000/0001";
      "\tbytes 00000000000000000000-99999999999999999999/* \t";
    ];
  List.iter
    (fun value ->
      check
        ("206 refuses Content-Range " ^ value)
        (refused (partial [ Resp.h H.Content_range value ])))
    [
      "";
      "bytes";
      "bytes ";
      "bytes 0-499";
      "bytes 499-0/1234";
      "bytes 0-1234/1234";
      "bytes -499/1234";
      "bytes 0-/1234";
      "bytes 0-499/0x10";
      "bytes 0-499/";
      "bytes */1234";
      "bytes */*";
      "items 0-499/1234";
      "bytes 0-499/1234, bytes 0-1/2";
    ];
  check "206 refuses repeated Content-Range"
    (refused
       (partial
          [
            Resp.h H.Content_range "bytes 0-0/2";
            Resp.h H.Content_range "bytes 1-1/2";
          ]));
  (* A multipart response repeats Content-Range once per part instead. *)
  check "206 accepts bounded multipart/byteranges without the field"
    (accepted (partial ~ct:"multipart/byteranges; boundary=x" Headers.empty));
  List.iter
    (fun ct ->
      check
        ("206 refuses malformed multipart type " ^ ct)
        (refused (partial ~ct Headers.empty)))
    [
      "multipart/byteranges";
      "multipart/byteranges; boundary=";
      "multipart/byteranges; boundary=x:junk";
      "multipart/byteranges; boundary=\"\"";
    ];
  check "206 multipart/byteranges refuses a top-level Content-Range"
    (refused
       (partial ~ct:"multipart/byteranges; BOUNDARY=x"
          [ Resp.h H.Content_range "bytes 0-4/5" ]));
  check "a 200 needs no Content-Range"
    (accepted (fun respond ->
         Resp.v respond ~headers:Headers.empty ~content_type:(This "text/plain")
           (Body.String "all")));
  check "401 needs WWW-Authenticate"
    (refused (fun respond ->
         Resp.text respond ~status:St.Unauthorized "authenticate"));
  check "401 accepts WWW-Authenticate"
    (accepted (fun respond ->
         Resp.text respond ~status:St.Unauthorized
           ~headers:[ Resp.h H.Www_authenticate {|Basic realm="site"|} ]
           "authenticate"));
  check "426 needs Upgrade"
    (refused (fun respond ->
         Resp.text respond ~status:St.Upgrade_required "upgrade"));
  check "426 accepts Upgrade"
    (accepted (fun respond ->
         Resp.text respond ~status:St.Upgrade_required
           ~headers:[ Resp.h H.Upgrade "HTTP/2.0" ] "upgrade"))

let empty_status status headers =
  fun respond ->
   Resp.v respond ~status ~headers ~content_type:Null Body.Empty

let () =
  check "405 without Allow is refused"
    (refused (empty_status St.Method_not_allowed Headers.empty));
  check "405 with Allow is accepted"
    (accepted
       (empty_status St.Method_not_allowed [ Resp.h H.Allow "GET, HEAD" ]));
  check "407 without Proxy-Authenticate is refused"
    (refused (empty_status St.Proxy_authentication_required Headers.empty));
  check "407 with Proxy-Authenticate is accepted"
    (accepted
       (empty_status St.Proxy_authentication_required
          [ Resp.h H.Proxy_authenticate "Basic realm=\"proxy\"" ]));
  check "416 without Content-Range is refused"
    (refused (empty_status St.Range_not_satisfiable Headers.empty));
  List.iter
    (fun value ->
      check
        ("416 accepts Content-Range " ^ value)
        (accepted
           (empty_status St.Range_not_satisfiable
              [ Resp.h H.Content_range value ])))
    [ "bytes */0"; "bytes */1234"; "BYTES */0001234"; "\tbytes */12 \t" ];
  List.iter
    (fun value ->
      check
        ("416 refuses Content-Range " ^ value)
        (refused
           (empty_status St.Range_not_satisfiable
              [ Resp.h H.Content_range value ])))
    [ "bytes */*"; "bytes 0-1/2"; "bytes */"; "items */12"; "bytes */01x" ];
  check "416 refuses repeated Content-Range"
    (refused
       (empty_status St.Range_not_satisfiable
          [
            Resp.h H.Content_range "bytes */12";
            Resp.h H.Content_range "bytes */12";
          ]))

let () = Printf.printf "test_resp: %d checks ok\n" !checks
