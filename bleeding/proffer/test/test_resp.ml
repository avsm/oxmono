(* Response construction: cache policy serialization, entity-tags and the
   redirect helpers. *)

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

(* A response is no longer a value a test can hold, so each case runs a
   responder through [Proffer_mock.describe]. That goes down the same
   [Proffer.Backend] path a socket backend does, so the block read back is the
   rendered one a client would see, which is what these checks are about. *)
let describe f = Proffer_mock.describe f
let header r n = Proffer_mock.header r n
let code r = Status.code (Proffer_mock.status r)

(* A response that cannot be written is refused where it is built. The
   responder runs under [Backend]'s guard, so the refusal reaches a site as a
   500 and an [Invalid_argument] reported to [on_error]. *)
let refused f =
  let seen = ref None in
  let r = Proffer_mock.describe ~on_error:(fun e -> seen := Some e) f in
  Status.code (Proffer_mock.status r) = 500
  && match !seen with Some (Invalid_argument _) -> true | _ -> false

let accepted f =
  let seen = ref None in
  let r = Proffer_mock.describe ~on_error:(fun e -> seen := Some e) f in
  Status.code (Proffer_mock.status r) <> 500 && !seen = None

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
       must-revalidate, immutable")

let () =
  check "strong etag" (Etag.to_string (`Strong "abc") = "\"abc\"");
  check "weak etag" (Etag.to_string (`Weak "abc") = "W/\"abc\"");
  check "weak_equal ignores strength"
    (Etag.weak_equal (`Weak "abc") (`Strong "abc"));
  check "weak_equal compares the value"
    (not (Etag.weak_equal (`Weak "abc") (`Weak "abd")))

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
  let hi = describe (fun respond -> Resp.html respond "hi") in
  check "a known name renders its canonical spelling"
    (List.mem_assoc "Content-Type" (Headers.to_list (Proffer_mock.headers hi)));
  let r =
    describe (fun respond -> Resp.html respond ~etag:(`Strong "v1") "hi")
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
  check "a header name that is not a token is rejected"
    (refused (fun respond ->
         Resp.v respond ~content_type:Null
           ~headers:[ Resp.other "X Bad" "1" ]
           Body.Empty));
  check "an empty header name is rejected"
    (refused (fun respond ->
         Resp.v respond ~content_type:Null
           ~headers:[ Resp.other "" "1" ] Body.Empty));
  check "a content type with a newline is rejected"
    (refused (fun respond ->
         Resp.media respond "text/plain\r\nX-A: b" "hi"));
  check "an etag holding a quote is rejected"
    (refused (fun respond ->
         Resp.html respond ~etag:(`Strong "a\"b") "hi"));
  check "a weak etag holding a CR is rejected"
    (refused (fun respond -> Resp.html respond ~etag:(`Weak "a\rb") "hi"));
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
  check "an ordinary response is built"
    (accepted (fun respond ->
         Resp.v respond ~content_type:Null
           ~headers:[ Resp.other "X-Frame-Options" "DENY" ]
           ~etag:(`Strong "v1") ~last_modified:0. Body.Empty))

(* Two mistakes a handler can make that only the responder can catch. *)
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
      (St.Request_timeout, 408, "Request Timeout");
      (St.Gone, 410, "Gone");
      (St.Precondition_failed, 412, "Precondition Failed");
      (* httpz spells 422 with RFC 4918's phrase, not RFC 9110's
         "Unprocessable Content". The phrase is advisory and no client parses
         it, but it is what goes on the wire now. *)
      (St.Unprocessable_entity, 422, "Unprocessable Entity");
      (St.Too_many_requests, 429, "Too Many Requests");
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
  (* Last-Modified is an IMF-fixdate, and the epoch itself pins the weekday
     and the day arithmetic. *)
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
  (* The ends of the range a fixed 29-byte layout can hold. *)
  check "the last representable second renders"
    (at 253402300799. = Some "Fri, 31 Dec 9999 23:59:59 GMT");
  (* httpz clamps below year 1, so that is where [Date.representable] now
     stops. The old lower bound was the proleptic 0000-01-01. *)
  check "the first representable second renders"
    (at (-62135596800.) = Some "Mon, 01 Jan 0001 00:00:00 GMT")

let () =
  (* [Method.t] is closed: httpz's parser rejects a token it does not name, so
     a method that reaches a handler is always one of the seventeen and there
     is no wire spelling left to compare. *)
  check "a method is its constructor"
    (Method.equal M.Get M.Get);
  check "and differs from another"
    (not (Method.equal M.Get M.Post))

(* A typed argument owns its field, so [headers] naming it too is rejected
   rather than emitted twice. Sending both would leave the copy a client reads
   first disagreeing with the one a conditional request is evaluated against. *)
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
           ~etag:(`Strong "v1") Body.Empty));
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
             ~content_type:(This "text/html") ~etag:(`Strong "v1") Body.Empty)
     in
     header r H.Vary = Some "Accept"
     && header r H.Content_type = Some "text/html"
     && header r H.Etag = Some "\"v1\"")

let () = Printf.printf "test_resp: %d checks ok\n" !checks
