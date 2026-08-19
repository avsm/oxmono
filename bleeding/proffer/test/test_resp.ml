(* Response construction: cache policy serialization, entity-tags and the
   redirect helpers. *)

open Proffer

let checks = ref 0

let check name b =
  incr checks;
  if not b then (
    prerr_endline ("FAIL: " ^ name);
    exit 1)

let header r n = Headers.find (Resp.headers r) n

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
  let r = Resp.see_other "/contact/avsm" in
  check "see_other status" (Status.code (Resp.status r) = 303);
  check "see_other location" (header r "Location" = Some "/contact/avsm");
  check "see_other has no body"
    (match Resp.body r with Body.Empty -> true | _ -> false);
  let r = Resp.redirect "/here" in
  check "redirect is 302" (Status.code (Resp.status r) = 302);
  let r = Resp.redirect ~permanent:true "/here" in
  check "permanent redirect is 301" (Status.code (Resp.status r) = 301);
  check "permanent redirect location" (header r "Location" = Some "/here")

let () =
  let r = Resp.html "<p>hi</p>" in
  check "html content type"
    (header r "Content-Type" = Some "text/html; charset=utf-8");
  check "html is 200 by default" (Status.code (Resp.status r) = 200);
  let r = Resp.text ~status:`Conflict "taken" in
  check "text status" (Status.code (Resp.status r) = 409);
  check "text content type"
    (header r "Content-Type" = Some "text/plain; charset=utf-8");
  let r = Resp.media "image/png" "\137PNG" in
  check "media content type" (header r "Content-Type" = Some "image/png");
  check "not_found is 404"
    (Status.code (Resp.status (Resp.not_found ())) = 404);
  check "bad_request is 400"
    (Status.code (Resp.status (Resp.bad_request ())) = 400)

let () =
  check "header lookup folds case"
    (header (Resp.html "hi") "content-type"
    = Some "text/html; charset=utf-8");
  check "the name goes on the wire as written"
    (List.mem_assoc "Content-Type"
       (Headers.to_list (Resp.headers (Resp.html "hi"))));
  let r = Resp.html ~etag:(`Strong "v1") "hi" in
  check "etag is readable back" (Resp.etag r = Some (`Strong "v1"));
  check "the etag is quoted on the wire" (header r "ETag" = Some "\"v1\"");
  let r = Resp.v ~last_modified:0. ~cache:Cache_control.no_store Body.Empty in
  check "last_modified is readable back" (Resp.last_modified r = Some 0.);
  check "cache is readable back"
    (match Resp.cache r with
    | Some c -> Cache_control.to_string c = "no-store"
    | None -> false)

(* A response that cannot be written is refused where it is built, not sent
   half-formed. *)
let raises f = match f () with _ -> false | exception Invalid_argument _ -> true

let () =
  check "a header value with CRLF is rejected"
    (raises (fun () -> Resp.see_other "/next\r\nSet-Cookie: x"));
  check "a lone LF is rejected"
    (raises (fun () -> Resp.v ~headers:[ ("X-A", "a\nb") ] Body.Empty));
  check "a NUL is rejected"
    (raises (fun () -> Resp.v ~headers:[ ("X-A", "a\000b") ] Body.Empty));
  check "a header name that is not a token is rejected"
    (raises (fun () -> Resp.v ~headers:[ ("X Bad", "1") ] Body.Empty));
  check "an empty header name is rejected"
    (raises (fun () -> Resp.v ~headers:[ ("", "1") ] Body.Empty));
  check "a content type with a newline is rejected"
    (raises (fun () -> Resp.media "text/plain\r\nX-A: b" "hi"));
  check "an etag holding a quote is rejected"
    (raises (fun () -> Resp.html ~etag:(`Strong "a\"b") "hi"));
  check "a weak etag holding a CR is rejected"
    (raises (fun () -> Resp.html ~etag:(`Weak "a\rb") "hi"));
  check "a last_modified that is not a number is rejected"
    (raises (fun () -> Resp.v ~last_modified:Float.nan Body.Empty));
  check "an infinite last_modified is rejected"
    (raises (fun () -> Resp.v ~last_modified:Float.infinity Body.Empty));
  check "a last_modified past year 9999 is rejected"
    (raises (fun () -> Resp.v ~last_modified:1e30 Body.Empty));
  check "an ordinary response is built"
    (not
       (raises (fun () ->
            Resp.v
              ~headers:[ ("X-Frame-Options", "DENY") ]
              ~etag:(`Strong "v1") ~last_modified:0. Body.Empty)))

let () =
  let cases : (Status.t * int * string) list =
    [
      (`Accepted, 202, "Accepted");
      (`Temporary_redirect, 307, "Temporary Redirect");
      (`Permanent_redirect, 308, "Permanent Redirect");
      (`Unauthorized, 401, "Unauthorized");
      (`Not_acceptable, 406, "Not Acceptable");
      (`Request_timeout, 408, "Request Timeout");
      (`Gone, 410, "Gone");
      (`Precondition_failed, 412, "Precondition Failed");
      (`Unprocessable_entity, 422, "Unprocessable Content");
      (`Too_many_requests, 429, "Too Many Requests");
      (`Bad_gateway, 502, "Bad Gateway");
      (`Service_unavailable, 503, "Service Unavailable");
      (`Gateway_timeout, 504, "Gateway Timeout");
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
    (Status.reason `Method_not_allowed = "Method Not Allowed");
  check "method roundtrip"
    (Method.equal (Method.of_string "PATCH") `PATCH
    && Method.to_string (`Other "PURGE") = "PURGE");
  check "unknown method" (Method.of_string "PURGE" = `Other "PURGE")

let () =
  (* Last-Modified is an IMF-fixdate, and the epoch itself pins the weekday
     and the day arithmetic. *)
  let r = Resp.v ~last_modified:0. ~content_type:"text/plain" Body.Empty in
  check "epoch renders"
    (header r "Last-Modified" = Some "Thu, 01 Jan 1970 00:00:00 GMT");
  let r =
    Resp.v ~last_modified:784111777. ~content_type:"text/plain" Body.Empty
  in
  check "rfc example renders"
    (header r "Last-Modified" = Some "Sun, 06 Nov 1994 08:49:37 GMT");
  let r =
    Resp.v ~last_modified:951782400. ~content_type:"text/plain" Body.Empty
  in
  check "leap day renders"
    (header r "Last-Modified" = Some "Tue, 29 Feb 2000 00:00:00 GMT");
  (* The ends of the range a fixed 29-byte layout can hold. *)
  let r =
    Resp.v ~last_modified:253402300799. ~content_type:"text/plain" Body.Empty
  in
  check "the last representable second renders"
    (header r "Last-Modified" = Some "Fri, 31 Dec 9999 23:59:59 GMT");
  let r =
    Resp.v ~last_modified:(-62167219200.) ~content_type:"text/plain" Body.Empty
  in
  check "the first representable second renders"
    (header r "Last-Modified" = Some "Sat, 01 Jan 0000 00:00:00 GMT")

let () =
  check "Other GET is GET" (Method.equal (`Other "GET") `GET);
  check "and the comparison is symmetric" (Method.equal `GET (`Other "GET"));
  check "a method token is case-sensitive"
    (not (Method.equal (`Other "get") `GET));
  check "an unrelated token is another method"
    (not (Method.equal (`Other "PURGE") `GET));
  check "of_string does not fold case"
    (Method.of_string "get" = `Other "get")

(* A typed argument owns its field, so [headers] naming it too is rejected
   rather than emitted twice. Sending both would leave the copy a client reads
   first disagreeing with the one a conditional request is evaluated against. *)
let () =
  check "headers may not repeat content_type"
    (raises (fun () ->
         Resp.v
           ~headers:[ ("Content-Type", "text/plain") ]
           ~content_type:"text/html" Body.Empty));
  check "headers may not repeat cache"
    (raises (fun () ->
         Resp.v
           ~headers:[ ("Cache-Control", "no-cache") ]
           ~cache:Cache_control.no_store Body.Empty));
  check "headers may not repeat etag"
    (raises (fun () ->
         Resp.v ~headers:[ ("ETag", "\"raw\"") ] ~etag:(`Strong "v1")
           Body.Empty));
  check "headers may not repeat last_modified"
    (raises (fun () ->
         Resp.v
           ~headers:[ ("Last-Modified", "Thu, 01 Jan 1970 00:00:00 GMT") ]
           ~last_modified:784111777. Body.Empty));
  check "the overlap check folds case"
    (raises (fun () ->
         Resp.v ~headers:[ ("etag", "\"raw\"") ] ~etag:(`Strong "v1")
           Body.Empty));
  check "a header the arguments do not set is kept"
    (let r =
       Resp.v ~headers:[ ("Vary", "Accept") ] ~content_type:"text/html"
         ~etag:(`Strong "v1") Body.Empty
     in
     header r "Vary" = Some "Accept"
     && header r "Content-Type" = Some "text/html"
     && header r "ETag" = Some "\"v1\"");
  check "an untyped repeat of a field is still allowed"
    (let r =
       Resp.v ~headers:[ ("ETag", "\"raw\"") ] Body.Empty
     in
     header r "ETag" = Some "\"raw\"" && Resp.etag r = None)

let () = Printf.printf "test_resp: %d checks ok\n" !checks
