(* Response construction: cache policy serialization, entity-tags and the
   redirect helpers. *)

open Proffer

let checks = ref 0

let check name b =
  incr checks;
  if not b then (
    prerr_endline ("FAIL: " ^ name);
    exit 1)

let header r n = List.assoc_opt n (Resp.headers r)

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
    (header r "Last-Modified" = Some "Tue, 29 Feb 2000 00:00:00 GMT")

let () = Printf.printf "test_resp: %d checks ok\n" !checks
