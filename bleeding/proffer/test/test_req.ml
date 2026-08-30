(* Request parsing: percent-decoding of segments, query and form, and header
   lookup. *)

open Proffer
module H = Httpz.Header_name
module M = Httpz.Method

let checks = ref 0

let check name b =
  incr checks;
  if not b then (
    prerr_endline ("FAIL: " ^ name);
    exit 1)

(* [Req.v] takes a block, so the association-list spelling a test finds
   readable is converted here rather than in the library. *)
(* A request is local, so a helper that makes one returns it into the
   caller's region. *)
let req ?headers ?body target = exclave_
  let headers = Option.map Headers.of_list headers in
  Req.v ~meth:M.Get ~target ?headers ?body ()

let () =
  let r = req "/caf%C3%A9/a%2Fb//x" in
  check "segments decoded"
    (Req.segments r = [ "caf\xc3\xa9"; "a/b"; "x" ]);
  check "path is raw" (Req.path r = "/caf%C3%A9/a%2Fb//x");
  check "root has no segments" (Req.segments (req "/") = []);
  check "plus is literal in a path" (Req.segments (req "/a+b") = [ "a+b" ])

let () =
  check "invalid escape is kept" (Req.segments (req "/a%zz") = [ "a%zz" ]);
  check "truncated escape is kept" (Req.segments (req "/a%4") = [ "a%4" ])

let () =
  let r = req "/s?q=hello+world&x=%41&empty&k=1&k=2" in
  check "query decoded"
    (Req.query r
    = [
        ("q", "hello world"); ("x", "A"); ("empty", ""); ("k", "1"); ("k", "2");
      ]);
  check "query_param first" (Req.query_param r "k" = Some "1");
  check "query_param absent" (Req.query_param r "nope" = None);
  check "target keeps the query"
    (Req.target r = "/s?q=hello+world&x=%41&empty&k=1&k=2");
  check "path stops at the query" (Req.path r = "/s")

let () =
  let r =
    Req.v ~meth:M.Get ~target:"http://example.test/from-wire?q=wire"
      ~path:"/from-wire" ~query:"q=parsed" ()
  in
  check "parsed path overrides absolute target" (Req.path r = "/from-wire");
  check "parsed query overrides absolute target"
    (Req.query_param r "q" = Some "parsed");
  check "absolute target is preserved"
    (Req.target r = "http://example.test/from-wire?q=wire")

let () =
  let form = [ ("Content-Type", "application/x-www-form-urlencoded") ] in
  let r = req ~headers:form ~body:"name=Ada+L.&org=%C3%89cole" "/new" in
  check "form decoded"
    (Req.form r = [ ("name", "Ada L."); ("org", "\xc3\x89cole") ]);
  check "form_param" (Req.form_param r "name" = Some "Ada L.");
  let r =
    req
      ~headers:
        [ ("content-type", "APPLICATION/X-WWW-FORM-URLENCODED; charset=utf-8") ]
      ~body:"a=1" "/new"
  in
  check "form content-type is matched loosely" (Req.form r = [ ("a", "1") ]);
  let r =
    req ~headers:[ ("Content-Type", "application/json") ] ~body:"a=1" "/new"
  in
  check "other media types are not forms" (Req.form r = []);
  check "body is still there" (Req.body r = "a=1");
  check "no body is empty" (Req.body (req "/") = "")

let () =
  let r =
    req
      ~headers:
        [
          ("X-Forwarded-For", "203.0.113.7, 198.51.100.1");
          ("X-Forwarded-Proto", " HTTPS ");
          ("If-None-Match", "\"abc\"");
        ]
      "/"
  in
  check "header lookup is case-insensitive"
    (Req.header r H.If_none_match = Some "\"abc\"");
  check "header lookup other case"
    (Req.header r H.If_none_match = Some "\"abc\"");
  check "absent header" (Req.header r H.Accept = None);
  check "forwarded_for is the first entry"
    (Req.forwarded_for r = Some "203.0.113.7");
  check "forwarded_proto is lowercased" (Req.forwarded_proto r = Some "https");
  check "to_list keeps the name as written"
    (List.mem_assoc "X-Forwarded-Proto" (Headers.to_list (Req.headers r)));
  check "mem folds case" (Headers.mem (Req.headers r) H.If_none_match);
  check "mem is not a prefix test"
    (not (Headers.mem (Req.headers r) (Headers.of_string "if-none")))

(* A field name may repeat. [find] answers with the first value and does not
   join them, and every repeat still goes on the wire as it was written. *)
let () =
  let r =
    req ~headers:[ ("X-Dup", "one"); ("x-dup", "two"); ("X-Dup", "three") ] "/"
  in
  check "find is the first value"
    (Headers.find_other (Req.headers r) "X-Dup" = Some "one");
  check "find folds case across a repeat"
    (Headers.find_other (Req.headers r) "X-Dup" = Some "one");
  check "header agrees with find"
    (Req.header_other r "X-Dup" = Some "one");
  check "every repeat is kept, spelled as written"
    (Headers.to_list (Req.headers r)
    = [ ("X-Dup", "one"); ("x-dup", "two"); ("X-Dup", "three") ]);
  check "mem folds case across a repeat"
    (Option.is_some (Headers.find_other (Req.headers r) "X-Dup"))

let () =
  let r = req "/s?" in
  check "an empty query has no parameters" (Req.query r = []);
  check "an empty query finds nothing" (Req.query_param r "q" = None);
  check "an empty query still ends the path" (Req.path r = "/s");
  check "an empty query stays in the target" (Req.target r = "/s?");
  check "no query at all has no parameters" (Req.query (req "/s") = []);
  check "an empty piece is dropped"
    (Req.query (req "/s?&a=1&") = [ ("a", "1") ])

let () =
  let headers = [ ("Content-Type", "application/x-www-form-urlencoded") ] in
  let r = req ~headers ~body:"k=1&k=2&bare&e=&s=a+b" "/new" in
  check "form keeps repeats in order"
    (Req.form r
    = [ ("k", "1"); ("k", "2"); ("bare", ""); ("e", ""); ("s", "a b") ]);
  check "form_param is the first value" (Req.form_param r "k" = Some "1");
  check "a form field with no = is empty" (Req.form_param r "bare" = Some "")

let () = Printf.printf "test_req: %d checks ok\n" !checks
