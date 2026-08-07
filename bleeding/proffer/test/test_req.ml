(* Request parsing: percent-decoding of segments, query and form, and header
   lookup. *)

open Proffer

let checks = ref 0

let check name b =
  incr checks;
  if not b then (
    prerr_endline ("FAIL: " ^ name);
    exit 1)

let req ?headers ?body target = Req.v ~meth:`GET ~target ?headers ?body ()

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
    (Req.header r "if-none-match" = Some "\"abc\"");
  check "header lookup other case"
    (Req.header r "IF-NONE-MATCH" = Some "\"abc\"");
  check "absent header" (Req.header r "accept" = None);
  check "forwarded_for is the first entry"
    (Req.forwarded_for r = Some "203.0.113.7");
  check "forwarded_proto is lowercased" (Req.forwarded_proto r = Some "https");
  check "headers to_list is lowercased"
    (List.mem_assoc "x-forwarded-proto" (Headers.to_list (Req.headers r)))

let () =
  let h = Headers.add (Headers.of_list [ ("A", "1") ]) "B" "2" in
  check "headers add" (Headers.find h "b" = Some "2");
  check "headers mem" (Headers.mem h "a" && not (Headers.mem h "c"));
  check "headers empty" (Headers.to_list Headers.empty = [])

let () = Printf.printf "test_req: %d checks ok\n" !checks
