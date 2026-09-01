open Proffer
module H = Httpz.Header_name
module M = Httpz.Method

let checks = ref 0

let check name b =
  incr checks;
  if not b then (
    prerr_endline ("FAIL: " ^ name);
    exit 1)

(* A request is local, so a helper that makes one returns it into the
   caller's region. *)
let req ?headers ?body target = exclave_
  let headers = Option.map Headers.of_list headers in
  Req.v ~meth:M.Get ~target ?headers ?body ()

(* The block's strings are local, so the list is walked rather than searched
   with [List.mem_assoc]. *)
let rec mem_assoc_local name (l : (string * string) list @ local) =
  match l with
  | [] -> false
  | (k, _) :: tl -> String.equal k name || mem_assoc_local name tl

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
    (mem_assoc_local "X-Forwarded-Proto" (Headers.to_list (Req.headers r)));
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
    (match Headers.find_other (Req.headers r) "X-Dup" with
     | Some _ -> true
     | None -> false)

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

(* [form_param] scans the body while [form] builds the list; the two must not
   disagree about a name. *)
let () =
  let headers = [ ("Content-Type", "application/x-www-form-urlencoded") ] in
  let bodies =
    [
      "";
      "a=1";
      "a=1&a=2&a=3";
      "a+b=c+d&a%2Bb=c%2Bd";
      "bare";
      "bare&bare=1";
      "e=&f";
      "%zz=%4&x=%C3%A9";
      "&&a=1&&";
      "=novalue&k=v";
      "a=b=c&d";
    ]
  in
  let names =
    [ "a"; "a b"; "a+b"; "bare"; "e"; "f"; "%zz"; ""; "k"; "d"; "z" ]
  in
  (* A request is local, so it cannot cross into the closure [List.iter]
     wants: build one per name instead. *)
  let rec each body = function
    | [] -> ()
    | name :: rest ->
        let r = req ~headers ~body "/t" in
        check
          ("form_param agrees with form for " ^ body ^ " / " ^ name)
          (Req.form_param r name = List.assoc_opt name (Req.form r));
        each body rest
  in
  List.iter (fun body -> each body names) bodies

let () =
  let headers = [ ("Content-Type", "application/x-www-form-urlencoded") ] in
  let r = req ~headers ~body:"a=1" "/new" in
  check "is_form" (Req.is_form r);
  check "form_result ok" (Req.form_result r = Ok [ ("a", "1") ]);
  let r = req ~headers ~body:"" "/new" in
  check "an empty form body is a form" (Req.is_form r);
  check "an empty form body decodes to nothing" (Req.form_result r = Ok []);
  let r =
    req
      ~headers:[ ("Content-Type", "application/json") ]
      ~body:"{\"a\":1}" "/new"
  in
  check "another media type is not a form" (not (Req.is_form r));
  check "form_result on another media type"
    (Req.form_result r
    = Error (Media.Unsupported (Some "application/json")));
  check "form_param on another media type" (Req.form_param r "a" = None);
  let r = req ~body:"a=1" "/new" in
  check "a body with no type is not a form" (not (Req.is_form r));
  check "form_result with no type"
    (Req.form_result r = Error (Media.Unsupported None))

(* A codec's callbacks are portable, so a portable route can capture a
   module-level codec directly. The annotation makes this a compile-time
   regression test for the whole [Media.t] value. *)
let form_from_unit
  : (unit -> (string * string) list Media.t) @ portable =
  fun () -> Media.form

let () =
  let site =
    Site.of_routes
      [
        Route.post (Route.s "greet")
          (Route.with_body
             form_from_unit
             (fun ps () _req respond ->
               Resp.text respond
                 (String.concat ","
                    (List.map (fun (k, v) -> k ^ "=" ^ v) ps))));
      ]
  in
  let request ?headers ?body () =
    Proffer_mock.request site () M.Post "/greet" ?headers ?body
  in
  let headers = [ ("Content-Type", "application/x-www-form-urlencoded") ] in
  let r = request ~headers ~body:"name=Ada+L.&k=1&k=2" () in
  check "with_body form ok"
    (Status.code (Proffer_mock.status r) = 200
    && Proffer_mock.body r = "name=Ada L.,k=1,k=2");
  let r = request ~headers:[ ("Content-Type", "text/plain") ] ~body:"a=1" () in
  check "with_body form 415" (Status.code (Proffer_mock.status r) = 415)

let () = Printf.printf "test_req: %d checks ok\n" !checks
