(* test_route.ml - behavioural tests for the router.

   The router previously walked a [Base_trie] keyed by a freshly split
   [string list]; it now walks a hand-rolled node whose literal keys are
   compared against the parse buffer in place, and only materialises a string
   when a [seg] or [tail] captures one. These tests pin the dispatch
   behaviour that rewrite has to preserve: literal matching, captures,
   priority between exact/wildcard/catch-all routes, HEAD falling back to GET,
   and the treatment of repeated and trailing slashes. *)

open Base
module R = Httpz_route
module I64 = Stdlib_upstream_compatible.Int64_u

let failures = ref 0

let check name cond detail =
  if not cond
  then begin
    Int.incr failures;
    if !failures <= 20 then Stdio.printf "FAIL [%s] %s\n" name (detail ())
  end
;;

let limits = Httpz.default_limits
let i16 = Httpz.Buf_read.i16

(* Drive a real request through the parser and then the router, so the test
   exercises the same spans dispatch sees at runtime. Returns the response
   body the handler produced, or [None] if no route matched. *)
let run routes ~meth ~target =
  let req_text = Printf.sprintf "%s %s HTTP/1.1\r\nHost: x\r\nX-Trace: t\r\n\r\n" meth target in
  let buf = Bytes.make Httpz.buffer_size '\000' in
  let len = String.length req_text in
  Bytes.From_string.blit ~src:req_text ~src_pos:0 ~dst:buf ~dst_pos:0 ~len;
  let #(status, req, headers) = Httpz.parse buf ~len:(i16 len) ~limits in
  (match status with
   | Httpz.Buf_read.Complete -> ()
   | _ -> check "parse" false (fun () -> Printf.sprintf "request did not parse: %s" req_text));
  let result = ref None in
  let matched =
    R.dispatch
      buf
      ~meth:req.#meth
      ~path:req.#path
      ~query:req.#query
      ~body:(Httpz.Span.make ~off:(i16 0) ~len:(i16 0))
      ~content_length:(I64.of_int64 (-1L))
      ~headers
      routes
      ~respond:(fun ~status:_ ~headers:_ body ->
        match body with
        | R.String s -> result := Some s
        | R.Empty -> result := Some ""
        | _ -> result := Some "<other>")
  in
  if matched then !result else None
;;

let expect routes ~meth ~target ~want =
  let got = run routes ~meth ~target in
  let show = function
    | None -> "<no match>"
    | Some s -> Printf.sprintf "%S" s
  in
  check "dispatch" (Option.equal String.equal got want) (fun () ->
    Printf.sprintf "%s %s -> %s, wanted %s" meth target (show got) (show want))
;;

let routes =
  R.of_list
    [ R.get_ [] (fun _ctx respond -> R.plain respond "root")
    ; R.get_ [ "api"; "status" ] (fun _ctx respond -> R.plain respond "status")
    ; R.get_ [ "api"; "v1"; "health" ] (fun _ctx respond -> R.plain respond "health")
    ; R.get (R.( / ) "users" (R.seg R.root)) (fun (id, ()) _ctx respond ->
        R.plain respond ("user:" ^ id))
    ; R.get (R.( / ) "api" (R.( / ) "v1" (R.seg R.root))) (fun (res, ()) _ctx respond ->
        R.plain respond ("v1:" ^ res))
    ; R.get (R.( / ) "static" R.tail) (fun segs _ctx respond ->
        R.plain respond ("static:" ^ String.concat ~sep:"|" segs))
    ; R.post (R.( / ) "users" (R.seg R.root)) (fun (id, ()) _ctx respond ->
        R.plain respond ("post-user:" ^ id))
    ]
;;

let test_literals () =
  expect routes ~meth:"GET" ~target:"/" ~want:(Some "root");
  expect routes ~meth:"GET" ~target:"/api/status" ~want:(Some "status");
  expect routes ~meth:"GET" ~target:"/api/v1/health" ~want:(Some "health")
;;

let test_captures () =
  expect routes ~meth:"GET" ~target:"/users/12345" ~want:(Some "user:12345");
  expect routes ~meth:"GET" ~target:"/api/v1/widgets" ~want:(Some "v1:widgets");
  expect routes ~meth:"GET" ~target:"/static/a/b/c.css" ~want:(Some "static:a|b|c.css");
  (* A tail may capture nothing at all. *)
  expect routes ~meth:"GET" ~target:"/static" ~want:(Some "static:");
  expect routes ~meth:"GET" ~target:"/static/" ~want:(Some "static:")
;;

(* A literal must win over a wildcard at the same depth. *)
let test_literal_beats_wildcard () =
  expect routes ~meth:"GET" ~target:"/api/v1/health" ~want:(Some "health");
  expect routes ~meth:"GET" ~target:"/api/v1/other" ~want:(Some "v1:other")
;;

let test_method_matching () =
  expect routes ~meth:"POST" ~target:"/users/7" ~want:(Some "post-user:7");
  (* HEAD falls back to a GET route. *)
  expect routes ~meth:"HEAD" ~target:"/api/status" ~want:(Some "status");
  (* No DELETE route is registered. *)
  expect routes ~meth:"DELETE" ~target:"/users/7" ~want:None
;;

let test_no_match () =
  expect routes ~meth:"GET" ~target:"/nope" ~want:None;
  expect routes ~meth:"GET" ~target:"/api" ~want:None;
  expect routes ~meth:"GET" ~target:"/api/v1/health/extra" ~want:None;
  expect routes ~meth:"GET" ~target:"/users" ~want:None;
  expect routes ~meth:"GET" ~target:"/users/1/2" ~want:None
;;

(* Empty segments from repeated or trailing slashes were previously dropped by
   [List.filter] on the split path; the in-place walk skips runs of '/'
   instead, which must behave the same way. *)
let test_slashes () =
  expect routes ~meth:"GET" ~target:"//api//status" ~want:(Some "status");
  expect routes ~meth:"GET" ~target:"/api/status/" ~want:(Some "status");
  expect routes ~meth:"GET" ~target:"/users/42/" ~want:(Some "user:42");
  expect routes ~meth:"GET" ~target:"///" ~want:(Some "root")
;;

(* [path] now returns the request path verbatim rather than rebuilding it from
   split segments, and query strings must not leak into it. *)
let test_ctx_path_and_query () =
  let probe =
    R.of_list
      [ R.get_ [ "probe" ] (fun ctx respond -> R.plain respond (R.path ctx))
      ; R.get_ [ "q" ] (fun ctx respond ->
          R.plain
            respond
            (match R.query_param ctx "name" with
             | None -> "<none>"
             | Some v -> v))
      ]
  in
  expect probe ~meth:"GET" ~target:"/probe" ~want:(Some "/probe");
  expect probe ~meth:"GET" ~target:"/probe?x=1" ~want:(Some "/probe");
  expect probe ~meth:"GET" ~target:"/q?name=alice" ~want:(Some "alice");
  (* Percent- and plus-decoding still applies on the decode path. *)
  expect probe ~meth:"GET" ~target:"/q?name=a%20b" ~want:(Some "a b");
  expect probe ~meth:"GET" ~target:"/q?name=a+b" ~want:(Some "a b");
  expect probe ~meth:"GET" ~target:"/q?other=1&name=bob" ~want:(Some "bob");
  expect probe ~meth:"GET" ~target:"/q" ~want:(Some "<none>")
;;

(* Whether the parser refuses [target] outright. *)
let rejected target =
  let req_text = Printf.sprintf "GET %s HTTP/1.1\r\nHost: x\r\n\r\n" target in
  let buf = Bytes.make Httpz.buffer_size '\000' in
  let len = String.length req_text in
  Bytes.From_string.blit ~src:req_text ~src_pos:0 ~dst:buf ~dst_pos:0 ~len;
  let #(status, _req, _headers) = Httpz.parse buf ~len:(i16 len) ~limits in
  match status with
  | Httpz.Buf_read.Complete -> false
  | _ -> true
;;

(* Query values are percent-decoded, and ['+'] reads as a space per
   application/x-www-form-urlencoded. A malformed ['%'] is not decoded
   leniently: the RFC 3986 grammar admits ['%'] only in a complete triplet, so
   the parser refuses the request before any handler sees it. *)
let test_query_decoding () =
  let probe =
    R.of_list
      [ R.get_ [ "q" ] (fun ctx respond ->
          R.plain
            respond
            (match R.query_param ctx "v" with
             | None -> "<none>"
             | Some v -> "[" ^ v ^ "]"))
      ; R.get_ [ "all" ] (fun ctx respond ->
          R.plain respond (String.concat ~sep:"|" (R.query_params ctx "v")))
      ]
  in
  let case target want =
    expect probe ~meth:"GET" ~target ~want:(Some ("[" ^ want ^ "]"))
  in
  case "/q?v=plain" "plain";
  case "/q?v=%41" "A";
  case "/q?v=a%2Fb" "a/b";
  case "/q?v=a%2fb" "a/b";
  case "/q?v=a+b" "a b";
  case "/q?v=a%2Bb" "a+b";
  case "/q?v=%20%41%2b" " A+";
  case "/q?v=" "";
  case "/q?u=1&v=x%21y&w=2" "x!y";
  expect probe ~meth:"GET" ~target:"/all?v=a+b&v=%41" ~want:(Some "a b|A");
  List.iter [ "/q?v=%"; "/q?v=%zz"; "/q?v=%4"; "/q?v=%2%41" ] ~f:(fun t ->
    check "malformed-query" (rejected t) (fun () -> Printf.sprintf "%s was accepted" t))
;;

(* Path segments reach a handler exactly as they were sent. The static file
   server decodes each one on its own with {!Uriz.pct_decode}, where ['+'] is
   an ordinary character rather than a space, and a ["%2f"] becomes a ['/']
   inside the segment without splitting it. *)
let test_path_decoding () =
  let probe =
    R.of_list
      [ R.get (R.( / ) "f" R.tail) (fun segs _ctx respond ->
          R.plain respond (String.concat ~sep:"|" segs))
      ]
  in
  expect probe ~meth:"GET" ~target:"/f/a+b/c%2Fd/e%20f" ~want:(Some "a+b|c%2Fd|e%20f");
  let decode s =
    match Uriz.pct_decode s with
    | Null -> "<bad>"
    | This d -> d
  in
  let case s want =
    check "path-decode" (String.equal (decode s) want) (fun () ->
      Printf.sprintf "%S -> %S, wanted %S" s (decode s) want)
  in
  case "plain" "plain";
  case "%41" "A";
  case "a%2fb" "a/b";
  case "a+b" "a+b";
  case "a%2Bb" "a+b";
  case "" "";
  case "hello%20world" "hello world";
  case "%e2%82%ac" "\xe2\x82\xac";
  List.iter [ "%"; "%zz"; "%4"; "a%2" ] ~f:(fun s ->
    check "path-decode" (String.equal (decode s) "<bad>") (fun () ->
      Printf.sprintf "%S decoded to %S" s (decode s)));
  (* The whole target is refused before the segments are ever split. *)
  List.iter [ "/f/a%"; "/f/a%zz" ] ~f:(fun t ->
    check "malformed-path" (rejected t) (fun () -> Printf.sprintf "%s was accepted" t))
;;

(* [add] prepends, so a later route shadows an earlier one at the same path.
   The rewrite rebuilds the tree on every [add] and must preserve that. *)
let test_priority () =
  let t = R.add (R.get_ [ "dup" ] (fun _ c -> R.plain c "second")) R.empty in
  let t = R.add (R.get_ [ "dup" ] (fun _ c -> R.plain c "third")) t in
  expect t ~meth:"GET" ~target:"/dup" ~want:(Some "third");
  (* of_list must agree with repeated add. *)
  let t2 =
    R.of_list
      [ R.get_ [ "dup" ] (fun _ c -> R.plain c "second")
      ; R.get_ [ "dup" ] (fun _ c -> R.plain c "third")
      ]
  in
  expect t2 ~meth:"GET" ~target:"/dup" ~want:(Some "third")
;;

(* Header extraction runs off the same local header list the parser returns. *)
let test_header_extraction () =
  let t =
    R.of_list
      [ R.get_h1 R.root Httpz.Header_name.Host (fun () h _ctx respond ->
          R.plain
            respond
            (match h with
             | None -> "<no host>"
             | Some v -> "host:" ^ v))
      ]
  in
  expect t ~meth:"GET" ~target:"/" ~want:(Some "host:x")
;;

let () =
  test_literals ();
  test_captures ();
  test_literal_beats_wildcard ();
  test_method_matching ();
  test_no_match ();
  test_slashes ();
  test_ctx_path_and_query ();
  test_query_decoding ();
  test_path_decoding ();
  test_priority ();
  test_header_extraction ();
  if !failures > 0
  then begin
    Stdio.printf "%d router failures\n" !failures;
    Stdlib.exit 1
  end;
  Stdio.printf "test_route: dispatch behaviour preserved\n"
;;
