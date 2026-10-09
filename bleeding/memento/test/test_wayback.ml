(* Copyright (c) 2026 Anil Madhavapeddy. SPDX-License-Identifier: ISC *)
let checks = ref 0
let check name b = incr checks; if not b then failwith name
let get = function Ok v -> v | Error e -> failwith e
let () = Eio_mock.Backend.run @@ fun () ->
  let calls = ref 0 and body = ref
    {|[["mimetype","original","timestamp","statuscode"],["text/html","https://example.org/a?q=1&b=2","20240102030405","200"],["text/html","https://example.org/a?q=1&b=2","20250102030405","404"]]|} in
  let media = ref "application/json" and status = ref 200 in
  let inspect = ref (fun _ -> ()) in
  let client = Fetch_mock.client (fun req ->
    incr calls;
    let uri = Httpz_uri.of_string_exn (Fetch.Middleware.Url.to_string req.Fetch.Middleware.url) in
    check "CDX endpoint" (Httpz_uri.encoded_host uri = This "web.archive.org"
      && Httpz_uri.encoded_path uri = "/cdx/search/cdx");
    check "GET only" (req.meth = `GET);
    check "user agent" (Http.Header.get req.headers "user-agent" = Some "memento/0.1.0");
    !inspect (List.map (fun (name, value) -> name, Option.get value)
      (Httpz_uri.query_params uri));
    Fetch_mock.respond ~status:!status
      ~headers:(Http.Header.of_list ["Content-Type", !media]) !body req) in
  inspect := (fun params ->
    check "original query encoded independently"
      (List.assoc "url" params = "https://example.org/a?q=1&b=2");
    check "exact scope" (List.assoc "matchType" params = "exact");
    check "latest limit" (List.assoc "limit" params = "-2");
    check "date bounds" (List.assoc "from" params = "2024" && List.assoc "to" params = "2025"));
  let versions = get (Memento_wayback.versions ~limit:2 ~latest:true
    ~from:"2024" ~until:"2025" client "https://example.org/a?q=1&b=2#fragment") in
  check "all indexed statuses included" (List.map (fun (v : Memento_wayback.version) -> v.status)
      versions = [Some 200; Some 404]);
  check "timestamp decoded"
    (Memento.Datetime.to_json (List.hd versions).capture.datetime = "2024-01-02T03:04:05Z");
  check "playback URL" ((List.hd versions).capture.uri =
    "https://web.archive.org/web/20240102030405/https://example.org/a?q=1&b=2");
  body := {|[["original"],["https://example.org/a"],["https://example.org/b"]]|};
  inspect := (fun params ->
    check "prefix scope" (List.assoc "matchType" params = "prefix");
    check "collapse URL keys" (List.assoc "collapse" params = "urlkey");
    check "bounded URL query" (List.assoc "limit" params = "2"));
  check "URL listing" (get (Memento_wayback.urls ~limit:2 client "https://example.org/")
      = ["https://example.org/a"; "https://example.org/b"]);
  inspect := (fun _ -> ());
  let before = !calls in
  List.iter (fun limit -> check "invalid limit has no request"
    (Result.is_error (Memento_wayback.versions ~limit client "https://example.org/")))
    [0; -1; 10001];
  List.iter (fun url -> check "invalid original has no request"
    (Result.is_error (Memento_wayback.urls client url)))
    ["relative"; "ftp://example.org/"; "https://user:pass@example.org/"; "https://example.org/%zz"];
  check "invalid date bound"
    (Result.is_error (Memento_wayback.versions ~from:"2024-01-01" client "https://example.org/"));
  check "invalid inputs bypass backend" (!calls = before);
  body := "[]";
  check "no captures" (get (Memento_wayback.versions client "https://example.org/") = []);
  body := {|[["original"]]|};
  check "no URLs" (get (Memento_wayback.urls client "https://example.org/") = []);
  List.iter (fun malformed -> body := malformed;
    check "malformed CDX response rejected"
      (Result.is_error (Memento_wayback.versions client "https://example.org/")))
    [ {|[["timestamp","original","statuscode","mimetype"],["20241301000000","https://example.org/","200","text/html"]]|};
      {|[["timestamp","original","statuscode","mimetype"],["20240101000000","https://example.org/","bad","text/html"]]|};
      {|[["timestamp","original","statuscode","mimetype"],["20240101","https://example.org/","200","text/html"]]|};
      {|[["timestamp","timestamp","original","statuscode","mimetype"]]|};
      {|[["timestamp","original","statuscode","mimetype"],["20240101000000"]]|};
      {|[["timestamp","original"]]|}; "invalid" ];
  body := {|[["timestamp","original","statuscode","mimetype"],["20240101000000","https://example.org/","-","warc/revisit"]]|};
  check "unknown recorded status"
    ((List.hd (get (Memento_wayback.versions client "https://example.org/"))).status = None);
  status := 429;
  check "rate limiting reported" (Result.is_error (Memento_wayback.urls client "https://example.org/"));
  status := 200; media := "text/html";
  check "unexpected media rejected" (Result.is_error (Memento_wayback.urls client "https://example.org/"));
  media := "application/json"; body := String.make (4 * 1024 * 1024 + 1) ' ';
  check "body bound enforced" (Result.is_error (Memento_wayback.urls client "https://example.org/"));
  body := {|[["original"],["https://example.org/a"],["https://example.org/b"]]|};
  check "row bound enforced" (Result.is_error (Memento_wayback.urls ~limit:1 client "https://example.org/"));
  let denied = Fetch.restrict ~filter:(fun _ -> `Reject "denied") client in
  let before = !calls in
  check "Fetch authority preserved"
    (try ignore (Memento_wayback.urls denied "https://example.org/"); false
     with Eio.Io (Fetch.E (Fetch.Denied _), _) -> !calls = before);
  Printf.printf "Wayback: %d checks passed\n" !checks
