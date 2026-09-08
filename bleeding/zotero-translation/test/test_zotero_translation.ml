module Z = Zotero_translation
module B = Z.Bibtex

let check = Alcotest.(check bool)
let text = Alcotest.(check string)
let int = Alcotest.(check int)
let tree s = Result.get_ok (Fetch.Media.decode (Fetch.Json.v Jsont.json) s)
let metadata = {|[{"title":"A paper","DOI":"10.1234/example"}]|}

let response ?(status = 200) ?(media = "application/json") ~closed body req =
  Fetch.Middleware.Pi.response ~status ~version:`HTTP_1_1
    ~headers:(Http.Header.of_list [ ("content-type", media) ])
    ~body:(Eio.Flow.string_source body)
    ~close:(fun () -> incr closed)
    ~url:req.Fetch.Middleware.url ()

let client ?max_response_bytes handler =
  Z.of_fetch ~base_url:"https://example.test/api/" ?max_response_bytes
    (Fetch_mock.client handler)

let request_body req =
  match req.Fetch.Middleware.body with
  | Fetch.String body -> body
  | _ -> Alcotest.fail "expected replayable request body"

let target req = Fetch.Middleware.Url.path_and_query req.Fetch.Middleware.url

let expect_fetch f =
  match f () with
  | _ -> Alcotest.fail "expected Fetch error"
  | exception Eio.Io (Fetch.E error, _) -> error

let expect_api f =
  match f () with
  | _ -> Alcotest.fail "expected API error"
  | exception Z.Api_error (status, body) -> (status, body)

let test_resolution () =
  List.iter
    (fun (resolve, path, body) ->
      let closed = ref 0 in
      let c =
        client (fun req ->
            check "POST" true (req.meth = `POST);
            text "endpoint" ("/api/" ^ path) (target req);
            text "payload" body (request_body req);
            check "text content type" true
              (Http.Header.get req.headers "content-type"
              = Some "text/plain; charset=utf-8");
            check "JSON accept" true
              (Http.Header.get req.headers "accept" = Some "application/json");
            response ~status:201 ~closed metadata req)
      in
      check "metadata" true (resolve c = tree metadata);
      int "response closed" 1 !closed)
    [
      ( (fun c -> Z.resolve_doi c "10.1234/example"),
        "web",
        "https://doi.org/10.1234/example" );
      ( (fun c -> Z.resolve_url c "https://paper.test/a?x=1&y=2"),
        "web",
        "https://paper.test/a?x=1&y=2" );
      ( (fun c -> Z.search_id c "10.1234/example"),
        "search",
        "https://doi.org/10.1234/example" );
    ]

let formats =
  Z.
    [
      Bibtex;
      Biblatex;
      Bookmarks;
      Coins;
      Csljson;
      Csv;
      Endnote_xml;
      Evernote;
      Mods;
      Rdf_bibliontology;
      Rdf_dc;
      Rdf_zotero;
      Refer;
      Refworks_tagged;
      Ris;
      Tei;
      Wikipedia;
    ]

let test_export () =
  List.iter
    (fun format ->
      let name = Z.format_to_string format in
      check "format round trip" true (Z.format_of_string name = Some format);
      let closed = ref 0 in
      let c =
        client (fun req ->
            check "POST" true (req.meth = `POST);
            text "format query" ("/api/export?format=" ^ name) (target req);
            check "JSON payload" true (tree (request_body req) = tree metadata);
            check "JSON content type" true
              (Http.Header.get req.headers "content-type"
              = Some "application/json");
            check "accept every export type" true
              (Http.Header.get req.headers "accept" = Some "*/*");
            response ~media:"application/x-bibtex" ~closed " \nexport\n " req)
      in
      let expected = if format = Z.Bibtex then "export" else " \nexport\n " in
      text "preserve export bytes" expected (Z.export c format (tree metadata));
      int "response closed" 1 !closed)
    formats;
  check "unknown format" true (Z.format_of_string "unknown" = None)

let test_configuration () =
  let fetch = Fetch_mock.client (fun _ -> Alcotest.fail "unexpected request") in
  List.iter
    (fun base_url ->
      match Z.of_fetch ~base_url fetch with
      | _ -> Alcotest.fail ("accepted invalid base: " ^ base_url)
      | exception Invalid_argument _ -> ())
    [
      "";
      "/api";
      "ftp://example.test";
      "https://user:pass@example.test";
      "https://example.test?";
      "https://example.test?key=value";
      "https://example.test#";
      "https://example.test#fragment";
    ];
  (match
     Z.of_fetch ~base_url:"https://example.test" ~max_response_bytes:(-1) fetch
   with
  | _ -> Alcotest.fail "accepted negative body limit"
  | exception Invalid_argument _ -> ());
  List.iter
    (fun (base_url, expected) ->
      let c = Z.of_fetch ~base_url fetch in
      text "canonical base" expected (Z.base_url c))
    [
      ("HTTPS://EXAMPLE.TEST:443/a/../api", "https://example.test/api/");
      ("http://localhost:1969", "http://localhost:1969/");
      ("https://example.test/api//", "https://example.test/api//");
    ]

let test_policy () =
  let called = ref 0 in
  let fetch =
    Fetch_mock.client (fun req ->
        incr called;
        Fetch_mock.respond "{}" req)
  in
  let c = Z.of_fetch ~base_url:"https://example.test/api" fetch in
  List.iter
    (fun (meth, url) ->
      match
        expect_fetch (fun () ->
            Fetch.with_response (Z.http_session c) meth url (fun _ -> ()))
      with
      | Fetch.Denied _ -> ()
      | _ -> Alcotest.fail "expected capability denial")
    [
      (`GET, "https://example.test/api/web");
      (`POST, "https://elsewhere.test/api/web");
      (`POST, "https://example.test/apix/web");
    ];
  let c =
    Z.of_fetch ~base_url:"https://example.test/api" (Fetch.read_only fetch)
  in
  (match expect_fetch (fun () -> Z.resolve_doi c "x") with
  | Fetch.Denied _ -> ()
  | _ -> Alcotest.fail "lost caller's restriction");
  int "no denied request sent" 0 !called

let test_redirects () =
  List.iter
    (fun status ->
      let called = ref 0 and closed = ref 0 in
      let c =
        client (fun req ->
            incr called;
            Fetch.Middleware.Pi.response ~status ~version:`HTTP_1_1
              ~headers:
                (Http.Header.of_list
                   [ ("location", "https://elsewhere.test/") ])
              ~body:(Eio.Flow.string_source "")
              ~close:(fun () -> incr closed)
              ~url:req.Fetch.Middleware.url ())
      in
      let actual_status, _ = expect_api (fun () -> Z.resolve_doi c "x") in
      int "redirect status preserved" status actual_status;
      int "no redirected POST" 1 !called;
      int "redirect closed" 1 !closed)
    [ 301; 302; 303; 307; 308 ]

let test_http_errors () =
  List.iter
    (fun status ->
      let closed = ref 0 in
      let c =
        client (response ~status ~media:"text/html" ~closed "unavailable")
      in
      let actual_status, body = expect_api (fun () -> Z.resolve_doi c "x") in
      int "HTTP status" status actual_status;
      text "diagnostic body" "unavailable" body;
      int "error closed" 1 !closed)
    [ 300; 400; 401; 404; 429; 500; 503 ];
  List.iter
    (fun (max_response_bytes, size) ->
      let closed = ref 0 in
      let c =
        client ~max_response_bytes
          (response ~status:503 ~closed (String.make size 'x'))
      in
      let status, body =
        expect_api (fun () -> Z.export c Z.Csljson (tree "[]"))
      in
      int "status survives oversized error" 503 status;
      text "bounded diagnostic" "[response exceeds diagnostic limit]" body;
      int "oversized error closed" 1 !closed)
    [ (16, 17); (1024 * 1024, 65537) ]

let test_decode_errors () =
  List.iter
    (fun (media, body) ->
      let closed = ref 0 in
      let c = client (response ~media ~closed body) in
      (match expect_fetch (fun () -> Z.resolve_doi c "x") with
      | Fetch.Decode_failure _ -> ()
      | _ -> Alcotest.fail "expected decoding error");
      int "invalid response closed" 1 !closed)
    [
      ("application/json", "{");
      ("text/html", "{}");
      ("application/json", "{} trailing");
    ]

let test_limits () =
  List.iter
    (fun (wire, operation) ->
      let size = String.length wire in
      let closed = ref 0 in
      let handler = response ~closed wire in
      ignore (operation (client ~max_response_bytes:size handler));
      (match
         expect_fetch (fun () ->
             operation (client ~max_response_bytes:(size - 1) handler))
       with
      | Fetch.Decode_failure { error = Too_large limit; _ } ->
          int "configured limit" (size - 1) limit
      | _ -> Alcotest.fail "expected body limit error");
      int "both responses closed" 2 !closed)
    [
      (metadata, fun c -> ignore (Z.resolve_doi c "x"));
      ("export", fun c -> ignore (Z.export c Z.Ris (tree "[]")));
    ];
  text "empty export at zero limit" ""
    (Z.export
       (client ~max_response_bytes:0 (Fetch_mock.respond ""))
       Z.Ris (tree "[]"))

let test_cancellation () =
  List.iter
    (fun operation ->
      let closed = ref 0 in
      let flow = Eio_mock.Flow.make "cancelled response" in
      Eio_mock.Flow.on_read flow [ `Raise (Eio.Cancel.Cancelled Exit) ];
      let c =
        client (fun req ->
            Fetch.Middleware.Pi.response ~status:200 ~version:`HTTP_1_1
              ~headers:
                (Http.Header.of_list [ ("content-type", "application/json") ])
              ~body:(flow :> Eio.Flow.source_ty Eio.Resource.t)
              ~close:(fun () -> incr closed)
              ~url:req.Fetch.Middleware.url ())
      in
      (match operation c with
      | () -> Alcotest.fail "lost cancellation"
      | exception Eio.Cancel.Cancelled Exit -> ());
      int "cancelled response closed" 1 !closed)
    [
      (fun c -> ignore (Z.json_of_doi c ~slug:"paper" "x"));
      (fun c -> ignore (Z.export c Z.Bibtex (tree "[]")));
    ]

let test_fallback () =
  let paths = ref [] and closed = ref 0 in
  let c =
    client (fun req ->
        let path = target req in
        paths := path :: !paths;
        match path with
        | "/api/web" -> response ~status:404 ~closed "not found" req
        | "/api/search" -> response ~closed metadata req
        | "/api/export?format=bibtex" ->
            response ~media:"text/plain" ~closed
              "@article{old, title = {A paper}, author = {Doe, Jane}, year = \
               {2026}}"
              req
        | _ -> Alcotest.fail "unexpected endpoint")
  in
  let result = Z.json_of_doi c ~slug:"my-paper" "10.1234/example" in
  let encoded = Fetch.Media.encode (Fetch.Json.v Jsont.json) result in
  check "enriched citation key" true
    (Astring.String.is_infix ~affix:"my_paper" encoded);
  Alcotest.(check (list string))
    "fallback order"
    [ "/api/web"; "/api/search"; "/api/export?format=bibtex" ]
    (List.rev !paths);
  int "all responses closed" 3 !closed;
  let called = ref 0 in
  let c =
    client (fun req ->
        incr called;
        response ~closed "invalid JSON" req)
  in
  ignore (expect_fetch (fun () -> Z.json_of_doi c ~slug:"paper" "x"));
  int "decode failure does not trigger fallback" 1 !called

let test_bibtex_labels () =
  let source = "@article{paper, title = {A paper}, year = {2026}}" in
  let entries = Result.get_ok (B.of_string ~file:"memory:papers" source) in
  (match entries with
  | [ entry ] ->
      text "entry key" "paper" (B.cite_key entry);
      text "entry title" "A paper" (B.SM.find "title" (B.fields entry))
  | _ -> Alcotest.fail "expected one entry");
  check "codec round trip" true (B.of_string (B.to_string entries) = Ok entries);
  List.iter
    (fun (file, label) ->
      match B.of_string' ?file "@article{" with
      | Ok _ -> Alcotest.fail "accepted invalid BibTeX"
      | Error message ->
          check "diagnostic source label" true
            (String.starts_with ~prefix:(label ^ ":") message))
    [
      (None, "-");
      (Some "memory:papers", "memory:papers");
      (Some "/does/not/exist/papers.bib", "/does/not/exist/papers.bib");
    ]

let () =
  Alcotest.run "Zotero Translation Fetch client"
    [
      ( "client",
        List.map
          (fun (name, test) ->
            (name, `Quick, fun () -> Eio_mock.Backend.run (fun () -> test ())))
          [
            ("resolution", test_resolution);
            ("export formats", test_export);
            ("configuration", test_configuration);
            ("capability restrictions", test_policy);
            ("redirects", test_redirects);
            ("HTTP errors", test_http_errors);
            ("decoding errors", test_decode_errors);
            ("response bounds", test_limits);
            ("cancellation cleanup", test_cancellation);
            ("DOI fallback", test_fallback);
            ("BibTeX source labels", test_bibtex_labels);
          ] );
    ]
