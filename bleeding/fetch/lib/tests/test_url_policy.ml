module Url = Fetch.Middleware.Url
module Template = Httpz.Uri_template
module Uriz = Httpz.Uriz

let fail name detail = failwith (name ^ ": " ^ detail)

let check name condition =
  if not condition then fail name "check failed"
;;

let ok name = function
  | Ok value -> value
  | Error reason -> fail name reason
;;

let uri s =
  match Uriz.of_string s with
  | This uri -> uri
  | Null -> fail "URI" (Printf.sprintf "invalid URI %S" s)
;;

let url s = ok ("URL " ^ s) (Url.of_string s)

let test_scheme_boundary () =
  let base = url "https://example.test/base/start" in
  List.iter
    (fun (scheme, reference) ->
      check ("reject scheme " ^ scheme)
        (Result.is_error (Url.of_string reference));
      check ("reject of_uri scheme " ^ scheme)
        (Result.is_error (Url.of_uri (uri reference)));
      check ("reject resolve scheme " ^ scheme)
        (Result.is_error (Url.resolve ~base reference)))
    [ "ftp", "ftp://example.test/path";
      "file", "file:///tmp/path";
      "gopher", "gopher://example.test/path";
      "javascript", "javascript:alert(1)" ];
  let http = url "HTTP://Example.test/path" in
  check "uppercase HTTP is accepted"
    (String.equal (Url.to_string http) "http://example.test/path");
  check "scheme-relative of_string is rejected"
    (Result.is_error (Url.of_string "//other.test/path"));
  check "relative of_string is rejected"
    (Result.is_error (Url.of_string "/path"));
  let relative = ok "relative resolve" (Url.resolve ~base "/next") in
  check "relative resolve stays HTTP"
    (String.equal (Url.to_string relative) "https://example.test/next");
  let scheme_relative = ok "scheme-relative resolve"
      (Url.resolve ~base "//other.test/next") in
  check "scheme-relative resolve stays HTTP"
    (String.equal (Url.to_string scheme_relative) "https://other.test/next")
;;

let template_result = function
  | Ok uri -> uri
  | Error error -> failwith (Format.asprintf "%a" Template.pp_error error)
;;

let expand source value =
  Template.expand_uri_assoc (Template.of_string_exn source) ["name", `String value]
  |> template_result
;;

let resolve base source value =
  Template.expand_resolve_assoc ~base (Template.of_string_exn source)
    ["name", `String value]
  |> template_result
;;

let test_reserved_encoding () =
  let vectors =
    [ "?", "%3F"; "#", "%23"; "/", "%2F"; "@", "%40";
      ":", "%3A"; "%", "%25"; "../", "..%2F" ]
  in
  List.iter
    (fun (value, encoded) ->
      check ("path encoding " ^ value)
        (String.equal
           (Uriz.to_string (expand "https://example.test/api/{name}" value))
           ("https://example.test/api/" ^ encoded));
      check ("query encoding " ^ value)
        (String.equal
           (Uriz.to_string (expand "https://example.test/api{?name}" value))
           ("https://example.test/api?name=" ^ encoded)))
    vectors;
  let query_value = "? # / @ : % ../" in
  let query = expand "https://example.test/api{?name}" query_value in
  check "query parameter round-trip"
    (Uriz.query_params query = [ "name", Some query_value ]);
  let resolved_query =
    resolve (uri "https://example.test/base/") "api{?name}" query_value
  in
  check "query parameter survives resolution"
    (Uriz.query_params resolved_query = [ "name", Some query_value ])
;;

let test_resolution_and_restriction () =
  let base = uri "https://example.test/api/" in
  let simple = resolve base "{name}" "../" in
  check "encoded slash survives resolution"
    (String.equal (Uriz.to_string simple) "https://example.test/api/..%2F");
  let reserved = resolve base "{+name}" "../secret" in
  check "reserved expansion traverses parent"
    (String.equal (Uriz.to_string reserved) "https://example.test/secret");
  Eio_mock.Backend.run @@ fun () ->
  let calls = ref 0 in
  let client = Fetch_mock.client (fun req ->
      incr calls; Fetch_mock.respond "ok" req)
    |> Fetch.restrict ~under:["https://example.test/api"] in
  List.iter (fun expanded ->
    match Fetch.read client (Uriz.to_string expanded) with
    | _ -> fail "expanded escape" "request was sent"
    | exception Eio.Io (Fetch.E (Fetch.Denied _), _) -> ()) [simple; reserved];
  check "expanded escapes fail before dispatch" (!calls = 0);
  let safe = resolve base "{name}" "report" in
  check "safe actual binding is dispatched"
    (Fetch.read client (Uriz.to_string safe) = "ok" && !calls = 1)
;;

let () =
  test_scheme_boundary ();
  test_reserved_encoding ();
  test_resolution_and_restriction ();
  print_endline "url policy checks passed"
