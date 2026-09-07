module Api = Regression_api
module Runtime = Openapi.Runtime

let check name value = Alcotest.(check bool) name true value
let json ?(status = 200) ?(media = "application/json") body req =
  Fetch_mock.respond ~status ~headers:(Http.Header.of_list ["Content-Type", media]) body req
let client ?max_response_bytes f =
  Api.of_fetch ?max_response_bytes ~base_url:"https://example.com/api/" (Fetch_mock.client f)
let widget = {|{"id":1,"label":null}|}
let get c = Api.Widget.get_widget ~id:"one" ~q:"query" c ()
let expect_decode f = match f () with
  | _ -> Alcotest.fail "expected Fetch decode failure"
  | exception Eio.Io (Fetch.E (Fetch.Decode_failure _), _) -> ()
let expect_invalid f = match f () with
  | _ -> Alcotest.fail "expected invalid argument"
  | exception Invalid_argument _ -> ()
let expect_error f = match f () with
  | _ -> Alcotest.fail "expected API error"
  | exception Runtime.Api_error e -> e
let body_string req = match req.Fetch.Middleware.body with
  | Fetch.String s -> s
  | _ -> Alcotest.fail "expected replayable string body"
let run f () = Eio_mock.Backend.run_full (fun _ -> f ())

let test_url_and_headers () =
  let c = client (fun req ->
    let uri = Uri.of_string (Fetch.Middleware.Url.to_string req.Fetch.Middleware.url) in
    Alcotest.(check string) "escaped path" "/api/widgets/a%2Fb%3Fc%23%7Bid%7D" (Uri.path uri);
    Alcotest.(check (option string)) "one overriding query" (Some "a+b&c") (Uri.get_query_param uri "q");
    check "no duplicate query" (List.length (List.filter (fun (n, _) -> n = "q") (Uri.query uri)) = 1);
    Alcotest.(check (option string)) "header parameter" (Some "trace") (Http.Header.get req.headers "x-trace");
    Alcotest.(check (option string)) "Accept" (Some "application/json") (Http.Header.get req.headers "accept");
    json widget req) in
  let value = Api.Widget.get_widget ~id:"a/b?c#{id}" ~q:"a+b&c" ~client_:"extra" ~x_trace:"trace" c () in
  Alcotest.(check int64) "typed object" 1L (Api.Widget.T.id value);
  Alcotest.(check string) "normalized base" "https://example.com/api" (Api.base_url c)

let test_response_shapes () =
  let values = Api.Widget.list_widgets (client (json ~status:202 ("[" ^ widget ^ "]"))) () in
  check "typed array" (List.map Api.Widget.T.id values = [1L]);
  let matrix = Api.Client.get_matrix (client (json "[[1,2],[3]]")) () in
  check "nested primitive arrays" (matrix = [[1;2];[3]]);
  check "optional empty" (Api.Widget.maybe_widget (client (Fetch_mock.respond ~status:204 "")) () = None);
  check "optional value" (Option.is_some (Api.Widget.maybe_widget (client (json widget)) ()));
  check "explicit success overrides wildcard" (Option.is_some (Api.Widget.wildcard_widget (client (json widget)) ()));
  check "empty wildcard" (Api.Widget.wildcard_widget (client (Fetch_mock.respond ~status:202 "")) () = None);
  Api.Client.get_head (client (Fetch_mock.respond "")) ();
  let text = Api.Client.put_raw ~body:(Fetch.String "input")
    (client (fun req ->
      check "binary request untouched" (body_string req = "input");
      json ~media:"text/plain" "plain reply" req)) () in
  Alcotest.(check string) "non-JSON response" "plain reply" text

let test_json_requests () =
  let value = Api.Widget.T.v ~id:1L () in
  let c = client (fun req ->
    check "DELETE method" (req.Fetch.Middleware.meth = `DELETE);
    let body = body_string req in
    check "array request codec" (Result.is_ok (Runtime.Json.decode Jsont.(list json) body));
    Alcotest.(check (option string)) "JSON type" (Some "application/json") (Http.Header.get req.headers "content-type");
    Fetch_mock.respond ~status:204 "" req) in
  Api.Client.delete_widgets ~body:[value] c ();
  let c = client (fun req ->
    Alcotest.(check (option string)) "vendor JSON type" (Some "application/vnd.example+json")
      (Http.Header.get req.Fetch.Middleware.headers "content-type");
    json ~status:201 ~media:"application/vnd.example+json" widget req) in
  ignore (Api.Widget.vendor_widget ~body:value c ());
  let c = client (fun req ->
    check "optional body omitted" (req.Fetch.Middleware.body = Fetch.Empty);
    check "no request content type" (Http.Header.get req.headers "content-type" = None);
    json ~status:201 ~media:"application/vnd.example+json" widget req) in
  ignore (Api.Widget.vendor_widget c ())

let test_forms () =
  let c = client (fun req ->
    check "form encoding" (body_string req = "q=a%2Bb+c&q=second");
    check "form content type" (Http.Header.get req.Fetch.Middleware.headers "content-type" = Some "application/x-www-form-urlencoded");
    json "true" req) in
  check "form response" (Api.Client.send_form ~body:["q", "a+b c"; "q", "second"] c ());
  let c = client (fun req ->
    let media = Option.get (Http.Header.get req.Fetch.Middleware.headers "content-type") in
    check "multipart boundary" (String.starts_with ~prefix:"multipart/form-data; boundary=" media);
    let body = body_string req in
    let contains fragment =
      let rec loop i = i + String.length fragment <= String.length body &&
        (String.sub body i (String.length fragment) = fragment || loop (i + 1)) in
      loop 0 in
    check "multipart field" (contains "name=\"caption\"\r\n\r\nx\r\n");
    check "multipart file" (contains "filename=\"a.txt\"" && contains "\r\n\r\ncontents\r\n");
    Fetch_mock.respond ~status:204 "" req) in
  Api.Client.upload_file ~body:Fetch.Form.[field "caption" "x"; file ~name:"file" ~filename:"a.txt" ~content_type:"text/plain" "contents"] c ()

let test_redirects () =
  List.iter (fun status ->
    let requests = ref 0 in
    let c = client (fun req ->
      incr requests;
      Fetch_mock.respond ~status ~headers:(Http.Header.of_list ["Location", "https://other.example/write"]) "" req) in
    let error = expect_error (fun () -> Api.Client.delete_widgets ~body:[] c ()) in
    check "redirect is returned as API error" (error.status = status);
    Alcotest.(check int) "write sent once" 1 !requests) [301;302;303;307;308];
  let credentials = ref [] in
  let fetch = Fetch_mock.client (fun req ->
    credentials := Http.Header.get req.Fetch.Middleware.headers "authorization" :: !credentials;
    if List.length !credentials = 1 then
      Fetch_mock.respond ~status:302 ~headers:(Http.Header.of_list ["Location", "https://other.example/widget"]) "" req
    else json widget req)
    |> Fetch.with_credentials ~scope:["https://example.com/api"]
         Fetch.Credential.[Bearer (fun () -> "secret")] in
  ignore (get (Api.of_fetch ~base_url:"https://example.com/api" fetch));
  check "injected scoped credentials" (List.rev !credentials = [Some "Bearer secret"; None])

let test_limits_and_errors () =
  expect_decode (fun () -> get (client ~max_response_bytes:2 (json widget)));
  expect_decode (fun () -> get (client (Fetch_mock.respond widget)));
  expect_decode (fun () -> get (client (json ~media:"text/html" widget)));
  expect_decode (fun () -> get (client (json (String.make 130 '[' ^ "0" ^ String.make 130 ']'))));
  let typed code =
    let e = expect_error (fun () -> get (client (json ~status:code {|{"message":"bad"}|}))) in
    match e.parsed_body with Some (Runtime.Typed ("Problem", _)) -> ()
    | _ -> Alcotest.fail "missing typed error" in
  typed 400;
  typed 503;
  let e = expect_error (fun () -> get (client (json ~status:404 {|{"message":"untyped exact status"}|}))) in
  (match e.parsed_body with Some (Runtime.Json _) -> () | _ -> Alcotest.fail "wildcard overrode exact response");
  let e = expect_error (fun () -> get (client (json ~status:400 "not JSON"))) in
  (match e.parsed_body with Some (Runtime.Raw _) -> () | _ -> Alcotest.fail "missing raw fallback");
  let e = expect_error (fun () -> get (client (Fetch_mock.respond ~status:503 (String.make 70000 'x')))) in
  check "oversized error preserves status" (e.status = 503 && String.length e.body < 100);
  let e = expect_error (fun () -> Api.Widget.get_widget ~id:"one" ~q:"secret" (client (json ~status:500 "{}")) ()) in
  check "error URL omits query secrets" (not (String.contains e.url '?'))

let test_codecs () =
  let decode = Runtime.Json.decode Api.Widget.T.jsont in
  check "nullable required field missing rejected" (Result.is_error (decode {|{"id":1}|}));
  check "nullable required field null accepted" (Result.is_ok (decode widget));
  check "nullable constraints preserved" (Result.is_error (decode {|{"id":1,"label":""}|}));
  check "array constraints preserved" (Result.is_error (decode {|{"id":1,"label":null,"tags":[]}|}));
  check "nullable array item" (Result.is_ok (decode {|{"id":1,"label":null,"tags":[null,"x"]}|}));
  let v = Api.Widget.T.v ~id:1L () in
  let tree = Runtime.Json.encode_json Api.Widget.T.jsont v in
  check "required null emitted" (match Openapi.Codegen.get_member "label" tree with Some (Jsont.Null _) -> true | _ -> false);
  check "nullable default does not erase absence" (Api.Widget.T.hint v = None);
  let width = Runtime.Json.decode Api.Width.T.jsont in
  check "int32 constraint" (Result.is_error (width {|{"n32":10,"n64":1}|}));
  check "integer fractions rejected" (Result.is_error (width {|{"n32":1.5,"n64":1}|}));
  check "integer strings rejected" (Result.is_error (width {|{"n32":9,"n64":"1"}|}));
  check "oneOf rejects ambiguity" (Result.is_error (Runtime.Json.decode Api.Choices.T.jsont {|{"one":1}|}));
  check "oneOf accepts a single match" (Result.is_ok (Runtime.Json.decode Api.Choices.T.jsont {|{"one":1.5}|}));
  check "anyOf allows ambiguity" (Result.is_ok (Runtime.Json.decode Api.Choices.T.jsont {|{"any":1}|}));
  check "top-level oneOf rejects ambiguity" (Result.is_error (Runtime.Json.decode Api.OneChoice.T.jsont {|{"id":1,"label":null,"message":"both"}|}));
  check "top-level oneOf accepts one branch" (Result.is_ok (Runtime.Json.decode Api.OneChoice.T.jsont widget));
  check "top-level anyOf allows ambiguity" (Result.is_ok (Runtime.Json.decode Api.AnyChoice.T.jsont {|{"id":1,"label":null,"message":"both"}|}));
  let called = ref false in
  let c = client (fun req -> called := true; Fetch_mock.respond ~status:204 "" req) in
  expect_invalid (fun () -> Api.Client.send_width ~body:(Api.Width.T.v ~n32:10l ~n64:1L ()) c ());
  check "invalid encoded values never sent" (not !called)

let test_response_lifetime () =
  List.iter (fun (status, media, body) ->
    let closed = ref 0 in
    let c = client (fun req ->
      Fetch.Middleware.Pi.response ~status ~version:`HTTP_1_1
        ~headers:(Http.Header.of_list ["Content-Type", media])
        ~body:(Eio.Flow.string_source body) ~close:(fun () -> incr closed)
        ~url:req.Fetch.Middleware.url ()) in
    (try ignore (get c) with Runtime.Api_error _ | Eio.Io _ -> ());
    Alcotest.(check int) "exchange closed exactly once" 1 !closed)
    [200, "application/json", widget;
     200, "text/html", widget;
     200, "application/json", "invalid";
     500, "application/json", "{}"]

let test_failures () =
  List.iter (fun base_url -> expect_invalid (fun () ->
    Api.of_fetch ~base_url (Fetch_mock.client (json widget))))
    ["/relative"; "https://user:secret@example.com"; "https://example.com?key=secret"; "https://example.com#fragment"];
  expect_invalid (fun () -> client ~max_response_bytes:(-1) (json widget));
  (match get (client (fun _ -> raise Exit)) with
   | _ -> Alcotest.fail "caller exception lost" | exception Exit -> ());
  (match get (client (fun _ -> raise (Eio.Cancel.Cancelled Exit))) with
   | _ -> Alcotest.fail "cancellation lost" | exception Eio.Cancel.Cancelled Exit -> ())

let () = Alcotest.run "generated OpenAPI client" ["Fetch", List.map (fun (n, f) -> n, `Quick, run f) [
  "URLs and headers", test_url_and_headers;
  "response shapes", test_response_shapes;
  "JSON requests", test_json_requests;
  "forms", test_forms;
  "redirect policy", test_redirects;
  "bounded decoding and errors", test_limits_and_errors;
  "generated codecs", test_codecs;
  "response lifetime", test_response_lifetime;
  "failure propagation", test_failures;
]]
