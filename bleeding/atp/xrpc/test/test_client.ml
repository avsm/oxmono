let check name p = if not p then failwith name
let json ?(status = 200) body req = Fetch_mock.respond ~status
  ~headers:(Http.Header.of_list ["Content-Type", "application/json"]) body req
let query client = Xrpc.Client.query client ~nsid:"com.example.get" ~params:[] ~decoder:Jsont.json
let procedure client = Xrpc.Client.procedure client ~nsid:"com.example.set" ~params:[]
  ~input:(Some Jsont.string) ~input_data:(Some "private") ~decoder:Jsont.json
let expect_parse f = match f () with _ -> failwith "expected parse error"
  | exception Eio.Io (Xrpc.Error.E (Parse_error _), _) -> ()
let expect_invalid f = match f () with _ -> failwith "expected invalid argument"
  | exception Invalid_argument _ -> ()
let session : Xrpc.Types.session = { access_jwt = "secret"; refresh_jwt = "refresh";
  did = "did:plc:alice"; handle = "alice.example"; pds_uri = None; email = None;
  email_confirmed = None; email_auth_factor = None; active = None; status = None }

let () = Eio_mock.Backend.run_full @@ fun _ ->
  List.iter (fun status ->
    let client = Xrpc.Client.of_fetch ~service:"https://example.com"
      (Fetch_mock.client (Fetch_mock.respond ~status "")) in
    Xrpc.Client.procedure_unit client ~nsid:"com.example.delete" ~params:[]
      ~input:None ~input_data:None) [200;204];
  List.iter (fun status ->
    let calls = ref 0 in
    let client = Xrpc.Client.of_fetch ~service:"https://example.com/"
      (Fetch_mock.client (fun req -> incr calls;
        Fetch_mock.respond ~status ~headers:(Http.Header.of_list ["Location", "https://other.example/xrpc/com.example.set"]) "" req)) in
    (match procedure client with _ -> failwith "write redirect accepted"
     | exception Eio.Io (Xrpc.Error.E (Xrpc_error {status = actual; _}), _) -> check "redirect status" (actual = status));
    check "write is never forwarded" (!calls = 1)) [301;302;303;307;308];
  let credentials = ref [] in
  let client = Xrpc.Client.of_fetch ~service:"https://example.com/" (Fetch_mock.client (fun req ->
    credentials := Http.Header.get req.Fetch.Middleware.headers "authorization" :: !credentials;
    if List.length !credentials = 1 then Fetch_mock.respond ~status:302
      ~headers:(Http.Header.of_list ["Location", "https://other.example/result"]) "" req
    else json "{}" req)) in
  Xrpc.Client.set_session client session;
  ignore (query client);
  check "scoped bearer on redirected GET" (List.rev !credentials = [Some "Bearer secret"; None]);
  let client = Xrpc.Client.of_fetch ~service:"https://example.com/" ~max_response_bytes:2
    (Fetch_mock.client (json "[1,2]")) in
  expect_parse (fun () -> query client);
  let client = Xrpc.Client.of_fetch ~service:"https://example.com/" (Fetch_mock.client (Fetch_mock.respond "{}")) in
  expect_parse (fun () -> query client);
  let requests = ref 0 in
  let client = Xrpc.Client.of_fetch ~service:"https://example.com/" (Fetch_mock.client (fun req ->
    incr requests; json "{}" req)) in
  expect_invalid (fun () -> Xrpc.Client.procedure client ~nsid:"com.example.set" ~params:[]
    ~input:(Some Jsont.string) ~input_data:None ~decoder:Jsont.json);
  let codec = Jsont.map Jsont.string ~dec:Fun.id ~enc:(fun _ -> Jsont.Error.msg Jsont.Meta.none "encoding rejected") in
  expect_invalid (fun () -> Xrpc.Client.procedure client ~nsid:"com.example.set" ~params:[]
    ~input:(Some codec) ~input_data:(Some "secret") ~decoder:Jsont.json);
  expect_invalid (fun () -> Xrpc.Client.query client ~nsid:"../outside" ~params:[] ~decoder:Jsont.json);
  check "invalid inputs cause no I/O" (!requests = 0);
  List.iter (fun service -> expect_invalid (fun () -> Xrpc.Client.of_fetch ~service
    (Fetch_mock.client (json "{}")))) ["https://example.com?query"; "https://user:secret@example.com"; "/relative"];
  let fetch = Fetch_mock.client (fun _ -> raise (Eio.Cancel.Cancelled Exit)) in
  let client = Xrpc.Client.of_fetch ~service:"https://example.com" fetch in
  (match query client with _ -> failwith "cancellation lost" | exception Eio.Cancel.Cancelled Exit -> ());
  let client = Xrpc.Client.of_fetch ~service:"https://example.com"
    (Fetch_mock.client (fun _ -> raise Exit)) in
  (match query client with _ -> failwith "caller exception lost" | exception Exit -> ());
  let client = Xrpc.Client.of_fetch ~service:"https://example.com"
    (Fetch_mock.client (fun _ -> raise (Fetch.err (Fetch.Tls_failure "test")))) in
  (match query client with _ -> failwith "network failure lost"
   | exception Eio.Io (Xrpc.Error.E (Network_error _), _) -> ());
  let client = Xrpc.Client.of_fetch ~service:"https://example.com"
    (Fetch_mock.client (fun req -> Fetch_mock.respond ~status:503 (String.make 70000 'x') req)) in
  (match query client with _ -> failwith "status lost"
   | exception Eio.Io (Xrpc.Error.E (Xrpc_error {status = 503; _}), _) -> ());
  let client = Xrpc.Client.of_fetch ~service:"https://example.com/" (Fetch_mock.client (fun req ->
    let uri = Uri.of_string (Fetch.Middleware.Url.to_string req.Fetch.Middleware.url) in
    check "canonical XRPC path" (Uri.path uri = "/xrpc/com.example.get");
    check "query value roundtrip" (Uri.get_query_param uri "q" = Some "a+b&c");
    json "{}" req)) in
  ignore (Xrpc.Client.query client ~nsid:"com.example.get" ~params:["q", "a+b&c"] ~decoder:Jsont.json)
