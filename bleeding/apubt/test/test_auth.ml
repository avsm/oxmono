let check name condition = if not condition then failwith name
let account = {|{"id":"local-id","username":"alice","acct":"alice","url":"https://example.com/@alice"}|}
let json body req = Fetch_mock.respond ~headers:(Http.Header.of_list ["Content-Type", "application/json"]) body req

let () = Eio_mock.Backend.run_full @@ fun _env ->
  List.iter (fun status ->
    let calls = ref 0 in
    let fetch = Fetch_mock.client (fun req ->
      incr calls;
      let body = match req.Fetch.Middleware.body with Fetch.String s -> s | _ -> failwith "expected form" in
      let params = match Fetch.Media.decode Fetch.Media.form body with
        | Ok pairs -> pairs | Error _ -> failwith "invalid form" in
      check "form preserves secret bytes" (List.assoc "client_secret" params = "a+b&c");
      Fetch_mock.respond ~status ~headers:(Http.Header.of_list ["Location", "https://elsewhere.example/token"]) "" req) in
    let result = Apub_mastodon_oauth.exchange_code fetch ~instance:"example.com"
        ~client_id:"client" ~client_secret:"a+b&c" ~code:"code" ~code_verifier:"verifier" in
    check "OAuth redirect rejected" (Result.is_error result);
    check "OAuth secrets not forwarded" (!calls = 1)) [301;302;303;307;308];
  let credentials = ref [] in
  let fetch = Fetch_mock.client (fun req ->
    credentials := Http.Header.get req.Fetch.Middleware.headers "authorization" :: !credentials;
    if List.length !credentials = 1 then
      Fetch_mock.respond ~status:302 ~headers:(Http.Header.of_list ["Location", "https://other.example/account"]) "" req
    else json account req) in
  check "credential verification" (Result.is_ok
    (Apub_mastodon_oauth.verify_credentials fetch ~instance:"example.com" ~access_token:"secret"));
  check "credential scope retained across GET redirects" (List.rev !credentials = [Some "Bearer secret"; None]);
  let fetch = Fetch_mock.client (fun _ -> raise (Fetch.err (Fetch.Tls_failure "test"))) in
  check "OAuth transport errors are results" (Result.is_error
    (Apub_mastodon_oauth.register_app fetch ~instance:"example.com"));
  let calls = ref 0 in
  let fetch = Fetch_mock.client (fun req ->
    incr calls;
    Fetch_mock.respond ~status:307 ~headers:(Http.Header.of_list ["Location", "https://other.example/statuses"]) "" req) in
  check "private status redirect rejected" (Result.is_error
    (Apub_mastodon_api.post_status fetch ~instance:"example.com" ~token:"secret"
       ~visibility:Private ~content:"private text" ()));
  check "private status not forwarded" (!calls = 1);
  let fetch = Fetch_mock.client (fun req -> Fetch_mock.respond "{}" req) in
  check "missing JSON Content-Type rejected" (Result.is_error
    (Apub_mastodon_oauth.register_app fetch ~instance:"example.com"))
