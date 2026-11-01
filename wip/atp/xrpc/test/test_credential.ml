let check name p = if not p then failwith name
let json body req = Fetch_mock.respond ~headers:(Http.Header.of_list ["Content-Type", "application/json"]) body req
let token exp = "eyJhbGciOiJIUzI1NiJ9." ^ exp ^ ".c2lnbmF0dXJl"
let fresh = token "eyJleHAiOjEwMDAwMH0"
let expired = token "eyJleHAiOjB9"
let session access_jwt : Xrpc.Types.session = { access_jwt; refresh_jwt = "refresh";
  did = "did:plc:alice"; handle = "alice.example"; pds_uri = Some "https://example.com"; email = None;
  email_confirmed = None; email_auth_factor = None; active = None; status = None }
let query client = Xrpc.Client.query client ~nsid:"com.example.get" ~params:[] ~decoder:Jsont.json
let encode value = match Jsont_bytesrw.encode_string Xrpc.Types.session_jsont value with Ok s -> s | Error e -> failwith e

let () = Eio_mock.Backend.run_full @@ fun mock -> Eio.Switch.run @@ fun sw ->
  let env = object
    method clock = mock#clock
    method mono_clock = mock#mono_clock
    method secure_random = Eio.Flow.string_source "unused"
  end in
  let calls = ref [] in
  let fetch = Fetch_mock.client (fun req ->
    let path = Fetch.Middleware.Url.path_and_query req.Fetch.Middleware.url in
    let auth = Http.Header.get req.headers "authorization" in
    calls := (path, auth) :: !calls;
    match path with
    | "/xrpc/com.atproto.server.refreshSession" ->
        check "refresh uses refresh token" (auth = Some "Bearer refresh");
        json (encode (session fresh)) req
    | "/xrpc/com.atproto.server.deleteSession" ->
        check "logout uses refresh token" (auth = Some "Bearer refresh");
        Fetch_mock.respond ~status:204 "" req
    | _ -> check "all clients use current access token" (auth = Some ("Bearer " ^ fresh)); json "{}" req) in
  let cred = Xrpc.Credential.create ~sw ~env ~service:"https://example.com/" ~http:fetch () in
  let first = Xrpc.Credential.resume cred ~session:(session expired) () in
  let second = Xrpc.Credential.resume cred ~session:(session expired) () in
  Eio.Fiber.both (fun () -> ignore (query first)) (fun () -> ignore (query second));
  check "one refresh across clients" (List.length (List.filter (fun (path, _) -> path = "/xrpc/com.atproto.server.refreshSession") !calls) = 1);
  Xrpc.Credential.logout cred;
  check "logout clears manager" (Xrpc.Credential.get_session cred = None);
  let count = List.length !calls in
  (match query first with _ -> failwith "retained client reused token"
   | exception Eio.Io (Xrpc.Error.E Session_required, _) -> ());
  check "no network after logout" (List.length !calls = count);
  let other = { (session fresh) with pds_uri = Some "https://other.example" } in
  (match Xrpc.Credential.resume cred ~session:other () with _ -> failwith "cross-PDS resume accepted"
   | exception Invalid_argument _ -> ());
  check "missing JWT exp treated as expired" (Xrpc.Jwt.is_expired ~now:Ptime.epoch (token "e30"));
  let malformed payload = token (Jsonwt.base64url_encode payload) in
  List.iter (fun payload ->
    check "malformed JWT metadata rejected"
      (Result.is_error (Xrpc.Jwt.decode_payload (malformed payload))))
    [ {|{"exp":1,"exp":100000}|}; {|{"exp":"100000"}|};
      {|{"aud":["service",42]}|} ];
  check "mock-clock JWT expiry" (not (Xrpc.Jwt.is_expired ~now:Ptime.epoch fresh));
  let cred = Xrpc.Credential.create ~sw ~env ~service:"https://example.com" ~http:(Fetch_mock.client (fun _ -> raise (Eio.Cancel.Cancelled Exit))) () in
  ignore (Xrpc.Credential.resume cred ~session:(session fresh) ());
  (match Xrpc.Credential.logout cred with () -> failwith "logout swallowed cancellation"
   | exception Eio.Cancel.Cancelled Exit -> ());
  check "cancellation still clears local credentials" (Xrpc.Credential.get_session cred = None)
