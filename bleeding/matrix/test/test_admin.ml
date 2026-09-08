(** Focused wire and schema coverage for the Client-Server admin whois call. *)

module Admin = Matrix_client.Admin
module Client = Matrix_client.Client
module Id = Matrix_proto.Id
module Event = Matrix_proto.Event

let uid s = Result.get_ok (Id.User_id.of_string s)
let did s = Result.get_ok (Id.Device_id.of_string s)

let env =
  object
    method secure_random =
      Eio.Flow.string_source (String.init 4096 (fun i -> Char.chr (i land 255)))
  end

type request = { meth : string; url : string; headers : Http.Header.t }

let client response seen =
  let fetch =
    Fetch_mock.client (fun req ->
        seen :=
          {
            meth = Http.Method.to_string req.meth;
            url = Fetch.Middleware.Url.to_string req.url;
            headers = req.headers;
          }
          :: !seen;
        Fetch_mock.respond response req)
  in
  let config =
    Client.config ~homeserver:(Uriz.of_string_exn "https://hs.example") ()
  in
  Client.with_session
    (Client.create ~config ~fetch ~random:(Matrix_client.Random.of_env env))
    {
      Client.user_id = uid "@admin:example.org";
      access_token = "admin-token";
      device_id = did "ADMINDEVICE";
      refresh_token = None;
    }

let test_full_decode_and_request () =
  Eio_mock.Backend.run @@ fun () ->
  let seen = ref [] in
  let t =
    client
      {|{"user_id":"@alice:example.org","devices":{"KNOWN":{"sessions":[{"connections":[{"ip":"192.0.2.1","last_seen":1234,"user_agent":"test"},{"ip":null,"user_agent":null}]}]},"UNKNOWN-DEVICE":{"sessions":[]}}}|}
      seen
  in
  let got =
    match Admin.whois t ~user_id:(uid "@a?x:example.org") with
    | Ok x -> x
    | Error e ->
        Alcotest.failf "whois failed: %s" (Matrix_client.Error.to_string e)
  in
  Alcotest.(check (option string))
    "user id" (Some "@alice:example.org")
    (Option.map Id.User_id.to_string got.user_id);
  Alcotest.(check int) "unknown device retained" 2 (List.length got.devices);
  let known = List.assoc "KNOWN" got.devices in
  Alcotest.(check int) "session count" 1 (List.length known.sessions);
  let connections = (List.hd known.sessions).connections in
  Alcotest.(check int) "connection count" 2 (List.length connections);
  let first = List.hd connections in
  Alcotest.(check (option string)) "ip" (Some "192.0.2.1") first.ip;
  Alcotest.(check (option int64))
    "last seen" (Some 1234L)
    (Option.map Event.Timestamp.to_ms first.last_seen);
  Alcotest.(check (option string)) "agent" (Some "test") first.user_agent;
  let second = List.nth connections 1 in
  Alcotest.(check (option string)) "null ip" None second.ip;
  Alcotest.(check (option string)) "null agent" None second.user_agent;
  match List.rev !seen with
  | [ request ] ->
      Alcotest.(check string) "method" "GET" request.meth;
      Alcotest.(check string)
        "safe path"
        "https://hs.example/_matrix/client/v3/admin/whois/@a%3Fx:example.org"
        request.url;
      Alcotest.(check (option string))
        "bearer" (Some "Bearer admin-token")
        (Http.Header.get request.headers "authorization");
      Alcotest.(check (option string))
        "accept" (Some "application/json")
        (Http.Header.get request.headers "accept")
  | _ -> Alcotest.fail "expected one request"

let test_omitted_connection_metadata () =
  match
    Jsont_bytesrw.decode_string Admin.response_jsont
      {|{"devices":{"D":{"sessions":[{"connections":[{}]}]}}}|}
  with
  | Ok response ->
      let connection =
        let session = List.hd (List.assoc "D" response.devices).sessions in
        List.hd session.connections
      in
      Alcotest.(check (option string)) "omitted ip" None connection.ip;
      Alcotest.(check (option int64))
        "omitted last seen" None
        (Option.map Event.Timestamp.to_ms connection.last_seen);
      Alcotest.(check (option string))
        "omitted agent" None connection.user_agent
  | Error e -> Alcotest.failf "omitted metadata rejected: %s" e

let test_malformed_required_data () =
  let bad json =
    match Jsont_bytesrw.decode_string Admin.response_jsont json with
    | Ok _ -> Alcotest.fail "malformed required data accepted"
    | Error _ -> ()
  in
  bad {|{"user_id":42,"devices":{}}|};
  bad {|{"devices":{"D":{"sessions":[{"connections":[{"last_seen":"bad"}]}]}}}|}

let () =
  Alcotest.run "admin"
    [
      ( "whois",
        [
          Alcotest.test_case "full decode/request" `Quick
            test_full_decode_and_request;
          Alcotest.test_case "omitted connection metadata" `Quick
            test_omitted_connection_metadata;
          Alcotest.test_case "malformed required data" `Quick
            test_malformed_required_data;
        ] );
    ]
