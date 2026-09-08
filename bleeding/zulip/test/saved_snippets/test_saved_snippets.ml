open Zulip_eio

let ok = function
  | Ok value -> value
  | Error error -> Alcotest.fail (Error.error_to_string error)

let headers = Http.Header.of_list [ ("content-type", "application/json") ]
let success = {|{"result":"success","msg":""}|}

let read_body (request : Fetch.Middleware.request) =
  match request.body with
  | Fetch.Empty -> ""
  | Fetch.String body -> body
  | Fetch.Stream { flow; _ } ->
      Eio.Buf_read.(take_all (of_flow ~max_size:65536 flow))

let form request = Httpz_media.Urlencoded.decode (read_body request)

let with_client handler run =
  Eio_mock.Backend.run_full @@ fun env ->
  let auth =
    Auth.create ~site:"https://zulip.test" ~email:"bot@zulip.test"
      ~api_key:"key"
    |> ok
  in
  let transport =
    Transport.of_fetch ~clock:env#clock (Fetch_mock.client handler)
  in
  let client = Client.create ~transport ~auth () |> ok in
  run client

let decode codec json =
  match Jsont_bytesrw.decode_string codec json with
  | Ok value -> value
  | Error message -> Alcotest.fail message

let has_member name = function
  | Jsont.Object (members, _) ->
      List.exists
        (fun ((member_name, _), _) -> String.equal name member_name)
        members
  | _ -> false

let test_list_codec () =
  let page =
    decode Saved_snippets.page_jsont
      {|{"result":"success","msg":"","saved_snippets":[{"id":17,"title":"Deploy checklist","content":"1. **Test**\n2. Ship 🚀","date_created":1681662420,"future_snippet":{"color":"violet"}}],"future_response":true}|}
  in
  Alcotest.(check int) "one saved snippet" 1 (List.length page.saved_snippets);
  let snippet = List.hd page.saved_snippets in
  Alcotest.(check int)
    "saved snippet ID" 17
    (Saved_snippets.Id.to_int (Saved_snippets.id snippet));
  Alcotest.(check string)
    "title" "Deploy checklist"
    (Saved_snippets.title snippet);
  Alcotest.(check string)
    "Markdown and Unicode content" "1. **Test**\n2. Ship 🚀"
    (Saved_snippets.content snippet);
  Alcotest.(check int)
    "creation timestamp" 1681662420
    (Saved_snippets.date_created snippet);
  Alcotest.(check bool)
    "snippet extension retained" true
    (has_member "future_snippet" (Saved_snippets.raw snippet));
  Alcotest.(check bool)
    "response extension retained" true
    (has_member "future_response" page.raw);
  let encoded =
    match Jsont_bytesrw.encode_string' Saved_snippets.page_jsont page with
    | Ok encoded -> encoded
    | Error error -> Alcotest.fail (Jsont.Error.to_string error)
  in
  let round_trip = decode Saved_snippets.page_jsont encoded in
  let snippet = List.hd round_trip.saved_snippets in
  Alcotest.(check bool)
    "snippet extension survives encoding" true
    (has_member "future_snippet" (Saved_snippets.raw snippet));
  Alcotest.(check bool)
    "response extension survives encoding" true
    (has_member "future_response" round_trip.raw)

let test_malformed_snippets () =
  let malformed =
    [
      {|{"id":1.5,"title":"x","content":"y","date_created":1}|};
      {|{"id":-1,"title":"x","content":"y","date_created":1}|};
      {|{"id":9007199254740992,"title":"x","content":"y","date_created":1}|};
      {|{"id":"1","title":"x","content":"y","date_created":1}|};
      {|{"id":1,"content":"y","date_created":1}|};
      {|{"id":1,"title":"x","content":"y","date_created":1.5}|};
    ]
  in
  List.iter
    (fun fixture ->
      match Jsont_bytesrw.decode_string Saved_snippets.jsont fixture with
      | Error _ -> ()
      | Ok _ ->
          Alcotest.failf "malformed saved snippet was accepted: %s" fixture)
    malformed;
  Alcotest.check_raises "negative local ID"
    (Invalid_argument "Zulip ID must be an exact nonnegative JSON integer")
    (fun () -> ignore (Saved_snippets.Id.of_int (-1)));
  Alcotest.check_raises "inexact local ID"
    (Invalid_argument "Zulip ID must be an exact nonnegative JSON integer")
    (fun () -> ignore (Saved_snippets.Id.of_int 9_007_199_254_740_992));
  let largest =
    decode Saved_snippets.Id.jsont "9007199254740991"
    |> Saved_snippets.Id.to_int
  in
  Alcotest.(check int) "largest exact JSON ID" 9_007_199_254_740_991 largest

let test_requests () =
  let calls = ref [] in
  let title = "Release & rollback = café 🚀" in
  let content =
    "**bold** & [runbook](https://example.test/?a=1&b=two)\n`x = y`"
  in
  with_client
    (fun request ->
      let target = Fetch.Middleware.Url.path_and_query request.url in
      calls := (request.meth, target, form request) :: !calls;
      match (request.meth, target) with
      | `POST, "/api/v1/saved_snippets" ->
          Fetch_mock.respond ~headers
            {|{"result":"success","msg":"","saved_snippet_id":23,"future_create":"kept"}|}
            request
      | _ -> Fetch_mock.respond ~headers success request)
    (fun client ->
      let created =
        Saved_snippets.create_detailed client ~title ~content |> ok
      in
      Alcotest.(check int) "created ID" 23 (Saved_snippets.Id.to_int created.id);
      Alcotest.(check bool)
        "create extension retained" true
        (has_member "future_create" created.raw);
      let id = created.id in
      Saved_snippets.edit client ~saved_snippet_id:id ~title:"Updated & renamed"
        ()
      |> ok;
      Saved_snippets.edit client ~saved_snippet_id:id
        ~content:"Only `content` changes ☕" ()
      |> ok;
      Saved_snippets.edit client ~saved_snippet_id:id () |> ok;
      Saved_snippets.delete client ~saved_snippet_id:id |> ok);
  match List.rev !calls with
  | [
   (`POST, "/api/v1/saved_snippets", create);
   (`PATCH, "/api/v1/saved_snippets/23", title_edit);
   (`PATCH, "/api/v1/saved_snippets/23", content_edit);
   (`PATCH, "/api/v1/saved_snippets/23", empty_edit);
   (`DELETE, "/api/v1/saved_snippets/23", delete);
  ] ->
      Alcotest.(check (list (pair string string)))
        "raw create form"
        [ ("title", title); ("content", content) ]
        create;
      Alcotest.(check (list (pair string string)))
        "title-only edit"
        [ ("title", "Updated & renamed") ]
        title_edit;
      Alcotest.(check (list (pair string string)))
        "content-only edit"
        [ ("content", "Only `content` changes ☕") ]
        content_edit;
      Alcotest.(check (list (pair string string)))
        "empty edit is sent" [] empty_edit;
      Alcotest.(check (list (pair string string)))
        "delete has no fields" [] delete
  | _ -> Alcotest.fail "unexpected saved-snippet request sequence"

let test_list_request () =
  with_client
    (fun request ->
      Alcotest.(check bool) "GET method" true (request.meth = `GET);
      Alcotest.(check string)
        "list path" "/api/v1/saved_snippets"
        (Fetch.Middleware.Url.path_and_query request.url);
      Fetch_mock.respond ~headers
        {|{"result":"success","msg":"","saved_snippets":[]}|} request)
    (fun client ->
      let page = Saved_snippets.list client |> ok in
      Alcotest.(check int) "empty list" 0 (List.length page.saved_snippets))

let test_api_failure () =
  with_client
    (Fetch_mock.respond ~status:400 ~headers
       {|{"result":"error","msg":"Saved snippet does not exist.","code":"BAD_REQUEST"}|})
    (fun client ->
      match
        Saved_snippets.delete client
          ~saved_snippet_id:(Saved_snippets.Id.of_int 404)
      with
      | Error (Error.Api { status = 400; code = "BAD_REQUEST"; message; _ }) ->
          Alcotest.(check string)
            "server message" "Saved snippet does not exist." message
      | Error error ->
          Alcotest.failf "unexpected error: %s" (Error.error_to_string error)
      | Ok () -> Alcotest.fail "missing saved snippet was deleted")

let () =
  Alcotest.run "saved snippets"
    [
      ( "codecs",
        [
          Alcotest.test_case "list and future fields" `Quick test_list_codec;
          Alcotest.test_case "malformed values" `Quick test_malformed_snippets;
        ] );
      ( "requests",
        [
          Alcotest.test_case "create edit delete forms" `Quick test_requests;
          Alcotest.test_case "list" `Quick test_list_request;
          Alcotest.test_case "API failure" `Quick test_api_failure;
        ] );
    ]
