module Id = Matrix_proto.Id
module Event = Matrix_proto.Event
module Direction = Matrix_proto.Common.Direction
module Client = Matrix_client.Client
module Relations = Matrix_client.Relations

type recorded = { meth : string; url : string }

let uid s = Result.get_ok (Id.User_id.of_string s)
let rid s = Result.get_ok (Id.Room_id.of_string s)
let eid s = Result.get_ok (Id.Event_id.of_string s)
let room_id = rid "!room:example.org"
let target = eid "$target"

let random_env =
  object
    method secure_random = Eio.Flow.string_source (String.make 4096 'r')
  end

let mock_seq responses =
  let log = ref [] in
  let remaining = ref responses in
  let fetch =
    Fetch_mock.client (fun request ->
        log :=
          {
            meth = Http.Method.to_string request.Fetch.Middleware.meth;
            url = Fetch.Middleware.Url.to_string request.url;
          }
          :: !log;
        match !remaining with
        | response :: rest ->
            remaining := rest;
            Fetch_mock.respond response request
        | [] -> Alcotest.fail "more requests than scripted responses")
  in
  (log, fetch)

let client fetch =
  let base =
    Client.create
      ~config:
        (Client.config ~homeserver:(Uriz.of_string_exn "https://hs.example") ())
      ~fetch
      ~random:(Matrix_client.Random.of_env random_env)
  in
  Client.with_session base
    {
      user_id = uid "@alice:example.org";
      access_token = "token";
      device_id = Result.get_ok (Id.Device_id.of_string "DEVICE");
      refresh_token = None;
    }

let event ?(id = "$edit") ?(sender = "@alice:example.org") ?(ts = 200)
    ?(type_ = "m.room.message") ?state_key ?unsigned content =
  Printf.sprintf
    {|{"event_id":%S,"sender":%S,"origin_server_ts":%d,"type":%S%s,"content":%s%s}|}
    id sender ts type_
    (match state_key with
    | None -> ""
    | Some key -> Printf.sprintf {|,"state_key":%S|} key)
    content
    (match unsigned with
    | None -> ""
    | Some unsigned -> Printf.sprintf {|,"unsigned":%s|} unsigned)

let message_content body =
  Printf.sprintf {|{"msgtype":"m.text","body":%S}|} body

let edit_content ?(target = "$target")
    ?(new_content = {|{"msgtype":"m.text","body":"new"}|}) () =
  Printf.sprintf
    {|{"msgtype":"m.text","body":"* new","m.new_content":%s,"m.relates_to":{"rel_type":"m.replace","event_id":%S}}|}
    new_content target

let page ?next events =
  Printf.sprintf {|{"chunk":[%s]%s}|} (String.concat "," events)
    (match next with
    | None -> ""
    | Some token -> Printf.sprintf {|,"next_batch":%S|} token)

let ids events =
  List.map
    (fun (event : Event.Raw_event.t) ->
      Option.fold ~none:"<none>" ~some:Id.Event_id.to_string event.event_id)
    events

let run f () = Eio_mock.Backend.run f

let test_exhaustive_validated_history () =
  let original = event ~id:"$target" ~ts:100 (message_content "original") in
  let late = event ~id:"$z" ~ts:300 (edit_content ()) in
  let early_a = event ~id:"$a" ~ts:200 (edit_content ()) in
  let early_b = event ~id:"$b" ~ts:200 (edit_content ()) in
  let wrong_sender =
    event ~id:"$wrong-sender" ~sender:"@mallory:example.org" (edit_content ())
  in
  let missing_new =
    event ~id:"$missing-new"
      {|{"msgtype":"m.text","body":"* x","m.relates_to":{"rel_type":"m.replace","event_id":"$target"}}|}
  in
  let scalar_new = event ~id:"$scalar" (edit_content ~new_content:"42" ()) in
  let wrong_target =
    event ~id:"$wrong-target" (edit_content ~target:"$elsewhere" ())
  in
  let state_edit = event ~id:"$state" ~state_key:"" (edit_content ()) in
  let redacted =
    event ~id:"$redacted" ~unsigned:{|{"redacted_because":{}}|}
      (edit_content ())
  in
  let wrong_type =
    event ~id:"$reaction" ~type_:"m.reaction" (edit_content ())
  in
  let log, fetch =
    mock_seq
      [
        original;
        page ~next:"page2"
          [
            late;
            wrong_sender;
            missing_new;
            scalar_new;
            wrong_target;
            state_edit;
            redacted;
            wrong_type;
          ];
        (* Repeating the token must stop rather than request a fourth page. *)
        page ~next:"page2" [ early_b; late; early_a ];
      ]
  in
  let revisions =
    Result.get_ok
      (Relations.get_edit_revisions (client fetch) ~room_id ~event_id:target ())
  in
  Alcotest.(check (list string))
    "original, then timestamp/id order"
    [ "$target"; "$a"; "$b"; "$z" ]
    (ids revisions);
  let requests = List.rev !log in
  Alcotest.(check int)
    "original plus two relation pages" 3 (List.length requests);
  Alcotest.(check string)
    "first relation page"
    "https://hs.example/_matrix/client/v1/rooms/!room:example.org/relations/$target/m.replace/m.room.message"
    (List.nth requests 1).url;
  Alcotest.(check string)
    "next relation page"
    "https://hs.example/_matrix/client/v1/rooms/!room:example.org/relations/$target/m.replace/m.room.message?from=page2"
    (List.nth requests 2).url

let test_replacement_original_is_rejected () =
  let log, fetch =
    mock_seq [ event ~id:"$target" ~ts:100 (edit_content ~target:"$older" ()) ]
  in
  let revisions =
    Result.get_ok
      (Relations.get_edit_revisions (client fetch) ~room_id ~event_id:target ())
  in
  Alcotest.(check (list string))
    "no history for an edit event" [] (ids revisions);
  Alcotest.(check int) "relations were not requested" 1 (List.length !log)

let test_reply_original_is_allowed () =
  let reply =
    {|{"msgtype":"m.text","body":"reply","m.relates_to":{"m.in_reply_to":{"event_id":"$older"}}}|}
  in
  let log, fetch = mock_seq [ event ~id:"$target" ~ts:100 reply; page [] ] in
  let revisions =
    Result.get_ok
      (Relations.get_edit_revisions (client fetch) ~room_id ~event_id:target ())
  in
  Alcotest.(check (list string))
    "reply can itself be edited" [ "$target" ] (ids revisions);
  Alcotest.(check int) "relations requested" 2 (List.length !log)

let test_raw_relation_filters () =
  let log, fetch = mock_seq [ page [] ] in
  ignore
    (Result.get_ok
       (Relations.get_raw_relations (client fetch) ~room_id ~event_id:target
          ~rel_type:Event.Rel_type.Annotation
          ~event_type:Event.Event_type.Reaction ~limit:5 ~from:"tok /" ()));
  let request = List.hd !log in
  Alcotest.(check string)
    "general raw relation path and query"
    "https://hs.example/_matrix/client/v1/rooms/!room:example.org/relations/$target/m.annotation/m.reaction?limit=5&from=tok%20/"
    request.url

let test_relation_pagination_controls () =
  let log, fetch = mock_seq [ page []; page [] ] in
  ignore
    (Result.get_ok
       (Relations.get_relations (client fetch) ~room_id ~event_id:target
          ~dir:Direction.Backward ~recurse:true ()));
  ignore
    (Result.get_ok
       (Relations.get_relations (client fetch) ~room_id ~event_id:target
          ~dir:Direction.Forward ~recurse:false ()));
  let requests = List.rev !log in
  Alcotest.(check (list string))
    "direction and recursion query controls"
    [
      "https://hs.example/_matrix/client/v1/rooms/!room:example.org/relations/$target?dir=b&recurse=true";
      "https://hs.example/_matrix/client/v1/rooms/!room:example.org/relations/$target?dir=f&recurse=false";
    ]
    (List.map (fun request -> request.url) requests)

let () =
  Alcotest.run "relations revisions"
    [
      ( "history",
        [
          Alcotest.test_case "exhaustive validation and ordering" `Quick
            (run test_exhaustive_validated_history);
          Alcotest.test_case "replacement original" `Quick
            (run test_replacement_original_is_rejected);
          Alcotest.test_case "reply original" `Quick
            (run test_reply_original_is_allowed);
          Alcotest.test_case "raw filters" `Quick
            (run test_raw_relation_filters);
          Alcotest.test_case "pagination controls" `Quick
            (run test_relation_pagination_controls);
        ] );
    ]
