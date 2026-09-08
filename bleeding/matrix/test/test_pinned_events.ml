module Ui = Matrix_ui
module Event = Matrix_proto.Event

let room_id = Matrix_proto.Id.Room_id.of_string_exn "!room:example.org"
let alice = Matrix_proto.Id.User_id.of_string_exn "@alice:example.org"

let raw json =
  match Jsont_bytesrw.decode_string Event.Raw_event.jsont json with
  | Ok event -> event
  | Error error -> Alcotest.fail error

let event id body =
  raw
    (Printf.sprintf
       {|{"event_id":"%s","sender":"@alice:example.org","origin_server_ts":1,"type":"m.room.message","content":{"msgtype":"m.text","body":"%s"}}|}
       id body)

let relation_event ~id ~target ~rel_type ~timestamp body =
  raw
    (Printf.sprintf
       {|{"event_id":"%s","sender":"@alice:example.org","origin_server_ts":%Ld,"type":"m.room.message","content":{"msgtype":"m.text","body":"%s","m.relates_to":{"rel_type":"%s","event_id":"%s"}}}|}
       id timestamp body rel_type target)

let page_json events ?next_batch () =
  let encoded =
    List.map
      (fun event ->
        Result.get_ok (Jsont_bytesrw.encode_string Event.Raw_event.jsont event))
      events
  in
  let next =
    match next_batch with
    | None -> ""
    | Some token -> Printf.sprintf ",\"next_batch\":%S" token
  in
  Printf.sprintf "{\"chunk\":[%s]%s}" (String.concat "," encoded) next

let base_state pins =
  let json =
    Printf.sprintf
      {|{"next_batch":"s1","rooms":{"join":{"!room:example.org":{"state":{"events":[{"event_id":"$pins:example.org","sender":"@alice:example.org","origin_server_ts":1,"type":"m.room.pinned_events","state_key":"","content":{"pinned":[%s]}}]},"timeline":{"events":[]}}}}}|}
      (String.concat "," (List.map (Printf.sprintf "%S") pins))
  in
  match Jsont_bytesrw.decode_string Matrix_proto.Sync.Response.jsont json with
  | Error error -> Alcotest.fail error
  | Ok response ->
      fst
        (Matrix_client.Base_client.apply
           (Matrix_client.Base_client.create ~user_id:alice ())
           response)

let client handler =
  Matrix_client.Client.create
    ~config:
      (Matrix_client.Client.config
         ~homeserver:(Uriz.of_string_exn "https://hs.example")
         ())
    ~fetch:(Fetch_mock.client handler)
    ~random:
      (Matrix_client.Random.of_source
         (Eio.Flow.string_source (String.make 1024 'r')))

let ids events =
  Array.to_list events
  |> List.filter_map (fun (event : Event.Raw_event.t) ->
      Option.map Matrix_proto.Id.Event_id.to_string event.event_id)

let is_relation_path path =
  List.exists (String.equal "relations") (String.split_on_char '/' path)

let response_with_timeline event =
  let encoded =
    Result.get_ok (Jsont_bytesrw.encode_string Event.Raw_event.jsont event)
  in
  let json =
    Printf.sprintf
      {|{"next_batch":"s1","rooms":{"join":{"!room:example.org":{"timeline":{"events":[%s]}}}}}|}
      encoded
  in
  match Jsont_bytesrw.decode_string Matrix_proto.Sync.Response.jsont json with
  | Ok response -> response
  | Error error -> Alcotest.fail error

let test_cached_missing_reorder_close () =
  Eio_main.run @@ fun _ ->
  Eio.Switch.run @@ fun _sw ->
  let cached = event "$cached:example.org" "cached" in
  let fetched = event "$fetched:example.org" "fetched" in
  let missing = event "$missing:example.org" "missing" in
  let requests = ref 0 in
  let allow_missing = ref false in
  let client =
    client (fun request ->
        let path =
          Uriz.path_decoded (Fetch.Middleware.Url.to_uri request.url)
        in
        if not (is_relation_path path) then incr requests;
        if
          String.ends_with ~suffix:"/$fetched:example.org" path
          || !allow_missing
             && String.ends_with ~suffix:"/$missing:example.org" path
        then
          Fetch_mock.respond
            (Result.get_ok
               (Jsont_bytesrw.encode_string Event.Raw_event.jsont
                  (if String.ends_with ~suffix:"/$fetched:example.org" path then
                     fetched
                   else missing)))
            request
        else
          Fetch_mock.respond ~status:404
            {|{"errcode":"M_NOT_FOUND","error":"missing"}|} request)
  in
  let cache = Ui.Event_cache.create () in
  Ui.Event_cache.prepend cache room_id ~events:[ cached ] ~prev_batch:None;
  let view = Ui.Pinned_events.create ~client ~event_cache:cache ~room_id () in
  let state =
    base_state
      [ "$cached:example.org"; "$fetched:example.org"; "$missing:example.org" ]
  in
  (* A missing event failure leaves the old list untouched and is retryable. *)
  Alcotest.(check bool)
    "failed fetch is reported" true
    (match Ui.Pinned_events.refresh view ~state with
    | Error _ -> true
    | Ok () -> false);
  Alcotest.(check (list string))
    "failure leaves list empty" []
    (ids (Ui.Pinned_events.snapshot view));
  allow_missing := true;
  Alcotest.(check bool)
    "failed suffix is retried" true
    (Result.is_ok (Ui.Pinned_events.refresh view ~state));
  Alcotest.(check (list string))
    "prefix and retried suffix resolve"
    [ "$cached:example.org"; "$fetched:example.org"; "$missing:example.org" ]
    (ids (Ui.Pinned_events.snapshot view));
  let state = base_state [ "$cached:example.org"; "$fetched:example.org" ] in
  Alcotest.(check bool)
    "cached and fetched events resolve" true
    (Result.is_ok (Ui.Pinned_events.refresh view ~state));
  Alcotest.(check (list string))
    "state order is retained"
    [ "$cached:example.org"; "$fetched:example.org" ]
    (ids (Ui.Pinned_events.snapshot view));
  let state =
    base_state
      [ "not-an-event-id"; "$fetched:example.org"; "$fetched:example.org" ]
  in
  Alcotest.(check bool)
    "duplicate state ids are harmless" true
    (Result.is_ok (Ui.Pinned_events.refresh view ~state));
  Alcotest.(check (list string))
    "duplicates are removed" [ "$fetched:example.org" ]
    (ids (Ui.Pinned_events.snapshot view));
  let state = base_state [ "$fetched:example.org"; "$cached:example.org" ] in
  Alcotest.(check bool)
    "reorder is published" true
    (Result.is_ok (Ui.Pinned_events.refresh view ~state));
  Alcotest.(check (list string))
    "reordered state is retained"
    [ "$fetched:example.org"; "$cached:example.org" ]
    (ids (Ui.Pinned_events.snapshot view));
  let state = base_state [] in
  Alcotest.(check bool)
    "unpinning clears only the projection" true
    (Result.is_ok (Ui.Pinned_events.refresh view ~state));
  let state = base_state [ "$fetched:example.org" ] in
  Alcotest.(check bool)
    "repinning reuses the shared external event" true
    (Result.is_ok (Ui.Pinned_events.refresh view ~state));
  Alcotest.(check int) "repinning does not fetch again" 3 !requests;
  Ui.Pinned_events.close view;
  Alcotest.(check (list string))
    "close clears list" []
    (ids (Ui.Pinned_events.snapshot view));
  Alcotest.(check bool)
    "refresh after close is a no-op" true
    (Result.is_ok (Ui.Pinned_events.refresh view ~state));
  Alcotest.(check int) "only unresolved suffixes were requested" 3 !requests

let test_external_registry () =
  Eio_main.run @@ fun _ ->
  Eio.Switch.run @@ fun _sw ->
  let first = event "$external:example.org" "first" in
  let replacement = event "$external:example.org" "replacement" in
  let second = event "$second:example.org" "second" in
  let third = event "$third:example.org" "third" in
  let cache = Ui.Event_cache.create ~max_events_per_room:2 () in
  Ui.Event_cache.register_external_event cache room_id
    ~event:
      (raw
         {|{"sender":"@alice:example.org","origin_server_ts":1,"type":"m.room.message","content":{"msgtype":"m.text","body":"no id"}}|});
  Alcotest.(check int)
    "events without IDs are ignored" 0
    (Array.length (Ui.Event_cache.snapshot cache room_id));
  Ui.Event_cache.register_external_event cache room_id ~event:first;
  Alcotest.(check int)
    "external event is not in the timeline" 0
    (Array.length (Ui.Event_cache.snapshot cache room_id));
  Alcotest.(check bool)
    "external event has no timeline position" true
    (Option.is_none
       (Ui.Event_cache.position cache room_id
          (Matrix_proto.Id.Event_id.of_string_exn "$external:example.org")));
  Alcotest.(check bool)
    "external event is discoverable" true
    (Option.is_some
       (Ui.Event_cache.find_event cache room_id
          (Matrix_proto.Id.Event_id.of_string_exn "$external:example.org")));
  Ui.Event_cache.register_external_event cache room_id ~event:replacement;
  let found =
    Ui.Event_cache.find_event cache room_id
      (Matrix_proto.Id.Event_id.of_string_exn "$external:example.org")
  in
  Alcotest.(check string)
    "same external ID is replaced" "replacement"
    (match found with
    | Some event -> (
        match Matrix_proto.Json.find_mem "body" event.content with
        | Some body ->
            Option.value (Matrix_proto.Json.as_string body) ~default:"?"
        | None -> "?")
    | None -> "missing");
  Ui.Event_cache.register_external_event cache room_id ~event:second;
  Ui.Event_cache.register_external_event cache room_id ~event:third;
  Alcotest.(check bool)
    "oldest external event is bounded away" true
    (Option.is_none
       (Ui.Event_cache.find_event cache room_id
          (Matrix_proto.Id.Event_id.of_string_exn "$external:example.org")));
  Alcotest.(check bool)
    "newest external events remain" true
    (Option.is_some
       (Ui.Event_cache.find_event cache room_id
          (Matrix_proto.Id.Event_id.of_string_exn "$third:example.org")));
  Ui.Event_cache.forget_room cache room_id;
  Ui.Event_cache.register_external_event cache room_id ~event:first;
  Alcotest.(check bool)
    "forget clears and tombstones external events" true
    (Option.is_none
       (Ui.Event_cache.find_event cache room_id
          (Matrix_proto.Id.Event_id.of_string_exn "$third:example.org")));
  Alcotest.(check bool)
    "late external save cannot resurrect a room" true
    (Option.is_none
       (Ui.Event_cache.find_event cache room_id
          (Matrix_proto.Id.Event_id.of_string_exn "$external:example.org")));
  let store = Ui.Event_store.memory () in
  let warm = Ui.Event_cache.create ~store () in
  Ui.Event_cache.register_external_event warm room_id ~event:first;
  let cold = Ui.Event_cache.create ~store () in
  Alcotest.(check bool)
    "external events survive a cold reload" true
    (Option.is_some
       (Ui.Event_cache.find_event cold room_id
          (Matrix_proto.Id.Event_id.of_string_exn "$external:example.org")))

let test_related_events_lookup () =
  Eio_main.run @@ fun _ ->
  Eio.Switch.run @@ fun _sw ->
  let target = Matrix_proto.Id.Event_id.of_string_exn "$target:example.org" in
  let annotation =
    relation_event ~id:"$annotation:example.org" ~target:"$target:example.org"
      ~rel_type:"m.annotation" ~timestamp:3L "physical"
  in
  let duplicate =
    relation_event ~id:"$annotation:example.org" ~target:"$target:example.org"
      ~rel_type:"m.annotation" ~timestamp:3L "detached"
  in
  let replacement =
    relation_event ~id:"$replace:example.org" ~target:"$target:example.org"
      ~rel_type:"m.replace" ~timestamp:2L "replacement"
  in
  let unrelated =
    relation_event ~id:"$other:example.org" ~target:"$other-target:example.org"
      ~rel_type:"m.annotation" ~timestamp:1L "other"
  in
  let cache = Ui.Event_cache.create () in
  Ui.Event_cache.register_external_event cache room_id ~event:duplicate;
  Ui.Event_cache.register_external_event cache room_id ~event:replacement;
  Ui.Event_cache.register_external_event cache room_id ~event:unrelated;
  Ui.Event_cache.prepend cache room_id ~events:[ annotation ] ~prev_batch:None;
  let all = Ui.Event_cache.related_events cache room_id ~target () in
  Alcotest.(check (list string))
    "relations are chronological, filtered and deduplicated"
    [ "$replace:example.org"; "$annotation:example.org" ]
    (ids (Array.of_list all));
  Alcotest.(check string)
    "physical relation wins over detached duplicate" "physical"
    (match List.rev all with
    | event :: _ -> (
        match Matrix_proto.Json.find_mem "body" event.content with
        | Some json ->
            Option.value (Matrix_proto.Json.as_string json) ~default:"?"
        | None -> "?")
    | [] -> "?");
  let annotations =
    Ui.Event_cache.related_events cache room_id ~target
      ~rel_type:Event.Rel_type.Annotation ()
  in
  Alcotest.(check (list string))
    "relation type narrows results"
    [ "$annotation:example.org" ]
    (ids (Array.of_list annotations))

let test_pinned_relation_pages () =
  Eio_main.run @@ fun _ ->
  Eio.Switch.run @@ fun _sw ->
  let target = event "$target:example.org" "target" in
  let annotation =
    relation_event ~id:"$annotation:example.org" ~target:"$target:example.org"
      ~rel_type:"m.annotation" ~timestamp:2L "reaction"
  in
  let replacement =
    relation_event ~id:"$replace:example.org" ~target:"$target:example.org"
      ~rel_type:"m.replace" ~timestamp:3L "edit"
  in
  let thread_reply =
    relation_event ~id:"$thread:example.org" ~target:"$target:example.org"
      ~rel_type:"m.thread" ~timestamp:4L "thread"
  in
  let relation_requests = ref [] in
  let client =
    client (fun request ->
        let uri =
          Uriz.of_string_exn (Fetch.Middleware.Url.to_string request.url)
        in
        let path = Uriz.path_decoded uri in
        let endpoint = List.hd (String.split_on_char '?' path) in
        if is_relation_path endpoint then begin
          let query = Uriz.query_params ~plus_as_space:true uri in
          relation_requests :=
            ( Option.join (List.assoc_opt "from" query),
              Option.join (List.assoc_opt "dir" query),
              Option.join (List.assoc_opt "recurse" query) )
            :: !relation_requests;
          let page =
            match Uriz.find_query ~plus_as_space:true uri "from" with
            | Null -> page_json [ annotation ] ~next_batch:"a2" ()
            | This "a2" ->
                page_json [ replacement; thread_reply ] ~next_batch:"a2" ()
            | This _ -> Alcotest.fail "unexpected relation pagination token"
          in
          Fetch_mock.respond page request
        end
        else if String.ends_with ~suffix:"/$target:example.org" endpoint then
          Fetch_mock.respond
            (Result.get_ok
               (Jsont_bytesrw.encode_string Event.Raw_event.jsont target))
            request
        else
          Fetch_mock.respond ~status:404
            {|{"errcode":"M_NOT_FOUND","error":"missing"}|} request)
  in
  let cache = Ui.Event_cache.create () in
  let view = Ui.Pinned_events.create ~client ~event_cache:cache ~room_id () in
  Alcotest.(check bool)
    "target and relations refresh" true
    (Result.is_ok
       (Ui.Pinned_events.refresh view
          ~state:(base_state [ "$target:example.org" ])));
  Alcotest.(check (list string))
    "target is projected" [ "$target:example.org" ]
    (ids (Ui.Pinned_events.snapshot view));
  Alcotest.(check (list string))
    "relations are registered in shared cache"
    [ "$annotation:example.org"; "$replace:example.org" ]
    (ids
       (Array.of_list
          (Ui.Event_cache.related_events cache room_id
             ~target:
               (Matrix_proto.Id.Event_id.of_string_exn "$target:example.org")
             ())));
  Alcotest.(
    check (list (triple (option string) (option string) (option string))))
    "relation pages carry recursion and stop on a token cycle"
    [ (None, Some "b", Some "true"); (Some "a2", Some "b", Some "true") ]
    (List.rev !relation_requests)

let test_stale_refresh_after_forget () =
  Eio_main.run @@ fun _ ->
  Eio.Switch.run @@ fun sw ->
  let started, started_u = Eio.Promise.create () in
  let release, release_u = Eio.Promise.create () in
  let fetched = event "$late:example.org" "late" in
  let client =
    client (fun request ->
        let path = Fetch.Middleware.Url.path_and_query request.url in
        if String.ends_with ~suffix:"/$late:example.org" path then begin
          Eio.Promise.resolve started_u ();
          Eio.Promise.await release;
          Fetch_mock.respond
            (Result.get_ok
               (Jsont_bytesrw.encode_string Event.Raw_event.jsont fetched))
            request
        end
        else Fetch_mock.respond (page_json [] ()) request)
  in
  let cache = Ui.Event_cache.create () in
  let view = Ui.Pinned_events.create ~client ~event_cache:cache ~room_id () in
  let finished, finished_u = Eio.Promise.create () in
  Eio.Fiber.fork ~sw (fun () ->
      ignore
        (Ui.Pinned_events.refresh view
           ~state:(base_state [ "$late:example.org" ]));
      Eio.Promise.resolve finished_u ());
  Eio.Promise.await started;
  Ui.Event_cache.forget_room cache room_id;
  Eio.Promise.resolve release_u ();
  Eio.Promise.await finished;
  Alcotest.(check (list string))
    "late refresh cannot republish" []
    (ids (Ui.Pinned_events.snapshot view));
  Alcotest.(check bool)
    "late target cannot resurrect forgotten cache" true
    (Option.is_none
       (Ui.Event_cache.find_event cache room_id
          (Matrix_proto.Id.Event_id.of_string_exn "$late:example.org")))

let test_stale_refresh_after_close () =
  Eio_main.run @@ fun _ ->
  Eio.Switch.run @@ fun sw ->
  let started, started_u = Eio.Promise.create () in
  let release, release_u = Eio.Promise.create () in
  let fetched = event "$late-close:example.org" "late" in
  let client =
    client (fun request ->
        let path = Fetch.Middleware.Url.path_and_query request.url in
        if String.ends_with ~suffix:"/$late-close:example.org" path then begin
          Eio.Promise.resolve started_u ();
          Eio.Promise.await release;
          Fetch_mock.respond
            (Result.get_ok
               (Jsont_bytesrw.encode_string Event.Raw_event.jsont fetched))
            request
        end
        else Fetch_mock.respond (page_json [] ()) request)
  in
  let cache = Ui.Event_cache.create () in
  let view = Ui.Pinned_events.create ~client ~event_cache:cache ~room_id () in
  let finished, finished_u = Eio.Promise.create () in
  Eio.Fiber.fork ~sw (fun () ->
      ignore
        (Ui.Pinned_events.refresh view
           ~state:(base_state [ "$late-close:example.org" ]));
      Eio.Promise.resolve finished_u ());
  Eio.Promise.await started;
  Ui.Pinned_events.close view;
  Eio.Promise.resolve release_u ();
  Eio.Promise.await finished;
  Alcotest.(check (list string))
    "late close cannot republish" []
    (ids (Ui.Pinned_events.snapshot view));
  Alcotest.(check bool)
    "late close cannot repopulate detached cache" true
    (Option.is_none
       (Ui.Event_cache.find_event cache room_id
          (Matrix_proto.Id.Event_id.of_string_exn "$late-close:example.org")))

let test_external_promotion_and_decryption () =
  Eio_main.run @@ fun _ ->
  Eio.Switch.run @@ fun _sw ->
  let fetched = event "$promote:example.org" "fetched" in
  let synced = event "$promote:example.org" "synced" in
  let cache = Ui.Event_cache.create () in
  Ui.Event_cache.register_external_event cache room_id ~event:fetched;
  Ui.Event_cache.apply_room_change cache
    (let state, changes =
       Matrix_client.Base_client.apply
         (Matrix_client.Base_client.create ~user_id:alice ())
         (response_with_timeline synced)
     in
     ignore state;
     match changes.room_changes with
     | [ change ] -> change
     | _ -> Alcotest.fail "expected one room change");
  Alcotest.(check int)
    "sync promotion inserts one timeline event" 1
    (Array.length (Ui.Event_cache.snapshot cache room_id));
  let promoted =
    Ui.Event_cache.find_event cache room_id
      (Matrix_proto.Id.Event_id.of_string_exn "$promote:example.org")
  in
  Alcotest.(check bool)
    "sync copy wins over external copy" true
    (match promoted with
    | Some event ->
        Option.equal String.equal
          (Option.bind
             (Matrix_proto.Json.find_mem "body" event.content)
             Matrix_proto.Json.as_string)
          (Some "synced")
    | None -> false);
  let paged_cache = Ui.Event_cache.create () in
  Ui.Event_cache.register_external_event paged_cache room_id ~event:fetched;
  Ui.Event_cache.prepend paged_cache room_id ~events:[ synced ] ~prev_batch:None;
  Alcotest.(check int)
    "pagination promotion inserts one timeline event" 1
    (Array.length (Ui.Event_cache.snapshot paged_cache room_id));
  Alcotest.(check bool)
    "pagination promotes the replacement copy" true
    (match
       Ui.Event_cache.find_event paged_cache room_id
         (Matrix_proto.Id.Event_id.of_string_exn "$promote:example.org")
     with
    | Some event ->
        Option.equal String.equal
          (Option.bind
             (Matrix_proto.Json.find_mem "body" event.content)
             Matrix_proto.Json.as_string)
          (Some "synced")
    | None -> false);
  let encrypted_wire =
    raw
      {|{"event_id":"$promote-plain:example.org","sender":"@alice:example.org","origin_server_ts":1,"type":"m.room.encrypted","content":{"algorithm":"m.megolm.v1.aes-sha2","ciphertext":"ciphertext","device_id":"ALICE","session_id":"session"}}|}
  in
  let plaintext = event "$promote-plain:example.org" "retained plaintext" in
  let retained_store =
    Ui.Event_store.memory ~plaintext_policy:Ui.Event_store.Store_plaintext ()
  in
  let retained_cache = Ui.Event_cache.create ~store:retained_store () in
  Ui.Event_cache.register_external_event retained_cache room_id
    ~event:encrypted_wire;
  Alcotest.(check bool)
    "detached plaintext is installed" true
    (Ui.Event_cache.set_decrypted retained_cache room_id
       ~encrypted:encrypted_wire ~plaintext);
  Ui.Event_cache.apply_room_change retained_cache
    (let _, changes =
       Matrix_client.Base_client.apply
         (Matrix_client.Base_client.create ~user_id:alice ())
         (response_with_timeline encrypted_wire)
     in
     match changes.room_changes with
     | [ change ] -> change
     | _ -> Alcotest.fail "expected one room change");
  Alcotest.(check string)
    "promotion retains detached plaintext" "retained plaintext"
    (match
       Ui.Event_cache.find_event retained_cache room_id
         (Matrix_proto.Id.Event_id.of_string_exn "$promote-plain:example.org")
     with
    | Some event -> (
        match Matrix_proto.Json.find_mem "body" event.content with
        | Some body ->
            Option.value (Matrix_proto.Json.as_string body) ~default:"?"
        | None -> "?")
    | None -> "missing");
  let stored =
    Result.get_ok (Ui.Event_store.load_room retained_store room_id)
    |> Option.get
  in
  Alcotest.(check int)
    "promotion removes the persisted detached copy" 0
    (List.length stored.external_events);
  let cold = Ui.Event_cache.create ~store:retained_store () in
  Alcotest.(check string)
    "promoted plaintext survives a cold reload" "retained plaintext"
    (match
       Ui.Event_cache.find_event cold room_id
         (Matrix_proto.Id.Event_id.of_string_exn "$promote-plain:example.org")
     with
    | Some event -> (
        match Matrix_proto.Json.find_mem "body" event.content with
        | Some body ->
            Option.value (Matrix_proto.Json.as_string body) ~default:"?"
        | None -> "?")
    | None -> "missing");
  let encrypted =
    raw
      {|{"event_id":"$encrypted:example.org","sender":"@alice:example.org","origin_server_ts":1,"type":"m.room.encrypted","content":{"algorithm":"m.megolm.v1.aes-sha2","ciphertext":"ciphertext","device_id":"ALICE","session_id":"session"}}|}
  in
  let plaintext = event "$encrypted:example.org" "plaintext" in
  let encrypted_cache = Ui.Event_cache.create () in
  Ui.Event_cache.register_external_event encrypted_cache room_id
    ~event:encrypted;
  Alcotest.(check int)
    "external ciphertext is retryable" 1
    (List.length (Ui.Event_cache.undecrypted encrypted_cache room_id));
  Alcotest.(check bool)
    "external ciphertext decrypts in place" true
    (Ui.Event_cache.set_decrypted encrypted_cache room_id ~encrypted ~plaintext);
  Alcotest.(check int)
    "decrypted external event is no longer pending" 0
    (List.length (Ui.Event_cache.undecrypted encrypted_cache room_id));
  Alcotest.(check bool)
    "decrypted external event is discoverable" true
    (match
       Ui.Event_cache.find_event encrypted_cache room_id
         (Matrix_proto.Id.Event_id.of_string_exn "$encrypted:example.org")
     with
    | Some event ->
        Option.equal String.equal
          (Option.bind
             (Matrix_proto.Json.find_mem "body" event.content)
             Matrix_proto.Json.as_string)
          (Some "plaintext")
    | None -> false)

let test_external_redaction_and_room_safety () =
  Eio_main.run @@ fun _ ->
  Eio.Switch.run @@ fun _sw ->
  let encrypted =
    raw
      {|{"event_id":"$redacted:example.org","room_id":"!room:example.org","sender":"@alice:example.org","origin_server_ts":1,"type":"m.room.encrypted","content":{"algorithm":"m.megolm.v1.aes-sha2","ciphertext":"ciphertext","device_id":"ALICE","session_id":"session"}}|}
  in
  let plaintext = event "$redacted:example.org" "secret" in
  let redacted =
    raw
      {|{"event_id":"$redacted:example.org","room_id":"!room:example.org","sender":"@alice:example.org","origin_server_ts":1,"type":"m.room.encrypted","content":{"algorithm":"m.megolm.v1.aes-sha2"},"unsigned":{"redacted_because":{}}}|}
  in
  let cache = Ui.Event_cache.create () in
  Ui.Event_cache.register_external_event cache room_id ~event:encrypted;
  Alcotest.(check bool)
    "external event decrypts before redaction" true
    (Ui.Event_cache.set_decrypted cache room_id ~encrypted ~plaintext);
  let changed = event "$redacted:example.org" "changed" in
  Ui.Event_cache.register_external_event cache room_id ~event:changed;
  Alcotest.(check string)
    "changed external wire drops stale plaintext" "changed"
    (match
       Ui.Event_cache.find_event cache room_id
         (Matrix_proto.Id.Event_id.of_string_exn "$redacted:example.org")
     with
    | Some event -> (
        match Matrix_proto.Json.find_mem "body" event.content with
        | Some body ->
            Option.value (Matrix_proto.Json.as_string body) ~default:"?"
        | None -> "?")
    | None -> "missing");
  Ui.Event_cache.register_external_event cache room_id ~event:redacted;
  Alcotest.(check bool)
    "redacted replacement drops plaintext" true
    (match
       Ui.Event_cache.find_event cache room_id
         (Matrix_proto.Id.Event_id.of_string_exn "$redacted:example.org")
     with
    | Some event ->
        Event.Event_type.equal event.type_
          Event.Event_type.Room_message_encrypted
    | None -> false);
  Alcotest.(check bool)
    "redacted external event cannot be decrypted" false
    (Ui.Event_cache.set_decrypted cache room_id ~encrypted:redacted ~plaintext);
  Alcotest.(check int)
    "redacted external event is not retryable" 0
    (List.length (Ui.Event_cache.undecrypted cache room_id));
  let wrong_room = Matrix_proto.Id.Room_id.of_string_exn "!other:example.org" in
  let wrong =
    {
      encrypted with
      Event.Raw_event.event_id =
        Some (Matrix_proto.Id.Event_id.of_string_exn "$wrong:example.org");
      room_id = Some wrong_room;
    }
  in
  Ui.Event_cache.register_external_event cache room_id ~event:wrong;
  Alcotest.(check bool)
    "conflicting room ID is ignored" true
    (Option.is_none
       (Ui.Event_cache.find_event cache room_id
          (Matrix_proto.Id.Event_id.of_string_exn "$wrong:example.org")));
  let wrong_client =
    client (fun request ->
        Fetch_mock.respond
          (Result.get_ok
             (Jsont_bytesrw.encode_string Event.Raw_event.jsont wrong))
          request)
  in
  let wrong_view =
    Ui.Pinned_events.create ~client:wrong_client
      ~event_cache:(Ui.Event_cache.create ()) ~room_id ()
  in
  Alcotest.(check bool)
    "pinned fetch rejects conflicting room ID" true
    (match
       Ui.Pinned_events.refresh wrong_view
         ~state:(base_state [ "$wrong:example.org" ])
     with
    | Error (Matrix_client.Error.Json_error _) -> true
    | _ -> false)

let test_forget_closes_standalone_view () =
  Eio_main.run @@ fun _ ->
  Eio.Switch.run @@ fun _sw ->
  let fetched = event "$forget:example.org" "forget" in
  let requests = ref 0 in
  let client =
    client (fun request ->
        if
          not
            (is_relation_path (Fetch.Middleware.Url.path_and_query request.url))
        then incr requests;
        Fetch_mock.respond
          (Result.get_ok
             (Jsont_bytesrw.encode_string Event.Raw_event.jsont fetched))
          request)
  in
  let cache = Ui.Event_cache.create () in
  let view = Ui.Pinned_events.create ~client ~event_cache:cache ~room_id () in
  let state = base_state [ "$forget:example.org" ] in
  Alcotest.(check bool)
    "standalone pinned view loads" true
    (Result.is_ok (Ui.Pinned_events.refresh view ~state));
  Ui.Event_cache.forget_room cache room_id;
  Alcotest.(check (list string))
    "forget clears standalone view" []
    (ids (Ui.Pinned_events.snapshot view));
  Alcotest.(check bool)
    "forget closes standalone view" true
    (Result.is_ok (Ui.Pinned_events.refresh view ~state));
  Alcotest.(check int) "closed view does not refetch" 1 !requests;
  let notified = ref false in
  let _unsubscribe =
    Ui.Event_cache.subscribe_forget_room cache room_id (fun () ->
        notified := true)
  in
  let _unsubscribe_broken =
    Ui.Event_cache.subscribe_forget_room cache room_id (fun () ->
        failwith "broken immediate forget listener")
  in
  Alcotest.(check bool)
    "an already-forgotten room notifies immediately" true !notified;
  let late_view =
    Ui.Pinned_events.create ~client ~event_cache:cache ~room_id ()
  in
  Alcotest.(check bool)
    "a view created after forget is closed" true
    (Result.is_ok (Ui.Pinned_events.refresh late_view ~state));
  Alcotest.(check int) "a late view does not fetch" 1 !requests

let test_forget_listener_isolation () =
  Eio_main.run @@ fun _ ->
  Eio.Switch.run @@ fun _sw ->
  let cache = Ui.Event_cache.create () in
  let notified = ref 0 in
  let _unsubscribe_notified =
    Ui.Event_cache.subscribe_forget_room cache room_id (fun () -> incr notified)
  in
  let _unsubscribe_broken =
    Ui.Event_cache.subscribe_forget_room cache room_id (fun () ->
        failwith "broken forget listener")
  in
  Ui.Event_cache.forget_room cache room_id;
  Alcotest.(check int)
    "a broken listener does not hide forget from later listeners" 1 !notified

let test_max_events_to_load () =
  Eio_main.run @@ fun _ ->
  Eio.Switch.run @@ fun _sw ->
  let all_ids =
    List.init 130 (fun index -> Printf.sprintf "$pin%03d:example.org" index)
  in
  let requests = ref [] in
  let client =
    client (fun request ->
        if is_relation_path (Fetch.Middleware.Url.path_and_query request.url)
        then Fetch_mock.respond (page_json [] ()) request
        else
          let segments = Fetch.Middleware.Url.path_segments request.url in
          let id = List.hd (List.rev segments) in
          requests := id :: !requests;
          Fetch_mock.respond
            (Result.get_ok
               (Jsont_bytesrw.encode_string Event.Raw_event.jsont (event id id)))
            request)
  in
  let view =
    Ui.Pinned_events.create ~max_events_to_load:128 ~client
      ~event_cache:(Ui.Event_cache.create ()) ~room_id ()
  in
  Alcotest.(check bool)
    "negative limit is rejected" true
    (try
       ignore
         (Ui.Pinned_events.create ~max_events_to_load:(-1) ~client
            ~event_cache:(Ui.Event_cache.create ()) ~room_id ());
       false
     with Invalid_argument _ -> true);
  Alcotest.(check bool)
    "tail refresh succeeds" true
    (Result.is_ok (Ui.Pinned_events.refresh view ~state:(base_state all_ids)));
  let expected = List.filteri (fun index _ -> index >= 2) all_ids in
  Alcotest.(check (list string))
    "newest tail is retained" expected
    (ids (Ui.Pinned_events.snapshot view));
  Alcotest.(check int)
    "only the configured tail was fetched" 128 (List.length !requests)

let () =
  Alcotest.run "pinned-events"
    [
      ( "pinned events",
        [
          Alcotest.test_case "cache, fetch, reorder and close" `Quick
            test_cached_missing_reorder_close;
          Alcotest.test_case "configured loading limit" `Quick
            test_max_events_to_load;
          Alcotest.test_case "external event registry" `Quick
            test_external_registry;
          Alcotest.test_case "external promotion and decryption" `Quick
            test_external_promotion_and_decryption;
          Alcotest.test_case "external redaction and room safety" `Quick
            test_external_redaction_and_room_safety;
          Alcotest.test_case "related event lookup" `Quick
            test_related_events_lookup;
          Alcotest.test_case "pinned relation pages" `Quick
            test_pinned_relation_pages;
          Alcotest.test_case "forget closes standalone view" `Quick
            test_forget_closes_standalone_view;
          Alcotest.test_case "forget listener isolation" `Quick
            test_forget_listener_isolation;
          Alcotest.test_case "stale refresh after forget" `Quick
            test_stale_refresh_after_forget;
          Alcotest.test_case "stale refresh after close" `Quick
            test_stale_refresh_after_close;
        ] );
    ]
