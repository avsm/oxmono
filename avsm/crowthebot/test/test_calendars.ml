open Crowthebot
module R = Jmap_eio.Calendars
module S = Calendar_store

let admin = "@admin:example.test"
let friend = "@friend:example.test"
let room = "!calendar:example.test"
let check name value = if not value then failwith name
let encode = Jmap_eio.Codec.encode_exn Jsont.json
let decode = Jmap_eio.Codec.decode_exn Jsont.json

let obj fields =
  Jsont.Json.object' (List.map (fun (k, v) -> ((k, Jsont.Meta.none), v)) fields)

let field name json =
  match json with
  | Jsont.Object (fields, _) ->
      List.assoc name (List.map (fun ((k, _), v) -> (k, v)) fields)
  | _ -> failwith "expected object"

let str = function Jsont.String (s, _) -> s | _ -> failwith "expected string"
let int json = Result.get_ok (Jsont.Json.decode Jsont.int json)
let array = function Jsont.Array (v, _) -> v | _ -> failwith "expected array"
let get_ok = function Ok v -> v | Error e -> failwith e

let reject f =
  try
    ignore (f ());
    false
  with Invalid_argument _ -> true

let scalar db sql =
  let stmt = Sqlite3_eio.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3_eio.finalize db stmt))
    (fun () ->
      ignore (Sqlite3_eio.step db stmt);
      Sqlite3.column_int stmt 0)

let session =
  {|{
 "capabilities":{"urn:ietf:params:jmap:core":{"maxSizeUpload":1000,"maxConcurrentUpload":1,"maxSizeRequest":100000,"maxConcurrentRequests":1,"maxCallsInRequest":4,"maxObjectsInGet":10,"maxObjectsInSet":10,"collationAlgorithms":[]},"urn:ietf:params:jmap:calendars":{}},
 "accounts":{"a":{"name":"Calendar","isPersonal":true,"isReadOnly":false,"accountCapabilities":{"urn:ietf:params:jmap:calendars":{}}}},
 "primaryAccounts":{"urn:ietf:params:jmap:calendars":"a"},"username":"owner",
 "apiUrl":"https://example.test/api","downloadUrl":"https://example.test/download/{accountId}/{blobId}/{name}?type={type}",
 "uploadUrl":"https://example.test/upload/{accountId}","eventSourceUrl":"https://example.test/events?types={types}&close={closeafter}&ping={ping}","state":"session1"}|}

let () =
  Eio_main.run @@ fun env ->
  let filename = Filename.temp_file "crow-calendar" ".sqlite3" in
  Fun.protect ~finally:(fun () -> Sys.remove filename) @@ fun () ->
  let path = Eio.Path.(Eio.Stdenv.fs env / filename) in
  let clock = ref 0.
  and reads = ref []
  and delta = ref false
  and expired = ref false
  and fail_get = ref false
  and query_changed = ref false in
  let notes =
    String.make 2047 'a' ^ "🤖 Telescope "
    ^ String.concat "" (List.init 1000 (fun _ -> "\"\\\t\n"))
  in
  let original =
    "{ \
     \"id\":\"e1\",\"uid\":\"u1\",\"title\":\"Office\",\"x-number\":9007199254740993,\"description\":"
    ^ encode (Jsont.Json.string notes)
    ^ ",\"recurrenceRules\":[{\"frequency\":\"daily\"}],\"recurrenceOverrides\":{\"2026-09-11T10:00:00\":{\"title\":\"Observatory\"}},\"links\":{\"a\":{\"rel\":\"enclosure\",\"blobId\":\"b1\"},\"external\":{\"href\":\"https://untrusted.test/attachment\"}} \
       }"
  in
  let headers = Http.Header.of_list [ ("content-type", "application/json") ] in
  let json body request = Fetch_mock.respond ~headers body request in
  let response name arguments request =
    json
      (Printf.sprintf
         {|{"methodResponses":[[%S,%s,"c0"]],"sessionState":"session1"}|} name
         arguments)
      request
  in
  let fetch =
    Fetch_mock.client (fun request ->
        let url = Fetch.Middleware.Url.to_string request.url in
        reads := url :: !reads;
        check "same origin only"
          (String.starts_with ~prefix:"https://example.test/" url);
        if String.starts_with ~prefix:"https://example.test/download/" url then
          Fetch_mock.respond "attachment\000bytes" request
        else if request.meth = `GET then json session request
        else begin
          check "only POST and GET" (request.meth = `POST);
          let value =
            match request.body with
            | Fetch.String s -> decode s
            | _ -> assert false
          in
          let name, args =
            match array (field "methodCalls" value) with
            | [ call ] -> (
                match array call with
                | [ name; args; _ ] -> (str name, args)
                | _ -> assert false)
            | _ -> assert false
          in
          check "account scoped" (str (field "accountId" args) = "a");
          check "remote writes impossible"
            (List.mem name
               [
                 "Calendar/get";
                 "Calendar/changes";
                 "ParticipantIdentity/get";
                 "ParticipantIdentity/changes";
                 "CalendarEvent/get";
                 "CalendarEvent/query";
                 "CalendarEvent/changes";
               ]);
          if name = "CalendarEvent/query" then begin
            let position =
              try int (field "position" args) with Not_found -> 0
            in
            let ids =
              if position = 0 then "\"e1\""
              else if position = 1 && not !delta then "\"e2\""
              else ""
            in
            response name
              (Printf.sprintf
                 {|{"accountId":"a","queryState":%S,"canCalculateChanges":true,"position":%d,"ids":[%s],"total":%d}|}
                 (if !query_changed then "q2" else "q1")
                 position ids
                 (if !delta then 1 else 2))
              request
          end
          else if String.ends_with ~suffix:"/changes" name then begin
            let since = str (field "sinceState" args) in
            if name = "CalendarEvent/changes" && !expired then begin
              expired := false;
              response "error" {|{"type":"cannotCalculateChanges"}|} request
            end
            else
              let changing =
                name = "CalendarEvent/changes" && !delta && since <> "s2"
              in
              response name
                (Printf.sprintf
                   {|{"accountId":"a","oldState":%S,"newState":%S,"hasMoreChanges":false,"created":[],"updated":[%s],"destroyed":[%s]}|}
                   since
                   (if changing then "s2" else since)
                   (if changing then "\"e1\"" else "")
                   (if changing then "\"e2\"" else ""))
                request
          end
          else begin
            if name = "CalendarEvent/get" && !fail_get then
              failwith "simulated failure, secret token-hidden";
            let ids =
              match
                try field "ids" args with Not_found -> Jsont.Json.null ()
              with
              | Jsont.Null _ -> None
              | value -> Some (List.map str (array value))
            in
            let ical =
              try
                ignore (field "properties" args);
                true
              with Not_found -> false
            in
            let objects =
              match (name, ids) with
              | "Calendar/get", None ->
                  [
                    {|{"id":"c1","name":"Personal","timeZone":"Europe/London"}|};
                  ]
              | "ParticipantIdentity/get", None ->
                  [
                    {|{"id":"p1","name":"Owner","sendTo":{"imip":"mailto:owner@example.test"}}|};
                  ]
              | "CalendarEvent/get", Some ids ->
                  List.map
                    (function
                      | "e1" when ical ->
                          {|{"id":"e1","iCalendar":{"@type":"ICalComponent","name":"vevent"}}|}
                      | "e2" when ical ->
                          {|{"id":"e2","iCalendar":{"@type":"ICalComponent","name":"vevent"}}|}
                      | "e1" when !delta ->
                          {|{"id":"e1","title":"Revised rendezvous","uid":"u1","links":{"a":{"blobId":"b1"}}}|}
                      | "e1" -> original
                      | "e2" -> {|{"id":"e2","title":"Lunch","uid":"u2"}|}
                      | _ -> assert false)
                    ids
              | _ -> assert false
            in
            let state =
              if name = "CalendarEvent/get" && ids <> Some [] then "get-future"
              else "s1"
            in
            response name
              (Printf.sprintf
                 {|{"accountId":"a","state":%S,"list":[%s],"notFound":[]}|}
                 state
                 (String.concat "," objects))
              request
          end
        end)
  in
  let open_store sw =
    let db = Sqlite3_eio.open_path ~sw path in
    let store = Store.create ~now:(fun () -> !clock) db ~admin in
    Store.add_room store room;
    let source =
      Calendar_source.initialize ~sw ~fetch ~clock:(Eio.Stdenv.clock env)
        (decode
           {|{"url":"https://example.test/session","token":"token-hidden","account":null,"max_bytes":1048576}|})
    in
    let calendars =
      Calendars.create ~state:(Store.calendars store)
        ~sources:[ ("personal", source) ]
        ~default:(Some "personal")
    in
    let invoke actor name args =
      Calendars.invoke
        (Calendars.for_request calendars ~actor ~room ~event:"$origin")
        name args
    in
    (db, store, calendars, invoke)
  in
  Eio.Switch.run (fun sw ->
      let _, store, calendars, invoke = open_store sw in
      let state = Store.calendars store in
      let before = List.length !reads in
      check "unapproved users cannot inspect or initiate reads"
        (Result.is_error
           (invoke "@stranger:example.test" "calendar_sources" "{}")
        && List.length !reads = before);
      ignore (get_ok (invoke admin "calendar_sync" "{}"));
      let event_cursor () =
        List.find
          (fun (c : S.cursor) -> c.kind = R.Event)
          (S.cursors state ~actor:admin 1)
      in
      check "initial anchor persisted"
        ((event_cursor ()).phase = "listing"
        && (event_cursor ()).state = Some "s1");
      ignore (get_ok (Calendars.poll calendars ~actor:admin 1));
      check "short server page is persisted" ((event_cursor ()).position = 1);
      check "partial calendar hidden"
        (S.search state ~actor:admin 1 ~kind:R.Event ~query:"" ~after:0 = []);
      check "one persistent job per connection"
        (List.length (Store.reminders store ~actor:admin) = 1));
  clock := 120.;
  Eio.Switch.run (fun sw ->
      let db, store, calendars, invoke = open_store sw in
      let state = Store.calendars store in
      let config =
        Config.default ~admin ~homeserver:"https://matrix.example.test"
      in
      let engine =
        Engine.create ~config ~store ~self:"@crow:example.test" ~plugins:[]
          ~complete:(fun _ _ -> failwith "mechanical sync must not call model")
          ~now:(fun () -> !clock)
        |> fun engine -> Engine.with_calendars engine calendars
      in
      let fire job ~run_id =
        Engine.fire engine
          ~send:(fun _ -> failwith "mechanical sync must not send")
          job ~run_id
      in
      Cron.run_due store ~fire;
      let event_cursor () =
        List.find
          (fun (c : S.cursor) -> c.kind = R.Event)
          (S.cursors state ~actor:admin 1)
      in
      check "restart continues next page"
        ((event_cursor ()).phase = "catchup" && (event_cursor ()).position = 2);
      clock := 180.;
      Cron.run_due store ~fire;
      check "catchup publishes snapshot"
        ((event_cursor ()).phase = "live" && (event_cursor ()).state = Some "s1");
      check "blobs mirrored mechanically"
        (S.blob_counts state ~actor:admin 1 = (1, 1));
      Store.set_person store ~actor:admin ~user:friend ~role:Friend
        ~allowed:true;
      let results =
        get_ok
          (invoke friend "calendar_search"
             {|{"mirror":1,"query":"Observatory"}|})
        |> decode
      in
      let first = List.hd (array (field "results" results)) in
      let version = int (field "version" first) in
      check "nested recurrence overrides searchable"
        (str (field "id" first) = "e1");
      List.iter
        (fun mirror ->
          check "calendar IDs require exact JSON integers"
            (Result.is_error
               (invoke friend "calendar_status"
                  (Printf.sprintf {|{"mirror":%s}|} mirror))))
        [ "1.5"; {|"1"|}; "null" ];
      let rec read offset acc =
        let output =
          get_ok
            (invoke friend "calendar_read"
               (Printf.sprintf {|{"mirror":1,"version":%d,"offset":%d}|} version
                  offset))
        in
        check "escaped JSON page fits tool cap" (String.length output <= 4096);
        let json = decode output in
        let acc = acc ^ str (field "text" json) in
        match field "next_offset" json with
        | Jsont.Null _ -> acc
        | next -> read (int next) acc
      in
      check "raw JSON reassembles byte for byte" (read 0 "" = original);
      check "receipts never include credential headers"
        (scalar db
           "SELECT count(*) FROM calendar_receipts WHERE request LIKE \
            '%token-hidden%' OR response LIKE '%token-hidden%'"
        = 0);
      let stale = event_cursor () in
      delta := true;
      fail_get := true;
      let failure = Calendars.poll calendars ~actor:admin 1 in
      check "failed fetch is redacted"
        (Result.is_error failure
        && failure <> Error "simulated failure, secret token-hidden");
      check "failed delta cannot advance cursor"
        ((event_cursor ()).revision = stale.revision);
      fail_get := false;
      ignore (get_ok (Calendars.poll calendars ~actor:admin 1));
      check "changes cursor wins over get future"
        ((event_cursor ()).state = Some "s2");
      check "deletions removed from current search"
        (S.search state ~actor:admin 1 ~kind:R.Event ~query:"Lunch" ~after:0
        = []);
      check "old versions retained privately"
        (scalar db
           "SELECT count(*) FROM calendar_versions WHERE kind='CalendarEvent'"
        = 3);
      check "tombstones retained"
        (scalar db
           "SELECT count(*) FROM calendar_deletions WHERE remote_id='e2'"
        > 0);
      check "obsolete raw pages refused"
        (Result.is_error
           (invoke admin "calendar_read"
              (Printf.sprintf {|{"mirror":1,"version":%d}|} version)));
      check "stale worker cannot overwrite new cursor"
        (reject (fun () ->
             S.commit state ~actor:admin stale stale ~items:[] ~destroyed:[]
               ~receipts:[]));
      expired := true;
      ignore (get_ok (Calendars.poll calendars ~actor:admin 1));
      check "expired state starts a new generation"
        ((event_cursor ()).phase = "new");
      check "last complete snapshot remains visible"
        (List.length
           (S.search state ~actor:admin 1 ~kind:R.Event ~query:"Revised"
              ~after:0)
        = 1);
      for _ = 1 to 3 do
        ignore (get_ok (Calendars.poll calendars ~actor:admin 1))
      done;
      check "expired state repaired" ((event_cursor ()).phase = "live");
      Store.set_person store ~actor:admin ~user:friend ~role:Friend
        ~allowed:false;
      check "revoked friend loses local access"
        (Result.is_error (invoke friend "calendar_status" "{}"));
      check "cancel stops polling" (Store.cancel_reminder store ~actor:admin 1);
      let before = List.length !reads in
      check "cancelled sync does no network reads"
        (Result.is_error (Calendars.poll calendars ~actor:admin 1)
        && before = List.length !reads));
  print_endline
    "Calendar mirror restart, fidelity and authorization tests passed."
