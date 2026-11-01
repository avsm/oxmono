(** Focused tests for folding MSC4186 responses into the common base state. *)

module Base = Matrix_client.Base_client
module Store = Matrix_client.Store
module Read_state = Matrix_client.Read_state
module Id = Matrix_proto.Id
module Event = Matrix_proto.Event
module Sync = Matrix_proto.Sync
module Sliding = Matrix_proto.Sliding_sync

let alice = Result.get_ok (Id.User_id.of_string "@alice:example.org")
let rid value = Result.get_ok (Id.Room_id.of_string value)
let check_bool = Alcotest.(check bool)
let check_int = Alcotest.(check int)
let check_string = Alcotest.(check string)

let check_string_option label expected actual =
  Alcotest.(check (option string)) label expected actual

let check_string_ints label expected actual =
  Alcotest.(check (list (pair string int))) label expected actual

let decode_sliding json =
  match Jsont_bytesrw.decode_string Sliding.Response.jsont json with
  | Ok response -> response
  | Error error -> Alcotest.failf "bad sliding response: %s\n%s" error json

let decode_classic json =
  match Jsont_bytesrw.decode_string Sync.Response.jsont json with
  | Ok response -> response
  | Error error -> Alcotest.failf "bad classic response: %s\n%s" error json

let response ?(lists = "{}") ?(rooms = "{}") ?(extensions = "{}") pos =
  decode_sliding
    (Printf.sprintf {|{"pos":%S,"lists":%s,"rooms":%s,"extensions":%s}|} pos
       lists rooms extensions)

let room_exn state room_id =
  match Base.find_room state room_id with
  | Some room -> room
  | None -> Alcotest.failf "room %s is absent" (Id.Room_id.to_string room_id)

let change_exn changes room_id =
  match
    List.find_opt
      (fun (change : Base.room_change) ->
        Id.Room_id.equal change.changed_room_id room_id)
      changes.Base.room_changes
  with
  | Some change -> change
  | None ->
      Alcotest.failf "change for %s is absent" (Id.Room_id.to_string room_id)

let event_type value = Event.Event_type.of_string value

let raw_event ?state_key ~event_id ~sender ~timestamp ~event_type content =
  let state_key =
    match state_key with
    | None -> ""
    | Some value -> Printf.sprintf ",\"state_key\":%S" value
  in
  Printf.sprintf
    {|{"event_id":%S,"sender":%S,"origin_server_ts":%Ld,"type":%S%s,"content":%s}|}
    event_id sender timestamp event_type state_key content

let message number sender =
  raw_event
    ~event_id:(Printf.sprintf "$message-%d:example.org" number)
    ~sender
    ~timestamp:(Int64.of_int (1000 + number))
    ~event_type:"m.room.message"
    (Printf.sprintf {|{"msgtype":"m.text","body":"message %d"}|} number)

let state_event number event_type state_key content =
  raw_event ~state_key
    ~event_id:(Printf.sprintf "$state-%d:example.org" number)
    ~sender:"@alice:example.org" ~timestamp:(Int64.of_int number) ~event_type
    content

let stripped_member ?is_direct membership =
  let is_direct =
    match is_direct with
    | None -> ""
    | Some value -> Printf.sprintf ",\"is_direct\":%b" value
  in
  Printf.sprintf
    {|{"sender":"@bob:example.org","type":"m.room.member","state_key":"@alice:example.org","content":{"membership":%S%s}}|}
    membership is_direct

let membership state room_id =
  Store.membership_to_string (room_exn state room_id).membership

let contains string substring =
  let string_length = String.length string in
  let substring_length = String.length substring in
  let rec loop offset =
    offset + substring_length <= string_length
    && (String.sub string offset substring_length = substring
       || loop (offset + 1))
  in
  substring_length = 0 || loop 0

let flush store =
  match Store.flush store with
  | Ok () -> ()
  | Error error ->
      Alcotest.failf "store flush failed: %s"
        (Matrix_client.Error.to_string error)

let make_temp_dir env prefix =
  let native_path = Filename.temp_file prefix ".d" in
  Unix.unlink native_path;
  Unix.mkdir native_path 0o700;
  (native_path, Eio.Path.(Eio.Stdenv.fs env / native_path))

let remove_temp_dir native =
  (try
     Sys.readdir native
     |> Array.iter (fun name ->
         try Unix.unlink (Filename.concat native name) with _ -> ())
   with _ -> ());
  try Unix.rmdir native with _ -> ()

let test_cursor_store_v4_and_migration () =
  Eio_main.run @@ fun env ->
  let native, dir = make_temp_dir env "matrix-base-sliding" in
  Fun.protect
    ~finally:(fun () -> remove_temp_dir native)
    (fun () ->
      let state = Base.create ~user_id:alice () in
      let state, _ =
        Base.apply state (decode_classic {|{"next_batch":"classic-1"}|})
      in
      let rooms =
        {|{"!persist:example.org":{"bump_stamp":7,"heroes":[{"user_id":"@bob:example.org","displayname":"Bob","avatar_url":"mxc://example.org/bob"}]}}|}
      in
      let extensions =
        {|{"to_device":{"next_batch":"to-device-1","events":[]}}|}
      in
      let state, _ =
        Base.apply_sliding state
          (response ~rooms ~lists:{|{"z":{"count":2},"a":{"count":1}}|}
             ~extensions "slide-1")
      in
      let state, _ =
        Base.apply_sliding state
          (response ~lists:{|{"z":{"count":3},"middle":{"count":4}}|} "slide-2")
      in
      check_string_option "classic cursor stays independent" (Some "classic-1")
        (Base.next_batch state);
      check_string_option "sliding cursor advances" (Some "slide-2")
        (Base.sliding_pos state);
      check_string_option "to-device cursor survives an absent delta"
        (Some "to-device-1")
        (Base.sliding_to_device_since state);
      check_string_ints "list deltas merge and normalize"
        [ ("a", 1); ("middle", 4); ("z", 3) ]
        (Base.sliding_lists state);
      let normalization_store = Store.memory () in
      Store.replace_sliding_session normalization_store ~pos:None
        ~to_device_since:None
        ~lists:[ ("z", 1); ("a", 2); ("z", 9) ];
      check_string_ints "store takes the last duplicate list count"
        [ ("a", 2); ("z", 9) ]
        (Store.sliding_lists normalization_store);
      let store = Store.on_disk ~dir in
      Base.persist store state;
      flush store;
      let serialized = Eio.Path.load Eio.Path.(dir / "base_state.json") in
      check_bool "store writes schema v4" true
        (contains serialized {|"format_version": 4|});
      let reopened = Store.on_disk ~dir in
      let restored = Base.of_store reopened ~user_id:alice () in
      check_string_option "classic cursor reloads" (Some "classic-1")
        (Base.next_batch restored);
      check_string_option "sliding cursor reloads" (Some "slide-2")
        (Base.sliding_pos restored);
      check_string_option "to-device cursor reloads" (Some "to-device-1")
        (Base.sliding_to_device_since restored);
      check_string_ints "list counts reload normalized"
        [ ("a", 1); ("middle", 4); ("z", 3) ]
        (Base.sliding_lists restored);
      let persisted_room = room_exn restored (rid "!persist:example.org") in
      Alcotest.(check (option int))
        "recency stamp reloads" (Some 7) persisted_room.recency_stamp;
      check_string_option "hero avatar reloads" (Some "mxc://example.org/bob")
        (match persisted_room.heroes with
        | [ hero ] -> hero.avatar_url
        | heroes ->
            Alcotest.failf "expected one hero, got %d" (List.length heroes));
      let reset = Base.reset_sliding_session restored in
      check_string_option "reset preserves classic cursor" (Some "classic-1")
        (Base.next_batch reset);
      check_string_option "reset clears sliding cursor" None
        (Base.sliding_pos reset);
      check_string_ints "reset clears list state" [] (Base.sliding_lists reset));
  let legacy_native, legacy_dir = make_temp_dir env "matrix-base-v3" in
  Fun.protect
    ~finally:(fun () -> remove_temp_dir legacy_native)
    (fun () ->
      let path = Eio.Path.(legacy_dir / "base_state.json") in
      Eio.Path.save ~create:(`Or_truncate 0o600) path
        {|{"format_version":3,"next_batch":"legacy-classic","rooms":[{"core":{"room_id":"!legacy:example.org","membership":"join","heroes":[{"user_id":"@bob:example.org","display_name":"Legacy Bob"}],"display_name":{"kind":"calculated","value":"Legacy Bob"}},"counts":{"joined_member_count":2,"last_active_ts":0}}],"account_data":{},"receipts":{},"profiles":{},"kv":{}}|};
      let legacy = Store.on_disk ~dir:legacy_dir in
      check_string_option "v3 classic cursor loads" (Some "legacy-classic")
        (Store.next_batch legacy);
      check_string_option "v3 defaults sliding cursor" None
        (Store.sliding_pos legacy);
      check_string_option "v3 defaults to-device cursor" None
        (Store.sliding_to_device_since legacy);
      check_string_ints "v3 defaults list state" [] (Store.sliding_lists legacy);
      let legacy_room =
        match Store.find_room legacy (rid "!legacy:example.org") with
        | Some room -> room
        | None -> Alcotest.fail "legacy room did not load"
      in
      Alcotest.(check (option int))
        "v3 defaults recency stamp" None legacy_room.recency_stamp;
      check_string_option "v3 defaults hero avatar" None
        (match legacy_room.heroes with
        | [ hero ] -> hero.avatar_url
        | heroes ->
            Alcotest.failf "expected one legacy hero, got %d"
              (List.length heroes));
      Store.set_next_batch legacy "legacy-rewritten";
      flush legacy;
      check_bool "v3 rewrites as schema v4" true
        (contains (Eio.Path.load path) {|"format_version": 4|}))

let test_required_state_excludes_timeline_state () =
  let room_id = rid "!state:example.org" in
  let required_name =
    state_event 1 "m.room.name" "" {|{"name":"Required name"}|}
  in
  let timeline_topic =
    state_event 2 "m.room.topic" "" {|{"topic":"Timeline topic"}|}
  in
  let rooms =
    Printf.sprintf
      {|{"!state:example.org":{"required_state":[%s],"timeline":[%s]}}|}
      required_name timeline_topic
  in
  let state, changes =
    Base.apply_sliding
      (Base.create ~user_id:alice ())
      (response ~rooms "state-1")
  in
  let room = room_exn state room_id in
  check_string_option "required state sets room name" (Some "Required name")
    room.name;
  check_string_option "timeline state does not set topic" None room.topic;
  check_int "only required state enters the durable projection" 1
    (List.length room.state_events);
  check_bool "sliding required state remains partial" true
    (room.state_completeness = Store.Partial);
  check_bool "sliding required state does not claim complete members" false
    room.members_complete;
  check_bool "required state is cached" true
    (Base.find_state_event state room_id ~event_type:(event_type "m.room.name")
       ()
    <> None);
  check_bool "timeline state is not cached" true
    (Base.find_state_event state room_id
       ~event_type:(event_type "m.room.topic")
       ()
    = None);
  let change = change_exn changes room_id in
  check_int "caller still sees the complete timeline" 1
    (List.length change.timeline);
  check_int "change reports required state only" 1
    (List.length change.state_events)

let test_membership_variants () =
  let join_event =
    state_event 10 "m.room.member" "@alice:example.org"
      {|{"membership":"join"}|}
  in
  let leave_event =
    state_event 11 "m.room.member" "@alice:example.org"
      {|{"membership":"leave"}|}
  in
  let rooms =
    Printf.sprintf
      {|{"!join:example.org":{"required_state":[%s]},"!invite:example.org":{"invite_state":[%s]},"!knock:example.org":{"invite_state":[%s]},"!leave:example.org":{"required_state":[%s]}}|}
      join_event (stripped_member "invite") (stripped_member "knock")
      leave_event
  in
  let state, _ =
    Base.apply_sliding
      (Base.create ~user_id:alice ())
      (response ~rooms "membership-1")
  in
  check_string "join membership" "join"
    (membership state (rid "!join:example.org"));
  check_string "invite membership" "invite"
    (membership state (rid "!invite:example.org"));
  check_string "knock membership" "knock"
    (membership state (rid "!knock:example.org"));
  check_string "leave membership" "leave"
    (membership state (rid "!leave:example.org"));
  let state, _ =
    Base.apply_sliding state
      (response ~rooms:{|{"!invite:example.org":{}}|} "membership-2")
  in
  check_string "absent invite_state defaults a prior invite to joined" "join"
    (membership state (rid "!invite:example.org"))

let test_directness_follows_membership_source () =
  let joined =
    state_event 12 "m.room.member" "@alice:example.org"
      {|{"membership":"join","is_direct":false}|}
  in
  let left =
    state_event 13 "m.room.member" "@alice:example.org"
      {|{"membership":"leave","is_direct":false}|}
  in
  let rooms =
    Printf.sprintf
      {|{"!joined-direct:example.org":{"required_state":[%s]},"!left-direct:example.org":{"required_state":[%s]},"!invite-direct:example.org":{"invite_state":[%s]},"!invite-listed:example.org":{"invite_state":[%s]},"!knock-listed:example.org":{"invite_state":[%s]}}|}
      joined left
      (stripped_member ~is_direct:true "invite")
      (stripped_member ~is_direct:false "invite")
      (stripped_member ~is_direct:true "knock")
  in
  let direct =
    {|{"account_data":{"global":[{"type":"m.direct","content":{"@bob:example.org":["!joined-direct:example.org","!left-direct:example.org","!invite-listed:example.org","!knock-listed:example.org"]}}]}}|}
  in
  let state, _ =
    Base.apply_sliding
      (Base.create ~user_id:alice ())
      (response ~rooms ~extensions:direct "direct-membership-1")
  in
  check_bool "joined room follows m.direct" true
    (room_exn state (rid "!joined-direct:example.org")).is_dm;
  check_bool "left room follows m.direct" true
    (room_exn state (rid "!left-direct:example.org")).is_dm;
  check_bool "unlisted invite follows its own is_direct" true
    (room_exn state (rid "!invite-direct:example.org")).is_dm;
  check_bool "m.direct cannot mark an invite" false
    (room_exn state (rid "!invite-listed:example.org")).is_dm;
  check_bool "m.direct and stripped is_direct cannot mark a knock" false
    (room_exn state (rid "!knock-listed:example.org")).is_dm;
  let clear_direct =
    {|{"account_data":{"global":[{"type":"m.direct","content":{}}]}}|}
  in
  let state, changes =
    Base.apply_sliding state
      (response ~extensions:clear_direct "direct-membership-2")
  in
  check_bool "empty m.direct clears joined room" false
    (room_exn state (rid "!joined-direct:example.org")).is_dm;
  check_bool "empty m.direct clears left room" false
    (room_exn state (rid "!left-direct:example.org")).is_dm;
  check_bool "reconciliation preserves invite-local directness" true
    (room_exn state (rid "!invite-direct:example.org")).is_dm;
  check_bool "reconciliation preserves non-direct invite" false
    (room_exn state (rid "!invite-listed:example.org")).is_dm;
  check_bool "reconciliation preserves knocked false" false
    (room_exn state (rid "!knock-listed:example.org")).is_dm;
  check_int "only joined and left rooms emit reconciled changes" 2
    (List.length changes.room_changes)

let test_avatar_tristate () =
  let room_id = rid "!avatar:example.org" in
  let state, _ =
    Base.apply_sliding
      (Base.create ~user_id:alice ())
      (response
         ~rooms:{|{"!avatar:example.org":{"avatar":"mxc://example.org/first"}}|}
         "avatar-1")
  in
  check_string_option "set installs avatar" (Some "mxc://example.org/first")
    (room_exn state room_id).avatar_url;
  let state, _ =
    Base.apply_sliding state
      (response ~rooms:{|{"!avatar:example.org":{"joined_count":2}}|} "avatar-2")
  in
  check_string_option "absent avatar is unchanged"
    (Some "mxc://example.org/first") (room_exn state room_id).avatar_url;
  let state, _ =
    Base.apply_sliding state
      (response ~rooms:{|{"!avatar:example.org":{"avatar":null}}|} "avatar-3")
  in
  check_string_option "explicit null removes avatar" None
    (room_exn state room_id).avatar_url

let test_server_name_and_direct_account_data () =
  let room_id = rid "!direct:example.org" in
  let empty_direct =
    {|{"account_data":{"global":[{"type":"m.direct","content":{}}]}}|}
  in
  let state, _ =
    Base.apply_sliding
      (Base.create ~user_id:alice ())
      (response
         ~rooms:{|{"!direct:example.org":{"name":"Server name","is_dm":true}}|}
         ~extensions:empty_direct "direct-1")
  in
  let room = room_exn state room_id in
  check_string_option "server-computed name is ignored" None room.name;
  check_bool "server is_dm is ignored" false room.is_dm;
  let mark_direct =
    {|{"account_data":{"global":[{"type":"m.direct","content":{"@bob:example.org":["!direct:example.org"]}}]}}|}
  in
  let state, changes =
    Base.apply_sliding state (response ~extensions:mark_direct "direct-2")
  in
  check_bool "m.direct marks an omitted known room" true
    (room_exn state room_id).is_dm;
  check_int "m.direct synthesizes one room change" 1
    (List.length changes.room_changes);
  let required_name =
    state_event 20 "m.room.name" "" {|{"name":"State name"}|}
  in
  let state, _ =
    Base.apply_sliding state
      (response
         ~rooms:
           (Printf.sprintf
              {|{"!direct:example.org":{"name":"Wrong again","is_dm":false,"required_state":[%s]}}|}
              required_name)
         "direct-3")
  in
  let room = room_exn state room_id in
  check_string_option "room state, not server name, supplies name"
    (Some "State name") room.name;
  check_bool "server false cannot override m.direct" true room.is_dm;
  let state, changes =
    Base.apply_sliding state (response ~extensions:empty_direct "direct-4")
  in
  check_bool "authoritative empty m.direct clears directness" false
    (room_exn state room_id).is_dm;
  check_int "clearing m.direct reports the omitted room" 1
    (List.length changes.room_changes)

let test_heroes_counts_prev_batch_and_ordering () =
  let older_id = rid "!older:example.org" in
  let newer_id = rid "!newer:example.org" in
  let rooms =
    {|{"!older:example.org":{"heroes":[{"user_id":"@bob:example.org","displayname":"Bob","avatar_url":"mxc://example.org/bob"}],"joined_count":3,"invited_count":1,"notification_count":8,"highlight_count":2,"prev_batch":"older-before","bump_stamp":4},"!newer:example.org":{"bump_stamp":9}}|}
  in
  let state, _ =
    Base.apply_sliding
      (Base.create ~user_id:alice ())
      (response ~rooms "summary-1")
  in
  let older = room_exn state older_id in
  check_int "joined count" 3 older.joined_member_count;
  check_int "invited count" 1 older.invited_member_count;
  check_int "server notification count" 8 older.notification_count;
  check_int "server highlight count" 2 older.highlight_count;
  check_string_option "prev_batch" (Some "older-before") older.prev_batch;
  Alcotest.(check (option int)) "bump stamp" (Some 4) older.recency_stamp;
  (match older.heroes with
  | [ hero ] ->
      check_string "hero id" "@bob:example.org"
        (Id.User_id.to_string hero.user_id);
      check_string_option "hero display name" (Some "Bob") hero.display_name;
      check_string_option "hero avatar" (Some "mxc://example.org/bob")
        hero.avatar_url
  | heroes -> Alcotest.failf "expected one hero, got %d" (List.length heroes));
  Alcotest.(check (list string))
    "bump stamp orders rooms"
    [ "!newer:example.org"; "!older:example.org" ]
    (List.map
       (fun (room : Store.room_info) -> Id.Room_id.to_string room.room_id)
       (Base.rooms state));
  let state, _ =
    Base.apply_sliding state
      (response ~rooms:{|{"!older:example.org":{"bump_stamp":12}}|} "summary-2")
  in
  let older = room_exn state older_id in
  check_int "absent counts preserve prior values" 3 older.joined_member_count;
  check_string_option "absent prev_batch clears the prior token" None
    older.prev_batch;
  check_int "absent heroes preserve prior value" 1 (List.length older.heroes);
  Alcotest.(check (list string))
    "new bump reorders rooms"
    [ "!older:example.org"; "!newer:example.org" ]
    (List.map
       (fun (room : Store.room_info) -> Id.Room_id.to_string room.room_id)
       (Base.rooms state));
  ignore (room_exn state newer_id)

let test_limited_room_revokes_complete_members () =
  let room_id = rid "!limited-members:example.org" in
  let own_member =
    state_event 30 "m.room.member" "@alice:example.org"
      {|{"membership":"join"}|}
  in
  let bob_member =
    raw_event ~state_key:"@bob:example.org" ~event_id:"$state-bob:example.org"
      ~sender:"@bob:example.org" ~timestamp:31L ~event_type:"m.room.member"
      {|{"membership":"join","displayname":"Bob"}|}
  in
  let classic =
    decode_classic
      (Printf.sprintf
         {|{"next_batch":"classic-complete","rooms":{"join":{"!limited-members:example.org":{"state":{"events":[%s,%s]},"timeline":{"events":[],"limited":false}}}}}|}
         own_member bob_member)
  in
  let state, _ =
    Base.apply ~coverage:Base.complete_state_coverage
      (Base.create ~user_id:alice ())
      classic
  in
  let before = room_exn state room_id in
  check_bool "classic complete state establishes complete members" true
    before.members_complete;
  check_bool "classic complete state establishes complete projection" true
    (before.state_completeness = Store.Complete);
  let state, _ =
    Base.apply_sliding state
      (response ~rooms:{|{"!limited-members:example.org":{"limited":true}}|}
         "limited-members-1")
  in
  let after = room_exn state room_id in
  check_bool "limited sliding room revokes member completeness" false
    after.members_complete;
  check_bool "sliding required-state coverage is partial" true
    (after.state_completeness = Store.Partial)

let test_num_live_and_limited_unread () =
  let room_id = rid "!unread:example.org" in
  let timeline =
    List.init 4 (fun index -> message (index + 1) "@bob:example.org")
    |> String.concat ","
  in
  let rooms =
    Printf.sprintf {|{"!unread:example.org":{"timeline":[%s],"num_live":2}}|}
      timeline
  in
  let state, changes =
    Base.apply_sliding
      (Base.create ~user_id:alice ())
      (response ~rooms "unread-1")
  in
  let change = change_exn changes room_id in
  check_int "caller sees historical and live timeline" 4
    (List.length change.timeline);
  check_int "only final live suffix contributes unread" 2 change.unread.unread;
  check_int "only final live suffix contributes notifications" 2
    change.unread.notifications;
  check_int "local unread stores the live suffix" 2
    (room_exn state room_id).local_unread_count;
  let next = message 5 "@bob:example.org" in
  let state, _ =
    Base.apply_sliding state
      (response
         ~rooms:
           (Printf.sprintf
              {|{"!unread:example.org":{"timeline":[%s],"num_live":1}}|} next)
         "unread-2")
  in
  check_int "non-limited delta accumulates" 3
    (room_exn state room_id).local_unread_count;
  let after_gap = message 6 "@bob:example.org" in
  let state, changes =
    Base.apply_sliding state
      (response
         ~rooms:
           (Printf.sprintf
              {|{"!unread:example.org":{"timeline":[%s],"num_live":1,"limited":true}}|}
              after_gap)
         "unread-3")
  in
  check_int "limited delta resets accumulated local unread" 1
    (room_exn state room_id).local_unread_count;
  check_bool "limited flag reaches the room change" true
    (change_exn changes room_id).limited

let test_extension_only_known_room () =
  let known_id = rid "!known:example.org" in
  let unknown_id = rid "!unknown:example.org" in
  let state, _ =
    Base.apply_sliding
      (Base.create ~user_id:alice ())
      (response ~rooms:{|{"!known:example.org":{}}|} "extensions-0")
  in
  let extensions =
    {|{
      "account_data":{"rooms":{
        "!known:example.org":[
          {"type":"m.tag","content":{"tags":{"m.favourite":{"order":0.25}}}},
          {"type":"m.fully_read","content":{"event_id":"$read:example.org"}}],
        "!unknown:example.org":[{"type":"m.tag","content":{"tags":{"m.lowpriority":{}}}}]}},
      "receipts":{"rooms":{
        "!known:example.org":{"type":"m.receipt","content":{"$read:example.org":{"m.read":{"@alice:example.org":{"ts":10}}}}},
        "!unknown:example.org":{"type":"m.receipt","content":{}}}},
      "typing":{"rooms":{
        "!known:example.org":{"type":"m.typing","content":{"user_ids":["@bob:example.org"]}},
        "!unknown:example.org":{"type":"m.typing","content":{"user_ids":[]}}}}
    }|}
  in
  let state, changes =
    Base.apply_sliding state (response ~extensions "extensions-1")
  in
  check_bool "unknown extension-only room is not invented" true
    (Base.find_room state unknown_id = None);
  check_int "only known extension-only room changes" 1
    (List.length changes.room_changes);
  let room = room_exn state known_id in
  check_bool "room account data updates tags" true
    (List.mem_assoc "m.favourite" room.tags);
  let change = change_exn changes known_id in
  Alcotest.(check (list string))
    "known room account-data is exposed"
    [ "m.tag"; "m.fully_read" ]
    (List.map fst change.room_account_data);
  check_int "receipt and typing are exposed" 2 (List.length change.ephemeral);
  let receipts = Base.receipts state known_id in
  check_string_option "receipt extension advances public receipt"
    (Some "$read:example.org")
    (Option.map
       (fun receipt -> Id.Event_id.to_string receipt.Read_state.event_id)
       (Read_state.public_read receipts));
  check_string_option "room account-data advances fully-read marker"
    (Some "$read:example.org")
    (Option.map Id.Event_id.to_string (Read_state.fully_read receipts))

let profile_string state user_id field =
  match Base.find_profile_field state user_id field with
  | Some (Jsont.String (value, _)) -> value
  | Some _ -> Alcotest.failf "profile field %S is not a string" field
  | None -> Alcotest.failf "profile field %S is absent" field

let test_profiles_once_and_changes () =
  let bob = Result.get_ok (Id.User_id.of_string "@bob:example.org") in
  let room_id = rid "!profile:example.org" in
  let rooms =
    {|{"!profile:example.org":{"heroes":[{"user_id":"@bob:example.org","displayname":"Room Bob","avatar_url":"mxc://example.org/room-bob"}],"joined_count":2}}|}
  in
  let initial_profiles =
    {|{"org.matrix.msc4262.profiles":{"users":{"@bob:example.org":{"updated":{"displayname":"Global Bob","custom":{"v":1}}}}}}|}
  in
  let state, changes =
    Base.apply_sliding
      (Base.create ~user_id:alice ())
      (response ~rooms ~extensions:initial_profiles "profiles-1")
  in
  check_int "initial profile is reported exactly once" 1
    (List.length changes.profile_changes);
  check_string "global profile was folded" "Global Bob"
    (profile_string state bob "displayname");
  check_string_option "global profile does not rewrite room hero"
    (Some "Room Bob")
    (match (room_exn state room_id).heroes with
    | [ hero ] -> hero.display_name
    | heroes -> Alcotest.failf "expected one hero, got %d" (List.length heroes));
  let state, replay_changes =
    Base.apply_sliding state
      (response ~extensions:initial_profiles "profiles-2")
  in
  check_int "semantic replay reports no profile change" 0
    (List.length replay_changes.profile_changes);
  check_int "profile-only replay reports no room change" 0
    (List.length replay_changes.room_changes);
  let patch =
    {|{"org.matrix.msc4262.profiles":{"users":{"@bob:example.org":{"updated":{"displayname":"Global Bob 2"}}}}}|}
  in
  let state, patch_changes =
    Base.apply_sliding state (response ~extensions:patch "profiles-3")
  in
  check_int "effective patch reports one change" 1
    (List.length patch_changes.profile_changes);
  check_string "patch updates one field" "Global Bob 2"
    (profile_string state bob "displayname");
  check_bool "patch retains an absent field" true
    (Base.find_profile_field state bob "custom" <> None);
  let drop =
    {|{"org.matrix.msc4262.profiles":{"users":{"@bob:example.org":null}}}|}
  in
  let state, drop_changes =
    Base.apply_sliding state (response ~extensions:drop "profiles-4")
  in
  check_int "drop reports one change" 1
    (List.length drop_changes.profile_changes);
  check_bool "drop removes complete profile" true
    (Base.find_profile state bob = None)

let test_to_device_cursor_and_e2ee_changes () =
  let first_extensions =
    {|{
      "to_device":{"next_batch":"td-1","events":[{"type":"m.room_key","content":{"x":1}}]},
      "e2ee":{"device_lists":{"changed":["@bob:example.org"],"left":["@carol:example.org"]},
              "device_one_time_keys_count":{"signed_curve25519":4},
              "device_unused_fallback_key_types":["signed_curve25519"]}
    }|}
  in
  let state, changes =
    Base.apply_sliding
      (Base.create ~user_id:alice ())
      (response ~extensions:first_extensions "e2ee-1")
  in
  check_string_option "enabled to-device advances cursor" (Some "td-1")
    (Base.sliding_to_device_since state);
  check_int "to-device events are exposed" 1 (List.length changes.to_device);
  (match changes.device_lists with
  | None -> Alcotest.fail "sliding E2EE device list is absent"
  | Some lists ->
      Alcotest.(check (list string))
        "changed devices" [ "@bob:example.org" ]
        (List.map Id.User_id.to_string lists.changed);
      Alcotest.(check (list string))
        "left devices" [ "@carol:example.org" ]
        (List.map Id.User_id.to_string lists.left));
  check_int "OTK count is exposed" 4
    (List.assoc "signed_curve25519" changes.one_time_keys_count);
  Alcotest.(check (option (list string)))
    "fallback keys are exposed" (Some [ "signed_curve25519" ])
    changes.unused_fallback_key_types;
  let disabled_extensions =
    {|{
      "to_device":{"next_batch":"td-2","events":[{"type":"m.test","content":{}}]},
      "e2ee":{"device_lists":{"changed":["@dave:example.org"]},
              "device_one_time_keys_count":{"curve25519":6}}
    }|}
  in
  let state, changes =
    Base.apply_sliding ~to_device_enabled:false state
      (response ~extensions:disabled_extensions "e2ee-2")
  in
  check_string_option "disabled to-device preserves request cursor"
    (Some "td-1")
    (Base.sliding_to_device_since state);
  check_string_option "disabled to-device still advances sliding position"
    (Some "e2ee-2") (Base.sliding_pos state);
  check_int "unsolicited to-device events remain observable" 1
    (List.length changes.to_device);
  check_int "disabled response still exposes E2EE counts" 6
    (List.assoc "curve25519" changes.one_time_keys_count);
  match changes.device_lists with
  | Some lists ->
      Alcotest.(check (list string))
        "disabled response exposes device changes" [ "@dave:example.org" ]
        (List.map Id.User_id.to_string lists.changed)
  | None -> Alcotest.fail "disabled response lost E2EE device changes"

let () =
  Alcotest.run "base_sliding"
    [
      ( "store",
        [
          Alcotest.test_case "v4 cursors, normalization and migration" `Quick
            test_cursor_store_v4_and_migration;
        ] );
      ( "rooms",
        [
          Alcotest.test_case "required state excludes timeline state" `Quick
            test_required_state_excludes_timeline_state;
          Alcotest.test_case "membership variants" `Quick
            test_membership_variants;
          Alcotest.test_case "directness follows membership source" `Quick
            test_directness_follows_membership_source;
          Alcotest.test_case "avatar tri-state" `Quick test_avatar_tristate;
          Alcotest.test_case "server name and m.direct" `Quick
            test_server_name_and_direct_account_data;
          Alcotest.test_case "heroes, counts and bump ordering" `Quick
            test_heroes_counts_prev_batch_and_ordering;
          Alcotest.test_case "limited room member coverage" `Quick
            test_limited_room_revokes_complete_members;
          Alcotest.test_case "num_live and limited unread" `Quick
            test_num_live_and_limited_unread;
        ] );
      ( "extensions",
        [
          Alcotest.test_case "known extension-only room" `Quick
            test_extension_only_known_room;
          Alcotest.test_case "profiles apply once" `Quick
            test_profiles_once_and_changes;
          Alcotest.test_case "to-device cursor and E2EE" `Quick
            test_to_device_cursor_and_e2ee_changes;
        ] );
    ]
