(** Hermetic tests for {!Matrix_ui.Room_list} and {!Matrix_ui.Matching}.

    Everything here is hand-built: a [/sync] response is folded through
    {!Matrix_client.Base_client} so the {!Matrix_client.Base_client.room_info}
    values the room list projects are the real ones, and the sections, filters,
    order and preview eligibility are checked against it.

    The live counterpart is [test/integration/scenario_room_list.ml]. *)

module Ui = Matrix_ui
module Id = Matrix_proto.Id
module Sync = Matrix_client.Base_client
module Filter = Ui.Room_list.Filter

let check_bool = Alcotest.(check bool)
let check_int = Alcotest.(check int)
let check_string = Alcotest.(check string)
let check_strings = Alcotest.(check (list string))
let user_id = Id.User_id.of_string_exn "@bob:example.org"
let alice = "@alice:example.org"
let room_id key = Id.Room_id.of_string_exn ("!" ^ key ^ ":example.org")

(* Text, not a codec: what is under test is the projection, and a literal
   response reads better than a tower of constructors. *)

let quote = Printf.sprintf "%S"

let message ~id ~body ~ts =
  Printf.sprintf
    {|{"event_id":%s,"sender":%s,"origin_server_ts":%Ld,"type":"m.room.message","content":{"msgtype":"m.text","body":%s}}|}
    (quote id) (quote alice) ts (quote body)

let member ?(user = alice) ?(membership = "join") ?previous ~id ~ts () =
  let unsigned =
    match previous with
    | None -> ""
    | Some previous ->
        Printf.sprintf {|,"unsigned":{"prev_content":{"membership":%s}}|}
          (quote previous)
  in
  Printf.sprintf
    {|{"event_id":%s,"sender":%s,"origin_server_ts":%Ld,"type":"m.room.member","state_key":%s,"content":{"membership":%s}%s}|}
    (quote id) (quote user) ts (quote user) (quote membership) unsigned

(* An [m.replace] of [target]: the fallback [body] is what a client that
   knows nothing of edits shows, and [m.new_content] is the real one. *)
let edit ~id ~target ~body ~ts =
  Printf.sprintf
    {|{"event_id":%s,"sender":%s,"origin_server_ts":%Ld,"type":"m.room.message","content":{"msgtype":"m.text","body":%s,"m.new_content":{"msgtype":"m.text","body":%s},"m.relates_to":{"rel_type":"m.replace","event_id":%s}}}|}
    (quote id) (quote alice) ts
    (quote ("* " ^ body))
    (quote body) (quote target)

let edit_without_new_content ~id ~target ~body ~ts =
  Printf.sprintf
    {|{"event_id":%s,"sender":%s,"origin_server_ts":%Ld,"type":"m.room.message","content":{"msgtype":"m.text","body":%s,"m.relates_to":{"rel_type":"m.replace","event_id":%s}}}|}
    (quote id) (quote alice) ts (quote body) (quote target)

let marked_unread_event ?(type_ = "m.marked_unread") unread =
  Printf.sprintf {|{"type":%s,"content":{"unread":%b}}|} (quote type_) unread

let receipt_event ~target ~ts =
  Printf.sprintf
    {|{"type":"m.receipt","content":{%s:{"m.read":{"@bob:example.org":{"ts":%Ld}}}}}|}
    (quote target) ts

let reaction ~id ~target ~ts =
  Printf.sprintf
    {|{"event_id":%s,"sender":%s,"origin_server_ts":%Ld,"type":"m.reaction","content":{"m.relates_to":{"rel_type":"m.annotation","event_id":%s,"key":"+1"}}}|}
    (quote id) (quote alice) ts (quote target)

let encrypted_event ~id ~ts =
  Printf.sprintf
    {|{"event_id":%s,"sender":%s,"origin_server_ts":%Ld,"type":"m.room.encrypted","content":{"algorithm":"m.megolm.v1.aes-sha2","ciphertext":"AAAA","sender_key":"k","session_id":"s","device_id":"D"}}|}
    (quote id) (quote alice) ts

let create_event ~id ~content =
  Printf.sprintf
    {|{"event_id":%s,"sender":%s,"origin_server_ts":1,"type":"m.room.create","state_key":"","content":%s}|}
    (quote id) (quote alice) content

let name_event ~name =
  Printf.sprintf
    {|{"event_id":"$name-%s","sender":%s,"origin_server_ts":1,"type":"m.room.name","state_key":"","content":{"name":%s}}|}
    (Digest.to_hex (Digest.string name))
    (quote alice) (quote name)

let tombstone_event ~replacement =
  Printf.sprintf
    {|{"event_id":"$tombstone-%s","sender":%s,"origin_server_ts":1,"type":"m.room.tombstone","state_key":"","content":{"body":"upgraded","replacement_room":%s}}|}
    (Digest.to_hex (Digest.string replacement))
    (quote alice)
    (quote ("!" ^ replacement ^ ":example.org"))

let encryption_event =
  Printf.sprintf
    {|{"event_id":"$enc","sender":%s,"origin_server_ts":1,"type":"m.room.encryption","state_key":"","content":{"algorithm":"m.megolm.v1.aes-sha2"}}|}
    (quote alice)

let power_levels_event ~bob ~alice:alice_level ~invite ~kick =
  Printf.sprintf
    {|{"event_id":"$power","sender":%s,"origin_server_ts":1,"type":"m.room.power_levels","state_key":"","content":{"users":{"@bob:example.org":%d,"@alice:example.org":%d},"invite":%d,"kick":%d}}|}
    (quote alice) bob alice_level invite kick

let tag_event tags =
  let entries =
    List.map (fun tag -> Printf.sprintf {|%s:{"order":0.5}|} (quote tag)) tags
    |> String.concat ","
  in
  Printf.sprintf {|{"type":"m.tag","content":{"tags":{%s}}}|} entries

type room_spec = {
  key : string;
  display : string option;
  events : string list;
  tags : string list;
  account_data : string list;
  ephemeral : string list;
  notifications : int;
  highlights : int;
  encryption : bool;
  tombstone : string option;
}

let spec ?display ?(events = []) ?(tags = []) ?(account_data = [])
    ?(ephemeral = []) ?(notifications = 0) ?(highlights = 0)
    ?(encryption = false) ?tombstone key =
  {
    key;
    display;
    events;
    tags;
    account_data;
    ephemeral;
    notifications;
    highlights;
    encryption;
    tombstone;
  }

let joined_room room =
  let state =
    (match room.display with None -> [] | Some name -> [ name_event ~name ])
    @ (if room.encryption then [ encryption_event ] else [])
    @ Option.fold ~none:[]
        ~some:(fun replacement -> [ tombstone_event ~replacement ])
        room.tombstone
  in
  let account_data =
    (if room.tags = [] then [] else [ tag_event room.tags ]) @ room.account_data
  in
  Printf.sprintf
    {|%s:{"state":{"events":[%s]},"timeline":{"events":[%s],"limited":false,"prev_batch":"p"},"account_data":{"events":[%s]},"ephemeral":{"events":[%s]},"unread_notifications":{"notification_count":%d,"highlight_count":%d}}|}
    (quote ("!" ^ room.key ^ ":example.org"))
    (String.concat "," state)
    (String.concat "," room.events)
    (String.concat "," account_data)
    (String.concat "," room.ephemeral)
    room.notifications room.highlights

let invited_room key =
  Printf.sprintf
    {|%s:{"invite_state":{"events":[{"sender":%s,"type":"m.room.member","state_key":"@bob:example.org","content":{"membership":"invite"}}]}}|}
    (quote ("!" ^ key ^ ":example.org"))
    (quote alice)

let left_room ?tombstone key =
  let state =
    match tombstone with
    | None -> ""
    | Some replacement ->
        Printf.sprintf {|"state":{"events":[%s]},|}
          (tombstone_event ~replacement)
  in
  Printf.sprintf
    {|%s:{%s"timeline":{"events":[],"limited":false,"prev_batch":"p"}}|}
    (quote ("!" ^ key ^ ":example.org"))
    state

let direct_account_data keys =
  let rooms =
    List.map (fun key -> quote ("!" ^ key ^ ":example.org")) keys
    |> String.concat ","
  in
  Printf.sprintf {|{"type":"m.direct","content":{%s:[%s]}}|} (quote alice) rooms

let response ?(joined = []) ?(invited = []) ?(left = []) ?(left_tombstones = [])
    ?(direct = []) ~batch () =
  let section name entries =
    if entries = [] then None
    else
      Some (Printf.sprintf {|%s:{%s}|} (quote name) (String.concat "," entries))
  in
  let rooms =
    List.filter_map Fun.id
      [
        section "join" (List.map joined_room joined);
        section "invite" (List.map invited_room invited);
        section "leave"
          (List.map left_room left
          @ List.map
              (fun (key, replacement) -> left_room ~tombstone:replacement key)
              left_tombstones);
      ]
    |> String.concat ","
  in
  let account_data =
    if direct = [] then ""
    else
      Printf.sprintf {|,"account_data":{"events":[%s]}|}
        (direct_account_data direct)
  in
  let json =
    Printf.sprintf {|{"next_batch":%s,"rooms":{%s}%s}|} (quote batch) rooms
      account_data
  in
  match Jsont_bytesrw.decode_string Matrix_proto.Sync.Response.jsont json with
  | Ok response -> response
  | Error message -> Alcotest.failf "sync response: %s" message

(* [Event_cache] guards each room with an [Eio.Mutex], so every test that
   builds one runs under a scheduler. *)

type fixture = {
  list : Ui.Room_list.t;
  cache : Ui.Event_cache.t;
  state : Sync.state;
}

let fold ?(feed_cache = true) responses =
  let cache = Ui.Event_cache.create () in
  let state =
    List.fold_left
      (fun state response ->
        let state, changes = Sync.apply state response in
        if feed_cache then
          List.iter
            (Ui.Event_cache.apply_room_change cache)
            changes.room_changes;
        state)
      (Sync.create ~user_id ~display_name:"Bob" ())
      responses
  in
  { list = Ui.Room_list.create cache state; cache; state }

let key_of room_id =
  let id = Id.Room_id.to_string room_id in
  String.sub id 1 (String.index id ':' - 1)

let visible fixture =
  Ui.Observable.List.snapshot (Ui.Room_list.rooms fixture.list) |> Array.to_list

let keys fixture =
  List.map (fun (room : Ui.Room_list.room) -> key_of room.id) (visible fixture)

let sorted_keys fixture = List.sort String.compare (keys fixture)

let find fixture key =
  Ui.Observable.List.snapshot (Ui.Room_list.all_rooms fixture.list)
  |> Array.to_list
  |> List.find_opt (fun (room : Ui.Room_list.room) ->
      String.equal (key_of room.id) key)
  |> function
  | Some room -> room
  | None -> Alcotest.failf "no room %s in the list" key

let with_predicate fixture predicate f =
  Ui.Room_list.set_filter fixture.list predicate;
  let result = f () in
  Ui.Room_list.set_filter fixture.list Filter.Non_left;
  result

(* [Text.search_key] case-folds and decomposes compatibly, so both
   directions of a diacritic query must match. *)

let test_normalization () =
  let matches needle haystack = Ui.Matching.contains ~haystack ~needle in
  check_bool "a bare query matches an accented name" true
    (matches "cafe" "Café");
  check_bool "an accented query matches a bare name" true
    (matches "café" "Cafe");
  check_bool "and an accented query an accented name" true
    (matches "CAFÉ" "café");
  check_bool "decomposed and composed spellings agree" true
    (matches "cafe\u{0301}" "café");
  check_bool "rust-sdk's own example" true (matches "stefan" "Ștefan");
  check_bool "and its other one" true (matches "un ete" "un été magnifique");
  check_bool "case folding maps the eszett onto ss" true
    (matches "STRASSE" "Straße");
  check_bool "compatibility decomposition folds a ligature" true
    (matches "office" "o\u{FB03}ce");
  check_bool "and fullwidth forms" true (matches "cafe" "\u{FF43}\u{FF41}fe");
  check_bool "a non-match is still a non-match" false
    (matches "matrxi" "MaTrIX");
  check_bool "the empty query matches everything" true (matches "" "anything");
  check_string "the key of an accented name is the bare one" "cafe"
    (Ui.Matching.search_key "CAFÉ");
  check_string "a script with no case or marks is left alone" "هند"
    (Ui.Matching.search_key "هند")

(* Membership first, then tags, then [is_dm], so a favourite DM is filed
   under [Favourites]. *)

let sections_fixture () =
  fold
    [
      response ~batch:"s1"
        ~joined:
          [
            spec "plain" ~display:"Plain";
            spec "fav" ~display:"Fav" ~tags:[ "m.favourite" ];
            spec "low" ~display:"Low" ~tags:[ "m.lowpriority" ];
            spec "dm" ~display:"Dm";
            spec "favdm" ~display:"Favdm" ~tags:[ "m.favourite" ];
          ]
        ~invited:[ "invite" ] ~left:[ "gone" ] ~direct:[ "dm"; "favdm" ] ();
    ]

let test_sections () =
  Eio_main.run @@ fun _ ->
  let fixture = sections_fixture () in
  let section key = (find fixture key).section in
  check_bool "a joined untagged group room is a Room" true
    (section "plain" = Ui.Room_list.Rooms);
  check_bool "m.favourite wins" true (section "fav" = Ui.Room_list.Favourites);
  check_bool "m.lowpriority too" true (section "low" = Ui.Room_list.Low_priority);
  check_bool "m.direct puts a room in People" true
    (section "dm" = Ui.Room_list.People);
  check_bool "but a tag outranks it" true
    (section "favdm" = Ui.Room_list.Favourites);
  check_bool "an invite is an Invite" true
    (section "invite" = Ui.Room_list.Invites);
  check_bool "a left room is Historical" true
    (section "gone" = Ui.Room_list.Historical);
  check_bool "is_dm is kept beside the section, which a tag can override" true
    (find fixture "favdm").is_dm;

  (* The default preset is rust-sdk's [non_left]: the left room is the only
     one it drops. *)
  check_strings "the default filter is non_left"
    [ "dm"; "fav"; "favdm"; "invite"; "low"; "plain" ]
    (sorted_keys fixture);
  check_bool "and the list is ordered by section" true
    (List.map (fun (room : Ui.Room_list.room) -> room.section) (visible fixture)
    = [
        Ui.Room_list.Invites;
        Ui.Room_list.Favourites;
        Ui.Room_list.Favourites;
        Ui.Room_list.People;
        Ui.Room_list.Rooms;
        Ui.Room_list.Low_priority;
      ])

let filters_fixture () =
  fold
    [
      response ~batch:"s1"
        ~joined:
          [
            spec "one" ~display:"Alpha" ~notifications:2;
            spec "two" ~display:"Beta" ~tags:[ "m.favourite" ];
            spec "three" ~display:"Gamma"
              ~tags:[ "m.favourite"; "m.lowpriority" ];
          ]
        ~invited:[ "four" ] ~left:[ "five" ] ~direct:[ "two" ] ();
    ]

let test_filters () =
  Eio_main.run @@ fun _ ->
  let fixture = filters_fixture () in
  let under predicate =
    with_predicate fixture predicate (fun () -> sorted_keys fixture)
  in
  let everything = [ "five"; "four"; "one"; "three"; "two" ] in
  check_strings "Everything admits every room" everything
    (under Filter.Everything);
  check_strings "Nothing admits none" [] (under Filter.Nothing);
  check_strings "an empty All is Everything" everything (under (Filter.All []));
  check_strings "an empty Any is Nothing" [] (under (Filter.Any []));
  check_strings "Non_left drops the left room"
    [ "four"; "one"; "three"; "two" ]
    (under Filter.Non_left);
  check_strings "Membership Joined is rust-sdk's joined filter"
    [ "one"; "three"; "two" ]
    (under (Filter.Membership Sync.Joined));
  check_strings "Membership Invited is its invite filter" [ "four" ]
    (under (Filter.Membership Sync.Invited));
  check_strings "Favourite" [ "three"; "two" ] (under Filter.Favourite);
  check_strings "Low_priority" [ "three" ] (under Filter.Low_priority);
  check_strings "Dm is category People" [ "two" ] (under Filter.Dm);
  check_strings "Not Dm is category Group"
    [ "five"; "four"; "one"; "three" ]
    (under (Filter.Not Filter.Dm));
  check_strings "Unread Notifications" [ "one" ]
    (under (Filter.Unread Filter.Notifications));
  check_strings "Unread Mentions is a different count" []
    (under (Filter.Unread Filter.Mentions));
  check_strings "Room_ids is the identifiers filter" [ "one"; "three" ]
    (under (Filter.Room_ids [ room_id "one"; room_id "three" ]));
  check_strings "In_section" [ "three"; "two" ]
    (under (Filter.In_section Ui.Room_list.Favourites));
  check_strings "Name matches the display name" [ "two" ]
    (under (Filter.Name "bet"));
  check_strings "an empty Name matches everything" everything
    (under (Filter.Name ""));
  check_strings "Name does not look at the room id" []
    (under (Filter.Name "!three"));
  check_strings "but Search does" [ "three" ] (under (Filter.Search "!three"));

  (* rust-sdk's documented example: non-left, People, favourite, and no
     unread notification. *)
  check_strings "all [non_left; People; favourite; not notifications]" [ "two" ]
    (under
       (Filter.All
          [
            Filter.Non_left;
            Filter.Dm;
            Filter.Favourite;
            Filter.Not (Filter.Unread Filter.Notifications);
          ]));
  check_strings "any [invite; low_priority]" [ "four"; "three" ]
    (under (Filter.Any [ Filter.Membership Sync.Invited; Filter.Low_priority ]));
  check_bool "Filter.everything sees through the trivial cases" true
    (Filter.everything Filter.Everything
    && Filter.everything (Filter.All [])
    && Filter.everything (Filter.Name "")
    && (not (Filter.everything Filter.Nothing))
    && not (Filter.everything Filter.Favourite))

let test_space_filter () =
  Eio_main.run @@ fun _ ->
  let fixture =
    fold
      [
        response ~batch:"s1"
          ~joined:
            [
              spec "space" ~display:"Space"
                ~events:
                  [
                    create_event ~id:"$create-space"
                      ~content:{|{"type":"m.space"}|};
                  ];
              spec "ordinary" ~display:"Ordinary"
                ~events:[ create_event ~id:"$create-ordinary" ~content:{|{}|} ];
              spec "malformed" ~display:"Malformed"
                ~events:
                  [
                    create_event ~id:"$create-malformed" ~content:{|{"type":7}|};
                  ];
              spec "missing" ~display:"Missing";
            ]
          ();
      ]
  in
  check_bool "m.space is projected from m.room.create" true
    (find fixture "space").is_space;
  check_bool "ordinary room create is not a space" false
    (find fixture "ordinary").is_space;
  check_bool "malformed room create is not a space" false
    (find fixture "malformed").is_space;
  check_bool "missing room create is not a space" false
    (find fixture "missing").is_space;
  let under filter =
    with_predicate fixture filter (fun () -> sorted_keys fixture)
  in
  check_strings "Space selects only m.space rooms" [ "space" ]
    (under Filter.Space);
  check_bool "Space matches room projection" true
    (Filter.matches Filter.Space (find fixture "space"));
  check_bool "Space excludes ordinary rooms" false
    (Filter.matches Filter.Space (find fixture "ordinary"));
  check_strings "Space composes with all" [ "space" ]
    (under (Filter.All [ Filter.Space; Filter.Name "space" ]));
  check_strings "Space composes with any" [ "ordinary"; "space" ]
    (under (Filter.Any [ Filter.Space; Filter.Name "ordinary" ]));
  check_bool "Space is not everything" false (Filter.everything Filter.Space);
  check_string "Space has a stable pretty-printer" "space"
    (Format.asprintf "%a" Filter.pp Filter.Space);
  let store = Matrix_client.Store.memory () in
  Sync.persist store fixture.state;
  let reopened = Sync.of_store store ~user_id () in
  let reloaded = Ui.Room_list.create fixture.cache reopened in
  Ui.Room_list.set_filter reloaded Filter.Space;
  check_strings "the space bit survives state persistence" [ "space" ]
    (Ui.Observable.List.snapshot (Ui.Room_list.rooms reloaded)
    |> Array.to_list
    |> List.map (fun (room : Ui.Room_list.room) -> key_of room.id)
    |> List.sort String.compare)

let test_composition () =
  Eio_main.run @@ fun _ ->
  let fixture =
    fold
      [
        response ~batch:"s1"
          ~joined:
            [
              spec "one" ~display:"Alpha" ~notifications:3;
              spec "two" ~display:"Beta" ~tags:[ "m.lowpriority" ];
              spec "three" ~display:"Gamma";
            ]
          ~left:[ "four" ] ();
      ]
  in
  let low_priority = Filter.In_section Ui.Room_list.Low_priority in
  Ui.Room_list.set_filter fixture.list
    (Filter.All [ Filter.Non_left; Filter.Not low_priority ]);
  check_strings "dropping low priority" [ "one"; "three" ] (sorted_keys fixture);
  Ui.Room_list.set_filter fixture.list
    (Filter.All
       [
         Filter.Non_left;
         Filter.Any
           [ Filter.Unread Filter.Notifications; Filter.Unread Filter.Messages ];
       ]);
  check_strings "unread only" [ "one" ] (sorted_keys fixture);
  Ui.Room_list.set_filter fixture.list
    (Filter.All [ Filter.Non_left; Filter.Search "gam" ]);
  check_strings "a query" [ "three" ] (sorted_keys fixture);
  check_bool "the predicate in force is the one that was set" true
    (Ui.Room_list.filter fixture.list
    = Filter.All [ Filter.Non_left; Filter.Search "gam" ]);
  Ui.Room_list.set_filter fixture.list Filter.Non_left;
  check_strings "clearing it brings every non-left room back"
    [ "one"; "three"; "two" ] (sorted_keys fixture)

let test_deduplicate_versions () =
  Eio_main.run @@ fun _ ->
  let under ?(left_tombstones = []) ?(joined = []) ?(left = []) ?(invited = [])
      () =
    let fixture =
      fold [ response ~batch:"s1" ~joined ~invited ~left ~left_tombstones () ]
    in
    Ui.Room_list.set_filter fixture.list Filter.Deduplicate_versions;
    sorted_keys fixture
  in
  check_strings "joined old is hidden by joined successor" [ "new" ]
    (under ~joined:[ spec "old" ~tombstone:"new"; spec "new" ] ());
  check_strings "joined old is hidden by left successor" [ "new" ]
    (under ~joined:[ spec "old" ~tombstone:"new" ] ~left:[ "new" ] ());
  check_strings "joined old stays with invited successor" [ "new"; "old" ]
    (under ~joined:[ spec "old" ~tombstone:"new" ] ~invited:[ "new" ] ());
  check_strings "joined old stays when successor is unknown" [ "old" ]
    (under ~joined:[ spec "old" ~tombstone:"missing" ] ());
  check_strings "left old is hidden by any known successor" [ "new" ]
    (under ~left_tombstones:[ ("old", "new") ] ~joined:[ spec "new" ] ());
  check_strings "left old stays when successor is unknown" [ "old" ]
    (under ~left_tombstones:[ ("old", "missing") ] ());
  (* A syntactically malformed tombstone is not evidence of an upgrade. *)
  let malformed =
    fold
      [
        response ~batch:"s1"
          ~joined:
            [
              spec "old"
                ~events:
                  [
                    {|{"event_id":"$bad","sender":"@alice:example.org","origin_server_ts":1,"type":"m.room.tombstone","state_key":"","content":{"body":"upgraded","replacement_room":42}}|};
                  ];
              spec "new";
            ]
          ();
      ]
  in
  Ui.Room_list.set_filter malformed.list Filter.Deduplicate_versions;
  check_strings "malformed tombstone does not hide" [ "new"; "old" ]
    (sorted_keys malformed);

  (* Refreshing after a sync must reconsider the whole projection: a
     successor arriving later changes the old room's visibility. *)
  let dynamic =
    fold [ response ~batch:"s1" ~joined:[ spec "old" ~tombstone:"new" ] () ]
  in
  Ui.Room_list.set_filter dynamic.list Filter.Deduplicate_versions;
  check_strings "old is visible before successor arrives" [ "old" ]
    (sorted_keys dynamic);
  let state, changes =
    Sync.apply dynamic.state (response ~batch:"s2" ~joined:[ spec "new" ] ())
  in
  List.iter
    (Ui.Event_cache.apply_room_change dynamic.cache)
    changes.room_changes;
  Ui.Room_list.refresh dynamic.list state;
  check_strings "old is hidden after successor arrives" [ "new" ]
    (sorted_keys dynamic)

(* A room with a latest event outranks one without whatever the recency
   stamps say, because the two are different scales and a sort must never
   switch scale midway. *)

let ts = Matrix_proto.Event.Timestamp.of_ms

let sample ~name ~latest ~last_active ?(unsent = false) key =
  {
    Ui.Room_list.id = room_id key;
    name;
    avatar_url = None;
    topic = None;
    membership = Sync.Joined;
    section = Ui.Room_list.Rooms;
    is_dm = false;
    is_space = false;
    latest = Some "body";
    latest_sender = None;
    latest_timestamp = Option.map ts latest;
    latest_is_unsent = unsent;
    last_active = ts last_active;
    notification_count = 0;
    highlight_count = 0;
    unread_messages = 0;
    marked_unread = false;
    encrypted = false;
    tags = [];
  }

let order order_ rooms =
  List.sort (Ui.Room_list.compare_rooms order_) rooms
  |> List.map (fun (room : Ui.Room_list.room) -> room.name)

let test_sorting () =
  let old_message =
    sample "a" ~name:"Zulu" ~latest:(Some 100L) ~last_active:100L
  in
  let new_message =
    sample "b" ~name:"Alpha" ~latest:(Some 200L) ~last_active:200L
  in
  (* No message yet, so the recency stamp is all there is — and here it is a
     bigger number than either timestamp above, which must not promote it. *)
  let silent = sample "c" ~name:"Mike" ~latest:None ~last_active:9_000L in
  let quieter = sample "d" ~name:"November" ~latest:None ~last_active:8_000L in
  check_strings "newest message first, then the rooms with no message at all"
    [ "Alpha"; "Zulu"; "Mike"; "November" ]
    (order Ui.Room_list.Activity [ silent; old_message; quieter; new_message ]);
  check_strings "a recency stamp never outranks a real message"
    [ "Zulu"; "Mike" ]
    (order Ui.Room_list.Activity [ silent; old_message ]);
  check_strings "rooms with no message fall back to the recency stamp"
    [ "Mike"; "November" ]
    (order Ui.Room_list.Activity [ quieter; silent ]);
  check_strings "Name sorts by display name, ascending"
    [ "Alpha"; "Mike"; "November"; "Zulu" ]
    (order Ui.Room_list.Name [ silent; old_message; quieter; new_message ]);
  let unsent =
    sample "e" ~name:"Yankee" ~latest:(Some 1L) ~last_active:1L ~unsent:true
  in
  check_strings "an unsent local echo sorts first however old it is"
    [ "Yankee"; "Alpha"; "Zulu" ]
    (order Ui.Room_list.Activity [ old_message; new_message; unsent ]);
  let same_a = sample "f" ~name:"Same" ~latest:(Some 5L) ~last_active:5L in
  let same_b = sample "g" ~name:"Same" ~latest:(Some 5L) ~last_active:5L in
  check_bool "the room id breaks a total tie" true
    (Ui.Room_list.compare_rooms Ui.Room_list.Activity same_a same_b < 0);
  let invite = { same_a with section = Ui.Room_list.Invites; name = "Zulu" } in
  check_strings "sections sort before anything else" [ "Zulu"; "Alpha" ]
    (order Ui.Room_list.Name [ new_message; invite ])

let test_sorting_live () =
  Eio_main.run @@ fun _ ->
  let fixture =
    fold
      [
        response ~batch:"s1"
          ~joined:
            [
              spec "one" ~display:"Alpha"
                ~events:[ message ~id:"$a" ~body:"first" ~ts:1_000L ];
              spec "two" ~display:"Zulu"
                ~events:[ message ~id:"$b" ~body:"second" ~ts:2_000L ];
              spec "three" ~display:"Mike";
            ]
          ();
      ]
  in
  check_strings "Activity puts the newest message on top"
    [ "two"; "one"; "three" ] (keys fixture);
  Ui.Room_list.set_sort fixture.list Ui.Room_list.Name;
  check_strings "Name reorders alphabetically" [ "one"; "three"; "two" ]
    (keys fixture);
  Ui.Room_list.set_sort fixture.list Ui.Room_list.Activity;
  check_strings "and back" [ "two"; "one"; "three" ] (keys fixture);
  check_bool "the room with no message has no latest timestamp" true
    (Option.is_none (find fixture "three").latest_timestamp);
  check_bool "and its latest event's timestamp is the message's" true
    ((find fixture "one").latest_timestamp = Some (ts 1_000L))

let test_preview () =
  Eio_main.run @@ fun _ ->
  let fixture =
    fold
      [
        response ~batch:"s1"
          ~joined:
            [
              (* A freshly joined room: a membership event is the newest thing
                 in the window, and used to preview as the literal string
                 "m.room.member". *)
              spec "joined" ~display:"Joined"
                ~events:
                  [
                    message ~id:"$m1" ~body:"hello" ~ts:1_000L;
                    member ~id:"$s1" ~ts:2_000L ();
                  ];
              spec "reacted" ~display:"Reacted"
                ~events:
                  [
                    message ~id:"$m2" ~body:"visible" ~ts:1_000L;
                    reaction ~id:"$r1" ~target:"$m2" ~ts:2_000L;
                  ];
              spec "state-only" ~display:"State"
                ~events:[ member ~id:"$s2" ~ts:1_000L () ];
            ]
          ();
      ]
  in
  let preview key = (find fixture key).latest in
  check_string "a state event does not become the preview" "hello"
    (Option.get (preview "joined"));
  check_bool "nor the sort timestamp" true
    ((find fixture "joined").latest_timestamp = Some (ts 1_000L));
  check_string "a reaction is skipped too" "visible"
    (Option.get (preview "reacted"));
  check_bool "a room with only state events has no preview" true
    (Option.is_none (preview "state-only"));
  check_bool "and falls back to the recency stamp for its order" true
    (Option.is_none (find fixture "state-only").latest_timestamp)

let raw json =
  match Jsont_bytesrw.decode_string Matrix_proto.Event.Raw_event.jsont json with
  | Ok event -> event
  | Error message -> Alcotest.failf "event: %s" message

let test_preview_decrypted () =
  Eio_main.run @@ fun _ ->
  let fixture =
    fold
      [
        response ~batch:"s1"
          ~joined:
            [
              spec "secret" ~display:"Secret" ~encryption:true
                ~events:[ encrypted_event ~id:"$e1" ~ts:1_000L ];
            ]
          ();
      ]
  in
  check_bool "the room is flagged encrypted" true
    (find fixture "secret").encrypted;
  check_bool "an event we cannot read is not a preview, as in rust-sdk" true
    (Option.is_none (find fixture "secret").latest);
  let installed =
    Ui.Event_cache.set_decrypted fixture.cache (room_id "secret")
      ~encrypted:(raw (encrypted_event ~id:"$e1" ~ts:1_000L))
      ~plaintext:(raw (message ~id:"$e1" ~body:"the plaintext" ~ts:1_000L))
  in
  check_bool "the plaintext is installed" true installed;
  Ui.Room_list.refresh fixture.list fixture.state;
  check_string "and the preview is the plaintext, not the ciphertext"
    "the plaintext"
    (Option.get (find fixture "secret").latest)

let test_plaintext_edit_of_decrypted_event_is_rejected () =
  Eio_main.run @@ fun _ ->
  let fixture =
    fold
      [
        response ~batch:"s1"
          ~joined:
            [
              spec "secret"
                ~events:
                  [
                    encrypted_event ~id:"$m1" ~ts:1_000L;
                    edit ~id:"$e1" ~target:"$m1" ~body:"plaintext edit"
                      ~ts:2_000L;
                  ]
                ~encryption:true;
            ]
          ();
      ]
  in
  check_bool "original decryption is installed" true
    (Ui.Event_cache.set_decrypted fixture.cache (room_id "secret")
       ~encrypted:(raw (encrypted_event ~id:"$m1" ~ts:1_000L))
       ~plaintext:(raw (message ~id:"$m1" ~body:"original" ~ts:1_000L)));
  Ui.Room_list.refresh fixture.list fixture.state;
  check_string "a plaintext edit cannot replace an encrypted original"
    "original"
    (Option.get (find fixture "secret").latest)

let test_encrypted_edit_requires_new_content () =
  let original =
    Ui.Presentation.of_event
      (raw (message ~id:"$m1" ~body:"original" ~ts:1_000L))
  in
  let missing =
    Ui.Presentation.of_event
      (raw
         (edit_without_new_content ~id:"$e1" ~target:"$m1"
            ~body:"missing new content" ~ts:2_000L))
  in
  check_bool "the encryption-aware validator requires new content" false
    (Ui.Presentation.is_valid_replacement_with_encryption ~original
       ~original_encrypted:true ~replacement:missing ~replacement_encrypted:true);
  let fixture_with edit_event =
    Eio_main.run @@ fun _ ->
    let fixture =
      fold
        [
          response ~batch:"s1"
            ~joined:
              [
                spec "secret" ~encryption:true
                  ~events:[ encrypted_event ~id:"$m1" ~ts:1_000L; edit_event ];
              ]
            ();
        ]
    in
    check_bool "original decryption is installed" true
      (Ui.Event_cache.set_decrypted fixture.cache (room_id "secret")
         ~encrypted:(raw (encrypted_event ~id:"$m1" ~ts:1_000L))
         ~plaintext:(raw (message ~id:"$m1" ~body:"original" ~ts:1_000L)));
    check_bool "edit decryption is installed" true
      (Ui.Event_cache.set_decrypted fixture.cache (room_id "secret")
         ~encrypted:(raw (encrypted_event ~id:"$e1" ~ts:2_000L))
         ~plaintext:(raw edit_event));
    Ui.Room_list.refresh fixture.list fixture.state;
    Option.get (find fixture "secret").latest
  in
  check_string "a decrypted encrypted edit with new content is accepted"
    "encrypted edit"
    (fixture_with
       (edit ~id:"$e1" ~target:"$m1" ~body:"encrypted edit" ~ts:2_000L));
  check_string "a decrypted encrypted edit without new content is rejected"
    "original"
    (fixture_with
       (edit_without_new_content ~id:"$e1" ~target:"$m1"
          ~body:"missing new content" ~ts:2_000L))

(* The fallback path: nothing reaches the cache, so [room_info.latest_event]
   is all there is, and it is a preview only if it passes the same test. *)
let test_preview_fallback () =
  Eio_main.run @@ fun _ ->
  let fixture =
    fold ~feed_cache:false
      [
        response ~batch:"s1"
          ~joined:
            [
              spec "words" ~display:"Words"
                ~events:[ message ~id:"$m" ~body:"from the summary" ~ts:1_000L ];
              spec "state" ~display:"State"
                ~events:[ member ~id:"$s" ~ts:1_000L () ];
            ]
          ();
      ]
  in
  check_int "the cache really is empty" 0
    (Array.length (Ui.Event_cache.snapshot fixture.cache (room_id "words")));
  check_string "the summary's latest event previews when it is worthy"
    "from the summary"
    (Option.get (find fixture "words").latest);
  check_bool "and is dropped when it is not" true
    (Option.is_none (find fixture "state").latest)

(* [Filter.Fuzzy] beside [Filter.Name], over the same normalized text. *)

let test_fuzzy () =
  Eio_main.run @@ fun _ ->
  let fixture =
    fold
      [
        response ~batch:"s1"
          ~joined:
            [
              spec "matrix" ~display:"Matrix HQ";
              spec "mixer" ~display:"Mixer";
              spec "other" ~display:"Something else";
            ]
          ();
      ]
  in
  let under predicate =
    with_predicate fixture predicate (fun () -> sorted_keys fixture)
  in
  check_strings "rust-sdk's own example: mtx matches matrix" [ "matrix" ]
    (under (Filter.Fuzzy "mtx"));
  check_strings "and mxt matches neither" [] (under (Filter.Fuzzy "mxt"));
  check_strings "a needle can match more than one room" [ "matrix"; "mixer" ]
    (under (Filter.Fuzzy "mx"));
  check_strings "the empty needle matches everything"
    [ "matrix"; "mixer"; "other" ]
    (under (Filter.Fuzzy ""));
  check_strings "where the exact matcher finds only the literal" [ "matrix" ]
    (under (Filter.Name "matrix"));
  check_strings "and nothing for the fuzzy spelling" []
    (under (Filter.Name "mtx"));
  check_bool "an empty fuzzy pattern reads as no filter" true
    (Filter.everything (Filter.Fuzzy "")
    && not (Filter.everything (Filter.Fuzzy "mtx")));

  (* The score is [Some] exactly where the filter matches, and ranks. *)
  let score filter key = Ui.Room_list.Filter.score filter (find fixture key) in
  let agrees filter key =
    Option.is_some (score filter key) = Filter.matches filter (find fixture key)
  in
  List.iter
    (fun (filter : Filter.t) ->
      List.iter
        (fun key ->
          check_bool "score agrees with matches" true (agrees filter key))
        [ "matrix"; "mixer"; "other" ])
    [
      Filter.Everything;
      Filter.Nothing;
      Filter.Fuzzy "mx";
      Filter.Not (Filter.Fuzzy "mx");
      Filter.All [ Filter.Non_left; Filter.Fuzzy "mx" ];
      Filter.Any [ Filter.Fuzzy "mtx"; Filter.Favourite ];
      Filter.All [];
      Filter.Any [];
    ];
  check_int "a filter with no fuzzy clause scores zero" 0
    (Option.get (score Filter.Everything "matrix"));
  check_bool "a closer match scores higher" true
    (Option.get (score (Filter.Fuzzy "mix") "mixer")
    > Option.get (score (Filter.Fuzzy "mix") "matrix"));
  check_bool "All sums the clauses it matched" true
    (Option.get
       (score (Filter.All [ Filter.Fuzzy "mx"; Filter.Fuzzy "mx" ]) "mixer")
    = 2 * Option.get (score (Filter.Fuzzy "mx") "mixer"));
  check_bool "Any takes the best of them" true
    (Option.get
       (score (Filter.Any [ Filter.Fuzzy "mix"; Filter.Fuzzy "mx" ]) "mixer")
    = Option.get (score (Filter.Fuzzy "mix") "mixer"))

(* Every unread category is [count > 0 || is_marked_unread], and the flag is
   the room's account data: [m.marked_unread] (MSC2867) or the older
   [com.famedly.marked_unread]. *)

let test_marked_unread () =
  Eio_main.run @@ fun _ ->
  let quiet ?(account_data = []) ?(ephemeral = []) key display =
    spec key ~display ~account_data ~ephemeral
  in
  let fixture =
    fold
      [
        response ~batch:"s1"
          ~joined:
            [
              quiet "flagged" "Flagged"
                ~account_data:[ marked_unread_event true ];
              quiet "unstable" "Unstable"
                ~account_data:
                  [
                    marked_unread_event ~type_:"com.famedly.marked_unread" true;
                  ];
              quiet "both" "Both"
                ~account_data:
                  [
                    marked_unread_event ~type_:"com.famedly.marked_unread" true;
                    marked_unread_event false;
                  ];
              quiet "plain" "Plain";
            ]
          ();
      ]
  in
  let marked key = (find fixture key).marked_unread in
  check_bool "the stable type sets the flag" true (marked "flagged");
  check_bool "so does the unstable one" true (marked "unstable");
  check_bool "and the stable type wins when both are in one response" false
    (marked "both");
  check_bool "a room with no such account data is not marked" false
    (marked "plain");
  check_bool "a marked room reads as unread with no messages at all" true
    (Ui.Room_list.unread (find fixture "flagged"));
  check_int "and no message count says so" 0
    (find fixture "flagged").unread_messages;
  let under predicate =
    with_predicate fixture predicate (fun () -> sorted_keys fixture)
  in
  check_strings "every unread category ORs the flag, as rust-sdk does"
    [ "flagged"; "unstable" ]
    (under (Filter.Unread Filter.Messages));
  check_strings "notifications too" [ "flagged"; "unstable" ]
    (under (Filter.Unread Filter.Notifications));
  check_strings "and mentions" [ "flagged"; "unstable" ]
    (under (Filter.Unread Filter.Mentions));
  Ui.Room_list.set_filter fixture.list
    (Filter.All
       [
         Filter.Non_left;
         Filter.Any
           [ Filter.Unread Filter.Notifications; Filter.Unread Filter.Messages ];
       ]);
  check_strings "so does the composed unread filter" [ "flagged"; "unstable" ]
    (sorted_keys fixture);
  Ui.Room_list.set_filter fixture.list Filter.Non_left;

  (* Clearing: our own read receipt, or the flag being written false. *)
  let cleared =
    fold
      [
        response ~batch:"s1"
          ~joined:
            [
              quiet "byreceipt" "By receipt"
                ~account_data:[ marked_unread_event true ];
              quiet "byflag" "By flag"
                ~account_data:[ marked_unread_event true ];
              quiet "kept" "Kept" ~account_data:[ marked_unread_event true ];
            ]
          ();
        response ~batch:"s2"
          ~joined:
            [
              spec "byreceipt"
                ~events:[ message ~id:"$r1" ~body:"hi" ~ts:1_000L ]
                ~ephemeral:[ receipt_event ~target:"$r1" ~ts:5L ];
              spec "byflag" ~account_data:[ marked_unread_event false ];
              spec "kept";
            ]
          ();
      ]
  in
  check_bool "our own read receipt clears the flag" false
    (find cleared "byreceipt").marked_unread;
  check_bool "writing the flag false clears it too" false
    (find cleared "byflag").marked_unread;
  check_bool "and a sync that says neither leaves it alone" true
    (find cleared "kept").marked_unread;

  (* A flag set in the same response as a receipt is the newer statement. *)
  let both =
    fold
      [
        response ~batch:"s1" ~joined:[ quiet "room" "Room" ] ();
        response ~batch:"s2"
          ~joined:
            [
              spec "room"
                ~events:[ message ~id:"$r2" ~body:"hi" ~ts:1_000L ]
                ~ephemeral:[ receipt_event ~target:"$r2" ~ts:5L ]
                ~account_data:[ marked_unread_event true ];
            ]
          ();
      ]
  in
  check_bool "an explicit flag beside a receipt still marks the room" true
    (find both "room").marked_unread

(* The newest edit of an event is substituted for the event it replaces. *)

let test_preview_edit () =
  Eio_main.run @@ fun _ ->
  let fixture =
    fold
      [
        response ~batch:"s1"
          ~joined:
            [
              spec "edited" ~display:"Edited"
                ~events:
                  [
                    message ~id:"$m1" ~body:"original" ~ts:1_000L;
                    edit ~id:"$e1" ~target:"$m1" ~body:"corrected" ~ts:2_000L;
                  ];
              spec "twice" ~display:"Twice"
                ~events:
                  [
                    message ~id:"$m2" ~body:"original" ~ts:1_000L;
                    edit ~id:"$e2" ~target:"$m2" ~body:"first go" ~ts:2_000L;
                    edit ~id:"$e3" ~target:"$m2" ~body:"second go" ~ts:3_000L;
                  ];
              spec "newer" ~display:"Newer"
                ~events:
                  [
                    message ~id:"$m3" ~body:"original" ~ts:1_000L;
                    edit ~id:"$e4" ~target:"$m3" ~body:"corrected" ~ts:2_000L;
                    message ~id:"$m4" ~body:"a later message" ~ts:3_000L;
                  ];
              spec "orphan" ~display:"Orphan"
                ~events:
                  [
                    message ~id:"$m5" ~body:"visible" ~ts:1_000L;
                    edit ~id:"$e5" ~target:"$nowhere" ~body:"lost" ~ts:2_000L;
                  ];
            ]
          ();
      ]
  in
  let preview key = Option.get (find fixture key).latest in
  check_string "the preview is the edited body, not the original" "corrected"
    (preview "edited");
  check_string "an edit does not become a preview of its own" "second go"
    (preview "twice");
  check_string "a message after the edit still wins" "a later message"
    (preview "newer");
  check_string "an edit of an event we do not have is skipped" "visible"
    (preview "orphan");
  check_bool "and the preview carries the edit's timestamp" true
    ((find fixture "edited").latest_timestamp = Some (ts 2_000L))

(* One state event may preview a room: the own user joining or being
   invited, so that a room with nothing else in it has something to show.
   Someone else's join is not a preview. *)

let test_preview_own_membership () =
  Eio_main.run @@ fun _ ->
  let bob = "@bob:example.org" in
  let fixture =
    fold
      [
        response ~batch:"s1"
          ~joined:
            [
              spec "mine" ~display:"Mine"
                ~events:
                  [
                    member ~user:bob ~previous:"invite" ~id:"$b1" ~ts:1_000L ();
                  ];
              spec "theirs" ~display:"Theirs"
                ~events:[ member ~id:"$a1" ~ts:1_000L () ];
              spec "after" ~display:"After"
                ~events:
                  [
                    member ~user:bob ~previous:"invite" ~id:"$b2" ~ts:1_000L ();
                    message ~id:"$m6" ~body:"a message" ~ts:2_000L;
                  ];
              spec "left" ~display:"Left"
                ~events:
                  [
                    member ~user:bob ~membership:"leave" ~previous:"join"
                      ~id:"$b3" ~ts:1_000L ();
                  ];
            ]
          ();
      ]
  in
  check_string "our own join previews the room"
    (bob ^ " accepted the invitation")
    (Option.get (find fixture "mine").latest);
  check_bool "someone else's join does not" true
    (Option.is_none (find fixture "theirs").latest);
  check_string "and a real message still wins" "a message"
    (Option.get (find fixture "after").latest);
  check_bool "our own departure is not one of the accepted changes" true
    (Option.is_none (find fixture "left").latest);
  check_bool "the predicate without an own user refuses every state event" false
    (Ui.Presentation.is_preview_worthy
       (Ui.Presentation.of_event
          (raw (member ~user:bob ~previous:"invite" ~id:"$b4" ~ts:1L ()))));
  check_bool "and with one it accepts the join" true
    (Ui.Presentation.is_preview_worthy
       ~own_user:(Id.User_id.of_string_exn bob)
       (Ui.Presentation.of_event
          (raw (member ~user:bob ~previous:"invite" ~id:"$b5" ~ts:1L ()))))

let test_preview_actionable_knock () =
  Eio_main.run @@ fun _ ->
  let knock id = member ~membership:"knock" ~id ~ts:2_000L () in
  let fixture =
    fold
      [
        response ~batch:"s1"
          ~joined:
            [
              spec "invite" ~display:"Invite"
                ~events:
                  [
                    power_levels_event ~bob:50 ~alice:0 ~invite:50 ~kick:60;
                    knock "$knock-invite";
                  ];
              spec "kick" ~display:"Kick"
                ~events:
                  [
                    power_levels_event ~bob:50 ~alice:0 ~invite:100 ~kick:50;
                    knock "$knock-kick";
                  ];
              spec "denied" ~display:"Denied"
                ~events:
                  [
                    power_levels_event ~bob:50 ~alice:50 ~invite:100 ~kick:50;
                    knock "$knock-denied";
                  ];
              spec "unknown" ~display:"Unknown"
                ~events:[ knock "$knock-unknown" ];
            ]
          ();
      ]
  in
  check_string "invite permission admits the knock"
    "@alice:example.org asked to join"
    (Option.get (find fixture "invite").latest);
  check_string "kick permission admits a lower-powered requester"
    "@alice:example.org asked to join"
    (Option.get (find fixture "kick").latest);
  check_bool "kick requires strictly greater power" true
    (Option.is_none (find fixture "denied").latest);
  check_bool "missing power state stays conservative" true
    (Option.is_none (find fixture "unknown").latest);
  let event = Ui.Presentation.of_event (raw (knock "$direct")) in
  check_bool "the bare predicate refuses a knock" false
    (Ui.Presentation.is_preview_worthy ~own_user:user_id event);
  check_bool "a caller can supply target-aware permission" true
    (Ui.Presentation.is_preview_worthy ~own_user:user_id
       ~can_accept_knock:(Id.User_id.equal (Id.User_id.of_string_exn alice))
       event)

(* Whatever the filter and the sort do, the diff stream must replay to the
   snapshot: a UI that follows the diffs must not drift from one that reads
   [snapshot]. *)

let test_diffs () =
  Eio_main.run @@ fun _ ->
  Eio.Switch.run @@ fun sw ->
  let fixture = sections_fixture () in
  let initial, subscription =
    Ui.Observable.List.subscribe ~sw (Ui.Room_list.rooms fixture.list)
  in
  let collected = ref [] in
  Eio.Fiber.fork ~sw (fun () ->
      let rec loop () =
        match Ui.Observable.List.next subscription with
        | Some diffs ->
            collected := !collected @ diffs;
            loop ()
        | None -> ()
      in
      loop ());
  let step change =
    change ();
    Eio.Fiber.yield ()
  in
  step (fun () -> Ui.Room_list.set_sort fixture.list Ui.Room_list.Name);
  step (fun () -> Ui.Room_list.set_filter fixture.list Filter.Favourite);
  step (fun () ->
      Ui.Room_list.set_filter fixture.list
        (Filter.Any [ Filter.Non_left; Filter.Membership Sync.Left ]));
  step (fun () -> Ui.Room_list.set_sort fixture.list Ui.Room_list.Activity);
  step (fun () -> Ui.Room_list.set_filter fixture.list Filter.Non_left);
  let snapshot =
    Ui.Observable.List.snapshot (Ui.Room_list.rooms fixture.list)
  in
  check_int "the replay has the snapshot's length" (Array.length snapshot)
    (Array.length (Ui.Observable.List.apply_all initial !collected));
  check_bool "and is the snapshot" true
    (Ui.Observable.List.apply_all initial !collected = snapshot);
  Ui.Observable.List.unsubscribe subscription;
  Eio.Fiber.yield ()

let () =
  Alcotest.run "matrix.ui room list"
    [
      ("text", [ Alcotest.test_case "normalization" `Quick test_normalization ]);
      ( "sections",
        [
          Alcotest.test_case "tags, DMs, invites and left rooms" `Quick
            test_sections;
        ] );
      ( "filters",
        [
          Alcotest.test_case "predicates and combinators" `Quick test_filters;
          Alcotest.test_case "space projection and filter" `Quick
            test_space_filter;
          Alcotest.test_case "composed filters" `Quick test_composition;
          Alcotest.test_case "deduplicate room versions" `Quick
            test_deduplicate_versions;
        ] );
      ( "sorters",
        [
          Alcotest.test_case "recency, name and unsent echoes" `Quick
            test_sorting;
          Alcotest.test_case "over a synced state" `Quick test_sorting_live;
        ] );
      ( "previews",
        [
          Alcotest.test_case "eligibility" `Quick test_preview;
          Alcotest.test_case "encryption" `Quick test_preview_decrypted;
          Alcotest.test_case "plaintext edit of encrypted event" `Quick
            test_plaintext_edit_of_decrypted_event_is_rejected;
          Alcotest.test_case "encrypted edit needs new content" `Quick
            test_encrypted_edit_requires_new_content;
          Alcotest.test_case "the summary fallback" `Quick test_preview_fallback;
          Alcotest.test_case "edits resolve onto their target" `Quick
            test_preview_edit;
          Alcotest.test_case "the own user's membership" `Quick
            test_preview_own_membership;
          Alcotest.test_case "an actionable knock" `Quick
            test_preview_actionable_knock;
        ] );
      ("fuzzy", [ Alcotest.test_case "matching and ranking" `Quick test_fuzzy ]);
      ( "marked unread",
        [ Alcotest.test_case "the account-data flag" `Quick test_marked_unread ]
      );
      ("diffs", [ Alcotest.test_case "replay" `Quick test_diffs ]);
    ]
