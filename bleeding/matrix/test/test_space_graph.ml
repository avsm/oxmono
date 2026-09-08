module Id = Matrix_proto.Id
module Event = Matrix_proto.Event
module Store = Matrix_client.Store
module Base_client = Matrix_client.Base_client
module Graph = Matrix_client.Space_graph

let rid value = Result.get_ok (Id.Room_id.of_string value)
let uid value = Result.get_ok (Id.User_id.of_string value)
let own_user = uid "@me:example.org"

let obj members =
  Jsont.Json.object'
    (List.map
       (fun (name, value) -> Jsont.Json.mem (Jsont.Json.name name) value)
       members)

let strings values =
  Jsont.Json.array (Array.of_list (List.map Jsont.Json.string values))

let event ?(timestamp = 1L) event_type state_key content : Store.state_event =
  {
    event_type = Event.Event_type.of_string event_type;
    state_key;
    content;
    sender = Some own_user;
    event_id = None;
    origin_server_ts = Some (Event.Timestamp.of_ms timestamp);
  }

let child ?order ?(via = [ "example.org" ]) ?timestamp room =
  let members =
    ("via", strings via)
    :: Option.fold ~none:[]
         ~some:(fun order -> [ ("order", Jsont.Json.string order) ])
         order
  in
  event ?timestamp "m.space.child" (Id.Room_id.to_string room) (obj members)

let parent ?(via = [ "example.org" ]) room =
  event "m.space.parent"
    (Id.Room_id.to_string room)
    (obj [ ("via", strings via) ])

let state rooms =
  let store = Store.memory () in
  List.iter
    (fun (room_id, membership, state_events) ->
      let room = Store.empty_room_info ~room_id ~membership in
      Store.set_room store { room with state_events })
    rooms;
  Base_client.of_store store ~user_id:own_user ()

let ids rooms = List.map Id.Room_id.to_string rooms

let check_ids label expected actual =
  Alcotest.(check (list string)) label expected (ids actual)

let test_mutual_links_only () =
  let space = rid "!space:example.org" in
  let child_room = rid "!child:example.org" in
  let one_sided = rid "!one:example.org" in
  let removed = rid "!removed:example.org" in
  let graph =
    state
      [
        ( space,
          Store.Joined,
          [
            child child_room;
            child one_sided;
            child ~via:[] removed;
            event "m.space.child" "not-a-room-id"
              (obj [ ("via", strings [ "example.org" ]) ]);
          ] );
        (child_room, Store.Joined, [ parent space ]);
        (one_sided, Store.Joined, []);
        (removed, Store.Joined, [ parent space ]);
      ]
    |> Graph.of_state
  in
  check_ids "only the mutual, live link" [ "!child:example.org" ]
    (Graph.children graph space);
  check_ids "child has its accepted parent" [ "!space:example.org" ]
    (Graph.parents graph child_room);
  check_ids "one-sided and removed children are roots"
    [ "!one:example.org"; "!removed:example.org"; "!space:example.org" ]
    (Graph.roots graph)

let test_left_parent_promotes_child () =
  let old_space = rid "!old:example.org" in
  let child_room = rid "!child:example.org" in
  let graph =
    state
      [
        (old_space, Store.Left, [ child child_room ]);
        (child_room, Store.Joined, [ parent old_space ]);
      ]
    |> Graph.of_state
  in
  check_ids "left parent is not retained" [] (Graph.parents graph child_room);
  check_ids "joined child is top-level" [ "!child:example.org" ]
    (Graph.roots graph)

let test_cycle_is_broken_deterministically () =
  let a = rid "!a:example.org" in
  let b = rid "!b:example.org" in
  let c = rid "!c:example.org" in
  let graph =
    state
      [
        (a, Store.Joined, [ child b; parent c ]);
        (b, Store.Joined, [ child c; parent a ]);
        (c, Store.Joined, [ child a; parent b ]);
      ]
    |> Graph.of_state
  in
  check_ids "lexically later closing edge is dropped" [] (Graph.parents graph a);
  check_ids "first edge remains" [ "!a:example.org" ] (Graph.parents graph b);
  check_ids "second edge remains" [ "!b:example.org" ] (Graph.parents graph c);
  check_ids "one deterministic root" [ "!a:example.org" ] (Graph.roots graph);
  check_ids "cycle-free preorder"
    [ "!a:example.org"; "!b:example.org"; "!c:example.org" ]
    (Graph.flattened_subtree graph a)

let test_multiple_parents_and_child_order () =
  let p = rid "!p:example.org" in
  let q = rid "!q:example.org" in
  let a = rid "!a:example.org" in
  let b = rid "!b:example.org" in
  let early = rid "!early:example.org" in
  let late = rid "!late:example.org" in
  let graph =
    state
      [
        ( p,
          Store.Joined,
          [
            child ~order:"b" ~timestamp:1L b;
            child ~order:"a" ~timestamp:50L a;
            child ~timestamp:20L late;
            child ~timestamp:10L early;
          ] );
        (q, Store.Joined, [ child a ]);
        (a, Store.Joined, [ parent p; parent q ]);
        (b, Store.Joined, [ parent p ]);
        (early, Store.Joined, [ parent p ]);
        (late, Store.Joined, [ parent p ]);
      ]
    |> Graph.of_state
  in
  check_ids "explicit order, then timestamp"
    [
      "!a:example.org";
      "!b:example.org";
      "!early:example.org";
      "!late:example.org";
    ]
    (Graph.children graph p);
  check_ids "multiple parents are retained"
    [ "!p:example.org"; "!q:example.org" ]
    (Graph.parents graph a);
  check_ids "both parents are roots"
    [ "!p:example.org"; "!q:example.org" ]
    (Graph.roots graph);
  check_ids "flatten follows the same child order"
    [
      "!p:example.org";
      "!a:example.org";
      "!b:example.org";
      "!early:example.org";
      "!late:example.org";
    ]
    (Graph.flattened_subtree graph p)

let () =
  Alcotest.run "space graph"
    [
      ( "graph",
        [
          Alcotest.test_case "mutual links only" `Quick test_mutual_links_only;
          Alcotest.test_case "left parent promotes child" `Quick
            test_left_parent_promotes_child;
          Alcotest.test_case "cycle handling" `Quick
            test_cycle_is_broken_deterministically;
          Alcotest.test_case "multiple parents and order" `Quick
            test_multiple_parents_and_child_order;
        ] );
    ]
