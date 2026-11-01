open Crowthebot

let check name condition = if not condition then failwith name

let () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let admin = "@admin:example.org" and room = "!room:example.org" in
  let store =
    Store.create ~now:(fun () -> 0.) (Sqlite3_eio.open_memory ~sw ()) ~admin
  in
  Store.add_room store room;
  let uses () =
    Store.tool_uses store ~day:"1970-01-01" ~after:0 ~through:max_int ~limit:100
  in
  let calls = ref 0 in
  let fetch =
    Fetch_mock.client (fun req ->
        incr calls;
        check "audited before lookup"
          (List.exists
             (fun (u : Store.tool_use) ->
               u.tool = "address_search" && u.status = "running")
             (uses ()));
        Fetch_mock.respond
          {|[{"place_id":1,"lat":"52.2","lon":"0.1","display_name":"ADDRESS_SENTINEL"}]|}
          req)
  in
  let addresses =
    Agentkit_osm.create ~fetch ~clock:(Eio.Stdenv.mono_clock env) ()
  in
  let offered = ref [] and rounds = ref 0 in
  let complete _ tools =
    offered := List.map Agentkit.Agent.Tool.name tools;
    incr rounds;
    if !rounds mod 2 = 1 then
      ( None,
        [
          {
            Agentkit.Agent.id = "lookup";
            name = "address_search";
            arguments = {|{"query":"QUERY_SENTINEL"}|};
          };
        ] )
    else (Some "Found it.", [])
  in
  let engine =
    Engine.create
      ~config:(Config.default ~admin ~homeserver:"https://matrix.example.org")
      ~store ~self:"@crow:example.org" ~plugins:[]
      ~complete:(Fake_model.v complete) ~now:(fun () -> 0.)
    |> fun engine -> Engine.with_addresses engine addresses
  in
  let ask sender id =
    Engine.handle engine ~direct:true
      ~send:(fun _ -> ())
      Engine.{ room; sender; id; body = "!crow ask Look up this address" }
  in
  ask admin "$address";
  check "both tools advertised"
    (List.mem "address_search" !offered && List.mem "address_reverse" !offered);
  check "no OwnTracks dependency" (not (List.mem "location_get" !offered));
  check "lookup performed" (!calls = 1);
  check "audit recorded with private payload omitted"
    (List.exists
       (fun (u : Store.tool_use) ->
         u.tool = "address_search" && u.status = "ok" && u.event = "$address"
         && u.arguments = "[location content omitted]"
         && u.result = "[location content omitted]")
       (uses ()));
  Store.set_person store ~actor:admin ~user:"@friend:example.org"
    ~role:Store.Friend ~allowed:true;
  ask "@friend:example.org" "$friend";
  check "allowed friend" (!calls = 2);
  Store.set_person store ~actor:admin ~user:"@bot:example.org" ~role:Store.Bot
    ~allowed:true;
  ask "@bot:example.org" "$bot";
  check "bot cannot force address lookup"
    (!calls = 2 && not (List.mem "address_search" !offered));
  ask "@stranger:example.org" "$stranger";
  check "unapproved sender cannot lookup" (!calls = 2);
  print_endline "Crow address tools, authority and audit integration passed"
