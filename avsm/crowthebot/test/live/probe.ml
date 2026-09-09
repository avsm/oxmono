(* Opt-in. Uses synthetic identities, an in-memory database and a printing
   delivery callback. No Matrix connection or local profile is opened. *)
let () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let open Crowthebot in
  let config =
    Config.default ~admin:"@admin:example.org"
      ~homeserver:"https://matrix.example.org"
  in
  let store =
    Store.create (Sqlite3_eio.open_memory ~sw ()) ~admin:config.admin
  in
  Store.add_room store "!probe:example.org";
  let fetch = Fetch_httpz.std ~cookies:`Off env in
  let client = Openrouter.of_fetch ~base_url:config.base_url fetch in
  let feeds =
    Feeds.create ~state:(Store.feeds store)
      ~download:(fun ~url:_ ~etag:_ ~last_modified:_ ->
        failwith "probe must not fetch feeds")
  in
  let calls = ref 0 in
  let engine =
    Engine.create ~config ~store ~self:"@crow:example.org" ~plugins:[]
      ~complete:(fun messages tools ->
        let text, tool_calls = App.complete env config client messages tools in
        List.iter
          (fun (call : Openrouter.Tool.call) ->
            if call.name = "feeds_list" then incr calls)
          tool_calls;
        (text, tool_calls))
      ~now:(fun () -> 0.)
    |> fun engine -> Engine.with_feeds engine feeds
  in
  Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 180. (fun () ->
      Engine.handle engine ~send:print_endline
        {
          room = "!probe:example.org";
          sender = config.admin;
          id = "$probe";
          body =
            "!crow ask Use feeds_list to check my subscriptions. Report what \
             you find.";
        });
  if !calls = 0 then failwith "model did not call the feed tool";
  Printf.printf
    "Live Crow model/tool/delivery workflow passed (%d tool calls).\n" !calls
