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
  let plugin = Plugin.blogroll ~fetch ~now:(fun () -> 0.) in
  let calls = ref 0 in
  let plugin =
    {
      plugin with
      run =
        (fun ~query ->
          incr calls;
          plugin.run ~query);
    }
  in
  let engine =
    Engine.create ~config ~store ~self:"@crow:example.org" ~plugins:[ plugin ]
      ~complete:(App.complete env config client) ~now:(fun () -> 0.)
  in
  Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 180. (fun () ->
      Engine.handle engine ~send:print_endline
        {
          room = "!probe:example.org";
          sender = config.admin;
          id = "$probe";
          body =
            "!crow ask Use the blogroll tool to find Anil Madhavapeddy's feed. \
             Report its URL.";
        });
  if !calls = 0 then failwith "model did not call the blogroll tool";
  Printf.printf
    "Live Crow model/tool/delivery workflow passed (%d tool calls).\n" !calls
