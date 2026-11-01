(* Synthetic identity and an in-memory store. No Matrix connection or profile. *)
let () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let open Crowthebot in
  let config =
    {
      (Config.default ~admin:"@admin:example.org"
         ~homeserver:"https://matrix.example.org")
      with
      base_url = "http://sequoia:8000/v1";
      allow_http_model = true;
    }
  in
  let store =
    Store.create (Sqlite3_eio.open_memory ~sw ()) ~admin:config.admin
  in
  Store.add_room store "!probe:example.org";
  let fetch = Fetch_httpz.std ~cookies:`Off env in
  let model = Openrouter.of_fetch ~base_url:config.base_url fetch in
  let addresses =
    Agentkit_osm.create ~fetch ~clock:(Eio.Stdenv.mono_clock env) ()
  in
  let engine =
    Engine.create ~config ~store ~self:"@crow:example.org" ~plugins:[]
      ~complete:(App.complete env config model) ~now:(fun () -> 0.)
    |> fun engine -> Engine.with_addresses engine addresses
  in
  Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 180. (fun () ->
      Engine.handle engine ~direct:true ~send:print_endline
        {
          room = "!probe:example.org";
          sender = config.admin;
          id = "$address-probe";
          body =
            "Use address_search to look up King's College, Cambridge, UK. Also \
             use address_reverse for latitude 52.2053 longitude 0.1218. Report \
             both results with source links.";
        });
  let day = String.sub (Store.timestamp (Store.now store)) 0 10 in
  let uses = Store.tool_uses store ~day ~after:0 ~through:max_int ~limit:100 in
  List.iter
    (fun name ->
      if
        not
          (List.exists
             (fun (u : Store.tool_use) -> u.tool = name && u.status = "ok")
             uses)
      then failwith ("Model did not successfully invoke " ^ name))
    Agentkit_osm.names;
  print_endline "Live Crow address search and reverse lookup passed."
