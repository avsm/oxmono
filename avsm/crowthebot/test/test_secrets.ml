open Crowthebot

let check name value = if not value then failwith name

let bad f =
  try
    ignore (f ());
    false
  with Invalid_argument _ | Eio.Io _ -> true

let json s = Result.get_ok (Jsont_bytesrw.decode_string Jsont.json s)

let () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let root = Eio.Path.(Eio.Stdenv.cwd env / "test-secret-config") in
  Eio.Path.mkdir ~perm:0o700 root;
  let value = json {|{"password":"fixture-secret"}|} in
  Secret_store.with_dir ~sw
    Eio.Path.(root / "one")
    (fun store ->
      Secret_store.put store ~tool:"owntracks" ~name:"home" ~replace:false value;
      Secret_store.put store ~tool:"owntracks" ~name:"away" ~replace:false value;
      check "first name selected"
        (Secret_store.list store ~tool:"owntracks"
        = [ ("away", false); ("home", true) ]);
      check "tools isolated" (Secret_store.list store ~tool:"openrouter" = []);
      check "duplicate add denied"
        (bad (fun () ->
             Secret_store.put store ~tool:"owntracks" ~name:"home"
               ~replace:false value));
      let changed = json {|{"password":"rotated-secret"}|} in
      Secret_store.put store ~tool:"owntracks" ~name:"home" ~replace:true
        changed;
      check "replace persisted"
        (Secret_store.get store ~tool:"owntracks" ~name:"home" = Some changed);
      Secret_store.rename store ~tool:"owntracks" ~name:"home" ~into:"house";
      check "rename retains selection"
        (Secret_store.selected store ~tool:"owntracks" = Some ("house", changed));
      check "rename collision denied"
        (bad (fun () ->
             Secret_store.rename store ~tool:"owntracks" ~name:"house"
               ~into:"away"));
      Secret_store.remove store ~tool:"owntracks" ~name:"house";
      check "remove selected has no fallback"
        (Secret_store.selected store ~tool:"owntracks" = None);
      Secret_store.select store ~tool:"owntracks" ~name:"away";
      check "select explicit"
        (Secret_store.selected store ~tool:"owntracks" = Some ("away", value));
      List.iter
        (fun name ->
          check "name traversal denied"
            (bad (fun () -> Secret_store.get store ~tool:"owntracks" ~name)))
        [ "../one"; "/tmp/x"; "x.json"; "" ];
      check "tool traversal denied"
        (bad (fun () -> Secret_store.list store ~tool:"../one")));
  Secret_store.with_dir ~sw
    Eio.Path.(root / "two")
    (fun store ->
      check "profiles isolated" (Secret_store.list store ~tool:"owntracks" = []));
  Secret_store.with_dir ~sw
    Eio.Path.(root / "one")
    (fun store ->
      check "reopen retains names"
        (Secret_store.list store ~tool:"owntracks" = [ ("away", true) ]));
  let path = Eio.Path.(root / "one/owntracks.json") in
  check "private file permissions"
    ((Unix.stat (Eio.Path.native_exn path)).st_perm = 0o600);
  Unix.chmod (Eio.Path.native_exn path) 0o644;
  check "permissive secrets rejected"
    (bad (fun () ->
         Secret_store.with_dir ~sw
           Eio.Path.(root / "one")
           (fun store -> Secret_store.list store ~tool:"owntracks")));
  Unix.chmod (Eio.Path.native_exn path) 0o600;
  Eio.Path.rename path Eio.Path.(root / "one/real.json");
  Eio.Path.symlink ~link_to:"real.json" path;
  check "secret symlinks rejected"
    (bad (fun () ->
         Secret_store.with_dir ~sw
           Eio.Path.(root / "one")
           (fun store -> Secret_store.list store ~tool:"owntracks")));
  Eio.Path.unlink path;
  Eio.Path.rename Eio.Path.(root / "one/real.json") path;
  Eio.Path.symlink ~link_to:"one" Eio.Path.(root / "alias");
  check "secret directory aliases rejected"
    (bad (fun () -> Secret_store.with_dir ~sw Eio.Path.(root / "alias") ignore));
  Eio.Path.unlink Eio.Path.(root / "alias");
  List.iter
    (fun profile ->
      List.iter
        (fun name -> Eio.Path.unlink Eio.Path.(root / profile / name))
        (Eio.Path.read_dir Eio.Path.(root / profile));
      Eio.Path.rmdir Eio.Path.(root / profile))
    [ "one"; "two" ];
  Eio.Path.rmdir root;
  let request_count = ref 0 in
  let mock =
    Fetch_mock.client (fun req ->
        incr request_count;
        check "bound model bearer"
          (Http.Header.get req.headers "authorization"
          = Some "Bearer fixture-key");
        check "model method restricted" (req.meth = `POST);
        Fetch_mock.respond ~status:307
          ~headers:
            (Http.Header.of_list
               [ ("Location", "https://elsewhere.example/v1/chat/completions") ])
          "" req)
  in
  let settings =
    json
      {|{"url":"https://model.example/v1","api_key":"fixture-key","allow_http":false}|}
  in
  let model = Model_config.initialize ~fetch:mock settings in
  check "model cannot redirect credentials to another endpoint"
    (bad (fun () ->
         Openrouter.Chat.complete model
           (Openrouter.Chat.request ~model:"test"
              ~messages:[ Openrouter.Message.user "hello" ]
              ()))
    && !request_count = 1);
  check "HTTP credentials require explicit operator choice"
    (bad (fun () ->
         Model_config.initialize ~fetch:mock
           (json
              {|{"url":"http://model.example/v1","api_key":"fixture-key","allow_http":false}|})));
  print_endline
    "crowthebot: named private configurations and credential scope passed"
