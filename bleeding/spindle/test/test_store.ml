(* SPDX-License-Identifier: ISC *)
module Store = Spindle__Store

let () =
  Eio_main.run @@ fun env ->
  let name = Filename.temp_file "spindle-replay-" "" in
  Sys.remove name;
  Unix.mkdir name 0o700;
  let directory = Eio.Path.(env#fs / name) in
  let consume store issuer nonce =
    Store.consume store ~now:1000. ~issuer ~jti:nonce ~expires:1060.
  in
  Fun.protect ~finally:(fun () ->
      Eio.Path.read_dir directory
      |> List.iter (fun file -> Eio.Path.unlink Eio.Path.(directory / file));
      Unix.rmdir name)
  @@ fun () ->
  Eio.Switch.run (fun sw ->
      let store = Store.open_ ~sw directory in
      let accepted = ref 0 in
      Eio.Fiber.all
        (List.init 20 (fun _ () ->
             if consume store "issuer-a" "nonce" then incr accepted));
      assert (!accepted = 1);
      assert (consume store "issuer-b" "nonce");
      Store.enqueue store ~source:"knot" ~cursor:"1234567890123456789"
        ~key:"event" ~value:"payload";
      assert (Store.get store "inbox" "event" = Some "payload");
      Store.batch store
        ~puts:[ ("done", "event", "") ]
        ~deletes:[ ("inbox", "event") ];
      Store.enqueue store ~source:"knot" ~cursor:"1234567890123456780"
        ~key:"event" ~value:"payload";
      assert (Store.get store "inbox" "event" = None);
      assert (Store.get store "cursor" "knot" = Some "1234567890123456789");
      Store.batch store
        ~puts:[ ("pipeline", "id", "result"); ("dispatch", "event", "id") ]
        ~deletes:[]);
  Eio.Switch.run (fun sw ->
      let store = Store.open_ ~sw directory in
      assert (not (consume store "issuer-a" "nonce"));
      assert (not (consume store "issuer-b" "nonce"));
      assert (Store.get store "dispatch" "event" = Some "id");
      assert (Store.get store "pipeline" "id" = Some "result");
      assert (
        Store.consume store ~now:1060. ~issuer:"issuer-a" ~jti:"nonce"
          ~expires:1120.));
  print_endline
    "replay store: concurrent use, issuer scope, expiry and reopen passed"
