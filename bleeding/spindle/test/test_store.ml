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
      Store.enqueue store ~source:"knot" ~cursor:"1234567890123456789"
        ~key:"event" ~value:"conflicting duplicate";
      assert (Store.get store "inbox" "event" = Some "payload");
      Store.batch store
        ~puts:[ ("done", "event", "") ]
        ~deletes:[ ("inbox", "event") ];
      Store.enqueue store ~source:"knot" ~cursor:"1234567890123456780"
        ~key:"event" ~value:"payload";
      assert (Store.get store "inbox" "event" = None);
      assert (Store.get store "cursor" "knot" = Some "1234567890123456789");
      Store.batch store ~deletes:[]
        ~puts:(List.init 300 (fun i -> ("tasks", Printf.sprintf "%03d" i, "")));
      let keys =
        Store.fold store "tasks" ~init:[]
          ~f:(fun keys (key, _) -> key :: keys)
          ()
      in
      assert (List.length keys = 300 && List.hd keys = "299");
      let keys =
        Store.fold store "tasks" ~descending:true ~init:[]
          ~f:(fun keys (key, _) -> key :: keys)
          ()
      in
      assert (List.length keys = 300 && List.hd keys = "000");
      assert (List.length (Store.ready store "tasks" ~now:1000. ~limit:64) = 64);
      Store.defer store "tasks" "000" ~now:1000.;
      assert (
        fst (List.hd (Store.ready store "tasks" ~now:1000. ~limit:1)) = "001");
      assert (
        fst (List.hd (Store.ready store "tasks" ~now:1002. ~limit:1)) = "000");
      Store.schedule store "refresh" "key";
      let old = Option.get (Store.get store "refresh" "key") in
      Store.schedule store "refresh" "key";
      assert (
        not
          (Store.complete store "refresh" "key" ~value:old
             ~puts:[ ("grant", "stale", "bad") ]
             ~deletes:[]));
      assert (Store.get store "grant" "stale" = None);
      let current = Option.get (Store.get store "refresh" "key") in
      assert (
        Store.complete store "refresh" "key" ~value:current
          ~puts:[ ("grant", "current", "ok") ]
          ~deletes:[]);
      assert (Store.get store "refresh" "key" = None);
      Store.batch store
        ~puts:[ ("pipeline", "id", "result"); ("dispatch", "event", "id") ]
        ~deletes:[];
      let first =
        Store.append_log store ~pipeline:"id" ~workflow:"job" "first"
      in
      let later =
        Store.append_log store ~pipeline:"id" ~workflow:"job" "later"
      in
      (* A snapshot captured before an append must leave that append in the
         journal, even when it commits after the append. *)
      Store.batch
        ~logs:[ ("id", "job", first) ]
        store
        ~puts:[ ("pipeline", "id", "result") ]
        ~deletes:[];
      assert (
        Store.logs store ~pipeline:"id" ~workflow:"job" = [ (later, "later") ]));
  Eio.Switch.run (fun sw ->
      let store = Store.open_ ~sw directory in
      assert (not (consume store "issuer-a" "nonce"));
      assert (not (consume store "issuer-b" "nonce"));
      assert (Store.get store "dispatch" "event" = Some "id");
      assert (Store.get store "pipeline" "id" = Some "result");
      let seq =
        match Store.logs store ~pipeline:"id" ~workflow:"job" with
        | [ (seq, "later") ] -> seq
        | _ -> assert false
      in
      Store.batch ~logs:[ ("id", "job", seq) ] store ~puts:[] ~deletes:[];
      let next = Store.append_log store ~pipeline:"id" ~workflow:"job" "next" in
      assert (next > seq);
      Store.batch ~logs:[ ("id", "job", seq) ] store ~puts:[] ~deletes:[];
      assert (
        Store.logs store ~pipeline:"id" ~workflow:"job" = [ (next, "next") ]);
      Store.delete store "pipeline" "id";
      assert (Store.logs store ~pipeline:"id" ~workflow:"job" = []);
      assert (
        not
          (List.mem_assoc "000"
             (Store.ready store "tasks" ~now:1000. ~limit:128)));
      Store.defer store "tasks" "000" ~now:1001.;
      assert (
        not
          (List.mem_assoc "000"
             (Store.ready store "tasks" ~now:1002. ~limit:128)));
      Store.delete store "tasks" "000";
      Store.put store "tasks" "000" "new";
      assert (
        fst (List.hd (Store.ready store "tasks" ~now:1002. ~limit:1)) = "001");
      assert (Store.get store "tasks" "000" = Some "new");
      assert (
        Store.consume store ~now:1060. ~issuer:"issuer-a" ~jti:"nonce"
          ~expires:1120.));
  print_endline
    "replay store: concurrent use, issuer scope, expiry and reopen passed"
