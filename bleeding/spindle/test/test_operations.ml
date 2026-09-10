(* SPDX-License-Identifier: ISC *)
module J = Spindle__Json
module Store = Spindle__Store
module Health = Spindle__Health
module Recovery = Spindle__Recovery
module Policy = Spindle.Operations
open J

let () =
  Eio_main.run @@ fun env ->
  let name = Filename.temp_file "spindle-operations-" "" in
  Sys.remove name;
  Unix.mkdir name 0o700;
  let directory = Eio.Path.(env#fs / name) in
  Fun.protect ~finally:(fun () ->
      Eio.Process.run env#process_mgr [ "rm"; "-rf"; "--"; name ])
  @@ fun () ->
  Eio.Switch.run (fun sw ->
      let store = Store.open_ ~sw directory in
      let now = Eio.Time.now env#clock in
      let policy =
        Policy.v ~history_limit:1 ~history_megabytes:1 ~receipt_limit:1
          ~inbox_limit:1 ()
      in
      Store.set_limits store policy;
      let pipeline id status payload =
        Store.batch store ~deletes:[]
          ~puts:
            [
              ("pipeline", id, payload);
              ( "pipeline-view",
                id,
                encode
                  (obj
                     [ ("workflows", arr [ obj [ ("status", str status) ] ]) ])
              );
            ]
      in
      pipeline "a" "success" "small";
      pipeline "b" "failed" "small";
      pipeline "pending" "pending" (String.make (2 * 1024 * 1024) 'p');
      let p, _ = Store.prune store ~now policy in
      assert (p = 1 && Store.get store "pipeline" "a" = None);
      assert (Store.get store "pipeline" "b" <> None);
      pipeline "large" "success" (String.make (2 * 1024 * 1024) 'l');
      ignore (Store.prune store ~now policy);
      assert (Store.get store "pipeline" "large" = None);
      assert (Store.get store "pipeline" "pending" <> None);
      let completed key cursor =
        Store.enqueue store ~source:"knot" ~key ~cursor ~value:"{}";
        Store.batch store
          ~puts:[ ("done", key, "") ]
          ~deletes:[ ("inbox", key) ]
      in
      completed "first" "100";
      completed "second" "200";
      Store.enqueue store ~source:"knot" ~key:"pending-event" ~cursor:"150"
        ~value:"{}";
      (match
         Store.enqueue store ~source:"knot" ~key:"full" ~cursor:"300"
           ~value:"{}"
       with
      | () -> failwith "inbox capacity did not apply backpressure"
      | exception Store.Inbox_full -> ());
      assert (Store.get store "cursor" "knot" = Some "200");
      assert (
        Store.consume store ~now ~issuer:"actor" ~jti:"live"
          ~expires:(now +. (100. *. 86400.)));
      let _, r = Store.prune store ~now:(now +. (40. *. 86400.)) policy in
      assert (r = 2);
      assert (Store.get store "inbox" "pending-event" <> None);
      assert (Store.get store "pipeline" "pending" <> None);
      assert (Store.get store "replay-floor" "knot" = Some "200");
      (* Expired receipts cannot cause a late duplicate to be re-enqueued. *)
      Store.enqueue store ~source:"knot" ~key:"first" ~cursor:"100" ~value:"{}";
      assert (Store.get store "inbox" "first" = None);
      assert (
        not
          (Store.consume store ~now ~issuer:"actor" ~jti:"live"
             ~expires:(now +. 1.)));
      Store.delete store "inbox" "pending-event";
      Store.put store "health" "maintenance" "{}";
      let health = Health.v ~store ~enabled:true in
      assert (not (fst (Health.report health ~now)));
      Health.starting health "jetstream" ~now;
      Health.connected health "jetstream" ~now;
      Health.event health "jetstream" ~at:0. ~now;
      Health.activity health "jetstream" ~now:(now +. 60.);
      assert (fst (Health.report health ~now:(now +. 60.)));
      assert (not (fst (Health.report health ~now:(now +. 181.))));
      Health.failed health "jetstream" ~now (Failure "offline");
      assert (not (fst (Health.report health ~now)));
      Health.connected health "jetstream" ~now;
      Store.schedule store "reconcile" "member";
      assert (not (fst (Health.report health ~now)));
      Store.delete store "reconcile" "member";
      Store.enqueue store ~source:"jetstream" ~key:"late" ~cursor:"1000"
        ~value:"{}";
      Health.activity health "jetstream" ~now:(now +. 61.);
      assert (not (fst (Health.report health ~now:(now +. 61.))));
      Store.delete store "inbox" "late";
      let old =
        Int64.to_string (Int64.of_float ((now -. (2. *. 86400.)) *. 1e6))
      in
      Store.put store "cursor" "jetstream" old;
      assert (
        Recovery.resume store policy ~source:"jetstream" ~jetstream:true ~now);
      assert (Store.get store "recover-source" "jetstream" <> None);
      assert (
        get "fromCursor"
          (decode (Option.get (Store.get store "gap" "jetstream")))
        = old);
      Recovery.gap store ~source:"jetstream" ~now:(now +. 10.) ~cursor:"new"
        ~reason:"a later ref mismatch";
      let gap = decode (Option.get (Store.get store "gap" "jetstream")) in
      assert (get "fromCursor" gap = old);
      assert (get "reason" gap = "upstream replay window exceeded");
      Store.put store "done" "legacy" "";
      Store.put store "cursor" "knot" "500";
      ignore (Store.prune store ~now:(now +. (40. *. 86400.)) policy);
      Store.enqueue store ~source:"knot" ~key:"legacy" ~cursor:"400" ~value:"{}";
      assert (Store.get store "inbox" "legacy" = None);
      let hash = String.make 40 'a' in
      Store.checkpoint_ref store ~repo:"repo" ~ref_:"refs/heads/main" ~sha:hash
        ~position:20L;
      Store.checkpoint_ref store ~repo:"repo" ~ref_:"refs/heads/main"
        ~sha:(String.make 40 'b') ~position:10L;
      assert (
        Store.ref_state store ~repo:"repo" ~ref_:"refs/heads/main"
        = Some (hash, 20L));
      let refs =
        Recovery.parse_refs
          ("ref: refs/heads/main\tHEAD\n" ^ hash ^ "\trefs/heads/main\n" ^ hash
         ^ "\trefs/tags/v1\n" ^ String.make 40 'b' ^ "\trefs/tags/v1^{}\n"
         ^ hash ^ "\trefs/notes/commits\n" ^ hash ^ "\trefs/remotes/test/main\n"
          )
      in
      assert (
        refs
        = [ ("refs/heads/main", hash, true); ("refs/tags/v1", hash, false) ]));
  Eio.Switch.run (fun sw ->
      let store = Store.open_ ~sw directory in
      assert (Store.get store "replay-floor" "knot" = Some "500");
      assert (Store.get store "recover-source" "jetstream" <> None);
      assert (Store.ref_state store ~repo:"repo" ~ref_:"refs/heads/main" <> None));
  print_endline
    "operations: retention, backpressure, replay floors, recovery obligations \
     and observer readiness passed"
