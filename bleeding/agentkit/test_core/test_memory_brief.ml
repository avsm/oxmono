let check name condition = if not condition then failwith name

let () =
  Eio_main.run @@ fun env ->
  let tmp = Filename.temp_dir "memo-brief" "" in
  let dir = Eio.Path.(Eio.Stdenv.fs env / tmp) in
  Fun.protect
    ~finally:(fun () ->
      List.iter
        (fun name -> Eio.Path.unlink Eio.Path.(dir / name))
        (Eio.Path.read_dir dir);
      Eio.Path.rmdir dir)
    (fun () ->
      let clock = Eio.Stdenv.clock env in
      let memory = Agentkit.Memory.create ~clock dir in
      let write id kind text =
        ignore
          (Agentkit.Memory.write memory ~seq:1 ~cause:"test"
             ~journal:(fun _ -> ())
             ~id ~kind ~title:id ~body:text ~tags:[])
      in
      write "always" Fact "Durable fact must remain pinned.";
      write "unfinished" Open_item "Resume this task.";
      for i = 0 to 23 do
        write (Printf.sprintf "episode-%02d" i) Episode "Observed café visit."
      done;
      let tree =
        Agentkit.Memory.episode_tree (Agentkit.Memory.entries memory)
      in
      ignore
        (Agentkit.Memo.maintain tree ~max_merges:100 ~limit:512
           ~lookup:(fun _ -> None)
           ~save:(fun ~key ~text ->
             Agentkit.Memory.save_summary memory ~key ~text)
           ~summarize:(fun ~limit:_ _ -> "Completed activity summary."));
      let reopened = Agentkit.Memory.create ~clock dir in
      let summaries = Agentkit.Memory.summaries reopened in
      check "derived cache survives restart" (summaries <> []);
      let brief =
        Brief.assemble_with_summaries ~summaries
          ~version:(Agentkit.Memory.version memory)
          ~entries:(Agentkit.Memory.entries memory)
          ~task:"test" ~prompt:"Act" ~session:1 ~history:[]
      in
      let contains text =
        let rec scan i =
          i + String.length text <= String.length brief.user
          && (String.sub brief.user i (String.length text) = text
             || scan (i + 1))
        in
        scan 0
      in
      check "enduring fact stays pinned"
        (contains "Durable fact must remain pinned.");
      check "unfinished task stays pinned" (contains "Resume this task.");
      check "brief uses summary tree" (contains "Completed activity summary.");
      let old_key = List.hd (Agentkit.Memo.keys tree) in
      write "episode-00" Episode "Corrected observation.";
      check "edited range invalidates cache"
        (not (List.mem_assoc old_key (Agentkit.Memory.summaries memory)));
      check "old summary write refused"
        (match
           Agentkit.Memory.save_summary memory ~key:old_key ~text:"stale"
         with
        | () -> false
        | exception Invalid_argument _ -> true);
      check "episode remains readable"
        (List.exists
           (fun (e : Agentkit.Memory.entry) ->
             e.id = "episode-00" && e.body = "Corrected observation.")
           (Agentkit.Memory.entries memory));
      let current_tree =
        Agentkit.Memory.episode_tree (Agentkit.Memory.entries memory)
      in
      let current_key = List.hd (Agentkit.Memo.keys current_tree) in
      Agentkit.Memory.save_summary memory ~key:current_key ~text:"before edit";
      let version = Agentkit.Memory.version memory in
      let path = Eio.Path.(dir / Printf.sprintf "%06d.json" version) in
      let data = Eio.Path.load path in
      let before = "Corrected observation." in
      let rec find i =
        if i + String.length before > String.length data then
          failwith "snapshot text not found"
        else if String.sub data i (String.length before) = before then i
        else find (i + 1)
      in
      let i = find 0 in
      let after = i + String.length before in
      Eio.Path.save ~create:(`Or_truncate 0o600) path
        (String.sub data 0 i ^ "Externally corrected observation."
        ^ String.sub data after (String.length data - after));
      check "same-version source edit invalidates cached summaries"
        (not (List.mem_assoc current_key (Agentkit.Memory.summaries memory)));
      check "same-version stale write refused"
        (match
           Agentkit.Memory.save_summary memory ~key:current_key ~text:"stale"
         with
        | () -> false
        | exception Invalid_argument _ -> true);
      let changed_tree =
        Agentkit.Memory.episode_tree (Agentkit.Memory.entries memory)
      in
      check "deletion during inference refuses stale save"
        (match
           Agentkit.Memo.maintain changed_tree ~max_merges:1 ~limit:512
             ~lookup:(fun _ -> None)
             ~save:(fun ~key ~text ->
               Agentkit.Memory.save_summary memory ~key ~text)
             ~summarize:(fun ~limit:_ _ ->
               ignore
                 (Agentkit.Memory.forget memory ~seq:1 ~cause:"during inference"
                    ~journal:(fun _ -> ()) "episode-00");
               "deleted source summary")
         with
        | _ -> false
        | exception Invalid_argument _ -> true);
      check "deleted source remains absent"
        (not
           (List.exists
              (fun (e : Agentkit.Memory.entry) -> e.id = "episode-00")
              (Agentkit.Memory.entries memory)));
      for i = 0 to 39 do
        write ("large-fact-" ^ string_of_int i) Fact (String.make 10000 'x');
        write
          ("large-task-" ^ string_of_int i)
          Open_item (String.make 10000 'x')
      done;
      let large =
        Brief.assemble
          ~version:(Agentkit.Memory.version memory)
          ~entries:(Agentkit.Memory.entries memory)
          ~task:"test" ~prompt:"Act" ~session:1 ~history:[]
      in
      check "large pinned memory remains bounded" (large.bytes <= 32768);
      check "original pinned records survive shortening"
        (List.exists
           (fun (e : Agentkit.Memory.entry) ->
             e.id = "large-fact-0" && String.length e.body = 10000)
           (Agentkit.Memory.entries memory));
      check "oversized task is refused without truncating instructions"
        (match
           Brief.assemble ~version:1 ~entries:[] ~task:"test"
             ~prompt:(String.make 8193 'x') ~session:1 ~history:[]
         with
        | _ -> false
        | exception Invalid_argument _ -> true);
      Eio.Path.save ~create:(`Or_truncate 0o600)
        Eio.Path.(dir / "summaries.json")
        {|[{"key":"irrelevant","text":""}]|};
      check "invalid cache reports corruption"
        (match Agentkit.Memory.summaries memory with
        | _ -> false
        | exception Agentkit.Memory.Corrupt _ -> true);
      check "obsolete key takes precedence over corrupt cache"
        (match
           Agentkit.Memory.save_summary memory ~key:old_key ~text:"stale"
         with
        | () -> false
        | exception Invalid_argument _ -> true);
      let key =
        Agentkit.Memory.episode_tree (Agentkit.Memory.entries memory)
        |> Agentkit.Memo.keys |> List.hd
      in
      check "current key still reports corrupt cache"
        (match Agentkit.Memory.save_summary memory ~key ~text:"current" with
        | () -> false
        | exception Agentkit.Memory.Corrupt _ -> true));
  print_endline
    "Persistent episode summaries, correction and pinned brief passed."
