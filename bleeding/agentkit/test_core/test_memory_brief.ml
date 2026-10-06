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
      for i = 0 to 39 do
        write ("large-fact-" ^ string_of_int i) Fact (String.make 10000 'x');
        write ("large-task-" ^ string_of_int i) Open_item (String.make 10000 'x')
      done;
      let large = Brief.assemble ~version:(Agentkit.Memory.version memory)
          ~entries:(Agentkit.Memory.entries memory) ~task:"test" ~prompt:"Act"
          ~session:1 ~history:[] in
      check "large pinned memory remains bounded" (large.bytes <= 32768);
      check "original pinned records survive shortening"
        (List.exists (fun (e : Agentkit.Memory.entry) ->
             e.id = "large-fact-0" && String.length e.body = 10000)
           (Agentkit.Memory.entries memory));
      check "oversized task is refused without truncating instructions"
        (match Brief.assemble ~version:1 ~entries:[] ~task:"test"
             ~prompt:(String.make 8193 'x') ~session:1 ~history:[] with
        | _ -> false | exception Invalid_argument _ -> true);
      Eio.Path.save ~create:(`Or_truncate 0o600)
        Eio.Path.(dir / "summaries.json")
        {|[{"key":"irrelevant","text":""}]|};
      check "invalid cache reports corruption"
        (match Agentkit.Memory.summaries memory with
        | _ -> false
        | exception Agentkit.Memory.Corrupt _ -> true));
  print_endline
    "Persistent episode summaries, correction and pinned brief passed."
