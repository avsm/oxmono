(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* The journal: its codec, its segments and its refusals.

   The properties that matter are mostly negative ones. A kind this build does
   not know must survive a read and a write with its JSON intact, since a later
   journal read by an earlier build has to show everything in it. A schema
   version above what this build reads must stop the read rather than be shown
   under a shape that may have moved. A record that cannot be written must
   raise, since an account with a hole in it reads as complete.

   The rest is arithmetic that has to hold across a crash: sequence numbers are
   gapless over a midnight segment roll and over a restart, which is what the
   recovery from the last line of the newest segment is for. *)

module Journal = Agentkit.Journal

let failures = ref 0

let check name cond =
  if cond then Printf.printf "ok   - %s\n" name
  else begin
    incr failures;
    Printf.printf "FAIL - %s\n" name
  end

let contains s sub =
  let n = String.length s and m = String.length sub in
  let rec at i = i + m <= n && (String.sub s i m = sub || at (i + 1)) in
  at 0

(* 2026-08-08T09:14:07Z, the design's own example, and the two instants either
   side of the midnight that follows it. *)
let t0 = 1786180447.
let before_midnight = 1786233599.
let after_midnight = 1786233601.

let record kind =
  {
    Journal.v = Journal.schema_version;
    seq = 412;
    time = "2026-08-08T09:14:07Z";
    run = 9;
    kind;
  }

let round_trip name kind =
  let r = record kind in
  match Journal.of_string (Journal.to_string r) with
  | Ok r' -> check name (r' = r)
  | Error e -> check (name ^ " (" ^ e ^ ")") false

(* Every kind, since a shape that does not survive a round trip is a record
   nobody can read back. *)

let () =
  round_trip "run_start round trips"
    (Journal.Run_start
       {
         pid = 4211;
         version = "0.1.0";
         backend = "metal";
         model = "/models/ds4.gguf";
         ctx_size = 32768;
       });
  round_trip "run_stop round trips" (Journal.Run_stop "sigterm");
  round_trip "wake round trips"
    (Journal.Wake
       {
         task = "feeds";
         due = "2026-08-08T09:00:00Z";
         why = "due";
         serial = None;
       });
  round_trip "wake with a serial round trips"
    (Journal.Wake
       {
         task = "feeds";
         due = "2026-08-08T09:00:00Z";
         why = "run_now";
         serial = Some 3;
       });
  round_trip "brief round trips"
    (Journal.Brief { version = 17; open_items = 4; bytes = 8192 });
  round_trip "prompt round trips" (Journal.Prompt "Check the feeds.");
  round_trip "reasoning round trips" (Journal.Reasoning "The tag moved.");
  round_trip "content round trips" (Journal.Content "Done.");
  round_trip "tool_call round trips"
    (Journal.Tool_call
       { call = 3; name = "fetch"; arguments = {|{"url":"https://x/"}|} });
  round_trip "tool_result round trips"
    (Journal.Tool_result
       {
         call = 3;
         name = "fetch";
         output = "200 OK\n";
         seconds = 1.25;
         truncated = false;
       });
  round_trip "stats round trips"
    (Journal.Stats
       {
         Agentkit.Agent.ctx_used = 9000;
         ctx_size = 32768;
         prompt_tokens = 8000;
         generated = 700;
         generate_seconds = 12.5;
         prefill_seconds = 3.25;
         tool_calls = 2;
         turns = 3;
         drafted = 500;
         total_generated = 4200;
         total_generate_seconds = 88.125;
       });
  round_trip "expanded round trips" (Journal.Expanded 65536);
  round_trip "squeezed round trips" (Journal.Squeezed 128);
  round_trip "cut_off round trips"
    (Journal.Cut_off { tokens = 2048; tool_call = true });
  round_trip "compacted round trips"
    (Journal.Compacted
       {
         Agentkit.Agent.before = 30000;
         after = 8200;
         summary = "Read three files.";
       });
  round_trip "continued round trips"
    (Journal.Continued { task = "feeds"; session = 2; previous = 1 });
  round_trip "memory_write round trips"
    (Journal.Memory_write
       { from = 16; to_ = 17; entry = "ds4-upstream"; why = "tag moved" });
  round_trip "handover round trips" (Journal.Handover 17);
  round_trip "error round trips"
    (Journal.Error { where = "fetch"; what = "connection refused" });
  round_trip "schedule_load round trips"
    (Journal.Schedule_load
       { tasks = [ "feeds"; "digest" ]; changed = [ "digest" ] })

(* The wire, against the design's own example. A change of member order or of
   the kind member's shape shows up here. *)

let () =
  let line =
    Journal.to_string
      (record
         (Journal.Tool_call
            { call = 3; name = "fetch"; arguments = {|{"url":"https://x/"}|} }))
  in
  check "the record's members are v, seq, t, run and then the kind"
    (line
   = {|{"v":1,"seq":412,"t":"2026-08-08T09:14:07Z","run":9,"tool_call":{"call":3,"name":"fetch","arguments":"{\"url\":\"https://x/\"}"}}|}
    )

(* Bytes that are not valid UTF-8 reach the journal in tool output and in model
   replies. The record has to survive them rather than be refused. *)

let () =
  let r = record (Journal.Content "a\xffb") in
  match Journal.of_string (Journal.to_string r) with
  | Ok { kind = Journal.Content c; _ } ->
      check "an invalid byte is written as U+FFFD" (c = "a\xef\xbf\xbdb")
  | Ok _ | Error _ -> check "an invalid byte is written as U+FFFD" false

(* [drafted] joined the stats shape after journals with it already existed, so
   a record with no such member is a version predating speculative decoding
   rather than a fault, and it held nothing drafted. *)

let () =
  let line =
    {|{"v":1,"seq":1,"t":"2026-08-08T09:14:07Z","run":1,"stats":{"ctx_used":100,"ctx_size":32768,"prompt_tokens":90,"generated":10,"generate_seconds":1.0,"prefill_seconds":0.5,"tool_calls":0,"turns":1,"total_generated":10,"total_generate_seconds":1.0}}|}
  in
  match Journal.of_string line with
  | Error e ->
      check ("a stats record with no drafted member decodes (" ^ e ^ ")") false
  | Ok { kind = Journal.Stats s; _ } ->
      check "a stats record with no drafted member reads as none drafted"
        (s.Agentkit.Agent.drafted = 0)
  | Ok _ -> check "a stats record with no drafted member decodes as stats" false

(* A kind this build does not know is carried whole. *)

let () =
  let line =
    {|{"v":1,"seq":7,"t":"2026-08-08T09:14:07Z","run":1,"gossip":{"n":3,"of":["a","b"]}}|}
  in
  match Journal.of_string line with
  | Error e -> check ("an unknown kind decodes (" ^ e ^ ")") false
  | Ok r ->
      check "an unknown kind keeps its name"
        (Journal.kind_name r.Journal.kind = "gossip");
      check "an unknown kind keeps its raw JSON" (Journal.to_string r = line);
      check "an unknown kind keeps the common members"
        (r.Journal.seq = 7 && r.Journal.run = 1)

(* A schema version above what this build reads stops the read. *)

let () =
  let line =
    {|{"v":2,"seq":1,"t":"2026-08-08T09:14:07Z","run":1,"prompt":{"text":"x"}}|}
  in
  match Journal.of_string line with
  | Ok _ -> check "a record above the schema version is refused" false
  | Error e ->
      check "a record above the schema version is refused"
        (contains e "schema version 2" && contains e "reads up to 1")

(* A record names exactly one kind. *)

let () =
  let no_kind = {|{"v":1,"seq":1,"t":"2026-08-08T09:14:07Z","run":1}|} in
  let two_kinds =
    {|{"v":1,"seq":1,"t":"2026-08-08T09:14:07Z","run":1,"prompt":{"text":"x"},"handover":{"version":1}}|}
  in
  check "a record with no kind is refused"
    (match Journal.of_string no_kind with
    | Error e -> contains e "naming its kind"
    | Ok _ -> false);
  check "a record naming two kinds is refused"
    (match Journal.of_string two_kinds with
    | Error e -> contains e "names one kind" && contains e "prompt, handover"
    | Ok _ -> false)

(* Segments, sequence numbers and recovery, on a real directory. *)

let seqs dir =
  let seen = ref [] in
  Journal.iter dir (fun r -> seen := r.Journal.seq :: !seen);
  List.rev !seen

let store dir =
  let clock = Eio_mock.Clock.make () in
  Eio_mock.Clock.set_time clock t0;
  check "an absent journal starts at one"
    (Journal.recover Eio.Path.(dir / "journal")
    = { Journal.next_seq = 1; next_run = 1 });
  let journal = Eio.Path.(dir / "journal") in
  (* The first run, which spans a midnight. *)
  Eio.Switch.run (fun sw ->
      let t = Journal.create ~sw ~clock ~run:1 ~seq:1 journal in
      check "the writer says which number comes next" (Journal.next_seq t = 1);
      ignore (Journal.append t (Journal.Prompt "one"));
      ignore (Journal.append t (Journal.Content "two"));
      Eio_mock.Clock.set_time clock before_midnight;
      ignore (Journal.append t (Journal.Prompt "three"));
      Eio_mock.Clock.set_time clock after_midnight;
      ignore (Journal.append t (Journal.Content "four"));
      let r = Journal.append t (Journal.Handover 1) in
      check "a record is stamped with the run" (r.Journal.run = 1);
      check "seq advances by one per record" (r.Journal.seq = 5);
      Journal.close t);
  check "midnight rolls to a new segment"
    (Journal.segments journal = [ "2026-08-08.jsonl"; "2026-08-09.jsonl" ]);
  check "seq carries across the roll" (seqs journal = [ 1; 2; 3; 4; 5 ]);
  (* A restart recovers both numbers from the last line of the newest
     segment, since there is no counter file to disagree with it. *)
  let next = Journal.recover journal in
  check "recovery reads the next seq from the journal"
    (next = { Journal.next_seq = 6; next_run = 2 });
  Eio.Switch.run (fun sw ->
      let t =
        Journal.create ~sw ~clock ~run:next.Journal.next_run
          ~seq:next.Journal.next_seq journal
      in
      let r = Journal.append t (Journal.Run_stop "done") in
      check "a restart continues the sequence" (r.Journal.seq = 6);
      check "a restart takes the next run number" (r.Journal.run = 2);
      Journal.close t);
  check "seq is gapless across a restart" (seqs journal = [ 1; 2; 3; 4; 5; 6 ]);
  (* Recovery and ownership are one operation for shared command journals. *)
  let owned = Eio.Path.(dir / "owned") in
  Eio.Switch.run (fun sw ->
      let first = Journal.open_ ~sw ~clock owned in
      let r = Journal.append first (Journal.Prompt "owned") in
      check "an owned journal starts at run one" (r.Journal.run = 1);
      check "a second writer in one process is refused"
        (match Journal.open_ ~sw ~clock owned with
        | _ -> false
        | exception Journal.Locked _ -> true);
      Journal.close first;
      let second = Journal.open_ ~sw ~clock owned in
      let r = Journal.append second (Journal.Run_stop "done") in
      check "ownership can pass to the next run" (r.Journal.run = 2));
  (* Filtering by kind, which is what a digest and a filtered log want. *)
  let names = ref [] in
  Journal.iter ~kinds:[ "prompt"; "handover" ] journal (fun r ->
      names := Journal.kind_name r.Journal.kind :: !names);
  check "iter filters by kind"
    (List.rev !names = [ "prompt"; "prompt"; "handover" ]);
  (* A line that does not decode stops the read and says where it was. *)
  Eio.Path.save ~append:true ~create:(`If_missing 0o600)
    Eio.Path.(journal / "2026-08-09.jsonl")
    "{\"v\":1,\"seq\":7}\n";
  check "a line that does not decode stops the read"
    (match seqs journal with
    | _ -> false
    | exception Journal.Bad_record { segment; line; _ } ->
        segment = "2026-08-09.jsonl" && line = 4);
  (* A journal that cannot be written stops the run. The record must raise
     rather than be reported as a value a caller can drop on the floor. *)
  let closed = Eio.Path.(dir / "closed") in
  Eio.Path.mkdirs ~exists_ok:true ~perm:0o700 closed;
  Eio.Switch.run (fun sw ->
      let t = Journal.create ~sw ~clock ~run:1 ~seq:1 closed in
      Unix.chmod (Eio.Path.native_exn closed) 0o500;
      let raised =
        match Journal.append t (Journal.Prompt "x") with
        | _ -> false
        | exception _ -> true
      in
      Unix.chmod (Eio.Path.native_exn closed) 0o700;
      check "an unwritable journal makes append raise" raised;
      check "a failed append does not consume the sequence number"
        (Journal.next_seq t = 1))

let run env =
  let fs = Eio.Stdenv.fs env in
  let tmp = Filename.temp_file "ds4-journal" "" in
  Sys.remove tmp;
  let dir = Eio.Path.(fs / tmp) in
  Eio.Path.mkdirs ~exists_ok:true ~perm:0o700 dir;
  Fun.protect
    ~finally:(fun () ->
      ignore (Sys.command (Filename.quote_command "rm" [ "-rf"; tmp ])))
    (fun () -> store dir)

let () =
  (* Running as root would write through the unwritable directory, so the one
     negative property this file exists for would pass without being tested. *)
  if Unix.geteuid () = 0 then
    print_string "skipped: the journal tests need a non-root user\n"
  else begin
    Eio_main.run run;
    if !failures = 0 then print_string "\nAll tests passed.\n"
    else begin
      Printf.printf "\n%d check(s) failed.\n" !failures;
      exit 1
    end
  end
