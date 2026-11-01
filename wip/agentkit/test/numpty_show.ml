(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* What [numpty log] and [numpty memory] print, against a store this test writes
   with the same writers a run uses. Neither takes the lock, so both work while
   numpty runs, while it is stopped, and on a store copied off the machine.

   The property worth pinning is that nothing is dropped. A log that skipped a
   record, a kind this build does not know among them, would read as complete,
   which is the failure the journal's forward compatibility exists against. The
   filters are the other half: a person asking for one task's wake-up must get
   that wake-up's records and not the next one's. *)

module Journal = Agentkit.Journal
module Memory = Agentkit.Memory
module Show = Numpty_daemon.Show
module Store = Numpty_daemon.Store

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

let t0 = 1786180447. (* 2026-08-08T09:14:07Z *)
let later = t0 +. 3600.

let lines ?since ?kinds ?task ?run dir =
  let out = ref [] in
  Show.log ?since ?kinds ?task ?run dir (fun l -> out := l :: !out);
  List.rev !out

let some_stats =
  {
    Agentkit.Agent.ctx_used = 8000;
    ctx_size = 32768;
    prompt_tokens = 7000;
    generated = 120;
    generate_seconds = 4.5;
    prefill_seconds = 1.5;
    tool_calls = 1;
    turns = 2;
    drafted = 0;
    total_generated = 120;
    total_generate_seconds = 4.5;
  }

(* A store two wake-ups have run against, written with the writers a run uses so
   that what is printed is what a run would have left. *)
let build ~clock dir =
  Eio.Switch.run @@ fun sw ->
  let j = Journal.create ~sw ~clock ~run:9 ~seq:1 (Store.journal_dir dir) in
  let m = Memory.create ~clock (Store.memory_dir dir) in
  let append k = ignore (Journal.append j k) in
  append
    (Journal.Run_start
       {
         Journal.pid = 4321;
         version = "0.1";
         backend = "CPU";
         model = "/models/ds4.gguf";
         ctx_size = 32768;
       });
  append
    (Journal.Schedule_load
       { Journal.tasks = [ "feeds"; "digest" ]; changed = [ "feeds" ] });
  append
    (Journal.Wake
       {
         Journal.task = "feeds";
         due = Journal.rfc3339 t0;
         why = "run_now";
         serial = Some 3;
       });
  append (Journal.Brief { Journal.version = 0; open_items = 0; bytes = 1234 });
  append (Journal.Prompt "the whole brief");
  append
    (Journal.Tool_call
       {
         Journal.call = 1;
         name = "fetch";
         arguments = {|{"url":"https://x/"}|};
       });
  append
    (Journal.Tool_result
       {
         Journal.call = 1;
         name = "fetch";
         output = "200 https://x/\ntext/html\nthe page";
         seconds = 2.25;
         truncated = false;
       });
  append (Journal.Stats some_stats);
  append (Journal.Content "the tag moved to v0.9");
  ignore
    (Memory.write m ~seq:(Journal.next_seq j) ~cause:"recorded the moved tag"
       ~journal:(fun mw -> append (Journal.Memory_write mw))
       ~id:"ds4-upstream" ~kind:Memory.Fact ~title:"the upstream tag moved"
       ~body:"upstream is at v0.9" ~tags:[ "ds4" ]);
  ignore
    (Memory.write m ~seq:(Journal.next_seq j) ~cause:"noted the unfinished feed"
       ~journal:(fun mw -> append (Journal.Memory_write mw))
       ~id:"finish-the-digest" ~kind:Memory.Open_item
       ~title:"the digest is half done" ~body:"three feeds left" ~tags:[]);
  append (Journal.Handover 2);
  (* An hour later, the second wake-up, on another task. A kind this build does
     not know goes in with it, since a log that dropped one would read as
     complete. *)
  Eio_mock.Clock.set_time clock later;
  append
    (Journal.Wake
       {
         Journal.task = "digest";
         due = Journal.rfc3339 later;
         why = "due";
         serial = None;
       });
  append
    (Journal.Unknown { Journal.name = "future_kind"; json = Jsont.Json.list [] });
  append (Journal.Content "nothing to summarise");
  ignore
    (Memory.forget m ~seq:(Journal.next_seq j) ~cause:"the digest is done"
       ~journal:(fun mw -> append (Journal.Memory_write mw))
       "finish-the-digest");
  append (Journal.Handover 3);
  append (Journal.Run_stop "SIGTERM: the turn in flight finished");
  Journal.close j

let run env =
  let clock = Eio_mock.Clock.make () in
  Eio_mock.Clock.set_time clock t0;
  let tmp = Filename.temp_file "ds4-numpty-show" "" in
  Sys.remove tmp;
  let dir = Eio.Path.(Eio.Stdenv.fs env / tmp) in
  Eio.Path.mkdirs ~exists_ok:true ~perm:0o700 dir;
  Fun.protect ~finally:(fun () ->
      ignore (Sys.command (Printf.sprintf "rm -rf %s" tmp)))
  @@ fun () ->
  build ~clock dir;
  let journal_dir = Store.journal_dir dir in
  let all = lines journal_dir in
  check "every record has a line" (List.length all = 18);
  check "a line carries the sequence number, the time, the run and the kind"
    (contains (List.hd all) "     1"
    && contains (List.hd all) "2026-08-08T09:14:07Z"
    && contains (List.hd all) "run 9"
    && contains (List.hd all) "run_start");
  check "a run_start says which model on which backend"
    (contains (List.hd all) "/models/ds4.gguf" && contains (List.hd all) "CPU");
  let text = String.concat "\n" all in
  check "a wake says which task, why, and the serial a person asked with"
    (contains text "feeds, due 2026-08-08T09:14:07Z, run_now serial 3");
  check "a tool call keeps the adapter's canonical arguments"
    (contains text {|fetch {"url":"https://x/"}|});
  check "a tool result says how long it took"
    (contains text "fetch in 2.2s" || contains text "fetch in 2.3s");
  check "and is cut to one line with its size named"
    (contains text "200 https://x/… (" && not (contains text "the page"));
  check "a memory write names both versions, the entry and why"
    (contains text "0 to 1, ds4-upstream: recorded the moved tag");
  check "a schedule load says what was read and what changed"
    (contains text "read feeds, digest, changed feeds");
  (* A kind this build does not know is shown as it was written. A reader asking
     what happened must be told even where this program cannot say what it
     means. *)
  check "a kind this build does not know is still a line"
    (contains text "future_kind");
  (* The filters. *)
  check "the kind filter keeps only that kind"
    (List.length (lines ~kinds:[ "content" ] journal_dir) = 2);
  check "the run filter keeps only that run"
    (List.length (lines ~run:9 journal_dir) = 18
    && lines ~run:1 journal_dir = []);
  (* A record does not name a task, so the wake before it is what says which
     wake-up it belongs to. *)
  let feeds = lines ~task:"feeds" journal_dir in
  check "the task filter starts at that task's wake"
    (contains (List.hd feeds) "wake" && contains (List.hd feeds) "feeds");
  check "and stops at the next wake"
    (List.length feeds = 10
    && not (List.exists (fun l -> contains l "nothing to summarise") feeds));
  check "the other task's wake-up is the rest of it"
    (List.length (lines ~task:"digest" journal_dir) = 6);
  check "the since filter drops what was written before it"
    (List.length (lines ~since:(later -. 1.) journal_dir) = 6);
  check "and keeps everything when it is older than the journal"
    (List.length (lines ~since:(t0 -. 1.) journal_dir) = 18);
  (* Memory. *)
  let m = Memory.create ~clock (Store.memory_dir dir) in
  let current = Show.show m ~at:None in
  check "show prints the version in force"
    (contains current "memory version 3" && contains current "ds4-upstream");
  check "and not an entry a later version forgot"
    (not (contains current "finish-the-digest"));
  let at2 = Show.show m ~at:(Some 2) in
  check "show at a version says what was there then"
    (contains at2 "finish-the-digest" && contains at2 "three feeds left");
  check "and names the journal record that caused it"
    (contains at2 "from journal record");
  let history = Show.history m in
  check "history has one line per version"
    (List.length (String.split_on_char '\n' (String.trim history)) = 3);
  check "and says why each was written"
    (contains history "recorded the moved tag"
    && contains history "the digest is done");
  let diff = Show.diff m 1 2 in
  check "a diff shows what was added" (contains diff "+ finish-the-digest");
  check "and what was taken away"
    (contains (Show.diff m 2 3) "- finish-the-digest");
  check "and says so where two versions hold the same entries"
    (contains (Show.diff m 3 3) "hold the same entries")

let () =
  Eio_main.run run;
  if !failures > 0 then begin
    Printf.printf "\n%d failure(s)\n" !failures;
    exit 1
  end
