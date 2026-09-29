(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* The store a run owns, and the memory tools over it, on a real temporary
   directory.

   The properties are the negative ones. A second run must be refused, and told
   which process holds the store, since two runs would interleave journal lines
   and race the version counter. A run that crashed must not leave a store
   nobody can open, so a lock file left behind with nothing holding it is not a
   refusal. A snapshot no journal record names was never in force, so opening
   removes it and says so in the journal.

   The memory tools are checked against the journal rather than against
   themselves. Each mutation must mint exactly one version and append exactly
   one record naming both versions, the entry and the reason, which is what
   makes the trace line up with the model's actions one to one. *)

module Journal = Agentkit.Journal
module Memory = Agentkit.Memory
module Store = Numpty_daemon.Store
module Memory_tools = Numpty_daemon.Memory_tools
module Tool = Ds4.Tool

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

let call tool arguments =
  Tool.invoke tool { Dsml.name = Tool.name tool; arguments; id = None }

let records dir =
  let seen = ref [] in
  Journal.iter dir (fun r -> seen := r :: !seen);
  List.rev !seen

(* The lock. *)

let lock_tests ~clock root =
  let open_ ~sw path = Store.open_ ~sw ~clock path in
  Eio.Switch.run (fun sw ->
      let store = open_ ~sw Eio.Path.(root / "one") in
      (match open_ ~sw Eio.Path.(root / "one") with
      | _ -> check "a second run on one store is refused" false
      | exception Store.Locked { pid; _ } ->
          check "a second run on one store is refused with the first one's pid"
            (pid = Unix.getpid ()));
      check "the lock file holds the pid of the run that took it"
        (String.trim (Eio.Path.load Eio.Path.(root / "one" / "lock"))
        = string_of_int (Unix.getpid ()));
      Store.close store;
      match open_ ~sw Eio.Path.(root / "one") with
      | store ->
          check "a store opens again once the run that held it has closed it"
            true;
          Store.close store
      | exception Store.Locked _ ->
          check "a store opens again once the run that held it has closed it"
            false);
  (* What a crashed run leaves is a lock file with nothing holding it. The file
     being there is not a refusal, or a crash would need a person with a
     broom. *)
  let crashed = Eio.Path.(root / "crashed") in
  Eio.Path.mkdirs ~exists_ok:true ~perm:0o700 crashed;
  Eio.Path.save ~create:(`Or_truncate 0o600)
    Eio.Path.(crashed / "lock")
    "999999\n";
  Eio.Path.save ~create:(`Or_truncate 0o600)
    Eio.Path.(crashed / "control")
    "not really a socket";
  Eio.Switch.run (fun sw ->
      match open_ ~sw crashed with
      | store ->
          check "a lock file a crashed run left does not refuse the next one"
            true;
          check "a socket file a crashed run left is unlinked"
            (not (Eio.Path.is_file Eio.Path.(crashed / "control")));
          Store.close store
      | exception Store.Locked _ ->
          check "a lock file a crashed run left does not refuse the next one"
            false)

(* Recovery, which is what makes the two stores agree after a crash. *)

let recovery_tests ~clock root =
  let dir = Eio.Path.(root / "recovered") in
  (* A snapshot with no journal record naming it is the leaving of a crash
     between the fsync and the append. *)
  Eio.Path.mkdirs ~exists_ok:true ~perm:0o700 Eio.Path.(dir / "memory");
  Eio.Path.save ~create:(`Or_truncate 0o600)
    Eio.Path.(dir / "memory" / "000001.json")
    {|{"version":1,"parent":0,"t":"2026-08-08T09:14:07Z","seq":4,"cause":"a crash","entries":[]}|};
  Eio.Switch.run (fun sw ->
      let store = Store.open_ ~sw ~clock dir in
      check "a snapshot the journal never named is removed"
        (Memory.versions (Store.memory store) = []);
      check "the store is back at version zero"
        (Memory.version (Store.memory store) = 0);
      Store.close store);
  let removal =
    List.filter
      (fun (r : Journal.record) ->
        match r.Journal.kind with
        | Journal.Error { where; _ } -> where = "memory recovery"
        | _ -> false)
      (records Eio.Path.(dir / "journal"))
  in
  check "removing it is in the journal" (List.length removal = 1);
  check "and the record says the snapshot was never in force"
    (match removal with
    | [ { Journal.kind = Journal.Error { what; _ }; _ } ] ->
        contains what "000001" && contains what "never in force"
    | _ -> false)

(* The memory tools, checked against the journal they write. *)

let memory_tool_tests ~clock root =
  Eio.Switch.run @@ fun sw ->
  let store = Store.open_ ~sw ~clock Eio.Path.(root / "tools") in
  let memory = Store.memory store and journal = Store.journal store in
  let tools = Memory_tools.all ~memory ~journal in
  check "the four tools are named as the model meets them"
    (List.map Tool.name tools
    = [ "memory_list"; "memory_read"; "memory_write"; "memory_forget" ]);
  let tool name = List.find (fun t -> Tool.name t = name) tools in
  let list = call (tool "memory_list") {|{}|} in
  check "an empty memory lists as empty" (contains list "no entry that matches");
  let wrote =
    call (tool "memory_write")
      {|{"id":"ds4-upstream","kind":"fact","title":"the tag moved",
         "body":"upstream is at v0.9","tags":["ds4"],
         "why":"recorded that the upstream tag moved"}|}
  in
  check "a write reports the version it minted" (contains wrote "version 1");
  check "and the store is at it" (Memory.version memory = 1);
  let wrote2 =
    call (tool "memory_write")
      {|{"id":"finish-the-digest","kind":"open_item","title":"digest half done",
         "body":"three feeds left","why":"left the digest unfinished"}|}
  in
  check "a second write mints the next version" (contains wrote2 "version 2");
  (* One version per call, and one record per version, which is what makes the
     trace line up with the model's actions. *)
  let writes =
    List.filter_map
      (fun (r : Journal.record) ->
        match r.Journal.kind with
        | Journal.Memory_write mw -> Some mw
        | _ -> None)
      (records Eio.Path.(root / "tools" / "journal"))
  in
  check "each mutation appended one journal record" (List.length writes = 2);
  check "and each record names both versions and the entry"
    (List.map
       (fun (mw : Journal.memory_write) -> (mw.from, mw.to_, mw.entry))
       writes
    = [ (0, 1, "ds4-upstream"); (1, 2, "finish-the-digest") ]);
  check "and carries the reason the model gave"
    (match writes with
    | mw :: _ -> mw.Journal.why = "recorded that the upstream tag moved"
    | [] -> false);
  check "the version points at the journal record that caused it"
    ((Memory.read memory 1).Memory.seq
    =
    match records Eio.Path.(root / "tools" / "journal") with
    | r :: _ -> r.Journal.seq
    | [] -> -1);
  (* Reading and listing. *)
  let read = call (tool "memory_read") {|{"id":"ds4-upstream"}|} in
  check "a read gives the body" (contains read "upstream is at v0.9");
  check "and says which kind it is" (contains read "(fact)");
  let opens = call (tool "memory_list") {|{"kind":"open_item"}|} in
  check "a listing filters on kind"
    (contains opens "finish-the-digest" && not (contains opens "ds4-upstream"));
  let tagged = call (tool "memory_list") {|{"tag":"ds4"}|} in
  check "a listing filters on tag"
    (contains tagged "ds4-upstream" && not (contains tagged "finish-the-digest"));
  (* A refusal says what is there rather than reporting nothing. *)
  let missing = call (tool "memory_read") {|{"id":"nothing"}|} in
  check "reading an id no entry has says what the entries are"
    (contains missing "ds4-upstream" && contains missing "finish-the-digest");
  let bad_kind =
    call (tool "memory_write")
      {|{"id":"x","kind":"note","title":"t","body":"b","why":"w"}|}
  in
  check "a kind that is not one of the four says what the four are"
    (contains bad_kind "open_item" && contains bad_kind "procedure");
  check "and mints no version" (Memory.version memory = 2);
  let no_why =
    call (tool "memory_write")
      {|{"id":"x","kind":"fact","title":"t","body":"b","why":""}|}
  in
  check "a write with no reason is refused" (contains no_why "why must say");
  check "and mints no version either" (Memory.version memory = 2);
  (* Forgetting leaves the versions that held the entry alone. *)
  let forgot =
    call (tool "memory_forget")
      {|{"id":"finish-the-digest","why":"the digest is done"}|}
  in
  check "a forget reports the version it minted" (contains forgot "version 3");
  check "the entry is gone from the version in force"
    (List.map (fun (e : Memory.entry) -> e.Memory.id) (Memory.entries memory)
    = [ "ds4-upstream" ]);
  check "and still there at the version that held it"
    (List.exists
       (fun (e : Memory.entry) -> e.Memory.id = "finish-the-digest")
       (Memory.read memory 2).Memory.entries);
  let gone =
    call (tool "memory_forget") {|{"id":"finish-the-digest","why":"again"}|}
  in
  check "forgetting it twice is refused" (contains gone "no entry");
  check "and mints no version" (Memory.version memory = 3);
  Store.close store

let run env =
  let clock = Eio.Stdenv.clock env in
  let tmp = Filename.temp_file "ds4-numpty-store" "" in
  Sys.remove tmp;
  let root = Eio.Path.(Eio.Stdenv.fs env / tmp) in
  Eio.Path.mkdirs ~exists_ok:true ~perm:0o700 root;
  Fun.protect
    ~finally:(fun () -> ignore (Sys.command (Printf.sprintf "rm -rf %s" tmp)))
    (fun () ->
      lock_tests ~clock root;
      recovery_tests ~clock root;
      memory_tool_tests ~clock root)

let () =
  Eio_main.run run;
  if !failures > 0 then begin
    Printf.printf "\n%d failure(s)\n" !failures;
    exit 1
  end
