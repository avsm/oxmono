(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* The memory store, on a real temporary directory.

   The properties are the ones an audit rests on. A version is written once and
   never rewritten, so a mutation that meets the version it would mint refuses
   rather than overwriting it. Forgetting an entry does not touch the version
   that held it, so [--at V] says what was there at V however much has been
   written since.

   The rest is the crash window. A mutation writes the snapshot, journals it,
   and moves [current] last, so the two failures are a snapshot no journal
   record names, which was never in force and is removed, and one a record does
   name, which is a version [current] never caught up with. Both are staged
   here by making the journal callback raise. *)

module Journal = Agentkit.Journal
module Memory = Agentkit.Memory

let failures = ref 0

let check name cond =
  if cond then Printf.printf "ok   - %s\n" name
  else begin
    incr failures;
    Printf.printf "FAIL - %s\n" name
  end

let t0 = 1786180447. (* 2026-08-08T09:14:07Z *)
let ids s = List.map (fun (e : Memory.entry) -> e.Memory.id) s.Memory.entries

let store dir =
  let clock = Eio_mock.Clock.make () in
  Eio_mock.Clock.set_time clock t0;
  let m = Memory.create ~clock Eio.Path.(dir / "memory") in
  let journalled = ref 0 in
  let written = ref [] in
  let journal mw =
    written := mw :: !written;
    journalled := mw.Journal.to_
  in
  let put ?(journal = journal) ?(seq = 1) ~cause id body =
    Memory.write m ~seq ~cause ~journal ~id ~kind:Memory.Fact ~title:id ~body
      ~tags:[ "x" ]
  in
  check "an empty store is at version zero" (Memory.version m = 0);
  check "an empty store has no entries" (Memory.entries m = []);

  (* One version per mutation, each naming the one before. *)
  let v1 = put ~seq:10 ~cause:"learned a" "a" "one" in
  let v2 = put ~seq:11 ~cause:"learned b" "b" "two" in
  check "a mutation mints the next version" (v1 = 1 && v2 = 2);
  check "the store is at the version it minted" (Memory.version m = 2);
  check "a version names its parent" ((Memory.read m 2).Memory.parent = 1);
  check "the first version has no parent" ((Memory.read m 1).Memory.parent = 0);
  check "a version points at the journal record that caused it"
    ((Memory.read m 2).Memory.seq = 11);
  check "a version says why it was written"
    ((Memory.read m 2).Memory.cause = "learned b");
  check "the journal record names both versions and the entry"
    (List.hd !written
    = { Journal.from = 1; to_ = 2; entry = "b"; why = "learned b" });

  (* Updating keeps the version the entry first appeared in. *)
  let v3 = put ~seq:12 ~cause:"a moved" "a" "one and a half" in
  let a3 =
    List.find (fun (e : Memory.entry) -> e.Memory.id = "a") (Memory.entries m)
  in
  check "an update keeps created and moves updated"
    (a3.Memory.created = 1 && a3.Memory.updated = 3);
  check "an update replaces rather than appends"
    (ids (Memory.read m 3) = [ "a"; "b" ]);

  (* Forgetting writes a version with the entry absent and leaves the ones
     that held it alone. *)
  let v4 = Memory.forget m ~seq:13 ~cause:"a is gone" ~journal "a" in
  check "forget mints the next version" (v3 = 3 && v4 = 4);
  check "forget leaves the entry absent" (ids (Memory.read m 4) = [ "b" ]);
  let body_at v id =
    let has (e : Memory.entry) = e.Memory.id = id in
    match List.find_opt has (Memory.read m v).Memory.entries with
    | Some e -> Some e.Memory.body
    | None -> None
  in
  check "forget does not touch the version that held the entry"
    (ids (Memory.read m 3) = [ "a"; "b" ]
    && ids (Memory.read m 1) = [ "a" ]
    && body_at 1 "a" = Some "one"
    && body_at 3 "a" = Some "one and a half");
  check "reading at a version is unaffected by later ones"
    (ids (Memory.read m 2) = [ "a"; "b" ]);
  check "forgetting an id no entry has is refused"
    (match Memory.forget m ~seq:14 ~cause:"gone" ~journal "nope" with
    | _ -> false
    | exception Memory.No_entry "nope" -> true);
  check "a refused forget mints no version" (Memory.version m = 4);
  check "the store holds every version it minted"
    (Memory.versions m = [ 1; 2; 3; 4 ]);

  (* The order the three writes happen in. The journal record is appended
     after the snapshot is on disk and before anything says it is in force. *)
  let seen = ref None in
  let watch mw =
    seen := Some (Memory.version m, List.mem mw.Journal.to_ (Memory.versions m));
    journal mw
  in
  let v5 = put ~journal:watch ~seq:15 ~cause:"learned c" "c" "three" in
  check "the snapshot is on disk before the journal record"
    (!seen = Some (4, true));
  check "current moves after the journal record" (v5 = 5 && Memory.version m = 5);

  (* A crash between the snapshot and the journal record leaves a snapshot the
     journal never named. It was never in force, so it is not a version. *)
  let before_journal _ = raise Exit in
  (try ignore (put ~journal:before_journal ~seq:16 ~cause:"lost" "d" "four")
   with Exit -> ());
  check "a failed journal leaves current where it was" (Memory.version m = 5);
  check "the orphan snapshot is on disk"
    (Memory.versions m = [ 1; 2; 3; 4; 5; 6 ]);
  check "a mutation that meets its own debris is refused"
    (match put ~seq:17 ~cause:"next" "e" "five" with
    | _ -> false
    | exception Memory.Version_exists 6 -> true);
  let recovered = Memory.recover m ~journalled:!journalled in
  check "recovery removes exactly the unjournalled snapshot"
    (recovered = { Memory.adopted = None; removed = [ 6 ] });
  check "recovery leaves the journalled versions alone"
    (Memory.versions m = [ 1; 2; 3; 4; 5 ] && Memory.version m = 5);
  check "a mutation works once the debris is gone"
    (put ~seq:18 ~cause:"next" "e" "five" = 6);

  (* A crash between the journal record and the move of current leaves a
     version the journal named. It is a version, so current catches up. *)
  let after_journal mw =
    journal mw;
    raise Exit
  in
  (try ignore (put ~journal:after_journal ~seq:19 ~cause:"named" "f" "six")
   with Exit -> ());
  check "a crash after the record leaves current behind" (Memory.version m = 6);
  let recovered = Memory.recover m ~journalled:!journalled in
  check "recovery adopts a snapshot the journal named"
    (recovered = { Memory.adopted = Some 7; removed = [] });
  check "the adopted version is in force"
    (Memory.version m = 7 && ids (Memory.read m 7) = [ "b"; "c"; "e"; "f" ]);
  check "recovery on a settled store does nothing"
    (Memory.recover m ~journalled:!journalled
    = { Memory.adopted = None; removed = [] });

  (* A version file is never rewritten, whatever the store is asked for. *)
  let at_5 = Memory.read m 5 in
  check "an old version file is untouched by everything since"
    (at_5.Memory.version = 5 && ids at_5 = [ "b"; "c" ]);
  check "reading a version the store does not hold is refused"
    (match Memory.read m 99 with
    | _ -> false
    | exception Memory.No_version 99 -> true)

let run env =
  let fs = Eio.Stdenv.fs env in
  let tmp = Filename.temp_file "ds4-memory" "" in
  Sys.remove tmp;
  let dir = Eio.Path.(fs / tmp) in
  Eio.Path.mkdirs ~exists_ok:true ~perm:0o700 dir;
  Fun.protect
    ~finally:(fun () ->
      ignore (Sys.command (Filename.quote_command "rm" [ "-rf"; tmp ])))
    (fun () -> store dir)

let () =
  Eio_main.run run;
  if !failures = 0 then print_string "\nAll tests passed.\n"
  else begin
    Printf.printf "\n%d check(s) failed.\n" !failures;
    exit 1
  end
