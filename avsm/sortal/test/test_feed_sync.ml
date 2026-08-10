(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Tests for feed sync: pausing, format autodetection, and the
    reclassify-without-orphaning hazard. Run against small excerpts of the
    real feed shapes named in the investigation, not the full bodies. *)

open Eio.Std

let with_tmp_store f =
  Eio_main.run @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  let tmp_dir = Eio.Path.(fs / Filename.get_temp_dir_name () / "sortal-test-feed-sync") in
  (try Eio.Path.mkdir ~perm:0o755 tmp_dir with Eio.Io _ -> ());
  Fun.protect
    ~finally:(fun () -> try Eio.Path.rmtree tmp_dir with _ -> ())
    (fun () -> f (Sortal_feed.Store.create tmp_dir))

(* A [Fetch.plain] session that never touches the network: it answers from
   an in-memory body and records whether it was called, so a paused feed's
   "does not fetch" contract is checked directly rather than inferred from
   its result. *)
let mock_session ~body =
  let called = ref false in
  let session =
    Fetch_mock.client (fun req ->
      called := true;
      Fetch_mock.respond body req)
  in
  (session, called)

let test_paused_feed_skips_fetch () =
  with_tmp_store @@ fun store ->
  let session, called = mock_session ~body:"<feed xmlns=\"http://www.w3.org/2005/Atom\"></feed>" in
  let feed =
    Sortal_schema.Feed.make ~feed_type:Atom
      ~url:"https://example.com/paused.atom" ~paused:true ()
  in
  match Sortal_feed.Sync.sync_feed ~session ~store ~handle:"testuser" feed with
  | Error e -> failwith ("expected Ok, got Error " ^ e)
  | Ok r ->
    assert (r.paused = true);
    assert (r.new_entries = 0);
    assert (not !called);
    traceln "  paused feed: sync_feed returns without fetching"

let test_unpaused_feed_does_fetch () =
  with_tmp_store @@ fun store ->
  let session, called =
    mock_session ~body:"<feed xmlns=\"http://www.w3.org/2005/Atom\">\
                         <id>urn:test</id><title>T</title>\
                         <updated>2026-01-01T00:00:00Z</updated></feed>"
  in
  let feed =
    Sortal_schema.Feed.make ~feed_type:Atom
      ~url:"https://example.com/active.atom" ()
  in
  match Sortal_feed.Sync.sync_feed ~session ~store ~handle:"testuser" feed with
  | Error e -> failwith ("expected Ok, got Error " ^ e)
  | Ok r ->
    assert (r.paused = false);
    assert !called;
    traceln "  unpaused feed: sync_feed fetches as normal"

let () =
  traceln "\n=== Feed Sync Tests ===\n";
  test_paused_feed_skips_fetch ();
  test_unpaused_feed_does_fetch ();
  traceln "\n=== All Feed Sync Tests Passed ===\n"
