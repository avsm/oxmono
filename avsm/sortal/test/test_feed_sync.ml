(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Tests for feed sync: pausing, format autodetection, and the
    reclassify-without-orphaning hazard. Run against small excerpts of the
    real feed shapes named in the investigation, not the full bodies. *)

open Eio.Std

let contains haystack needle =
  let n = String.length needle and h = String.length haystack in
  let rec go i = i + n <= h && (String.sub haystack i n = needle || go (i + 1)) in
  n = 0 || go 0

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
let mock_session ?content_type ~body () =
  let called = ref false in
  let headers =
    Option.map (fun ct -> Http.Header.init_with "Content-Type" ct) content_type
  in
  let session =
    Fetch_mock.client (fun req ->
      called := true;
      Fetch_mock.respond ?headers body req)
  in
  (session, called)

let test_paused_feed_skips_fetch () =
  with_tmp_store @@ fun store ->
  let session, called = mock_session ~body:"<feed xmlns=\"http://www.w3.org/2005/Atom\"></feed>" () in
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

(* Small excerpts of the real shapes from the investigation. Not the full
   bodies: those are large, and one is a gambling site. *)

(* digitalflapjack.com/blog/index.xml: recorded [rss], actually Atom. *)
let mdales_atom_body =
  "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n\
   <feed xmlns=\"http://www.w3.org/2005/Atom\">\
   <id>https://digitalflapjack.com/blog/</id>\
   <title>Digital Flapjack</title>\
   <updated>2026-02-17T07:45:00-00:00</updated>\
   <entry><id>https://digitalflapjack.com/blog/post-1</id>\
   <title>A post</title>\
   <updated>2026-02-17T07:45:00-00:00</updated></entry>\
   </feed>"

(* A minimal, valid RSS2 document, standing in for content genuinely
   downloaded before a feed's format changed server-side, as happened for
   mdales: the historical file on disk really is RSS, not just mislabelled
   Atom. *)
let old_rss_body =
  "<?xml version=\"1.0\"?>\n\
   <rss version=\"2.0\"><channel><title>Old</title>\
   <link>https://example.com</link><description>d</description>\
   <item><title>Old post</title><link>https://example.com/old</link>\
   <guid>old-1</guid></item>\
   </channel></rss>"

(* nick.recoil.org/index.xml: an HTML-escaped XML declaration, which must
   not be tolerated. *)
let nickludlam_body =
  "&lt;?xml version=\"1.0\" encoding=\"UTF-8\"?>\n\
   <rss version=\"2.0\">\n  <channel>\n    <title>nick.recoil.org</title>"

let test_mislabelled_atom_feed_syncs () =
  with_tmp_store @@ fun store ->
  let session, _ = mock_session ~body:mdales_atom_body () in
  let feed = Sortal_schema.Feed.make ~feed_type:Rss ~url:"https://digitalflapjack.com/blog/index.xml" () in
  match Sortal_feed.Sync.sync_feed ~session ~store ~handle:"mdales" feed with
  | Error e -> failwith ("expected the mislabelled Atom feed to sync, got: " ^ e)
  | Ok r ->
    assert (r.total_entries = 1);
    let entries = Sortal_feed.Store.entries_of_feed store ~handle:"mdales" feed in
    assert (List.length entries = 1);
    traceln "  detection: a feed recorded rss but served as Atom syncs correctly"

(* A feed that has always been Atom, only ever mislabelled [rss]: the
   bytes already on disk under the [rss]-typed path genuinely parse as
   Atom. This is mdales's actual bug. *)
let mislabelled_only_atom_body =
  "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n\
   <feed xmlns=\"http://www.w3.org/2005/Atom\">\
   <id>https://digitalflapjack.com/blog/</id>\
   <title>Digital Flapjack</title>\
   <updated>2026-01-01T00:00:00-00:00</updated>\
   <entry><id>https://digitalflapjack.com/blog/old-post</id>\
   <title>Old post</title>\
   <updated>2026-01-01T00:00:00-00:00</updated></entry>\
   </feed>"

let test_reclassify_merges_when_content_already_matches () =
  with_tmp_store @@ fun store ->
  let handle = "always-atom" in
  let url = "https://example.com/mislabelled.atom" in
  let rss_feed = Sortal_schema.Feed.make ~feed_type:Rss ~url () in
  Sortal_feed.Store.ensure_feed_dir store handle;
  let rss_path = Sortal_feed.Store.feed_file store handle rss_feed in
  Sortal_feed.Store.save_rss_raw rss_path mislabelled_only_atom_body;
  let meta_path = Sortal_feed.Store.meta_file store handle rss_feed in
  Sortal_feed.Meta.save meta_path
    { feed_url = url; feed_type = Rss; last_sync = None; etag = None;
      last_modified = None; entry_count = 1 };
  let annot_path = Sortal_feed.Store.annotations_file store handle rss_feed in
  Eio.Path.save ~create:(`Or_truncate 0o644) annot_path "{}";

  let session, _ = mock_session ~body:mdales_atom_body () in
  (match Sortal_feed.Sync.sync_feed ~session ~store ~handle rss_feed with
   | Error e -> failwith ("expected the reclassified feed to sync, got: " ^ e)
   | Ok _ -> ());

  (* The old path is gone: its content moved rather than being copied. *)
  assert (not (Eio.Path.is_file rss_path));
  assert (Sortal_feed.Store.effective_type store handle rss_feed = Atom);
  let atom_feed = Sortal_schema.Feed.set_feed_type rss_feed Atom in
  assert (Eio.Path.is_file (Sortal_feed.Store.meta_file store handle atom_feed));
  assert (Eio.Path.is_file (Sortal_feed.Store.annotations_file store handle atom_feed));
  (* Both the entry that was already there and the newly-fetched one
     survive, merged into one feed. *)
  let entries = Sortal_feed.Store.entries_of_feed store ~handle rss_feed in
  assert (List.length entries = 2);
  traceln "  reclassify: mislabelled-only-ever content moves and keeps merging"

let test_reclassify_preserves_genuinely_old_format () =
  with_tmp_store @@ fun store ->
  let handle = "mdales" in
  let url = "https://digitalflapjack.com/blog/index.xml" in
  let rss_feed = Sortal_schema.Feed.make ~feed_type:Rss ~url () in
  (* Seed the store as if an earlier sync, back when the site really did
     serve RSS, had already downloaded one entry. *)
  Sortal_feed.Store.ensure_feed_dir store handle;
  let rss_path = Sortal_feed.Store.feed_file store handle rss_feed in
  Sortal_feed.Store.save_rss_raw rss_path old_rss_body;
  let meta_path = Sortal_feed.Store.meta_file store handle rss_feed in
  Sortal_feed.Meta.save meta_path
    { feed_url = url; feed_type = Rss; last_sync = None; etag = None;
      last_modified = None; entry_count = 1 };

  (* The site has since switched to Atom, still at the same URL. *)
  let session, _ = mock_session ~body:mdales_atom_body () in
  (match Sortal_feed.Sync.sync_feed ~session ~store ~handle rss_feed with
   | Error e -> failwith ("expected the reclassified feed to sync, got: " ^ e)
   | Ok _ -> ());

  (* The old, genuinely RSS-formatted file cannot be reinterpreted as
     Atom, so it is left exactly where it was: nothing is deleted or
     silently overwritten. *)
  assert (Eio.Path.load rss_path = old_rss_body);
  assert (
    match Sortal_feed.Store.load_rss rss_path with
    | Some channel -> List.length channel.items = 1
    | None -> false);
  (* Reads now prefer the freshly-synced Atom content. *)
  assert (Sortal_feed.Store.effective_type store handle rss_feed = Atom);
  let entries = Sortal_feed.Store.entries_of_feed store ~handle rss_feed in
  assert (List.length entries = 1);
  traceln "  reclassify: a genuine historical format change is preserved, not merged"

let test_undetectable_body_fails_legibly () =
  with_tmp_store @@ fun store ->
  let session, _ = mock_session ~body:nickludlam_body () in
  let feed = Sortal_schema.Feed.make ~feed_type:Rss ~url:"https://nick.recoil.org/index.xml" () in
  match Sortal_feed.Sync.sync_feed ~session ~store ~handle:"nickludlam" feed with
  | Ok _ -> failwith "expected the escaped declaration to fail detection"
  | Error msg ->
    assert (
      contains msg "could not detect feed format"
      && contains msg "&lt;?xml");
    traceln "  detection: an HTML-escaped declaration fails with a legible message"

(* gabrielmahler.org/feed.xml: the domain lapsed and now serves an HTML
   gambling site at every URL, including the feed's. *)
let gmahler_html_body =
  "<!DOCTYPE html>\n\
   <html lang=\"id\">\n\
   <head><meta charset=\"UTF-8\">\n\
   <title>Tapir328 | Nikmati Pengalaman Bermain Game Online</title>"

let test_html_response_fails_with_content_type () =
  with_tmp_store @@ fun store ->
  let session, _ = mock_session ~content_type:"text/html" ~body:gmahler_html_body () in
  let feed = Sortal_schema.Feed.make ~feed_type:Atom ~url:"https://gabrielmahler.org/feed.xml" () in
  match Sortal_feed.Sync.sync_feed ~session ~store ~handle:"gmahler" feed with
  | Ok _ -> failwith "expected an HTML response to fail"
  | Error msg ->
    assert (contains msg "HTML");
    assert (contains msg "text/html");
    traceln "  detection: an HTML response fails naming the Content-Type"

let test_html_response_without_content_type_still_fails () =
  with_tmp_store @@ fun store ->
  let session, _ = mock_session ~body:gmahler_html_body () in
  let feed = Sortal_schema.Feed.make ~feed_type:Atom ~url:"https://example.com/no-content-type" () in
  match Sortal_feed.Sync.sync_feed ~session ~store ~handle:"nohdr" feed with
  | Ok _ -> failwith "expected an HTML response to fail"
  | Error msg ->
    assert (contains msg "HTML");
    traceln "  detection: an HTML response fails even without a Content-Type header"

let test_manual_feed_is_not_sniffed () =
  with_tmp_store @@ fun store ->
  let session, _ = mock_session ~body:"this is not a feed of any kind" () in
  let feed = Sortal_schema.Feed.make ~feed_type:Manual ~url:"https://example.com/about" () in
  match Sortal_feed.Sync.sync_feed ~session ~store ~handle:"someone" feed with
  | Error e -> failwith ("Manual feeds must not be sniffed, got: " ^ e)
  | Ok _ -> traceln "  detection: a Manual feed dispatches on its recorded type, unsniffed"

let test_unpaused_feed_does_fetch () =
  with_tmp_store @@ fun store ->
  let session, called =
    mock_session ~body:"<feed xmlns=\"http://www.w3.org/2005/Atom\">\
                         <id>urn:test</id><title>T</title>\
                         <updated>2026-01-01T00:00:00Z</updated></feed>" ()
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
  test_mislabelled_atom_feed_syncs ();
  test_reclassify_merges_when_content_already_matches ();
  test_reclassify_preserves_genuinely_old_format ();
  test_undetectable_body_fails_legibly ();
  test_html_response_fails_with_content_type ();
  test_html_response_without_content_type_still_fails ();
  test_manual_feed_is_not_sniffed ();
  traceln "\n=== All Feed Sync Tests Passed ===\n"
