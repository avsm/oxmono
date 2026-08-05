(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

open Eio.Std

let test_url_to_filename () =
  let f1 = Sortal_feed.Store.url_to_filename "https://example.com/feed.xml" in
  let f2 = Sortal_feed.Store.url_to_filename "https://example.com/feed.xml" in
  let f3 = Sortal_feed.Store.url_to_filename "https://other.com/feed.xml" in
  assert (f1 = f2);
  assert (String.length f1 = 16);
  assert (f1 <> f3);
  traceln "  url_to_filename: deterministic and correct length"

let test_atom_entry_conversion () =
  let now = Ptime_clock.now () in
  let entry : Syndic.Atom.entry = {
    authors = (Syndic.Atom.author "Test Author", []);
    categories = [];
    content = Some (Syndic.Atom.Text "Some content");
    contributors = [];
    id = Uri.of_string "urn:uuid:test-1";
    links = [Syndic.Atom.link ~rel:Syndic.Atom.Alternate (Uri.of_string "https://example.com/post/1")];
    published = Some now;
    rights = None;
    source = None;
    summary = Some (Syndic.Atom.Text "A summary");
    title = Syndic.Atom.Text "Test Post";
    updated = now;
  } in
  let e = Sortal_feed.Entry.of_atom_entry ~source_feed:"https://example.com/atom.xml" entry in
  assert (e.id = "urn:uuid:test-1");
  assert (e.title = Some "Test Post");
  assert (e.date = Some now);
  assert (e.summary = Some "A summary");
  assert (e.content = Some "Some content");
  assert (e.url = Some (Uri.of_string "https://example.com/post/1"));
  assert (e.source_type = Sortal_schema.Feed.Atom);
  traceln "  of_atom_entry: fields extracted correctly"

let test_rss2_item_conversion () =
  let now = Ptime_clock.now () in
  let item : Syndic.Rss2.item = {
    story = Syndic.Rss2.All ("RSS Title", None, "RSS description");
    content = (None, "Full content");
    link = Some (Uri.of_string "https://example.com/rss/1");
    author = Some "author@example.com";
    categories = [];
    comments = None;
    enclosure = None;
    guid = Some { data = Uri.of_string "guid-1"; permalink = false };
    pubDate = Some now;
    source = None;
  } in
  let e = Sortal_feed.Entry.of_rss2_item ~source_feed:"https://example.com/rss.xml" item in
  assert (e.id = "guid-1");
  assert (e.title = Some "RSS Title");
  assert (e.date = Some now);
  assert (e.summary = Some "RSS description");
  assert (e.content = Some "Full content");
  assert (e.url = Some (Uri.of_string "https://example.com/rss/1"));
  assert (e.source_type = Sortal_schema.Feed.Rss);
  traceln "  of_rss2_item: fields extracted correctly"

let test_jsonfeed_item_conversion () =
  let now = Ptime_clock.now () in
  let item = Jsonfeed.Item.create
    ~id:"json-1"
    ~content:(`Text "JSON content")
    ~title:"JSON Title"
    ~summary:"JSON summary"
    ~url:"https://example.com/json/1"
    ~date_published:now
    ()
  in
  let e = Sortal_feed.Entry.of_jsonfeed_item ~source_feed:"https://example.com/feed.json" item in
  assert (e.id = "json-1");
  assert (e.title = Some "JSON Title");
  assert (e.date = Some now);
  assert (e.summary = Some "JSON summary");
  assert (e.content = Some "JSON content");
  assert (e.url = Some (Uri.of_string "https://example.com/json/1"));
  assert (e.source_type = Sortal_schema.Feed.Json);
  traceln "  of_jsonfeed_item: fields extracted correctly"

let test_compare_by_date () =
  let make_entry ?date id =
    { Sortal_feed.Entry.id; title = None; date; summary = None;
      content = None; url = None; source_feed = "test";
      source_type = Sortal_schema.Feed.Atom }
  in
  let t1 = Option.get (Ptime.of_rfc3339 "2024-01-01T00:00:00Z" |> Result.to_option |> Option.map (fun (t,_,_) -> t)) in
  let t2 = Option.get (Ptime.of_rfc3339 "2024-06-01T00:00:00Z" |> Result.to_option |> Option.map (fun (t,_,_) -> t)) in
  let e1 = make_entry ~date:t1 "old" in
  let e2 = make_entry ~date:t2 "new" in
  let e3 = make_entry "no-date" in
  (* Newest first *)
  assert (Sortal_feed.Entry.compare_by_date e2 e1 < 0);
  assert (Sortal_feed.Entry.compare_by_date e1 e2 > 0);
  (* Entries with dates before entries without *)
  assert (Sortal_feed.Entry.compare_by_date e1 e3 < 0);
  assert (Sortal_feed.Entry.compare_by_date e3 e1 > 0);
  (* No-date entries equal *)
  assert (Sortal_feed.Entry.compare_by_date e3 e3 = 0);
  traceln "  compare_by_date: ordering correct"

let test_dedup () =
  let t1 = Option.get (Ptime.of_rfc3339 "2024-01-01T00:00:00Z" |> Result.to_option |> Option.map (fun (t,_,_) -> t)) in
  let t2 = Option.get (Ptime.of_rfc3339 "2024-06-01T00:00:00Z" |> Result.to_option |> Option.map (fun (t,_,_) -> t)) in
  Eio_main.run @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  let tmp_dir = Eio.Path.(fs / Filename.get_temp_dir_name () / "sortal-test-feed") in
  (try Eio.Path.mkdir ~perm:0o755 tmp_dir with Eio.Io _ -> ());
  let feeds_dir = Eio.Path.(tmp_dir / "feeds") in
  (try Eio.Path.mkdir ~perm:0o755 feeds_dir with Eio.Io _ -> ());
  let handle_dir = Eio.Path.(feeds_dir / "testuser") in
  (try Eio.Path.mkdir ~perm:0o755 handle_dir with Eio.Io _ -> ());
  let store = Sortal_feed.Store.create tmp_dir in
  (* Create two JSON feeds with overlapping IDs *)
  let feed1 = Sortal_schema.Feed.make ~feed_type:Json ~url:"https://example.com/feed1.json" () in
  let feed2 = Sortal_schema.Feed.make ~feed_type:Json ~url:"https://example.com/feed2.json" () in
  let item_old = Jsonfeed.Item.create ~id:"shared-id" ~content:(`Text "old") ~date_published:t1 () in
  let item_new = Jsonfeed.Item.create ~id:"shared-id" ~content:(`Text "new") ~date_published:t2 () in
  let item_unique = Jsonfeed.Item.create ~id:"unique-id" ~content:(`Text "unique") ~date_published:t1 () in
  let jf1 = Jsonfeed.create ~title:"Feed 1" ~items:[item_old; item_unique] () in
  let jf2 = Jsonfeed.create ~title:"Feed 2" ~items:[item_new] () in
  let path1 = Sortal_feed.Store.feed_file store "testuser" feed1 in
  let path2 = Sortal_feed.Store.feed_file store "testuser" feed2 in
  Sortal_feed.Store.save_jsonfeed path1 jf1;
  Sortal_feed.Store.save_jsonfeed path2 jf2;
  let entries = Sortal_feed.Store.all_entries store ~handle:"testuser" [feed1; feed2] in
  (* Should have 2 entries (shared-id deduped, unique-id kept) *)
  assert (List.length entries = 2);
  (* The shared-id entry should be the newer one *)
  let shared = List.find (fun (e : Sortal_feed.Entry.t) -> e.id = "shared-id") entries in
  assert (shared.date = Some t2);
  traceln "  dedup: keeps newer entry, correct count";
  (* Cleanup *)
  List.iter (fun f ->
    let p = Sortal_feed.Store.feed_file store "testuser" f in
    (try Eio.Path.unlink p with _ -> ())
  ) [feed1; feed2];
  (try Eio.Path.unlink handle_dir with _ -> ());
  (try Eio.Path.unlink feeds_dir with _ -> ());
  (try Eio.Path.unlink tmp_dir with _ -> ())

let () =
  traceln "\n=== Feed Tests ===\n";
  test_url_to_filename ();
  test_atom_entry_conversion ();
  test_rss2_item_conversion ();
  test_jsonfeed_item_conversion ();
  test_compare_by_date ();
  test_dedup ();
  traceln "\n=== All Feed Tests Passed ===\n"
