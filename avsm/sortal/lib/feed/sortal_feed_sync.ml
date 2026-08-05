(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

type sync_result = {
  new_entries : int;
  total_entries : int;
  feed_name : string option;
}

let parse_error_message url exn =
  match exn with
  | Xmlm.Error ((line, col), err) ->
    Printf.sprintf "Failed to parse %s (line %d, col %d): %s" url line col (Xmlm.error_message err)
  | _ ->
    (* Syndic_error.Error is not re-exported, try to_string from Syndic *)
    let msg = Syndic.Rss2.Error.to_string exn in
    Printf.sprintf "Failed to parse %s: %s" url msg

let update_meta meta_path feed now ~entry_count =
  let meta : Sortal_feed_meta.t = match Sortal_feed_meta.load meta_path with
    | Some m -> { m with last_sync = Some now; entry_count }
    | None -> {
        feed_url = Sortal_schema.Feed.url feed;
        feed_type = Sortal_schema.Feed.feed_type feed;
        last_sync = Some now;
        etag = None;
        last_modified = None;
        entry_count;
      }
  in
  Sortal_feed_meta.save meta_path meta

let dedup_atom_entries entries =
  (* Deduplicate Atom entries by id URI, keeping the first occurrence
     (which is the newest when entries are sorted newest-first). *)
  let tbl = Hashtbl.create (List.length entries) in
  List.filter (fun (e : Syndic.Atom.entry) ->
    let key = Uri.to_string e.id in
    if Hashtbl.mem tbl key then false
    else (Hashtbl.replace tbl key (); true)
  ) entries

let sync_atom ~store ~handle feed body meta_path =
  let url = Sortal_schema.Feed.url feed in
  try
    let path = Sortal_feed_store.feed_file store handle feed in
    let input = Xmlm.make_input (`String (0, body)) in
    let new_feed = Syndic.Atom.parse input in
    let existing_count, merged = match Sortal_feed_store.load_atom path with
      | Some existing ->
        let count = List.length existing.entries in
        (* Syndic.Atom.aggregate does NOT deduplicate — it just concatenates.
           We must deduplicate by entry ID ourselves. *)
        let combined =
          Syndic.Atom.aggregate ~sort:`Newest_first [existing; new_feed]
        in
        let deduped = { combined with entries = dedup_atom_entries combined.entries } in
        (count, deduped)
      | None -> (0, new_feed)
    in
    Sortal_feed_store.save_atom path merged;
    let total = List.length merged.entries in
    let new_entries = max 0 (total - existing_count) in
    update_meta meta_path feed (Ptime_clock.now ()) ~entry_count:total;
    Ok { new_entries; total_entries = total;
         feed_name = Sortal_schema.Feed.name feed }
  with exn ->
    Error (parse_error_message url exn)

let sync_rss ~store ~handle feed body meta_path =
  let url = Sortal_schema.Feed.url feed in
  try
    let path = Sortal_feed_store.feed_file store handle feed in
    let input = Xmlm.make_input (`String (0, body)) in
    let channel = Syndic.Rss2.parse input in
    let total = List.length channel.items in
    (* RSS is stored as raw XML with no merge — determine new entries by
       comparing against the previous known count from metadata. *)
    let existing_count = match Sortal_feed_meta.load meta_path with
      | Some m -> m.entry_count
      | None -> 0
    in
    Sortal_feed_store.save_rss_raw path body;
    let new_entries = max 0 (total - existing_count) in
    update_meta meta_path feed (Ptime_clock.now ()) ~entry_count:total;
    Ok { new_entries; total_entries = total;
         feed_name = Sortal_schema.Feed.name feed }
  with exn ->
    Error (parse_error_message url exn)

let sync_jsonfeed ~store ~handle feed body meta_path =
  let url = Sortal_schema.Feed.url feed in
  let path = Sortal_feed_store.feed_file store handle feed in
  match Jsonfeed.of_string body with
  | Error err ->
    Error (Printf.sprintf "Failed to parse %s: %s" url (Jsont.Error.to_string err))
  | Ok new_feed ->
    try
      let new_items = Jsonfeed.items new_feed in
      let existing_count, merged_items = match Sortal_feed_store.load_jsonfeed path with
        | Some existing ->
          let existing_items = Jsonfeed.items existing in
          let count = List.length existing_items in
          let tbl = Hashtbl.create 128 in
          List.iter (fun item ->
            Hashtbl.replace tbl (Jsonfeed.Item.id item) item
          ) existing_items;
          List.iter (fun item ->
            let id = Jsonfeed.Item.id item in
            match Hashtbl.find_opt tbl id with
            | None -> Hashtbl.replace tbl id item
            | Some old ->
              if Jsonfeed.Item.compare item old > 0 then
                Hashtbl.replace tbl id item
          ) new_items;
          (count, Hashtbl.fold (fun _ v acc -> v :: acc) tbl [])
        | None -> (0, new_items)
      in
      let merged_feed = Jsonfeed.create
        ~title:(Jsonfeed.title new_feed)
        ?home_page_url:(Jsonfeed.home_page_url new_feed)
        ?feed_url:(Jsonfeed.feed_url new_feed)
        ?description:(Jsonfeed.description new_feed)
        ~items:merged_items
        ()
      in
      Sortal_feed_store.save_jsonfeed path merged_feed;
      let total = List.length merged_items in
      let new_entries = max 0 (total - existing_count) in
      update_meta meta_path feed (Ptime_clock.now ()) ~entry_count:total;
      Ok { new_entries; total_entries = total;
           feed_name = Sortal_schema.Feed.name feed }
    with exn ->
      Error (parse_error_message url exn)

let sync_feed ~session ~store ~handle ?(force=false) feed =
  let meta_path = Sortal_feed_store.meta_file store handle feed in
  let existing_meta = Sortal_feed_meta.load meta_path in
  let etag = if force then None else Option.bind existing_meta (fun m -> m.etag) in
  let last_modified = if force then None else Option.bind existing_meta (fun m -> m.last_modified) in
  let url = Sortal_schema.Feed.url feed in
  (* Ensure directory structure exists *)
  Sortal_feed_store.ensure_feed_dir store handle;
  match Sortal_feed_fetch.fetch ~session ?etag ?last_modified url with
  | Error `Not_modified ->
    let total = match existing_meta with
      | Some m -> m.entry_count
      | None -> 0
    in
    Ok { new_entries = 0; total_entries = total;
         feed_name = Sortal_schema.Feed.name feed }
  | Error (`Error msg) ->
    Error (Printf.sprintf "Failed to fetch %s: %s" url msg)
  | Ok result ->
    let update_http_meta meta_path etag last_modified =
      match Sortal_feed_meta.load meta_path with
      | Some m ->
        let m = { m with
          Sortal_feed_meta.etag = (match etag with Some _ -> etag | None -> m.etag);
          last_modified = (match last_modified with Some _ -> last_modified | None -> m.last_modified);
        } in
        Sortal_feed_meta.save meta_path m
      | None -> ()
    in
    let res = match Sortal_schema.Feed.feed_type feed with
      | Atom -> sync_atom ~store ~handle feed result.body meta_path
      | Rss -> sync_rss ~store ~handle feed result.body meta_path
      | Json -> sync_jsonfeed ~store ~handle feed result.body meta_path
      | Manual ->
        (* Manual feeds are handled by sortal.discover, not HTTP sync *)
        Ok { new_entries = 0; total_entries = 0;
             feed_name = Sortal_schema.Feed.name feed }
    in
    (match res with
     | Ok _ -> update_http_meta meta_path result.etag result.last_modified
     | Error _ -> ());
    res

let sync_all ~session ~store ~handle ?force feeds =
  let ok_results = ref [] in
  let err_results = ref [] in
  List.iter (fun feed ->
    let url = Sortal_schema.Feed.url feed in
    match sync_feed ~session ~store ~handle ?force feed with
    | Ok r -> ok_results := r :: !ok_results
    | Error e ->
      Logs.warn (fun m -> m "%s" e);
      err_results := e :: !err_results
    | exception exn ->
      let msg = Printf.sprintf "Failed to sync %s: %s" url (Printexc.to_string exn) in
      Logs.warn (fun m -> m "%s" msg);
      err_results := msg :: !err_results
  ) feeds;
  let oks = List.rev !ok_results in
  let errs = List.rev !err_results in
  if oks = [] && errs <> [] then
    Error (String.concat "; " errs)
  else
    Ok oks
