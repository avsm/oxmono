(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

type t = {
  data_dir : Eio.Fs.dir_ty Eio.Path.t;
}

let create data_dir = { data_dir }

let create_from_xdg xdg =
  let data_dir = Xdge.data_dir xdg in
  { data_dir }

let url_to_filename url =
  let hash = Digest.to_hex (Digest.string url) in
  String.sub hash 0 16

let feed_ext feed =
  match Sortal_schema.Feed.feed_type feed with
  | Atom | Manual -> ".atom"
  | Rss -> ".rss"
  | Json -> ".json"

let feed_dir t handle =
  Eio.Path.(t.data_dir / "feeds" / handle)

let ensure_dir path =
  try Eio.Path.mkdir ~perm:0o755 path with
  | Eio.Io _ -> ()

let ensure_feed_dir t handle =
  let feeds_dir = Eio.Path.(t.data_dir / "feeds") in
  ensure_dir feeds_dir;
  let dir = feed_dir t handle in
  ensure_dir dir


let feed_file t handle feed =
  let dir = feed_dir t handle in
  let hash = url_to_filename (Sortal_schema.Feed.url feed) in
  Eio.Path.(dir / (hash ^ feed_ext feed))

let meta_file t handle feed =
  let dir = feed_dir t handle in
  let hash = url_to_filename (Sortal_schema.Feed.url feed) in
  Eio.Path.(dir / (hash ^ feed_ext feed ^ ".meta.json"))

let annotations_file t handle feed =
  let dir = feed_dir t handle in
  let hash = url_to_filename (Sortal_schema.Feed.url feed) in
  Eio.Path.(dir / (hash ^ feed_ext feed ^ ".annotations.json"))

let atom_ns_prefix s =
  match s with
  | "http://www.w3.org/2005/Atom" -> Some ""
  | "http://www.w3.org/1999/xhtml" -> Some ""
  | _ -> Some s

let save_atom path feed =
  let xml = Syndic.Atom.to_xml feed in
  let data = Syndic.XML.to_string ~ns_prefix:atom_ns_prefix xml in
  Eio.Path.save ~create:(`Or_truncate 0o644) path data

let load_atom path =
  try
    let data = Eio.Path.load path in
    let input = Xmlm.make_input (`String (0, data)) in
    Some (Syndic.Atom.parse input)
  with
  | Eio.Io (Eio.Fs.E (Eio.Fs.Not_found _), _) -> None
  | exn ->
    Logs.warn (fun m -> m "Failed to parse Atom feed %a: %s"
      Eio.Path.pp path (Printexc.to_string exn));
    None

let save_rss_raw path data =
  Eio.Path.save ~create:(`Or_truncate 0o644) path data

let load_rss path =
  try
    let data = Eio.Path.load path in
    let input = Xmlm.make_input (`String (0, data)) in
    Some (Syndic.Rss2.parse input)
  with
  | Eio.Io (Eio.Fs.E (Eio.Fs.Not_found _), _) -> None
  | exn ->
    Logs.warn (fun m -> m "Failed to parse RSS feed %a: %s"
      Eio.Path.pp path (Printexc.to_string exn));
    None

let save_jsonfeed path feed =
  match Jsonfeed.to_string feed with
  | Ok data -> Eio.Path.save ~create:(`Or_truncate 0o644) path data
  | Error err -> failwith ("Failed to encode JSON Feed: " ^ Jsont.Error.to_string err)

let load_jsonfeed path =
  try
    let data = Eio.Path.load path in
    match Jsonfeed.of_string data with
    | Ok feed -> Some feed
    | Error err ->
      Logs.warn (fun m -> m "Failed to decode JSON Feed: %s" (Jsont.Error.to_string err));
      None
  with
  | Eio.Io (Eio.Fs.E (Eio.Fs.Not_found _), _) -> None
  | exn ->
    Logs.warn (fun m -> m "Failed to load JSON Feed %a: %s"
      Eio.Path.pp path (Printexc.to_string exn));
    None

let entries_of_feed t ~handle feed =
  let source_feed = Sortal_schema.Feed.url feed in
  let path = feed_file t handle feed in
  match Sortal_schema.Feed.feed_type feed with
  | Atom | Manual ->
    (match load_atom path with
     | Some atom_feed ->
       List.map (Sortal_feed_entry.of_atom_entry ~source_feed) atom_feed.entries
     | None -> [])
  | Rss ->
    (match load_rss path with
     | Some channel ->
       List.map (Sortal_feed_entry.of_rss2_item ~source_feed) channel.items
     | None -> [])
  | Json ->
    (match load_jsonfeed path with
     | Some jf ->
       List.map (Sortal_feed_entry.of_jsonfeed_item ~source_feed) (Jsonfeed.items jf)
     | None -> [])

let all_entries t ~handle feeds =
  let all = List.concat_map (entries_of_feed t ~handle) feeds in
  let tbl = Hashtbl.create (List.length all) in
  List.iter (fun (entry : Sortal_feed_entry.t) ->
    match Hashtbl.find_opt tbl entry.id with
    | None -> Hashtbl.replace tbl entry.id entry
    | Some existing ->
      let keep = match existing.date, entry.date with
        | Some d1, Some d2 -> if Ptime.compare d2 d1 > 0 then entry else existing
        | None, Some _ -> entry
        | Some _, None -> existing
        | None, None -> existing
      in
      Hashtbl.replace tbl entry.id keep
  ) all;
  let entries = Hashtbl.fold (fun _ v acc -> v :: acc) tbl [] in
  List.sort Sortal_feed_entry.compare_by_date entries
