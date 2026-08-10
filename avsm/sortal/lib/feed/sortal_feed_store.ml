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

(* [feed_file]/[meta_file]/[annotations_file] derive their path from
   [feed]'s recorded type, so a feed reclassified by [Sortal_feed_sync]
   (its actual format found to differ from what the contact YAML records)
   would point at a fresh, empty path, orphaning everything already
   downloaded under the old one. [effective_type] and [relocate] below
   keep reads and writes pointed at wherever the content actually is. *)
let known_types = Sortal_schema.Feed.[ Atom; Rss; Json ]

(* A silent probe, deliberately not routed through [load_atom]/[load_rss]/
   [load_jsonfeed]: those log a warning on a parse failure, which is the
   expected, ordinary outcome here for a feed whose format has genuinely
   changed, not something a reader needs to see. *)
let parses_as feed_type path =
  try
    let data = Eio.Path.load path in
    match feed_type with
    | Sortal_schema.Feed.Atom | Sortal_schema.Feed.Manual ->
      ignore (Syndic.Atom.parse (Xmlm.make_input (`String (0, data))));
      true
    | Sortal_schema.Feed.Rss ->
      ignore (Syndic.Rss2.parse (Xmlm.make_input (`String (0, data))));
      true
    | Sortal_schema.Feed.Json -> Result.is_ok (Jsonfeed.of_string data)
  with _ -> false

let mtime path =
  try Some (Eio.Path.stat ~follow:true path).Eio.File.Stat.mtime
  with Eio.Io _ -> None

let effective_type t handle feed =
  match Sortal_schema.Feed.feed_type feed with
  | Sortal_schema.Feed.Manual as ft -> ft
  | recorded ->
    let candidates =
      List.filter_map
        (fun ft ->
          let p = feed_file t handle (Sortal_schema.Feed.set_feed_type feed ft) in
          if Eio.Path.is_file p then Some (ft, Option.value ~default:0. (mtime p))
          else None)
        known_types
    in
    (match candidates with
     | [] -> recorded
     | first :: rest ->
       (* The freshest file wins: once a reclassified feed has been
          synced at all, its new file is more recent than whatever was
          left behind at the old, no-longer-matching type. *)
       fst
         (List.fold_left
            (fun (bft, bmt) (ft, mt) -> if mt > bmt then (ft, mt) else (bft, bmt))
            first rest))

let relocate t handle feed to_type =
  match Sortal_schema.Feed.feed_type feed with
  | Manual -> ()
  | _ ->
    let from_type = effective_type t handle feed in
    if from_type <> to_type then begin
      let from_feed = Sortal_schema.Feed.set_feed_type feed from_type in
      let to_feed = Sortal_schema.Feed.set_feed_type feed to_type in
      let src = feed_file t handle from_feed in
      (* Only relocate content that genuinely reads as [to_type]: a feed
         simply mislabelled from the start moves over and keeps merging
         normally. A feed whose format has truly changed server-side
         (mdales's blog once really did serve RSS, and now serves Atom at
         the same URL) is left exactly where it is rather than being
         moved somewhere it can only fail to parse and get silently
         replaced by [save_atom]/[save_rss_raw]/[save_jsonfeed]'s
         truncating write. [effective_type] then finds it by trying every
         known type, so it is read, not lost, just no longer preferred
         once a fresher file exists. *)
      if Eio.Path.is_file src && parses_as to_type src then begin
        let dst = feed_file t handle to_feed in
        if not (Eio.Path.is_file dst) then Eio.Path.rename src dst;
        let move_companion accessor =
          let s = accessor t handle from_feed and d = accessor t handle to_feed in
          if Eio.Path.is_file s && not (Eio.Path.is_file d) then Eio.Path.rename s d
        in
        move_companion meta_file;
        move_companion annotations_file
      end
    end

let entries_of_feed t ~handle feed =
  let source_feed = Sortal_schema.Feed.url feed in
  (* Read from wherever [effective_type] finds content, not from [feed]'s
     recorded type: sync may have found a feed's real format to differ
     from what the contact YAML still records. *)
  let ft = effective_type t handle feed in
  let path = feed_file t handle (Sortal_schema.Feed.set_feed_type feed ft) in
  match ft with
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
