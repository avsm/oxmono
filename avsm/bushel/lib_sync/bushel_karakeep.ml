(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Sync bushel links with karakeep bookmark service *)

let src = Logs.Src.create "bushel.karakeep" ~doc:"Bushel-Karakeep sync"
module Log = (val Logs.src_log src : Logs.LOG)

(** {1 JSON Helpers} *)

let json_obj fields =
  let m = Jsont.Meta.none in
  Jsont.Object (List.map (fun (k, v) -> ((k, m), v)) fields, m)

let json_string s = Jsont.String (s, Jsont.Meta.none)

let find_member key = function
  | Jsont.Object (members, _) ->
    List.find_map (fun ((k, _), v) ->
      if k = key then Some v else None
    ) members
  | _ -> None

let get_string key json =
  match find_member key json with
  | Some (Jsont.String (s, _)) -> Some s
  | _ -> None

let get_array key json =
  match find_member key json with
  | Some (Jsont.Array (items, _)) -> items
  | _ -> []

let extract_tag_name (json : Jsont.json) =
  get_string "name" json

(** {1 Pagination} *)

let fetch_all_bookmarks api =
  let all = ref [] in
  let cursor = ref None in
  let continue = ref true in
  while !continue do
    let result =
      Karakeep.PaginatedBookmarks.get_bookmarks ?cursor:!cursor api ()
    in
    let bookmarks = Karakeep.PaginatedBookmarks.T.bookmarks result in
    all := bookmarks @ !all;
    match Karakeep.PaginatedBookmarks.T.next_cursor result with
    | Some c -> cursor := Some c
    | None -> continue := false
  done;
  !all

(** {1 URL-to-Bookmark Index} *)

let build_url_index bookmarks =
  let tbl = Hashtbl.create (List.length bookmarks) in
  List.iter (fun (b : Karakeep.Bookmark.T.t) ->
    let content = Karakeep.Bookmark.T.content b in
    match get_string "url" content with
    | Some url -> Hashtbl.replace tbl url b
    | None -> ()
  ) bookmarks;
  tbl

(** {1 Sync Logic} *)

let sync_links ~dry_run ~api ~data_dir =
  let links_file = Filename.concat data_dir "links.yml" in
  let links = Bushel.Link.load_links_file links_file in

  Log.info (fun m -> m "Loaded %d links from %s" (List.length links) links_file);

  (* Fetch all karakeep bookmarks and build URL index *)
  Log.info (fun m -> m "Fetching all karakeep bookmarks...");
  let bookmarks = fetch_all_bookmarks api in
  Log.info (fun m -> m "Fetched %d bookmarks from karakeep" (List.length bookmarks));
  let url_index = build_url_index bookmarks in

  (* Build id index for pull phase *)
  let id_index = Hashtbl.create (List.length bookmarks) in
  List.iter (fun (b : Karakeep.Bookmark.T.t) ->
    Hashtbl.replace id_index (Karakeep.Bookmark.T.id b) b
  ) bookmarks;

  let pushed = ref 0 in
  let pulled = ref 0 in
  let details = ref [] in

  (* Process each link *)
  let updated_links = List.map (fun (link : Bushel.Link.t) ->
    let url = Bushel.Link.url link in

    match link.karakeep with
    | None ->
      (* Push: link has no karakeep data yet *)
      (match Hashtbl.find_opt url_index url with
       | Some existing_bookmark ->
         (* URL already exists in karakeep, just record the ID *)
         let id = Karakeep.Bookmark.T.id existing_bookmark in
         Log.info (fun m -> m "Link %s already in karakeep as %s" url id);
         incr pushed;
         details := Printf.sprintf "Linked %s -> %s" url id :: !details;
         let karakeep : Bushel.Link.karakeep_data = {
           remote_url = "https://hoard.recoil.org";
           id;
           tags = [];
           metadata = [];
         } in
         { link with karakeep = Some karakeep }
       | None ->
         if dry_run then begin
           Log.info (fun m -> m "Would create karakeep bookmark for %s" url);
           incr pushed;
           details := Printf.sprintf "Would create: %s" url :: !details;
           link
         end else begin
           Log.info (fun m -> m "Creating karakeep bookmark for %s" url);
           let body = json_obj [
             ("type", json_string "link");
             ("url", json_string url);
           ] in
           try
             let bookmark = Karakeep.Bookmark.post_bookmarks ~body api () in
             let id = Karakeep.Bookmark.T.id bookmark in
             incr pushed;
             details := Printf.sprintf "Created: %s -> %s" url id :: !details;
             let karakeep : Bushel.Link.karakeep_data = {
               remote_url = "https://hoard.recoil.org";
               id;
               tags = [];
               metadata = [];
             } in
             { link with karakeep = Some karakeep }
           with e ->
             Log.err (fun m -> m "Failed to create bookmark for %s: %s"
               url (Printexc.to_string e));
             link
         end)
    | Some kd ->
      (* Pull: link has karakeep data, update metadata from karakeep *)
      let bookmark = match Hashtbl.find_opt id_index kd.id with
        | Some b -> Some b
        | None ->
          (* Bookmark might have been deleted from karakeep *)
          Log.warn (fun m -> m "Bookmark %s not found in karakeep" kd.id);
          None
      in
      (match bookmark with
       | None -> link
       | Some b ->
         let content = Karakeep.Bookmark.T.content b in
         let title = get_string "title" content in
         let tags_json = get_array "tags" (Jsont_bytesrw.encode_string Karakeep.Bookmark.T.jsont b
           |> Result.get_ok
           |> Jsont_bytesrw.decode_string Jsont.json
           |> Result.get_ok) in
         let tag_names = List.filter_map extract_tag_name tags_json in

         let new_metadata =
           (match title with Some t -> [("title", t)] | None -> [])
           @ (match Karakeep.Bookmark.T.summary b with
              | Some s when s <> "" -> [("summary", s)]
              | _ -> [])
           @ (match get_string "favicon" content with
              | Some f when f <> "" -> [("favicon", f)]
              | _ -> [])
         in
         let merged_metadata =
           let tbl = Hashtbl.create (List.length kd.metadata) in
           List.iter (fun (k, v) -> Hashtbl.replace tbl k v) kd.metadata;
           List.iter (fun (k, v) -> Hashtbl.replace tbl k v) new_metadata;
           Hashtbl.fold (fun k v acc -> (k, v) :: acc) tbl []
         in
         let merged_tags = List.sort_uniq String.compare (kd.tags @ tag_names) in

         if merged_metadata <> kd.metadata || merged_tags <> kd.tags then begin
           incr pulled;
           details := Printf.sprintf "Updated metadata for %s" url :: !details
         end;

         let updated_kd = { kd with
           metadata = merged_metadata;
           tags = merged_tags;
         } in
         { link with karakeep = Some updated_kd })
  ) links in

  (* Save updated links *)
  if not dry_run && (!pushed > 0 || !pulled > 0) then
    Bushel.Link.save_links_file links_file updated_links;

  (true,
   Printf.sprintf "%d pushed, %d pulled" !pushed !pulled,
   List.rev !details)
