(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Cmdliner
open Karakeep_auth

let version = "0.2.0"

let run_with_client ~profile f env =
  Cmd.with_client ?profile (fun client ->
    f (Client.client client)
  ) env

(** Build a JSON object from a list of key-value pairs *)
let json_obj fields =
  let m = Jsont.Meta.none in
  Jsont.Object (List.map (fun (k, v) -> ((k, m), v)) fields, m)

let json_string s = Jsont.String (s, Jsont.Meta.none)
let json_array items = Jsont.Array (items, Jsont.Meta.none)
let json_bool b = Jsont.Bool (b, Jsont.Meta.none)

let opt_fields f = function Some v -> [f v] | None -> []

(* Bookmark commands *)

let list_bookmarks_cmd env =
  let run (style_renderer, level) profile fmt archived favourited include_content limit cursor =
    Cmd.setup_logging_simple style_renderer level;
    Error.wrap (fun () ->
      run_with_client ~profile (fun api ->
        let result =
          Karakeep.PaginatedBookmarks.get_bookmarks
            ?archived ?favourited ?include_content ?limit ?cursor api ()
        in
        Cmd.print_bookmarks fmt (Karakeep.PaginatedBookmarks.T.bookmarks result);
        Option.iter (fun c -> Logs.info (fun m -> m "Next cursor: %s" c))
          (Karakeep.PaginatedBookmarks.T.next_cursor result)
      ) env)
  in
  let doc = "List bookmarks with optional filters." in
  let info = Cmdliner.Cmd.info "list" ~doc in
  Cmdliner.Cmd.v info
    Term.(const run $ Cmd.setup_logging $ Cmd.profile_arg $ Cmd.output_format_term
      $ Cmd.archived_term $ Cmd.favourited_term $ Cmd.include_content_term
      $ Cmd.limit_term $ Cmd.cursor_term)

let get_bookmark_cmd env =
  let run (style_renderer, level) profile fmt bookmark_id =
    Cmd.setup_logging_simple style_renderer level;
    Error.wrap (fun () ->
      run_with_client ~profile (fun api ->
        let bookmark = Karakeep.Bookmark.get_bookmarks ~bookmark_id api () in
        Cmd.print_bookmark fmt bookmark
      ) env)
  in
  let doc = "Get details of a specific bookmark." in
  let info = Cmdliner.Cmd.info "get" ~doc in
  Cmdliner.Cmd.v info
    Term.(const run $ Cmd.setup_logging $ Cmd.profile_arg $ Cmd.output_format_term
      $ Cmd.bookmark_id_term)

let create_bookmark_cmd env =
  let run (style_renderer, level) profile fmt url title note _tags =
    Cmd.setup_logging_simple style_renderer level;
    Error.wrap (fun () ->
      run_with_client ~profile (fun api ->
        let body = json_obj (
          [("type", json_string "link"); ("url", json_string url)]
          @ opt_fields (fun t -> ("title", json_string t)) title
          @ opt_fields (fun n -> ("note", json_string n)) note
        ) in
        let bookmark = Karakeep.Bookmark.post_bookmarks ~body api () in
        Cmd.print_bookmark fmt bookmark
      ) env)
  in
  let doc = "Create a new bookmark." in
  let info = Cmdliner.Cmd.info "create" ~doc in
  Cmdliner.Cmd.v info
    Term.(const run $ Cmd.setup_logging $ Cmd.profile_arg $ Cmd.output_format_term
      $ Cmd.url_term $ Cmd.title_term $ Cmd.note_term $ Cmd.tags_term)

let update_bookmark_cmd env =
  let run (style_renderer, level) profile fmt bookmark_id title note summary =
    Cmd.setup_logging_simple style_renderer level;
    Error.wrap (fun () ->
      run_with_client ~profile (fun api ->
        let body = json_obj (
          opt_fields (fun t -> ("title", json_string t)) title
          @ opt_fields (fun n -> ("note", json_string n)) note
          @ opt_fields (fun s -> ("summary", json_string s)) summary
        ) in
        let result = Karakeep.Client.patch_bookmarks ~bookmark_id ~body api () in
        match fmt with
        | Cmd.Json -> print_endline (Jsont_bytesrw.encode_string Jsont.json result |> Result.get_ok)
        | _ ->
            match Jsont_bytesrw.decode_string Karakeep.Bookmark.T.jsont
                    (Jsont_bytesrw.encode_string Jsont.json result |> Result.get_ok) with
            | Ok b -> Cmd.print_bookmark fmt b
            | Error _ -> ()
      ) env)
  in
  let doc = "Update a bookmark." in
  let info = Cmdliner.Cmd.info "update" ~doc in
  Cmdliner.Cmd.v info
    Term.(const run $ Cmd.setup_logging $ Cmd.profile_arg $ Cmd.output_format_term
      $ Cmd.bookmark_id_term $ Cmd.title_term $ Cmd.note_term $ Cmd.summary_term)

let delete_bookmark_cmd env =
  let run (style_renderer, level) profile bookmark_id =
    Cmd.setup_logging_simple style_renderer level;
    Error.wrap (fun () ->
      run_with_client ~profile (fun api ->
        let _ = Karakeep.Client.delete_bookmarks ~bookmark_id api () in
        Logs.app (fun m -> m "Deleted bookmark %s" bookmark_id)
      ) env)
  in
  let doc = "Delete a bookmark." in
  let info = Cmdliner.Cmd.info "delete" ~doc in
  Cmdliner.Cmd.v info
    Term.(const run $ Cmd.setup_logging $ Cmd.profile_arg $ Cmd.bookmark_id_term)

let archive_bookmark_cmd env =
  let run (style_renderer, level) profile fmt bookmark_id =
    Cmd.setup_logging_simple style_renderer level;
    Error.wrap (fun () ->
      run_with_client ~profile (fun api ->
        let body = json_obj [("archived", json_bool true)] in
        let result = Karakeep.Client.patch_bookmarks ~bookmark_id ~body api () in
        match Jsont_bytesrw.decode_string Karakeep.Bookmark.T.jsont
                (Jsont_bytesrw.encode_string Jsont.json result |> Result.get_ok) with
        | Ok b -> Cmd.print_bookmark fmt b
        | Error _ -> ()
      ) env)
  in
  let doc = "Archive a bookmark." in
  let info = Cmdliner.Cmd.info "archive" ~doc in
  Cmdliner.Cmd.v info
    Term.(const run $ Cmd.setup_logging $ Cmd.profile_arg $ Cmd.output_format_term
      $ Cmd.bookmark_id_term)

let unarchive_bookmark_cmd env =
  let run (style_renderer, level) profile fmt bookmark_id =
    Cmd.setup_logging_simple style_renderer level;
    Error.wrap (fun () ->
      run_with_client ~profile (fun api ->
        let body = json_obj [("archived", json_bool false)] in
        let result = Karakeep.Client.patch_bookmarks ~bookmark_id ~body api () in
        match Jsont_bytesrw.decode_string Karakeep.Bookmark.T.jsont
                (Jsont_bytesrw.encode_string Jsont.json result |> Result.get_ok) with
        | Ok b -> Cmd.print_bookmark fmt b
        | Error _ -> ()
      ) env)
  in
  let doc = "Unarchive a bookmark." in
  let info = Cmdliner.Cmd.info "unarchive" ~doc in
  Cmdliner.Cmd.v info
    Term.(const run $ Cmd.setup_logging $ Cmd.profile_arg $ Cmd.output_format_term
      $ Cmd.bookmark_id_term)

let favourite_bookmark_cmd env =
  let run (style_renderer, level) profile fmt bookmark_id =
    Cmd.setup_logging_simple style_renderer level;
    Error.wrap (fun () ->
      run_with_client ~profile (fun api ->
        let body = json_obj [("favourited", json_bool true)] in
        let result = Karakeep.Client.patch_bookmarks ~bookmark_id ~body api () in
        match Jsont_bytesrw.decode_string Karakeep.Bookmark.T.jsont
                (Jsont_bytesrw.encode_string Jsont.json result |> Result.get_ok) with
        | Ok b -> Cmd.print_bookmark fmt b
        | Error _ -> ()
      ) env)
  in
  let doc = "Mark a bookmark as favourite." in
  let info = Cmdliner.Cmd.info "fav" ~doc in
  Cmdliner.Cmd.v info
    Term.(const run $ Cmd.setup_logging $ Cmd.profile_arg $ Cmd.output_format_term
      $ Cmd.bookmark_id_term)

let unfavourite_bookmark_cmd env =
  let run (style_renderer, level) profile fmt bookmark_id =
    Cmd.setup_logging_simple style_renderer level;
    Error.wrap (fun () ->
      run_with_client ~profile (fun api ->
        let body = json_obj [("favourited", json_bool false)] in
        let result = Karakeep.Client.patch_bookmarks ~bookmark_id ~body api () in
        match Jsont_bytesrw.decode_string Karakeep.Bookmark.T.jsont
                (Jsont_bytesrw.encode_string Jsont.json result |> Result.get_ok) with
        | Ok b -> Cmd.print_bookmark fmt b
        | Error _ -> ()
      ) env)
  in
  let doc = "Remove favourite mark from a bookmark." in
  let info = Cmdliner.Cmd.info "unfav" ~doc in
  Cmdliner.Cmd.v info
    Term.(const run $ Cmd.setup_logging $ Cmd.profile_arg $ Cmd.output_format_term
      $ Cmd.bookmark_id_term)

let summarize_bookmark_cmd env =
  let run (style_renderer, level) profile fmt bookmark_id =
    Cmd.setup_logging_simple style_renderer level;
    Error.wrap (fun () ->
      run_with_client ~profile (fun api ->
        let result = Karakeep.Client.post_bookmarks_summarize ~bookmark_id api () in
        match fmt with
        | Cmd.Json -> print_endline (Jsont_bytesrw.encode_string Jsont.json result |> Result.get_ok)
        | _ ->
            (* Extract summary from response *)
            (match result with
             | Jsont.Object (members, _) ->
                 (match List.find_map (fun ((k, _), v) -> if k = "summary" then Some v else None) members with
                  | Some (Jsont.String (s, _)) -> print_endline s
                  | _ -> ())
             | _ -> ())
      ) env)
  in
  let doc = "Generate an AI summary for a bookmark." in
  let info = Cmdliner.Cmd.info "summarize" ~doc in
  Cmdliner.Cmd.v info
    Term.(const run $ Cmd.setup_logging $ Cmd.profile_arg $ Cmd.output_format_term
      $ Cmd.bookmark_id_term)

let search_bookmarks_cmd env =
  let run (style_renderer, level) profile fmt query limit cursor =
    Cmd.setup_logging_simple style_renderer level;
    Error.wrap (fun () ->
      run_with_client ~profile (fun api ->
        let result =
          Karakeep.PaginatedBookmarks.get_bookmarks_search ~q:query ?limit ?cursor api ()
        in
        Cmd.print_bookmarks fmt (Karakeep.PaginatedBookmarks.T.bookmarks result);
        Option.iter (fun c -> Logs.info (fun m -> m "Next cursor: %s" c))
          (Karakeep.PaginatedBookmarks.T.next_cursor result)
      ) env)
  in
  let doc = "Search bookmarks." in
  let info = Cmdliner.Cmd.info "search" ~doc in
  Cmdliner.Cmd.v info
    Term.(const run $ Cmd.setup_logging $ Cmd.profile_arg $ Cmd.output_format_term
      $ Cmd.search_query_term $ Cmd.limit_term $ Cmd.cursor_term)

let bookmarks_cmd env =
  let doc = "Bookmark operations." in
  let info = Cmdliner.Cmd.info "bookmarks" ~doc in
  Cmdliner.Cmd.group info
    [ list_bookmarks_cmd env
    ; get_bookmark_cmd env
    ; create_bookmark_cmd env
    ; update_bookmark_cmd env
    ; delete_bookmark_cmd env
    ; archive_bookmark_cmd env
    ; unarchive_bookmark_cmd env
    ; favourite_bookmark_cmd env
    ; unfavourite_bookmark_cmd env
    ; summarize_bookmark_cmd env
    ; search_bookmarks_cmd env
    ]

(* Tag commands *)

let list_tags_cmd env =
  let run (style_renderer, level) profile fmt =
    Cmd.setup_logging_simple style_renderer level;
    Error.wrap (fun () ->
      run_with_client ~profile (fun api ->
        let result = Karakeep.Client.get_tags api () in
        (* Result is JSON with "tags" array *)
        match result with
        | Jsont.Object (members, _) ->
            (match List.find_map (fun ((k, _), v) -> if k = "tags" then Some v else None) members with
             | Some (Jsont.Array (items, _)) ->
                 let tags = List.filter_map (fun item ->
                   match Jsont_bytesrw.decode_string Karakeep.Tag.T.jsont
                           (Jsont_bytesrw.encode_string Jsont.json item |> Result.get_ok) with
                   | Ok t -> Some t
                   | Error _ -> None
                 ) items in
                 Cmd.print_tags fmt tags
             | _ -> ())
        | _ -> ()
      ) env)
  in
  let doc = "List all tags." in
  let info = Cmdliner.Cmd.info "list" ~doc in
  Cmdliner.Cmd.v info
    Term.(const run $ Cmd.setup_logging $ Cmd.profile_arg $ Cmd.output_format_term)

let get_tag_cmd env =
  let run (style_renderer, level) profile fmt tag_id =
    Cmd.setup_logging_simple style_renderer level;
    Error.wrap (fun () ->
      run_with_client ~profile (fun api ->
        let tag = Karakeep.Tag.get_tags ~tag_id api () in
        Cmd.print_tag fmt tag
      ) env)
  in
  let doc = "Get details of a specific tag." in
  let info = Cmdliner.Cmd.info "get" ~doc in
  Cmdliner.Cmd.v info
    Term.(const run $ Cmd.setup_logging $ Cmd.profile_arg $ Cmd.output_format_term
      $ Cmd.tag_id_term)

let tag_bookmarks_cmd env =
  let run (style_renderer, level) profile fmt tag_id limit cursor =
    Cmd.setup_logging_simple style_renderer level;
    Error.wrap (fun () ->
      run_with_client ~profile (fun api ->
        let result =
          Karakeep.PaginatedBookmarks.get_tags_bookmarks ~tag_id ?limit ?cursor api ()
        in
        Cmd.print_bookmarks fmt (Karakeep.PaginatedBookmarks.T.bookmarks result)
      ) env)
  in
  let doc = "List bookmarks with a specific tag." in
  let info = Cmdliner.Cmd.info "bookmarks" ~doc in
  Cmdliner.Cmd.v info
    Term.(const run $ Cmd.setup_logging $ Cmd.profile_arg $ Cmd.output_format_term
      $ Cmd.tag_id_term $ Cmd.limit_term $ Cmd.cursor_term)

let rename_tag_cmd env =
  let run (style_renderer, level) profile fmt tag_id name =
    Cmd.setup_logging_simple style_renderer level;
    Error.wrap (fun () ->
      run_with_client ~profile (fun api ->
        let body = json_obj [("name", json_string name)] in
        let result = Karakeep.Client.patch_tags ~tag_id ~body api () in
        match Jsont_bytesrw.decode_string Karakeep.Tag.T.jsont
                (Jsont_bytesrw.encode_string Jsont.json result |> Result.get_ok) with
        | Ok tag -> Cmd.print_tag fmt tag
        | Error _ -> ()
      ) env)
  in
  let doc = "Rename a tag." in
  let info = Cmdliner.Cmd.info "rename" ~doc in
  Cmdliner.Cmd.v info
    Term.(const run $ Cmd.setup_logging $ Cmd.profile_arg $ Cmd.output_format_term
      $ Cmd.tag_id_term $ Cmd.name_term)

let delete_tag_cmd env =
  let run (style_renderer, level) profile tag_id =
    Cmd.setup_logging_simple style_renderer level;
    Error.wrap (fun () ->
      run_with_client ~profile (fun api ->
        let _ = Karakeep.Client.delete_tags ~tag_id api () in
        Logs.app (fun m -> m "Deleted tag %s" tag_id)
      ) env)
  in
  let doc = "Delete a tag." in
  let info = Cmdliner.Cmd.info "delete" ~doc in
  Cmdliner.Cmd.v info
    Term.(const run $ Cmd.setup_logging $ Cmd.profile_arg $ Cmd.tag_id_term)

let attach_tags_cmd env =
  let run (style_renderer, level) profile bookmark_id tags =
    Cmd.setup_logging_simple style_renderer level;
    Error.wrap (fun () ->
      run_with_client ~profile (fun api ->
        let tag_objects = List.map (fun t ->
          json_obj [("tagName", json_string t)]
        ) tags in
        let body = json_obj [("tags", json_array tag_objects)] in
        let _ = Karakeep.Client.post_bookmarks_tags ~bookmark_id ~body api () in
        Logs.app (fun m -> m "Attached tags to bookmark %s" bookmark_id)
      ) env)
  in
  let doc = "Attach tags to a bookmark." in
  let info = Cmdliner.Cmd.info "attach" ~doc in
  Cmdliner.Cmd.v info
    Term.(const run $ Cmd.setup_logging $ Cmd.profile_arg $ Cmd.bookmark_id_term
      $ Cmd.tags_term)

let detach_tags_cmd env =
  let run (style_renderer, level) profile bookmark_id _tags =
    Cmd.setup_logging_simple style_renderer level;
    Error.wrap (fun () ->
      run_with_client ~profile (fun api ->
        let _ = Karakeep.Client.delete_bookmarks_tags ~bookmark_id api () in
        Logs.app (fun m -> m "Detached tags from bookmark %s" bookmark_id)
      ) env)
  in
  let doc = "Detach tags from a bookmark." in
  let info = Cmdliner.Cmd.info "detach" ~doc in
  Cmdliner.Cmd.v info
    Term.(const run $ Cmd.setup_logging $ Cmd.profile_arg $ Cmd.bookmark_id_term
      $ Cmd.tags_term)

let tags_cmd env =
  let doc = "Tag operations." in
  let info = Cmdliner.Cmd.info "tags" ~doc in
  Cmdliner.Cmd.group info
    [ list_tags_cmd env
    ; get_tag_cmd env
    ; tag_bookmarks_cmd env
    ; rename_tag_cmd env
    ; delete_tag_cmd env
    ; attach_tags_cmd env
    ; detach_tags_cmd env
    ]

(* List commands *)

let list_lists_cmd env =
  let run (style_renderer, level) profile fmt =
    Cmd.setup_logging_simple style_renderer level;
    Error.wrap (fun () ->
      run_with_client ~profile (fun api ->
        let result = Karakeep.Client.get_lists api () in
        match result with
        | Jsont.Object (members, _) ->
            (match List.find_map (fun ((k, _), v) -> if k = "lists" then Some v else None) members with
             | Some (Jsont.Array (items, _)) ->
                 let lists = List.filter_map (fun item ->
                   match Jsont_bytesrw.decode_string Karakeep.List.T.jsont
                           (Jsont_bytesrw.encode_string Jsont.json item |> Result.get_ok) with
                   | Ok l -> Some l
                   | Error _ -> None
                 ) items in
                 Cmd.print_lists fmt lists
             | _ -> ())
        | _ -> ()
      ) env)
  in
  let doc = "List all lists." in
  let info = Cmdliner.Cmd.info "list" ~doc in
  Cmdliner.Cmd.v info
    Term.(const run $ Cmd.setup_logging $ Cmd.profile_arg $ Cmd.output_format_term)

let get_list_cmd env =
  let run (style_renderer, level) profile fmt list_id =
    Cmd.setup_logging_simple style_renderer level;
    Error.wrap (fun () ->
      run_with_client ~profile (fun api ->
        let lst = Karakeep.List.get_lists ~list_id api () in
        Cmd.print_list fmt lst
      ) env)
  in
  let doc = "Get details of a specific list." in
  let info = Cmdliner.Cmd.info "get" ~doc in
  Cmdliner.Cmd.v info
    Term.(const run $ Cmd.setup_logging $ Cmd.profile_arg $ Cmd.output_format_term
      $ Cmd.list_id_term)

let create_list_cmd env =
  let run (style_renderer, level) profile fmt name icon description parent_id query =
    Cmd.setup_logging_simple style_renderer level;
    Error.wrap (fun () ->
      run_with_client ~profile (fun api ->
        let body = json_obj (
          [("name", json_string name); ("icon", json_string icon)]
          @ opt_fields (fun d -> ("description", json_string d)) description
          @ opt_fields (fun p -> ("parentId", json_string p)) parent_id
          @ opt_fields (fun q -> ("query", json_string q)) query
        ) in
        let lst = Karakeep.List.post_lists ~body api () in
        Cmd.print_list fmt lst
      ) env)
  in
  let doc = "Create a new list." in
  let info = Cmdliner.Cmd.info "create" ~doc in
  Cmdliner.Cmd.v info
    Term.(const run $ Cmd.setup_logging $ Cmd.profile_arg $ Cmd.output_format_term
      $ Cmd.name_term $ Cmd.icon_term $ Cmd.description_term
      $ Cmd.parent_id_term $ Cmd.query_term)

let update_list_cmd env =
  let run (style_renderer, level) profile fmt list_id name icon description query =
    Cmd.setup_logging_simple style_renderer level;
    Error.wrap (fun () ->
      run_with_client ~profile (fun api ->
        let body = json_obj (
          opt_fields (fun n -> ("name", json_string n)) name
          @ opt_fields (fun i -> ("icon", json_string i)) icon
          @ opt_fields (fun d -> ("description", json_string d)) description
          @ opt_fields (fun q -> ("query", json_string q)) query
        ) in
        let lst = Karakeep.List.patch_lists ~list_id ~body api () in
        Cmd.print_list fmt lst
      ) env)
  in
  let doc = "Update a list." in
  let info = Cmdliner.Cmd.info "update" ~doc in
  Cmdliner.Cmd.v info
    Term.(const run $ Cmd.setup_logging $ Cmd.profile_arg $ Cmd.output_format_term
      $ Cmd.list_id_term $ Cmd.name_opt_term $ Cmd.icon_opt_term
      $ Cmd.description_term $ Cmd.query_term)

let delete_list_cmd env =
  let run (style_renderer, level) profile list_id =
    Cmd.setup_logging_simple style_renderer level;
    Error.wrap (fun () ->
      run_with_client ~profile (fun api ->
        let _ = Karakeep.Client.delete_lists ~list_id api () in
        Logs.app (fun m -> m "Deleted list %s" list_id)
      ) env)
  in
  let doc = "Delete a list." in
  let info = Cmdliner.Cmd.info "delete" ~doc in
  Cmdliner.Cmd.v info
    Term.(const run $ Cmd.setup_logging $ Cmd.profile_arg $ Cmd.list_id_term)

let list_bookmarks_in_list_cmd env =
  let run (style_renderer, level) profile fmt list_id limit cursor =
    Cmd.setup_logging_simple style_renderer level;
    Error.wrap (fun () ->
      run_with_client ~profile (fun api ->
        let result =
          Karakeep.PaginatedBookmarks.get_lists_bookmarks ~list_id ?limit ?cursor api ()
        in
        Cmd.print_bookmarks fmt (Karakeep.PaginatedBookmarks.T.bookmarks result)
      ) env)
  in
  let doc = "List bookmarks in a list." in
  let info = Cmdliner.Cmd.info "bookmarks" ~doc in
  Cmdliner.Cmd.v info
    Term.(const run $ Cmd.setup_logging $ Cmd.profile_arg $ Cmd.output_format_term
      $ Cmd.list_id_term $ Cmd.limit_term $ Cmd.cursor_term)

let add_to_list_cmd env =
  let run (style_renderer, level) profile list_id bookmark_id =
    Cmd.setup_logging_simple style_renderer level;
    Error.wrap (fun () ->
      run_with_client ~profile (fun api ->
        let _ = Karakeep.Client.put_lists_bookmarks ~list_id ~bookmark_id api () in
        Logs.app (fun m -> m "Added bookmark %s to list %s" bookmark_id list_id)
      ) env)
  in
  let doc = "Add a bookmark to a list." in
  let info = Cmdliner.Cmd.info "add" ~doc in
  let bid_term =
    let doc = "Bookmark ID to add." in
    Arg.(required & pos 1 (some string) None & info [] ~docv:"BOOKMARK_ID" ~doc)
  in
  Cmdliner.Cmd.v info
    Term.(const run $ Cmd.setup_logging $ Cmd.profile_arg $ Cmd.list_id_term $ bid_term)

let remove_from_list_cmd env =
  let run (style_renderer, level) profile list_id bookmark_id =
    Cmd.setup_logging_simple style_renderer level;
    Error.wrap (fun () ->
      run_with_client ~profile (fun api ->
        let _ = Karakeep.Client.delete_lists_bookmarks ~list_id ~bookmark_id api () in
        Logs.app (fun m -> m "Removed bookmark %s from list %s" bookmark_id list_id)
      ) env)
  in
  let doc = "Remove a bookmark from a list." in
  let info = Cmdliner.Cmd.info "remove" ~doc in
  let bid_term =
    let doc = "Bookmark ID to remove." in
    Arg.(required & pos 1 (some string) None & info [] ~docv:"BOOKMARK_ID" ~doc)
  in
  Cmdliner.Cmd.v info
    Term.(const run $ Cmd.setup_logging $ Cmd.profile_arg $ Cmd.list_id_term $ bid_term)

let lists_cmd env =
  let doc = "List operations." in
  let info = Cmdliner.Cmd.info "lists" ~doc in
  Cmdliner.Cmd.group info
    [ list_lists_cmd env
    ; get_list_cmd env
    ; create_list_cmd env
    ; update_list_cmd env
    ; delete_list_cmd env
    ; list_bookmarks_in_list_cmd env
    ; add_to_list_cmd env
    ; remove_from_list_cmd env
    ]

(* Highlight commands *)

let list_highlights_cmd env =
  let run (style_renderer, level) profile fmt limit cursor =
    Cmd.setup_logging_simple style_renderer level;
    Error.wrap (fun () ->
      run_with_client ~profile (fun api ->
        let result = Karakeep.PaginatedHighlights.get_highlights ?limit ?cursor api () in
        Cmd.print_highlights fmt (Karakeep.PaginatedHighlights.T.highlights result)
      ) env)
  in
  let doc = "List all highlights." in
  let info = Cmdliner.Cmd.info "list" ~doc in
  Cmdliner.Cmd.v info
    Term.(const run $ Cmd.setup_logging $ Cmd.profile_arg $ Cmd.output_format_term
      $ Cmd.limit_term $ Cmd.cursor_term)

let get_highlight_cmd env =
  let run (style_renderer, level) profile fmt highlight_id =
    Cmd.setup_logging_simple style_renderer level;
    Error.wrap (fun () ->
      run_with_client ~profile (fun api ->
        let highlight = Karakeep.Highlight.get_highlights ~highlight_id api () in
        Cmd.print_highlight fmt highlight
      ) env)
  in
  let doc = "Get details of a specific highlight." in
  let info = Cmdliner.Cmd.info "get" ~doc in
  Cmdliner.Cmd.v info
    Term.(const run $ Cmd.setup_logging $ Cmd.profile_arg $ Cmd.output_format_term
      $ Cmd.highlight_id_term)

let bookmark_highlights_cmd env =
  let run (style_renderer, level) profile fmt bookmark_id =
    Cmd.setup_logging_simple style_renderer level;
    Error.wrap (fun () ->
      run_with_client ~profile (fun api ->
        let result = Karakeep.Client.get_bookmarks_highlights ~bookmark_id api () in
        match result with
        | Jsont.Object (members, _) ->
            (match List.find_map (fun ((k, _), v) -> if k = "highlights" then Some v else None) members with
             | Some (Jsont.Array (items, _)) ->
                 let highlights = List.filter_map (fun item ->
                   match Jsont_bytesrw.decode_string Karakeep.Highlight.T.jsont
                           (Jsont_bytesrw.encode_string Jsont.json item |> Result.get_ok) with
                   | Ok h -> Some h
                   | Error _ -> None
                 ) items in
                 Cmd.print_highlights fmt highlights
             | _ -> ())
        | _ -> ()
      ) env)
  in
  let doc = "List highlights for a bookmark." in
  let info = Cmdliner.Cmd.info "for-bookmark" ~doc in
  Cmdliner.Cmd.v info
    Term.(const run $ Cmd.setup_logging $ Cmd.profile_arg $ Cmd.output_format_term
      $ Cmd.bookmark_id_term)

let delete_highlight_cmd env =
  let run (style_renderer, level) profile highlight_id =
    Cmd.setup_logging_simple style_renderer level;
    Error.wrap (fun () ->
      run_with_client ~profile (fun api ->
        let _ = Karakeep.Highlight.delete_highlights ~highlight_id api () in
        Logs.app (fun m -> m "Deleted highlight %s" highlight_id)
      ) env)
  in
  let doc = "Delete a highlight." in
  let info = Cmdliner.Cmd.info "delete" ~doc in
  Cmdliner.Cmd.v info
    Term.(const run $ Cmd.setup_logging $ Cmd.profile_arg $ Cmd.highlight_id_term)

let highlights_cmd env =
  let doc = "Highlight operations." in
  let info = Cmdliner.Cmd.info "highlights" ~doc in
  Cmdliner.Cmd.group info
    [ list_highlights_cmd env
    ; get_highlight_cmd env
    ; bookmark_highlights_cmd env
    ; delete_highlight_cmd env
    ]

(* User commands *)

let whoami_cmd env =
  let run (style_renderer, level) profile fmt =
    Cmd.setup_logging_simple style_renderer level;
    Error.wrap (fun () ->
      run_with_client ~profile (fun api ->
        let user = Karakeep.Client.get_users_me api () in
        Cmd.print_user fmt user
      ) env)
  in
  let doc = "Show current user info." in
  let info = Cmdliner.Cmd.info "whoami" ~doc in
  Cmdliner.Cmd.v info
    Term.(const run $ Cmd.setup_logging $ Cmd.profile_arg $ Cmd.output_format_term)

let stats_cmd env =
  let run (style_renderer, level) profile fmt =
    Cmd.setup_logging_simple style_renderer level;
    Error.wrap (fun () ->
      run_with_client ~profile (fun api ->
        let stats = Karakeep.Client.get_users_me_stats api () in
        Cmd.print_stats fmt stats
      ) env)
  in
  let doc = "Show user statistics." in
  let info = Cmdliner.Cmd.info "stats" ~doc in
  Cmdliner.Cmd.v info
    Term.(const run $ Cmd.setup_logging $ Cmd.profile_arg $ Cmd.output_format_term)

(* Main command *)

let () =
  let exit_code =
    try
      Eio_main.run @@ fun env ->
      let doc = "Karakeep CLI - interact with a Karakeep bookmark service." in
      let man =
        [ `S Manpage.s_description
        ; `P "okarakeep is a command-line tool for interacting with a Karakeep \
              bookmark service instance."
        ; `S "AUTHENTICATION"
        ; `P "Use $(b,okarakeep auth login) to configure credentials interactively."
        ; `P "Credentials are stored in ~/.config/karakeep/profiles/<profile>/credentials.toml"
        ; `S Manpage.s_bugs
        ; `P "Report bugs at https://github.com/avsm/ocaml-karakeep/issues"
        ]
      in
      let info = Cmdliner.Cmd.info "okarakeep" ~version ~doc ~man in
      let cmds =
        [ Cmd.auth_cmd env
        ; bookmarks_cmd env
        ; tags_cmd env
        ; lists_cmd env
        ; highlights_cmd env
        ; whoami_cmd env
        ; stats_cmd env
        ]
      in
      Cmdliner.Cmd.eval ~catch:false (Cmdliner.Cmd.group info cmds)
    with
    | Eio.Cancel.Cancelled Stdlib.Exit -> 0
    | Error.Exit_code code -> code
    | Openapi.Runtime.Api_error _ as exn -> Error.handle_exn exn
    | Failure msg ->
        Fmt.epr "Error: %s@." msg;
        1
    | exn ->
        Fmt.epr "Unexpected error: %s@." (Printexc.to_string exn);
        125
  in
  exit exit_code
