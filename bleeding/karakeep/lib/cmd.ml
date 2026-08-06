(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Cmdliner

(* {1 Styled output helpers} *)

let header_style = Fmt.(styled `Bold string)
let label_style = Fmt.(styled `Faint string)
let value_style = Fmt.(styled (`Fg `Cyan) string)
let success_style = Fmt.(styled (`Fg `Green) string)
let warning_style = Fmt.(styled (`Fg `Yellow) string)
let error_style = Fmt.(styled (`Fg `Red) string)
let profile_style = Fmt.(styled (`Fg `Magenta) string)
let current_style = Fmt.(styled (`Fg `Green) (styled `Bold string))

(* {1 Logging setup} *)

let setup_logging_simple style_renderer level =
  Fmt_tty.setup_std_outputs ?style_renderer ();
  Logs.set_level level;
  Logs.set_reporter (Logs_fmt.reporter ())

let setup_logging =
  Term.(const (fun style_renderer level -> (style_renderer, level))
  $ Fmt_cli.style_renderer ()
  $ Logs_cli.level ())

(* {1 Common arguments} *)

let profile_arg =
  let doc = "Profile name (default: current profile)." in
  Arg.(value & opt (some string) None & info [ "profile"; "P" ] ~docv:"PROFILE" ~doc)

let base_url_term =
  let doc = "Base URL of the Karakeep instance." in
  let env = Cmd.Env.info "KARAKEEP_BASE_URL" ~doc in
  Arg.(value & opt string Session.default_base_url & info [ "base-url"; "u" ] ~docv:"URL" ~doc ~env)

(* Pagination terms *)
let limit_term =
  let doc = "Maximum number of items to return." in
  Arg.(value & opt (some string) None & info [ "limit"; "n" ] ~docv:"N" ~doc)

let cursor_term =
  let doc = "Pagination cursor for fetching next page." in
  Arg.(value & opt (some string) None & info [ "cursor"; "c" ] ~docv:"CURSOR" ~doc)

(* Filter terms *)
let archived_term =
  let doc = "Filter for archived items." in
  let archived = (Some "true", Arg.info [ "archived" ] ~doc) in
  let not_archived =
    (Some "false", Arg.info [ "no-archived" ] ~doc:"Filter for non-archived items.")
  in
  Arg.(value & vflag None [ archived; not_archived ])

let favourited_term =
  let doc = "Filter for favourited items." in
  let fav = (Some "true", Arg.info [ "favourited"; "fav" ] ~doc) in
  let not_fav =
    (Some "false", Arg.info [ "no-favourited"; "no-fav" ] ~doc:"Filter for non-favourited items.")
  in
  Arg.(value & vflag None [ fav; not_fav ])

let include_content_term =
  let doc = "Include full content in response." in
  let include_it = (Some "true", Arg.info [ "include-content" ] ~doc) in
  let exclude_it = (Some "false", Arg.info [ "no-content" ] ~doc:"Exclude content from response.") in
  Arg.(value & vflag None [ include_it; exclude_it ])

(* Entity ID terms *)
let bookmark_id_term =
  let doc = "Bookmark ID." in
  Arg.(required & pos 0 (some string) None & info [] ~docv:"BOOKMARK_ID" ~doc)

let tag_id_term =
  let doc = "Tag ID." in
  Arg.(required & pos 0 (some string) None & info [] ~docv:"TAG_ID" ~doc)

let list_id_term =
  let doc = "List ID." in
  Arg.(required & pos 0 (some string) None & info [] ~docv:"LIST_ID" ~doc)

let highlight_id_term =
  let doc = "Highlight ID." in
  Arg.(required & pos 0 (some string) None & info [] ~docv:"HIGHLIGHT_ID" ~doc)

(* Bookmark terms *)
let url_term =
  let doc = "URL to bookmark." in
  Arg.(required & pos 0 (some string) None & info [] ~docv:"URL" ~doc)

let title_term =
  let doc = "Title for the bookmark." in
  Arg.(value & opt (some string) None & info [ "title"; "t" ] ~docv:"TITLE" ~doc)

let note_term =
  let doc = "Note to attach to the bookmark." in
  Arg.(value & opt (some string) None & info [ "note" ] ~docv:"NOTE" ~doc)

let summary_term =
  let doc = "Summary text for the bookmark." in
  Arg.(value & opt (some string) None & info [ "summary" ] ~docv:"TEXT" ~doc)

let tags_term =
  let doc = "Tag to attach (can be repeated)." in
  Arg.(value & opt_all string [] & info [ "tag" ] ~docv:"TAG" ~doc)

(* List terms *)
let name_term =
  let doc = "Name for the list." in
  Arg.(required & opt (some string) None & info [ "name" ] ~docv:"NAME" ~doc)

let name_opt_term =
  let doc = "Name for the list." in
  Arg.(value & opt (some string) None & info [ "name" ] ~docv:"NAME" ~doc)

let icon_term =
  let doc = "Icon for the list (emoji or identifier)." in
  Arg.(required & opt (some string) None & info [ "icon" ] ~docv:"ICON" ~doc)

let icon_opt_term =
  let doc = "Icon for the list (emoji or identifier)." in
  Arg.(value & opt (some string) None & info [ "icon" ] ~docv:"ICON" ~doc)

let description_term =
  let doc = "Description for the list." in
  Arg.(value & opt (some string) None & info [ "description"; "d" ] ~docv:"TEXT" ~doc)

let parent_id_term =
  let doc = "Parent list ID for nesting." in
  Arg.(value & opt (some string) None & info [ "parent-id" ] ~docv:"ID" ~doc)

let query_term =
  let doc = "Query for smart list." in
  Arg.(value & opt (some string) None & info [ "query"; "q" ] ~docv:"QUERY" ~doc)

let search_query_term =
  let doc = "Search query." in
  Arg.(required & pos 0 (some string) None & info [] ~docv:"QUERY" ~doc)

(* Highlight terms *)
let color_term =
  let doc = "Highlight color (yellow, red, green, blue)." in
  Arg.(value & opt (some string) None & info [ "color" ] ~docv:"COLOR" ~doc)

(* Output format *)
type output_format = Text | Json | Quiet

let output_format_term =
  let json = (Json, Arg.info [ "json"; "J" ] ~doc:"Output in JSON format.") in
  let ids_only = (Quiet, Arg.info [ "ids-only" ] ~doc:"Output only IDs (one per line).") in
  Arg.(value & vflag Text [ json; ids_only ])

(* {1 JSON helpers} *)

let encode_json codec v =
  match Jsont_bytesrw.encode_string codec v with
  | Ok s -> s
  | Error e -> failwith ("JSON encoding error: " ^ e)

let print_json_array to_json items =
  print_string "[";
  List.iteri (fun i item ->
    if i > 0 then print_string ",";
    print_string (to_json item)) items;
  print_endline "]"

(* {1 Helper to extract title from bookmark content JSON} *)

let title_from_content_json (content : Jsont.json) =
  match content with
  | Jsont.Object (members, _) ->
      let find key =
        List.find_map (fun ((k, _), v) ->
          if k = key then Some v else None
        ) members
      in
      (match find "title" with
       | Some (Jsont.String (s, _)) when s <> "" -> s
       | _ ->
           match find "url" with
           | Some (Jsont.String (s, _)) -> s
           | _ ->
               match find "text" with
               | Some (Jsont.String (s, _)) ->
                   if String.length s > 50 then String.sub s 0 50 ^ "..." else s
               | _ ->
                   match find "fileName" with
                   | Some (Jsont.String (s, _)) -> s
                   | _ -> "(no title)")
  | _ -> "(no title)"

let bookmark_title (b : Karakeep.Bookmark.T.t) =
  match Karakeep.Bookmark.T.title b with
  | Some t -> t
  | None -> title_from_content_json (Karakeep.Bookmark.T.content b)

(* {1 Output helpers} *)

let print_bookmark fmt (b : Karakeep.Bookmark.T.t) =
  match fmt with
  | Text ->
      let title = bookmark_title b in
      let status =
        (if Karakeep.Bookmark.T.archived b then "[A]" else "")
        ^ (if Karakeep.Bookmark.T.favourited b then "[*]" else "")
      in
      Printf.printf "%s  %s %s\n" (Karakeep.Bookmark.T.id b) title status
  | Json -> print_endline (encode_json Karakeep.Bookmark.T.jsont b)
  | Quiet -> print_endline (Karakeep.Bookmark.T.id b)

let print_bookmarks fmt bookmarks =
  match fmt with
  | Json -> print_json_array (encode_json Karakeep.Bookmark.T.jsont) bookmarks
  | _ -> List.iter (print_bookmark fmt) bookmarks

let print_tag fmt (t : Karakeep.Tag.T.t) =
  match fmt with
  | Text -> Printf.printf "%s  %s (%g)\n" (Karakeep.Tag.T.id t) (Karakeep.Tag.T.name t) (Karakeep.Tag.T.num_bookmarks t)
  | Json -> print_endline (encode_json Karakeep.Tag.T.jsont t)
  | Quiet -> print_endline (Karakeep.Tag.T.id t)

let print_tags fmt tags =
  match fmt with
  | Json -> print_json_array (encode_json Karakeep.Tag.T.jsont) tags
  | _ -> List.iter (print_tag fmt) tags

let print_list fmt (l : Karakeep.List.T.t) =
  match fmt with
  | Text ->
      let type_str =
        match Karakeep.List.T.type_ l with
        | "smart" -> "[smart]"
        | _ -> ""
      in
      Printf.printf "%s  %s %s %s\n" (Karakeep.List.T.id l) (Karakeep.List.T.icon l) (Karakeep.List.T.name l) type_str
  | Json -> print_endline (encode_json Karakeep.List.T.jsont l)
  | Quiet -> print_endline (Karakeep.List.T.id l)

let print_lists fmt lists =
  match fmt with
  | Json -> print_json_array (encode_json Karakeep.List.T.jsont) lists
  | _ -> List.iter (print_list fmt) lists

let print_highlight fmt (h : Karakeep.Highlight.T.t) =
  match fmt with
  | Text ->
      let text = Option.value ~default:"" (Karakeep.Highlight.T.text h) in
      let note = Option.value ~default:"" (Karakeep.Highlight.T.note h) in
      Printf.printf "%s  \"%s\" %s\n" (Karakeep.Highlight.T.id h) text note
  | Json -> print_endline (encode_json Karakeep.Highlight.T.jsont h)
  | Quiet -> print_endline (Karakeep.Highlight.T.id h)

let print_highlights fmt highlights =
  match fmt with
  | Json -> print_json_array (encode_json Karakeep.Highlight.T.jsont) highlights
  | _ -> List.iter (print_highlight fmt) highlights

let print_user fmt (u : Jsont.json) =
  match fmt with
  | Text ->
      let find key = match u with
        | Jsont.Object (members, _) ->
            List.find_map (fun ((k, _), v) -> if k = key then Some v else None) members
        | _ -> None
      in
      let str key = match find key with
        | Some (Jsont.String (s, _)) -> s
        | _ -> "(unknown)"
      in
      Printf.printf "User: %s\n" (str "name");
      Printf.printf "Email: %s\n" (str "email");
      Printf.printf "ID: %s\n" (str "id")
  | Json -> print_endline (encode_json Jsont.json u)
  | Quiet ->
      (match u with
       | Jsont.Object (members, _) ->
           (match List.find_map (fun ((k, _), v) -> if k = "id" then Some v else None) members with
            | Some (Jsont.String (s, _)) -> print_endline s
            | _ -> ())
       | _ -> ())

let print_stats fmt (s : Jsont.json) =
  match fmt with
  | Text ->
      let find key = match s with
        | Jsont.Object (members, _) ->
            List.find_map (fun ((k, _), v) -> if k = key then Some v else None) members
        | _ -> None
      in
      let num key = match find key with
        | Some (Jsont.Number (f, _)) -> int_of_float f
        | _ -> 0
      in
      Printf.printf "Bookmarks: %d\n" (num "numBookmarks");
      Printf.printf "Favorites: %d\n" (num "numFavourites");
      Printf.printf "Archived: %d\n" (num "numArchived");
      Printf.printf "Tags: %d\n" (num "numTags");
      Printf.printf "Lists: %d\n" (num "numLists");
      Printf.printf "Highlights: %d\n" (num "numHighlights")
  | Json -> print_endline (encode_json Jsont.json s)
  | Quiet ->
      let find key = match s with
        | Jsont.Object (members, _) ->
            List.find_map (fun ((k, _), v) -> if k = key then Some v else None) members
        | _ -> None
      in
      let num key = match find key with
        | Some (Jsont.Number (f, _)) -> int_of_float f
        | _ -> 0
      in
      Printf.printf "%d %d %d %d %d %d\n"
        (num "numBookmarks") (num "numFavourites") (num "numArchived")
        (num "numTags") (num "numLists") (num "numHighlights")

(* {1 Session helpers} *)

let with_session ?profile f env =
  let fs = env#fs in
  match Session.load fs ?profile () with
  | None ->
      let profile_name = match profile with
        | Some p -> p
        | None -> Session.get_current_profile fs
      in
      Fmt.epr "%a Not logged in (profile: %a). Use '%a' first.@."
        error_style "Error:"
        profile_style profile_name
        Fmt.(styled `Bold string) "okarakeep auth login";
      raise (Error.Exit_code 1)
  | Some session -> f fs session

let with_client ?profile f env =
  with_session ?profile (fun _fs session ->
    Eio.Switch.run @@ fun sw ->
    let client = Client.resume ~sw ~env ?profile ~session () in
    f client
  ) env

(* {1 Auth commands} *)

let profile_name_arg =
  let doc = "Profile name." in
  Arg.(required & pos 0 (some string) None & info [] ~docv:"PROFILE" ~doc)

let login_action ~base_url ~profile env =
  Fmt.pr "API Key: @?";
  let api_key = read_line () |> String.trim in
  if api_key = "" then begin
    Fmt.epr "%a API key cannot be empty.@." error_style "Error:";
    raise (Error.Exit_code 1)
  end;
  Eio.Switch.run @@ fun sw ->
  let _client = Client.login ~sw ~env ?profile ~base_url ~api_key () in
  let profile_name = Option.value ~default:Session.default_profile profile in
  Fmt.pr "%a Logged in to %a (profile: %a)@."
    success_style "Success:" value_style base_url profile_style profile_name

let login_cmd env =
  let doc = "Login to a Karakeep instance." in
  let info = Cmd.info "login" ~doc in
  let login' (style_renderer, level) base_url profile =
    setup_logging_simple style_renderer level;
    Error.wrap (fun () -> login_action ~base_url ~profile env)
  in
  Cmd.v info Term.(const login' $ setup_logging $ base_url_term $ profile_arg)

let logout_action ~profile env =
  let fs = env#fs in
  match Session.load fs ?profile () with
  | None -> Fmt.pr "%a Not logged in.@." warning_style "Note:"
  | Some session ->
      Session.clear fs ?profile ();
      let profile_name = match profile with
        | Some p -> p
        | None -> Session.get_current_profile fs
      in
      Fmt.pr "%a Logged out from %a (profile: %a).@."
        success_style "Success:"
        value_style (Session.base_url session)
        profile_style profile_name

let logout_cmd env =
  let doc = "Logout and clear saved session." in
  let info = Cmd.info "logout" ~doc in
  let logout' (style_renderer, level) profile =
    setup_logging_simple style_renderer level;
    logout_action ~profile env
  in
  Cmd.v info Term.(const logout' $ setup_logging $ profile_arg)

let status_action ~profile env =
  let fs = env#fs in
  let home = Sys.getenv "HOME" in
  Fmt.pr "%a %a@." label_style "Config directory:" value_style (home ^ "/.config/karakeep");
  let current = Session.get_current_profile fs in
  Fmt.pr "%a %a@." label_style "Current profile:" current_style current;
  let profiles = Session.list_profiles fs in
  if profiles <> [] then
    Fmt.pr "%a %a@." label_style "Available profiles:"
      Fmt.(list ~sep:(any ", ") profile_style) profiles;
  Fmt.pr "@.";
  let profile_name = Option.value ~default:current profile in
  match Session.load fs ~profile:profile_name () with
  | None ->
      Fmt.pr "%a %a: %a@."
        header_style "Profile"
        profile_style profile_name
        warning_style "Not logged in"
  | Some session ->
      Fmt.pr "%a %a:@." header_style "Profile" profile_style profile_name;
      Fmt.pr "  %a@." Session.pp session

let auth_status_cmd env =
  let doc = "Show authentication status." in
  let info = Cmd.info "status" ~doc in
  let status' (style_renderer, level) profile =
    setup_logging_simple style_renderer level;
    status_action ~profile env
  in
  Cmd.v info Term.(const status' $ setup_logging $ profile_arg)

let profile_list_action env =
  let fs = env#fs in
  let current = Session.get_current_profile fs in
  let profiles = Session.list_profiles fs in
  if profiles = [] then
    Fmt.pr "%a No profiles found. Use '%a' to create one.@."
      warning_style "Note:"
      Fmt.(styled `Bold string) "okarakeep auth login"
  else begin
    Fmt.pr "%a@." header_style "Profiles:";
    List.iter (fun prof ->
      let is_current = prof = current in
      match Session.load fs ~profile:prof () with
      | Some session ->
          if is_current then
            Fmt.pr "  %a %a - %a@."
              current_style prof success_style "(current)" value_style (Session.base_url session)
          else
            Fmt.pr "  %a - %a@." profile_style prof value_style (Session.base_url session)
      | None ->
          if is_current then
            Fmt.pr "  %a %a@." current_style prof success_style "(current)"
          else
            Fmt.pr "  %a@." profile_style prof
    ) profiles
  end

let profile_list_cmd env =
  let doc = "List available profiles." in
  let info = Cmd.info "list" ~doc in
  let list' (style_renderer, level) () =
    setup_logging_simple style_renderer level;
    profile_list_action env
  in
  Cmd.v info Term.(const list' $ setup_logging $ const ())

let profile_switch_action ~profile env =
  let fs = env#fs in
  let profiles = Session.list_profiles fs in
  if List.mem profile profiles then begin
    Session.set_current_profile fs profile;
    Fmt.pr "%a Switched to profile: %a@."
      success_style "Success:" profile_style profile
  end else begin
    Fmt.epr "%a Profile '%a' not found.@."
      error_style "Error:" profile_style profile;
    if profiles <> [] then
      Fmt.epr "%a %a@." label_style "Available profiles:"
        Fmt.(list ~sep:(any ", ") profile_style) profiles;
    raise (Error.Exit_code 1)
  end

let profile_switch_cmd env =
  let doc = "Switch to a different profile." in
  let info = Cmd.info "switch" ~doc in
  let switch' (style_renderer, level) profile =
    setup_logging_simple style_renderer level;
    profile_switch_action ~profile env
  in
  Cmd.v info Term.(const switch' $ setup_logging $ profile_name_arg)

let profile_current_action env =
  let fs = env#fs in
  let current = Session.get_current_profile fs in
  Fmt.pr "%a@." current_style current

let profile_current_cmd env =
  let doc = "Show current profile name." in
  let info = Cmd.info "current" ~doc in
  let current' (style_renderer, level) () =
    setup_logging_simple style_renderer level;
    profile_current_action env
  in
  Cmd.v info Term.(const current' $ setup_logging $ const ())

let profile_cmd env =
  let doc = "Profile management commands." in
  let info = Cmd.info "profile" ~doc in
  Cmd.group info
    [ profile_list_cmd env
    ; profile_switch_cmd env
    ; profile_current_cmd env
    ]

let auth_cmd env =
  let doc = "Authentication commands." in
  let info = Cmd.info "auth" ~doc in
  Cmd.group info
    [ login_cmd env
    ; logout_cmd env
    ; auth_status_cmd env
    ; profile_cmd env
    ]
