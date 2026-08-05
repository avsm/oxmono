(*---------------------------------------------------------------------------
  Copyright (c) 2026 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

open Cmdliner

let setup_logging style_renderer level =
  Fmt_tty.setup_std_outputs ?style_renderer ();
  Logs.set_level level;
  Logs.set_reporter (Logs_fmt.reporter ())

let logging_t =
  Term.(const setup_logging $ Fmt_cli.style_renderer () $ Logs_cli.level ())

(* --- helpers --- *)

(** Load all feed entries for monitored contacts. Returns
    [(handle, contact_name, entry) list] sorted newest-first. *)
let load_entries env cfg =
  let fs = Eio.Stdenv.fs env in
  let xdg = Xdge.create fs "sortal" in
  let sortal = Sortal.create_from_xdg xdg in
  let feed_store = Sortal_feed.Store.create_from_xdg xdg in
  let contacts = Sortal.list sortal in
  let monitored = Tessabot.Config.contacts cfg contacts in
  let entries = List.concat_map (fun contact ->
    let handle = Sortal.Contact.handle contact in
    let names = Sortal.Contact.names contact in
    let name = match names with n :: _ -> n | [] -> handle in
    let feeds = Sortal.Contact.feeds contact |> Option.value ~default:[] in
    let entries = Sortal_feed.Store.all_entries feed_store ~handle feeds in
    List.map (fun entry -> (handle, name, entry)) entries
  ) monitored in
  (* Sort by date, newest first *)
  List.sort (fun (_, _, a) (_, _, b) ->
    Sortal_feed.Entry.compare_by_date b a
  ) entries

let pp_date ppf = function
  | None -> Fmt.pf ppf "          "
  | Some t ->
    let (y, m, d), _ = Ptime.to_date_time t in
    Fmt.pf ppf "%04d-%02d-%02d" y m d

(* --- feed list command --- *)

let feed_list_run () =
  let cfg = Tessabot.Config.load_or_fail () in
  Eio_main.run @@ fun env ->
  let entries = load_entries env cfg in
  if entries = [] then
    Fmt.pr "No feed entries found. Run 'sortal feed sync' first.@."
  else begin
    Fmt.pr "@[<v>%d entries:@,@," (List.length entries);
    List.iter (fun (handle, _name, (entry : Sortal_feed.Entry.t)) ->
      let title = Option.value ~default:"(no title)" entry.title in
      Fmt.pr "  %a  @%-8s  %-60s  %s@,"
        pp_date entry.date handle title entry.id
    ) entries;
    Fmt.pr "@]"
  end

let feed_list_cmd =
  let doc = "List feed entries for monitored contacts" in
  let info = Cmd.info "list" ~doc in
  Cmd.v info Term.(const feed_list_run $ logging_t)

(* --- feed post command --- *)

let feed_post_run () dry_run entry_id text_override link_override scheduling mode due_at image_urls =
  let cfg = Tessabot.Config.load_or_fail () in
  Eio_main.run @@ fun env ->
  (* Find the entry *)
  let entries = load_entries env cfg in
  let matching = List.filter (fun (_, _, (e : Sortal_feed.Entry.t)) ->
    e.id = entry_id
  ) entries in
  let (_handle, contact_name, entry) = match matching with
    | [x] -> x
    | [] ->
      Fmt.epr "Error: no entry found with id %S@." entry_id;
      exit 1
    | _ ->
      (* Take first match *)
      List.hd matching
  in
  (* Build post text — don't include URL in text since it goes in linkAttachment *)
  let text = match text_override with
    | Some t -> t
    | None ->
      let title = Option.value ~default:"New post" entry.title in
      Fmt.str "%s by %s" title contact_name
  in
  (* Link attachment: explicit --link overrides, otherwise use entry URL *)
  let link_attachment = match link_override with
    | Some _ -> link_override
    | None -> Option.map Uri.to_string entry.url
  in
  let scheduling_type = match scheduling with
    | `Automatic -> Tessabot.Buffer.Automatic
    | `Notification -> Tessabot.Buffer.Notification
  in
  let post_mode = match mode with
    | `Now -> Tessabot.Buffer.Share_now
    | `Custom -> Tessabot.Buffer.Custom_schedule
    | `Next -> Tessabot.Buffer.Share_next
  in
  if dry_run then begin
    (* Dry run: show what would be posted to all channels *)
    Fmt.pr "@[<v>--- DRY RUN ---@,@,";
    Fmt.pr "Entry: %s@," (Option.value ~default:"(no title)" entry.title);
    Fmt.pr "Post text: %s@,@," text;
    Fmt.pr "Would fetch default org and all channels, then post to each.@,@,";
    (* Show example GraphQL for one channel *)
    let example_input : Tessabot.Buffer.create_post_input = {
      text;
      channel_id = "<CHANNEL_ID>";
      service = "<SERVICE>";
      scheduling_type;
      mode = post_mode;
      due_at;
      image_urls;
      link_attachment;
    } in
    let query, variables = Tessabot.Buffer.create_post_graphql example_input in
    Fmt.pr "GraphQL Query:@,%s@,@," query;
    Fmt.pr "Variables (per channel):@,%s@,@," variables;
    Fmt.pr "--- END DRY RUN ---@]@."
  end else begin
    Eio.Switch.run @@ fun sw ->
    let session = Requests.create ~sw env in
    let session = Requests.set_auth session
      (Requests.Auth.bearer ~token:cfg.buffer.api_key) in
    (* Get default org *)
    let org_id = match Tessabot.Buffer.list_organizations ~session with
      | Ok (org :: _) -> org.id
      | Ok [] ->
        Fmt.epr "Error: no Buffer organizations found@.";
        exit 1
      | Error msg ->
        Fmt.epr "Error listing organizations: %s@." msg;
        exit 1
    in
    (* Get all channels *)
    let channels = match Tessabot.Buffer.list_channels ~session ~org_id with
      | Ok chs -> chs
      | Error msg ->
        Fmt.epr "Error listing channels: %s@." msg;
        exit 1
    in
    Fmt.pr "Posting to %d channel(s) in org %s:@." (List.length channels) org_id;
    List.iter (fun (ch : Tessabot.Buffer.channel) ->
      let input : Tessabot.Buffer.create_post_input = {
        text;
        channel_id = ch.id;
        service = ch.service;
        scheduling_type;
        mode = post_mode;
        due_at;
        image_urls;
        link_attachment;
      } in
      match Tessabot.Buffer.create_post ~session input with
      | Ok result ->
        Fmt.pr "  [%s/%s] posted: id=%s@." ch.service ch.name result.id
      | Error msg ->
        Fmt.epr "  [%s/%s] error: %s@." ch.service ch.name msg
    ) channels
  end

let dry_run_flag =
  let doc = "Show the GraphQL query without sending it" in
  Arg.(value & flag & info ["dry-run"; "n"] ~doc)

let entry_id_arg =
  let doc = "Feed entry ID (as shown by 'feed list')" in
  Arg.(required & pos 0 (some string) None & info [] ~docv:"ENTRY_ID" ~doc)

let text_override_arg =
  let doc = "Override the post text (default: entry title + author)" in
  Arg.(value & opt (some string) None & info ["text"; "t"] ~docv:"TEXT" ~doc)

let link_override_arg =
  let doc = "Override the link attachment URL (default: entry URL). \
             Sent as linkAttachment metadata for social card previews." in
  Arg.(value & opt (some string) None & info ["link"; "l"] ~docv:"URL" ~doc)

let scheduling_arg =
  let doc = "Scheduling type: automatic or notification" in
  let types = ["automatic", `Automatic; "notification", `Notification] in
  Arg.(value & opt (enum types) `Automatic & info ["scheduling"] ~doc)

let mode_arg =
  let doc = "Post mode: now, custom, or next" in
  let modes = ["now", `Now; "custom", `Custom; "next", `Next] in
  Arg.(value & opt (enum modes) `Now & info ["mode"] ~doc)

let ptime_conv =
  let parse s =
    match Ptime.of_rfc3339 s with
    | Ok (t, _, _) -> Ok t
    | Error _ ->
      (try
         let t = Ptime.of_date_time
           ((int_of_string (String.sub s 0 4),
             int_of_string (String.sub s 5 2),
             int_of_string (String.sub s 8 2)),
            ((0, 0, 0), 0)) in
         match t with
         | Some t -> Ok t
         | None -> Error (`Msg (Fmt.str "Invalid date/time: %s" s))
       with _ -> Error (`Msg (Fmt.str "Invalid date/time: %s" s)))
  in
  let pp = Ptime.pp_rfc3339 () in
  Arg.conv (parse, pp)

let due_at_arg =
  let doc = "When to publish (RFC 3339 timestamp). Required when mode is 'custom'." in
  Arg.(value & opt (some ptime_conv) None & info ["due-at"] ~docv:"TIMESTAMP" ~doc)

let image_urls_arg =
  let doc = "Image URL to attach to the post (can be repeated)" in
  Arg.(value & opt_all string [] & info ["image"] ~docv:"URL" ~doc)

let feed_post_cmd =
  let doc = "Post a feed entry to all Buffer channels" in
  let info = Cmd.info "post" ~doc in
  Cmd.v info Term.(const feed_post_run $ logging_t $ dry_run_flag
    $ entry_id_arg $ text_override_arg $ link_override_arg
    $ scheduling_arg $ mode_arg $ due_at_arg $ image_urls_arg)

(* --- feed group command --- *)

let feed_cmd =
  let doc = "Feed management commands" in
  let info = Cmd.info "feed" ~doc in
  Cmd.group info [feed_list_cmd; feed_post_cmd]

(* --- buffer orgs command --- *)

let buffer_orgs_run () =
  let cfg = Tessabot.Config.load_or_fail () in
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let session = Requests.create ~sw env in
  let session = Requests.set_auth session
    (Requests.Auth.bearer ~token:cfg.buffer.api_key) in
  match Tessabot.Buffer.list_organizations ~session with
  | Ok orgs ->
    Fmt.pr "@[<v>%d organization(s):@," (List.length orgs);
    List.iter (fun (org : Tessabot.Buffer.organization) ->
      Fmt.pr "  %s@," org.id
    ) orgs;
    Fmt.pr "@]"
  | Error msg ->
    Fmt.epr "Error: %s@." msg;
    exit 1

let buffer_orgs_cmd =
  let doc = "List Buffer organizations" in
  let info = Cmd.info "orgs" ~doc in
  Cmd.v info Term.(const buffer_orgs_run $ logging_t)

(* --- buffer channels command --- *)

let buffer_channels_run () org_id =
  let cfg = Tessabot.Config.load_or_fail () in
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let session = Requests.create ~sw env in
  let session = Requests.set_auth session
    (Requests.Auth.bearer ~token:cfg.buffer.api_key) in
  match Tessabot.Buffer.list_channels ~session ~org_id with
  | Ok channels ->
    Fmt.pr "@[<v>%d channel(s):@," (List.length channels);
    List.iter (fun (ch : Tessabot.Buffer.channel) ->
      Fmt.pr "  %-20s %-12s %s@," ch.id ch.service ch.name
    ) channels;
    Fmt.pr "@]"
  | Error msg ->
    Fmt.epr "Error: %s@." msg;
    exit 1

let buffer_org_id_arg =
  let doc = "Buffer organization ID" in
  Arg.(required & pos 0 (some string) None & info [] ~docv:"ORG_ID" ~doc)

let buffer_channels_cmd =
  let doc = "List channels in a Buffer organization" in
  let info = Cmd.info "channels" ~doc in
  Cmd.v info Term.(const buffer_channels_run $ logging_t $ buffer_org_id_arg)

(* --- buffer post command (direct, single channel) --- *)

let buffer_post_run () dry_run channel_id service text link_attachment scheduling mode due_at image_urls =
  let scheduling_type = match scheduling with
    | `Automatic -> Tessabot.Buffer.Automatic
    | `Notification -> Tessabot.Buffer.Notification
  in
  let post_mode = match mode with
    | `Now -> Tessabot.Buffer.Share_now
    | `Custom -> Tessabot.Buffer.Custom_schedule
    | `Next -> Tessabot.Buffer.Share_next
  in
  let input : Tessabot.Buffer.create_post_input = {
    text;
    channel_id;
    service;
    scheduling_type;
    mode = post_mode;
    due_at;
    image_urls;
    link_attachment;
  } in
  if dry_run then begin
    let query, variables = Tessabot.Buffer.create_post_graphql input in
    Fmt.pr "@[<v>--- DRY RUN ---@,@,";
    Fmt.pr "Endpoint: POST https://api.buffer.com@,@,";
    Fmt.pr "GraphQL Query:@,%s@,@," query;
    Fmt.pr "Variables:@,%s@,@," variables;
    Fmt.pr "--- END DRY RUN ---@]@."
  end else begin
    let cfg = Tessabot.Config.load_or_fail () in
    Eio_main.run @@ fun env ->
    Eio.Switch.run @@ fun sw ->
    let session = Requests.create ~sw env in
    let session = Requests.set_auth session
      (Requests.Auth.bearer ~token:cfg.buffer.api_key) in
    match Tessabot.Buffer.create_post ~session input with
    | Ok result ->
      Fmt.pr "Post created: id=%s@." result.id
    | Error msg ->
      Fmt.epr "Error: %s@." msg;
      exit 1
  end

let buffer_dry_run_flag =
  let doc = "Show the GraphQL query without sending it" in
  Arg.(value & flag & info ["dry-run"; "n"] ~doc)

let buffer_channel_id_arg =
  let doc = "Buffer channel ID to post to" in
  Arg.(required & opt (some string) None & info ["channel"; "c"] ~docv:"CHANNEL_ID" ~doc)

let buffer_text_arg =
  let doc = "Post text content" in
  Arg.(required & pos 0 (some string) None & info [] ~docv:"TEXT" ~doc)

let buffer_scheduling_arg =
  let doc = "Scheduling type: automatic or notification" in
  let types = ["automatic", `Automatic; "notification", `Notification] in
  Arg.(value & opt (enum types) `Automatic & info ["scheduling"] ~doc)

let buffer_mode_arg =
  let doc = "Post mode: now, custom, or next" in
  let modes = ["now", `Now; "custom", `Custom; "next", `Next] in
  Arg.(value & opt (enum modes) `Now & info ["mode"] ~doc)

let buffer_due_at_arg =
  let doc = "When to publish (RFC 3339 timestamp). Required when mode is 'custom'." in
  Arg.(value & opt (some ptime_conv) None & info ["due-at"] ~docv:"TIMESTAMP" ~doc)

let buffer_service_arg =
  let doc = "Channel service type (e.g. bluesky, linkedin, threads). \
             Required when using --link for social card metadata." in
  Arg.(value & opt string "" & info ["service"; "s"] ~docv:"SERVICE" ~doc)

let buffer_link_arg =
  let doc = "Link attachment URL for social card previews" in
  Arg.(value & opt (some string) None & info ["link"; "l"] ~docv:"URL" ~doc)

let buffer_image_urls_arg =
  let doc = "Image URL to attach to the post (can be repeated)" in
  Arg.(value & opt_all string [] & info ["image"] ~docv:"URL" ~doc)

let buffer_post_cmd =
  let doc = "Create a post on a specific Buffer channel" in
  let info = Cmd.info "post" ~doc in
  Cmd.v info Term.(const buffer_post_run $ logging_t $ buffer_dry_run_flag
    $ buffer_channel_id_arg $ buffer_service_arg $ buffer_text_arg
    $ buffer_link_arg $ buffer_scheduling_arg $ buffer_mode_arg
    $ buffer_due_at_arg $ buffer_image_urls_arg)

(* --- buffer group command --- *)

let buffer_cmd =
  let doc = "Buffer social media management commands" in
  let info = Cmd.info "buffer" ~doc in
  Cmd.group info [buffer_orgs_cmd; buffer_channels_cmd; buffer_post_cmd]

(* --- init command --- *)

let init_run () =
  let path = Tessabot.Config.config_file () in
  if Sys.file_exists path then
    Fmt.pr "Config already exists at %s@." path
  else begin
    let dir = Filename.dirname path in
    if not (Sys.file_exists dir) then
      Sys.mkdir dir 0o755;
    let oc = open_out path in
    output_string oc Tessabot.Config.sample_config;
    close_out oc;
    Fmt.pr "Config written to %s@." path;
    Fmt.pr "Edit it to add your Buffer API key.@."
  end

let init_cmd =
  let doc = "Create a default configuration file" in
  let info = Cmd.info "init" ~doc in
  Cmd.v info Term.(const init_run $ logging_t)

(* --- main --- *)

let main_cmd =
  let doc = "Social media automation from sortal feeds" in
  let info = Cmd.info "tessabot" ~version:"0.1.0" ~doc in
  Cmd.group info [feed_cmd; buffer_cmd; init_cmd]

let () = exit (Cmd.eval main_cmd)
