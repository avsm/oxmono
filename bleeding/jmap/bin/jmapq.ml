(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Cmdliner
open Jmap_cli_util
module Proto = Jmap.Proto
module Chain = Jmap.Chain
module Cli = Jmap_eio.Cli

module Zulip_message = struct
  type t = {
    id : string;
    date : Ptime.t;
    thread_id : string;
    channel : string;
    topic : string;
    server : string;
    is_read : bool;
    labels : string list;
  }

  let is_seen keywords =
    List.exists (fun (k, v) -> v && Proto.Keyword.equal k `Seen) keywords

  let extract_labels keywords =
    keywords
    |> List.filter_map (fun (k, v) ->
        let name = Proto.Keyword.to_string k in
        if v && not (String.length name > 0 && name.[0] = '$') then Some name
        else if v && Proto.Keyword.equal k `Flagged then Some "flagged"
        else None)

  let of_email (email : Proto.Email.t) : t option =
    let id =
      match email.id with Some id -> Proto.Id.to_string id | None -> ""
    in
    let date =
      match email.received_at with Some t -> t | None -> Ptime.epoch
    in
    let thread_id =
      match email.thread_id with Some id -> Proto.Id.to_string id | None -> ""
    in
    let subject = Option.value ~default:"" email.subject in
    match Jmapq_subject.parse subject with
    | None -> None
    | Some (channel, topic, server) ->
        let keywords = Option.value ~default:[] email.keywords in
        let is_read = is_seen keywords in
        let labels = extract_labels keywords in
        Some { id; date; thread_id; channel; topic; server; is_read; labels }

  let jsont : t Jsont.t =
    let kind = "ZulipMessage" in
    let make id date thread_id channel topic server is_read labels =
      { id; date; thread_id; channel; topic; server; is_read; labels }
    in
    Jsont.Object.map ~kind make
    |> Jsont.Object.mem "id" Jsont.string ~enc:(fun t -> t.id)
    |> Jsont.Object.mem "date" Proto.Date.utc_jsont ~enc:(fun t -> t.date)
    |> Jsont.Object.mem "thread_id" Jsont.string ~enc:(fun t -> t.thread_id)
    |> Jsont.Object.mem "channel" Jsont.string ~enc:(fun t -> t.channel)
    |> Jsont.Object.mem "topic" Jsont.string ~enc:(fun t -> t.topic)
    |> Jsont.Object.mem "server" Jsont.string ~enc:(fun t -> t.server)
    |> Jsont.Object.mem "is_read" Jsont.bool ~enc:(fun t -> t.is_read)
    |> Jsont.Object.mem "labels" (Jsont.list Jsont.string) ~enc:(fun t ->
        t.labels)
    |> Jsont.Object.finish

  let list_jsont : t list Jsont.t = Jsont.list jsont
end

let zulip_processed_keyword = Proto.Keyword.of_string "zulip-processed"
let zulip_processed_name = Proto.Keyword.to_string zulip_processed_keyword
let zulip_sender = "noreply@zulip.com"
let zulip_properties = [ `Id; `Thread_id; `Keywords; `Received_at; `Subject ]

let fetch_zulip cfg client ~account_id ~has_keyword ~limit =
  let filter = Proto.Email.filter ~from:zulip_sender ?has_keyword () in
  let sort = [ Proto.Email.sort ~ascending:false `Received_at ] in
  let email_ids =
    query_ids client ?max:limit (fun ~position ~limit ->
        Chain.email_query ~account_id ~filter ~sort ~position ~limit ())
  in
  Cli.debug cfg "Found %d Zulip email IDs" (List.length email_ids);
  if email_ids = [] then None
  else begin
    let emails =
      get_emails client ~account_id ~properties:zulip_properties email_ids
    in
    let messages = List.filter_map Zulip_message.of_email emails in
    let unparsed = List.length emails - List.length messages in
    if unparsed > 0 then
      warn
        "%d of %d email(s) from %s did not carry a Zulip subject and were \
         skipped"
        unparsed (List.length emails) zulip_sender;
    Cli.debug cfg "Parsed %d Zulip messages from %d emails"
      (List.length messages) (List.length emails);
    Some messages
  end

(* [group_by key items] is [items] gathered under [key], the groups in key
   order and each group in the order the items came in. Hash order would make
   two runs over one mailbox print the same messages differently. *)
let group_by key items =
  let table = Hashtbl.create 8 in
  List.iter
    (fun item ->
      let k = key item in
      let existing = try Hashtbl.find table k with Not_found -> [] in
      Hashtbl.replace table k (item :: existing))
    items;
  Hashtbl.fold (fun k group acc -> (k, List.rev group) :: acc) table []
  |> List.sort (fun (a, _) (b, _) -> String.compare a b)

let print_zulip_messages title messages =
  listing_header title (Fmt.str "%d messages" (List.length messages));
  List.iter
    (fun (server, server_msgs) ->
      Fmt.pr "%a [%s]@,"
        Fmt.(styled `Bold string)
        "Server:" (terminal_text server);
      List.iter
        (fun (channel, channel_msgs) ->
          Fmt.pr "  %a #%s (%d)@,"
            Fmt.(styled `Cyan string)
            "Channel:" (terminal_text channel) (List.length channel_msgs);
          let sorted =
            List.sort
              (fun a b ->
                Ptime.compare b.Zulip_message.date a.Zulip_message.date)
              channel_msgs
          in
          List.iter
            (fun (msg : Zulip_message.t) ->
              let read_marker = if msg.is_read then " " else "*" in
              let labels_str =
                match msg.labels with
                | [] -> ""
                | ls ->
                    " [" ^ String.concat ", " (List.map terminal_text ls) ^ "]"
              in
              Fmt.pr "    %s %s  %a  %s%s@," read_marker
                (ptime_to_string msg.date)
                Fmt.(styled `Yellow string)
                (truncate_string 40 msg.topic)
                (truncate_string 12 msg.id)
                labels_str)
            sorted;
          Fmt.pr "@,")
        (group_by (fun (m : Zulip_message.t) -> m.channel) server_msgs))
    (group_by (fun (m : Zulip_message.t) -> m.server) messages);
  Fmt.pr "@]@."

let output_zulip_messages ~json_output ~title ~empty_message = function
  | None -> if json_output then Fmt.pr "[]@." else Fmt.pr "%s@." empty_message
  | Some messages ->
      if json_output then
        match
          Proto.Json.encode ~format:Jsont.Indent Zulip_message.list_jsont
            messages
        with
        | Ok json_str -> Fmt.pr "%s@." json_str
        | Error e ->
            die "the messages did not encode as JSON: %s"
              (Jsont.Error.to_string e)
      else print_zulip_messages title messages

let json_term =
  let doc = "Output as JSON" in
  Arg.(value & flag & info [ "json" ] ~doc)

let limit_term =
  let doc = "Maximum number of messages to fetch (default: all)" in
  Arg.(
    value & opt (some positive_int) None & info [ "limit"; "n" ] ~docv:"N" ~doc)

let zulip_list_cmd =
  let run cfg json_output limit =
    Eio_main.run @@ fun env ->
    Eio.Switch.run @@ fun sw ->
    let client = Cli.create_client ~sw env cfg in
    let account_id = resolve_account_id cfg client in

    Cli.debug cfg "Searching for Zulip notification emails";

    fetch_zulip cfg client ~account_id ~has_keyword:None ~limit
    |> output_zulip_messages ~json_output ~title:"Zulip Notifications"
         ~empty_message:"No Zulip notification emails found."
  in
  let doc = "List Zulip notification emails with parsed channel/topic info" in
  let man =
    [
      `S Manpage.s_description;
      `P
        "Lists all emails from noreply@zulip.com and parses the subject line \
         to extract the Zulip channel, topic, and server name.";
      `P "Subject format expected: \"#Channel > topic [Server Name]\"";
      `S Manpage.s_examples;
      `P "List all Zulip notifications:";
      `Pre "  jmapq zulip-list";
      `P "Output as JSON:";
      `Pre "  jmapq zulip-list --json";
      `P "Limit to 50 most recent:";
      `Pre "  jmapq zulip-list -n 50";
    ]
  in
  let info = Cmd.info "zulip-list" ~doc ~man in
  Cmd.v info Term.(const run $ Cli.config_term $ json_term $ limit_term)

let zulip_timeout_cmd =
  let email_ids_term =
    let doc = "Email IDs to mark as processed" in
    Arg.(non_empty & pos_all id [] & info [] ~docv:"EMAIL_ID" ~doc)
  in
  let email_ids_term =
    Term.(term_result' ~usage:true (const unique_ids $ email_ids_term))
  in
  let run cfg email_ids =
    Eio_main.run @@ fun env ->
    Eio.Switch.run @@ fun sw ->
    let client = Cli.create_client ~sw env cfg in
    let account_id = resolve_account_id cfg client in
    Cli.debug cfg "Marking %d email(s) with '%s' keyword"
      (List.length email_ids) zulip_processed_name;

    let patch =
      let open Proto.Email.Patch in
      Proto.Patch.v [ set_keyword zulip_processed_keyword; set_keyword `Seen ]
    in
    let report =
      run_set client cfg email_ids
        ~outcome:(fun (result : _ Proto.Method.set_response) ->
          ( result.updated |> Option.value ~default:[] |> List.map fst,
            Option.value ~default:[] result.not_updated ))
        (fun ~ids ->
          Chain.email_set ~account_id
            ~update:(List.map (fun id -> (id, patch)) ids)
            ())
    in
    report_set
      { succeeded = "Marked"; action = "mark"; activity = "Marking" }
      report
  in
  let doc = "Mark Zulip notification emails as processed" in
  let man =
    [
      `S Manpage.s_description;
      `P
        (Printf.sprintf
           "Adds the '%s' and '\\$seen' keywords to the specified email(s), \
            marking each notification processed and read. The custom keyword \
            can be used to filter processed notifications or set up \
            server-side rules to auto-archive them."
           zulip_processed_name);
      `S Manpage.s_examples;
      `P "Mark a single email as processed:";
      `Pre "  jmapq zulip-timeout StrrDTS_WEa3";
      `P "Mark multiple emails as processed:";
      `Pre "  jmapq zulip-timeout StrrDTS_WEa3 StrsGZ7P8Dpc StrsGuCSXJ3Z";
    ]
  in
  let info = Cmd.info "zulip-timeout" ~doc ~man in
  Cmd.v info Term.(const run $ Cli.config_term $ email_ids_term)

let zulip_view_cmd =
  let run cfg json_output limit =
    Eio_main.run @@ fun env ->
    Eio.Switch.run @@ fun sw ->
    let client = Cli.create_client ~sw env cfg in
    let account_id = resolve_account_id cfg client in

    Cli.debug cfg "Searching for Zulip emails marked as processed";

    fetch_zulip cfg client ~account_id
      ~has_keyword:(Some zulip_processed_keyword) ~limit
    |> output_zulip_messages ~json_output ~title:"Processed Zulip Notifications"
         ~empty_message:"No Zulip emails marked as processed."
  in
  let doc = "List Zulip emails that have been marked as processed" in
  let man =
    [
      `S Manpage.s_description;
      `P
        (Printf.sprintf
           "Lists all Zulip notification emails that have the '%s' keyword."
           zulip_processed_name);
      `S Manpage.s_examples;
      `P "List all processed Zulip notifications:";
      `Pre "  jmapq zulip-view";
      `P "Output as JSON:";
      `Pre "  jmapq zulip-view --json";
      `P "Limit to 50 most recent:";
      `Pre "  jmapq zulip-view -n 50";
    ]
  in
  let info = Cmd.info "zulip-view" ~doc ~man in
  Cmd.v info Term.(const run $ Cli.config_term $ json_term $ limit_term)

let main_cmd =
  let doc = "JMAPQ - Specialist JMAP workflow commands" in
  let man =
    [
      `S Manpage.s_description;
      `P
        "A collection of specialist workflow commands for JMAP email \
         processing.";
      `S Manpage.s_environment;
      `Pre Cli.env_docs;
    ]
  in
  let info = Cmd.info "jmapq" ~version:"0.1.0" ~doc ~man in
  Cmd.group info [ zulip_list_cmd; zulip_timeout_cmd; zulip_view_cmd ]

let () =
  Fmt_tty.setup_std_outputs ();
  exit (Cmd.eval main_cmd)
