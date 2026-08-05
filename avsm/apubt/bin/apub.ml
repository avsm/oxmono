(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** ActivityPub CLI tool *)

open Cmdliner

let app_name = "apub"

let setup_log style_renderer level =
  Fmt_tty.setup_std_outputs ?style_renderer ();
  Logs.set_level level;
  Logs.set_reporter (Logs_fmt.reporter ())

(* Common options *)
let setup_log_term =
  Term.(const setup_log $ Fmt_cli.style_renderer () $ Logs_cli.level ())

let timeout =
  let doc = "Request timeout in seconds." in
  Arg.(value & opt float 30.0 & info ["timeout"; "t"] ~docv:"SECONDS" ~doc)

let user_agent =
  let doc = "User-Agent header for HTTP requests." in
  Arg.(value & opt string "apub/0.1" & info ["user-agent"; "A"] ~docv:"STRING" ~doc)

(* Webfinger command *)
module Webfinger_cmd = struct
  let account =
    let doc = "Account to look up (e.g., user@example.com or acct:user@example.com)." in
    Arg.(required & pos 0 (some string) None & info [] ~docv:"ACCOUNT" ~doc)

  let json_output =
    let doc = "Output raw JSON response." in
    Arg.(value & flag & info ["json"; "j"] ~doc)

  let run () timeout user_agent json_output account =
    Eio_main.run @@ fun env ->
    Eio.Switch.run @@ fun sw ->
    let client = Apubt.create ~sw ~user_agent ~timeout env in
    try
      let jrd = Apubt.Webfinger.lookup client account in
      if json_output then begin
        match Jsont_bytesrw.encode_string Apubt.Proto.Webfinger.jsont jrd with
        | Ok s -> print_endline s
        | Error e -> Fmt.epr "JSON encoding error: %s@." e
      end else begin
        Fmt.pr "@[<v>";
        Fmt.pr "Subject: %s@," (Apubt.Proto.Webfinger.subject jrd);
        (match Apubt.Proto.Webfinger.aliases jrd with
         | Some aliases ->
             Fmt.pr "Aliases:@,";
             List.iter (fun a -> Fmt.pr "  - %s@," a) aliases
         | None -> ());
        (match Apubt.Proto.Webfinger.links jrd with
         | Some links ->
             Fmt.pr "Links:@,";
             List.iter (fun link ->
               let rel = Apubt.Proto.Webfinger.Jrd_link.rel link in
               let href = Apubt.Proto.Webfinger.Jrd_link.href link in
               let type_ = Apubt.Proto.Webfinger.Jrd_link.type_ link in
               Fmt.pr "  - rel: %s@," rel;
               Option.iter (fun t -> Fmt.pr "    type: %s@," t) type_;
               Option.iter (fun h -> Fmt.pr "    href: %s@," (Uri.to_string h)) href
             ) links
         | None -> ());
        (* Show extracted ActivityPub actor URI *)
        (match Apubt.Webfinger.actor_uri jrd with
         | Some uri ->
             Fmt.pr "@,ActivityPub Actor: %s@," (Uri.to_string uri)
         | None ->
             Fmt.pr "@,No ActivityPub actor link found.@,");
        Fmt.pr "@]"
      end;
      `Ok ()
    with
    | Apubt.E err ->
        Fmt.epr "Error: %a@." Apubt.Error.pp err;
        `Error (false, Apubt.Error.to_string err)

  let term =
    Term.(ret (const run $ setup_log_term $ timeout $ user_agent $ json_output $ account))

  let cmd =
    let doc = "Look up an account via Webfinger." in
    let man = [
      `S Manpage.s_description;
      `P "Performs a Webfinger lookup for the given account and displays \
          the JSON Resource Descriptor (JRD) response.";
      `P "The account can be specified as 'user@domain' or 'acct:user@domain'.";
      `S Manpage.s_examples;
      `Pre "  apub webfinger anil@recoil.org";
      `Pre "  apub webfinger --json acct:gargron@mastodon.social";
    ] in
    Cmd.v (Cmd.info "webfinger" ~doc ~man) term
end

(* Actor command *)
module Actor_cmd = struct
  let uri_or_acct =
    let doc = "Actor URI or account (user@domain) to fetch." in
    Arg.(required & pos 0 (some string) None & info [] ~docv:"URI|ACCOUNT" ~doc)

  let json_output =
    let doc = "Output raw JSON response." in
    Arg.(value & flag & info ["json"; "j"] ~doc)

  let run () timeout user_agent json_output uri_or_acct =
    Eio_main.run @@ fun env ->
    Eio.Switch.run @@ fun sw ->
    let client = Apubt.create ~sw ~user_agent ~timeout env in
    try
      let actor =
        if String.contains uri_or_acct '@' && not (String.starts_with ~prefix:"http" uri_or_acct) then
          Apubt.Actor.lookup client uri_or_acct
        else
          Apubt.Actor.fetch client (Uri.of_string uri_or_acct)
      in
      if json_output then begin
        match Jsont_bytesrw.encode_string Apubt.Proto.Actor.jsont actor with
        | Ok s -> print_endline s
        | Error e -> Fmt.epr "JSON encoding error: %s@." e
      end else begin
        Fmt.pr "@[<v>";
        Fmt.pr "ID: %s@," (Uri.to_string (Apubt.Proto.Actor.id actor));
        Fmt.pr "Type: %s@," (Apubt.Proto.Actor_type.to_string (Apubt.Proto.Actor.type_ actor));
        Option.iter (fun n -> Fmt.pr "Name: %s@," n) (Apubt.Proto.Actor.name actor);
        Option.iter (fun u -> Fmt.pr "Username: %s@," u) (Apubt.Proto.Actor.preferred_username actor);
        Option.iter (fun s -> Fmt.pr "Summary: %s@," s) (Apubt.Proto.Actor.summary actor);
        Option.iter (fun u -> Fmt.pr "URL: %s@," (Uri.to_string u)) (Apubt.Proto.Actor.url actor);
        Fmt.pr "Inbox: %s@," (Uri.to_string (Apubt.Proto.Actor.inbox actor));
        Fmt.pr "Outbox: %s@," (Uri.to_string (Apubt.Proto.Actor.outbox actor));
        Option.iter (fun u -> Fmt.pr "Followers: %s@," (Uri.to_string u)) (Apubt.Proto.Actor.followers actor);
        Option.iter (fun u -> Fmt.pr "Following: %s@," (Uri.to_string u)) (Apubt.Proto.Actor.following actor);
        Fmt.pr "@]"
      end;
      `Ok ()
    with
    | Apubt.E err ->
        Fmt.epr "Error: %a@." Apubt.Error.pp err;
        `Error (false, Apubt.Error.to_string err)

  let term =
    Term.(ret (const run $ setup_log_term $ timeout $ user_agent $ json_output $ uri_or_acct))

  let cmd =
    let doc = "Fetch an ActivityPub actor." in
    let man = [
      `S Manpage.s_description;
      `P "Fetches an ActivityPub actor by URI or performs a Webfinger lookup \
          and then fetches the actor.";
      `S Manpage.s_examples;
      `Pre "  apub actor anil@recoil.org";
      `Pre "  apub actor https://mastodon.social/users/Gargron";
      `Pre "  apub actor --json anil@recoil.org";
    ] in
    Cmd.v (Cmd.info "actor" ~doc ~man) term
end

(* Outbox command *)
module Outbox_cmd = struct
  let uri_or_acct =
    let doc = "Actor URI or account (user@domain) whose outbox to fetch." in
    Arg.(required & pos 0 (some string) None & info [] ~docv:"URI|ACCOUNT" ~doc)

  let limit =
    let doc = "Maximum number of activities to display." in
    Arg.(value & opt int 10 & info ["limit"; "n"] ~docv:"N" ~doc)

  let json_output =
    let doc = "Output raw JSON response." in
    Arg.(value & flag & info ["json"; "j"] ~doc)

  let run () timeout user_agent json_output limit uri_or_acct =
    Eio_main.run @@ fun env ->
    Eio.Switch.run @@ fun sw ->
    let client = Apubt.create ~sw ~user_agent ~timeout env in
    try
      let actor =
        if String.contains uri_or_acct '@' && not (String.starts_with ~prefix:"http" uri_or_acct) then
          Apubt.Actor.lookup client uri_or_acct
        else
          Apubt.Actor.fetch client (Uri.of_string uri_or_acct)
      in
      let outbox = Apubt.Actor.outbox client actor in
      if json_output then begin
        match Jsont_bytesrw.encode_string Apubt.Proto.Activity_collection.jsont outbox with
        | Ok s -> print_endline s
        | Error e -> Fmt.epr "JSON encoding error: %s@." e
      end else begin
        Fmt.pr "@[<v>";
        Fmt.pr "Outbox for: %s@," (Uri.to_string (Apubt.Proto.Actor.id actor));
        Option.iter (fun n -> Fmt.pr "Total items: %d@," n) (Apubt.Proto.Collection.total_items outbox);
        Fmt.pr "@,";
        (* Try to get items from collection or first page *)
        let items = match Apubt.Proto.Collection.items outbox with
          | Some items -> items
          | None ->
              (* Try first page *)
              (try
                 let page = Apubt.Actor.outbox_page client actor () in
                 Apubt.Proto.Collection_page.items page |> Option.value ~default:[]
               with Apubt.E e ->
                 Fmt.pr "(Error fetching first page: %a)@," Apubt.Error.pp e;
                 [])
        in
        let items = if List.length items > limit then
            List.filteri (fun i _ -> i < limit) items
          else items
        in
        List.iteri (fun i activity ->
          Fmt.pr "--- Activity %d ---@," (i + 1);
          Option.iter (fun id -> Fmt.pr "ID: %s@," (Uri.to_string id)) (Apubt.Proto.Activity.id activity);
          Fmt.pr "Type: %s@," (Apubt.Proto.Activity_type.to_string (Apubt.Proto.Activity.type_ activity));
          Option.iter (fun p -> Fmt.pr "Published: %s@," (Apubt.Proto.Datetime.to_string p)) (Apubt.Proto.Activity.published activity);
          Option.iter (fun s -> Fmt.pr "Summary: %s@," s) (Apubt.Proto.Activity.summary activity);
          (* Show object info if present *)
          (match Apubt.Proto.Activity.object_ activity with
           | Some (Apubt.Proto.Object_ref.Uri uri) ->
               Fmt.pr "Object: %s@," (Uri.to_string uri)
           | Some (Apubt.Proto.Object_ref.Object obj) ->
               Fmt.pr "Object type: %s@," (Apubt.Proto.Object_type.to_string (Apubt.Proto.Object.type_ obj));
               Option.iter (fun c ->
                 let c = if String.length c > 100 then String.sub c 0 100 ^ "..." else c in
                 Fmt.pr "Content: %s@," c
               ) (Apubt.Proto.Object.content obj)
           | None -> ());
          Fmt.pr "@,"
        ) items;
        if List.length items = 0 then
          Fmt.pr "(No activities found or outbox is empty)@,";
        Fmt.pr "@]"
      end;
      `Ok ()
    with
    | Apubt.E err ->
        Fmt.epr "Error: %a@." Apubt.Error.pp err;
        `Error (false, Apubt.Error.to_string err)

  let term =
    Term.(ret (const run $ setup_log_term $ timeout $ user_agent $ json_output $ limit $ uri_or_acct))

  let cmd =
    let doc = "Fetch an actor's outbox." in
    let man = [
      `S Manpage.s_description;
      `P "Fetches the outbox of an ActivityPub actor, displaying recent activities.";
      `S Manpage.s_examples;
      `Pre "  apub outbox anil@recoil.org";
      `Pre "  apub outbox --limit 5 https://mastodon.social/users/Gargron";
      `Pre "  apub outbox --json anil@recoil.org";
    ] in
    Cmd.v (Cmd.info "outbox" ~doc ~man) term
end

(* Common signing options for write operations *)
let key_file =
  let doc = "Path to PEM file containing the private key for signing (overrides saved session)." in
  Arg.(value & opt (some file) None & info ["key-file"; "k"] ~docv:"FILE" ~doc)

let key_id =
  let doc = "Key ID for signing (overrides saved session)." in
  Arg.(value & opt (some string) None & info ["key-id"; "K"] ~docv:"URI" ~doc)

let actor_uri =
  let doc = "Your actor URI (overrides saved session)." in
  Arg.(value & opt (some string) None & info ["actor"; "a"] ~docv:"URI" ~doc)

let profile_arg =
  let doc = "Profile to use for credentials (default: current profile)." in
  Arg.(value & opt (some string) None & info ["profile"; "P"] ~docv:"PROFILE" ~doc)

(* Auth mode - signature-based or OAuth-based *)
type auth_mode =
  | Signature_auth of Apubt.Signing.t
  | OAuth_auth of { instance : string; token : string }
  | No_auth

(* Result type for credential resolution *)
type credentials = {
  actor_uri : string;
  auth : auth_mode;
  session : Apub_auth_session.t option; [@warning "-69"]
}

(* Resolve credentials from CLI args or saved session *)
let resolve_credentials env ~key_file ~key_id ~actor_uri ~profile =
  (* If explicit key_file and key_id provided, use those *)
  match key_file, key_id, actor_uri with
  | Some kf, Some kid, Some actor ->
      let pem = In_channel.with_open_bin kf In_channel.input_all in
      let signing = Apubt.Signing.from_pem_exn ~key_id:kid ~pem () in
      Ok { actor_uri = actor; auth = Signature_auth signing; session = None }
  | None, None, None ->
      (* Try loading from session *)
      let fs = env#fs in
      (match Apub_auth_session.load fs ~app_name ?profile () with
       | Some session ->
           (* Prefer OAuth if available, otherwise use signature *)
           let auth = match session.oauth_access_token, session.oauth_instance with
             | Some token, Some instance ->
                 OAuth_auth { instance; token }
             | _ ->
                 (* Fall back to signature auth if available *)
                 (match session.key_id, session.private_key_pem with
                  | Some key_id, Some pem ->
                      let signing = Apubt.Signing.from_pem_exn ~key_id ~pem () in
                      Signature_auth signing
                  | _ -> No_auth)
           in
           Ok { actor_uri = session.actor_uri; auth; session = Some session }
       | None ->
           let profile_name = Option.value ~default:(Apub_auth_session.get_current_profile fs ~app_name) profile in
           Error (Printf.sprintf "No credentials found (profile: %s). Use 'apub auth setup' or 'apub auth login' first." profile_name))
  | _, _, Some actor ->
      (* Actor provided but no keys - try loading keys from session *)
      let fs = env#fs in
      (match Apub_auth_session.load fs ~app_name ?profile () with
       | Some session ->
           let auth = match session.key_id, session.private_key_pem with
             | Some key_id, Some pem ->
                 let signing = Apubt.Signing.from_pem_exn ~key_id ~pem () in
                 Signature_auth signing
             | _ -> No_auth
           in
           Ok { actor_uri = actor; auth; session = Some session }
       | None ->
           (* Just use the actor without signing *)
           Ok { actor_uri = actor; auth = No_auth; session = None })
  | _ ->
      Error "Incomplete credentials. Provide all of --actor, --key-file, --key-id, or use 'apub auth setup'."

(* Helper to create client with resolved credentials *)
let create_client_with_credentials ~sw ~user_agent ~timeout env creds =
  match creds.auth with
  | Signature_auth signing -> Apubt.create ~sw ~signing ~user_agent ~timeout env
  | OAuth_auth _ | No_auth -> Apubt.create ~sw ~user_agent ~timeout env

(* Post command - create a note *)
module Post_cmd = struct
  let content =
    let doc = "Content of the note to post (HTML allowed)." in
    Arg.(required & pos 0 (some string) None & info [] ~docv:"CONTENT" ~doc)

  let reply_to =
    let doc = "URI of the note to reply to." in
    Arg.(value & opt (some string) None & info ["reply-to"; "r"] ~docv:"URI" ~doc)

  let public =
    let doc = "Post publicly (default)." in
    Arg.(value & flag & info ["public"; "p"] ~doc)

  let followers_only =
    let doc = "Post to followers only." in
    Arg.(value & flag & info ["followers-only"; "f"] ~doc)

  let sensitive =
    let doc = "Mark as sensitive content." in
    Arg.(value & flag & info ["sensitive"; "s"] ~doc)

  let summary =
    let doc = "Content warning / summary text." in
    Arg.(value & opt (some string) None & info ["summary"; "w"] ~docv:"TEXT" ~doc)

  let run () timeout user_agent key_file key_id actor_uri profile content reply_to
      _public followers_only sensitive cw_summary =
    Eio_main.run @@ fun env ->
    match resolve_credentials env ~key_file ~key_id ~actor_uri ~profile with
    | Error msg ->
        Fmt.epr "Error: %s@." msg;
        `Error (false, msg)
    | Ok creds ->
        Eio.Switch.run @@ fun sw ->
        (* Use Mastodon API if OAuth is available *)
        match creds.auth with
        | OAuth_auth { instance; token } ->
            let timeout_config = Requests.Timeout.create ~connect:timeout ~read:timeout () in
            let requests = Requests.create ~sw ~timeout:timeout_config env in
            let visibility = if followers_only then Apub_mastodon_api.Private else Apub_mastodon_api.Public in
            let spoiler_text = if sensitive then cw_summary else None in
            (match Apub_mastodon_api.post_status requests ~instance ~token ~content
              ~visibility ?in_reply_to_id:reply_to ?sensitive:(if sensitive then Some true else None)
              ?spoiler_text () with
            | Ok status ->
                Fmt.pr "Posted: %s@." status.uri;
                Option.iter (fun url -> Fmt.pr "URL: %s@." url) status.url;
                `Ok ()
            | Error msg ->
                Fmt.epr "Error: %s@." msg;
                `Error (false, msg))
        | Signature_auth _ | No_auth ->
            (* Use ActivityPub federation with HTTP signatures *)
            let client = create_client_with_credentials ~sw ~user_agent ~timeout env creds in
            try
              let actor = Apubt.Actor.fetch client (Uri.of_string creds.actor_uri) in
              let in_reply_to = Option.map Uri.of_string reply_to in
              let _summary = if sensitive then cw_summary else None in
              let activity =
                if followers_only then
                  Apubt.Outbox.followers_only_note client ~actor ?in_reply_to ~content ()
                else
                  Apubt.Outbox.public_note client ~actor ?in_reply_to ~content ()
              in
              let activity_id = Option.get (Apubt.Proto.Activity.id activity) in
              Fmt.pr "Posted: %s@." (Uri.to_string activity_id);
              `Ok ()
            with
            | Apubt.E err ->
                Fmt.epr "Error: %a@." Apubt.Error.pp err;
                `Error (false, Apubt.Error.to_string err)

  let term =
    Term.(ret (const run $ setup_log_term $ timeout $ user_agent $ key_file
               $ key_id $ actor_uri $ profile_arg $ content $ reply_to $ public
               $ followers_only $ sensitive $ summary))

  let cmd =
    let doc = "Post a note." in
    let man = [
      `S Manpage.s_description;
      `P "Creates and posts a new note (status update).";
      `P "Uses saved credentials from 'apub auth setup', or override with --actor, --key-file, --key-id.";
      `S Manpage.s_examples;
      `Pre "  apub post \"Hello world!\"";
      `Pre "  apub post --reply-to https://other.com/notes/123 \"Nice post!\"";
      `Pre "  apub post --followers-only \"Followers only content\"";
      `Pre "  apub post --profile work \"Posting from work account\"";
    ] in
    Cmd.v (Cmd.info "post" ~doc ~man) term
end

(* Follow command *)
module Follow_cmd = struct
  let target =
    let doc = "Account to follow (user@domain or URI)." in
    Arg.(required & pos 0 (some string) None & info [] ~docv:"ACCOUNT" ~doc)

  let run () timeout user_agent key_file key_id actor_uri profile target =
    Eio_main.run @@ fun env ->
    match resolve_credentials env ~key_file ~key_id ~actor_uri ~profile with
    | Error msg ->
        Fmt.epr "Error: %s@." msg;
        `Error (false, msg)
    | Ok creds ->
        Eio.Switch.run @@ fun sw ->
        (* Use Mastodon API if OAuth is available *)
        match creds.auth with
        | OAuth_auth { instance; token } ->
            let timeout_config = Requests.Timeout.create ~connect:timeout ~read:timeout () in
            let requests = Requests.create ~sw ~timeout:timeout_config env in
            (* Look up the account first to get its ID *)
            (match Apub_mastodon_api.lookup_account requests ~instance ~token ~acct:target with
            | Ok account ->
                (match Apub_mastodon_api.follow requests ~instance ~token ~account_id:account.id with
                | Ok rel ->
                    Fmt.pr "Follow request sent to: %s@." account.acct;
                    if rel.following then Fmt.pr "Status: Now following@."
                    else if rel.requested then Fmt.pr "Status: Follow request pending@.";
                    `Ok ()
                | Error msg ->
                    Fmt.epr "Error: %s@." msg;
                    `Error (false, msg))
            | Error msg ->
                Fmt.epr "Error looking up account: %s@." msg;
                `Error (false, msg))
        | Signature_auth _ | No_auth ->
            (* Use ActivityPub federation with HTTP signatures *)
            let client = create_client_with_credentials ~sw ~user_agent ~timeout env creds in
            try
              let actor = Apubt.Actor.fetch client (Uri.of_string creds.actor_uri) in
              let target_actor =
                if String.contains target '@' && not (String.starts_with ~prefix:"http" target) then
                  Apubt.Actor.lookup client target
                else
                  Apubt.Actor.fetch client (Uri.of_string target)
              in
              let activity = Apubt.Actor.follow client ~actor ~target:target_actor in
              let activity_id = Option.get (Apubt.Proto.Activity.id activity) in
              Fmt.pr "Sent follow request: %s@." (Uri.to_string activity_id);
              Fmt.pr "Target: %s (%s)@."
                (Option.value ~default:"" (Apubt.Proto.Actor.preferred_username target_actor))
                (Uri.to_string (Apubt.Proto.Actor.id target_actor));
              `Ok ()
            with
            | Apubt.E err ->
                Fmt.epr "Error: %a@." Apubt.Error.pp err;
                `Error (false, Apubt.Error.to_string err)

  let term =
    Term.(ret (const run $ setup_log_term $ timeout $ user_agent $ key_file
               $ key_id $ actor_uri $ profile_arg $ target))

  let cmd =
    let doc = "Follow an actor." in
    let man = [
      `S Manpage.s_description;
      `P "Sends a Follow activity to another actor.";
      `P "Uses saved credentials from 'apub auth setup' or 'apub auth login'.";
      `S Manpage.s_examples;
      `Pre "  apub follow gargron@mastodon.social";
      `Pre "  apub follow https://mastodon.social/users/Gargron";
      `Pre "  apub follow --profile work colleague@example.com";
    ] in
    Cmd.v (Cmd.info "follow" ~doc ~man) term
end

(* Like command *)
module Like_cmd = struct
  let object_uri =
    let doc = "URI of the object to like." in
    Arg.(required & pos 0 (some string) None & info [] ~docv:"URI" ~doc)

  let run () timeout user_agent key_file key_id actor_uri profile object_uri =
    Eio_main.run @@ fun env ->
    match resolve_credentials env ~key_file ~key_id ~actor_uri ~profile with
    | Error msg ->
        Fmt.epr "Error: %s@." msg;
        `Error (false, msg)
    | Ok creds ->
        Eio.Switch.run @@ fun sw ->
        (* Use Mastodon API if OAuth is available *)
        match creds.auth with
        | OAuth_auth { instance; token } ->
            let timeout_config = Requests.Timeout.create ~connect:timeout ~read:timeout () in
            let requests = Requests.create ~sw ~timeout:timeout_config env in
            (* Extract status ID from URL *)
            (match Apub_mastodon_api.status_id_of_url object_uri with
            | Some status_id ->
                (match Apub_mastodon_api.favourite requests ~instance ~token ~status_id with
                | Ok status ->
                    Fmt.pr "Liked: %s@." status.uri;
                    `Ok ()
                | Error msg ->
                    Fmt.epr "Error: %s@." msg;
                    `Error (false, msg))
            | None ->
                Fmt.epr "Error: Could not extract status ID from URL: %s@." object_uri;
                `Error (false, "Invalid status URL"))
        | Signature_auth _ | No_auth ->
            (* Use ActivityPub federation with HTTP signatures *)
            let client = create_client_with_credentials ~sw ~user_agent ~timeout env creds in
            try
              let actor = Apubt.Actor.fetch client (Uri.of_string creds.actor_uri) in
              let activity = Apubt.Outbox.like client ~actor ~object_:(Uri.of_string object_uri) in
              let activity_id = Option.get (Apubt.Proto.Activity.id activity) in
              Fmt.pr "Liked: %s@." object_uri;
              Fmt.pr "Activity: %s@." (Uri.to_string activity_id);
              `Ok ()
            with
            | Apubt.E err ->
                Fmt.epr "Error: %a@." Apubt.Error.pp err;
                `Error (false, Apubt.Error.to_string err)

  let term =
    Term.(ret (const run $ setup_log_term $ timeout $ user_agent $ key_file
               $ key_id $ actor_uri $ profile_arg $ object_uri))

  let cmd =
    let doc = "Like an object." in
    let man = [
      `S Manpage.s_description;
      `P "Sends a Like activity for the specified object (note, article, etc).";
      `P "Uses saved credentials from 'apub auth setup' or 'apub auth login'.";
      `S Manpage.s_examples;
      `Pre "  apub like https://mastodon.social/notes/123";
      `Pre "  apub like --profile work https://example.com/notes/456";
    ] in
    Cmd.v (Cmd.info "like" ~doc ~man) term
end

(* Boost command (Announce) *)
module Boost_cmd = struct
  let object_uri =
    let doc = "URI of the object to boost." in
    Arg.(required & pos 0 (some string) None & info [] ~docv:"URI" ~doc)

  let run () timeout user_agent key_file key_id actor_uri profile object_uri =
    Eio_main.run @@ fun env ->
    match resolve_credentials env ~key_file ~key_id ~actor_uri ~profile with
    | Error msg ->
        Fmt.epr "Error: %s@." msg;
        `Error (false, msg)
    | Ok creds ->
        Eio.Switch.run @@ fun sw ->
        (* Use Mastodon API if OAuth is available *)
        match creds.auth with
        | OAuth_auth { instance; token } ->
            let timeout_config = Requests.Timeout.create ~connect:timeout ~read:timeout () in
            let requests = Requests.create ~sw ~timeout:timeout_config env in
            (* Extract status ID from URL *)
            (match Apub_mastodon_api.status_id_of_url object_uri with
            | Some status_id ->
                (match Apub_mastodon_api.reblog requests ~instance ~token ~status_id with
                | Ok status ->
                    Fmt.pr "Boosted: %s@." status.uri;
                    `Ok ()
                | Error msg ->
                    Fmt.epr "Error: %s@." msg;
                    `Error (false, msg))
            | None ->
                Fmt.epr "Error: Could not extract status ID from URL: %s@." object_uri;
                `Error (false, "Invalid status URL"))
        | Signature_auth _ | No_auth ->
            (* Use ActivityPub federation with HTTP signatures *)
            let client = create_client_with_credentials ~sw ~user_agent ~timeout env creds in
            try
              let actor = Apubt.Actor.fetch client (Uri.of_string creds.actor_uri) in
              let activity = Apubt.Outbox.announce client ~actor ~object_:(Uri.of_string object_uri) in
              let activity_id = Option.get (Apubt.Proto.Activity.id activity) in
              Fmt.pr "Boosted: %s@." object_uri;
              Fmt.pr "Activity: %s@." (Uri.to_string activity_id);
              `Ok ()
            with
            | Apubt.E err ->
                Fmt.epr "Error: %a@." Apubt.Error.pp err;
                `Error (false, Apubt.Error.to_string err)

  let term =
    Term.(ret (const run $ setup_log_term $ timeout $ user_agent $ key_file
               $ key_id $ actor_uri $ profile_arg $ object_uri))

  let cmd =
    let doc = "Boost (announce/reblog) an object." in
    let man = [
      `S Manpage.s_description;
      `P "Sends an Announce activity (boost/reblog) for the specified object.";
      `P "Uses saved credentials from 'apub auth setup' or 'apub auth login'.";
      `S Manpage.s_examples;
      `Pre "  apub boost https://mastodon.social/notes/123";
      `Pre "  apub boost --profile work https://example.com/notes/456";
    ] in
    Cmd.v (Cmd.info "boost" ~doc ~man) term
end

(* Main command group *)
let main_cmd =
  let doc = "ActivityPub command-line client" in
  let man = [
    `S Manpage.s_description;
    `P "apub is a command-line tool for interacting with ActivityPub servers.";
    `P "Use 'apub <command> --help' for more information on a specific command.";
    `P "There are two authentication methods:";
    `P "- OAuth login: 'apub auth login user@mastodon.social' (for Mastodon instances)";
    `P "- HTTP signatures: 'apub auth setup <actor-uri> -k <key.pem>' (for federation)";
    `S Manpage.s_commands;
    `S Manpage.s_examples;
    `Pre "  # Login to a Mastodon instance via OAuth";
    `Pre "  apub auth login alice@mastodon.social";
    `Pre "";
    `Pre "  # Or setup with PEM key for federation";
    `Pre "  apub auth setup https://example.com/users/alice -k ~/.config/apub/key.pem";
    `Pre "";
    `Pre "  # Then use commands without --actor/--key-file/--key-id";
    `Pre "  apub post \"Hello world!\"";
    `Pre "  apub follow gargron@mastodon.social";
    `Pre "  apub like https://mastodon.social/notes/123";
    `Pre "";
    `Pre "  # Read-only commands (no credentials needed)";
    `Pre "  apub webfinger anil@recoil.org";
    `Pre "  apub actor anil@recoil.org";
    `Pre "  apub outbox anil@recoil.org";
  ] in
  let info = Cmd.info "apub" ~version:"0.1" ~doc ~man in
  Cmd.group info [
    Apub_auth_cmd.auth_cmd ~app_name ();
    Webfinger_cmd.cmd;
    Actor_cmd.cmd;
    Outbox_cmd.cmd;
    Post_cmd.cmd;
    Follow_cmd.cmd;
    Like_cmd.cmd;
    Boost_cmd.cmd;
  ]

let () = exit (Cmd.eval main_cmd)
