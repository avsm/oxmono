(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Cmdliner
open Jmap_cli_util
module Proto = Jmap.Proto
module Chain = Jmap.Chain
module Client = Jmap_eio.Client
module Cli = Jmap_eio.Cli
module Sync = Jmap_eio.Sync

let submission_capabilities =
  [ Proto.Capability.core; Proto.Capability.submission ]

(* RFC 9610 lives behind its own capability, and a server that does not
   implement it fails the whole request with unknownCapability, so a contacts
   command names it rather than widening the default. *)
let contacts_capabilities = [ Proto.Capability.core; Proto.Capability.contacts ]

let format_email_address (addr : Proto.Email_address.t) =
  match addr.name with
  | Some name ->
      Printf.sprintf "%s <%s>" (terminal_text name) (terminal_text addr.email)
  | None -> terminal_text addr.email

let format_email_addresses addrs =
  String.concat ", " (List.map format_email_address addrs)

let format_keywords keywords =
  keywords
  |> List.filter_map (fun (k, v) ->
      if v then Some (terminal_text (Proto.Keyword.to_string k)) else None)
  |> String.concat " "

let email_id (e : Proto.Email.t) =
  match e.id with Some id -> Proto.Id.to_string id | None -> "?"

let email_received_at (e : Proto.Email.t) =
  match e.received_at with Some t -> ptime_to_string t | None -> "?"

let email_keywords (e : Proto.Email.t) = Option.value ~default:[] e.keywords

let email_preview (e : Proto.Email.t) =
  terminal_text (Option.value ~default:"" e.preview)

let email_subject (e : Proto.Email.t) =
  terminal_text (Option.value e.subject ~default:"(no subject)")

let email_thread_id (e : Proto.Email.t) =
  match e.thread_id with Some id -> Proto.Id.to_string id | None -> "?"

let email_size (e : Proto.Email.t) = Option.value ~default:0L e.size
let received_at_first = [ Proto.Email.sort ~ascending:false `Received_at ]

(* {1 Contacts} *)

(* RFC 9553 Section 2.2.1: a Name has a full form, a list of components, or
   both. Joining the components skips the separators, whose value is the
   punctuation between the others. *)
let card_display_name (card : Jscontact.Card.t) =
  match card.name with
  | Some { full = Some full; _ } -> terminal_text full
  | Some { components = Some parts; _ } ->
      let word (c : Jscontact.Name.Component.t) =
        match c.kind with
        | `Separator -> None
        | _ -> Some (terminal_text c.value)
      in
      String.concat " " (List.filter_map word parts)
  | Some _ | None -> "(unnamed)"

let card_emails (card : Jscontact.Card.t) =
  List.map
    (fun (_, (e : Jscontact.Contact.Email_address.t)) ->
      terminal_text e.address)
    (Option.value card.emails ~default:[])

let card_phones (card : Jscontact.Card.t) =
  List.map
    (fun (_, (p : Jscontact.Contact.Phone.t)) -> terminal_text p.number)
    (Option.value card.phones ~default:[])

let card_organizations (card : Jscontact.Card.t) =
  List.filter_map
    (fun (_, (o : Jscontact.Org.Organization.t)) ->
      Option.map terminal_text o.name)
    (Option.value card.organizations ~default:[])

let addressbooks_cmd =
  let run cfg =
    Eio_main.run @@ fun env ->
    Eio.Switch.run @@ fun sw ->
    let client = Cli.create_client ~sw env cfg in
    let account_id =
      resolve_account_id ~capability:Proto.Capability.contacts cfg client
    in
    Cli.debug cfg "Fetching address books for account %s"
      (Proto.Id.to_string account_id);
    let result =
      call client ~capabilities:contacts_capabilities
        (Chain.address_book_get ~account_id ())
    in
    listing_header "Address books"
      (Fmt.str "%d, state %s" (List.length result.list)
         (terminal_text result.state));
    let sorted =
      List.sort
        (fun (a : Proto.Address_book.t) (b : Proto.Address_book.t) ->
          let order x = Option.value ~default:0L x in
          let cmp = Int64.compare (order a.sort_order) (order b.sort_order) in
          if cmp <> 0 then cmp
          else
            String.compare
              (Option.value ~default:"" a.name)
              (Option.value ~default:"" b.name))
        result.list
    in
    List.iter
      (fun (book : Proto.Address_book.t) ->
        let id_str =
          match book.id with Some id -> Proto.Id.to_string id | None -> "?"
        in
        let name =
          terminal_text (Option.value ~default:"(unnamed)" book.name)
        in
        let default =
          if book.is_default = Some true then " [default]" else ""
        in
        let rights =
          match book.my_rights with
          | Some r when not r.may_write -> " (read only)"
          | _ -> ""
        in
        Fmt.pr "  %a %a%s%s@,"
          Fmt.(styled `Cyan string)
          id_str
          Fmt.(styled `White string)
          name default rights;
        match book.description with
        | Some d -> Fmt.pr "    %s@," (truncate_string 70 (terminal_text d))
        | None -> ())
      sorted;
    Fmt.pr "@]@."
  in
  let doc = "List address books" in
  let info = Cmd.info "addressbooks" ~doc in
  Cmd.v info Term.(const run $ Cli.config_term)

let contacts_cmd =
  let limit_term =
    let doc = "Maximum number of cards to list" in
    Arg.(value & opt positive_int 20 & info [ "limit"; "n" ] ~docv:"N" ~doc)
  in
  let book_term =
    let doc = "Address book ID to filter by" in
    Arg.(value & opt (some id) None & info [ "book"; "b" ] ~docv:"ID" ~doc)
  in
  let search_term =
    let doc = "Only cards matching this text" in
    Arg.(
      value & opt (some string) None & info [ "search"; "s" ] ~docv:"TEXT" ~doc)
  in
  let run cfg limit book text =
    Eio_main.run @@ fun env ->
    Eio.Switch.run @@ fun sw ->
    let client = Cli.create_client ~sw env cfg in
    let account_id =
      resolve_account_id ~capability:Proto.Capability.contacts cfg client
    in
    let filter =
      match (book, text) with
      | None, None -> None
      | in_address_book, text ->
          Some (Proto.Contact_card.filter ?in_address_book ?text ())
    in
    Cli.debug cfg "Querying contact cards with limit %d" limit;
    let ids =
      query_ids client ~capabilities:contacts_capabilities ~max:limit
        (fun ~position ~limit ->
          Chain.contact_card_query ~account_id ?filter
            ~sort:
              [
                Proto.Filter.comparator ~is_ascending:true
                  (Proto.Contact_card.Sort.to_string `Name_surname);
              ]
            ~position ~limit ())
    in
    if ids = [] then Fmt.pr "No contact cards found.@."
    else begin
      let result =
        call client ~capabilities:contacts_capabilities
          (Chain.contact_card_get ~account_id ~ids:(Chain.ids ids) ())
      in
      let shown = List.length result.list in
      listing_header "Contacts"
        (Fmt.str "%d%s" shown (limit_note ~shown ~limit));
      List.iter
        (fun (c : Proto.Contact_card.t) ->
          let id_str =
            match c.id with Some id -> Proto.Id.to_string id | None -> "?"
          in
          Fmt.pr "  %a %a@,"
            Fmt.(styled `Cyan string)
            id_str
            Fmt.(styled `White string)
            (truncate_string 50 (card_display_name c.card));
          List.iter (fun e -> Fmt.pr "    Email: %s@," e) (card_emails c.card);
          List.iter (fun p -> Fmt.pr "    Phone: %s@," p) (card_phones c.card);
          List.iter
            (fun o -> Fmt.pr "    Org:   %s@," (truncate_string 60 o))
            (card_organizations c.card);
          Fmt.pr "@,")
        result.list;
      Fmt.pr "@]@."
    end
  in
  let doc = "List contact cards" in
  let info = Cmd.info "contacts" ~doc in
  Cmd.v info
    Term.(const run $ Cli.config_term $ limit_term $ book_term $ search_term)

let session_cmd =
  let run cfg =
    Eio_main.run @@ fun env ->
    Eio.Switch.run @@ fun sw ->
    let client = Cli.create_client ~sw env cfg in
    let session = Client.session client in

    Fmt.pr "@[<v>%a@," Fmt.(styled `Bold string) "Session Information:";
    Fmt.pr "  Username: %a@,"
      Fmt.(styled `Green string)
      (terminal_text session.username);
    Fmt.pr "  State: %s@," (terminal_text session.state);
    Fmt.pr "  API URL: %s@," (terminal_text session.api_url);
    Fmt.pr "  Upload URL: %s@," (terminal_text session.upload_url);
    Fmt.pr "  Download URL: %s@," (terminal_text session.download_url);
    Fmt.pr "@,  %a@," Fmt.(styled `Bold string) "Capabilities:";
    List.iter
      (fun (cap, _) -> Fmt.pr "    %s@," (terminal_text cap))
      session.capabilities;
    Fmt.pr "@,  %a@," Fmt.(styled `Bold string) "Accounts:";
    List.iter
      (fun (id, acct) ->
        let acct : Proto.Session.Account.t = acct in
        Fmt.pr "    %a: %s (personal=%b, read_only=%b)@,"
          Fmt.(styled `Cyan string)
          (Proto.Id.to_string id) (terminal_text acct.name) acct.is_personal
          acct.is_read_only)
      session.accounts;
    Fmt.pr "@,  %a@," Fmt.(styled `Bold string) "Primary Accounts:";
    List.iter
      (fun (cap, id) ->
        Fmt.pr "    %s: %s@," (terminal_text cap) (Proto.Id.to_string id))
      session.primary_accounts;
    Fmt.pr "@]@."
  in
  let doc = "Show JMAP session information" in
  let info = Cmd.info "session" ~doc in
  Cmd.v info Term.(const run $ Cli.config_term)

let mailboxes_cmd =
  let run cfg =
    Eio_main.run @@ fun env ->
    Eio.Switch.run @@ fun sw ->
    let client = Cli.create_client ~sw env cfg in
    let account_id = resolve_account_id cfg client in

    Cli.debug cfg "Fetching mailboxes for account %s"
      (Proto.Id.to_string account_id);

    let result = call client (Chain.mailbox_get ~account_id ()) in
    listing_header "Mailboxes"
      (Fmt.str "%d, state %s" (List.length result.list)
         (terminal_text result.state));
    let sorted =
      List.sort
        (fun (a : Proto.Mailbox.t) (b : Proto.Mailbox.t) ->
          let sort_a = Option.value ~default:0L a.sort_order in
          let sort_b = Option.value ~default:0L b.sort_order in
          let cmp = Int64.compare sort_a sort_b in
          let name_a = Option.value ~default:"" a.name in
          let name_b = Option.value ~default:"" b.name in
          if cmp <> 0 then cmp else String.compare name_a name_b)
        result.list
    in
    List.iter
      (fun (mbox : Proto.Mailbox.t) ->
        let role_str =
          match mbox.role with
          | Some role ->
              Printf.sprintf " [%s]"
                (terminal_text (Proto.Mailbox.role_to_string role))
          | None -> ""
        in
        let id_str =
          match mbox.id with Some id -> Proto.Id.to_string id | None -> "?"
        in
        let name =
          terminal_text (Option.value ~default:"(unnamed)" mbox.name)
        in
        let total = Option.value ~default:0L mbox.total_emails in
        let unread = Option.value ~default:0L mbox.unread_emails in
        Fmt.pr "  %a %s%a  (%Ld total, %Ld unread)@,"
          Fmt.(styled `Cyan string)
          id_str name
          Fmt.(styled `Yellow string)
          role_str total unread)
      sorted;
    Fmt.pr "@]@."
  in
  let doc = "List mailboxes" in
  let info = Cmd.info "mailboxes" ~doc in
  Cmd.v info Term.(const run $ Cli.config_term)

let list_properties =
  [
    `Id;
    `Blob_id;
    `Thread_id;
    `Mailbox_ids;
    `Keywords;
    `Size;
    `Received_at;
    `Subject;
    `From;
    `Preview;
  ]

let emails_cmd =
  let limit_term =
    let doc = "Maximum number of emails to list" in
    Arg.(value & opt positive_int 20 & info [ "limit"; "n" ] ~docv:"N" ~doc)
  in
  let mailbox_term =
    let doc = "Mailbox ID to filter by" in
    Arg.(value & opt (some id) None & info [ "mailbox"; "m" ] ~docv:"ID" ~doc)
  in
  let run cfg limit mailbox_id_str =
    Eio_main.run @@ fun env ->
    Eio.Switch.run @@ fun sw ->
    let client = Cli.create_client ~sw env cfg in
    let account_id = resolve_account_id cfg client in

    Cli.debug cfg "Querying emails with limit %d" limit;

    let filter =
      Option.map (fun id -> Proto.Email.filter ~in_mailbox:id ()) mailbox_id_str
    in
    let email_ids =
      query_ids client ~max:limit (fun ~position ~limit ->
          Chain.email_query ~account_id ?filter ~sort:received_at_first
            ~position ~limit ())
    in
    Cli.debug cfg "Found %d email IDs" (List.length email_ids);
    let emails =
      get_emails client ~account_id ~properties:list_properties email_ids
    in

    if emails = [] then Fmt.pr "No emails found.@."
    else begin
      let shown = List.length emails in
      listing_header "Emails" (Fmt.str "%d%s" shown (limit_note ~shown ~limit));
      List.iter
        (fun (email : Proto.Email.t) ->
          let from_str =
            match email.from with
            | Some addrs -> format_email_addresses addrs
            | None -> "(unknown)"
          in
          let subject = email_subject email in
          let flags = format_keywords (email_keywords email) in
          let flag_str = if flags = "" then "" else " [" ^ flags ^ "]" in
          Fmt.pr "  %a %s@,"
            Fmt.(styled `Cyan string)
            (email_id email) (email_received_at email);
          Fmt.pr "    From: %s@," (truncate_string 60 from_str);
          Fmt.pr "    Subject: %a%s@,"
            Fmt.(styled `White string)
            (truncate_string 60 subject)
            flag_str;
          Fmt.pr "    Preview: %s@,@,"
            (truncate_string 70 (email_preview email)))
        emails;
      Fmt.pr "@]@."
    end
  in
  let doc = "List emails" in
  let info = Cmd.info "emails" ~doc in
  Cmd.v info Term.(const run $ Cli.config_term $ limit_term $ mailbox_term)

let search_cmd =
  let query_term =
    let doc = "Search query text" in
    Arg.(required & pos 0 (some string) None & info [] ~docv:"QUERY" ~doc)
  in
  let limit_term =
    let doc = "Maximum number of results" in
    Arg.(value & opt positive_int 20 & info [ "limit"; "n" ] ~docv:"N" ~doc)
  in
  let run cfg query limit =
    Eio_main.run @@ fun env ->
    Eio.Switch.run @@ fun sw ->
    let client = Cli.create_client ~sw env cfg in
    let account_id = resolve_account_id cfg client in

    Cli.debug cfg "Searching for: %s" query;

    let filter = Proto.Email.filter ~text:query () in
    let email_ids =
      query_ids client ~max:limit (fun ~position ~limit ->
          Chain.email_query ~account_id ~filter ~sort:received_at_first
            ~position ~limit ())
    in
    let emails =
      get_emails client ~account_id ~properties:list_properties email_ids
    in

    if emails = [] then
      Fmt.pr "No emails found matching: %s@." (terminal_text query)
    else begin
      let shown = List.length emails in
      listing_header
        (Fmt.str "Search results for \"%s\"" (terminal_text query))
        (Fmt.str "%d%s" shown (limit_note ~shown ~limit));
      List.iter
        (fun (email : Proto.Email.t) ->
          let from_str =
            match email.from with
            | Some addrs -> format_email_addresses addrs
            | None -> "(unknown)"
          in
          let subject = email_subject email in
          Fmt.pr "  %a %s@,"
            Fmt.(styled `Cyan string)
            (email_id email) (email_received_at email);
          Fmt.pr "    From: %s@," (truncate_string 60 from_str);
          Fmt.pr "    Subject: %a@,"
            Fmt.(styled `White string)
            (truncate_string 60 subject);
          Fmt.pr "    Preview: %s@,@,"
            (truncate_string 70 (email_preview email)))
        emails;
      Fmt.pr "@]@."
    end
  in
  let doc = "Search emails by text" in
  let info = Cmd.info "search" ~doc in
  Cmd.v info Term.(const run $ Cli.config_term $ query_term $ limit_term)

let recent_cmd =
  let limit_term =
    let doc = "Number of recent emails to show" in
    Arg.(value & opt positive_int 100 & info [ "limit"; "n" ] ~docv:"N" ~doc)
  in
  let format_term =
    let doc = "Output format: table, compact, or detailed" in
    Arg.(
      value
      & opt
          (enum
             [
               ("table", `Table); ("compact", `Compact); ("detailed", `Detailed);
             ])
          `Table
      & info [ "format"; "f" ] ~docv:"FORMAT" ~doc)
  in
  let run cfg limit format =
    Eio_main.run @@ fun env ->
    Eio.Switch.run @@ fun sw ->
    let client = Cli.create_client ~sw env cfg in
    let account_id = resolve_account_id cfg client in

    Cli.debug cfg "Fetching %d most recent emails" limit;

    let properties =
      [
        `Id;
        `Blob_id;
        `Thread_id;
        `Mailbox_ids;
        `Keywords;
        `Size;
        `Received_at;
        `Subject;
        `From;
        `To;
        `Cc;
        `Preview;
      ]
    in
    let email_ids =
      query_ids client ~max:limit (fun ~position ~limit ->
          Chain.email_query ~account_id ~sort:received_at_first ~position ~limit
            ())
    in
    Cli.debug cfg "Query returned %d email IDs" (List.length email_ids);
    let emails = get_emails client ~account_id ~properties email_ids in

    if emails = [] then Fmt.pr "No emails found.@."
    else begin
      let shown = List.length emails in
      let summary = Fmt.str "%d%s" shown (limit_note ~shown ~limit) in
      Cli.debug cfg "Got %d emails" shown;

      match format with
      | `Compact ->
          List.iter
            (fun (email : Proto.Email.t) ->
              let from_str =
                match email.from with
                | Some (addr :: _) ->
                    terminal_text (Option.value addr.name ~default:addr.email)
                | _ -> "?"
              in
              let subject = email_subject email in
              let flags = format_keywords (email_keywords email) in
              Fmt.pr "%s\t%s\t%s\t%s\t%s@." (email_id email)
                (email_received_at email)
                (truncate_string 20 from_str)
                (truncate_string 50 subject)
                flags)
            emails
      | `Table ->
          listing_header "Recent Emails" summary;
          Fmt.pr "%-12s  %-19s  %-20s  %-40s  %s@," "ID" "Date" "From" "Subject"
            "Flags";
          Fmt.pr "%s@," (String.make 110 '-');
          List.iter
            (fun (email : Proto.Email.t) ->
              let from_str =
                match email.from with
                | Some (addr :: _) ->
                    terminal_text (Option.value addr.name ~default:addr.email)
                | _ -> "?"
              in
              let subject = email_subject email in
              let flags = format_keywords (email_keywords email) in
              Fmt.pr "%-12s  %-19s  %-20s  %-40s  %s@,"
                (truncate_string 12 (email_id email))
                (email_received_at email)
                (truncate_string 20 from_str)
                (truncate_string 40 subject)
                flags)
            emails;
          Fmt.pr "@]@."
      | `Detailed ->
          listing_header "Recent Emails" summary;
          List.iteri
            (fun i (email : Proto.Email.t) ->
              let from_str =
                match email.from with
                | Some addrs -> format_email_addresses addrs
                | None -> "(unknown)"
              in
              let to_str =
                match email.to_ with
                | Some addrs -> format_email_addresses addrs
                | None -> ""
              in
              let cc_str =
                match email.cc with
                | Some addrs -> format_email_addresses addrs
                | None -> ""
              in
              let subject = email_subject email in
              let flags = format_keywords (email_keywords email) in
              let mailbox_count =
                List.length (Proto.Email.mailbox_list email)
              in

              Fmt.pr "@[<v 2>%a Email %d of %d@,"
                Fmt.(styled `Bold string)
                "---" (i + 1) shown;
              Fmt.pr "ID:       %a@," Fmt.(styled `Cyan string) (email_id email);
              Fmt.pr "Thread:   %s@," (email_thread_id email);
              Fmt.pr "Date:     %s@," (email_received_at email);
              Fmt.pr "From:     %s@," from_str;
              if to_str <> "" then Fmt.pr "To:       %s@," to_str;
              if cc_str <> "" then Fmt.pr "Cc:       %s@," cc_str;
              Fmt.pr "Subject:  %a@," Fmt.(styled `White string) subject;
              Fmt.pr "Size:     %Ld bytes@," (email_size email);
              Fmt.pr "Mailboxes: %d@," mailbox_count;
              if flags <> "" then Fmt.pr "Flags:    %s@," flags;
              Fmt.pr "Preview:  %s@]@,@," (email_preview email))
            emails;
          Fmt.pr "@]@."
    end
  in
  let doc = "List recent emails as a table, a compact line, or in full" in
  let info = Cmd.info "recent" ~doc in
  Cmd.v info Term.(const run $ Cli.config_term $ limit_term $ format_term)

let threads_cmd =
  let email_id_term =
    let doc = "Email ID to get thread for" in
    Arg.(required & pos 0 (some id) None & info [] ~docv:"EMAIL_ID" ~doc)
  in
  let run cfg target_email_id =
    Eio_main.run @@ fun env ->
    Eio.Switch.run @@ fun sw ->
    let client = Cli.create_client ~sw env cfg in
    let account_id = resolve_account_id cfg client in
    let email_result =
      call client
        (Chain.email_get ~account_id ~ids:(Chain.id target_email_id)
           ~properties:
             [ `Id; `Blob_id; `Thread_id; `Mailbox_ids; `Size; `Received_at ]
           ())
    in
    match email_result.list with
    | [] -> die "email not found: %a" Proto.Id.pp target_email_id
    | email :: _ -> (
        let thread_id =
          match email.thread_id with
          | Some id -> id
          | None -> die "email %a has no thread id" Proto.Id.pp target_email_id
        in
        Cli.debug cfg "Thread ID: %s" (Proto.Id.to_string thread_id);
        let thread_result =
          call client
            (Chain.thread_get ~account_id ~ids:(Chain.id thread_id) ())
        in
        match thread_result.list with
        | [] -> die "thread not found"
        | thread :: _ ->
            let thread_id_str =
              match thread.id with
              | Some id -> Proto.Id.to_string id
              | None -> "?"
            in
            let thread_email_ids = Option.value ~default:[] thread.email_ids in
            let emails =
              get_emails client ~account_id ~properties:list_properties
                thread_email_ids
            in
            listing_header
              (Fmt.str "Thread %s" thread_id_str)
              (Fmt.str "%d" (List.length emails));
            List.iter
              (fun (email : Proto.Email.t) ->
                let from_str =
                  match email.from with
                  | Some addrs -> format_email_addresses addrs
                  | None -> "(unknown)"
                in
                let subject = email_subject email in
                Fmt.pr "  %a %s@,"
                  Fmt.(styled `Cyan string)
                  (email_id email) (email_received_at email);
                Fmt.pr "    From: %s@," (truncate_string 60 from_str);
                Fmt.pr "    Subject: %a@,@,"
                  Fmt.(styled `White string)
                  (truncate_string 60 subject))
              emails;
            Fmt.pr "@]@.")
  in
  let doc = "Show email thread" in
  let info = Cmd.info "thread" ~doc in
  Cmd.v info Term.(const run $ Cli.config_term $ email_id_term)

let identities_cmd =
  let run cfg =
    Eio_main.run @@ fun env ->
    Eio.Switch.run @@ fun sw ->
    let client = Cli.create_client ~sw env cfg in
    let account_id =
      resolve_account_id ~capability:Proto.Capability.submission cfg client
    in

    let result =
      call client ~capabilities:submission_capabilities
        (Chain.identity_get ~account_id ())
    in
    listing_header "Identities"
      (Fmt.str "%d, state %s" (List.length result.list)
         (terminal_text result.state));
    List.iter
      (fun (ident : Proto.Identity.t) ->
        let ident_id =
          match ident.id with Some id -> Proto.Id.to_string id | None -> "?"
        in
        let ident_name =
          terminal_text (Option.value ~default:"(unnamed)" ident.name)
        in
        let ident_email =
          terminal_text (Option.value ~default:"(no email)" ident.email)
        in
        let ident_sig =
          terminal_text (Option.value ~default:"" ident.text_signature)
        in
        let ident_may_delete = Option.value ~default:false ident.may_delete in
        Fmt.pr "  %a@," Fmt.(styled `Cyan string) ident_id;
        Fmt.pr "    Name: %s@," ident_name;
        Fmt.pr "    Email: %a@," Fmt.(styled `Green string) ident_email;
        if ident_sig <> "" then
          Fmt.pr "    Signature: %s@," (truncate_string 50 ident_sig);
        Fmt.pr "    May delete: %b@,@," ident_may_delete)
      result.list;
    Fmt.pr "@]@."
  in
  let doc = "List email identities" in
  let info = Cmd.info "identities" ~doc in
  Cmd.v info Term.(const run $ Cli.config_term)

let inbox_cmd =
  let limit_term =
    let doc = "Maximum number of emails to show" in
    Arg.(value & opt positive_int 20 & info [ "limit"; "n" ] ~docv:"N" ~doc)
  in
  let run cfg limit =
    Eio_main.run @@ fun env ->
    Eio.Switch.run @@ fun sw ->
    let client = Cli.create_client ~sw env cfg in
    let account_id = resolve_account_id cfg client in

    Cli.debug cfg "Fetching inbox emails";

    let inbox_id =
      match Sync.mailbox_id client ~account_id `Inbox with
      | Ok id -> id
      | Error error -> fail_sync error
    in
    Cli.debug cfg "Found inbox: %s" (Proto.Id.to_string inbox_id);
    let filter = Proto.Email.filter ~in_mailbox:inbox_id () in
    let email_ids =
      query_ids client ~max:limit (fun ~position ~limit ->
          Chain.email_query ~account_id ~filter ~sort:received_at_first
            ~position ~limit ())
    in
    let emails =
      get_emails client ~account_id ~properties:list_properties email_ids
    in
    let shown = List.length emails in
    listing_header "Inbox" (Fmt.str "%d%s" shown (limit_note ~shown ~limit));
    List.iter
      (fun (email : Proto.Email.t) ->
        let from_str =
          match email.from with
          | Some (addr :: _) ->
              terminal_text (Option.value addr.name ~default:addr.email)
          | _ -> "?"
        in
        let subject = email_subject email in
        let flags = format_keywords (email_keywords email) in
        Fmt.pr "  %a %s@,"
          Fmt.(styled `Cyan string)
          (email_id email) (email_received_at email);
        Fmt.pr "    From: %s@," (truncate_string 40 from_str);
        Fmt.pr "    Subject: %a%s@,"
          Fmt.(styled `White string)
          (truncate_string 50 subject)
          (if flags = "" then "" else " [" ^ flags ^ "]");
        Fmt.pr "@,")
      emails;
    Fmt.pr "@]@."
  in
  let doc = "List inbox emails" in
  let info = Cmd.info "inbox" ~doc in
  Cmd.v info Term.(const run $ Cli.config_term $ limit_term)

let thread_view_cmd =
  let limit_term =
    let doc = "Number of threads to show" in
    Arg.(value & opt positive_int 10 & info [ "limit"; "n" ] ~docv:"N" ~doc)
  in
  let run cfg limit =
    Eio_main.run @@ fun env ->
    Eio.Switch.run @@ fun sw ->
    let client = Cli.create_client ~sw env cfg in
    let account_id = resolve_account_id cfg client in

    Cli.debug cfg "Fetching threaded view";
    let representative_ids =
      query_ids client ~max:limit (fun ~position ~limit ->
          Chain.email_query ~account_id ~sort:received_at_first
            ~collapse_threads:true ~position ~limit ())
    in
    let representative_emails =
      get_emails client ~account_id ~properties:[ `Id; `Thread_id ]
        representative_ids
    in
    (* [collapse_threads] returns one Email per Thread, in the sort order asked
       for, so the representatives give the order the threads are printed in. *)
    let thread_ids =
      List.filter_map
        (fun (email : Proto.Email.t) -> email.thread_id)
        representative_emails
    in
    let threads =
      get_objects client ~kind:"threads" ~id:Proto.Thread.id thread_ids
        (fun ~ids -> Chain.thread_get ~account_id ~ids:(Chain.ids ids) ())
    in
    let email_ids =
      List.concat_map
        (fun (thread : Proto.Thread.t) ->
          Option.value ~default:[] thread.email_ids)
        threads
      |> List.sort_uniq Proto.Id.compare
    in
    let emails =
      get_emails client ~account_id
        ~properties:
          [
            `Id;
            `Blob_id;
            `Thread_id;
            `Mailbox_ids;
            `Size;
            `Received_at;
            `Subject;
            `From;
            `Preview;
          ]
        email_ids
    in
    let threads_map = Hashtbl.create 16 in
    List.iter
      (fun (email : Proto.Email.t) ->
        let tid = email_thread_id email in
        let existing =
          try Hashtbl.find threads_map tid with Not_found -> []
        in
        Hashtbl.replace threads_map tid (email :: existing))
      emails;

    let shown = List.length threads in
    listing_header "Threaded View"
      (Fmt.str "%d threads over %d emails%s" shown (List.length emails)
         (limit_note ~shown ~limit));

    List.iter
      (fun thread_id ->
        let emails =
          try Hashtbl.find threads_map (Proto.Id.to_string thread_id)
          with Not_found -> []
        in
        let emails =
          List.sort
            (fun (a : Proto.Email.t) (b : Proto.Email.t) ->
              let a_time = Option.value ~default:Ptime.epoch a.received_at in
              let b_time = Option.value ~default:Ptime.epoch b.received_at in
              Ptime.compare a_time b_time)
            emails
        in
        match emails with
        | [] -> ()
        | first_email :: _ ->
            let subject = email_subject first_email in
            Fmt.pr "  %a Thread: %s (%d emails)@,"
              Fmt.(styled `Bold string)
              "▸"
              (truncate_string 50 subject)
              (List.length emails);
            List.iter
              (fun (email : Proto.Email.t) ->
                let from_str =
                  match email.from with
                  | Some (addr :: _) ->
                      terminal_text (Option.value addr.name ~default:addr.email)
                  | _ -> "?"
                in
                Fmt.pr "      %s  %s  %s@,"
                  (email_id email |> truncate_string 12)
                  (email_received_at email)
                  (truncate_string 30 from_str))
              emails;
            Fmt.pr "@,")
      thread_ids;
    Fmt.pr "@]@."
  in
  let doc = "Show threaded view" in
  let info = Cmd.info "thread-view" ~doc in
  Cmd.v info Term.(const run $ Cli.config_term $ limit_term)

let mark_read_cmd =
  let email_id_term =
    let doc = "Email ID to mark as read" in
    Arg.(required & pos 0 (some id) None & info [] ~docv:"EMAIL_ID" ~doc)
  in
  let unread_term =
    let doc = "Mark as unread instead of read" in
    Arg.(value & flag & info [ "unread" ] ~doc)
  in
  let run cfg email_id unread =
    Eio_main.run @@ fun env ->
    Eio.Switch.run @@ fun sw ->
    let client = Cli.create_client ~sw env cfg in
    let account_id = resolve_account_id cfg client in
    Cli.debug cfg "%s email %s"
      (if unread then "Marking as unread" else "Marking as read")
      (Proto.Id.to_string email_id);

    (* RFC 8620 Section 5.3: patching "keywords/$seen" to null removes the
       keyword, anything else sets it. *)
    let patch =
      let open Proto.Email.Patch in
      if unread then Proto.Patch.v [ remove_keyword `Seen ]
      else Proto.Patch.v [ set_keyword `Seen ]
    in

    let result =
      call client (Chain.email_set ~account_id ~update:[ (email_id, patch) ] ())
    in
    let updated_ids =
      result.updated |> Option.value ~default:[] |> List.map fst
    in
    if List.exists (Proto.Id.equal email_id) updated_ids then
      Fmt.pr "Email %a marked as %s@." Proto.Id.pp email_id
        (if unread then "unread" else "read")
    else
      let not_updated = Option.value ~default:[] result.not_updated in
      match
        List.find_opt (fun (id, _) -> Proto.Id.equal id email_id) not_updated
      with
      | Some failure -> die "%a" Proto.Method.pp_set_failure failure
      | None ->
          die "the server reported no outcome for email %a" Proto.Id.pp email_id
  in
  let doc = "Mark an email as read/unread (demonstrates Email/set)" in
  let info = Cmd.info "mark-read" ~doc in
  Cmd.v info Term.(const run $ Cli.config_term $ email_id_term $ unread_term)

let delete_email_cmd =
  let email_ids_term =
    let doc = "Email IDs to permanently delete" in
    Arg.(non_empty & pos_all id [] & info [] ~docv:"EMAIL_ID" ~doc)
  in
  let yes_term =
    let doc =
      "Confirm permanent deletion. Email/set destroy cannot be undone."
    in
    Arg.(value & flag & info [ "yes" ] ~doc)
  in
  let arguments =
    let validate yes ids =
      if not yes then
        Error
          "permanent deletion requires --yes; this operation cannot be undone"
      else unique_ids ids
    in
    Term.(term_result' ~usage:true (const validate $ yes_term $ email_ids_term))
  in
  let run cfg email_ids =
    Eio_main.run @@ fun env ->
    Eio.Switch.run @@ fun sw ->
    let client = Cli.create_client ~sw env cfg in
    let account_id = resolve_account_id cfg client in

    Cli.debug cfg "Deleting %d email(s)" (List.length email_ids);
    let report =
      run_set client cfg email_ids
        ~outcome:(fun (result : _ Proto.Method.set_response) ->
          ( Option.value ~default:[] result.destroyed,
            Option.value ~default:[] result.not_destroyed ))
        (fun ~ids -> Chain.email_set ~account_id ~destroy:(Chain.ids ids) ())
    in
    report_set
      { succeeded = "Deleted"; action = "delete"; activity = "Deletion" }
      report
  in
  let doc = "Permanently delete emails (requires --yes)" in
  let info = Cmd.info "delete" ~doc in
  Cmd.v info Term.(const run $ Cli.config_term $ arguments)

let state_cmd =
  let run cfg =
    Eio_main.run @@ fun env ->
    Eio.Switch.run @@ fun sw ->
    let client = Cli.create_client ~sw env cfg in
    let account_id = resolve_account_id cfg client in
    let state = call client (Chain.email_state ~account_id) in
    Fmt.pr "%a@." Fmt.(styled `Cyan string) (terminal_text state)
  in
  let doc = "Show the current email state, to hand to changes or sync" in
  let info = Cmd.info "state" ~doc in
  Cmd.v info Term.(const run $ Cli.config_term)

let changes_cmd =
  let state_term =
    let doc = "State to get changes since, as printed by the state command" in
    Arg.(required & pos 0 (some string) None & info [] ~docv:"STATE" ~doc)
  in
  let run cfg since =
    Eio_main.run @@ fun env ->
    Eio.Switch.run @@ fun sw ->
    let client = Cli.create_client ~sw env cfg in
    let account_id = resolve_account_id cfg client in

    Cli.debug cfg "Getting changes since state: %s" since;
    let result =
      match Sync.email_changes client ~account_id ~since () with
      | Ok (`Changes changes) -> changes
      | Ok `Cannot_calculate_changes ->
          die
            "the server can no longer calculate changes from that state; \
             perform a full resync"
      | Error error -> fail_sync error
    in
    Fmt.pr "@[<v>%a@,@," Fmt.(styled `Bold string) "Email Changes";
    Fmt.pr "Old state: %s@," (terminal_text since);
    Fmt.pr "New state: %a@,"
      Fmt.(styled `Cyan string)
      (terminal_text result.new_state);
    Fmt.pr "Has more changes: %b@,@," result.has_more;
    Fmt.pr "Created: %d email(s)@," (List.length result.created);
    List.iter
      (fun id -> Fmt.pr "  + %s@," (Proto.Id.to_string id))
      result.created;
    Fmt.pr "Updated: %d email(s)@," (List.length result.updated);
    List.iter
      (fun id -> Fmt.pr "  ~ %s@," (Proto.Id.to_string id))
      result.updated;
    Fmt.pr "Destroyed: %d email(s)@," (List.length result.destroyed);
    List.iter
      (fun id -> Fmt.pr "  - %s@," (Proto.Id.to_string id))
      result.destroyed;
    Fmt.pr "@]@."
  in
  let doc = "Show email changes since a state (demonstrates Email/changes)" in
  let info = Cmd.info "changes" ~doc in
  Cmd.v info Term.(const run $ Cli.config_term $ state_term)

let sync_cmd =
  let state_term =
    let doc = "State to sync from" in
    Arg.(required & pos 0 (some string) None & info [] ~docv:"STATE" ~doc)
  in
  let run cfg state_str =
    Eio_main.run @@ fun env ->
    Eio.Switch.run @@ fun sw ->
    let client = Cli.create_client ~sw env cfg in
    let account_id = resolve_account_id cfg client in

    Cli.debug cfg "Syncing from state: %s" state_str;
    let changes_result =
      match Sync.email_changes client ~account_id ~since:state_str () with
      | Ok (`Changes changes) -> changes
      | Ok `Cannot_calculate_changes ->
          die
            "the server can no longer calculate changes from that state; \
             perform a full resync"
      | Error error -> fail_sync error
    in
    let created_properties =
      [
        `Id;
        `Blob_id;
        `Thread_id;
        `Mailbox_ids;
        `Size;
        `Received_at;
        `Subject;
        `From;
        `Preview;
      ]
    in
    let updated_properties =
      [
        `Id;
        `Blob_id;
        `Thread_id;
        `Mailbox_ids;
        `Size;
        `Received_at;
        `Subject;
        `From;
        `Keywords;
      ]
    in
    let created =
      get_emails client ~account_id ~properties:created_properties
        changes_result.created
    in
    let updated =
      get_emails client ~account_id ~properties:updated_properties
        changes_result.updated
    in
    Fmt.pr "@[<v>%a (state: %s → %s)@,@,"
      Fmt.(styled `Bold string)
      "Sync Results" (terminal_text state_str)
      (terminal_text changes_result.new_state);

    if created <> [] then begin
      Fmt.pr "%a (%d)@,"
        Fmt.(styled `Green string)
        "New emails" (List.length created);
      List.iter
        (fun (email : Proto.Email.t) ->
          let from_str =
            match email.from with
            | Some (addr :: _) ->
                terminal_text (Option.value addr.name ~default:addr.email)
            | _ -> "?"
          in
          let subject = email_subject email in
          Fmt.pr "  + %s  %s  %s@,"
            (email_id email |> truncate_string 12)
            (truncate_string 20 from_str)
            (truncate_string 40 subject))
        created;
      Fmt.pr "@,"
    end;

    if updated <> [] then begin
      Fmt.pr "%a (%d)@,"
        Fmt.(styled `Yellow string)
        "Updated emails" (List.length updated);
      List.iter
        (fun (email : Proto.Email.t) ->
          let flags = format_keywords (email_keywords email) in
          Fmt.pr "  ~ %s  [%s]@," (email_id email |> truncate_string 12) flags)
        updated;
      Fmt.pr "@,"
    end;

    if List.length changes_result.destroyed > 0 then begin
      Fmt.pr "%a (%d)@,"
        Fmt.(styled `Red string)
        "Deleted emails"
        (List.length changes_result.destroyed);
      List.iter
        (fun id -> Fmt.pr "  - %s@," (Proto.Id.to_string id))
        changes_result.destroyed;
      Fmt.pr "@,"
    end;

    if changes_result.has_more then
      Fmt.pr "%a - call sync again with state %s@,"
        Fmt.(styled `Bold string)
        "More changes available"
        (terminal_text changes_result.new_state);

    Fmt.pr "@]@."
  in
  let doc = "Incremental email sync" in
  let info = Cmd.info "sync" ~doc in
  Cmd.v info Term.(const run $ Cli.config_term $ state_term)

let headers_cmd =
  let email_id_term =
    let doc = "Email ID to get headers for" in
    Arg.(required & pos 0 (some id) None & info [] ~docv:"EMAIL_ID" ~doc)
  in

  let format_header_value = function
    | Proto.Email_header.String_single None -> "(null)"
    | Proto.Email_header.String_single (Some s) -> s
    | Proto.Email_header.String_all [] -> "(empty list)"
    | Proto.Email_header.String_all strs -> String.concat "; " strs
    | Proto.Email_header.Addresses_single None -> "(null)"
    | Proto.Email_header.Addresses_single (Some []) -> "(empty)"
    | Proto.Email_header.Addresses_single (Some addrs) ->
        format_email_addresses addrs
    | Proto.Email_header.Addresses_all [] -> "(empty list)"
    | Proto.Email_header.Addresses_all groups ->
        String.concat " | " (List.map format_email_addresses groups)
    | Proto.Email_header.Grouped_single None -> "(null)"
    | Proto.Email_header.Grouped_single (Some groups) ->
        String.concat "; "
          (List.map
             (fun (g : Proto.Email_address.Group.t) ->
               let name = Option.value ~default:"(ungrouped)" g.name in
               Printf.sprintf "%s: %s" name (format_email_addresses g.addresses))
             groups)
    | Proto.Email_header.Grouped_all _ -> "(grouped addresses list)"
    | Proto.Email_header.Date_single None -> "(null)"
    | Proto.Email_header.Date_single (Some t) -> ptime_to_string t
    | Proto.Email_header.Date_all [] -> "(empty list)"
    | Proto.Email_header.Date_all dates ->
        String.concat "; "
          (List.map
             (function None -> "(null)" | Some t -> ptime_to_string t)
             dates)
    | Proto.Email_header.Strings_single None -> "(null)"
    | Proto.Email_header.Strings_single (Some []) -> "(empty)"
    | Proto.Email_header.Strings_single (Some strs) -> String.concat ", " strs
    | Proto.Email_header.Strings_all [] -> "(empty list)"
    | Proto.Email_header.Strings_all groups ->
        String.concat " | "
          (List.map
             (function
               | None -> "(null)" | Some strs -> String.concat ", " strs)
             groups)
  in

  let run cfg target_email_id =
    Eio_main.run @@ fun env ->
    Eio.Switch.run @@ fun sw ->
    let client = Cli.create_client ~sw env cfg in
    let account_id = resolve_account_id cfg client in
    Cli.debug cfg "Fetching headers for email %s"
      (Proto.Id.to_string target_email_id);

    (* Every parsed form of RFC 8621 Section 4.1.2, plus the Section 4.1.3
       [:all] suffix on a header the message repeats. *)
    let header_props =
      let open Proto.Email_header in
      [
        `Header (raw "Subject");
        `Header (text `Subject);
        `Header (addresses `From);
        `Header (addresses `To);
        `Header (addresses `Cc);
        `Header (addresses `Bcc);
        `Header (addresses `Reply_to);
        `Header (addresses `Sender);
        `Header (grouped_addresses `From);
        `Header (message_ids `Message_id);
        `Header (message_ids `In_reply_to);
        `Header (message_ids `References);
        `Header (date `Date);
        `Header (urls `List_unsubscribe);
        `Header (urls `List_post);
        `Header (urls `List_archive);
        `Header (text (`Custom "X-Mailer"));
        `Header (raw "X-Priority");
        `Header (text (`Custom "X-Spam-Status"));
        `Header (raw "Content-Type");
        `Header (raw "MIME-Version");
        `Header (raw ~all:true "Received");
      ]
    in
    let properties = `Id :: `Thread_id :: `Subject :: header_props in

    let email_result =
      call client
        (Chain.email_get ~account_id ~ids:(Chain.id target_email_id) ~properties
           ())
    in
    match email_result.list with
    | [] -> die "email not found: %a" Proto.Id.pp target_email_id
    | email :: _ ->
        Fmt.pr "@[<v>%a@,"
          Fmt.(styled `Bold string)
          "Email Headers (RFC 8621 §4.1)";
        Fmt.pr "ID: %s@," (email_id email);
        Fmt.pr "Thread: %s@," (email_thread_id email);
        (match email.subject with
        | Some s -> Fmt.pr "Subject (convenience): %s@," (terminal_text s)
        | None -> ());
        Fmt.pr "@,";

        let raw_headers = email.dynamic_headers in
        if raw_headers = [] then
          Fmt.pr "%a@,"
            Fmt.(styled `Yellow string)
            "No dynamic headers returned"
        else begin
          Fmt.pr "%a (%d properties)@,@,"
            Fmt.(styled `Bold string)
            "Dynamic Header Properties" (List.length raw_headers);

          List.iter
            (fun (name, json) ->
              match Proto.Email.decode_header_value name json with
              | None ->
                  Fmt.pr "  %a: (decode failed)@,"
                    Fmt.(styled `Red string)
                    (terminal_text name)
              | Some value ->
                  let formatted = terminal_text (format_header_value value) in
                  if String.length formatted > 80 then
                    Fmt.pr "  %a:@,    %s@,"
                      Fmt.(styled `Cyan string)
                      (terminal_text name) formatted
                  else
                    Fmt.pr "  %a: %s@,"
                      Fmt.(styled `Cyan string)
                      (terminal_text name) formatted)
            raw_headers
        end;
        Fmt.pr "@]@."
  in
  let doc =
    "Show email headers in various forms (demonstrates RFC 8621 §4.1)"
  in
  let info = Cmd.info "headers" ~doc in
  Cmd.v info Term.(const run $ Cli.config_term $ email_id_term)

let main_cmd =
  let doc = "JMAP command-line client" in
  let man =
    [
      `S Manpage.s_description;
      `P
        "A command-line client for JMAP (JSON Meta Application Protocol) email \
         servers.";
      `P
        "The listing commands $(b,emails), $(b,search), $(b,inbox), \
         $(b,recent) and $(b,thread-view) all take $(b,--limit). Each pages \
         the query under the limit the server reports, and says so when the \
         rows it printed filled the limit asked for, since more may then \
         remain. $(b,--mailbox) narrows $(b,emails) alone, and $(b,--format) \
         chooses the shape of $(b,recent) alone.";
      `S Manpage.s_environment;
      `Pre Cli.env_docs;
      `S Manpage.s_examples;
      `P "List mailboxes, reading the credential from a file:";
      `Pre
        "  jmap mailboxes --url https://api.fastmail.com/jmap/session \
         --api-key-file ~/.config/jmap/key";
      `P "Show recent emails:";
      `Pre "  jmap recent -n 50 --format detailed";
      `P "Search emails:";
      `Pre "  jmap search \"meeting notes\" -n 10";
    ]
  in
  let info = Cmd.info "jmap" ~version:"0.1.0" ~doc ~man in
  Cmd.group info
    [
      session_cmd;
      mailboxes_cmd;
      emails_cmd;
      search_cmd;
      recent_cmd;
      threads_cmd;
      identities_cmd;
      addressbooks_cmd;
      contacts_cmd;
      headers_cmd;
      inbox_cmd;
      thread_view_cmd;
      mark_read_cmd;
      delete_email_cmd;
      state_cmd;
      changes_cmd;
      sync_cmd;
    ]

let () =
  Fmt_tty.setup_std_outputs ();
  exit (Cmd.eval main_cmd)
