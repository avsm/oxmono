(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

open Cmdliner

module Contact = Sortal_schema.Contact
module Temporal = Sortal_schema.Temporal

let list_cmd xdg =
  let store = Sortal_store.create_from_xdg xdg in
  let contacts = Sortal_store.list store in
  let sorted = List.sort Contact.compare contacts in
  Printf.printf "Total contacts: %d\n" (List.length sorted);
  List.iter (fun c ->
    Printf.printf "@%s: %s\n" (Contact.handle c) (Contact.name c)
  ) sorted;
  0

let show_cmd handle xdg =
  let store = Sortal_store.create_from_xdg xdg in
  match Sortal_store.lookup store handle with
  | Some c ->
    (* Use the pretty printer for rich temporal display *)
    Fmt.pr "%a@." Contact.pp c;
    0
  | None -> Logs.err (fun m -> m "Contact not found: %s" handle); 1

let thumbnail_cmd handle xdg =
  let store = Sortal_store.create_from_xdg xdg in
  match Sortal_store.lookup store handle with
  | None -> Logs.err (fun m -> m "Contact not found: %s" handle); 1
  | Some c ->
    match Sortal_store.thumbnail_path store c with
    | Some path ->
      Printf.printf "%s\n" (Eio.Path.native_exn path);
      0
    | None ->
      Logs.err (fun m -> m "No thumbnail for contact: %s" handle);
      1

let search_cmd query xdg =
  let store = Sortal_store.create_from_xdg xdg in
  match Sortal_store.search_all store query with
  | [] ->
    Logs.warn (fun m -> m "No contacts found matching: %s" query);
    1
  | matches ->
    Logs.app (fun m -> m "Found %d match%s:"
      (List.length matches)
      (if List.length matches = 1 then "" else "es"));
    List.iter (fun c ->
      Logs.app (fun m -> m "@%s: %s" (Contact.handle c) (Contact.name c));
      Option.iter (fun e -> Logs.app (fun m -> m "  Email: %s" e)) (Contact.current_email c);
      Option.iter (fun u -> Logs.app (fun m -> m "  URL: %s" u)) (Contact.best_url c)
    ) matches;
    0

let stats_cmd () xdg =
  let store = Sortal_store.create_from_xdg xdg in
  let contacts = Sortal_store.list store in
  let total = List.length contacts in
  let count pred = List.filter pred contacts |> List.length in
  let with_email = count (fun c -> Contact.emails c <> []) in
  let with_org = count (fun c -> Contact.organizations c <> []) in
  let with_url = count (fun c -> Contact.urls c <> []) in
  let with_service = count (fun c -> Contact.services c <> []) in
  let with_orcid = count (fun c -> Option.is_some (Contact.orcid c)) in
  let with_feeds = count (fun c -> Option.is_some (Contact.feeds c)) in
  let total_feeds =
    List.fold_left (fun acc c ->
      acc + Option.fold ~none:0 ~some:List.length (Contact.feeds c)
    ) 0 contacts
  in
  let total_services =
    List.fold_left (fun acc c ->
      acc + List.length (Contact.services c)
    ) 0 contacts
  in
  let pct n = float_of_int n /. float_of_int total *. 100. in
  Logs.app (fun m -> m "Contact Database Statistics:");
  Logs.app (fun m -> m "  Total contacts: %d" total);
  Logs.app (fun m -> m "  With email: %d (%.1f%%)" with_email (pct with_email));
  Logs.app (fun m -> m "  With organization: %d (%.1f%%)" with_org (pct with_org));
  Logs.app (fun m -> m "  With services: %d (%.1f%%), total %d services" with_service (pct with_service) total_services);
  Logs.app (fun m -> m "  With ORCID: %d (%.1f%%)" with_orcid (pct with_orcid));
  Logs.app (fun m -> m "  With URL: %d (%.1f%%)" with_url (pct with_url));
  Logs.app (fun m -> m "  With feeds: %d (%.1f%%), total %d feeds" with_feeds (pct with_feeds) total_feeds);
  0

(* DID resolution *)
let resolve_atproto_did http_session atp_handle =
  let url = Printf.sprintf
    "https://bsky.social/xrpc/com.atproto.identity.resolveHandle?handle=%s"
    (Uri.pct_encode atp_handle) in
  try
    Logs.info (fun m -> m "Resolving ATProto handle: %s" atp_handle);
    let response = Requests.get http_session url in
    if Requests.Response.ok response then begin
      let body = Requests.Response.body response |> Eio.Flow.read_all in
      let did_jsont =
        let open Jsont in
        let open Jsont.Object in
        map ~kind:"did_response" (fun did -> did)
        |> mem "did" string ~enc:(fun d -> d)
        |> skip_unknown
        |> finish
      in
      match Jsont_bytesrw.decode_string did_jsont body with
      | Ok did -> Some did
      | Error msg ->
        Logs.warn (fun m -> m "Failed to decode DID response for %s: %s" atp_handle msg);
        None
    end else begin
      Logs.warn (fun m -> m "DID resolution HTTP error for %s: %d"
        atp_handle (Requests.Response.status_code response));
      None
    end
  with exn ->
    Logs.warn (fun m -> m "DID resolution exception for %s: %s"
      atp_handle (Printexc.to_string exn));
    None

let sync_cmd ~force () xdg env =
  let store = Sortal_store.create_from_xdg xdg in
  let contacts = Sortal_store.list store in
  Logs.app (fun m -> m "Syncing %d contacts..." (List.length contacts));
  (* Immich face fetching *)
  let immich_errors = ref 0 in
  begin match Immich_auth.Session.load (env#fs) () with
  | None ->
    Logs.info (fun m -> m "No Immich session found, skipping face fetch (login with immich CLI first)")
  | Some immich_session ->
    let targets = if force then contacts
      else List.filter (fun c ->
        Option.is_none (Contact.thumbnail c)
      ) contacts in
    if targets = [] then
      Logs.app (fun m -> m "All contacts have thumbnails, skipping Immich fetch")
    else begin
      Logs.app (fun m -> m "%s faces from Immich for %d contacts..."
        (if force then "Force-fetching" else "Fetching")
        (List.length targets));
      let data_dir = Sortal_store.data_dir store in
      let fetched = ref 0 in
      let immich_skipped = ref 0 in
      let not_found = ref 0 in
      begin match
        Eio.Switch.run @@ fun sw ->
        match Immich_auth.Client.resume ~sw ~env ~session:immich_session () with
        | exception Failure msg ->
          Logs.warn (fun m -> m "Immich session error: %s" msg);
          immich_errors := 1
        | immich_client ->
        let api = Immich_auth.Client.client immich_client in
        let http_session = Immich.session api in
        let base_url = Immich.base_url api in
        let person_jsont =
          let open Jsont in
          let open Jsont.Object in
          map ~kind:"person" (fun id name -> (id, name))
          |> mem "id" string ~enc:(fun (id, _) -> id)
          |> mem "name" string ~enc:(fun (_, name) -> name)
          |> skip_unknown
          |> finish
        in
        let people_jsont = Jsont.list person_jsont in
        List.iter (fun contact ->
          let handle = Contact.handle contact in
          let names = Contact.names contact in
          let rec try_names = function
            | [] ->
              Logs.info (fun m -> m "@%s: no match in Immich" handle);
              incr not_found
            | name :: rest ->
              let encoded_name = Uri.pct_encode name in
              let url = Printf.sprintf "%s/search/person?name=%s"
                base_url encoded_name in
              try
                let response = Requests.get http_session url in
                if Requests.Response.ok response then begin
                  let body = Requests.Response.body response |> Eio.Flow.read_all in
                  match Jsont_bytesrw.decode_string people_jsont body with
                  | Error _ ->
                    Logs.info (fun m -> m "@%s: failed to parse Immich response" handle);
                    try_names rest
                  | Ok [] ->
                    Logs.info (fun m -> m "@%s: no results for '%s'" handle name);
                    try_names rest
                  | Ok ((person_id, person_name) :: _) ->
                    Logs.info (fun m -> m "@%s: found match '%s'" handle person_name);
                    let thumb_url = Printf.sprintf "%s/people/%s/thumbnail"
                      base_url person_id in
                    begin try
                      let thumb_response = Requests.get http_session thumb_url in
                      if Requests.Response.ok thumb_response then begin
                        let thumb_data = Requests.Response.body thumb_response
                          |> Eio.Flow.read_all in
                        let filename = handle ^ ".jpg" in
                        let output_path = Filename.concat
                          (Eio.Path.native_exn data_dir) filename in
                        let oc = open_out_bin output_path in
                        output_string oc thumb_data;
                        close_out oc;
                        let updated = { contact with Contact.thumbnail = Some filename } in
                        Sortal_store.save store updated;
                        Logs.app (fun m -> m "  @%s: fetched face from Immich" handle);
                        incr fetched
                      end else begin
                        Logs.warn (fun m -> m "@%s: thumbnail download failed (HTTP %d)"
                          handle (Requests.Response.status_code thumb_response));
                        incr immich_errors
                      end
                    with exn ->
                      Logs.err (fun m -> m "@%s: thumbnail download error: %s"
                        handle (Printexc.to_string exn));
                      incr immich_errors
                    end
                end else begin
                  Logs.warn (fun m -> m "@%s: Immich search failed (HTTP %d)"
                    handle (Requests.Response.status_code response));
                  incr immich_errors
                end
              with exn ->
                Logs.err (fun m -> m "@%s: Immich request error: %s"
                  handle (Printexc.to_string exn));
                incr immich_errors
          in
          try_names names
        ) targets;
        Logs.app (fun m -> m "Immich face sync: %d fetched, %d skipped, %d not found, %d errors"
          !fetched !immich_skipped !not_found !immich_errors)
      with
      | () -> ()
      | exception Eio.Cancel.Cancelled _ -> ()
      | exception exn ->
        Logs.warn (fun m -> m "Immich sync error: %s" (Printexc.to_string exn));
        incr immich_errors
      end
    end
  end;
  (* ATProto DID resolution *)
  let atproto_errors = ref 0 in
  let atproto_targets = if force then
    List.filter (fun c -> Option.is_some (Contact.atproto c)) contacts
  else
    List.filter (fun c ->
      match Contact.atproto c with
      | Some a -> Option.is_none a.atp_did
      | None -> false
    ) contacts
  in
  if atproto_targets <> [] then begin
    Logs.app (fun m -> m "Resolving ATProto DIDs for %d contacts..."
      (List.length atproto_targets));
    Eio.Switch.run @@ fun sw ->
    let session = Requests.create ~sw env in
    List.iter (fun contact ->
      let handle = Contact.handle contact in
      match Contact.atproto contact with
      | None -> ()
      | Some atp ->
        match resolve_atproto_did session atp.atp_handle with
        | Some did ->
          let updated = Contact.set_atproto_did contact did in
          Sortal_store.save store updated;
          Logs.app (fun m -> m "  @%s: %s -> %s" handle atp.atp_handle did)
        | None ->
          Logs.warn (fun m -> m "  @%s: failed to resolve %s" handle atp.atp_handle);
          incr atproto_errors
    ) atproto_targets;
    Logs.app (fun m -> m "ATProto DID resolution: %d resolved, %d errors"
      (List.length atproto_targets - !atproto_errors) !atproto_errors)
  end;
  if !immich_errors > 0 || !atproto_errors > 0 then 1 else 0

(* Initialize git repository *)
let git_init_cmd xdg env =
  let store = Sortal_store.create_from_xdg xdg in
  let git_store = Sortal_git_store.create store env in
  match Sortal_git_store.init git_store with
  | Ok () ->
      if Sortal_git_store.is_initialized git_store then
        Logs.app (fun m -> m "Git repository initialized in data directory")
      else
        Logs.app (fun m -> m "Git repository already initialized");
      0
  | Error msg ->
      Logs.err (fun m -> m "Failed to initialize git repository: %s" msg);
      1

(* Add a new contact *)
let add_cmd handle names kind email github url orcid xdg env =
  let store = Sortal_store.create_from_xdg xdg in
  let git_store = Sortal_git_store.create store env in
  (* Check if contact already exists *)
  match Sortal_store.lookup store handle with
  | Some _ ->
      Logs.err (fun m -> m "Contact @%s already exists" handle);
      1
  | None ->
      let emails = match email with
        | Some e -> [Contact.make_email e]
        | None -> []
      in
      let services = match github with
        | Some gh -> [Contact.make_service ~kind:Contact.Github ~handle:gh (Printf.sprintf "https://github.com/%s" gh)]
        | None -> []
      in
      let urls = match url with
        | Some u -> [Contact.make_url u]
        | None -> []
      in
      let contact = Contact.make
        ~handle
        ~names
        ?kind
        ~emails
        ~services
        ~urls
        ?orcid
        ()
      in
      match Sortal_git_store.save git_store contact with
      | Ok () ->
          Logs.app (fun m -> m "Created contact @%s: %s" handle (Contact.name contact));
          0
      | Error msg ->
          Logs.err (fun m -> m "Failed to save contact: %s" msg);
          1

(* Delete a contact *)
let delete_cmd handle xdg env =
  let store = Sortal_store.create_from_xdg xdg in
  let git_store = Sortal_git_store.create store env in
  match Sortal_git_store.delete git_store handle with
  | Ok () ->
      Logs.app (fun m -> m "Deleted contact @%s" handle);
      0
  | Error msg ->
      Logs.err (fun m -> m "%s" msg);
      1

(* Convert string option to Ptime.date option *)
let parse_date_opt (s_opt : string option) : Sortal_schema.Temporal.date option =
  match s_opt with
  | None -> None
  | Some s ->
      match Sortal_schema.Temporal.parse_date_string s with
      | Some d -> Some d
      | None ->
          Logs.warn (fun m -> m "Invalid date format: %s (using ISO 8601: YYYY, YYYY-MM, or YYYY-MM-DD)" s);
          None

(* Add email to existing contact *)
let add_email_cmd handle address type_ from until note xdg env =
  let store = Sortal_store.create_from_xdg xdg in
  let git_store = Sortal_git_store.create store env in
  let from = parse_date_opt from in
  let until = parse_date_opt until in
  let email = Contact.make_email ?type_ ?from ?until ?note address in
  match Sortal_git_store.add_email git_store handle email with
  | Ok () ->
      Logs.app (fun m -> m "Added email %s to @%s" address handle);
      0
  | Error msg ->
      Logs.err (fun m -> m "%s" msg);
      1

(* Remove email from contact *)
let remove_email_cmd handle address xdg env =
  let store = Sortal_store.create_from_xdg xdg in
  let git_store = Sortal_git_store.create store env in
  match Sortal_git_store.remove_email git_store handle address with
  | Ok () ->
      Logs.app (fun m -> m "Removed email %s from @%s" address handle);
      0
  | Error msg ->
      Logs.err (fun m -> m "%s" msg);
      1

(* Add service to existing contact *)
let add_service_cmd handle url kind service_handle label xdg env =
  let store = Sortal_store.create_from_xdg xdg in
  let git_store = Sortal_git_store.create store env in
  let service = Contact.make_service ?kind ?handle:service_handle ?label url in
  match Sortal_git_store.add_service git_store handle service with
  | Ok () ->
      Logs.app (fun m -> m "Added service %s to @%s" url handle);
      0
  | Error msg ->
      Logs.err (fun m -> m "%s" msg);
      1

(* Remove service from contact *)
let remove_service_cmd handle url xdg env =
  let store = Sortal_store.create_from_xdg xdg in
  let git_store = Sortal_git_store.create store env in
  match Sortal_git_store.remove_service git_store handle url with
  | Ok () ->
      Logs.app (fun m -> m "Removed service %s from @%s" url handle);
      0
  | Error msg ->
      Logs.err (fun m -> m "%s" msg);
      1

(* Add organization to existing contact *)
let add_org_cmd handle org_name title department from until org_email org_url xdg env =
  let store = Sortal_store.create_from_xdg xdg in
  let git_store = Sortal_git_store.create store env in
  let from = parse_date_opt from in
  let until = parse_date_opt until in
  let org = Contact.make_org ?title ?department ?from ?until ?email:org_email ?url:org_url org_name in
  match Sortal_git_store.add_organization git_store handle org with
  | Ok () ->
      Logs.app (fun m -> m "Added organization %s to @%s" org_name handle);
      0
  | Error msg ->
      Logs.err (fun m -> m "%s" msg);
      1

(* Remove organization from contact *)
let remove_org_cmd handle org_name xdg env =
  let store = Sortal_store.create_from_xdg xdg in
  let git_store = Sortal_git_store.create store env in
  match Sortal_git_store.remove_organization git_store handle org_name with
  | Ok () ->
      Logs.app (fun m -> m "Removed organization %s from @%s" org_name handle);
      0
  | Error msg ->
      Logs.err (fun m -> m "%s" msg);
      1

(* Add URL to existing contact *)
let add_url_cmd handle url label xdg env =
  let store = Sortal_store.create_from_xdg xdg in
  let git_store = Sortal_git_store.create store env in
  let url_entry = Contact.make_url ?label url in
  match Sortal_git_store.add_url git_store handle url_entry with
  | Ok () ->
      Logs.app (fun m -> m "Added URL %s to @%s" url handle);
      0
  | Error msg ->
      Logs.err (fun m -> m "%s" msg);
      1

(* Remove URL from contact *)
let remove_url_cmd handle url xdg env =
  let store = Sortal_store.create_from_xdg xdg in
  let git_store = Sortal_git_store.create store env in
  match Sortal_git_store.remove_url git_store handle url with
  | Ok () ->
      Logs.app (fun m -> m "Removed URL %s from @%s" url handle);
      0
  | Error msg ->
      Logs.err (fun m -> m "%s" msg);
      1

(* Add ATProto identity to contact *)
let add_atproto_cmd handle atproto_handle svc_type svc_url xdg env =
  let store = Sortal_store.create_from_xdg xdg in
  let git_store = Sortal_git_store.create store env in
  let services = match svc_type, svc_url with
    | Some t, Some u ->
      [{ Contact.atp_type = Contact.atproto_service_type_of_string t;
         atp_url = u }]
    | _ -> []
  in
  let atp : Contact.atproto = {
    atp_handle = atproto_handle;
    atp_did = None;
    atp_services = services;
  } in
  match Sortal_git_store.update_contact git_store handle
    ~msg:(Printf.sprintf "Set ATProto identity %s on @%s" atproto_handle handle)
    (fun c -> { c with atproto = Some atp }) with
  | Ok () ->
      Logs.app (fun m -> m "Set ATProto identity %s on @%s" atproto_handle handle);
      0
  | Error msg ->
      Logs.err (fun m -> m "%s" msg);
      1

(* Add ATProto service to existing identity *)
let add_atproto_service_cmd handle svc_type svc_url xdg env =
  let store = Sortal_store.create_from_xdg xdg in
  let git_store = Sortal_git_store.create store env in
  match Sortal_store.lookup store handle with
  | None ->
      Logs.err (fun m -> m "Contact not found: %s" handle);
      1
  | Some contact ->
    match Contact.atproto contact with
    | None ->
        Logs.err (fun m -> m "No ATProto identity set for @%s. Use add-atproto first." handle);
        1
    | Some atp ->
      let svc : Contact.atproto_service = {
        atp_type = Contact.atproto_service_type_of_string svc_type;
        atp_url = svc_url;
      } in
      let updated_atp = { atp with atp_services = atp.atp_services @ [svc] } in
      match Sortal_git_store.update_contact git_store handle
        ~msg:(Printf.sprintf "Add ATProto service %s to @%s" svc_type handle)
        (fun c -> { c with atproto = Some updated_atp }) with
      | Ok () ->
          Logs.app (fun m -> m "Added ATProto service %s (%s) to @%s" svc_type svc_url handle);
          0
      | Error msg ->
          Logs.err (fun m -> m "%s" msg);
          1

(* Command info and args *)
let list_info = Cmd.info "list" ~doc:"List all contacts"
let show_info = Cmd.info "show" ~doc:"Show detailed information about a contact"
let thumbnail_info = Cmd.info "thumbnail" ~doc:"Print the thumbnail file path for a contact"
let search_info = Cmd.info "search" ~doc:"Search contacts by name"
let stats_info = Cmd.info "stats" ~doc:"Show statistics about the contact database"
let sync_info = Cmd.info "sync" ~doc:"Synchronize and normalize contact data"

let git_init_info = Cmd.info "git-init" ~doc:"Initialize git repository for contact versioning"
  ~man:[
    `S Manpage.s_description;
    `P "Initialize a git repository in the XDG data directory to track contact changes.";
    `P "Once initialized, all contact modifications will be automatically committed with descriptive messages.";
  ]

let add_info = Cmd.info "add" ~doc:"Create a new contact"
  ~man:[
    `S Manpage.s_description;
    `P "Create a new contact with the given handle and name.";
    `P "Additional metadata can be added using options or via add-email, add-service, etc. commands.";
  ]

let delete_info = Cmd.info "delete" ~doc:"Delete a contact"
let add_email_info = Cmd.info "add-email" ~doc:"Add an email address to a contact"
let remove_email_info = Cmd.info "remove-email" ~doc:"Remove an email address from a contact"
let add_service_info = Cmd.info "add-service" ~doc:"Add a service (GitHub, Twitter, etc.) to a contact"
let remove_service_info = Cmd.info "remove-service" ~doc:"Remove a service from a contact"
let add_org_info = Cmd.info "add-org" ~doc:"Add an organization/affiliation to a contact"
let remove_org_info = Cmd.info "remove-org" ~doc:"Remove an organization from a contact"
let add_url_info = Cmd.info "add-url" ~doc:"Add a URL to a contact"
let remove_url_info = Cmd.info "remove-url" ~doc:"Remove a URL from a contact"
let add_atproto_info = Cmd.info "add-atproto" ~doc:"Set AT Protocol identity on a contact"
let add_atproto_service_info = Cmd.info "add-atproto-service" ~doc:"Add a service to a contact's AT Protocol identity"

let handle_arg =
  Arg.(required & pos 0 (some string) None & info [] ~docv:"HANDLE"
    ~doc:"Contact handle to display")

let query_arg =
  Arg.(required & pos 0 (some string) None & info [] ~docv:"QUERY"
    ~doc:"Name or partial name to search for")

(* Add command arguments *)
let add_handle_arg =
  Arg.(required & pos 0 (some string) None & info [] ~docv:"HANDLE"
    ~doc:"Contact handle (unique identifier)")

let add_names_arg =
  Arg.(non_empty & opt_all string [] & info ["n"; "name"] ~docv:"NAME"
    ~doc:"Full name (can be specified multiple times for aliases)")

let add_kind_arg =
  let kind_conv =
    let parse s = match Contact.contact_kind_of_string s with
      | Some k -> Ok k
      | None -> Error (`Msg (Printf.sprintf "Invalid kind: %s" s))
    in
    let print ppf k = Format.pp_print_string ppf (Contact.contact_kind_to_string k) in
    Arg.conv (parse, print)
  in
  Arg.(value & opt (some kind_conv) None & info ["k"; "kind"] ~docv:"KIND"
    ~doc:"Contact kind (person, organization)")

let add_email_arg =
  Arg.(value & opt (some string) None & info ["e"; "email"] ~docv:"EMAIL"
    ~doc:"Email address")

let add_github_arg =
  Arg.(value & opt (some string) None & info ["g"; "github"] ~docv:"HANDLE"
    ~doc:"GitHub handle")

let add_url_arg =
  Arg.(value & opt (some string) None & info ["u"; "url"] ~docv:"URL"
    ~doc:"Personal/professional website URL")

let add_orcid_arg =
  Arg.(value & opt (some string) None & info ["orcid"] ~docv:"ORCID"
    ~doc:"ORCID identifier")

(* Add-email command arguments *)
let email_address_arg =
  Arg.(required & pos 1 (some string) None & info [] ~docv:"EMAIL"
    ~doc:"Email address")

let email_type_arg =
  let type_conv =
    let parse s = match Contact.email_type_of_string s with
      | Some t -> Ok t
      | None -> Error (`Msg (Printf.sprintf "Invalid email type: %s" s))
    in
    let print ppf t = Format.pp_print_string ppf (Contact.email_type_to_string t) in
    Arg.conv (parse, print)
  in
  Arg.(value & opt (some type_conv) None & info ["t"; "type"] ~docv:"TYPE"
    ~doc:"Email type (work, personal, other)")

let date_arg name =
  Arg.(value & opt (some string) None & info [name] ~docv:"DATE"
    ~doc:"ISO 8601 date (e.g., 2023, 2023-01, 2023-01-15)")

let note_arg =
  Arg.(value & opt (some string) None & info ["note"] ~docv:"NOTE"
    ~doc:"Contextual note")

(* Add-service command arguments *)
let service_url_arg =
  Arg.(required & pos 1 (some string) None & info [] ~docv:"URL"
    ~doc:"Service URL")

let service_kind_arg =
  let kind_conv =
    let parse s = match Contact.service_kind_of_string s with
      | Some k -> Ok k
      | None -> Error (`Msg (Printf.sprintf "Invalid service kind: %s" s))
    in
    let print ppf k = Format.pp_print_string ppf (Contact.service_kind_to_string k) in
    Arg.conv (parse, print)
  in
  Arg.(value & opt (some kind_conv) None & info ["k"; "kind"] ~docv:"KIND"
    ~doc:"Service kind (github, git, social, activitypub, photo)")

let service_handle_arg =
  Arg.(value & opt (some string) None & info ["handle"] ~docv:"HANDLE"
    ~doc:"Service handle/username")

let label_arg =
  Arg.(value & opt (some string) None & info ["l"; "label"] ~docv:"LABEL"
    ~doc:"Human-readable label")

(* Add-org command arguments *)
let org_name_arg =
  Arg.(required & pos 1 (some string) None & info [] ~docv:"ORG"
    ~doc:"Organization name")

let org_title_arg =
  Arg.(value & opt (some string) None & info ["title"] ~docv:"TITLE"
    ~doc:"Job title")

let org_department_arg =
  Arg.(value & opt (some string) None & info ["dept"; "department"] ~docv:"DEPT"
    ~doc:"Department")

let org_email_arg =
  Arg.(value & opt (some string) None & info ["email"] ~docv:"EMAIL"
    ~doc:"Work email during this period")

let org_url_arg =
  Arg.(value & opt (some string) None & info ["url"] ~docv:"URL"
    ~doc:"Work homepage during this period")

(* URL command arguments *)
let url_value_arg =
  Arg.(required & pos 1 (some string) None & info [] ~docv:"URL"
    ~doc:"URL")

(* ATProto command arguments *)
let atproto_handle_arg =
  Arg.(required & pos 1 (some string) None & info [] ~docv:"ATPROTO_HANDLE"
    ~doc:"AT Protocol handle (e.g., user.bsky.social)")

let atproto_svc_type_arg =
  Arg.(required & pos 1 (some string) None & info [] ~docv:"TYPE"
    ~doc:"Service type (bluesky, tangled, or custom)")

let atproto_svc_url_arg =
  Arg.(required & pos 2 (some string) None & info [] ~docv:"URL"
    ~doc:"Service URL")

let atproto_opt_service_type =
  Arg.(value & opt (some string) None & info ["service"] ~docv:"TYPE"
    ~doc:"Initial service type (bluesky, tangled, or custom)")

let atproto_opt_service_url =
  Arg.(value & opt (some string) None & info ["service-url"] ~docv:"URL"
    ~doc:"Initial service URL")
