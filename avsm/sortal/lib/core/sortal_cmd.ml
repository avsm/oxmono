(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

open Cmdliner

module Contact = Sortal_schema.Contact
module Temporal = Sortal_schema.Temporal

let is_png path =
  let ext = String.lowercase_ascii (Filename.extension path) in
  ext = ".png"

let convert_to_png src_path =
  let base = Filename.remove_extension src_path in
  let dst_path = base ^ ".png" in
  let cmd = Printf.sprintf "magick %s %s" (Filename.quote src_path) (Filename.quote dst_path) in
  let ret = Unix.system cmd in
  match ret with
  | Unix.WEXITED 0 -> Ok dst_path
  | Unix.WEXITED n -> Error (Printf.sprintf "magick exited with code %d" n)
  | Unix.WSIGNALED n -> Error (Printf.sprintf "magick killed by signal %d" n)
  | Unix.WSTOPPED n -> Error (Printf.sprintf "magick stopped by signal %d" n)

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

let sync_cmd () xdg =
  let store = Sortal_store.create_from_xdg xdg in
  let contacts = Sortal_store.list store in
  Logs.app (fun m -> m "Syncing %d contacts..." (List.length contacts));
  let converted = ref 0 in
  let skipped = ref 0 in
  let no_thumbnail = ref 0 in
  let errors = ref 0 in
  List.iter (fun contact ->
    let handle = Contact.handle contact in
    match Sortal_store.thumbnail_path store contact with
    | None ->
      Logs.info (fun m -> m "@%s: no thumbnail" handle);
      incr no_thumbnail
    | Some eio_path ->
      let path = Eio.Path.native_exn eio_path in
      if is_png path then begin
        Logs.info (fun m -> m "@%s: already PNG (%s)" handle (Filename.basename path));
        incr skipped
      end else begin
        (* Check if PNG version already exists *)
        let png_path = Filename.remove_extension path ^ ".png" in
        if Sys.file_exists png_path then begin
          Logs.info (fun m -> m "@%s: PNG already exists (%s)" handle (Filename.basename png_path));
          incr skipped
        end else begin
          Logs.app (fun m -> m "@%s: converting %s to PNG..." handle (Filename.basename path));
          match convert_to_png path with
          | Ok new_path ->
            Logs.app (fun m -> m "  Converted: %s -> %s"
              (Filename.basename path) (Filename.basename new_path));
            incr converted
          | Error msg ->
            Logs.err (fun m -> m "  Failed to convert %s: %s" path msg);
            incr errors
        end
      end
  ) contacts;
  Logs.app (fun m -> m "Sync complete:");
  Logs.app (fun m -> m "  %d contacts without thumbnails" !no_thumbnail);
  Logs.app (fun m -> m "  %d already PNG (skipped)" !skipped);
  Logs.app (fun m -> m "  %d converted to PNG" !converted);
  Logs.app (fun m -> m "  %d errors" !errors);
  if !errors > 0 then 1 else 0

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
    ~doc:"Contact kind (person, organization, group, role)")

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
