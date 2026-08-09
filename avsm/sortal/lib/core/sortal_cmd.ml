(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

open Cmdliner

module Contact = Sortal_schema.Contact
module Platform = Sortal_schema.Platform

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
      Option.iter (fun e -> Logs.app (fun m -> m "  Email: %s" e))
        (List.nth_opt (Contact.emails c) 0);
      Option.iter (fun u -> Logs.app (fun m -> m "  URL: %s" u)) (Contact.best_url c)
    ) matches;
    0

let stats_cmd () xdg =
  let store = Sortal_store.create_from_xdg xdg in
  let contacts = Sortal_store.list store in
  let total = List.length contacts in
  let count pred = List.filter pred contacts |> List.length in
  let with_email = count (fun c -> Contact.emails c <> []) in
  let with_org = count (fun c -> Contact.affiliations c <> []) in
  let with_url = count (fun c -> Contact.links c <> []) in
  let with_service = count (fun c -> Contact.accounts c <> []) in
  let with_orcid = count (fun c -> Option.is_some (Contact.handle_on c (Simple Orcid))) in
  let with_feeds = count (fun c -> Contact.feeds c <> []) in
  let total_feeds =
    List.fold_left (fun acc c -> acc + List.length (Contact.feeds c)) 0 contacts
  in
  let total_services =
    List.fold_left (fun acc c ->
      acc + List.length (Contact.accounts c)
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

let with_photo contact photo =
  Contact.make
    ~handle:(Contact.handle contact)
    ~names:(Contact.names contact)
    ~kind:(Contact.kind contact)
    ~emails:(Contact.emails contact)
    ~accounts:(Contact.accounts contact)
    ~links:(Contact.links contact)
    ~affiliations:(Contact.affiliations contact)
    ?photo
    ~feeds:(Contact.feeds contact)
    ~vcard:(Contact.vcard contact)
    ()

(* DID resolution *)
let resolve_atproto_did http_session atp_handle =
  let url = Printf.sprintf
    "https://bsky.social/xrpc/com.atproto.identity.resolveHandle?handle=%s"
    (Uri.pct_encode atp_handle) in
  try
    Logs.info (fun m -> m "Resolving ATProto handle: %s" atp_handle);
    Fetch.with_response http_session `GET url @@ fun response ->
    let status = Fetch.status response in
    if status >= 200 && status < 300 then begin
      let body = Fetch.body response |> Eio.Flow.read_all in
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
        atp_handle status);
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
        Option.is_none (Contact.photo c)
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
                let status, body =
                  Fetch.with_response http_session `GET url @@ fun response ->
                  (Fetch.status response, Eio.Flow.read_all (Fetch.body response))
                in
                if status >= 200 && status < 300 then begin
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
                      let thumb_status, thumb_data =
                        Fetch.with_response http_session `GET thumb_url
                        @@ fun thumb_response ->
                        ( Fetch.status thumb_response,
                          Eio.Flow.read_all (Fetch.body thumb_response) )
                      in
                      if thumb_status >= 200 && thumb_status < 300 then begin
                        let filename = handle ^ ".jpg" in
                        let output_path = Filename.concat
                          (Eio.Path.native_exn data_dir) filename in
                        let oc = open_out_bin output_path in
                        output_string oc thumb_data;
                        close_out oc;
                        let updated = with_photo contact (Some filename) in
                        Sortal_store.save store updated;
                        Logs.app (fun m -> m "  @%s: fetched face from Immich" handle);
                        incr fetched
                      end else begin
                        Logs.warn (fun m -> m "@%s: thumbnail download failed (HTTP %d)"
                          handle thumb_status);
                        incr immich_errors
                      end
                    with exn ->
                      Logs.err (fun m -> m "@%s: thumbnail download error: %s"
                        handle (Printexc.to_string exn));
                      incr immich_errors
                    end
                end else begin
                  Logs.warn (fun m -> m "@%s: Immich search failed (HTTP %d)"
                    handle status);
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
      | Some a -> Option.is_none a.did
      | None -> false
    ) contacts
  in
  if atproto_targets <> [] then begin
    Logs.app (fun m -> m "Resolving ATProto DIDs for %d contacts..."
      (List.length atproto_targets));
    Eio.Switch.run @@ fun sw ->
    let session = Fetch_curl.std ~sw env in
    List.iter (fun contact ->
      let handle = Contact.handle contact in
      match Contact.atproto contact with
      | None -> ()
      | Some atp ->
        match resolve_atproto_did session atp.handle with
        | Some did ->
          let updated = Contact.set_atproto_did contact did in
          Sortal_store.save store updated;
          Logs.app (fun m -> m "  @%s: %s -> %s" handle atp.handle did)
        | None ->
          Logs.warn (fun m -> m "  @%s: failed to resolve %s" handle atp.handle);
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
      let emails = match email with Some e -> [ e ] | None -> [] in
      let accounts =
        (match github with
        | Some gh -> [ Contact.Account.Simple (Platform.Github, gh) ]
        | None -> [])
        @
        match orcid with
        | Some o -> [ Contact.Account.Simple (Platform.Orcid, o) ]
        | None -> []
      in
      let links = match url with
        | Some u -> [ { Contact.url = u; label = None } ]
        | None -> []
      in
      let contact = Contact.make ~handle ~names ?kind ~emails ~accounts ~links () in
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

(* The error message for an unknown platform key names every known one. *)
let unknown_platform key =
  Fmt.epr "unknown platform %S@.known platforms: %s@." key
    (String.concat ", " (List.map Platform.key Platform.all));
  exit 1

(* [value] as an account on [platform]. A federated platform expects
   [user@host]; anything else is a syntax error the caller reports. *)
let account_of_value platform value =
  match (platform : Platform.id) with
  | Simple p -> Ok (Contact.Account.Simple (p, value))
  | Atproto -> Ok (Contact.Account.Atproto { handle = value; did = None; apps = [] })
  | Federated p -> (
      match String.index_opt value '@' with
      | Some i ->
          let user = String.sub value 0 i in
          let host = String.sub value (i + 1) (String.length value - i - 1) in
          Ok (Contact.Account.Federated (p, user, host))
      | None ->
          Error
            (Printf.sprintf "%S is not user@host, required for %s" value
               (Platform.key platform)))

(* Set a contact's account on a platform *)
let set_cmd handle platform_key value xdg env =
  match Platform.of_key platform_key with
  | None -> unknown_platform platform_key
  | Some platform -> (
      match account_of_value platform value with
      | Error msg ->
          Logs.err (fun m -> m "%s" msg);
          1
      | Ok account -> (
          let store = Sortal_store.create_from_xdg xdg in
          let git_store = Sortal_git_store.create store env in
          match Sortal_git_store.set_account git_store handle account with
          | Ok () ->
              Logs.app (fun m ->
                  m "Set %s account %s on @%s" platform_key value handle);
              0
          | Error msg ->
              Logs.err (fun m -> m "%s" msg);
              1))

(* Remove a contact's account on a platform *)
let unset_cmd handle platform_key xdg env =
  match Platform.of_key platform_key with
  | None -> unknown_platform platform_key
  | Some platform ->
      let store = Sortal_store.create_from_xdg xdg in
      let git_store = Sortal_git_store.create store env in
      (match Sortal_git_store.unset_account git_store handle platform with
      | Ok () ->
          Logs.app (fun m -> m "Removed %s account from @%s" platform_key handle);
          0
      | Error msg ->
          Logs.err (fun m -> m "%s" msg);
          1)

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
let set_info = Cmd.info "set" ~doc:"Set a contact's account on a platform"
let unset_info = Cmd.info "unset" ~doc:"Remove a contact's account on a platform"

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

let kind_of_string = function
  | "person" -> Some Contact.Person
  | "organization" | "org" -> Some Contact.Organization
  | _ -> None

let kind_to_string = function
  | Contact.Person -> "person"
  | Contact.Organization -> "organization"

let add_kind_arg =
  let kind_conv =
    let parse s = match kind_of_string s with
      | Some k -> Ok k
      | None -> Error (`Msg (Printf.sprintf "Invalid kind: %s" s))
    in
    let print ppf k = Format.pp_print_string ppf (kind_to_string k) in
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

(* Set and unset command arguments *)
let platform_arg =
  Arg.(required & pos 1 (some string) None & info [] ~docv:"PLATFORM"
    ~doc:"The platform key, such as $(b,github).")

let value_arg =
  Arg.(required & pos 2 (some string) None & info [] ~docv:"HANDLE"
    ~doc:"The handle on that platform.")
