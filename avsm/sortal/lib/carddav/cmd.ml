(* Copyright (c) 2026 Anil Madhavapeddy. SPDX-License-Identifier: ISC *)
open Common
open Cmdliner

let required names doc =
  Arg.(required & opt (some string) None & info names ~docv:"VALUE" ~doc)

let optional names doc =
  Arg.(value & opt (some string) None & info names ~docv:"VALUE" ~doc)

let source = required [ "source" ] "Current Sortal root."

let bundle =
  required [ "bundle" ]
    "Export bundle retaining stable Sortal/CardDAV identities."

let output =
  required [ "output" ] "New private output directory outside the source."

let report =
  required [ "report" ] "New private report directory outside the source."

let username = required [ "username" ] "CardDAV account login."

let password_file =
  required [ "password-file" ] "File containing the account app password."

let server =
  Arg.(
    value
    & opt string Remote.default_server
    & info [ "server" ] ~docv:"URL"
        ~doc:"HTTPS CardDAV origin or discovery endpoint (default: Fastmail).")

let collection =
  optional [ "collection" ] "Full address-book URL when discovery is ambiguous."

let previous =
  optional [ "previous-pull" ]
    "Last applied pull journal for this account and collection."

let seed =
  optional [ "seed" ]
    "Verified seed report directory (defaults to BUNDLE/seed, or the legacy \
     BUNDLE/fastmail-seed)."

let dry_run =
  Arg.(
    value & flag
    & info [ "dry-run"; "n" ]
        ~doc:
          "Preview without changing source contacts, server contacts, or \
           existing journals.")

let apply =
  Arg.(
    value & flag
    & info [ "apply" ]
        ~doc:"Apply the explicitly selected seed or pull operation.")

let guard f =
  try
    f ();
    0
  with
  | Error e ->
      Printf.eprintf "CardDAV: %s\n%!" e;
      1
  | exn ->
      Printf.eprintf "CardDAV: %s\n%!" (Printexc.to_string exn);
      1

let with_dav ~server ~username ~password_file ~readonly f =
  let password = Remote.password password_file in
  Eio.Switch.run (fun sw ->
      let fetch =
        Fetch_curl.v ~sw ~timeout:(Duration.of_sec 40)
          ~max_response:Remote.limit ~user_agent:"Sortal-CardDAV/1" ()
      in
      f (Remote.make ~fetch ~root:server ~username ~password ~readonly))

let counts report =
  let c = get "upload_counts" report and p = get "pull" report in
  Printf.printf
    "Dry run: %d would create, %d unchanged, %d need review.\n\
     Pull: %d local updates proposed; %s.\n\
     %!"
    (number (get "create" c))
    (number (get "unchanged" c))
    (number (get "review" c))
    (number (get "local_updates" p))
    (field "status" p)

let cmd =
  let export =
    let term =
      let open Term.Syntax in
      let+ source = source
      and+ output = output
      and+ previous =
        optional [ "previous" ]
          "Previous export, required to preserve identities on re-export."
      and+ renames =
        Arg.(
          value & opt_all string []
          & info [ "rename" ] ~docv:"OLD=NEW"
              ~doc:"Retain a UID across a handle rename.")
      and+ as_of =
        optional [ "as-of" ]
          "Reference date recorded in the manifest; all affiliation history is \
           exported."
      and+ version =
        Arg.(
          value
          & opt (enum [ ("3.0", "3.0"); ("4.0", "4.0") ]) "3.0"
          & info [ "vcard-version" ]
              ~doc:"vCard version (3.0 for maximum compatibility).")
      in
      guard (fun () ->
          let m =
            Bundle.export ?previous ~renames ?as_of ~version ~source ~output ()
          in
          Printf.printf
            "Verified %d vCards and %d original files; no network operations.\n\
             %!"
            (List.length (items "contacts" m))
            (List.length (assoc (get "files" m))))
    in
    Cmd.v
      (Cmd.info "export" ~doc:"Export and verify a lossless local vCard bundle.")
      term
  in
  let verify =
    let term =
      let open Term.Syntax in
      let+ bundle = bundle
      and+ source =
        optional [ "source" ]
          "Also compare every original file with the current Sortal root."
      in
      guard (fun () ->
          let m = Bundle.verify ?source bundle in
          Printf.printf "Verified %d vCards and %d original files.\n%!"
            (List.length (items "contacts" m))
            (List.length (assoc (get "files" m))))
    in
    Cmd.v
      (Cmd.info "verify"
         ~doc:"Verify checksums, field recovery and original photo bytes.")
      term
  in
  let sync =
    let term =
      let open Term.Syntax in
      let+ source = source
      and+ bundle = bundle
      and+ output = report
      and+ server = server
      and+ username = username
      and+ password_file = password_file
      and+ collection = collection
      and+ previous = previous
      and+ seed = seed
      and+ _dry_run = dry_run in
      guard (fun () ->
          with_dav ~server ~username ~password_file ~readonly:true (fun dav ->
              counts
                (Sync.preview ?collection ?previous ?seed ~dav ~source ~bundle
                   ~username ~output ());
              Printf.printf "Report: %s/report.md\n%!" output))
    in
    Cmd.v
      (Cmd.info "sync"
         ~doc:"Preview current Sortal contacts against a CardDAV server."
         ~man:
           [
             `S Manpage.s_description;
             `P
               "Dry-run is the default and only combined sync mode. It exports \
                current source edits, reads the server and proposes uploads \
                and supported pull merges. Applying an initial seed or \
                prepared pull is a separate explicit operation. General \
                updates of existing server cards and deletion propagation \
                require reconciliation.";
           ])
      term
  in
  let seed_cmd =
    let term =
      let open Term.Syntax in
      let+ bundle = bundle
      and+ report = report
      and+ server = server
      and+ username = username
      and+ password_file = password_file
      and+ collection = collection
      and+ dry_run = dry_run
      and+ apply = apply in
      guard (fun () ->
          if dry_run && apply then
            fail "--dry-run and --apply cannot be combined";
          with_dav ~server ~username ~password_file ~readonly:(not apply)
            (fun dav ->
              let r =
                Remote.inspect ?collection ~apply ~dav ~bundle ~username ~report
                  ()
              in
              Printf.printf
                "%s %d contacts; %d upload results. Report: %s/report.json\n%!"
                (if apply then "Inspected and seeded" else "Previewed")
                (List.length (items "plan" r))
                (List.length (items "results" r))
                report))
    in
    Cmd.v
      (Cmd.info "seed"
         ~doc:
           "Inspect or explicitly seed new contacts; hold possible duplicates \
            for review.")
      term
  in
  let prepare =
    let term =
      let open Term.Syntax in
      let+ bundle = bundle
      and+ source = source
      and+ output = output
      and+ previous = previous
      and+ seed = seed
      and+ snapshot =
        required [ "snapshot" ]
          "Server inspection report containing the fetched cards."
      in
      guard (fun () ->
          let r =
            Pull.prepare ?previous ?seed ~bundle ~snapshot ~source ~output ()
          in
          Printf.printf
            "Prepared %d remote changes. Journal: %s/report.json\n%!"
            (List.length (items "changes" r))
            output)
    in
    Cmd.v
      (Cmd.info "prepare"
         ~doc:"Prepare a journal for conservative remote-to-Sortal edits.")
      term
  in
  let apply_pull =
    let term =
      let open Term.Syntax in
      let+ journal = required [ "journal" ] "Prepared pull journal directory."
      and+ server =
        optional [ "server" ]
          "HTTPS CardDAV origin (defaults to the journal's collection)."
      and+ username = username
      and+ password_file = password_file
      and+ dry_run = dry_run
      and+ apply = apply in
      guard (fun () ->
          if dry_run && apply then
            fail "--dry-run and --apply cannot be combined";
          let r = load_json (Filename.concat journal "report.json") in
          let server =
            Option.value ~default:(field "href" (get "book" r)) server
          in
          with_dav ~server ~username ~password_file ~readonly:true (fun dav ->
              let result =
                Pull.apply ~dav ~dry_run:(not apply) ~username journal
              in
              Printf.printf "%s %d local updates; no server writes.\n%!"
                (if apply then "Applied" else "Would apply")
                (List.length (items "local_updates" result))))
    in
    Cmd.v
      (Cmd.info "apply"
         ~doc:
           "Validate a pull journal; --apply explicitly writes local changes.")
      term
  in
  Cmd.group
    (Cmd.info "carddav"
       ~doc:"Lossless vCard export and conservative CardDAV synchronization.")
    [
      export;
      verify;
      sync;
      seed_cmd;
      Cmd.group
        (Cmd.info "pull" ~doc:"Prepare and apply supported remote edits.")
        [ prepare; apply_pull ];
    ]
