(* Copyright (c) 2026 Anil Madhavapeddy. SPDX-License-Identifier: ISC *)
module Config = Config
open Termanil_model
module C = Dooit.Common

let require what = function Some x -> x | None -> C.fail "configure %s" what

let protect f =
  try Ok (f ()) with
  | C.Error s | Sortal_carddav.Common.Error s | Failure s -> Error (safe_text s)
  | Unix.Unix_error (e, _, _) -> Error (Unix.error_message e)
  | Sys_error _ -> Error "Cannot access configured local files"
  | Invalid_argument _ -> Error "Invalid backend data or configuration"
  | Yamlrw.Yamlrw_error _ -> Error "Invalid YAML; original file retained"
  | exn ->
      (* Network exceptions can embed headers and response bodies. *)
      ignore exn;
      Error "Backend operation failed; refresh before retrying"

let with_mail env sw (config : Config.t) source f =
  match config.demo_root with
  | Some root ->
      let client = Termanil_fake_jmap.connect ~sw ~root in
      f
        (Termanil_mail.create ~client ~service:Termanil_fake_jmap.service
           ~account:Termanil_fake_jmap.account)
  | None ->
      let profile_name = require "mail.profile" config.profile in
      let store =
        match Jmap_eio.Profile.xdg_store env with
        | Ok s -> s
        | Error _ -> C.fail "cannot locate JMAP profile store"
      in
      let profile =
        match Jmap_eio.Profile.load store profile_name with
        | Ok p -> p
        | Error _ -> C.fail "cannot load JMAP profile %s" profile_name
      in
      let service =
        Dooit.Link.service
          (Option.value config.service
             ~default:(Jmap_eio.Profile.session_url profile))
      in
      Option.iter
        (fun (s : email_ref) ->
          if s.service <> service then
            C.fail "message belongs to another JMAP service";
          Option.iter
            (fun a ->
              if a <> s.account then
                C.fail "message belongs to another configured account")
            config.account)
        source;
      let client =
        match Jmap_eio.Profile.connect ~sw env profile with
        | Ok c -> c
        | Error _ -> C.fail "cannot connect to JMAP profile %s" profile_name
      in
      let account =
        match (config.account, source) with
        | Some a, _ -> a
        | None, Some s -> s.account
        | None, None ->
            Jmap.Proto.Session.primary_account_for Jmap.Proto.Capability.mail
              (Jmap_eio.Client.session client)
            |> Option.map Jmap.Proto.Id.to_string
            |> require "mail.account"
      in
      f (Termanil_mail.create ~client ~service ~account)

let contacts sw (config : Config.t) =
  let read optional f =
    match optional with
    | None -> ([], [])
    | Some x -> (
        match protect (fun () -> f x) with Ok r -> r | Error s -> ([], [ s ]))
  in
  let local_root =
    match config.vcard_root with
    | Some _ as root -> root
    | None -> config.sortal_root
  in
  let local, warnings = read local_root Termanil_contacts.local in
  let remote, remote_warnings =
    read config.carddav (fun (c : Config.carddav) ->
        let secret : Dooit.Config.webdav =
          {
            url = c.url;
            collection = "";
            subdir = "";
            username = c.username;
            secret = File c.password_file;
          }
        in
        let fetch =
          Fetch_curl.v ~sw ~timeout:(Duration.of_sec 60)
            ~max_response:(64 * 1024 * 1024)
            ()
        in
        let remote =
          Sortal_carddav.Remote.make ~fetch ~root:c.url ~username:c.username
            ~password:(Dooit.Config.password secret)
            ~readonly:true
        in
        (Termanil_contacts.remote ?collection:c.collection remote, []))
  in
  let warnings = warnings @ remote_warnings in
  let warnings =
    if
      config.sortal_root = None && config.vcard_root = None
      && config.carddav = None
    then [ "Configure contacts.vcard_root or contacts.carddav" ]
    else warnings
  in
  Contacts_loaded (Termanil_contacts.combine local remote, warnings)

let execute config request =
  protect (fun () ->
      Eio_main.run (fun env ->
          Eio.Time.with_timeout_exn env#clock 60. (fun () ->
              Eio.Switch.run (fun sw ->
                  let mail ?source f = with_mail env sw config source f in
                  let tasks () =
                    match config.Config.demo_root with
                    | Some root ->
                        Dooit.Config.parse
                          ~path:(Filename.concat root "config.toml")
                          ~default_root:(Filename.concat root "tasks")
                          ""
                    | None ->
                        Dooit.Config.load ~fs:env#fs
                          ?path:config.Config.dooit_config
                          ?root:config.dooit_root ()
                  in
                  let task_list () =
                    let c = tasks () in
                    if not (C.exists c.root) then ([], [])
                    else Termanil_tasks.list (Dooit.Store.open_ c.root)
                  in
                  let replies = Config.replies config in
                  (match config.demo_root with
                  | Some _ ->
                      let c = tasks () in
                      if not (C.exists c.root) then (
                        let store = Dooit.Store.init c.root in
                        List.iter
                          (fun m -> ignore (Termanil_tasks.capture store m))
                          [
                            List.hd Termanil_demo.messages;
                            List.nth Termanil_demo.messages 5;
                          ];
                        let m = List.nth Termanil_demo.messages 1 in
                        ignore
                          (Termanil_drafts.save replies ~source:m.source
                             ~thread_id:m.thread_id
                             ~subject:("Re: " ^ m.subject)
                             ~recipients:m.addresses
                             ~body:
                               "Hi Grace,\n\n\
                                Noon by the library works for me. See you \
                                Friday!\n"
                             ~expected:None))
                  | None -> ());
                  match request with
                  | Verify_replies ->
                      let ds, lines =
                        Termanil_drafts.verify replies ~lookup:(fun source ->
                            mail ~source (fun c ->
                                Termanil_mail.submission_for_email c source))
                      in
                      Replies_sent (ds, lines)
                  | Workspace ->
                      let ns, warnings =
                        match protect task_list with
                        | Ok x -> x
                        | Error s -> ([], [ s ])
                      in
                      let ds, more = Termanil_drafts.list replies in
                      Workspace_loaded (ns, ds, warnings @ more)
                  | Conversation source ->
                      mail ~source (fun c ->
                          Conversation_loaded
                            ( source,
                              Termanil_mail.conversation c source,
                              protect (fun () ->
                                  Termanil_mail.signature c
                                    ~identity:config.identity
                                    ~override:config.signature) ))
                  | Save_reply { source; body; expected } ->
                      mail ~source (fun c ->
                          let thread_id, subject, recipients =
                            Termanil_mail.reply_target c source
                          in
                          Reply_saved
                            (Termanil_drafts.save replies ~source ~thread_id
                               ~subject ~recipients ~body ~expected))
                  | Queue_reply (d, ready) ->
                      Reply_saved (Termanil_drafts.queue replies d ready)
                  | Send_replies ds ->
                      let sent, lines =
                        Termanil_drafts.send replies ds
                          ~prepare:(fun (d : draft) ->
                            mail ~source:d.source (fun c ->
                                Termanil_mail.prepare_reply c
                                  ~identity:config.identity d))
                      in
                      Replies_sent (sent, lines)
                  | Contacts when config.demo_root <> None ->
                      Contacts_loaded (Termanil_demo.contacts, [])
                  | Sync_tasks _ when config.demo_root <> None ->
                      Synced
                        [
                          "Demo tasks are local. No WebDAV connection is made.";
                        ]
                  | Mailboxes ->
                      mail (fun c ->
                          Mailboxes_loaded (Termanil_mail.mailboxes c))
                  | Messages { mailbox; query; position } ->
                      mail (fun c ->
                          Messages_loaded
                            (Termanil_mail.messages c ~mailbox ~query ~position))
                  | Read source ->
                      mail ~source (fun c ->
                          let m, body = Termanil_mail.read c source in
                          Message_loaded (m, body))
                  | Archive source ->
                      mail ~source (fun c ->
                          Termanil_mail.archive c source;
                          Archived source)
                  | Set_seen (source, value) ->
                      mail ~source (fun c ->
                          Termanil_mail.set_keyword c source `Seen value;
                          Message_changed (source, `Seen, value))
                  | Set_flagged (source, value) ->
                      mail ~source (fun c ->
                          Termanil_mail.set_keyword c source `Flagged value;
                          Message_changed (source, `Flagged, value))
                  | Contacts -> contacts sw config
                  | Tasks ->
                      let c = tasks () in
                      if not (C.exists c.root) then
                        Tasks_loaded
                          ( [],
                            [
                              "Store not initialized. Capture mail or run \
                               dooit init.";
                            ] )
                      else
                        let ns, warnings =
                          Termanil_tasks.list (Dooit.Store.open_ c.root)
                        in
                        Tasks_loaded (ns, warnings)
                  | Capture m ->
                      let c = tasks () in
                      let store = Dooit.Store.init c.root in
                      Captured
                        (Termanil_tasks.capture ~tags:config.capture_tags store
                           m)
                  | Complete n ->
                      let c = tasks () in
                      Completed
                        (Termanil_tasks.complete (Dooit.Store.open_ c.root) n)
                  | Sync_tasks { dry_run } ->
                      let c = tasks () in
                      let store = Dooit.Store.open_ c.root in
                      let fetch =
                        Fetch_curl.v ~sw ~timeout:(Duration.of_sec 60)
                          ~max_response:(16 * 1024 * 1024)
                          ()
                      in
                      let remote =
                        Dooit.Remote.make ~fetch
                          ~config:(Dooit.Config.require_webdav c)
                          ~readonly:dry_run
                      in
                      let lines = Termanil_tasks.sync ~dry_run store remote in
                      Synced
                        ((if dry_run then "Dry run: no writes"
                          else "Dooit sync results")
                        :: lines)))))
