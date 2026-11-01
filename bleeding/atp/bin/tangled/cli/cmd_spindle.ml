(* SPDX-License-Identifier: ISC *)
open Cmdliner
module Api = Tangled.Api

let spindle = Common.positional 0 "SPINDLE" "Spindle hostname or DID."
let subject = Common.positional 1 "USER" "User handle or DID."

let describe_cmd =
  let action spindle json =
    Common.run (fun env ->
        let d =
          Common.with_api ~authenticated:false ~busy:("Asking " ^ spindle) env
            (fun api ->
              Xrpc.Client.query
                (Api.public_client api ~service:(Api.spindle_url api spindle))
                ~nsid:"org.tangled.spindle.describeSpindle" ~params:[]
                ~decoder:Api.Lex.Spindle.DescribeSpindle.output_jsont)
        in
        if json || not (Ui.tty ()) then
          Common.print Api.Lex.Spindle.DescribeSpindle.output_jsont d
        else
          Ui.service ~kind:"Spindle" ~host:spindle ~did:d.did ~version:d.version
            ~capabilities:d.capabilities ~contact:(Common.email d.contact))
  in
  Common.cmd
    (Cmd.info "describe"
       ~doc:"Show a spindle's DID, version and capabilities. Pipes get JSON.")
    Term.(term_result (const action $ spindle $ Common.json_flag))

let members_cmd =
  let action spindle audience json =
    Common.run (fun env ->
        let rich = not (json || not (Ui.tty ())) in
        let list, accounts =
          Common.with_api ~busy:("Reading allow-list of " ^ spindle) env
            (fun api ->
              let list =
                Api.allow_list api
                  ~spindle:(Api.spindle_url api spindle)
                  ?audience ()
              in
              let dids =
                match Tangled.Schema.member "dids" list with
                | Some (Jsont.Array (dids, _)) ->
                    List.map Tangled.Schema.text dids
                | _ -> []
              in
              (list, if rich then Common.handles api dids else []))
        in
        if rich then Ui.accounts "Allowed owners" accounts
        else Common.json list)
  in
  Common.cmd
    (Cmd.info "list"
       ~doc:
         "List the owners whose repositories may run CI. Log in as a spindle \
          admin.")
    Term.(
      term_result (const action $ spindle $ Common.audience $ Common.json_flag))

let change name ~add =
  let action spindle subject audience =
    Common.run (fun env ->
        Common.with_api ~busy:"Updating allow-list" env (fun api ->
            Api.allow_list_change api
              ~spindle:(Api.spindle_url api spindle)
              ?audience ~add
              (Api.resolve_handle api subject));
        if add then Ui.ok "%s may now run CI on %s" subject spindle
        else Ui.ok "%s may no longer run CI on %s" subject spindle)
  in
  Common.cmd
    (Cmd.info name
       ~doc:
         (if add then "Admit a repository owner to the spindle."
          else "Withdraw an owner's admission."))
    Term.(term_result (const action $ spindle $ subject $ Common.audience))

let member_group =
  Cmd.group
    (Cmd.info "member"
       ~doc:"Manage whose repositories may run CI on a spindle.")
    [ members_cmd; change "add" ~add:true; change "remove" ~add:false ]

(* A ban names a repository or an account, by handle, repository name or DID. *)
let subject_did api subject =
  if String.starts_with ~prefix:"did:" subject then subject
  else if String.contains subject '/' then (Api.resolve_repo api subject).did
  else Api.resolve_handle api subject

let bans_cmd =
  let action spindle audience =
    Common.run (fun env ->
        Common.json
          (Common.with_api ~busy:"Reading bans" env (fun api ->
               Api.spindle_bans api
                 ~spindle:(Api.spindle_url api spindle)
                 ?audience ())))
  in
  Common.cmd
    (Cmd.info "bans"
       ~doc:"List the repositories and owners banned from a spindle.")
    Term.(term_result (const action $ spindle $ Common.audience))

let ban name ~ban =
  let subject =
    Common.positional 1 "SUBJECT"
      "Account handle, owner/name of a repository, or a DID."
  in
  let action spindle subject audience =
    Common.run (fun env ->
        Common.with_api ~busy:"Updating bans" env (fun api ->
            Api.spindle_ban api
              ~spindle:(Api.spindle_url api spindle)
              ?audience ~ban (subject_did api subject));
        if ban then Ui.ok "%s is banned from %s" subject spindle
        else Ui.ok "%s is no longer banned from %s" subject spindle)
  in
  Common.cmd
    (Cmd.info name
       ~doc:
         (if ban then
            "Ban a repository or owner, wiping its state on the spindle."
          else "Lift a ban."))
    Term.(term_result (const action $ spindle $ subject $ Common.audience))

let quota_list =
  let action spindle audience =
    Common.run (fun env ->
        Common.json
          (Common.with_api ~busy:"Reading quotas" env (fun api ->
               Api.quotas api
                 ~spindle:(Api.spindle_url api spindle)
                 ?audience ())))
  in
  Common.cmd
    (Cmd.info "list" ~doc:"List quota overrides.")
    Term.(term_result (const action $ spindle $ Common.audience))

let quota_usage =
  let who =
    Common.optional [ "for" ] "SUBJECT" "Only this repository or owner."
  in
  let action spindle audience who =
    Common.run (fun env ->
        Common.json
          (Common.with_api ~busy:"Reading usage" env (fun api ->
               Api.quota_usage api
                 ~spindle:(Api.spindle_url api spindle)
                 ?audience
                 ?did:(Option.map (subject_did api) who)
                 ())))
  in
  Common.cmd
    (Cmd.info "usage" ~doc:"Show resource usage by repository and owner.")
    Term.(term_result (const action $ spindle $ Common.audience $ who))

let resource =
  Common.positional 2 "RESOURCE"
    "Resource: workflows, vcpus, memory_mib, disk_mib or cache_storage_bytes."

let quota_subject =
  Common.positional 1 "SUBJECT"
    "Account handle, owner/name of a repository, or a DID."

let quota_set =
  let limit =
    Arg.(
      value
      & pos 3 (some string) None
      & info [] ~docv:"LIMIT"
          ~doc:"Maximum, in the resource's own units, or unlimited.")
  in
  let action spindle audience subject resource limit =
    Common.run (fun env ->
        let limit =
          match limit with
          | None | Some "unlimited" -> None
          | Some n -> (
              match int_of_string_opt n with
              | Some n when n > 0 -> Some n
              | _ -> failwith "LIMIT must be a positive number or unlimited")
        in
        Common.with_api ~busy:"Setting quota" env (fun api ->
            Api.set_quota api
              ~spindle:(Api.spindle_url api spindle)
              ?audience ~did:(subject_did api subject) ~resource limit);
        Ui.ok "Set %s for %s" resource subject)
  in
  Common.cmd
    (Cmd.info "set" ~doc:"Override a quota. Without LIMIT it is unlimited.")
    Term.(
      term_result
        (const action $ spindle $ Common.audience $ quota_subject $ resource
       $ limit))

let quota_unset =
  let action spindle audience subject resource =
    Common.run (fun env ->
        Common.with_api ~busy:"Removing quota" env (fun api ->
            Api.unset_quota api
              ~spindle:(Api.spindle_url api spindle)
              ?audience ~did:(subject_did api subject) ~resource ());
        Ui.ok "%s for %s is back to the default" resource subject)
  in
  Common.cmd
    (Cmd.info "unset" ~doc:"Remove a quota override.")
    Term.(
      term_result
        (const action $ spindle $ Common.audience $ quota_subject $ resource))

let quota_group =
  Cmd.group
    (Cmd.info "quota"
       ~doc:"Inspect usage and override quotas. Log in as a spindle admin.")
    [ quota_list; quota_usage; quota_set; quota_unset ]

let spindle_service api spindle = Api.spindle_url api spindle

let cmd =
  Cmd.group
    (Cmd.info "spindle" ~doc:"Inspect and administer a spindle, which runs CI.")
    [
      describe_cmd;
      member_group;
      Cmd_knot.invite_cmd ~kind:"spindle" ~service:spindle_service;
      Cmd_knot.join_cmd ~kind:"spindle" ~service:spindle_service;
      bans_cmd;
      ban "ban" ~ban:true;
      ban "unban" ~ban:false;
      quota_group;
    ]
