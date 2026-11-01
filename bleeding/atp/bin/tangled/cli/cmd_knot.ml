(* SPDX-License-Identifier: ISC *)
open Cmdliner
module Api = Tangled.Api

let knot = Common.positional 0 "KNOT" "Knot hostname or HTTP(S) origin."

let describe_cmd =
  let action knot json =
    Common.run (fun env ->
        let d =
          Common.with_api ~authenticated:false ~busy:("Asking " ^ knot) env
            (fun api ->
              Xrpc.Client.query
                (Api.public_client api ~service:knot)
                ~nsid:"org.tangled.knot.describeKnot" ~params:[]
                ~decoder:Api.Lex.Knot.DescribeKnot.output_jsont)
        in
        if json || not (Ui.tty ()) then
          Common.print Api.Lex.Knot.DescribeKnot.output_jsont d
        else
          Ui.service ~kind:"Knot" ~host:knot ~did:d.did ~version:d.version
            ~capabilities:d.capabilities ~contact:(Common.email d.contact))
  in
  Common.cmd
    (Cmd.info "describe"
       ~doc:"Show a knot's DID, version and capabilities. Pipes get JSON.")
    Term.(term_result (const action $ knot $ Common.json_flag))

let members_cmd =
  let action knot =
    Common.run (fun env ->
        let tty = Ui.tty () in
        let members =
          Common.with_api ~busy:("Reading members of " ^ knot) env (fun api ->
              let dids = Api.knot_members api ~knot in
              if tty then Common.handles api dids
              else List.map (fun d -> (d, None)) dids)
        in
        if tty then Ui.accounts "Members" members
        else List.iter (fun (d, _) -> print_endline d) members)
  in
  Common.cmd
    (Cmd.info "list" ~doc:"List the accounts that may host repositories.")
    Term.(term_result (const action $ knot))

let user = Common.positional 1 "USER" "Handle or DID."

let member name ~add =
  let action knot user audience =
    Common.run (fun env ->
        let who = user in
        Common.with_api ~busy:"Updating membership" env (fun api ->
            let user = Api.resolve_handle api user in
            if add then Api.add_knot_member api ~knot ?audience user
            else Api.remove_knot_member api ~knot ?audience user);
        if add then Ui.ok "%s is now a member of %s" who knot
        else Ui.ok "%s is no longer a member of %s" who knot)
  in
  Common.cmd
    (Cmd.info name
       ~doc:
         (if add then "Let a user host repositories. Log in as a knot admin."
          else "Remove a member. Log in as a knot admin."))
    Term.(term_result (const action $ knot $ user $ Common.audience))

let member_group =
  Cmd.group
    (Cmd.info "member" ~doc:"Manage who may host repositories on a knot.")
    [ members_cmd; member "add" ~add:true; member "remove" ~add:false ]

let bans_cmd =
  let action knot =
    Common.run (fun env ->
        let tty = Ui.tty () in
        let banned =
          Common.with_api ~busy:("Reading bans of " ^ knot) env (fun api ->
              let dids = Api.knot_bans api ~knot in
              if tty then Common.handles api dids
              else List.map (fun d -> (d, None)) dids)
        in
        if tty then Ui.accounts "Banned" banned
        else List.iter (fun (d, _) -> print_endline d) banned)
  in
  Common.cmd
    (Cmd.info "bans" ~doc:"List the accounts banned from a knot.")
    Term.(term_result (const action $ knot))

let ban name ~ban =
  let action knot user audience =
    Common.run (fun env ->
        let who = user in
        Common.with_api ~busy:"Updating bans" env (fun api ->
            let user = Api.resolve_handle api user in
            if ban then Api.ban_from_knot api ~knot ?audience user
            else Api.unban_from_knot api ~knot ?audience user);
        if ban then Ui.ok "%s is banned from %s" who knot
        else Ui.ok "%s is no longer banned from %s" who knot)
  in
  Common.cmd
    (Cmd.info name
       ~doc:
         (if ban then "Ban an account from a knot. Log in as a knot admin."
          else "Lift a ban. Log in as a knot admin."))
    Term.(term_result (const action $ knot $ user $ Common.audience))

(* Invite codes admit members to knots and spindles alike. *)
let invite_cmd ~kind ~service =
  let uses =
    Arg.(
      value & opt int 1
      & info [ "uses" ] ~docv:"N" ~doc:"How many accounts the code admits.")
  in
  let for_ =
    Common.optional [ "for" ] "USER" "Admit only USER, a handle or DID."
  in
  let action service_name audience uses for_ =
    Common.run (fun env ->
        let code =
          Common.with_api ~busy:"Creating invite" env (fun api ->
              let for_account = Option.map (Api.resolve_handle api) for_ in
              Api.invite_code api ~service:(service api service_name) ?audience
                ~uses ?for_account ())
        in
        if Ui.tty () then
          Ui.print
            (Ui.V.vcat
               [
                 Ui.V.concat
                   [
                     Ui.text "Invite code for ";
                     Ui.bold service_name;
                     Ui.text ":";
                   ];
                 Ui.title code;
                 Ui.muted
                   (Printf.sprintf "Redeem it with: tangled %s join %s %s" kind
                      service_name code);
               ])
        else print_endline code)
  in
  Common.cmd
    (Cmd.info "invite"
       ~doc:
         (Printf.sprintf "Create an invite code. Log in as a %s admin." kind))
    Term.(
      term_result
        (const action
        $ Common.positional 0
            (String.uppercase_ascii kind)
            (Printf.sprintf "%s hostname." (String.capitalize_ascii kind))
        $ Common.audience $ uses $ for_))

let join_cmd ~kind ~service =
  let code = Common.positional 1 "CODE" "Invite code." in
  let action service_name audience code =
    Common.run (fun env ->
        Common.with_api ~busy:("Joining " ^ service_name) env (fun api ->
            Api.redeem_invite api ~service:(service api service_name) ?audience
              code);
        Ui.ok "You are now a member of %s" service_name)
  in
  Common.cmd
    (Cmd.info "join"
       ~doc:(Printf.sprintf "Join a %s with an invite code." kind))
    Term.(
      term_result
        (const action
        $ Common.positional 0
            (String.uppercase_ascii kind)
            (Printf.sprintf "%s hostname." (String.capitalize_ascii kind))
        $ Common.audience $ code))

let knot_service _ knot = Api.service_url knot

let cmd =
  Cmd.group
    (Cmd.info "knot"
       ~doc:"Inspect and administer a knot, which hosts repositories.")
    [
      describe_cmd;
      member_group;
      invite_cmd ~kind:"knot" ~service:knot_service;
      join_cmd ~kind:"knot" ~service:knot_service;
      bans_cmd;
      ban "ban" ~ban:true;
      ban "unban" ~ban:false;
    ]
