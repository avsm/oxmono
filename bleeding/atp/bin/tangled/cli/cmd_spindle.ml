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
  Cmd.v
    (Cmd.info "describe"
       ~doc:"Show a spindle's DID, version and capabilities. Pipes get JSON.")
    Term.(term_result (const action $ spindle $ Common.json_flag))

let members_cmd =
  let action spindle audience json =
    Common.run (fun env ->
        let list =
          Common.with_api ~busy:("Reading allow-list of " ^ spindle) env
            (fun api ->
              Api.allow_list api
                ~spindle:(Api.spindle_url api spindle)
                ?audience ())
        in
        if json || not (Ui.tty ()) then Common.json list
        else
          let dids =
            match Tangled.Schema.member "dids" list with
            | Some (Jsont.Array (dids, _)) -> List.map Tangled.Schema.text dids
            | _ -> []
          in
          Ui.table
            ~heading:(Ui.heading "Allowed owners" (List.length dids))
            [ Ui.column "DID" ]
            (List.map (fun d -> [ Ui.accent d ]) dids))
  in
  Cmd.v
    (Cmd.info "members"
       ~doc:"List the spindle's allow-list. Log in as a spindle admin.")
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
  Cmd.v
    (Cmd.info name
       ~doc:
         (if add then "Admit a repository owner to the spindle."
          else "Withdraw an owner's admission."))
    Term.(term_result (const action $ spindle $ subject $ Common.audience))

let cmd =
  Cmd.group
    (Cmd.info "spindle" ~doc:"Inspect a spindle and manage its allow-list.")
    [
      describe_cmd;
      members_cmd;
      change "add-member" ~add:true;
      change "remove-member" ~add:false;
    ]
