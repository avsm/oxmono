(* SPDX-License-Identifier: ISC *)
open Cmdliner
module Api = Tangled.Api

let spindle = Common.positional 0 "SPINDLE" "Spindle hostname or DID."
let subject = Common.positional 1 "USER" "User handle or DID."

let describe_cmd =
  let action spindle =
    Common.run (fun env ->
        Common.with_api ~authenticated:false env (fun api ->
            Common.print Api.Lex.Spindle.DescribeSpindle.output_jsont
              (Xrpc.Client.query
                 (Api.public_client api ~service:(Api.spindle_url api spindle))
                 ~nsid:"org.tangled.spindle.describeSpindle" ~params:[]
                 ~decoder:Api.Lex.Spindle.DescribeSpindle.output_jsont)))
  in
  Cmd.v
    (Cmd.info "describe" ~doc:"Show a spindle's DID, version and capabilities.")
    Term.(term_result (const action $ spindle))

let members_cmd =
  let action spindle audience =
    Common.run (fun env ->
        Common.with_api env (fun api ->
            Common.json
              (Api.allow_list api ~spindle:(Api.spindle_url api spindle)
                 ?audience ())))
  in
  Cmd.v
    (Cmd.info "members"
       ~doc:"List the spindle's allow-list. Log in as a spindle admin.")
    Term.(term_result (const action $ spindle $ Common.audience))

let change name ~add =
  let action spindle subject audience =
    Common.run (fun env ->
        Common.with_api env (fun api ->
            Api.allow_list_change api ~spindle:(Api.spindle_url api spindle)
              ?audience ~add
              (Api.resolve_handle api subject)))
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
