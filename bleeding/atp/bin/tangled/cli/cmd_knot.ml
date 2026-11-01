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
  Cmd.v
    (Cmd.info "describe"
       ~doc:"Show a knot's DID, version and capabilities. Pipes get JSON.")
    Term.(term_result (const action $ knot $ Common.json_flag))

let members_cmd =
  let action knot =
    Common.run (fun env ->
        let members =
          Common.with_api ~busy:("Reading members of " ^ knot) env (fun api ->
              Api.knot_members api ~knot)
        in
        if Ui.tty () then
          Ui.table
            ~heading:(Ui.heading "Members" (List.length members))
            [ Ui.column "DID" ]
            (List.map (fun d -> [ Ui.accent d ]) members)
        else List.iter print_endline members)
  in
  Cmd.v
    (Cmd.info "members" ~doc:"List the DIDs granted knot membership.")
    Term.(term_result (const action $ knot))

let member name ~add =
  let user = Common.positional 1 "USER" "Member handle or DID." in
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
  Cmd.v
    (Cmd.info name ~doc:"Change knot membership. Log in as a knot admin.")
    Term.(term_result (const action $ knot $ user $ Common.audience))

let cmd =
  Cmd.group
    (Cmd.info "knot" ~doc:"Inspect a knot and manage its members.")
    [
      describe_cmd;
      members_cmd;
      member "add-member" ~add:true;
      member "remove-member" ~add:false;
    ]
