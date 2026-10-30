(* SPDX-License-Identifier: ISC *)
open Cmdliner
module Api = Tangled.Api

let knot = Common.positional 0 "KNOT" "Knot hostname or HTTP(S) origin."

let describe_cmd =
  let action knot =
    Common.run (fun env ->
        Common.with_api ~authenticated:false env (fun api ->
            Common.print Api.Lex.Knot.DescribeKnot.output_jsont
              (Xrpc.Client.query
                 (Api.public_client api ~service:knot)
                 ~nsid:"org.tangled.knot.describeKnot" ~params:[]
                 ~decoder:Api.Lex.Knot.DescribeKnot.output_jsont)))
  in
  Cmd.v
    (Cmd.info "describe" ~doc:"Show a knot's DID, version and capabilities.")
    Term.(term_result (const action $ knot))

let members_cmd =
  let action knot =
    Common.run (fun env ->
        Common.with_api env (fun api ->
            List.iter print_endline (Api.knot_members api ~knot)))
  in
  Cmd.v
    (Cmd.info "members" ~doc:"List the DIDs granted knot membership.")
    Term.(term_result (const action $ knot))

let member name ~add =
  let user = Common.positional 1 "USER" "Member handle or DID." in
  let action knot user audience =
    Common.run (fun env ->
        Common.with_api env (fun api ->
            let user = Api.resolve_handle api user in
            if add then Api.add_knot_member api ~knot ?audience user
            else Api.remove_knot_member api ~knot ?audience user))
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
