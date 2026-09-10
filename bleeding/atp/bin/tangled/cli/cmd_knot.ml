(* SPDX-License-Identifier: ISC *)
open Cmdliner
module Api = Tangled.Api

let knot = Common.positional 0 "KNOT" "Knot hostname or HTTP(S) origin."

let query name nsid ~paged ~subject =
  let action knot limit cursor =
    Common.run (fun env ->
        Common.with_api ~authenticated:false env (fun api ->
            let params =
              (if subject then [ ("subject", Api.service_host knot) ] else [])
              @ (if paged then [ ("limit", string_of_int limit) ] else [])
              @ Option.to_list (Option.map (fun x -> ("cursor", x)) cursor)
            in
            Tangled.Schema.params nsid params;
            Common.json
              (Xrpc.Client.query
                 (Api.public_client api ~service:knot)
                 ~nsid ~params ~decoder:Jsont.json)))
  in
  let term =
    if paged then Term.(const action $ knot $ Common.limit $ Common.cursor)
    else Term.(const action $ knot $ const 50 $ const None)
  in
  Cmd.v (Cmd.info name ~doc:("Read knot " ^ name ^ ".")) Term.(term_result term)

let member name method_ =
  let user = Common.positional 1 "USER" "Member handle or DID." in
  let action knot user audience =
    Common.run (fun env ->
        Common.with_api env (fun api ->
            let nsid = "sh.tangled.knot." ^ method_ in
            let input =
              Common.object_
                [ ("subject", Common.string (Api.resolve_handle api user)) ]
            in
            Tangled.Schema.validate_input nsid input;
            Xrpc.Client.procedure_unit
              (Api.service_client api ~service:knot ?audience ~nsid ())
              ~nsid ~params:[] ~input:(Some Jsont.json) ~input_data:(Some input)))
  in
  Cmd.v
    (Cmd.info name ~doc:"Change knot membership using its authoritative API.")
    Term.(term_result (const action $ knot $ user $ Common.audience))

let cmd =
  Cmd.group
    (Cmd.info "knot" ~doc:"Inspect a knot and manage its members.")
    [
      query "version" "sh.tangled.knot.version" ~paged:false ~subject:false;
      query "keys" "sh.tangled.knot.listKeys" ~paged:true ~subject:false;
      query "members" "sh.tangled.knot.listMembers" ~paged:true ~subject:true;
      member "add-member" "addMember";
      member "remove-member" "removeMember";
    ]
