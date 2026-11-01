(* SPDX-License-Identifier: ISC *)
open Cmdliner
module Api = Tangled.Api
module Member = Atp_lexicon_tangled.Sh.Tangled.Spindle.Member

let spindle = Common.positional 0 "SPINDLE" "Spindle hostname."
let subject = Common.positional 1 "USER" "Member handle or DID."
let collection = "sh.tangled.spindle.member"

let members api ~owner ~spindle =
  Api.list_records api ~did:owner ~collection
  |> List.filter_map (fun (record : Api.Atproto.Repo.ListRecords.record) ->
      let member = Api.decode Member.main_jsont record.value in
      if member.instance = Api.service_host spindle then Some (record, member)
      else None)

let list_cmd =
  let action spindle owner =
    Common.run (fun env ->
        Common.with_api env (fun api ->
            let members = members api ~owner:(Common.did api owner) ~spindle in
            Common.print (Jsont.list Member.main_jsont) (List.map snd members)))
  in
  Cmd.v
    (Cmd.info "members"
       ~doc:"List grants in the spindle owner's PDS. Use --user for the owner.")
    Term.(term_result (const action $ spindle $ Common.user))

let add_cmd =
  let action spindle subject =
    Common.run (fun env ->
        Common.with_api env (fun api ->
            let subject = Api.resolve_handle api subject in
            let existing = members api ~owner:(Api.get_did api) ~spindle in
            if
              not
                (List.exists
                   (fun (_, (member : Member.main)) -> member.subject = subject)
                   existing)
            then
              let member : Member.main =
                {
                  subject;
                  instance = Api.service_host spindle;
                  created_at = Api.now ();
                }
              in
              Common.print Api.Atproto.Repo.CreateRecord.output_jsont
                (Api.create_record api ~collection
                   (Api.encode Member.main_jsont member))))
  in
  Cmd.v
    (Cmd.info "add-member"
       ~doc:"Publish a spindle membership grant. Log in as its owner.")
    Term.(term_result (const action $ spindle $ subject))

let remove_cmd =
  let action spindle subject =
    Common.run (fun env ->
        Common.with_api env (fun api ->
            let subject = Api.resolve_handle api subject in
            members api ~owner:(Api.get_did api) ~spindle
            |> List.iter
                 (fun
                   ( (record : Api.Atproto.Repo.ListRecords.record),
                     (member : Member.main) )
                 ->
                   if member.subject = subject then
                     Api.delete_record api ~collection
                       ~rkey:(Api.rkey_of_uri record.uri)
                       ~swap_record:record.cid ())))
  in
  Cmd.v
    (Cmd.info "remove-member"
       ~doc:"Remove matching grants from your PDS. Log in as spindle owner.")
    Term.(term_result (const action $ spindle $ subject))

let cmd =
  Cmd.group
    (Cmd.info "spindle" ~doc:"Manage spindle membership records.")
    [ list_cmd; add_cmd; remove_cmd ]
