(* SPDX-License-Identifier: ISC *)
open Cmdliner
module Api = Tangled.Api
module Star = Atp_lexicon_tangled.Sh.Tangled.Feed.Star

let collection = "sh.tangled.feed.star"
let repo = Common.positional 0 "REPO" "Repository DID."

let list_cmd =
  let action user =
    Common.run (fun env ->
        Common.with_api env (fun api ->
            Common.print
              (Jsont.list Star.main_jsont)
              (List.map snd (Api.list_stars api ~did:(Common.did api user) ()))))
  in
  Cmd.v
    (Cmd.info "list" ~doc:"List starred repositories and strings.")
    Term.(term_result (const action $ Common.user))

let subject repo =
  Common.object_
    [
      ("$type", Common.string "sh.tangled.feed.star#repo");
      ("did", Common.string repo);
    ]

let matches repo (star : Star.main) =
  Tangled.Schema.member "did" star.subject
  |> Option.fold ~none:false ~some:(fun did -> Tangled.Schema.text did = repo)

let add_cmd =
  let action repo =
    Common.run (fun env ->
        Common.with_api env (fun api ->
            if not (Atp.Did.is_valid repo) then
              invalid_arg "Expected repository DID";
            if
              not
                (List.exists
                   (fun (_, star) -> matches repo star)
                   (Api.list_stars api ()))
            then
              let star : Star.main =
                { subject = subject repo; created_at = Api.now () }
              in
              Common.print Api.Atproto.Repo.CreateRecord.output_jsont
                (Api.create_record api ~collection
                   (Api.encode Star.main_jsont star))))
  in
  Cmd.v
    (Cmd.info "add" ~doc:"Star a repository.")
    Term.(term_result (const action $ repo))

let remove_cmd =
  let action repo =
    Common.run (fun env ->
        Common.with_api env (fun api ->
            Api.list_records api ~did:(Api.get_did api) ~collection
            |> List.iter (fun (record : Api.Atproto.Repo.ListRecords.record) ->
                if matches repo (Api.decode Star.main_jsont record.value) then
                  Api.delete_record api ~collection
                    ~rkey:(Api.rkey_of_uri record.uri)
                    ~swap_record:record.cid ())))
  in
  Cmd.v
    (Cmd.info "remove" ~doc:"Unstar a repository.")
    Term.(term_result (const action $ repo))

let cmd =
  Cmd.group
    (Cmd.info "star" ~doc:"Manage stars.")
    [ list_cmd; add_cmd; remove_cmd ]
