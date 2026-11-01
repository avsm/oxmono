(* SPDX-License-Identifier: ISC *)
open Cmdliner
module Api = Tangled.Api
module Star = Tangled.Api.Lex.Feed.Star

let collection = "org.tangled.feed.star"
let repo = Common.positional 0 "REPO" "Repository DID."

let identity (star : Star.main) =
  match Tangled.Schema.member "identity" star.subject with
  | Some did -> Tangled.Schema.text did
  | None -> (
      match Tangled.Schema.member "uri" star.subject with
      | Some uri -> Tangled.Schema.text uri
      | None -> "")

let list_cmd =
  let action user json =
    Common.run (fun env ->
        let stars =
          Common.with_api ~busy:"Reading stars" env (fun api ->
              Api.list_stars api ~did:(Common.did api user) ())
        in
        if json || not (Ui.tty ()) then
          Common.print (Jsont.list Star.main_jsont) (List.map snd stars)
        else
          Ui.table
            ~heading:(Ui.heading "Stars" (List.length stars))
            [ Ui.column "Subject"; Ui.column "Starred" ]
            (List.map
               (fun (_, (s : Star.main)) ->
                 [ Ui.accent (identity s); Ui.muted s.created_at ])
               stars))
  in
  Cmd.v
    (Cmd.info "list"
       ~doc:"List starred repositories and strings. Pipes get JSON.")
    Term.(term_result (const action $ Common.user $ Common.json_flag))

let subject repo =
  Common.object_
    [
      ("$type", Common.string "org.tangled.feed.star#repo");
      ("identity", Common.string repo);
    ]

let matches repo (star : Star.main) =
  Tangled.Schema.member "identity" star.subject
  |> Option.fold ~none:false ~some:(fun did -> Tangled.Schema.text did = repo)

let add_cmd =
  let action repo =
    Common.run (fun env ->
        if not (Atp.Did.is_valid repo) then
          invalid_arg "Expected repository DID";
        let created =
          Common.with_api ~busy:"Starring" env (fun api ->
              if
                List.exists
                  (fun (_, star) -> matches repo star)
                  (Api.list_stars api ())
              then None
              else
                let star : Star.main =
                  { subject = subject repo; created_at = Api.now () }
                in
                Some
                  (Api.create_record api ~collection
                     (Api.encode Star.main_jsont star)))
        in
        match created with
        | None -> Ui.ok "Already starred %s" repo
        | Some _ when Ui.tty () -> Ui.ok "Starred %s" repo
        | Some created ->
            Common.print Api.Atproto.Repo.CreateRecord.output_jsont created)
  in
  Cmd.v
    (Cmd.info "add" ~doc:"Star a repository.")
    Term.(term_result (const action $ repo))

let remove_cmd =
  let action repo =
    Common.run (fun env ->
        Common.with_api ~busy:"Unstarring" env (fun api ->
            Api.list_records api ~did:(Api.get_did api) ~collection ()
            |> List.iter (fun (record : Api.Atproto.Repo.ListRecords.record) ->
                if matches repo (Api.decode Star.main_jsont record.value) then
                  Api.delete_record api ~collection
                    ~rkey:(Api.rkey_of_uri record.uri)
                    ~swap_record:record.cid ()));
        Ui.ok "Unstarred %s" repo)
  in
  Cmd.v
    (Cmd.info "remove" ~doc:"Unstar a repository.")
    Term.(term_result (const action $ repo))

let cmd =
  Cmd.group
    (Cmd.info "star" ~doc:"Manage stars.")
    [ list_cmd; add_cmd; remove_cmd ]
