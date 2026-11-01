(* SPDX-License-Identifier: ISC *)
open Cmdliner
module Api = Tangled.Api
module Star = Tangled.Api.Lex.Feed.Star

let collection = "org.tangled.feed.star"
let repo =
  Common.positional 0 "REPO"
    "Repository as handle/name, or a name of your own. Its DID also works."

(* The DID of [repo]. A DID is taken as it is, so that a deleted repository
   can still be unstarred. *)
let repo_did api repo =
  if Atp.Did.is_valid repo then repo else (Api.resolve_repo api repo).did

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
        let rich = not (json || not (Ui.tty ())) in
        let stars =
          Common.with_api ~busy:"Reading stars" env (fun api ->
              let stars = Api.list_stars api ~did:(Common.did api user) () in
              (* A terminal names each starred repository by its owner's
                 handle and its name, where they can still be found. *)
              Eio.Fiber.List.map ~max_fibers:8
                (fun (_, (s : Star.main)) ->
                  let name =
                    if not rich then None
                    else
                      match Api.resolve_repo api (identity s) with
                      | r ->
                          let owner =
                            Option.value ~default:r.owner
                              (Api.handle_of_did api r.owner)
                          in
                          Some (owner ^ "/" ^ r.name)
                      | exception (Eio.Io _ | Failure _ | Invalid_argument _)
                        ->
                          None
                  in
                  (s, name))
                stars)
        in
        if rich then
          Ui.table
            ~heading:(Ui.heading "Stars" (List.length stars))
            [ Ui.column "Repository"; Ui.column "DID"; Ui.column "Starred" ]
            (List.map
               (fun ((s : Star.main), name) ->
                 [
                   (match name with
                   | Some n -> Ui.accent n
                   | None -> Ui.muted "(not found)");
                   Ui.muted (identity s);
                   Ui.muted s.created_at;
                 ])
               stars)
        else Common.print (Jsont.list Star.main_jsont) (List.map fst stars))
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
        let created =
          Common.with_api ~busy:"Starring" env (fun api ->
              let repo = repo_did api repo in
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
            let repo = repo_did api repo in
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
