(* SPDX-License-Identifier: ISC *)
open Cmdliner
module Api = Tangled.Api
module Repo = Atp_lexicon_tangled.Sh.Tangled.Repo

let repo =
  Common.positional 0 "REPO" "Repository DID, owner/name, or record URI."

let name = Common.positional 0 "NAME" "New repository name."

let knot_required =
  Arg.(
    required
    & opt (some string) None
    & info [ "knot"; "k" ] ~docv:"KNOT" ~doc:"Knot hostname or HTTP(S) origin.")

let description =
  Common.optional [ "description"; "d" ] "TEXT" "Repository description."

let branch = Common.optional [ "branch"; "b" ] "BRANCH" "Default Git branch."
let source = Common.optional [ "source" ] "REPO" "Repository to fork."

let pp_repository ppf (rkey, (r : Repo.main)) =
  Fmt.pf ppf "%s  %s@,  %s%a%a@."
    (Option.value ~default:rkey r.name)
    r.knot
    (Option.value ~default:"(legacy repository without DID)" r.repo_did)
    Fmt.(option (fmt "@,  %s"))
    r.description
    Fmt.(option (fmt "@,  spindle: %s"))
    r.spindle

let list_cmd =
  let action user json =
    Common.run (fun env ->
        Common.with_api env (fun api ->
            let repos = Api.list_repos api ~did:(Common.did api user) () in
            if json then
              Common.print (Jsont.list Repo.main_jsont) (List.map snd repos)
            else List.iter (fun r -> Fmt.pr "%a" pp_repository r) repos))
  in
  Cmd.v
    (Cmd.info "list" ~doc:"List all repository records on the configured PDS.")
    Term.(term_result (const action $ Common.user $ Common.json_flag))

let create_cmd =
  let action name knot audience description branch source =
    Common.run (fun env ->
        Common.with_api env (fun api ->
            let source = Option.map (Api.resolve_repo api) source in
            let repository =
              Api.create_repo api ~name ~knot ?audience ?description
                ?default_branch:branch ?source ()
            in
            Fmt.pr "Repository DID: %s@.Clone URL: %s@.AT URI: %s@."
              (Api.repo_did repository) (Api.git_url repository)
              (Tangled.Types.make_at_uri ~did:repository.owner
                 ~collection:"sh.tangled.repo" ~rkey:repository.rkey)))
  in
  Cmd.v
    (Cmd.info "create" ~doc:"Create a repository, or fork one with --source.")
    Term.(
      term_result
        (const action $ name $ knot_required $ Common.audience $ description
       $ branch $ source))

let clone_cmd =
  let dir = Arg.(value & pos 1 (some string) None & info [] ~docv:"DIR") in
  let action repo knot dir =
    Common.run (fun env ->
        Common.with_api env (fun api -> Api.clone api ~repo ?knot ?dir ()))
  in
  Cmd.v
    (Cmd.info "clone" ~doc:"Clone a repository using its stable DID.")
    Term.(term_result (const action $ repo $ Common.knot $ dir))

let info_cmd =
  let action repo knot =
    Common.run (fun env ->
        Common.with_api env (fun api ->
            let repository = Api.resolve_repo api ?knot repo in
            Common.print Repo.main_jsont repository.record))
  in
  Cmd.v
    (Cmd.info "info" ~doc:"Show the repository record and its stable identity.")
    Term.(term_result (const action $ repo $ Common.knot))

let delete_cmd =
  let force =
    Arg.(
      value & flag
      & info [ "force"; "f" ]
          ~doc:"Confirm deletion without an interactive prompt.")
  in
  let action repo knot audience force =
    Common.run (fun env ->
        Common.with_api env (fun api ->
            let repository = Api.resolve_repo api ?knot repo in
            let confirmed =
              force
              ||
              (Fmt.pr "Delete %s? [y/N] @?" (Api.repo_did repository);
               match String.lowercase_ascii (read_line ()) with
               | "y" | "yes" -> true
               | _ -> false)
            in
            if confirmed then Api.delete_repo api ?audience repository))
  in
  Cmd.v
    (Cmd.info "delete"
       ~doc:"Remove the PDS record, then tear down the knot repository.")
    Term.(
      term_result (const action $ repo $ Common.knot $ Common.audience $ force))

let spindle_cmd =
  let spindle =
    Arg.(
      value
      & pos 1 (some string) None
      & info [] ~docv:"SPINDLE"
          ~doc:"Assign this spindle. Omit to remove the assignment.")
  in
  let action repo knot spindle =
    Common.run (fun env ->
        Common.with_api env (fun api ->
            let repository = Api.resolve_repo api ?knot repo in
            if repository.owner <> Api.get_did api then
              failwith "Only the owner can assign a spindle";
            let existing =
              match
                Api.get_record api ~did:repository.owner
                  ~collection:"sh.tangled.repo" ~rkey:repository.rkey
              with
              | Some record -> record
              | None -> failwith "Repository disappeared"
            in
            let current = Api.decode Repo.main_jsont existing.value in
            if
              current.repo_did <> repository.record.repo_did
              || current.knot <> repository.record.knot
            then failwith "Repository identity changed. Resolve it again.";
            let fields =
              match existing.value with
              | Jsont.Object (fields, _) -> fields
              | _ -> assert false
            in
            let fields =
              List.filter (fun ((name, _), _) -> name <> "spindle") fields
            in
            let fields =
              match spindle with
              | None -> fields
              | Some spindle ->
                  Jsont.Json.mem
                    ("spindle", Jsont.Meta.none)
                    (Common.string (Api.service_host spindle))
                  :: fields
            in
            let cid =
              match existing.cid with
              | Some cid -> cid
              | None -> failwith "PDS did not return a record CID"
            in
            ignore
              (Api.put_record api ~collection:"sh.tangled.repo"
                 ~rkey:repository.rkey ~swap_record:(Some cid)
                 (Jsont.Json.object' fields))))
  in
  Cmd.v
    (Cmd.info "spindle"
       ~doc:
         "Assign or remove a repository's spindle, preserving other metadata.")
    Term.(term_result (const action $ repo $ Common.knot $ spindle))

let collaborators_cmd =
  let action repo knot limit cursor =
    Common.run (fun env ->
        Common.with_api env (fun api ->
            let repository = Api.resolve_repo api ?knot repo in
            let params =
              [
                ("subject", Api.repo_did repository);
                ("limit", string_of_int limit);
              ]
              @ Option.to_list
                  (Option.map (fun value -> ("cursor", value)) cursor)
            in
            Tangled.Schema.params "sh.tangled.repo.listCollaborators" params;
            Common.json
              (Xrpc.Client.query
                 (Api.public_client api ~service:repository.service)
                 ~nsid:"sh.tangled.repo.listCollaborators" ~params
                 ~decoder:Jsont.json)))
  in
  Cmd.v
    (Cmd.info "collaborators"
       ~doc:"List the knot's authoritative collaborators.")
    Term.(
      term_result
        (const action $ repo $ Common.knot $ Common.limit $ Common.cursor))

let collaborator_cmd name method_ =
  let subject = Common.positional 1 "USER" "Collaborator handle or DID." in
  let action repo subject knot audience =
    Common.run (fun env ->
        Common.with_api env (fun api ->
            let repository = Api.resolve_repo api ?knot repo in
            let nsid = "sh.tangled.repo." ^ method_ in
            let input =
              Common.object_
                [
                  ("repo", Common.string (Api.repo_did repository));
                  ("subject", Common.string (Api.resolve_handle api subject));
                ]
            in
            Tangled.Schema.validate_input nsid input;
            Xrpc.Client.procedure_unit
              (Api.service_client api ~service:repository.service ?audience
                 ~nsid ())
              ~nsid ~params:[] ~input:(Some Jsont.json) ~input_data:(Some input)))
  in
  Cmd.v
    (Cmd.info name ~doc:"Change the knot's authoritative collaborators.")
    Term.(
      term_result (const action $ repo $ subject $ Common.knot $ Common.audience))

let cmd =
  Cmd.group
    (Cmd.info "repo"
       ~doc:"Manage repositories and access to their knots and spindles.")
    [
      list_cmd;
      create_cmd;
      clone_cmd;
      info_cmd;
      delete_cmd;
      spindle_cmd;
      collaborators_cmd;
      collaborator_cmd "add-collaborator" "addCollaborator";
      collaborator_cmd "remove-collaborator" "removeCollaborator";
    ]
