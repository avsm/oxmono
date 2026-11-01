(* SPDX-License-Identifier: ISC *)
open Cmdliner
module Api = Tangled.Api

let repo =
  Common.positional 0 "REPO" "Repository DID, owner/name, or declaration URI."

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

let spindle_opt =
  Common.optional [ "spindle"; "s" ] "SPINDLE" "Spindle hostname to run CI."

let list_cmd =
  let action user json =
    Common.run (fun env ->
        let repos =
          Common.with_api ~busy:"Reading repositories" env (fun api ->
              Api.list_declarations api ~did:(Common.did api user) ())
        in
        if json then
          Common.print
            (Jsont.list Api.Lex.Repo.Declaration.main_jsont)
            (List.map snd repos)
        else if Ui.tty () then
          Ui.table
            ~heading:(Ui.heading "Repositories" (List.length repos))
            [ Ui.column "Name"; Ui.column "DID" ]
            (List.map
               (fun (slug, (d : Api.Lex.Repo.Declaration.main)) ->
                 [ Ui.accent slug; Ui.muted d.repo ])
               repos)
        else
          List.iter
            (fun (slug, (d : Api.Lex.Repo.Declaration.main)) ->
              Fmt.pr "%s  %s@." slug d.repo)
            repos)
  in
  Cmd.v
    (Cmd.info "list" ~doc:"List the repositories an account declares.")
    Term.(term_result (const action $ Common.user $ Common.json_flag))

let pp_repository ppf (r : Api.repository) =
  Fmt.pf ppf "@[<v>%s/%s@,DID: %s@,Knot: %s@,Clone: %s%a%a@]@." r.owner r.name
    r.did r.knot (Api.git_url r)
    Fmt.(option (fmt "@,Description: %s"))
    r.manifest.description
    Fmt.(option (fmt "@,Spindle: %s"))
    r.manifest.spindle

let show_repository (r : Api.repository) =
  if Ui.tty () then
    Ui.panel (Ui.title r.name)
      (Ui.fields
         [
           ("Description", Option.map Ui.text r.manifest.description);
           ("DID", Some (Ui.accent r.did));
           ("Owner", Some (Ui.text r.owner));
           ("Knot", Some (Ui.text r.knot));
           ("Spindle", Option.map Ui.text r.manifest.spindle);
           ("Clone", Some (Ui.link (Api.git_url r)));
         ])
  else Fmt.pr "%a" pp_repository r

let create_cmd =
  let action name knot audience description branch spindle source =
    Common.run (fun env ->
        show_repository
          (Common.with_api ~busy:("Creating " ^ name) env (fun api ->
               let source = Option.map (Api.resolve_repo api) source in
               Api.create_repo api ~name ~knot ?audience ?description
                 ?default_branch:branch ?spindle ?source ())))
  in
  Cmd.v
    (Cmd.info "create" ~doc:"Create a repository, or fork one with --source.")
    Term.(
      term_result
        (const action $ name $ knot_required $ Common.audience $ description
       $ branch $ spindle_opt $ source))

let clone_cmd =
  let dir = Arg.(value & pos 1 (some string) None & info [] ~docv:"DIR") in
  let action repo knot dir =
    Common.run (fun env ->
        Common.with_api env (fun api -> Api.clone api ~repo ?knot ?dir ()))
  in
  Cmd.v
    (Cmd.info "clone" ~doc:"Clone a repository by its stable DID.")
    Term.(term_result (const action $ repo $ Common.knot $ dir))

let info_cmd =
  let action repo knot json =
    Common.run (fun env ->
        let r =
          Common.with_api ~busy:"Resolving repository" env (fun api ->
              Api.resolve_repo api ?knot repo)
        in
        if json then Common.json r.manifest_value else show_repository r)
  in
  Cmd.v
    (Cmd.info "info" ~doc:"Show a repository's identity and manifest.")
    Term.(term_result (const action $ repo $ Common.knot $ Common.json_flag))

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
            let repository =
              Ui.busy "Resolving repository" (fun () ->
                  Api.resolve_repo api ?knot repo)
            in
            if force || Ui.confirm ("Delete " ^ repository.did ^ "?") then begin
              Ui.busy ("Deleting " ^ repository.name) (fun () ->
                  Api.delete_repo api ?audience repository);
              Ui.ok "Deleted %s (%s)" repository.name repository.did
            end))
  in
  Cmd.v
    (Cmd.info "delete"
       ~doc:
         "Remove the PDS declaration, then delete the repository on its knot.")
    Term.(
      term_result (const action $ repo $ Common.knot $ Common.audience $ force))

let spindle_cmd =
  let spindle =
    Arg.(
      value
      & pos 1 (some string) None
      & info [] ~docv:"SPINDLE"
          ~doc:"Assign the spindle at this hostname. Omit to remove it.")
  in
  let action repo knot spindle =
    Common.run (fun env ->
        Common.with_api ~busy:"Updating manifest" env (fun api ->
            Api.set_spindle api (Api.resolve_repo api ?knot repo) spindle);
        match spindle with
        | Some s -> Ui.ok "CI for %s runs on %s" repo s
        | None -> Ui.ok "Removed the spindle of %s" repo)
  in
  Cmd.v
    (Cmd.info "spindle"
       ~doc:"Assign or remove a repository's spindle in its manifest.")
    Term.(term_result (const action $ repo $ Common.knot $ spindle))

let collaborators_cmd =
  let action repo knot =
    Common.run (fun env ->
        let dids =
          Common.with_api ~busy:"Reading collaborators" env (fun api ->
              Api.collaborators api (Api.resolve_repo api ?knot repo))
        in
        if Ui.tty () then
          Ui.table
            ~heading:(Ui.heading "Collaborators" (List.length dids))
            [ Ui.column "DID" ]
            (List.map (fun d -> [ Ui.accent d ]) dids)
        else List.iter print_endline dids)
  in
  Cmd.v
    (Cmd.info "collaborators" ~doc:"List the DIDs granted collaboration.")
    Term.(term_result (const action $ repo $ Common.knot))

let collaborator_cmd name ~add =
  let subject = Common.positional 1 "USER" "Collaborator handle or DID." in
  let action repo subject knot =
    Common.run (fun env ->
        let who = subject in
        Common.with_api ~busy:"Updating collaborators" env (fun api ->
            let repository = Api.resolve_repo api ?knot repo in
            let subject = Api.resolve_handle api subject in
            if add then Api.add_collaborator api repository subject
            else Api.remove_collaborator api repository subject);
        if add then Ui.ok "%s may now push to %s" who repo
        else Ui.ok "%s may no longer push to %s" who repo)
  in
  Cmd.v
    (Cmd.info name
       ~doc:
         (if add then "Grant a user collaboration."
          else "Revoke collaboration."))
    Term.(term_result (const action $ repo $ subject $ Common.knot))

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
      collaborator_cmd "add-collaborator" ~add:true;
      collaborator_cmd "remove-collaborator" ~add:false;
    ]
