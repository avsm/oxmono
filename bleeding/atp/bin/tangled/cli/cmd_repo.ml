(* SPDX-License-Identifier: ISC *)
open Cmdliner
module Api = Tangled.Api

let name = Common.positional 0 "NAME" "New repository name."

let knot_required =
  Arg.(
    required
    & opt (some string) None
    & info [ "knot"; "k" ] ~docv:"KNOT"
        ~env:(Cmd.Env.info "TANGLED_KNOT")
        ~doc:"Knot hostname or HTTP(S) origin to host the repository.")

let description =
  Common.optional [ "description"; "d" ] "TEXT" "Repository description."

let branch = Common.optional [ "branch"; "b" ] "BRANCH" "Default Git branch."
let fork = Common.optional [ "fork" ] "REPO" "Create a fork of REPO."

let import =
  Common.optional [ "import" ] "URL"
    "Import the Git repository at URL. The knot clones it after creating the \
     repository."

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
  Common.cmd
    (Cmd.info "list" ~doc:"List the repositories an account declares.")
    Term.(term_result (const action $ Common.user $ Common.json_flag))

let pp_repository ppf (r : Api.repository) =
  Fmt.pf ppf "@[<v>%s/%s@,DID: %s@,Knot: %s@,Clone: %s%a%a@]@." r.owner r.name
    r.did r.knot (Api.git_url r)
    Fmt.(option (fmt "@,Description: %s"))
    r.manifest.description
    Fmt.(option (fmt "@,Spindle: %s"))
    r.manifest.spindle

(* [with_owner api r] is [r] with its owner's handle, looked up only for a
   terminal, which shows it. *)
let with_owner api (r : Api.repository) =
  (r, if Ui.tty () then Api.handle_of_did api r.owner else None)

let show_repository ?default_branch ?import ((r : Api.repository), owner) =
  if Ui.tty () then
    let name = match owner with Some h -> h ^ "/" ^ r.name | None -> r.name in
    Ui.panel (Ui.title name)
      (Ui.fields
         [
           ("Description", Option.map Ui.text r.manifest.description);
           ("DID", Some (Ui.accent r.did));
           ( "Owner",
             Some
               (Ui.V.concat
                  [
                    Ui.account (r.owner, owner);
                    (if owner = None then Ui.V.empty
                     else Ui.muted ("  " ^ r.owner));
                  ]) );
           ("Knot", Some (Ui.text r.knot));
           ("Spindle", Option.map Ui.text r.manifest.spindle);
           ("Default branch", Option.map Ui.accent default_branch);
           ( "Import",
             Option.bind import
               (fun (i : Api.Lex.Repo.GetImportStatus.output) ->
                 Option.map
                   (fun status ->
                     Ui.V.concat
                       [
                         Ui.state status;
                         Option.fold ~none:Ui.V.empty
                           ~some:(fun r -> Ui.muted ("  " ^ r))
                           i.reason;
                       ])
                   i.status) );
           ("Clone", Some (Ui.link (Api.git_url r)));
         ])
  else Fmt.pr "%a" pp_repository r

let create_cmd =
  let action name knot audience description branch spindle fork import =
    Common.run (fun env ->
        show_repository
          (Common.with_api ~busy:("Creating " ^ name) env (fun api ->
               let source = Option.map (Api.resolve_repo api) fork in
               Api.create_repo api ~name ~knot ?audience ?description
                 ?default_branch:branch ?spindle ?source ?import ()
               |> with_owner api));
        if import <> None then
          Ui.ok "The knot is importing %s. Follow it with: tangled repo info %s"
            (Option.get import) name)
  in
  Common.cmd
    (Cmd.info "create"
       ~doc:
         "Create a repository, empty, as a fork with --fork, or imported with \
          --import. The knot defaults to $(b,TANGLED_KNOT).")
    Term.(
      term_result
        (const action $ name $ knot_required $ Common.audience $ description
       $ branch $ spindle_opt $ fork $ import))

let clone_cmd =
  let repo =
    Common.positional 0 "REPO"
      "Repository as handle/name, or a name of your own. Its DID or \
       declaration URI also work."
  in
  let dir = Arg.(value & pos 1 (some string) None & info [] ~docv:"DIR") in
  let action repo knot dir =
    Common.run (fun env ->
        Common.with_api env (fun api -> Api.clone api ~repo ?knot ?dir ()))
  in
  Common.cmd
    (Cmd.info "clone"
       ~doc:
         "Clone a repository. The remote names it by its DID, which survives \
          renames.")
    Term.(term_result (const action $ repo $ Common.knot $ dir))

let info_cmd =
  let action repo knot json =
    Common.run (fun env ->
        let (((r : Api.repository), _) as shown), default_branch, import =
          Common.with_api ~busy:"Resolving repository" env (fun api ->
              let shown = with_owner api (Common.resolve api ?knot repo) in
              let r = fst shown in
              (* An empty or still-importing repository has no branches to
                 read, which is no reason to fail. *)
              let quietly f = try f () with Eio.Io _ | Failure _ -> None in
              if json then (shown, None, None)
              else
                ( shown,
                  quietly (fun () -> Api.default_branch api r),
                  quietly (fun () -> Some (Api.import_status api r)) ))
        in
        if json then Common.json r.manifest_value
        else show_repository ?default_branch ?import shown)
  in
  Common.cmd
    (Cmd.info "info"
       ~doc:
         "Show a repository's identity, manifest, default branch and \
          import.")
    Term.(
      term_result
        (const action $ Common.repo_arg $ Common.knot $ Common.json_flag))

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
                  Common.resolve api ?knot repo)
            in
            if
              force
              || Ui.confirm
                   (Printf.sprintf "Delete %s (%s) and all it holds?"
                      repository.name repository.did)
            then begin
              Ui.busy ("Deleting " ^ repository.name) (fun () ->
                  Api.delete_repo api ?audience repository);
              Ui.ok "Deleted %s (%s)" repository.name repository.did
            end))
  in
  Common.cmd
    (Cmd.info "delete"
       ~doc:
         "Remove the PDS declaration, then delete the repository on its knot.")
    Term.(
      term_result
        (const action $ Common.repo_arg $ Common.knot $ Common.audience
        $ force))

let spindle_cmd ~set =
  let spindle =
    Common.positional 0 "SPINDLE" "Spindle hostname to run the repository's CI."
  in
  let action repo knot spindle =
    Common.run (fun env ->
        let name =
          Common.with_api ~busy:"Updating manifest" env (fun api ->
              let r = Common.resolve api ?knot repo in
              Api.set_spindle api r spindle;
              r.name)
        in
        match spindle with
        | Some s -> Ui.ok "CI for %s runs on %s" name s
        | None -> Ui.ok "%s has no spindle now" name)
  in
  if set then
    Common.cmd
      (Cmd.info "set-spindle" ~doc:"Run the repository's CI on a spindle.")
      Term.(
        term_result
          (const action $ Common.repo $ Common.knot
          $ (const Option.some $ spindle)))
  else
    Common.cmd
      (Cmd.info "unset-spindle" ~doc:"Stop running the repository's CI.")
      Term.(term_result (const action $ Common.repo $ Common.knot $ const None))

let collaborators_cmd =
  let action repo knot =
    Common.run (fun env ->
        let tty = Ui.tty () in
        let dids =
          Common.with_api ~busy:"Reading collaborators" env (fun api ->
              let dids =
                Api.collaborators api (Common.resolve api ?knot repo)
              in
              if tty then Common.handles api dids
              else List.map (fun d -> (d, None)) dids)
        in
        if tty then Ui.accounts "Collaborators" dids
        else List.iter (fun (d, _) -> print_endline d) dids)
  in
  Common.cmd
    (Cmd.info "list" ~doc:"List the accounts that may push.")
    Term.(term_result (const action $ Common.repo $ Common.knot))

let collaborator_cmd name ~add =
  let subject = Common.positional 0 "USER" "Collaborator handle or DID." in
  let action repo subject knot =
    Common.run (fun env ->
        let who = subject in
        let name =
          Common.with_api ~busy:"Updating collaborators" env (fun api ->
              let repository = Common.resolve api ?knot repo in
              let subject = Api.resolve_handle api subject in
              if add then Api.add_collaborator api repository subject
              else Api.remove_collaborator api repository subject;
              repository.name)
        in
        if add then Ui.ok "%s may now push to %s" who name
        else Ui.ok "%s may no longer push to %s" who name)
  in
  Common.cmd
    (Cmd.info name
       ~doc:(if add then "Let a user push." else "Stop a user pushing."))
    Term.(term_result (const action $ Common.repo $ subject $ Common.knot))

let collaborator_group =
  Cmd.group
    (Cmd.info "collaborator" ~doc:"Manage who may push to a repository.")
    [
      collaborators_cmd;
      collaborator_cmd "add" ~add:true;
      collaborator_cmd "remove" ~add:false;
    ]

let cmd =
  Cmd.group
    (Cmd.info "repo"
       ~doc:
         "Create, browse and configure repositories. Commands that act in a \
          repository take it with --repo, and those about one take it as an \
          argument. Both default to the repository the current directory is a \
          clone of.")
    ([
       list_cmd;
       create_cmd;
       clone_cmd;
       info_cmd;
       delete_cmd;
       spindle_cmd ~set:true;
       spindle_cmd ~set:false;
       collaborator_group;
       Cmd_release.cmd;
       Cmd_webhook.cmd;
     ]
    @ Cmd_browse.cmds)
