(* SPDX-License-Identifier: ISC *)
(* Reading a repository's Git data from its knot, without a clone. *)
open Cmdliner
module Api = Tangled.Api
module Git = Api.Lex.Git

let ref_opt =
  Common.optional [ "ref" ] "REF"
    "Read REF, a branch, tag or commit, instead of the default branch."

let signature json = Api.decode Git.Defs.signature_jsont json
let date s = if String.length s >= 10 then String.sub s 0 10 else s

let summary message =
  match String.index_opt message '\n' with
  | Some i -> String.sub message 0 i
  | None -> message

let branch_list =
  let action repo knot json =
    Common.run (fun env ->
        let out =
          Common.with_api ~busy:"Reading branches" env (fun api ->
              Api.branches api (Common.resolve api ?knot repo) ~limit:100 ())
        in
        let branches =
          List.map (Api.decode Git.ListBranches.branch_info_jsont) out.branches
        in
        let default (b : Git.ListBranches.branch_info) =
          Some b.name = out.default_branch
        in
        if json then Common.print Git.ListBranches.output_jsont out
        else if Ui.tty () then
          Ui.table
            ~heading:(Ui.heading "Branches" out.total)
            [ Ui.column "Name"; Ui.column "Commit"; Ui.column "Message" ]
            (List.map
               (fun (b : Git.ListBranches.branch_info) ->
                 [
                   (if default b then
                      Ui.V.concat [ Ui.title b.name; Ui.muted " (default)" ]
                    else Ui.bold b.name);
                   Ui.accent (Ui.short_sha b.oid);
                   Ui.text (summary b.message);
                 ])
               branches)
        else
          List.iter
            (fun (b : Git.ListBranches.branch_info) ->
              Fmt.pr "%s %s %s@." (if default b then "*" else " ") b.oid b.name)
            branches)
  in
  Common.cmd
    (Cmd.info "list" ~doc:"List the branches, marking the default.")
    Term.(
      term_result (const action $ Common.repo $ Common.knot $ Common.json_flag))

let branch_change name ~doc f done_ =
  let branch = Common.positional 0 "BRANCH" "The branch's name." in
  let action repo knot branch =
    Common.run (fun env ->
        Common.with_api ~busy:"Updating branches" env (fun api ->
            f api (Common.resolve api ?knot repo) branch);
        Ui.ok done_ branch)
  in
  Common.cmd (Cmd.info name ~doc)
    Term.(term_result (const action $ Common.repo $ Common.knot $ branch))

let branch_group =
  Cmd.group
    (Cmd.info "branch" ~doc:"List branches, delete them and choose the default."
       ~man:
         (Common.man
            [
              "The default branch is the one a clone checks out and that pull \
               requests target unless they name another. Changing it or \
               deleting a branch needs push access.";
            ]))
    [
      branch_list;
      branch_change "delete" ~doc:"Delete a branch from the repository."
        Api.delete_branch "Deleted %s";
      branch_change "default" ~doc:"Make a branch the default."
        Api.set_default_branch "%s is now the default branch";
    ]

let tags_cmd =
  let action repo knot json =
    Common.run (fun env ->
        let out =
          Common.with_api ~busy:"Reading tags" env (fun api ->
              Api.tags api (Common.resolve api ?knot repo) ~limit:100 ())
        in
        let tags = List.map (Api.decode Git.ListTags.tag_info_jsont) out.tags in
        if json then Common.print Git.ListTags.output_jsont out
        else if Ui.tty () then
          Ui.table
            ~heading:(Ui.heading "Tags" out.total)
            [ Ui.column "Name"; Ui.column "Commit"; Ui.column "Date" ]
            (List.map
               (fun (t : Git.ListTags.tag_info) ->
                 [
                   Ui.bold t.name;
                   Ui.accent (Ui.short_sha t.oid);
                   Ui.muted (date (signature t.author).when_);
                 ])
               tags)
        else
          List.iter
            (fun (t : Git.ListTags.tag_info) -> Fmt.pr "%s %s@." t.oid t.name)
            tags)
  in
  Common.cmd
    (Cmd.info "tags" ~doc:"List the repository's tags.")
    Term.(
      term_result (const action $ Common.repo $ Common.knot $ Common.json_flag))

let log_cmd =
  let ref_ =
    Arg.(
      value
      & pos 0 (some string) None
      & info [] ~docv:"REF"
          ~doc:
            "Start from REF, a branch, tag or commit, instead of the default \
             branch.")
  in
  let path =
    Common.optional [ "path" ] "PATH" "Show only commits that change PATH."
  in
  let count =
    Arg.(
      value & opt int 20
      & info [ "n"; "max-count" ] ~docv:"N" ~doc:"Show at most N commits.")
  in
  let action repo knot ref_ path count json =
    Common.run (fun env ->
        let out =
          Common.with_api ~busy:"Reading history" env (fun api ->
              let r = Common.resolve api ?knot repo in
              let ref_ =
                match ref_ with
                | Some r -> r
                | None ->
                    Option.value ~default:"HEAD" (Api.default_branch api r)
              in
              Api.commits api r ~ref_ ?path ~limit:count ())
        in
        let commits = List.map (Api.decode Git.Defs.commit_jsont) out.commits in
        if json then Common.print Git.ListCommits.output_jsont out
        else if Ui.tty () then
          Ui.table
            [
              Ui.column "Commit";
              Ui.column "Date";
              Ui.column "Author";
              Ui.column "Message";
            ]
            (List.map
               (fun (c : Git.Defs.commit) ->
                 let a = signature c.author in
                 [
                   Ui.accent (Ui.short_sha c.hash);
                   Ui.muted (date a.when_);
                   Ui.text a.name;
                   Ui.bold (summary c.message);
                 ])
               commits)
        else
          List.iter
            (fun (c : Git.Defs.commit) ->
              Fmt.pr "%s %s@." c.hash (summary c.message))
            commits)
  in
  Common.cmd
    (Cmd.info "log" ~doc:"Show the commit history, newest first.")
    Term.(
      term_result
        (const action $ Common.repo $ Common.knot $ ref_ $ path $ count
       $ Common.json_flag))

let path_arg ~required =
  if required then
    Common.positional 0 "PATH" "The file, as a path from the repository's top."
  else
    Term.(
      const (Option.value ~default:"")
      $ Arg.(
          value
          & pos 0 (some string) None
          & info [] ~docv:"PATH"
              ~doc:"The directory to list instead of the top one."))

let ls_cmd =
  let action repo knot path ref_ json =
    Common.run (fun env ->
        let out =
          Common.with_api ~busy:"Reading tree" env (fun api ->
              let path = if path = "" then None else Some path in
              Api.tree api (Common.resolve api ?knot repo) ?ref_ ?path ())
        in
        let entries =
          List.map (Api.decode Git.GetTree.tree_entry_jsont) out.entries
        in
        (* Modes are octal, with or without leading zeros. *)
        let is_dir (e : Git.GetTree.tree_entry) =
          match int_of_string_opt ("0o" ^ e.mode) with
          | Some m -> m land 0o170000 = 0o040000
          | None -> false
        in
        if json then Common.print Git.GetTree.output_jsont out
        else if Ui.tty () then
          Ui.table
            [ Ui.column "Name"; Ui.column ~align:`Right "Size" ]
            (List.map
               (fun (e : Git.GetTree.tree_entry) ->
                 if is_dir e then [ Ui.accent (e.name ^ "/"); Ui.text "" ]
                 else [ Ui.text e.name; Ui.muted (string_of_int e.size) ])
               entries)
        else
          List.iter
            (fun (e : Git.GetTree.tree_entry) ->
              print_endline (if is_dir e then e.name ^ "/" else e.name))
            entries)
  in
  Common.cmd
    (Cmd.info "ls" ~doc:"List the files in a directory of the repository.")
    Term.(
      term_result
        (const action $ Common.repo $ Common.knot $ path_arg ~required:false
       $ ref_opt $ Common.json_flag))

let cat_cmd =
  let action repo knot path ref_ =
    Common.run (fun env ->
        print_string
          (Common.with_api ~busy:("Reading " ^ path) env (fun api ->
               Api.blob api (Common.resolve api ?knot repo) ?ref_ ~path ())))
  in
  Common.cmd
    (Cmd.info "cat" ~doc:"Print a file from the repository.")
    Term.(
      term_result
        (const action $ Common.repo $ Common.knot $ path_arg ~required:true
       $ ref_opt))

let diff_cmd =
  let base =
    Common.positional 0 "BASE" "The branch, tag or commit to compare from."
  in
  let head =
    Common.positional 1 "HEAD" "The branch, tag or commit to compare to."
  in
  let stat =
    Arg.(
      value & flag
      & info [ "stat" ] ~doc:"List the files changed instead of the changes.")
  in
  let action repo knot base head stat =
    Common.run (fun env ->
        Common.with_api env (fun api ->
            let r =
              Ui.busy "Resolving repository" (fun () ->
                  Common.resolve api ?knot repo)
            in
            Diff_view.show api r ~base ~head ~stat))
  in
  Common.cmd
    (Cmd.info "diff" ~doc:"Show the changes between two commits.")
    Term.(
      term_result
        (const action $ Common.repo $ Common.knot $ base $ head $ stat))

let cmds = [ branch_group; tags_cmd; log_cmd; ls_cmd; cat_cmd; diff_cmd ]
