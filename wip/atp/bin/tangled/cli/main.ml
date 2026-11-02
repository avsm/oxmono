(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Cmdliner

let version = "0.2.0"
let app_name = "tangled"

let main_cmd =
  let doc = "work with Git repositories on Tangled" in
  let man =
    [
      `S Manpage.s_description;
      `P
        "$(mname) creates and browses repositories on Tangled, opens and \
         reviews issues and pull requests, and follows their CI. Tangled is a \
         place to host Git repositories and collaborate on them, built on the \
         AT Protocol that Bluesky also uses.";
      `P "These words name the parts of Tangled:";
      `I
        ( "account",
          "You sign in with an AT Protocol account, such as a Bluesky account. \
           A handle such as $(i,alice.bsky.social) names it. Your stars, SSH \
           keys and profile are kept in your account." );
      `I
        ( "knot",
          "A server that hosts Git repositories. Anyone can run one. You \
           create a repository on a knot, then clone it and push to it as with \
           any Git server." );
      `I
        ( "repository",
          "A Git repository on a knot, named $(i,owner/name), such as \
           $(i,alice.bsky.social/demo). Its owner may let collaborators push \
           to it." );
      `I
        ( "issue",
          "A report or request filed against a repository, which people \
           discuss in comments until it is closed." );
      `I
        ( "pull request",
          "A proposal to merge a branch into a repository, which reviewers \
           comment on and its author revises." );
      `I
        ( "label",
          "A coloured tag, such as $(i,bug), that sorts issues and pull \
           requests." );
      `I
        ( "spindle",
          "A server that runs CI. When you push, it runs the workflows in the \
           repository's $(i,.tangled/workflows) directory." );
      `I
        ( "pipeline",
          "One CI run for one commit: the workflows the spindle ran and \
           whether each passed." );
      `S Manpage.s_commands;
      `P "Run $(mname) $(i,COMMAND) --help for help on a command.";
      `P
        "Commands that work in a repository use the one the current directory \
         is a clone of, unless $(b,--repo) names another.";
      `S Manpage.s_examples;
      `P "Sign in, create a repository and push to it:";
      `Pre
        "\\$ tangled auth login --oauth --handle alice.bsky.social\n\
         \\$ tangled key add ~/.ssh/id_ed25519.pub --name laptop\n\
         \\$ tangled repo create demo --knot knot.example.com\n\
         \\$ tangled repo clone demo && cd demo\n\
         \\$ git commit --allow-empty -m 'First commit' && git push";
      `P "Propose a change from a branch and watch its CI:";
      `Pre
        "\\$ git switch -c fix && git commit -am 'Fix the build'\n\
         \\$ git push -u origin fix\n\
         \\$ tangled pull create --title 'Fix the build'\n\
         \\$ tangled pipeline logs";
      `S "OUTPUT";
      `P
        "On a terminal, results are drawn as tables and panels. When output \
         goes to a pipe or a file, it is plain text, one item to a line, or \
         JSON where a command says so, for scripts to read. $(b,--json) asks \
         for JSON wherever a command offers it.";
      `S Manpage.s_environment;
      `I
        ( "$(b,TANGLED_KNOT)",
          "The knot $(b,repo create) uses when $(b,--knot) is not given." );
      `I
        ( "$(b,TANGLED_PDS)",
          "The address of your account's server. A password login needs it for \
           an account hosted outside Bluesky. Other commands default to the \
           server of the saved login." );
      `I
        ( "$(b,TANGLED_PASSWORD)",
          "The app password $(b,auth login) uses instead of asking for one." );
      `I
        ( "$(b,TANGLED_COLOR), $(b,TANGLED_THEME)",
          "Defaults for $(b,--color) and $(b,--theme)." );
      `I
        ( "$(b,SSL_CERT_FILE)",
          "A file of certificate authorities to trust instead of the system's."
        );
      `I
        ( "$(b,XDG_CONFIG_HOME)",
          "Saved logins are kept in its $(i,tangled) directory. The default is \
           $(i,~/.config/tangled)." );
    ]
  in
  let info = Cmd.info "tangled" ~version ~doc ~man in
  Cmd.group info
    [
      Cmd_auth.cmd;
      Cmd_repo.cmd;
      Cmd_knot.cmd;
      Cmd_profile.cmd;
      Cmd_star.cmd;
      Cmd_pipeline.cmd;
      Cmd_spindle.cmd;
      Cmd_api.cmd;
      Cmd_issue.cmd;
      Cmd_issue.pull_cmd;
      Cmd_key.cmd;
    ]

let () =
  Ui.setup ();
  exit (Cmd.eval main_cmd)
