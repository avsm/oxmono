open Cmdliner
module S = Ox_lib.Support

let guard f =
  try
    f ();
    `Ok ()
  with
  | Failure message -> `Error (false, message)
  | (OpamPp.Bad_format _ | OpamPp.Bad_format_list _ | OpamPp.Bad_version _) as
    exn ->
      `Error (false, Printexc.to_string exn)
  | Eio.Exn.Io _ as exn -> `Error (false, Printexc.to_string exn)
  | Unix.Unix_error (e, fn, arg) ->
      `Error (false, Printf.sprintf "%s(%s): %s" fn arg (Unix.error_message e))

let data =
  Arg.(value & opt string (S.data_dir ()) & info [ "data-dir" ] ~docv:"DIR")

let cache =
  Arg.(value & opt string (S.cache_dir ()) & info [ "cache-dir" ] ~docv:"DIR")

let stamp repo revision source output data =
  guard @@ fun () ->
  Eio_main.run @@ fun env ->
  let revision = Option.value revision ~default:"HEAD" in
  let output = Option.value output ~default:(Filename.concat data "overlay") in
  let snapshot =
    Ox_lib.Stamp.export
      (Eio.Stdenv.process_mgr env)
      ~repo ~revision ~source ~output
  in
  List.iter
    (fun p -> Printf.printf "%s.%s\n" p.Ox_lib.Stamp.name p.version)
    snapshot.packages;
  S.log "Wrote %d packages to %s" (List.length snapshot.packages) output

let revision =
  Arg.(
    value
    & opt (some string) None
    & info [ "ref" ] ~docv:"REV"
        ~doc:
          "Committed source revision. Overrides the --from URL fragment. \
           Defaults to HEAD. Uncommitted files are not included.")

let stamp_cmd =
  let repo = Arg.(value & pos 0 string "." & info [] ~docv:"REPO") in
  let source =
    Arg.(
      value
      & opt (some string) None
      & info [ "source" ] ~docv:"GIT-URL"
          ~doc:
            "Shareable source URL. Defaults to a Git file URL for this \
             checkout.")
  in
  let output =
    Arg.(
      value
      & opt (some string) None
      & info [ "output"; "o" ] ~docv:"DIR"
          ~doc:
            "New opam repository directory. Existing directories are never \
             replaced.")
  in
  Cmd.v
    (Cmd.info "stamp"
       ~doc:"Export committed monorepo packages as an opam repository.")
    Term.(ret (const stamp $ repo $ revision $ source $ output $ data))

let with_runtime f =
  Eio_main.run @@ fun env ->
  let proc = Eio.Stdenv.process_mgr env in
  let fs = Eio.Stdenv.fs env and clock = Eio.Stdenv.clock env in
  let sys =
    D10.Sysops.v ~proc_mgr:proc ~fs ~net:(Eio.Stdenv.net env) ~clock ()
  in
  f proc fs clock sys

let user_arg =
  Arg.(
    value
    & opt (some string) None
    & info [ "user" ] ~docv:"OWNER"
        ~doc:"Select one owner's manifest and an isolated source workspace.")

let owner_workspace config owner =
  Eio_main.run @@ fun env ->
  let proc = Eio.Stdenv.process_mgr env in
  let selected = Ox_lib.Layout.select proc ~repo:"." ~owner:(Some owner) () in
  let config = Ox_lib.Layout.locked_config ~repo:selected.root config in
  let output =
    Filename.concat config.data
      (Printf.sprintf "owners/%s/%d-%.0f" owner (Unix.getpid ())
         (Unix.gettimeofday () *. 1e6))
  in
  Ox_lib.Layout.stage proc selected ~output;
  S.command proc [ "git"; "init"; "-q"; output ];
  Unix.chdir output;
  (config, selected.roots)

let run config with_packages dry_run target args user local =
  guard @@ fun () ->
  if Filename.basename target <> target || List.mem target [ "."; ".." ] then
    S.fail "Expected a binary name: %s" target;
  if local && user <> None then S.fail "--local cannot be combined with --user";
  if local then (
    let workspace =
      with_runtime @@ fun proc fs clock sys ->
      Ox_lib.Workspace.prepare ~binary:target proc ~clock ~fs ~sys config
        ~roots:with_packages ~action:Build ~dry_run
    in
    if not dry_run then
      Ox_lib.Workspace.run workspace ~profile:"release" ~jobs:config.jobs
        ~binary:target args)
  else
    match user with
    | None ->
        let prepared =
          with_runtime @@ fun proc fs clock sys ->
          Ox_lib.Runner.prepare ~sys proc ~clock ~fs config ~target
            ~with_packages ~dry_run
        in
        if not dry_run then Ox_lib.Runner.exec prepared args
    | Some owner ->
        if
          with_packages <> []
          || config.Ox_lib.Runner.from <> None
          || config.revision <> None
        then S.fail "--user cannot be combined with --with, --from or --ref";
        let cwd = Sys.getcwd () in
        let config, _roots = owner_workspace config owner in
        let workspace =
          with_runtime @@ fun proc fs clock sys ->
          Ox_lib.Workspace.prepare ~binary:target proc ~clock ~fs ~sys config
            ~roots:[] ~action:Build ~dry_run
        in
        Unix.chdir cwd;
        if not dry_run then
          Ox_lib.Workspace.run workspace ~profile:"release" ~jobs:config.jobs
            ~binary:target args

let config_term =
  let toolchain =
    Arg.(
      value & opt string "oxcaml"
      & info [ "toolchain" ] ~docv:"PACKAGE"
          ~doc:"OxCaml toolchain package atom to build.")
  in
  let repositories =
    Arg.(
      value & opt_all string []
      & info [ "repository" ] ~docv:"REPO"
          ~doc:
            "Opam repository path or Git URL, highest priority first. \
             Supplying this replaces the default OxCaml and ordinary opam \
             repositories.")
  in
  let overlays =
    Arg.(
      value & opt_all string []
      & info [ "overlay" ] ~docv:"REPO"
          ~doc:"Additional opam repository before the base repositories.")
  in
  let from =
    Arg.(
      value
      & opt (some string) None
      & info [ "from" ] ~docv:"GIT-REPO[#REV]"
          ~doc:
            "Stamp packages from a checkout or clone a Git URL, optionally \
             followed by #BRANCH, #TAG or #COMMIT, then resolve from that \
             snapshot first.")
  in
  let refresh =
    Arg.(
      value & flag
      & info [ "refresh" ]
          ~doc:
            "Fetch updates in cached Git sources and repositories before \
             resolving. Local checkouts are read without updating them.")
  in
  let jobs = Arg.(value & opt int 4 & info [ "jobs"; "j" ] ~docv:"N") in
  let tag =
    Arg.(
      value & opt string ""
      & info [ "cache-tag" ] ~docv:"TAG"
          ~doc:
            "Additional cache identity, e.g. after changing system libraries.")
  in
  let make cache data toolchain repositories overlays from revision refresh jobs
      cache_tag : Ox_lib.Runner.config =
    {
      cache;
      data;
      toolchain;
      repositories;
      overlays;
      from;
      revision;
      refresh;
      jobs;
      cache_tag;
    }
  in
  Term.(
    const make $ cache $ data $ toolchain $ repositories $ overlays $ from
    $ revision $ refresh $ jobs $ tag)

let with_packages =
  Arg.(
    value & opt_all string []
    & info [ "with" ] ~docv:"PACKAGE"
        ~doc:"Package atoms to include in the prepared environment.")

let local_arg =
  Arg.(
    value & flag
    & info [ "local" ]
        ~doc:"Use the editable Git working tree and scoped Dune targets.")

let run_cmd =
  let dry =
    Arg.(
      value & flag
      & info [ "n"; "dry-run" ]
          ~doc:
            "Resolve and display day10 build actions without fetching or \
             building packages.")
  in
  let target =
    Arg.(required & pos 0 (some string) None & info [] ~docv:"BINARY")
  in
  let args = Arg.(value & pos_right 0 string [] & info [] ~docv:"ARG") in
  Cmd.v
    (Cmd.info "run" ~doc:"Fetch dependencies, build, cache and run a binary.")
    Term.(
      ret
        (const run $ config_term $ with_packages $ dry $ target $ args
       $ user_arg $ local_arg))

let dry_arg =
  Arg.(
    value & flag
    & info [ "n"; "dry-run" ] ~doc:"Display selected packages without building.")

let roots_arg = Arg.(value & pos_all string [] & info [] ~docv:"PACKAGE")

let all_arg =
  Arg.(
    value & flag
    & info [ "all" ]
        ~doc:"Build all packages from the selected source snapshot.")

let build_command ~test config roots all local deps_only fetch depext dry
    profile user =
  guard @@ fun () ->
  let config, roots, local =
    match user with
    | None -> (config, roots, local)
    | Some owner ->
        if
          roots <> [] || all
          || config.Ox_lib.Runner.from <> None
          || config.revision <> None
        then
          S.fail
            "--user cannot be combined with package roots, --all, --from or \
             --ref";
        let config, roots = owner_workspace config owner in
        (config, roots, true)
  in
  if (fetch && depext) || ((test || deps_only) && (fetch || depext)) then
    S.fail "--fetch and --depext are separate build actions";
  let local =
    local || (roots = [] && config.Ox_lib.Runner.from = None && not all)
  in
  let action =
    if depext then Ox_lib.Runner.Depexts
    else if fetch then Fetch
    else if test then Test
    else Build
  in
  if local then (
    if all then S.fail "--local cannot be combined with --all";
    let workspace =
      with_runtime (fun proc fs clock sys ->
          Ox_lib.Workspace.prepare proc ~clock ~fs ~sys config ~roots ~action
            ~dry_run:dry)
    in
    if (not dry) && (action = Build || action = Test) then
      if deps_only then print_endline (Ox_lib.Runner.prefix workspace.prepared)
      else Ox_lib.Workspace.execute workspace ~test ~profile ~jobs:config.jobs)
  else (
    if deps_only then S.fail "--deps-only is for local project builds";
    let prepared =
      with_runtime (fun proc fs clock sys ->
          Ox_lib.Runner.packages proc ~clock ~fs ~sys config ~roots ~all ~action
            ~dry_run:dry ())
    in
    if (not dry) && (action = Build || action = Test) then
      print_endline (Ox_lib.Runner.prefix prepared))

let build_cmd test =
  let deps =
    Arg.(
      value & flag
      & info [ "deps-only" ]
          ~doc:"Prepare the local project's dependencies without running Dune.")
  in
  let fetch =
    Arg.(
      value & flag
      & info [ "fetch" ] ~doc:"Fetch selected package sources without building.")
  in
  let depext =
    Arg.(
      value & flag
      & info [ "depext" ]
          ~doc:"Print required system packages without building.")
  in
  let profile =
    Arg.(
      value & opt string "release"
      & info [ "profile" ] ~docv:"PROFILE" ~doc:"Dune profile for local builds.")
  in
  Cmd.v
    (Cmd.info
       (if test then "test" else "build")
       ~doc:
         (if test then "Run package tests or local Dune tests."
          else
            "Build packages or the editable local project with day10 \
             dependencies."))
    Term.(
      ret
        (const (build_command ~test)
        $ config_term $ roots_arg $ all_arg $ local_arg $ deps $ fetch $ depext
        $ dry_arg $ profile $ user_arg))

let environment_command config packages command user =
  guard @@ fun () ->
  let cwd = Sys.getcwd () in
  let config, owner_roots =
    match user with
    | None -> (config, None)
    | Some owner ->
        if
          packages <> []
          || config.Ox_lib.Runner.from <> None
          || config.revision <> None
        then S.fail "--user cannot be combined with --with, --from or --ref";
        let config, roots = owner_workspace config owner in
        (config, Some roots)
  in
  let prepared =
    with_runtime (fun proc fs clock sys ->
        if owner_roots <> None then
          (Ox_lib.Workspace.prepare proc ~clock ~fs ~sys config
             ~roots:(Option.get owner_roots) ~action:Build ~dry_run:false)
            .prepared
        else if config.Ox_lib.Runner.from <> None || packages <> [] then
          Ox_lib.Runner.packages proc ~clock ~fs ~sys config ~roots:packages
            ~all:(packages = []) ~action:Build ~dry_run:false ()
        else
          (Ox_lib.Workspace.prepare proc ~clock ~fs ~sys config ~roots:[]
             ~action:Build ~dry_run:false)
            .prepared)
  in
  Unix.chdir cwd;
  match command with
  | [] -> Ox_lib.Runner.exports prepared
  | args -> Ox_lib.Runner.exec_command prepared args

let env_cmd =
  Cmd.v
    (Cmd.info "env"
       ~doc:"Print shell exports for project or package dependencies.")
    Term.(
      ret
        (const (fun config packages user ->
             environment_command config packages [] user)
        $ config_term $ with_packages $ user_arg))

let exec_cmd =
  let command = Arg.(non_empty & pos_all string [] & info [] ~docv:"COMMAND") in
  Cmd.v
    (Cmd.info "exec"
       ~doc:"Execute a command with project or package dependencies.")
    Term.(
      ret
        (const environment_command $ config_term $ with_packages $ command
       $ user_arg))

let show_cmd =
  Cmd.v
    (Cmd.info "show" ~doc:"Resolve and list package build dependencies.")
    Term.(
      ret
        (const (fun config roots all local user ->
             build_command ~test:false config roots all local false false false
               true "release" user)
        $ config_term $ roots_arg $ all_arg $ local_arg $ user_arg))

let dist config with_packages tags arch pkg_name pkg_version maintainer output
    build target =
  guard @@ fun () ->
  let targets = Ox_lib.Dist.targets tags arch in
  with_runtime @@ fun proc fs clock sys ->
  Ox_lib.Dist.run proc ~clock ~fs ~sys config ~target ~with_packages ~targets
    ~pkg_name ~pkg_version ~maintainer ~output ~build

let dist_cmd =
  let tags =
    Arg.(
      value & opt string "debian-13"
      & info [ "distros"; "target" ] ~docv:"TAGS"
          ~doc:"Comma-separated osdist targets. Defaults to debian-13.")
  in
  let arch =
    Arg.(
      value & opt string "x86_64"
      & info [ "arch" ] ~docv:"ARCH"
          ~doc:
            "Package architecture: x86_64 or aarch64. Containers use this \
             platform.")
  in
  let pkg_name =
    Arg.(
      value
      & opt (some string) None
      & info [ "pkg-name" ] ~docv:"NAME"
          ~doc:"Native package name. Defaults to the first root package.")
  in
  let pkg_version =
    Arg.(
      value
      & opt (some string) None
      & info [ "pkg-version" ] ~docv:"VERSION"
          ~doc:"Override the resolved package version.")
  in
  let maintainer =
    Arg.(
      value
      & opt (some string) None
      & info [ "maintainer" ] ~docv:"NAME <EMAIL>"
          ~doc:"Override the opam package maintainer.")
  in
  let output =
    Arg.(
      required
      & opt (some string) None
      & info [ "o"; "output" ] ~docv:"DIR"
          ~doc:"New directory for source bundles and packaging files.")
  in
  let build =
    Arg.(
      value & flag
      & info [ "build" ]
          ~doc:"Build the generated packages using Docker Compose.")
  in
  let target =
    Arg.(required & pos 0 (some string) None & info [] ~docv:"BINARY")
  in
  let pkg =
    Cmd.v
      (Cmd.info "pkg"
         ~doc:"Export source bundles and Debian, RPM or static packaging.")
      Term.(
        ret
          (const dist $ config_term $ with_packages $ tags $ arch $ pkg_name
         $ pkg_version $ maintainer $ output $ build $ target))
  in
  let owner =
    Arg.(required & opt (some string) None & info [ "user" ] ~docv:"OWNER")
  in
  let prepared =
    Arg.(
      value & flag
      & info [ "prepared" ]
          ~doc:
            "Use the current compiler environment instead of preparing day10 \
             dependencies.")
  in
  let profile =
    Arg.(value & opt string "release" & info [ "profile" ] ~docv:"PROFILE")
  in
  let bundle config owner prepared profile output =
    guard @@ fun () ->
    with_runtime @@ fun proc fs clock sys ->
    Ox_lib.Bundle.run proc ~clock ~fs ~sys config ~owner ~prepared ~profile
      ~output
  in
  let bundle_cmd =
    Cmd.v
      (Cmd.info "bundle"
         ~doc:"Build and install one owner's commands and assets.")
      Term.(
        ret (const bundle $ config_term $ owner $ prepared $ profile $ output))
  in
  let image owner arch tag =
    guard @@ fun () ->
    if not (Ox_lib.Layout.valid_owner owner) then S.fail "Invalid owner";
    if not (List.mem arch [ "amd64"; "arm64" ]) then
      S.fail "Image architecture must be amd64 or arm64";
    Eio_main.run @@ fun env ->
    let proc = Eio.Stdenv.process_mgr env in
    let root = S.git proc "." [ "rev-parse"; "--show-toplevel" ] in
    S.command proc
      [
        "docker";
        "buildx";
        "build";
        "--load";
        "--platform";
        "linux/" ^ arch;
        "-f";
        Filename.concat root ("u/" ^ owner ^ "/Dockerfile");
        "-t";
        tag;
        root;
      ]
  in
  let image_arch =
    Arg.(value & opt string "amd64" & info [ "arch" ] ~docv:"ARCH")
  in
  let tag =
    Arg.(required & opt (some string) None & info [ "t"; "tag" ] ~docv:"TAG")
  in
  let image_cmd =
    Cmd.v
      (Cmd.info "image" ~doc:"Build one owner's Linux runtime image.")
      Term.(ret (const image $ owner $ image_arch $ tag))
  in
  Cmd.group
    (Cmd.info "dist"
       ~doc:"Build owner bundles, images and distribution packages.")
    [ pkg; bundle_cmd; image_cmd ]

let init_cmd =
  let owner =
    Arg.(required & opt (some string) None & info [ "user" ] ~docv:"OWNER")
  in
  let run owner =
    guard @@ fun () ->
    Eio_main.run @@ fun env ->
    Ox_lib.Layout.init (Eio.Stdenv.process_mgr env) ~repo:"." ~owner
  in
  Cmd.v
    (Cmd.info "init"
       ~doc:"Create an independent user workspace and container recipe.")
    Term.(ret (const run $ owner))

let inventory_cmd =
  let check =
    Arg.(
      value & flag
      & info [ "check" ] ~doc:"Validate selected package and binary ownership.")
  in
  let output =
    Arg.(
      value
      & opt (some string) None
      & info [ "stage" ] ~docv:"DIR" ~doc:"Write an isolated source workspace.")
  in
  let run owner _check output =
    guard @@ fun () ->
    Eio_main.run @@ fun env ->
    let proc = Eio.Stdenv.process_mgr env in
    let selected = Ox_lib.Layout.select proc ~repo:"." ~owner () in
    Option.iter (fun output -> Ox_lib.Layout.stage proc selected ~output) output;
    print_endline (OpamJson.to_string (Ox_lib.Layout.json selected))
  in
  Cmd.v
    (Cmd.info "inventory" ~doc:"Inspect packages and owner source closures.")
    Term.(ret (const run $ user_arg $ check $ output))

let release_cmd =
  let repo = Arg.(value & opt string "." & info [ "repo" ] ~docv:"DIR") in
  let revision =
    Arg.(value & opt string "HEAD" & info [ "ref" ] ~docv:"COMMIT")
  in
  let output =
    Arg.(
      required & opt (some string) None & info [ "o"; "output" ] ~docv:"PATH")
  in
  let tag =
    Arg.(value & opt string "" & info [ "tag" ] ~docv:"YYYYMMDD-EPOCH")
  in
  let base_url =
    Arg.(value & opt (some string) None & info [ "base-url" ] ~docv:"URL")
  in
  let guards =
    Arg.(
      value
      & opt (some string) None
      & info [ "guard-repository" ] ~docv:"DIR"
          ~doc:"Pinned OxCaml overlay whose guard allowlists are extended.")
  in
  let previous =
    Arg.(
      value
      & opt (some string) None
      & info [ "previous" ] ~docv:"RELEASE-DIR"
          ~doc:
            "Reuse unchanged distfiles and package versions from an earlier \
             release.")
  in
  let plan_file =
    Arg.(value & opt (some string) None & info [ "plan" ] ~docv:"JSON")
  in
  let url = function
    | Some s -> s
    | None -> S.fail "Supply --base-url or --plan"
  in
  let plan repo revision packages owner tag base_url guard_repository previous
      output =
    guard @@ fun () ->
    Eio_main.run @@ fun env ->
    if S.exists output then S.fail "Output already exists: %s" output;
    let json =
      Ox_lib.Release.plan
        (Eio.Stdenv.process_mgr env)
        ~repo ~revision ~packages ~owner ~tag ~base_url:(url base_url)
        ~guard_repository ~previous
    in
    S.atomic_write output (OpamJson.to_string json)
  in
  let plan_cmd =
    Cmd.v
      (Cmd.info "plan" ~doc:"Record a committed release request for review.")
      Term.(
        ret
          (const plan $ repo $ revision $ roots_arg $ user_arg $ tag $ base_url
         $ guards $ previous $ output))
  in
  let export repo revision packages owner tag base_url guard_repository previous
      plan_file output =
    guard @@ fun () ->
    Eio_main.run @@ fun env ->
    let ( repo,
          revision,
          packages,
          owner,
          tag,
          base_url,
          guard_repository,
          previous ) =
      match plan_file with
      | None ->
          ( repo,
            revision,
            packages,
            owner,
            tag,
            url base_url,
            guard_repository,
            previous )
      | Some path ->
          if
            repo <> "." || revision <> "HEAD" || packages <> [] || owner <> None
            || tag <> "" || base_url <> None || guard_repository <> None
          then S.fail "--plan cannot be combined with release selection options";
          let fields =
            match OpamJson.of_string (S.read path) with
            | Some (`O f) -> f
            | _ -> S.fail "Invalid release plan"
          in
          let str key =
            match List.assoc_opt key fields with
            | Some (`String s) -> s
            | _ -> S.fail "Missing plan field %s" key
          in
          let optional key =
            match List.assoc_opt key fields with
            | Some (`String s) -> Some s
            | Some `Null -> None
            | _ -> S.fail "Invalid plan field %s" key
          in
          if List.assoc_opt "schema_version" fields <> Some (`Float 1.) then
            S.fail "Unsupported plan schema";
          let packages =
            match List.assoc_opt "packages" fields with
            | Some (`A xs) ->
                List.map
                  (function
                    | `String s -> s | _ -> S.fail "Invalid plan package")
                  xs
            | _ -> S.fail "Missing plan packages"
          in
          let planned_previous = optional "previous" in
          if
            previous <> None
            && Option.map Unix.realpath previous <> planned_previous
          then S.fail "--previous differs from the release plan";
          if
            Option.map
              (fun p -> S.hash_file (Filename.concat p "manifest.json"))
              planned_previous
            <> optional "previous_hash"
          then S.fail "Previous release manifest changed since planning";
          let tag = str "tag" in
          let guards = optional "guard_repository" in
          if Option.map S.tree_hash guards <> optional "guard_hash" then
            S.fail "Release guard inputs changed since planning";
          ( str "repo",
            str "commit",
            packages,
            optional "owner",
            tag,
            str "base_url",
            guards,
            planned_previous )
    in
    Ox_lib.Release.export
      (Eio.Stdenv.process_mgr env)
      ~repo ~revision ~packages ~owner ~tag ~base_url ~guard_repository
      ~previous ~output
  in
  let export_cmd =
    Cmd.v
      (Cmd.info "export"
         ~doc:"Export committed project archives and an opam overlay.")
      Term.(
        ret
          (const export $ repo $ revision $ roots_arg $ user_arg $ tag
         $ base_url $ guards $ previous $ plan_file $ output))
  in
  let directory =
    Arg.(required & pos 0 (some string) None & info [] ~docv:"RELEASE-DIR")
  in
  let verify directory =
    guard @@ fun () ->
    Eio_main.run @@ fun env ->
    Ox_lib.Release.verify (Eio.Stdenv.process_mgr env) ~directory
  in
  let verify_cmd =
    Cmd.v
      (Cmd.info "verify" ~doc:"Verify release archive and metadata checksums.")
      Term.(ret (const verify $ directory))
  in
  let create_tag repo revision tag =
    guard @@ fun () ->
    if not (Ox_lib.Release.valid_tag tag) then
      S.fail "Supply --tag YYYYMMDD-EPOCH with a valid date and positive epoch";
    Eio_main.run @@ fun env ->
    let proc = Eio.Stdenv.process_mgr env in
    if S.git proc repo [ "status"; "--porcelain" ] <> "" then
      S.fail "Commit working-tree changes before creating a release tag";
    S.command proc
      [
        "git";
        "-C";
        repo;
        "tag";
        "-a";
        tag;
        "-m";
        "OxMono release " ^ tag;
        revision;
      ]
  in
  let tag_cmd =
    Cmd.v
      (Cmd.info "tag"
         ~doc:
           "Create an annotated YYYYMMDD-EPOCH release tag from committed \
            sources.")
      Term.(ret (const create_tag $ repo $ revision $ tag))
  in
  Cmd.group
    (Cmd.info "release"
       ~doc:"Create and verify source releases for ordinary opam clients.")
    [ tag_cmd; plan_cmd; export_cmd; verify_cmd ]

let () =
  let cmd =
    Cmd.group
      (Cmd.info "ox" ~version:"0.1.0"
         ~doc:"Build and run opam packages with OxCaml and a local day10 cache.")
      [
        init_cmd;
        release_cmd;
        inventory_cmd;
        run_cmd;
        build_cmd false;
        build_cmd true;
        env_cmd;
        exec_cmd;
        show_cmd;
        stamp_cmd;
        dist_cmd;
      ]
  in
  exit (Cmd.eval cmd)
