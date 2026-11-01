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

let run config with_packages dry_run target args =
  guard @@ fun () ->
  let prepared =
    with_runtime @@ fun proc fs clock sys ->
    Ox_lib.Runner.prepare ~sys proc ~clock ~fs config ~target ~with_packages
      ~dry_run
  in
  if not dry_run then Ox_lib.Runner.exec prepared args

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
    Term.(ret (const run $ config_term $ with_packages $ dry $ target $ args))

let dry_arg =
  Arg.(
    value & flag
    & info [ "n"; "dry-run" ] ~doc:"Display selected packages without building.")

let local_arg =
  Arg.(
    value & flag
    & info [ "local" ]
        ~doc:"Use the editable Git working tree and scoped Dune targets.")

let roots_arg = Arg.(value & pos_all string [] & info [] ~docv:"PACKAGE")

let all_arg =
  Arg.(
    value & flag
    & info [ "all" ]
        ~doc:"Build all packages from the selected source snapshot.")

let build_command ~test config roots all local deps_only fetch depext dry
    profile =
  guard @@ fun () ->
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
        $ dry_arg $ profile))

let environment_command config packages command =
  guard @@ fun () ->
  let prepared =
    with_runtime (fun proc fs clock sys ->
        if config.Ox_lib.Runner.from <> None || packages <> [] then
          Ox_lib.Runner.packages proc ~clock ~fs ~sys config ~roots:packages
            ~all:(packages = []) ~action:Build ~dry_run:false ()
        else
          (Ox_lib.Workspace.prepare proc ~clock ~fs ~sys config ~roots:[]
             ~action:Build ~dry_run:false)
            .prepared)
  in
  match command with
  | [] -> Ox_lib.Runner.exports prepared
  | args -> Ox_lib.Runner.exec_command prepared args

let env_cmd =
  Cmd.v
    (Cmd.info "env"
       ~doc:"Print shell exports for project or package dependencies.")
    Term.(
      ret
        (const (fun config packages -> environment_command config packages [])
        $ config_term $ with_packages))

let exec_cmd =
  let command = Arg.(non_empty & pos_all string [] & info [] ~docv:"COMMAND") in
  Cmd.v
    (Cmd.info "exec"
       ~doc:"Execute a command with project or package dependencies.")
    Term.(
      ret (const environment_command $ config_term $ with_packages $ command))

let show_cmd =
  Cmd.v
    (Cmd.info "show" ~doc:"Resolve and list package build dependencies.")
    Term.(
      ret
        (const (fun config roots all local ->
             build_command ~test:false config roots all local false false false
               true "release")
        $ config_term $ roots_arg $ all_arg $ local_arg))

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
  Cmd.group
    (Cmd.info "dist" ~doc:"Generate native Linux distribution packages.")
    [ pkg ]

let () =
  let cmd =
    Cmd.group
      (Cmd.info "ox" ~version:"0.1.0"
         ~doc:"Build and run opam packages with OxCaml and a local day10 cache.")
      [
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
