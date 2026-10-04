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

let run cache data toolchain repositories overlays from revision refresh jobs
    cache_tag with_packages dry_run target args =
  guard @@ fun () ->
  let config : Ox_lib.Runner.config =
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
  let prepared =
    Eio_main.run @@ fun env ->
    let sys =
      D10.Sysops.v
        ~proc_mgr:(Eio.Stdenv.process_mgr env)
        ~fs:(Eio.Stdenv.fs env) ~net:(Eio.Stdenv.net env)
        ~clock:(Eio.Stdenv.clock env) ()
    in
    Ox_lib.Runner.prepare ~sys
      (Eio.Stdenv.process_mgr env)
      ~clock:(Eio.Stdenv.clock env) ~fs:(Eio.Stdenv.fs env) config ~target
      ~with_packages ~dry_run
  in
  if not dry_run then Ox_lib.Runner.exec prepared args

let run_cmd =
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
  let with_packages =
    Arg.(
      value & opt_all string []
      & info [ "with" ] ~docv:"PACKAGE"
          ~doc:"Package atoms providing the binary and additional dependencies.")
  in
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
        (const run $ cache $ data $ toolchain $ repositories $ overlays $ from
       $ revision $ refresh $ jobs $ tag $ with_packages $ dry $ target $ args))

let () =
  let cmd =
    Cmd.group
      (Cmd.info "ox" ~version:"0.1.0"
         ~doc:"Run opam packages with OxCaml and a per-user local cache.")
      [ run_cmd; stamp_cmd ]
  in
  exit (Cmd.eval cmd)
