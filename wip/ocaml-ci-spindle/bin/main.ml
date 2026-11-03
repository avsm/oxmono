(* SPDX-License-Identifier: ISC *)
open Cmdliner
open Ocaml_ci_spindle

let options =
  let open Arg in
  Term.(
    const (fun cap pool cache compilers oxcaml ->
        (cap, pool, cache, compilers, oxcaml))
    $ required
        (opt (some file) None
        & info [ "ocluster-cap" ] ~docv:"FILE"
            ~doc:"The OCluster scheduler's submission capability file.")
    $ value
        (opt string "linux-x86_64"
        & info [ "pool" ] ~docv:"POOL" ~doc:"The OCluster pool to build on.")
    $ value
        (opt string "opam-cache"
        & info [ "opam-cache" ] ~docv:"DIR"
            ~doc:"Where the opam repositories are cloned to solve.")
    $ value
        (opt (list string) [ "auto" ]
        & info [ "compilers" ] ~docv:"MINORS"
            ~doc:
              "OCaml minor versions to build with, such as $(b,4.14,5.4). \
               $(b,auto) picks 4.14 and the two newest.")
    $ value
        (flag
        & info [ "oxcaml" ]
            ~doc:"Also build with OxCaml 5.2.0minus39, as oxmono does."))

let () =
  Spindle_cli.main ~name:"ocaml-ci-spindle"
    ~doc:"Run a Tangled spindle that tests OCaml projects on OCluster" options
    (fun env ~sw:_ (cap, pool, cache, compilers, oxcaml) ->
      let repo name url =
        Opam_repo.v env#process_mgr env#clock ~dir:cache ~name url
      in
      let config =
        {
          Ci.cap;
          pool;
          platform = Solve.debian_12_x86_64;
          opam_repository =
            repo "opam-repository"
              "https://github.com/ocaml/opam-repository.git";
          oxcaml_repository =
            (if oxcaml then
               Some
                 (repo "oxcaml-repository"
                    "https://github.com/oxcaml/opam-repository.git")
             else None);
          compilers;
        }
      in
      (Ci.jobs, Ci.executor ~net:env#net config))
