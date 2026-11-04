(* SPDX-License-Identifier: ISC *)
open Cmdliner
open Ocaml_ci_spindle

let options =
  let open Arg in
  Term.(
    const (fun cap cache compilers platforms oxcaml ->
        (cap, cache, compilers, platforms, oxcaml))
    $ required
        (opt (some file) None
        & info [ "ocluster-cap" ] ~docv:"FILE"
            ~doc:"The OCluster scheduler's submission capability file.")
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
        (opt
           (list (enum Variant.platforms))
           Variant.[ Debian Arm64; Debian Ppc64le; Debian Riscv64; Windows ]
        & info [ "platforms" ] ~docv:"PLATFORMS"
            ~doc:
              "Platforms that build the newest compiler once its Debian 13 \
               x86_64 build passes, from $(b,arm64), $(b,ppc64le), \
               $(b,riscv64) and $(b,windows). All four by default.")
    $ value
        (flag
        & info [ "oxcaml" ]
            ~doc:"Also build with OxCaml 5.2.0minus39, as oxmono does."))

let () =
  Spindle_cli.main ~name:"ocaml-ci-spindle" ~workflow_timeout:10800.
    ~repository_workflows:false
    ~doc:"Run a Tangled spindle that tests OCaml projects on OCluster" options
    (fun env ~sw (cap, cache, compilers, platforms, oxcaml) ->
      let repo name url =
        Opam_repo.v env#process_mgr env#mono_clock ~dir:cache ~name url
      in
      let config =
        {
          Ci.scheduler = Ocluster_client.scheduler ~sw env#net cap;
          clock = (env#mono_clock :> Eio.Time.Mono.ty Eio.Resource.t);
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
          platforms;
        }
      in
      (Ci.jobs, Ci.executor config))
