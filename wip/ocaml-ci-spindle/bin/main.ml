(* SPDX-License-Identifier: ISC *)
open Cmdliner
open Ocaml_ci_spindle

(* The model is asked only when [--diagnose] is given. Its key comes from a
   file or the environment, never the command line, which others can read. *)
let diagnose =
  let open Arg in
  let make enabled url model key_file =
    if not enabled then None
    else
      let key =
        match key_file with
        | Some f ->
            Some (String.trim (In_channel.with_open_bin f In_channel.input_all))
        | None -> (
            match Sys.getenv_opt "OPENROUTER_API_KEY" with
            | Some "" | None -> None
            | key -> key)
      in
      Some (url, model, key)
  in
  Term.(
    const make
    $ value
        (flag
        & info [ "diagnose" ]
            ~doc:
              "When builds of a pull request that the repository's owner or a \
               collaborator labels $(b,ci:diagnose) fail, add a $(b,diagnose) \
               workflow that asks a language model, with no tools, to explain \
               the failures from their logs and suggest fixes. Logs and \
               project files are sent to $(b,--diagnose-url). Off by default.")
    $ value
        (opt string Diagnose.default_url
        & info [ "diagnose-url" ] ~docv:"URL"
            ~doc:
              "The OpenAI-compatible API to ask, such as \
               $(b,https://openrouter.ai/api/v1).")
    $ value
        (opt string Diagnose.default_model
        & info [ "diagnose-model" ] ~docv:"MODEL" ~doc:"The model to ask.")
    $ value
        (opt (some file) None
        & info [ "diagnose-key-file" ] ~docv:"FILE"
            ~doc:
              "A file holding the API key. $(b,OPENROUTER_API_KEY) is used \
               otherwise, and no key is sent when neither is set."))

let options =
  let open Arg in
  Term.(
    const (fun cap cache compilers platforms full_platforms oxcaml diagnose ->
        (cap, cache, compilers, (platforms, full_platforms), oxcaml, diagnose))
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
        (opt (list (enum Variant.platforms)) Variant.[ Debian Arm64 ]
        & info [ "platforms" ] ~docv:"PLATFORMS"
            ~doc:
              "Platforms that build the newest compiler once its Debian 13 \
               x86_64 build passes, from $(b,arm64), $(b,ppc64le), \
               $(b,riscv64) and $(b,windows). $(b,arm64) by default.")
    $ value
        (opt
           (list (enum Variant.platforms))
           Variant.[ Debian Ppc64le; Debian Riscv64; Windows ]
        & info [ "full-platforms" ] ~docv:"PLATFORMS"
            ~doc:
              "Further platforms built that way only for a pull request that \
               the repository's owner or a collaborator labels $(b,ci:full). \
               $(b,ppc64le), $(b,riscv64) and $(b,windows) by default.")
    $ value
        (flag
        & info [ "oxcaml" ]
            ~doc:"Also build with OxCaml 5.2.0minus39, as oxmono does.")
    $ diagnose)

let () =
  Spindle_cli.main ~name:"ocaml-ci-spindle" ~workflow_timeout:21600.
    ~repository_workflows:false
    ~doc:"Run a Tangled spindle that tests OCaml projects on OCluster" options
    (fun env ~sw (cap, cache, compilers, platforms, oxcaml, diagnose) ->
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
          platforms = fst platforms;
          full_platforms = snd platforms;
          diagnose =
            Option.map
              (fun (url, model, key) ->
                Diagnose.v ?key ~model ~clock:env#mono_clock
                  (Fetch_httpz.v env#net ~clock:env#mono_clock
                     ~https:Httpz_tls.system
                     ~max_response:(4 * 1024 * 1024)
                     ())
                  url)
              diagnose;
        }
      in
      (Ci.jobs, Ci.executor config))
