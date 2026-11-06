(* SPDX-License-Identifier: ISC *)
open Cmdliner
open Ocaml_ci_spindle

(* The model is asked only when [--diagnose] is given. Its key comes from a
   file or the environment, never the command line, which others can read. *)
let diagnose =
  let open Arg in
  let make enabled url model key_file =
    if not enabled then `Ok None
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
      match url with
      | None -> `Error (true, "--diagnose requires --diagnose-url")
      | Some url -> `Ok (Some (url, model, key))
  in
  Term.ret @@ Term.(
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
        (opt (some string) None
        & info [ "diagnose-url" ] ~docv:"URL"
            ~doc:
              "The OpenAI-compatible API to ask, such as \
               $(b,https://openrouter.ai/api/v1). Required with \
               $(b,--diagnose). A key is sent only over https or to a \
               loopback address.")
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
    const
      (fun cap cache compilers platforms full_platforms day10 oxcaml diagnose ->
        ( cap,
          cache,
          compilers,
          (platforms, full_platforms, day10),
          oxcaml,
          diagnose ))
    $ required
        (opt (some file) None
        & info [ "ocluster-cap" ] ~docv:"FILE"
            ~doc:"The OCluster scheduler's submission capability file.")
    $ (const (fun dir hosts -> (dir, hosts))
      $ value
          (opt string "opam-cache"
          & info [ "opam-cache" ] ~docv:"DIR"
              ~doc:"Where the opam repositories are cloned to solve.")
      $ value
          (opt (list string) Opam_repo.default_hosts
          & info [ "remote-hosts" ] ~docv:"HOSTS"
              ~doc:
                "The only hosts whose opam repositories a project may name in \
                 $(b,x-opam-repositories). The build workers fetch them \
                 themselves, so name only public forges that cannot point a \
                 worker at a private address. $(b,github.com), \
                 $(b,gitlab.com), $(b,codeberg.org), $(b,bitbucket.org), \
                 $(b,git.sr.ht) and $(b,tangled.org) by default."))
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
        (opt
           (list
              (enum
                 Variant.
                   [
                     ("x86_64", [ X86_64 ]);
                     ("arm64", [ Arm64 ]);
                     ("ppc64le", [ Ppc64le ]);
                     ("riscv64", [ Riscv64 ]);
                     ("none", []);
                   ]))
           Variant.[ [ X86_64 ]; [ Riscv64 ] ]
        & info [ "day10" ] ~docv:"ARCHES"
            ~doc:
              "Build Debian on these architectures with day10 instead of \
               OBuilder, on the OCluster pools named \
               $(b,day10-linux-)$(i,ARCH), from $(b,x86_64), $(b,arm64), \
               $(b,ppc64le) and $(b,riscv64), or $(b,none). \
               $(b,x86_64,riscv64) by default. OxCaml and Windows always build \
               with OBuilder.")
    $ value
        (flag
        & info [ "oxcaml" ]
            ~doc:"Also build with OxCaml 5.2.0minus39, as oxmono does.")
    $ diagnose)

let () =
  Spindle_cli.main ~name:"ocaml-ci-spindle" ~workflow_timeout:21600.
    ~repository_workflows:false
    ~doc:"Run a Tangled spindle that tests OCaml projects on OCluster" options
    (fun env ~sw (cap, (cache, hosts), compilers, platforms, oxcaml, diagnose)
       ->
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
          remotes =
            Opam_repo.remotes ~hosts env#process_mgr env#mono_clock ~dir:cache;
          compilers;
          platforms =
            (let p, _, _ = platforms in
             p);
          full_platforms =
            (let _, f, _ = platforms in
             f);
          day10 =
            (let _, _, d = platforms in
             List.concat d);
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
