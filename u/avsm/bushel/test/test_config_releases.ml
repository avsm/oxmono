let check name b =
  if not b then (
    prerr_endline ("FAIL: " ^ name);
    exit 1)

let load s =
  match Bushel_config.of_string s with
  | Ok c -> c.Bushel_config.releases
  | Error e ->
    prerr_endline e;
    exit 1

let () =
  let r =
    load
      {|[releases]
github_user = "avsm"
github = ["mirage/ocaml-cohttp", "ucam-eo/geotessera"]
tangled = ["anil.recoil.org/dune-rpc-eio"]
registries = ["pypi.org"]

[releases.projects]
"ucam-eo/geotessera" = "tessera"
|}
  in
  check "user" (r.github_user = Some "avsm");
  check "github" (r.github = [ "mirage/ocaml-cohttp"; "ucam-eo/geotessera" ]);
  check "tangled" (r.tangled = [ "anil.recoil.org/dune-rpc-eio" ]);
  check "registries" (r.registries = [ "pypi.org" ]);
  check "projects" (r.projects = [ ("ucam-eo/geotessera", "tessera") ]);
  let d = load "" in
  check "no section is empty"
    (d.github = [] && d.tangled = [] && d.github_user = None
    && d.projects = []);
  check "registries default" (d.registries = Bushel_config.default_registries);
  check "the default list"
    (Bushel_config.default_registries
    = [ "pypi.org"; "opam.ocaml.org"; "npmjs.org"; "crates.io" ]);
  print_endline "ok"
