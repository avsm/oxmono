module B = Bushel_sync.Registries

let check name b =
  if not b then (
    prerr_endline ("FAIL: " ^ name);
    exit 1)

let () =
  check "github url"
    (B.repository_url Bushel.Release.Github "ucam-eo/geotessera"
    = "https://github.com/ucam-eo/geotessera");
  check "tangled url uses the form ecosyste.ms stores"
    (B.repository_url Bushel.Release.Tangled "anil.recoil.org/dune-rpc-eio"
    = "git+https://tangled.org/anil.recoil.org/dune-rpc-eio");
  let packages =
    [
      ("nixpkgs-unstable", "ocamlPackages.mdx");
      ("opam.ocaml.org", "mdx");
      ("proxy.golang.org", "github.com/realworldocaml/mdx");
      ("pypi.org", "mdx");
    ]
  in
  let carries ~registry ~package:_ =
    match registry with
    | "opam.ocaml.org" -> Some "https://opam.ocaml.org/packages/mdx/mdx.2.6.0/"
    | "nixpkgs-unstable" -> Some "https://example.org/nix"
    | _ -> None
  in
  let allowed = [ "pypi.org"; "opam.ocaml.org" ] in
  let regs = B.attach ~allowed ~packages ~carries () in
  check "only an allowed registry that carries the version"
    (List.map (fun r -> r.Bushel.Release.name) regs = [ "opam.ocaml.org" ]);
  check "the url comes from the registry"
    ((List.hd regs).Bushel.Release.url
    = "https://opam.ocaml.org/packages/mdx/mdx.2.6.0/");
  check "the package name is kept"
    ((List.hd regs).Bushel.Release.package = "mdx");
  check "a repackaging is never attached"
    (B.attach ~allowed ~packages:[ ("nixpkgs-unstable", "x") ] ~carries ()
    = []);
  check "no packages, no registries"
    (B.attach ~allowed ~packages:[] ~carries () = []);
  let carries_all ~registry:_ ~package:_ = Some "u" in
  check "the result follows the order of the allowed list"
    (List.map
       (fun r -> r.Bushel.Release.name)
       (B.attach ~allowed
          ~packages:[ ("opam.ocaml.org", "m"); ("pypi.org", "m") ]
          ~carries:carries_all ())
    = [ "pypi.org"; "opam.ocaml.org" ]);
  (* A registry with several packages uses the one the repository is named
     after, and otherwise the first. *)
  let many =
    [ ("opam.ocaml.org", "cohttp-curl"); ("opam.ocaml.org", "cohttp") ]
  in
  let package_of ?prefer () =
    (List.hd
       (B.attach ?prefer ~allowed:[ "opam.ocaml.org" ] ~packages:many
          ~carries:carries_all ()))
      .Bushel.Release.package
  in
  check "the preferred package is used"
    (package_of ~prefer:[ "cohttp" ] () = "cohttp");
  check "the first preference that exists is used"
    (package_of ~prefer:[ "ocaml-cohttp"; "cohttp" ] () = "cohttp");
  check "with no match the first package is used"
    (package_of ~prefer:[ "other" ] () = "cohttp-curl");
  check "with no preference the first package is used"
    (package_of () = "cohttp-curl");
  check "candidates for a repository name"
    (B.preferred_packages "mirage/ocaml-cohttp" = [ "ocaml-cohttp"; "cohttp" ]);
  check "a repository name without the prefix"
    (B.preferred_packages "ucam-eo/geotessera" = [ "geotessera" ]);
  check "first sentence"

    (B.summary_of_description
       "Executable code blocks inside markdown files. More text."
    = "Executable code blocks inside markdown files.");
  check "no full stop"
    (B.summary_of_description "Executable code blocks"
    = "Executable code blocks");
  check "cut to 120 characters"
    (String.length (B.summary_of_description (String.make 300 'a')) <= 120);
  check "a cut ends in an ellipsis"
    (String.ends_with ~suffix:"..."
       (B.summary_of_description (String.make 300 'a')));
  check "newlines become spaces"
    (B.summary_of_description "One line\nsecond line" = "One line second line");
  check "a decimal point is not a sentence end"
    (B.summary_of_description "Version 1.5 of the thing. Next."
    = "Version 1.5 of the thing.");
  check "empty stays empty" (B.summary_of_description "  " = "");
  print_endline "ok"
