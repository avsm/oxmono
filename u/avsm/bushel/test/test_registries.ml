module B = Bushel_sync.Registries

let check name b =
  if not b then (
    prerr_endline ("FAIL: " ^ name);
    exit 1)

let contains_sub ~sub s =
  let n = String.length sub in
  let rec go i =
    i + n <= String.length s && (String.sub s i n = sub || go (i + 1))
  in
  go 0

let () =
  check "github has one url"
    (B.repository_urls Bushel.Release.Github "ucam-eo/geotessera"
    = [ "https://github.com/ucam-eo/geotessera" ]);
  let tangled =
    B.repository_urls Bushel.Release.Tangled "anil.recoil.org/ocaml-jsonfeed"
  in
  check "tangled has every spelling an opam file uses"
    (List.for_all
       (fun u -> List.mem u tangled)
       [ "git+https://tangled.org/anil.recoil.org/ocaml-jsonfeed";
         "git+https://tangled.org/anil.recoil.org/ocaml-jsonfeed.git";
         "git+https://tangled.org/@anil.recoil.org/ocaml-jsonfeed";
         "git+https://tangled.sh/anil.recoil.org/ocaml-jsonfeed";
         "git+https://tangled.sh/@anil.recoil.org/ocaml-jsonfeed";
         "git+https://tangled.sh/@anil.recoil.org/ocaml-jsonfeed.git" ]);
  check "the spellings are distinct"
    (List.length (List.sort_uniq compare tangled) = List.length tangled);
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
  (* The description comes from the attached package, and otherwise from an
     allowed registry, so a release no registry carries yet still has one. *)
  let found =
    [
      ("nixpkgs-unstable", "ocamlPackages.mdx", Some "Nix description.");
      ("pypi.org", "mdx", None);
      ("opam.ocaml.org", "mdx-extras", Some "Other package.");
      ("opam.ocaml.org", "mdx", Some "Executable code blocks.");
    ]
  in
  let attached =
    [
      {
        Bushel.Release.name = "opam.ocaml.org";
        package = "mdx-extras";
        url = "u";
      };
    ]
  in
  check "the attached package's description wins"
    (B.pick_description ~allowed ~attached ~found () = Some "Other package.");
  check "with nothing attached the preferred package is used"
    (B.pick_description ~prefer:[ "mdx" ] ~allowed ~attached:[] ~found ()
    = Some "Executable code blocks.");
  check "with no preference the first allowed package with one is used"
    (B.pick_description ~allowed ~attached:[] ~found ()
    = Some "Other package.");
  check "a repackaging's description is never used"
    (B.pick_description ~allowed
       ~attached:[]
       ~found:[ ("nixpkgs-unstable", "x", Some "Nix description.") ]
       ()
    = None);
  check "no packages, no description"
    (B.pick_description ~allowed ~attached:[] ~found:[] () = None);
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
  (* One lookup of a repository serves every version asked for. *)
  Eio_mock.Backend.run @@ fun () ->
  let lookups = ref 0 in
  let http =
    Fetch_mock.client (fun req ->
        let url = Fetch.Middleware.Url.to_string req.Fetch.Middleware.url in
        let headers =
          Http.Header.of_list [ ("Content-Type", "application/json") ]
        in
        if contains_sub ~sub:"/packages/lookup" url then (
          incr lookups;
          Fetch_mock.respond ~headers
            (In_channel.with_open_bin "fixtures/ecosystems_lookup.json"
               In_channel.input_all)
            req)
        else Fetch_mock.respond ~status:404 ~headers "{}" req)
  in
  let eco =
    Ecosystems.of_fetch ~base_url:"https://packages.ecosyste.ms/api/v1" http
  in
  let find ?cache version =
    match
      B.lookup ?cache eco ~allowed:[ "npmjs.org" ]
        ~forge:Bushel.Release.Github ~repo:"minimistjs/minimist" ~version
    with
    | Ok (_, description) -> description
    | Error e -> failwith e
  in
  ignore (find "1.2.8");
  ignore (find "1.2.7");
  check "without a cache each lookup asks again" (!lookups = 2);
  lookups := 0;
  let cache = B.create_cache () in
  let d1 = find ~cache "1.2.8" in
  let d2 = find ~cache "1.2.7" in
  check "with a cache the repository is looked up once" (!lookups = 1);
  check "the description still comes through"
    (d1 = Some "parse argument options" && d2 = d1);
  (* ecosyste.ms matches a repository URL exactly, so a tangled repository is
     found under whichever spelling its opam file used. *)
  let asked = ref [] in
  let spelled =
    Fetch_mock.client (fun req ->
        let url = Fetch.Middleware.Url.to_string req.Fetch.Middleware.url in
        let headers =
          Http.Header.of_list [ ("Content-Type", "application/json") ]
        in
        if contains_sub ~sub:"/packages/lookup" url then (
          asked := url :: !asked;
          if
            contains_sub ~sub:"tangled.sh" url
            && (contains_sub ~sub:"%40anil" url
               || contains_sub ~sub:"@anil" url)
            && not (contains_sub ~sub:".git" url)
          then
            Fetch_mock.respond ~headers
              (In_channel.with_open_bin "fixtures/ecosystems_lookup.json"
                 In_channel.input_all)
              req
          else Fetch_mock.respond ~headers "[]" req)
        else Fetch_mock.respond ~status:404 ~headers "{}" req)
  in
  let eco2 =
    Ecosystems.of_fetch ~base_url:"https://packages.ecosyste.ms/api/v1" spelled
  in
  (match
     B.lookup eco2 ~allowed:[ "npmjs.org" ] ~forge:Bushel.Release.Tangled
       ~repo:"anil.recoil.org/minimist" ~version:"1.2.8"
   with
  | Ok (_, description) ->
    check "found under the tangled.sh spelling"
      (description = Some "parse argument options")
  | Error e -> failwith e);
  check "every spelling was tried"
    (List.length !asked = List.length tangled);
  print_endline "ok"
