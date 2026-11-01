module F = Bushel_sync.Forge

let check name b =
  if not b then (
    prerr_endline ("FAIL: " ^ name);
    exit 1)

let read p = In_channel.with_open_bin ("fixtures/" ^ p) In_channel.input_all

let ok = function
  | Ok v -> v
  | Error e ->
    prerr_endline e;
    exit 1

let () =
  check "plain tag" (F.version_of_tag "2.6.0" = "2.6.0");
  check "v tag" (F.version_of_tag "v0.10.2" = "0.10.2");
  check "a v that starts a word stays" (F.version_of_tag "vim-9" = "vim-9");
  check "a bare v stays" (F.version_of_tag "v" = "v");

  let rs =
    ok
      (F.github_releases ~repo:"realworldocaml/mdx"
         (read "github_releases.json"))
  in
  check "releases parsed" (List.length rs = 4);
  let r = List.find (fun c -> c.F.tag = "2.6.0") rs in
  check "mdx version" (r.F.version = "2.6.0");
  check "mdx date from published_at" (r.F.date = (2026, 7, 22));
  check "mdx author" (r.F.author = Some "avsm");
  check "mdx url"
    (r.F.url = "https://github.com/realworldocaml/mdx/releases/tag/2.6.0");
  check "github forge" (r.F.forge = Bushel.Release.Github);
  check "another author is kept as that author"
    ((List.find (fun c -> c.F.tag = "2.5.2") rs).F.author = Some "Julow");
  let g =
    ok
      (F.github_release ~repo:"ucam-eo/geotessera"
         (read "github_release_tag.json"))
  in
  check "tag with v" (g.F.tag = "v0.10.2" && g.F.version = "0.10.2");
  check "geotessera date" (g.F.date = (2026, 9, 4));
  check "malformed json is an error"
    (match F.github_releases ~repo:"a/b" "{" with
    | Error _ -> true
    | Ok _ -> false);
  check "a draft is not listed"
    (let json =
       {|[{"tag_name":"v1","name":"n","published_at":"2026-01-02T03:04:05Z",
           "html_url":"u","draft":true,"prerelease":false,
           "author":{"login":"a"}}]|}
     in
     ok (F.github_releases ~repo:"a/b" json) = []);

  check "release events"
    (F.github_events (read "github_events.json")
    = [ ("realworldocaml/mdx", "2.7.0") ]);

  (* A Tangled artifact is a release once its name gives a version. *)
  (match
     F.tangled_candidate ~repo:"anil.recoil.org/dune-rpc-eio"
       ~name:"dune-rpc-eio-0.1.0.tbz" ~created_at:"2026-08-09T13:21:57+03:00"
   with
  | None -> check "an artifact with a version is a release" false
  | Some d ->
    check "tangled version" (d.F.version = "0.1.0");
    check "tangled date from createdAt" (d.F.date = (2026, 8, 9));
    check "tangled forge" (d.F.forge = Bushel.Release.Tangled);
    check "tangled tag is the version" (d.F.tag = "0.1.0");
    check "a file name is not a title" (d.F.title = None);
    check "tangled url"
      (d.F.url = "https://tangled.org/anil.recoil.org/dune-rpc-eio");
    check "tangled has no author" (d.F.author = None));
  check "an artifact with no version is no release"
    (F.tangled_candidate ~repo:"h/x" ~name:"readme.tbz"
       ~created_at:"2026-08-09T13:21:57+03:00"
    = None);
  check "an unreadable date is no release"
    (F.tangled_candidate ~repo:"h/x" ~name:"x-1.0.tbz" ~created_at:"soon"
    = None);


  (* Artifacts of one version are one release. *)
  let tc name =
    Option.get
      (F.tangled_candidate ~repo:"h/x" ~name
         ~created_at:"2026-08-09T13:21:57+03:00")
  in
  check "one release per version"
    (List.map
       (fun c -> c.F.version)
       (F.one_per_version [ tc "x-1.0.tbz"; tc "x-1.0.tar.gz"; tc "x-1.1.tbz" ])
    = [ "1.0"; "1.1" ]);
  (* [unregistered] keeps the author's releases that are not registered. *)
  let mk ?(author = Some "avsm") ?(prerelease = false) tag =
    { r with F.tag; version = F.version_of_tag tag; author; prerelease }
  in
  let registered =
    [
      {
        Bushel.Release.repo = "realworldocaml/mdx";
        forge = Bushel.Release.Github;
        project = None;
        releases =
          [
            {
              Bushel.Release.version = "2.6.0";
              tag = None;
              date = (2026, 7, 22);
              summary = "s";
              url = "u";
              registries = [];
            };
          ];
      };
    ]
  in
  let out =
    F.unregistered ~author:"avsm" ~registered
      [
        mk "2.6.0";
        mk "2.7.0";
        mk ~author:(Some "other") "2.8.0";
        mk ~prerelease:true "2.9.0-rc1";
      ]
  in
  check "only the new release by the author"
    (List.map (fun c -> c.F.tag) out = [ "2.7.0" ]);
  let t =
    {
      (mk "0.2.0") with
      F.author = None;
      forge = Bushel.Release.Tangled;
      repo = "h/x";
    }
  in
  check "a tangled candidate needs no author"
    (F.unregistered ~author:"avsm" ~registered [ t ] = [ t ]);
  print_endline "ok"
