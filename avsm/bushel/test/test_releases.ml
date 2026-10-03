module R = Bushel_sync.Releases

let check name b =
  if not b then (
    prerr_endline ("FAIL: " ^ name);
    exit 1)

let cand =
  {
    Bushel_sync.Forge.repo = "realworldocaml/mdx";
    forge = Bushel.Release.Github;
    tag = "2.6.0";
    version = "2.6.0";
    date = (2026, 7, 22);
    title = Some "2.6.0";
    url = "https://github.com/realworldocaml/mdx/releases/tag/2.6.0";
    author = Some "avsm";
    prerelease = false;
  }

let () =
  let reg =
    {
      Bushel.Release.name = "opam.ocaml.org";
      package = "mdx";
      url = "https://opam.ocaml.org/packages/mdx/mdx.2.6.0/";
    }
  in
  let r =
    R.build cand ~registries:[ reg ]
      ~description:(Some "Executable code blocks inside markdown files. Extra.")
      ~summary:None
  in
  check "summary from the description"
    (r.Bushel.Release.summary
    = "Executable code blocks inside markdown files.");
  check "date from the forge" (r.Bushel.Release.date = (2026, 7, 22));
  check "url from the forge"
    (r.Bushel.Release.url = cand.Bushel_sync.Forge.url);
  check "no tag when it equals the version" (r.Bushel.Release.tag = None);
  check "registries attached" (r.Bushel.Release.registries = [ reg ]);
  let r =
    R.build cand ~registries:[] ~description:(Some "d.") ~summary:(Some "Mine")
  in
  check "an explicit summary wins" (r.Bushel.Release.summary = "Mine");
  let r = R.build cand ~registries:[] ~description:None ~summary:None in
  check "a title that is only the version is not a summary"
    (r.Bushel.Release.summary = "mdx 2.6.0");
  let r =
    R.build
      { cand with Bushel_sync.Forge.title = Some "Faster parsing" }
      ~registries:[] ~description:None ~summary:None
  in
  check "a real title is the next fallback"
    (r.Bushel.Release.summary = "Faster parsing");
  let r =
    R.build
      { cand with Bushel_sync.Forge.title = None }
      ~registries:[] ~description:None ~summary:None
  in
  check "the package and version are the last fallback"
    (r.Bushel.Release.summary = "mdx 2.6.0");
  let r =
    R.build
      { cand with Bushel_sync.Forge.tag = "v2.6.0" }
      ~registries:[] ~description:None ~summary:None
  in
  check "a tag that differs from the version is kept"
    (r.Bushel.Release.tag = Some "v2.6.0");
  let r =
    R.build cand ~registries:[] ~description:(Some "  ") ~summary:(Some "")
  in
  check "blank text falls through"
    (r.Bushel.Release.summary = "mdx 2.6.0");
  check "no token when the variable is unset" (R.token_of_env None = None);
  check "no token when the variable is empty" (R.token_of_env (Some "") = None);
  check "a token is read from the variable"
    (R.token_of_env (Some "abc") = Some "abc");
  print_endline "ok"
