(* Registered releases appear in the notes view as one line inside the month
   they were made, among the notes of that month. *)

let check name cond =
  if not cond then (
    prerr_endline ("FAIL: " ^ name);
    exit 1)

let index_of hay needle =
  let n = String.length needle and h = String.length hay in
  let rec go i =
    if i + n > h then None
    else if String.sub hay i n = needle then Some i
    else go (i + 1)
  in
  go 0

let contains hay needle = index_of hay needle <> None

let before hay a b =
  match (index_of hay a, index_of hay b) with
  | Some i, Some j -> i < j
  | _ -> false

let cfg : Arod.Config.t =
  { Arod.Config.default with
    site = { Arod.Config.default.site with base_url = "https://example.com" } }

let note slug title date : Bushel.Note.t =
  { Bushel.Note.title; date; slug; body = "Body."; tags = []; draft = false;
    updated = None; sidebar = None; index_page = false; perma = false;
    weeknote = false; featured = false; doi = None; synopsis = None;
    titleimage = None; via = None; slug_ent = None; source = None; url = None;
    author = None; category = None; standardsite = None; social = None;
    source_file = None }

let notes =
  [ note "august" "An August note" (2026, 8, 10);
    note "june" "A June note" (2026, 6, 1) ]

let release ?(registries = []) ~version ~date summary url =
  { Bushel.Release.version; tag = None; date; summary; url; registries }

let repo_of ?(repo = "realworldocaml/mdx") releases =
  { Bushel.Release.repo; forge = Bushel.Release.Github; project = None;
    releases }

let opam =
  { Bushel.Release.name = "opam.ocaml.org"; package = "mdx";
    url = "https://opam.ocaml.org/packages/mdx/mdx.2.6.0/" }

let render ?releases () =
  let ctx =
    Arod.Ctx.of_entries ~config:cfg ?releases
      (Bushel.Entry.v ~papers:[] ~notes ~projects:[] ~ideas:[] ~videos:[]
         ~contacts:[] ~data_dir:"." ())
  in
  let article, _sidebar = Arod_component.Note.notes_list ~ctx in
  Htmlit.El.to_string ~doctype:false article

let () =
  let mdx_url = "https://github.com/realworldocaml/mdx/releases/tag/2.6.0" in
  let releases =
    [ repo_of
        [ release ~version:"2.6.0" ~date:(2026, 7, 22) ~registries:[ opam ]
            "Executable code blocks" mdx_url;
          release ~version:"2.7.0" ~date:(2026, 8, 20) "Newer release"
            "https://github.com/realworldocaml/mdx/releases/tag/2.7.0" ] ]
  in
  let html = render ~releases () in
  check "the summary is shown" (contains html "Executable code blocks");
  check "the release links to its page"
    (contains html (Printf.sprintf {|href="%s"|} mdx_url));
  check "the name and version are shown" (contains html "mdx 2.6.0");
  check "the ecosyste.ms metadata is linked"
    (contains html
       ("https://packages.ecosyste.ms/registries/opam.ocaml.org/packages/mdx/"
       ^ "versions/2.6.0"));
  check "the registry is named" (contains html "opam.ocaml.org");
  check "a release-only month appears" (contains html {|id="month-2026-07"|});
  check "months run newest first"
    (before html {|id="month-2026-08"|} {|id="month-2026-07"|}
    && before html {|id="month-2026-07"|} {|id="month-2026-06"|});
  check "a release sits before an older note of its month"
    (before html "Newer release" "An August note");
  let forge_only =
    render
      ~releases:
        [ repo_of [ release ~version:"2.7.0" ~date:(2026, 8, 20) "Forge only"
                      "https://example.org/2.7.0" ] ]
      ()
  in
  check "a release with no registries has no metadata link"
    (contains forge_only "Forge only"
    && not (contains forge_only "packages.ecosyste.ms"));
  check "a release with no registries has no empty tag list"
    (not (contains forge_only "release-registries"));
  let hostile =
    [ repo_of [ release ~version:"1.0.0" ~date:(2026, 8, 20)
                  {|<b> & "quoted"|} "https://example.org/1" ] ]
  in
  let escaped = render ~releases:hostile () in
  check "a summary is escaped"
    (contains escaped "&lt;b&gt;" && not (contains escaped "<b> &"));
  check "no releases leaves the page as it was"
    (render () = render ~releases:[] ());
  check "no releases means no release lines"
    (not (contains (render ()) "release-row"));
  print_endline "ok"
