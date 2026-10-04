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

let note ?(weeknote = false) ?(featured = false) ?(perma = false) slug title
    date : Bushel.Note.t =
  { Bushel.Note.title; date; slug; body = "Body."; tags = []; draft = false;
    updated = None; sidebar = None; index_page = false; perma;
    weeknote; featured; doi = None; synopsis = None;
    titleimage = None; via = None; slug_ent = None; source = None; url = None;
    author = None; category = None; standardsite = None; social = None;
    source_file = None }

(* Journal notes, a weeknote, a featured note and a permanent one, so that the
   page without releases has every kind of section to compare. *)
let picture =
  Srcsetter.v "pic.webp" "pic" "pic.jpg" Srcsetter.MS.empty (800, 600)

let notes =
  [ { (note "august" "An August note" (2026, 8, 10)) with
      Bushel.Note.titleimage = Some "pic" };
    note "june" "A June note" (2026, 6, 1);
    note ~weeknote:true "week-28" ".plan-2026w28: Week 28" (2026, 7, 8);
    note ~weeknote:true "week-30" ".plan-2026w30: Week 30" (2026, 7, 22);
    note ~featured:true "featured" "A featured note" (2026, 5, 2);
    note ~perma:true "perma" "A permanent note" (2026, 4, 2) ]

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
         ~contacts:[] ~images:[ picture ] ~data_dir:"." ())
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
  check "a code release is a line that runs on to its entry, with no marker"
    (contains html "sn-release" && not (contains html "sn-node-release")
    && not (contains html "release-mark"));
  check "a tag filter can hide it"
    (contains html {|data-tags=""|} && contains html "note-item");
  check "a release's date follows its name, and is not in a column"
    (before html "mdx 2.6.0" {|>22 Jul<|}
    && contains html {|class="release-date"|}
    && not (contains html "note-compact-meta"));
  check "a registry is an icon that names it"
    (contains html {|aria-label="opam.ocaml.org on ecosyste.ms"|}
    && contains html {|title="opam.ocaml.org on ecosyste.ms"|});
  check "the registry icon is the one for that registry"
    (contains html (Arod.Icons.registry_icon ~size:12 "opam.ocaml.org"));
  (* The page is one timeline. The weeknote rail is folded into it. *)
  check "the page is a single timeline"
    (contains html {|class="snake|}
    && not (contains html "week-rail")
    && not (contains html "notes-split")
    && not (contains html "lg:hidden"));
  check "a weeknote is a row of its own kind"
    (contains html "sn-week"
    && contains html "Week 28" && contains html "Week 30");
  check "the weeknote prefix is not shown" (not (contains html ".plan-"));
  check "a weeknote can be hidden by a tag filter"
    (contains html "sn-week note-item");
  check "a missing week is marked on the line"
    (contains html "1 quiet week");
  check "a note's image begins its card, before its title"
    (before html {|src="/images/pic.webp"|} "An August note"
    && contains html {|class="sn-node-img"|}
    && contains html "sn-node-note");
  check "a note without an image has no placeholder, only a line to its text"
    (not (contains html "sn-node-icon") && contains html "sn-exit-fade");
  check "a note's date is a caption above its title"
    (before html {|>10 Aug<|} "An August note");
  let occurrences html sub =
    let n = String.length sub in
    let rec go i acc =
      if i + n > String.length html then acc
      else go (i + 1) (if String.sub html i n = sub then acc + 1 else acc)
    in
    go 0 0
  in
  check "every month carries the motifs of its season"
    (occurrences html "sn-season sn-season-" = occurrences html {|class="sn-pill"|}
    && contains html "sn-season-summer" && contains html "sn-season-spring"
    && contains html "sn-s-summer");
  check "every month has a pill on the spine"
    (let months = occurrences html {|class="sn-month"|} in
     months > 0 && months = occurrences html {|class="sn-pill"|});
  check "one spine, as an svg path"
    (occurrences html {|class="snake-spine"|} = 1
    && contains html {|class="snake-line"|});
  check "every row has an exit curve from the spine"
    (let rows = occurrences html "sn-item " in
     rows > 0
     && rows
        = occurrences html {|class="sn-exit"|}
          + occurrences html {|class="sn-exit sn-exit-fade"|});
  check "no quiet week is a row with an exit"
    (contains html "sn-quiet");
  (* The first exit on the page starts exactly on the spine, which is drawn
     from the same geometry. The first row of the first month is one month
     header down. *)
  check "an exit meets the spine"
    (let marker = {|class="sn-exit-path" d="M |} in
     let n = String.length marker in
     let rec find i =
       if i + n > String.length html then None
       else if String.sub html i n = marker then Some (i + n)
       else find (i + 1)
     in
     match find 0 with
     | None -> false
     | Some i ->
       let rest = String.sub html i (String.length html - i) in
       Scanf.sscanf rest "%f %f" (fun x y ->
           Float.abs (x -. Arod_component.Snake.spine_x
                         (Arod_component.Snake.month_height +. y)) < 1e-3));
  check "each month is a section the page script can track"
    (contains html {|data-month-id="2026-08"|}
    && contains html {|data-month-id="2026-07"|});
  check "a release is the smallest row"
    (contains html "sn-release");
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
  let rubygems =
    { opam with Bushel.Release.name = "rubygems.org"; package = "rake" }
  in
  let other =
    render
      ~releases:
        [ repo_of
            [ release ~version:"1.0.0" ~date:(2026, 8, 20)
                ~registries:[ rubygems ] "Ruby" "https://example.org/r" ] ]
      ()
  in
  check "an unknown registry gets the generic package icon"
    (contains other (Arod.Icons.registry_icon ~size:12 "rubygems.org")
    && Arod.Icons.registry_icon "rubygems.org"
       = Arod.Icons.registry_icon "something-else.example");
  check "the generic icon is not one of the brand icons"
    (List.for_all
       (fun r -> Arod.Icons.registry_icon r <> Arod.Icons.registry_icon "x.y")
       [ "pypi.org"; "npmjs.org"; "crates.io"; "opam.ocaml.org" ]);
  check "each known registry has its own icon"
    (let icons =
       List.map Arod.Icons.registry_icon
         [ "pypi.org"; "npmjs.org"; "crates.io"; "opam.ocaml.org" ]
     in
     List.length (List.sort_uniq compare icons) = 4
     && List.for_all (fun i -> contains i "<svg") icons);
  (* Releases of one repository in a month are one line. *)
  let busy =
    render
      ~releases:
        [ repo_of ~repo:"ucam-eo/geotessera"
            [ release ~version:"0.10.0" ~date:(2026, 8, 3) "Python library"
                "https://example.org/0.10.0";
              release ~version:"0.10.1" ~date:(2026, 8, 12) "Python library"
                "https://example.org/0.10.1";
              release ~version:"0.10.2" ~date:(2026, 8, 20) "Python library"
                "https://example.org/0.10.2";
              release ~version:"0.9.0" ~date:(2026, 7, 1) "Python library"
                "https://example.org/0.9.0" ] ]
      ()
  in
  let count sub =
    let n = String.length sub in
    let rec go i acc =
      if i + n > String.length busy then acc
      else go (i + 1) (if String.sub busy i n = sub then acc + 1 else acc)
    in
    go 0 0
  in
  check "one line for a repository's releases in a month"
    (count "geotessera 0.10.2" = 1 && count "geotessera 0.10.1" = 0
    && count "geotessera 0.10.0" = 0);
  check "the line says how many more there were" (contains busy "+2 earlier");
  check "the earlier versions are named" (contains busy "0.10.0, 0.10.1");
  check "another month is its own line" (count "geotessera 0.9.0" = 1);
  let hostile =
    [ repo_of [ release ~version:"1.0.0" ~date:(2026, 8, 20)
                  {|<b> & "quoted"|} "https://example.org/1" ] ]
  in
  let escaped = render ~releases:hostile () in
  check "a summary is escaped"
    (contains escaped "&lt;b&gt;" && not (contains escaped "<b> &"));
  check "no releases leaves the page as it was"
    (render () = render ~releases:[] ());
  (* The page without releases is pinned. It was first rendered by the code from
     before releases existed. It was regenerated deliberately each time the
     notes view was redesigned: as a single timeline, with week branches and
     round bullets, and as one snaking spine with exits, and again for a subtler look with cards. *)
  check "the page matches the one rendered before releases existed"
    (render ()
    = In_channel.with_open_bin "fixtures/notes/notes_no_releases.html"
        In_channel.input_all);
  check "no releases means no release lines"
    (not (contains (render ()) "release-row"));
  print_endline "ok"
