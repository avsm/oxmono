(* The list pages served as markdown follow the HTML pages: the same order,
   the same grouping and the same facts for each entry. *)

let checks = ref 0

let check name cond =
  incr checks;
  if not cond then (
    prerr_endline ("FAIL: " ^ name);
    exit 1)

let find hay needle =
  let n = String.length needle and h = String.length hay in
  let rec go i =
    if i + n > h then None
    else if String.sub hay i n = needle then Some i
    else go (i + 1)
  in
  go 0

let contains hay needle = find hay needle <> None

let before hay a b =
  match (find hay a, find hay b) with Some i, Some j -> i < j | _ -> false

let cfg : Arod.Config.t =
  { Arod.Config.default with
    site = { Arod.Config.default.site with base_url = "https://example.com" } }

let ctx_of ?(papers = []) ?(notes = []) ?(projects = []) ?(ideas = [])
    ?(videos = []) ?(contacts = []) ?releases ?external_links () =
  let entries =
    Bushel.Entry.v ~papers ~notes ~projects ~ideas ~videos ~contacts
      ~data_dir:"." ()
  in
  let entries =
    match external_links with
    | None -> entries
    | Some links ->
      Bushel.Entry.with_graph entries
        (Bushel.Link_graph.v ~internal_links:[] ~external_links:links)
  in
  Arod.Ctx.of_entries ~config:cfg ?releases entries

(* {1 Projects} *)

let project ?(finish = None) ?(tags = []) ~slug ~title ~start body :
    Bushel.Project.t =
  { Bushel.Project.slug; title; start; finish; tags; ideas = ""; body;
    social = None }

(* Given in no useful order, so that the order of the list is the order of the
   HTML cards and not the order of loading. *)
let projects =
  [ project ~slug:"zeta" ~title:"Zeta" ~start:2020 ~finish:(Some 2022)
      ~tags:[ "alpha"; "beta" ] "Zeta opens here.\n\nA second paragraph.";
    project ~slug:"alpha" ~title:"Alpha" ~start:2024 "Alpha opens here.";
    project ~slug:"mid" ~title:"Mid" ~start:2021 "" ]

let () =
  let md =
    Arod_component.Markdown_export.projects_list_md
      ~ctx:(ctx_of ~projects ())
  in
  let expected =
    List.sort Bushel.Project.compare projects |> List.map Bushel.Project.title
  in
  let positions =
    List.map
      (fun t -> find md ("[" ^ t ^ "](https://example.com/projects/"))
      expected
  in
  check "every project is listed" (List.for_all (fun p -> p <> None) positions);
  check "projects come in the order of the HTML cards"
    (let ps = List.filter_map Fun.id positions in
     ps = List.sort compare ps);
  check "the introduction of the HTML page opens the list"
    (before md "I work on a number of research projects" "[Zeta]"
    && contains md "[EEG Zulip](https://eeg.zulipchat.com)");
  check "years are given as the card gives them"
    (contains md "(2020\xE2\x80\x932022)"
    && contains md "(2024\xE2\x80\x93now)");
  check "a card opens with its summary"
    (contains md "Zeta opens here." && contains md "Alpha opens here.");
  check "tags are listed" (contains md "Tags: alpha, beta");
  check "a project with no body is still listed" (contains md "[Mid]")

(* {1 Ideas} *)

let idea ?(level = Bushel.Idea.MPhil) ?(year = 2025) ~slug ~title ~project
    ~status body : Bushel.Idea.t =
  { Bushel.Idea.slug; title; level; project; status; month = 1; year;
    supervisors = []; students = []; supervisor_handles = [];
    student_handles = []; reading = ""; body; url = None; tags = [];
    social = None }

(* [small] has the fewer open ideas, so the HTML index puts it second even
   though it is loaded first. In [big] the open ideas come first, then the one
   under way, and the completed one is last. *)
let ideas_projects =
  [ project ~slug:"small" ~title:"Small Project" ~start:2019 "";
    project ~slug:"big" ~title:"Big Project" ~start:2020 "" ]

let ideas =
  let open Bushel.Idea in
  [ idea ~slug:"e" ~title:"Done Small" ~project:"small" ~status:Completed
      ~year:2020 "E opens here.";
    idea ~slug:"d" ~title:"Done Big" ~project:"big" ~status:Completed
      ~year:2021 "D opens here.";
    idea ~slug:"c" ~title:"Going Big" ~project:"big" ~status:Ongoing
      "C opens here.";
    idea ~slug:"b" ~title:"Open Big B" ~project:"big" ~status:Available
      "B opens here.";
    idea ~slug:"a" ~title:"Open Big A" ~project:"big" ~status:Available
      ~year:2026 "A opens here." ]

let () =
  let md =
    Arod_component.Markdown_export.ideas_list_md
      ~ctx:(ctx_of ~projects:ideas_projects ~ideas ())
  in
  let at s = find md s in
  check "the introduction of the HTML index opens the list"
    (before md "These are research ideas" "## [Big Project]"
    && contains md "*much*");
  check "the status and level counts of the filters are given"
    (contains md "Status: 2 open, 1 under way, 2 completed"
    && contains md "Level: 5 MPhil");
  check "the contents list the projects as the page does"
    (before md "- Big Project: 4 (2 open, 1 under way, 1 completed)"
       "- Small Project: 1 (1 completed)");
  check "the project with more open ideas comes first"
    (before md "## [Big Project]" "## [Small Project]");
  check "a group states its counts as the HTML head does"
    (contains md "2 open, 1 under way, 1 previous");
  check "live ideas come before past ones, open before under way"
    (let order =
       List.map at
         [ "(https://example.com/ideas/a)"; "(https://example.com/ideas/b)";
           "(https://example.com/ideas/c)"; "(https://example.com/ideas/d)" ]
     in
     List.for_all (fun p -> p <> None) order
     && order = List.sort compare order);
  check "a live idea has its sentence and a summary"
    (contains md "An MPhil or Part III project, proposed in 2026."
    && contains md "A opens here.");
  check "a past idea has its sentence"
    (contains md "An MPhil or Part III project, completed in 2021")

(* {1 Talks} *)

let ptime y m d =
  match Ptime.of_date (y, m, d) with Some t -> t | None -> assert false

let video ?(talk = true) ?(project = None) ?(tags = []) ~slug ~title ~date
    description : Bushel.Video.t =
  let y, m, d = date in
  { Bushel.Video.slug; title; published_date = ptime y m d; uuid = slug;
    description; url = "https://videos.example/" ^ slug; talk; vertical = false;
    paper = None; project; tags; social = None }

(* Loaded oldest first. Only talks are on the HTML page, and newest first. *)
let videos =
  [ video ~slug:"old" ~title:"Old Talk" ~date:(2019, 3, 1) "Old talk text.";
    video ~slug:"clip" ~title:"A Plain Video" ~talk:false ~date:(2024, 1, 1) "";
    video ~slug:"new" ~title:"New Talk" ~date:(2024, 6, 1)
      ~project:(Some "big") ~tags:[ "one"; "two" ] "New talk text." ]

let () =
  let md =
    Arod_component.Markdown_export.videos_list_md
      ~ctx:(ctx_of ~projects:ideas_projects ~videos ())
  in
  check "a video that is not a talk is left out, as in the HTML"
    (not (contains md "A Plain Video"));
  check "talks come newest first, as in the HTML"
    (before md "[New Talk]" "[Old Talk]");
  check "a talk gives its month"
    (contains md "(Jun 2024)" && contains md "(Mar 2019)");
  check "a talk gives where to watch it"
    (contains md "Watch: <https://videos.example/new>");
  check "a talk gives the opening of its description and its tags"
    (before md "[New Talk]" "New talk text."
    && contains md "Tags: one, two");
  check "a talk gives the references of its card"
    (contains md "[Big Project](https://example.com/projects/big) (project)")

(* {1 Notes} *)

let note ?(weeknote = false) ?(featured = false) ?(tags = []) ?(doi = None)
    ?(synopsis = None) ~slug ~title ~date () : Bushel.Note.t =
  { Bushel.Note.title; date; slug; body = "One two three."; tags;
    draft = false; updated = None; sidebar = None; index_page = false;
    perma = false; weeknote; featured; doi; synopsis; titleimage = None;
    via = None; slug_ent = None; source = None; url = None; author = None;
    category = None; standardsite = None; social = None; source_file = None }

let notes =
  [ note ~slug:"old" ~title:"An Old Note" ~date:(2026, 6, 1) ();
    note ~weeknote:true ~slug:"w28" ~title:".plan-2026w28: Week 28"
      ~date:(2026, 7, 8) ();
    note ~weeknote:true ~slug:"w30" ~title:".plan-2026w30: Week 30"
      ~date:(2026, 7, 22) ~synopsis:(Some "A busy week.") ();
    note ~slug:"late" ~title:"A Late Note" ~date:(2026, 8, 10)
      ~tags:[ "rare"; "common" ] ~synopsis:(Some "Late synopsis.") ();
    note ~slug:"other" ~title:"Another Note" ~date:(2026, 5, 20)
      ~tags:[ "common" ] ();
    note ~featured:true ~slug:"feat" ~title:"A Featured Note"
      ~date:(2026, 5, 2) ~doi:(Some "10.1/feat") () ]

let release ~version ~date ?(registries = []) summary =
  { Bushel.Release.version; tag = None; date; summary;
    url = "https://forge.example/mdx/" ^ version; registries }

let opam =
  { Bushel.Release.name = "opam.ocaml.org"; package = "mdx";
    url = "https://opam.ocaml.org/packages/mdx/mdx.2.6.0/" }

let releases =
  [ { Bushel.Release.repo = "owner/mdx"; forge = Bushel.Release.Github;
      project = None;
      releases =
        [ release ~version:"2.6.0" ~date:(2026, 7, 22) ~registries:[ opam ]
            "Executable code blocks";
          release ~version:"2.5.0" ~date:(2026, 7, 2) "Earlier release" ] } ]

let () =
  let md =
    Arod_component.Markdown_export.notes_list_md
      ~ctx:(ctx_of ~notes ~releases ())
  in
  check "months run newest first, as the timeline does"
    (before md "## August 2026" "## July 2026"
    && before md "## July 2026" "## June 2026"
    && before md "## June 2026" "## May 2026");
  check "notes and weeknotes are in one list, newest first"
    (before md "[A Late Note]" "[Week 30]"
    && before md "[Week 30]" "[Week 28]"
    && before md "[Week 28]" "[An Old Note]");
  check "a weeknote loses its prefix and says which week it is"
    (not (contains md ".plan-") && contains md "(Week 30, "
    && contains md "A busy week.");
  check "on one day a note comes before a release"
    (before md "[Week 30]" "[mdx 2.6.0]");
  check "a release gives its date, its summary and its registries"
    (contains md "(2026-07-22, code release)"
    && contains md "Executable code blocks"
    && contains md
         "[opam.ocaml.org](https://packages.ecosyste.ms/registries/opam.ocaml.org/packages/mdx/versions/2.6.0)");
  check "a release's other versions in the month are said to be earlier"
    (contains md "1 earlier: 2.5.0");
  check "the weeks with nothing in them are marked, as the timeline marks them"
    (before md "[Week 30]" "- *1 quiet week*"
    && before md "- *1 quiet week*" "[Week 28]");
  check "tags come most popular first"
    (before md "Tags: common, rare" "Late synopsis." |> not
    && contains md "Tags: common, rare");
  check "the featured notes follow the timeline"
    (before md "[An Old Note]" "## Featured"
    && before md "## Featured" "Canonical:"
    && contains md "[DOI](https://doi.org/10.1/feat)")

(* {1 Links} *)

let linking ~slug ~title ~date body =
  { (note ~slug ~title ~date ()) with Bushel.Note.body }

let ext source url : Bushel.Link_graph.external_link =
  { Bushel.Link_graph.source;
    domain = List.nth (String.split_on_char '/' url) 2;
    url }

let () =
  let notes =
    [ linking ~slug:"older" ~title:"Older Entry" ~date:(2026, 3, 1)
        "See [Alpha](https://alpha.example/a) and [Beta](https://beta.example/b).";
      linking ~slug:"newer" ~title:"A title\nthat [breaks] lines"
        ~date:(2026, 8, 1)
        "See [Gamma](https://alpha.example/c)." ]
  in
  let md =
    Arod_component.Markdown_export.links_list_md
      ~ctx:
        (ctx_of ~notes
           ~external_links:
             [ ext "older" "https://alpha.example/a";
               ext "older" "https://beta.example/b";
               ext "newer" "https://alpha.example/c" ]
           ())
  in
  check "the introduction of the HTML page opens the list"
    (before md "These are all the outbound links" "## Links by entry"
    && contains md "[Karakeep](https://karakeep.app)");
  check "the counts of the sidebar are given"
    (contains md "3 links, 2 domains" && contains md "alpha.example: 2");
  check "groups come newest entry first, as in the HTML"
    (before md "(note, Aug 2026)" "(note, Mar 2026)");
  check "each link is under the entry that makes it"
    (before md "https://alpha.example/c" "[Older Entry]"
    && before md "[Older Entry]" "https://alpha.example/a");
  check "a title with a line break and brackets stays on one line"
    (contains md "[A title that \\[breaks\\] lines](")

(* {1 Network} *)

module Contact = Sortal_schema.Contact

let feed url =
  Sortal_schema.Feed.make ~feed_type:Sortal_schema.Feed.Atom ~url ()

let () =
  let contacts =
    [ Contact.make ~handle:"zed" ~names:[ "Zed Person" ]
        ~feeds:[ feed "https://zed.example/feed.xml" ] ();
      Contact.make ~handle:"acme" ~names:[ "Acme Org" ]
        ~kind:Contact.Organization
        ~feeds:[ feed "https://acme.example/feed.xml" ] ();
      Contact.make ~handle:"amy" ~names:[ "Amy Person" ]
        ~feeds:[ feed "https://amy.example/feed.xml" ] ();
      Contact.make ~handle:"quiet" ~names:[ "No Feed" ] () ]
  in
  let md =
    Arod_component.Markdown_export.network_md ~ctx:(ctx_of ~contacts ())
  in
  check "the introduction of the HTML page opens the page"
    (before md "I track a number of online blogs" "0 posts"
    && contains md "[OPML here](https://example.com/network/blogroll.opml)"
    && contains md "[let me know](mailto:anil@recoil.org)");
  check "the counts of the sidebar are given"
    (contains md "0 posts, 3 contacts.");
  check "the timeline comes before the blogroll, as the page does"
    (before md "0 posts" "## People");
  check "people come before organisations, as in the sidebar"
    (before md "## People" "## Organisations"
    && before md "- Amy Person" "## Organisations"
    && before md "## Organisations" "- Acme Org");
  check "people with no post are alphabetical"
    (before md "- Amy Person" "- Zed Person");
  check "a contact with no feed is not listed"
    (not (contains md "No Feed"));
  check "feeds are linked by type"
    (contains md "[Atom](https://amy.example/feed.xml)")

(* {1 Entries} *)

let () =
  let n =
    { (note ~slug:"tagged" ~title:"Tagged" ~date:(2026, 1, 2) ()) with
      Bushel.Note.body =
        "On [systems](##systems) and [##fp], not [plain](https://x.example)." }
  in
  let md =
    Arod_component.Markdown_export.entry_to_markdown
      ~ctx:(ctx_of ~notes:[ n ] ()) (`Note n)
  in
  check "a tag link shows its hash, as the HTML shows it"
    (contains md "[#systems](https://example.com/tags/systems)"
    && contains md "[#fp](https://example.com/tags/fp)");
  check "a link that is not a tag has no hash"
    (contains md "[plain](https://x.example)")

let paper_entry ?(projects = []) ~slug ~title ~year () : Bushel.Paper.t =
  { Bushel.Paper.slug; ver = "1"; title; authors = [ "Ada Lovelace"; "B. Bee" ];
    year; month = 6; bibtype = "article"; publisher = "A Publisher";
    booktitle = ""; journal = "A Journal"; institution = ""; pages = "";
    volume = Some "7"; number = None; doi = Some "10.1/paper";
    url = Some "https://www.journal.example/paper"; video = None; isbn = "";
    editor = ""; bib = ""; tags = []; projects; slides = [];
    abstract = "The abstract text."; latest = true; selected = false;
    classification = None; note = None; social = None }

let link_between source target target_type =
  { Bushel.Link_graph.source; target; target_type }

let () =
  let paper =
    paper_entry ~slug:"p1" ~title:"A Paper" ~year:2025 ~projects:[ "big" ] ()
  in
  let early =
    { (note ~slug:"early" ~title:"Early Note" ~date:(2025, 1, 1)
         ~synopsis:(Some "Early synopsis.") ()) with
      Bushel.Note.body = "Early body." }
  in
  let late =
    { (note ~slug:"late" ~title:"Late Note" ~date:(2025, 9, 1) ()) with
      Bushel.Note.body = "Late body." }
  in
  let target =
    { (note ~slug:"target" ~title:"Target" ~date:(2025, 5, 1) ()) with
      Bushel.Note.body = "Target body." }
  in
  let vid =
    video ~slug:"v1" ~title:"A Video" ~date:(2025, 2, 1) "The description."
  in
  let ctx_with_graph =
    let entries =
      Bushel.Entry.v ~papers:[ paper ] ~notes:[ early; late; target ]
        ~projects:ideas_projects ~ideas ~videos:[ vid ] ~contacts:[]
        ~data_dir:"." ()
    in
    let entries =
      Bushel.Entry.with_graph entries
        (Bushel.Link_graph.v
           ~internal_links:
             [ link_between "early" "target" `Note;
               link_between "late" "target" `Note;
               link_between "late" "big" `Project ]
           ~external_links:[])
    in
    Arod.Ctx.of_entries ~config:cfg entries
  in
  let export ent =
    Arod_component.Markdown_export.entry_to_markdown ~ctx:ctx_with_graph ent
  in
  let md_paper = export (`Paper paper) in
  check "a paper gives its authors, its links and then its abstract"
    (before md_paper "Authors:" "Links:"
    && before md_paper "Links:" "## Abstract"
    && before md_paper "## Abstract" "The abstract text.");
  check "a paper's links are in the order of the page"
    (before md_paper "[BIB]" "[DOI]" && before md_paper "[DOI]" "[URL]");
  let md_video = export (`Video vid) in
  check "a video says where to watch it before its description"
    (before md_video "Watch: <https://videos.example/v1>" "The description.");
  let md_idea =
    export (`Idea (List.find (fun i -> Bushel.Idea.slug i = "a") ideas))
  in
  check "an idea gives its status line before its body"
    (before md_idea "Status: Available" "A opens here.");
  let md_target = export (`Note target) in
  check "related content lists the entries that link to a note, newest first"
    (before md_target "[Late Note]" "[Early Note]"
    && before md_target "## Related" "[Late Note]");
  check "a related entry has the detail line of its row"
    (contains md_target "Early synopsis.");
  check "a note that cites nothing has no references"
    (not (contains md_target "## References"));
  let md_project =
    export
      (`Project
         (List.find (fun p -> Bushel.Project.slug p = "big") ideas_projects))
  in
  check "a project lists its ideas and then its activity"
    (before md_project "## Ideas" "## Activity"
    && before md_project "[Open Big A]" "## Activity");
  check "a project's activity includes its papers and the notes that link to it"
    (before md_project "## Activity" "[A Paper]"
    && contains md_project "[Late Note]");
  check "a project has no related section, as its page has none"
    (not (contains md_project "## Related"))

(* {1 The index of the Markdown twins} *)

let () =
  let ctx =
    ctx_of ~videos:[
      video ~slug:"talk" ~title:"A Talk" ~date:(2025, 2, 1) "";
      video ~slug:"clip" ~title:"A Plain Video" ~talk:false ~date:(2025, 3, 1)
        "" ] ()
  in
  let txt = Arod_handlers.Render.llms_txt ~ctx in
  check "llms.txt lists talks under talks, as the talks page does"
    (before txt "## Talks" "[A Talk]"
    && before txt "[A Talk]" "## Other videos");
  check "llms.txt lists the videos that are not talks apart"
    (before txt "## Other videos" "[A Plain Video]");
  Printf.printf "test_markdown_pages: %d checks passed\n" !checks
