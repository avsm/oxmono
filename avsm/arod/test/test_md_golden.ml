(* The rendering oracle. Every other test in this directory pins a decision
   the server makes about a request. None of them looks at what the markdown
   path emits: the route tests drive a mock backend whose renderers are stubs,
   so the whole of Cmarkit, the Bushel link extensions and the Arod HTML
   renderers are outside anything that runs under [dune runtest].

   That matters because those renderers are about to be rebuilt underneath.
   Cmarkit is to be vendored and annotated, Bushel.Entry.t is to lose its hash
   tables, and the render path is to move from opam uri to uriz. Each of those
   is meant to leave the bytes alone, and none of them can be shown to have
   done so without a corpus and the bytes it produced before the change.

   So this test renders a small checked-in corpus through the real
   Arod.Md.to_html, Arod.Md.to_plain_html and Arod.Md.to_atom_html and
   byte-compares each result against a golden file. It also renders each
   corpus twice, with a different corpus rendered in between, and requires
   the two results to be equal: the heading numbers, the footnote numbers and
   the sidenote list are all produced by counters and refs, and a counter that
   is not reset per render would show up here and nowhere else. The golden
   comparison is the same check across two processes, since the golden bytes
   were written by an earlier run.

   The corpus stops at the three markdown renderers and does not reach
   Arod_handlers.Render.listing or .entry, both of which do run over a
   context built by Arod.Ctx.of_entries. Two things argue against pinning a
   whole page. A page is about 110 kB of which the rendered body is under
   one, so a nav or stylesheet edit would rewrite the golden file and hide
   the bytes this test exists to watch. And some pages read the clock, so a
   full-page golden would start failing on a later day with nothing having
   changed: the weeknotes listing walks back from the current ISO week
   (lib_component/note.ml:243) and the blogroll stamps the time it was
   generated (lib_handlers/arod_render.ml:393).

   Three warts in the golden bytes are production behaviour, pinned on
   purpose. Do not quietly correct one while working on something else: the
   fix belongs in its own commit with its own regeneration, or this test will
   blame whichever change happens to be passing through.

   - A slug that names no entry renders as a link with an empty href, in all
     three renderers. Bushel.Entry.lookup_site_url returns "" for a miss.
   - A tag link, a kind link and an inline contact link keep their target
     only in the article renderer. The plain and atom renderers emit an empty
     href, because the ##tag, ###kind and @handle destinations are rewritten
     into real ones by the article renderer alone.
   - The atom footnote list nests a paragraph inside a paragraph. The item
     template at arod_md.ml:864 wraps in <p>, and the block it wraps is
     already rendered as one. That is invalid HTML which feed readers accept.

   To regenerate the golden files after an intended change, from the
   workspace root:

     dune exec avsm/arod/test/test_md_golden.exe -- --regen avsm/arod/test/fixtures

   That writes the golden files the current corpus produces and deletes
   nothing, so a golden file left behind by a corpus document that has been
   renamed or removed would survive it. Both a plain run and a regeneration
   refuse to proceed while such a file is there, naming it. Delete it by
   hand and run again.

   Read the diff before committing it. A change in these bytes is a change to
   every page the site serves. *)

module Contact = Sortal_schema.Contact

let checks = ref 0

let check name b =
  incr checks;
  if not b then (
    prerr_endline ("FAIL: " ^ name);
    exit 1)

(* {1 The entry set the corpus links into}

   These are the link targets, not the thing under test. They are built here
   rather than loaded from fixture files so that the fixture directory holds
   only the markdown that is rendered and the bytes it must produce. *)

let note ?titleimage ~slug ~title ~date () =
  {
    Bushel.Note.title;
    date;
    slug;
    body = "";
    tags = [];
    draft = false;
    updated = None;
    sidebar = None;
    index_page = false;
    perma = false;
    weeknote = false;
    featured = false;
    doi = None;
    synopsis = None;
    titleimage;
    via = None;
    slug_ent = None;
    source = None;
    url = None;
    author = None;
    category = None;
    standardsite = None;
    social = None;
    source_file = None;
  }

let paper ~slug ~title ~year =
  {
    Bushel.Paper.slug;
    ver = "1";
    title;
    authors = [ "Ada Lovelace" ];
    year;
    month = 6;
    bibtype = "inproceedings";
    publisher = "A Publisher";
    booktitle = "A Conference";
    journal = "";
    institution = "";
    pages = "1-10";
    volume = None;
    number = None;
    doi = Some "10.1234/sample";
    url = None;
    video = None;
    isbn = "";
    editor = "";
    bib = "";
    tags = [];
    projects = [];
    slides = [];
    abstract = "";
    latest = true;
    selected = false;
    classification = None;
    note = None;
    social = None;
  }

let project ~slug ~title ~start =
  {
    Bushel.Project.slug;
    title;
    start;
    finish = Some 2024;
    tags = [];
    ideas = "";
    body = "";
    social = None;
  }

let idea ~slug ~title ~year =
  {
    Bushel.Idea.slug;
    title;
    level = Bushel.Idea.MPhil;
    project = ":a-project";
    status = Bushel.Idea.Available;
    month = 3;
    year;
    supervisors = [];
    students = [];
    supervisor_handles = [];
    student_handles = [];
    reading = "";
    body = "";
    url = None;
    tags = [];
    social = None;
  }

let video ~slug ~title ~date =
  {
    Bushel.Video.slug;
    title;
    published_date = Option.get (Ptime.of_date date);
    uuid = "video-uuid";
    description = "";
    url = "https://example.com/watch";
    talk = true;
    vertical = false;
    paper = None;
    project = None;
    tags = [];
    social = None;
  }

let sample_image =
  let variants =
    Srcsetter.MS.of_list
      [ ("sample-480.webp", (480, 320)); ("sample-960.webp", (960, 640)) ]
  in
  Srcsetter.v "sample.webp" "sample-image" "src/sample.png" variants (1920, 1280)

let ada = Contact.make ~handle:"ada" ~names:[ "Ada Lovelace"; "A. Lovelace" ] ()

(* [hello-note] carries a title image so that one sidenote in the golden
   output has a thumbnail URL rather than the [-] the others print. The image
   it names is [sample_image], which is also what fixtures/md/image.md
   renders, so one entry covers both. *)
let entries =
  Bushel.Entry.v
    ~papers:[ paper ~slug:"a-paper" ~title:"A Sample Paper" ~year:2023 ]
    ~notes:
      [
        note ~slug:"hello-note" ~title:"Hello Note" ~date:(2025, 1, 5)
          ~titleimage:"sample-image" ();
      ]
    ~projects:[ project ~slug:"a-project" ~title:"A Sample Project" ~start:2020 ]
    ~ideas:[ idea ~slug:"an-idea" ~title:"A Sample Idea" ~year:2024 ]
    ~videos:[ video ~slug:"a-video" ~title:"A Sample Video" ~date:(2022, 9, 1) ]
    ~contacts:[ ada ] ~images:[ sample_image ] ~data_dir:"." ()

let ctx =
  let config = Arod.Config.default in
  let site = { config.Arod.Config.site with author_handle = "ada" } in
  Arod.Ctx.of_entries ~config:{ config with site } entries

(* {1 Files} *)

let read path =
  let ic = open_in_bin path in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  s

let write path s =
  let oc = open_out_bin path in
  output_string oc s;
  close_out oc

(* The first line on which two renderings differ, with the byte offset of the
   first differing byte. Enough to point at the construct that moved without
   printing two pages of HTML. *)
let diff_hint ~expected ~actual =
  let n = min (String.length expected) (String.length actual) in
  let rec first i = if i >= n then n else if expected.[i] = actual.[i] then first (i + 1) else i in
  let i = first 0 in
  let line s upto =
    let start = match String.rindex_from_opt s (max 0 (upto - 1)) '\n' with
      | Some j -> j + 1
      | None -> 0
    in
    let stop = match String.index_from_opt s start '\n' with Some j -> j | None -> String.length s in
    String.sub s start (stop - start)
  in
  let count_nl s upto = String.fold_left (fun n c -> if c = '\n' then n + 1 else n) 0 (String.sub s 0 upto) in
  Printf.sprintf
    "  first difference at byte %d, line %d\n  expected: %s\n  actual:   %s"
    i (count_nl expected i + 1)
    (if i >= String.length expected then "<end of file>" else line expected i)
    (if i >= String.length actual then "<end of file>" else line actual i)

(* {1 The corpus}

   Every file under [md/] is rendered by every renderer named here, and by
   the sidenote collector below. Adding a markdown file and running the
   regeneration command adds four golden files. *)

let renderers =
  [
    ("article", fun content -> fst (Arod.Md.to_html ~ctx content));
    ("plain", fun content -> Arod.Md.to_plain_html ~ctx content);
    ("atom", fun content -> Arod.Md.to_atom_html ~ctx content);
  ]

(* [to_html] also returns the sidenotes it collected, which are the sidebar
   and are as much a part of the rendering as the article. They are compared
   as their own golden file rather than folded into the HTML one, so that a
   sidenote that stops being collected reads as a sidenote failure. *)
let sidenotes_of content =
  let _, sns = Arod.Md.to_html ~ctx content in
  String.concat ""
    (List.map
       (fun (s : Arod.Md.sidenote) ->
         Printf.sprintf "%s\t%s\t%s\n" s.slug
           (match s.thumb_url with Some u -> u | None -> "-")
           s.content_html)
       sns)

let corpus dir =
  Sys.readdir (Filename.concat dir "md")
  |> Array.to_list
  |> List.filter (fun f -> Filename.check_suffix f ".md")
  |> List.sort String.compare

let () =
  let regen, dir =
    match Array.to_list Sys.argv with
    | _ :: "--regen" :: dir :: _ -> (true, dir)
    | _ :: dir :: _ -> (false, dir)
    | _ -> (false, "fixtures")
  in
  let names = corpus dir in
  check "the corpus is not empty" (names <> []);
  let sources =
    List.map (fun n -> (n, read (Filename.concat (Filename.concat dir "md") n))) names
  in
  (* Render everything once, then render it all again in the same process.
     The second pass is what would catch a counter or a table that survives a
     render. Interleaving the corpus means a leak from one document into the
     next is caught as well as a leak from a document into itself. *)
  let render_all () =
    List.concat_map
      (fun (name, content) ->
        let base = Filename.remove_extension name in
        List.map
          (fun (which, f) -> (Printf.sprintf "%s.%s.html" base which, f content))
          renderers
        @ [ (base ^ ".sidenotes.txt", sidenotes_of content) ])
      sources
  in
  let first = render_all () in
  let second = render_all () in
  (* One assertion per rendered file. [List.iter2] raises if the two passes
     disagree on how many files they produced, which cannot happen without a
     bug in [render_all] itself. *)
  List.iter2
    (fun (n, a) (_, b) ->
      if a <> b then (
        prerr_endline ("FAIL: " ^ n ^ " is not deterministic within one process");
        prerr_endline (diff_hint ~expected:a ~actual:b);
        exit 1);
      incr checks)
    first second;
  let golden_dir = Filename.concat dir "golden" in
  (* A golden file that the corpus no longer produces is stale: its markdown
     was renamed or deleted, and regeneration writes files without clearing
     the directory, so nothing else would notice. A stale golden is worse
     than a missing one, because it reads as coverage that is not there. *)
  let stale =
    if not (Sys.file_exists golden_dir) then []
    else
      let produced = List.map fst first in
      Sys.readdir golden_dir |> Array.to_list
      |> List.filter (fun f -> not (List.mem f produced))
      |> List.sort String.compare
  in
  incr checks;
  if stale <> [] then (
    prerr_endline "FAIL: golden files that no corpus document produces:";
    List.iter (fun f -> prerr_endline ("  " ^ Filename.concat golden_dir f)) stale;
    prerr_endline "  delete them by hand, then regenerate";
    exit 1);
  if regen then (
    if not (Sys.file_exists golden_dir) then Sys.mkdir golden_dir 0o755;
    List.iter (fun (n, s) -> write (Filename.concat golden_dir n) s) first;
    Printf.printf "wrote %d golden files to %s\n" (List.length first) golden_dir)
  else (
    List.iter
      (fun (n, actual) ->
        let path = Filename.concat golden_dir n in
        incr checks;
        if not (Sys.file_exists path) then (
          prerr_endline ("FAIL: no golden file " ^ path);
          prerr_endline "  run the regeneration command in the header comment";
          exit 1);
        let expected = read path in
        if expected <> actual then (
          prerr_endline ("FAIL: " ^ n ^ " does not match its golden file");
          prerr_endline (diff_hint ~expected ~actual);
          prerr_endline "  if the change is intended, regenerate as the header comment says";
          exit 1))
      first;
    Printf.printf "test_md_golden: %d checks over %d documents passed\n" !checks
      (List.length sources))
