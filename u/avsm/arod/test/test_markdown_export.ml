(* The papers listing served as markdown is pinned by shape: the order the
   HTML listing uses, one heading per year, and the detail a reader of the
   HTML card gets. *)

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
  match (find hay a, find hay b) with
  | Some i, Some j -> i < j
  | _ -> false

let cfg : Arod.Config.t =
  { Arod.Config.default with
    site = { Arod.Config.default.site with base_url = "https://example.com" } }

let paper ?(url = None) ?(doi = None) ?(bibtype = "inproceedings")
    ?(booktitle = "") ?(journal = "") ?(volume = None) ~slug ~title ~year
    ~month () : Bushel.Paper.t =
  { Bushel.Paper.slug; ver = "1"; title; authors = [ "Ada Lovelace"; "B. Bee" ];
    year; month; bibtype; publisher = "A Publisher"; booktitle; journal;
    institution = ""; pages = ""; volume; number = None; doi; url;
    video = None; isbn = ""; editor = ""; bib = ""; tags = []; projects = [];
    slides = []; abstract = ""; latest = true; selected = false;
    classification = None; note = None; social = None }

(* Given oldest first, so that a listing in load order is the wrong order. *)
let papers =
  [ paper ~slug:"old" ~title:"Old Paper" ~year:2019 ~month:3
      ~booktitle:"Old Conference" ~doi:(Some "10.1/old") ();
    paper ~slug:"mid" ~title:"Mid Paper" ~year:2021 ~month:11
      ~bibtype:"article" ~journal:"A Journal" ~volume:(Some "7")
      ~url:(Some "https://www.journal.example/mid") ();
    paper ~slug:"new" ~title:"New Paper" ~year:2021 ~month:12
      ~bibtype:"misc" ~doi:(Some "10.1/new") () ]

let ctx =
  Arod.Ctx.of_entries ~config:cfg
    (Bushel.Entry.v ~papers ~notes:[] ~projects:[] ~ideas:[] ~videos:[]
       ~contacts:[] ~data_dir:"." ())

let md = Arod_component.Markdown_export.papers_list_md ~ctx

let () =
  check "newest first" (before md "New Paper" "Mid Paper");
  check "oldest last" (before md "Mid Paper" "Old Paper");
  check "year headings" (contains md "## 2021\n" && contains md "## 2019\n");
  check "one heading per year"
    (find md "## 2021" = find md "## 2021"
    && not (before md "## 2019" "## 2021"));
  check "entry link"
    (contains md "[New Paper](https://example.com/papers/new)");
  check "month and classification" (contains md "Dec 2021, preprint");
  check "authors" (contains md "Ada Lovelace and B. Bee.");
  check "conference venue" (contains md "Paper in the Old Conference.");
  check "journal venue with link and volume"
    (contains md
       "Journal paper in [A Journal](https://www.journal.example/mid) (vol 7).");
  check "doi link" (contains md "[DOI](https://doi.org/10.1/old)");
  check "bib link" (contains md "[BIB](https://example.com/papers/old.bib)");
  check "url link with host"
    (contains md "[URL](https://www.journal.example/mid) (journal.example)");
  check "summary counts" (contains md "3 papers");
  Printf.printf "test_markdown_export: %d checks passed\n" !checks
