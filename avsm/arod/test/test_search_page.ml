(* The search page markup is pinned by shape, not byte: each check names one
   element the JavaScript or the reader relies on. *)

let checks = ref 0

let check name cond =
  incr checks;
  if not cond then (
    prerr_endline ("FAIL: " ^ name);
    exit 1)

let contains hay needle =
  let n = String.length needle and h = String.length hay in
  let rec go i = i + n <= h && (String.sub hay i n = needle || go (i + 1)) in
  go 0

let cfg : Arod.Config.t =
  { Arod.Config.default with
    site = { Arod.Config.default.site with base_url = "https://example.com" } }

let note : Bushel.Note.t =
  { Bushel.Note.title = "Xen Hypervisor"; date = (2024, 2, 3); slug = "xen";
    body = "Body."; tags = [ "xen" ]; draft = false; updated = None;
    sidebar = None; index_page = false; perma = false; weeknote = false;
    featured = false; doi = None; synopsis = None; titleimage = None;
    via = None; slug_ent = None; source = None; url = None; author = None;
    category = None; standardsite = None; social = None; source_file = None }

let ctx =
  Arod.Ctx.of_entries ~config:cfg
    (Bushel.Entry.v ~papers:[] ~notes:[ note ] ~projects:[] ~ideas:[]
       ~videos:[] ~contacts:[] ~data_dir:"." ())

let hit ?(kind = "note") ?(url = "/notes/xen") ?(parent_slugs = [])
    ?(tags = []) title : Arod_search.hit =
  { Arod_search.slug = "xen"; kind; url; title; snippet = "<b>Xen</b> body";
    date = "2024-02-03"; tags; parent_slugs; score = 3.0 }

let results : Arod_search.results =
  { Arod_search.terms = [ "xen" ];
    goto = [ { Arod_search.label = "xen"; url = "/search?q=%23xen";
               detail = "1 entry"; goto_kind = `Tag } ];
    work = [ hit ~tags:[ "xen"; "systems" ] "Xen Hypervisor" ];
    work_total = 30;
    links = [ hit ~kind:"link" ~url:"https://wiki.xen.org/XenStore"
                ~parent_slugs:[ "xen" ] "XenStore - Xen" ];
    links_total = 5;
    kinds = [ ("note", 30) ]; years = [ (2024, 30) ];
    tags = [ ("xen", 29) ] }

let unresolved_via_results : Arod_search.results =
  { results with
    links = [ hit ~kind:"link" ~url:"https://example.com/x"
                ~parent_slugs:[ "unknown" ] "Example" ] }

let html = Htmlit.El.to_string ~doctype:false

let () =
  let f =
    html
      (Arod_component.Search.fragment ~ctx ~q:"xen" ~order:`Relevance
         results)
  in
  check "the fragment has the id the script swaps"
    (contains f {|id="search-results"|});
  check "the count line states both totals"
    (contains f "30 on this site" && contains f "5 links");
  check "a go-to chip links to the tag filter"
    (contains f {|href="/search?q=%23xen"|} && contains f "1 entry");
  check "a work row marks the matched title word"
    (contains f "<b>Xen</b> Hypervisor");
  check "a work row shows its tags"
    (contains f "#systems");
  check "a link row falls back to the glyph when there is no favicon"
    (contains f {|class="sp-fav"|} && not (contains f "<img"));
  check "a link row links the citing entry to its page"
    (contains f
       {|<a href="/notes/xen" class="sp-via"><span class="sp-via-in">in |});
  check "and names it"
    (contains f {|in </span>Xen Hypervisor</a>|});
  check "the row carries its own destination for the script"
    (contains f {|data-href="https://wiki.xen.org/XenStore"|});
  check "the sort toggle marks the active order"
    (contains f {|data-sort="relevance"|} && contains f {|data-sort="date"|}
    && contains f {|class="sp-sort-opt on" data-sort="relevance"|}
    || contains f {|data-sort="relevance" class="sp-sort-opt on"|});
  check "the link row shows the host"
    (contains f "wiki.xen.org");
  let f2 =
    html
      (Arod_component.Search.fragment ~ctx ~q:"xen" ~order:`Relevance
         unresolved_via_results)
  in
  check "a link row with an unresolved parent slug shows no via span"
    (not (contains f2 {|class="sp-via"|}));
  check "a truncated tier offers more"
    (contains f "Show 29 more" && contains f "Show 4 more");
  check "facets carry the filter to add"
    (contains f {|data-kind="note"|} && contains f {|data-tag="xen"|});
  check "the histogram has a bar per year"
    (contains f {|class="sp-year hot"|})

let () =
  let article, _ =
    Arod_component.Search.page_body ~ctx ~q:"xen" ~order:`Relevance results
  in
  let a = html article in
  check "the page has a form that submits q to /search"
    (contains a {|action="/search"|} && contains a {|name="q"|}
    && contains a {|value="xen"|});
  check "and contains the fragment" (contains a {|id="search-results"|});
  check "and the spinner the script animates"
    (contains a {|id="search-spinner"|});
  check "the input carries the id the page script binds to"
    (contains a {|id="search-page-input"|});
  let article, _ =
    Arod_component.Search.page_body ~ctx ~q:"" ~order:`Relevance
      Arod_search.empty in
  check "an empty query shows the prompt and autofocuses"
    (contains (html article) "autofocus"
    && contains (html article) "Type to search")

let () =
  check "mark wraps a word starting with a term"
    (html (Arod_component.Search.mark ~terms:[ "uni" ] "The Unikernel way")
     = "The <b>Unikernel</b> way");
  check "mark escapes what it does not wrap"
    (html (Arod_component.Search.mark ~terms:[] "a < b") = "a &lt; b")

let () = Printf.printf "test_search_page: %d checks ok\n" !checks
