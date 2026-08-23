(* The route table is where a literal and a capture at the same depth can
   silently swap places, and where the auth scope and the cache headers are
   decided. It is driven here through the mock backend over an environment
   whose remaining closures are stubs, so no database and no filesystem is
   needed. The context is real, because every page render is portable and the
   handlers reach those through it. *)

module H = Httpz.Header_name
module M = Httpz.Method

let checks = ref 0

let check name b =
  incr checks;
  if not b then (
    prerr_endline ("FAIL: " ^ name);
    exit 1)

let cfg : Arod.Config.t =
  {
    Arod.Config.default with
    server = { Arod.Config.default.server with stats_password = Some "pw" };
    site = { Arod.Config.default.site with base_url = "https://example.com" };
    well_known = [ { Arod.Config.key = "known"; value = "value" } ];
  }

(* The context below holds one paper and one note, which is the smallest
   collection every page route can render from. The paper gives the [.bib] and
   [.pdf] routes something to name, and the blogroll answers an OPML document
   with no outlines in it, since a blogroll lists contacts that have feeds and
   this context has no contacts. *)
let paper : Bushel.Paper.t =
  {
    Bushel.Paper.slug = "x";
    ver = "1";
    title = "A Paper";
    authors = [ "Ada Lovelace" ];
    year = 2024;
    month = 1;
    bibtype = "article";
    publisher = "";
    booktitle = "";
    journal = "";
    institution = "";
    pages = "";
    volume = None;
    number = None;
    doi = None;
    url = None;
    video = None;
    isbn = "";
    editor = "";
    bib = "@x";
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

(* The front page renders the entry whose slug is ["index"], so the context
   needs one for [/] to answer anything but the empty string. *)
let index_note : Bushel.Note.t =
  {
    Bushel.Note.title = "Index";
    date = (2024, 2, 3);
    slug = "index";
    body = "Hello from the index.";
    tags = [];
    draft = false;
    updated = None;
    sidebar = None;
    index_page = true;
    perma = false;
    weeknote = false;
    featured = false;
    doi = None;
    synopsis = None;
    titleimage = None;
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

let ctx =
  Arod.Ctx.of_entries ~config:cfg
    (Bushel.Entry.v ~papers:[ paper ] ~notes:[ index_note ] ~projects:[]
       ~ideas:[] ~videos:[] ~contacts:[] ~data_dir:"." ())

(* A handler is portable, so it cannot reach a log source. The search API
   reports through a closure in the environment instead, and the stub below
   records each call for the checks to read back. The stub search returns the
   length of the query as its result count, which is enough to tell the two
   log points apart. *)
let searches = ref []

let env =
  {
    Arod_handlers.Env.ctx;
    config = cfg;
    cache = Proffer.Cache.create ~ttl:60.0;
    now = (fun () -> 0.0);
    feed =
      (fun which ->
        match which with
        | `Atom path -> "atom:" ^ path
        | `Json -> "json"
        | `Perma_atom -> "perma-atom"
        | `Perma_json -> "perma-json");
    pagination =
      (fun ~collection ~offset ~limit ~types ->
        ignore types;
        let s =
          Printf.sprintf "%s/%d/%d"
            (Option.value ~default:"none" collection)
            offset limit
        in
        fun sink -> Proffer.Body.Sink.write sink s);
    search =
      (fun ~q ~limit ~link_limit ->
        let s = Printf.sprintf "%s/%d/%d" q limit link_limit in
        ((fun sink -> Proffer.Body.Sink.write sink s), String.length q));
    search_page =
      (fun ~q ~limit ~link_limit ~fragment ->
        Printf.sprintf "page:%s/%d/%d/%b" q limit link_limit fragment);
    log_search =
      (fun ~query ~limit ~results ->
        let results =
          match results with None -> "?" | Some n -> string_of_int n
        in
        searches := Printf.sprintf "%s/%d/%s" query limit results :: !searches);
    read_image =
      (fun segs ->
        match Proffer.Static.confine segs with
        | Some path -> Some ("image:" ^ path)
        | None -> None);
    read_paper = (fun name -> if name = "gone.pdf" then None else Some name);
    report = (fun _which ~range -> "report:" ^ range);
  }

(* Every page render is portable, so the environment above holds no rendering
   closure for one and the routes below reach the real render through the
   context. The context holds one paper and one note, so the pages are small
   and the bytes are stable, but they are still whole pages: what is checked
   is that the route reached the render that belongs to it, not what that
   render says. [test_md_golden] owns the bytes. *)
let contains hay needle =
  let n = String.length needle and h = String.length hay in
  let rec go i = i + n <= h && (String.sub hay i n = needle || go (i + 1)) in
  go 0

let site = Arod_server.Site.build cfg
let header_other o n = Proffer_mock.header_other o n
let get ?headers target = Proffer_mock.request ?headers site env
  M.Get target
let code r = Proffer.Status.code (Proffer_mock.status r)
let body = Proffer_mock.body
let header = Proffer_mock.header

let () =
  let front = body (get "/") in
  check "the front page is served"
    (String.starts_with ~prefix:"<!DOCTYPE html>" front
    && contains front "<title>Index | My Site</title>");
  check "so is /about" (body (get "/about") = front);
  (* The markdown rendering of the front page is short enough to pin whole,
     which is the one place here that says a real render ran rather than a
     page-shaped stub. *)
  check "a markdown Accept picks the markdown variant"
    (body (get ~headers:[ ("Accept", "text/markdown") ] "/")
    = "# Index\n\nHello from the index.\n\n---\nCanonical: https://example.com\n");
  check "a negotiated response varies on Accept"
    (header (get "/") H.Vary = Some "Accept");
  check "a .md URL is the markdown variant"
    (let b = body (get "/papers.md") in
     String.starts_with ~prefix:"# Papers\n" b
     && contains b "[A Paper](https://example.com/papers/x)")

let () =
  check "a feed path is not swallowed by the note route"
    (body (get "/notes/atom.xml") = "atom:/notes/atom.xml");
  check "and neither is the JSON feed"
    (body (get "/notes/feed.json") = "json");
  check "a note slug still reaches the note route"
    (contains (body (get "/notes/index"))
       "<link rel=\"canonical\" href=\"https://example.com/notes/index\">");
  check "and a slug that names no entry renders as the empty string"
    (body (get "/notes/hello") = "");
  check "the Atom feed names the path it was fetched from"
    (body (get "/news.xml") = "atom:/news.xml")

let () =
  let r = get "/atom.xml" in
  check "a retired feed path is a permanent redirect" (code r = 301);
  check "and it points at the current one"
    (header r H.Location = Some "/news.xml");
  let r = get "/notes/index.html" in
  check "a list index.html redirects to the list" (code r = 301);
  check "at the canonical path"
    (header r H.Location = Some "/notes");
  let r = get "/notes/hello/index.html" in
  check "an entry index.html redirects to the entry" (code r = 301);
  check "carrying the slug"
    (header r H.Location = Some "/notes/hello");
  let r = get "/tags/ocaml" in
  check "a tag link is a temporary redirect" (code r = 302);
  check "into the front page's tag filter"
    (header r H.Location = Some "/#tag=ocaml");
  let r = get "/news/hello" in
  check "a legacy news URL redirects to the note" (code r = 301);
  check "keeping the slug"
    (header r H.Location = Some "/notes/hello")

let () =
  (* A capture arrives decoded, so what goes back into a Location has to be
     encoded again. A space would otherwise redirect to the wrong place, and a
     CR would make Resp.redirect refuse the field and answer 500. *)
  let r = get "/news/foo%20bar" in
  check "a space in a capture is encoded back into the Location"
    (header r H.Location = Some "/notes/foo%20bar");
  check "and the redirect is still a redirect" (code r = 301);
  let r = get "/news/a%0Db" in
  check "a CR in a capture does not become a 500" (code r = 301);
  check "it is encoded instead"
    (header r H.Location = Some "/notes/a%0Db");
  let r = get "/notes/foo%20bar/index.html" in
  check "an entry index.html encodes its slug too"
    (header r H.Location = Some "/notes/foo%20bar");
  let r = get "/tags/two%20words" in
  check "so does the tag redirect"
    (header r H.Location = Some "/#tag=two%20words");
  check "an ordinary slug is left alone"
    (header (get "/news/hello-there") H.Location
    = Some "/notes/hello-there")

let () =
  let enc = Arod_handlers.encode_segment in
  check "an unreserved slug encodes to itself" (enc "a-b_c.d~e9" = "a-b_c.d~e9");
  check "a separator is encoded" (enc "a/b" = "a%2Fb");
  check "so is a byte above ASCII" (enc "\xc3\xa9" = "%C3%A9");
  check "and the empty segment stays empty" (enc "" = "")

let () =
  (* The feed names itself from the route table, not from the target, so an
     oddly encoded target cannot mint a cache entry of its own. Proffer's
     cache never removes an expired entry, which is what makes that matter. *)
  check "the feed path comes from the route, not the request"
    (body (get "/%6Eotes/%61tom.xml") = "atom:/notes/atom.xml")

let () =
  check "a paper PDF is served from the paper directory"
    (body (get "/papers/x.pdf") = "x.pdf");
  check "with a Content-Type from its extension"
    (header (get "/papers/x.pdf") H.Content_type
    = Some "application/pdf");
  check "a missing PDF is a 404" (code (get "/papers/gone.pdf") = 404);
  check "a .bib URL is the BibTeX entry" (body (get "/papers/x.bib") = "@x");
  check "and a .bib URL for no paper is a 404"
    (code (get "/papers/gone.bib") = 404);
  (* The blogroll is a real render, not a stub, so what is pinned here is
     that it ran and produced an OPML document rather than what it says. Its
     head stamps the clock, so the bytes cannot be pinned whole. *)
  check "the blogroll route runs the real render"
    (let b = body (get "/network/blogroll.opml") in
     String.starts_with ~prefix:"<?xml version=\"1.0\" encoding=\"UTF-8\"?>" b
     && String.ends_with ~suffix:"<body/></opml>" b);
  check "and serves it as OPML"
    (header (get "/network/blogroll.opml") H.Content_type
    = Some "text/x-opml+xml; charset=utf-8");
  check "an entry .md URL is that entry as markdown"
    (let b = body (get "/papers/x.md") in
     String.starts_with ~prefix:"# A Paper\n" b
     && contains b "Canonical: https://example.com/papers/x");
  check "and an unknown one is a 404" (code (get "/papers/gone.md") = 404)

let () =
  check "an image is read through the confined directory"
    (body (get "/images/a/b.png") = "image:a/b.png");
  check "a traversal in the tail is refused"
    (code (get "/images/../etc/passwd") = 404);
  check "an embedded asset is served"
    (header (get "/favicon.svg") H.Content_type
    = Some "image/svg+xml");
  check "a versioned asset may be cached for ever"
    (header (get "/tw.css") H.Cache_control
    = Some "public, max-age=31536000, immutable")

let () =
  check "the pagination API reads its query"
    (body (get "/api/entries?collection=links&offset=5&limit=10")
    = "links/5/10");
  check "an out of range limit is clamped"
    (body (get "/api/entries?collection=links&limit=9999") = "links/0/100");
  check "the search API reads its query"
    (body (get "/api/search?q=ocaml&limit=3&link_limit=2") = "ocaml/3/2");
  check "and logs before and after"
    (List.rev !searches = [ "ocaml/3/?"; "ocaml/3/5" ]);
  searches := [];
  check "an absent query is the empty string"
    (body (get "/api/search") = "/20/12");
  check "the search page reads its query and limits"
    (body (get "/search?q=xen&limit=5&link_limit=3") = "page:xen/5/3/false");
  check "a fragment request is marked"
    (body (get "/search?q=xen&fragment=1") = "page:xen/20/12/true");
  check "the search page is HTML"
    (header (get "/search") H.Content_type
     = Some "text/html; charset=utf-8");
  check "the page script is served"
    (contains (body (get "/js/search.js")) "search-results");
  check "a known well-known key is served"
    (body (get "/.well-known/known") = "value");
  check "an unknown one is a 404" (code (get "/.well-known/other") = 404);
  check "robots.txt names the configured sitemap"
    (body (get "/robots.txt")
    = "User-agent: *\nAllow: /\n\nSitemap: https://example.com/sitemap.xml\n")

let () =
  let r = get "/action" in
  check "the dashboard is gated" (code r = 401);
  check "and says how to authenticate"
    (header r H.Www_authenticate = Some "Basic realm=\"stats\"");
  check "a path under the gate that names no route is gated too"
    (code (get "/action/nothing") = 401);
  let auth = [ ("Authorization", "Basic dTpwdw==") ] in
  check "the right password gets in"
    (body (get ~headers:auth "/action") = "report:7d");
  check "the range comes from the query"
    (body (get ~headers:auth "/action?range=30d") = "report:30d");
  check "the JSON views are gated the same way"
    (body (get ~headers:auth "/action/api/overview") = "report:7d")

let () =
  check "every response carries the sniffing guard"
    (header_other (get "/") "X-Content-Type-Options"
    = Some "nosniff");
  check "and the referrer policy"
    (header_other (get "/") "Referrer-Policy"
    = Some "strict-origin-when-cross-origin")

let () =
  (* No test above fetches /links, so this pair sees a miss and then a hit. *)
  let r = get "/links" in
  check "the first render is a cache miss"
    (header r H.X_cache = Some "miss");
  let r' = get "/links" in
  check "the second is a hit"
    (header r' H.X_cache = Some "hit");
  match header r' H.Etag with
  | None -> check "a cached page carries an entity-tag" false
  | Some etag ->
    let r'' = get ~headers:[ ("If-None-Match", etag) ] "/links" in
    check "which answers a conditional request" (code r'' = 304);
    check "with no body" (body r'' = "")

let () =
  let r = Proffer_mock.request site env M.Head "/" in
  check "a HEAD has no body" (body r = "");
  check "but reports the length it would have sent"
    (Proffer_mock.content_length r
    = Some (Int64.of_int (String.length (body (get "/")))));
  check "an unrouted path is a 404" (code (get "/nowhere") = 404);
  Printf.printf "test_routes: %d checks ok\n" !checks
