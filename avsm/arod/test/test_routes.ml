(* The route table is where a literal and a capture at the same depth can
   silently swap places, and where the auth scope and the cache headers are
   decided. It is driven here through the mock backend over a stub
   environment, so no context, database or filesystem is needed. *)

let checks = ref 0

let check name b =
  incr checks;
  if not b then (
    prerr_endline ("FAIL: " ^ name);
    exit 1)

let flavour = function `Html -> "html" | `Markdown -> "md"

let env =
  {
    Arod_handlers.Env.cache = Proffer.Cache.create ~ttl:60.0;
    now = (fun () -> 0.0);
    listing =
      (fun which f ->
        let name =
          match which with
          | `Index -> "index"
          | `Papers -> "papers"
          | `Notes -> "notes"
          | `Ideas -> "ideas"
          | `Projects -> "projects"
          | `Videos -> "videos"
          | `Links -> "links"
          | `Network -> "network"
        in
        name ^ ":" ^ flavour f);
    entry =
      (fun kind slug f ->
        let name =
          match kind with
          | `Paper -> "paper"
          | `Note -> "note"
          | `Idea -> "idea"
          | `Project -> "project"
          | `Video -> "video"
        in
        String.concat ":" [ name; slug; flavour f ]);
    entry_markdown = (fun slug -> if slug = "gone" then None else Some slug);
    paper_bib = (fun slug -> if slug = "gone" then None else Some ("@" ^ slug));
    feed =
      (fun which ->
        match which with
        | `Atom path -> "atom:" ^ path
        | `Json -> "json"
        | `Perma_atom -> "perma-atom"
        | `Perma_json -> "perma-json");
    sitemap = (fun () -> "<urlset/>");
    blogroll = (fun () -> "<opml/>");
    robots = (fun () -> "User-agent: *\n");
    well_known = (fun key -> if key = "known" then Some "value" else None);
    pagination =
      (fun ~collection ~offset ~limit ~types ->
        ignore types;
        Printf.sprintf "%s/%d/%d"
          (Option.value ~default:"none" collection)
          offset limit);
    search = (fun ~q ~limit -> Printf.sprintf "%s/%d" q limit);
    read_image =
      (fun segs ->
        match Proffer.Static.confine segs with
        | Some path -> Some ("image:" ^ path)
        | None -> None);
    read_paper = (fun name -> if name = "gone.pdf" then None else Some name);
    report = (fun _which ~range -> "report:" ^ range);
  }

let cfg : Arod.Config.t =
  {
    Arod.Config.default with
    server = { Arod.Config.default.server with stats_password = Some "pw" };
  }

let site = Arod_server.Site.build cfg
let get ?headers target = Proffer_mock.request ?headers site env `GET target
let code r = Proffer.Status.code (Proffer_mock.status r)
let body = Proffer_mock.body
let header = Proffer_mock.header

let () =
  check "the front page is served" (body (get "/") = "index:html");
  check "so is /about" (body (get "/about") = "index:html");
  check "a markdown Accept picks the markdown variant"
    (body (get ~headers:[ ("Accept", "text/markdown") ] "/") = "index:md");
  check "a negotiated response varies on Accept"
    (header (get "/") "Vary" = Some "Accept");
  check "a .md URL is the markdown variant"
    (body (get "/papers.md") = "papers:md")

let () =
  check "a feed path is not swallowed by the note route"
    (body (get "/notes/atom.xml") = "atom:/notes/atom.xml");
  check "and neither is the JSON feed"
    (body (get "/notes/feed.json") = "json");
  check "a note slug still reaches the note route"
    (body (get "/notes/hello") = "note:hello:html");
  check "the Atom feed names the path it was fetched from"
    (body (get "/news.xml") = "atom:/news.xml")

let () =
  let r = get "/atom.xml" in
  check "a retired feed path is a permanent redirect" (code r = 301);
  check "and it points at the current one"
    (header r "Location" = Some "/news.xml");
  let r = get "/notes/index.html" in
  check "a list index.html redirects to the list" (code r = 301);
  check "at the canonical path" (header r "Location" = Some "/notes");
  let r = get "/notes/hello/index.html" in
  check "an entry index.html redirects to the entry" (code r = 301);
  check "carrying the slug" (header r "Location" = Some "/notes/hello");
  let r = get "/tags/ocaml" in
  check "a tag link is a temporary redirect" (code r = 302);
  check "into the front page's tag filter"
    (header r "Location" = Some "/#tag=ocaml");
  let r = get "/news/hello" in
  check "a legacy news URL redirects to the note" (code r = 301);
  check "keeping the slug" (header r "Location" = Some "/notes/hello")

let () =
  (* A capture arrives decoded, so what goes back into a Location has to be
     encoded again. A space would otherwise redirect to the wrong place, and a
     CR would make Resp.redirect refuse the field and answer 500. *)
  let r = get "/news/foo%20bar" in
  check "a space in a capture is encoded back into the Location"
    (header r "Location" = Some "/notes/foo%20bar");
  check "and the redirect is still a redirect" (code r = 301);
  let r = get "/news/a%0Db" in
  check "a CR in a capture does not become a 500" (code r = 301);
  check "it is encoded instead" (header r "Location" = Some "/notes/a%0Db");
  let r = get "/notes/foo%20bar/index.html" in
  check "an entry index.html encodes its slug too"
    (header r "Location" = Some "/notes/foo%20bar");
  let r = get "/tags/two%20words" in
  check "so does the tag redirect"
    (header r "Location" = Some "/#tag=two%20words");
  check "an ordinary slug is left alone"
    (header (get "/news/hello-there") "Location" = Some "/notes/hello-there")

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
    (header (get "/papers/x.pdf") "Content-Type" = Some "application/pdf");
  check "a missing PDF is a 404" (code (get "/papers/gone.pdf") = 404);
  check "a .bib URL is the BibTeX entry" (body (get "/papers/x.bib") = "@x");
  check "an entry .md URL is that entry as markdown"
    (body (get "/papers/x.md") = "x");
  check "and an unknown one is a 404" (code (get "/papers/gone.md") = 404)

let () =
  check "an image is read through the confined directory"
    (body (get "/images/a/b.png") = "image:a/b.png");
  check "a traversal in the tail is refused"
    (code (get "/images/../etc/passwd") = 404);
  check "an embedded asset is served"
    (header (get "/favicon.svg") "Content-Type" = Some "image/svg+xml");
  check "a versioned asset may be cached for ever"
    (header (get "/tw.css") "Cache-Control"
    = Some "public, max-age=31536000, immutable")

let () =
  check "the pagination API reads its query"
    (body (get "/api/entries?collection=links&offset=5&limit=10")
    = "links/5/10");
  check "an out of range limit is clamped"
    (body (get "/api/entries?collection=links&limit=9999") = "links/0/100");
  check "the search API reads its query"
    (body (get "/api/search?q=ocaml&limit=3") = "ocaml/3");
  check "a known well-known key is served"
    (body (get "/.well-known/known") = "value");
  check "an unknown one is a 404" (code (get "/.well-known/other") = 404)

let () =
  let r = get "/action" in
  check "the dashboard is gated" (code r = 401);
  check "and says how to authenticate"
    (header r "WWW-Authenticate" = Some "Basic realm=\"stats\"");
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
    (header (get "/") "X-Content-Type-Options" = Some "nosniff");
  check "and the referrer policy"
    (header (get "/") "Referrer-Policy" = Some "strict-origin-when-cross-origin")

let () =
  (* No test above fetches /links, so this pair sees a miss and then a hit. *)
  let r = get "/links" in
  check "the first render is a cache miss" (header r "X-Cache" = Some "miss");
  let r' = get "/links" in
  check "the second is a hit" (header r' "X-Cache" = Some "hit");
  match header r' "ETag" with
  | None -> check "a cached page carries an entity-tag" false
  | Some etag ->
    let r'' = get ~headers:[ ("If-None-Match", etag) ] "/links" in
    check "which answers a conditional request" (code r'' = 304);
    check "with no body" (body r'' = "")

let () =
  let r = Proffer_mock.request site env `HEAD "/" in
  check "a HEAD has no body" (body r = "");
  check "but reports the length it would have sent"
    (Proffer_mock.content_length r = Some 10L);
  check "an unrouted path is a 404" (code (get "/nowhere") = 404);
  Printf.printf "test_routes: %d checks ok\n" !checks
