(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Route handlers for arod, described as proffer responses. *)

module Env = Arod_env
module Render = Arod_render
module E = Env
open Proffer
module H = Httpz.Header_name

type handler = Env.t Proffer.Route.handler

(** {1 Media types} *)

let html_type = "text/html; charset=utf-8"
let markdown_type = "text/markdown; charset=utf-8"
let atom_type = "application/atom+xml; charset=utf-8"
let json_type = "application/json; charset=utf-8"

(* A versioned asset is fetched with a content hash in its query string, so a
   change to the file changes the URL and the old response may be kept for
   ever. *)
let immutable_cache =
  Cache_control.public ~max_age:(`Days 365) ~immutable:true ()

(** {1 Cached renders} *)

(* [Cache.memoize] does not report whether it hit, so the hit is read from the
   cache's counters either side of the call. Arod serves from a single domain,
   where that delta is exact. It is approximate under concurrent domains,
   since another request may bump the same counter in between. The field is
   kept because the stats dashboard breaks traffic down by it. *)
let cached (respond : Resp.respond @ local) env ~key ~content_type gen =
  let hits_before, _ = Cache.stats env.E.cache in
  let body, etag = Cache.memoize env.E.cache ~now:(env.E.now ()) ~key gen in
  let hits_after, _ = Cache.stats env.E.cache in
  let status = if hits_after > hits_before then "hit" else "miss" in
  (* Through [Resp.v] with [stack_] rather than [Resp.media ~headers], because
     this is every cached page. An optional argument is passed as an allocated
     [Some] that the block cannot cross, so [~headers] on the sugar
     constructors puts the field on the heap; and a local argument cannot be
     passed in a tail call, which is what the [let () = ... in ()] is for.
     Neither is obvious from the call site, so both are written out here. *)
  let () =
    Resp.v respond ~etag
      ~headers:(stack_ [ Resp.h_local H.X_cache status ])
      ~content_type:(This content_type) (Body.String body)
  in
  ()

(* The markdown rendering of a page is cached under the HTML page's key with a
   suffix, so a ".md" URL and an Accept of text/markdown share one entry. *)
let md_key key = key ^ ":md"

(** {1 Content pages} *)

let listing_page ~key which =
  Negotiate.v
    [
      ( `Html,
        fun env _req respond ->
          cached respond env ~key ~content_type:html_type (fun () ->
              Render.listing ~ctx:env.E.ctx which `Html) );
      ( `Markdown,
        fun env _req respond ->
          cached respond env ~key:(md_key key) ~content_type:markdown_type
            (fun () -> Render.listing ~ctx:env.E.ctx which `Markdown) );
    ]

let listing_markdown ~key which env _req respond =
  cached respond env ~key:(md_key key) ~content_type:markdown_type (fun () ->
      Render.listing ~ctx:env.E.ctx which `Markdown)

let index = listing_page ~key:"/" `Index
let papers_list = listing_page ~key:"/papers" `Papers
let notes_list = listing_page ~key:"/notes" `Notes
let ideas_list = listing_page ~key:"/ideas" `Ideas
let projects_list = listing_page ~key:"/projects" `Projects
let videos_list = listing_page ~key:"/videos" `Videos
let links_list = listing_page ~key:"/links" `Links
let network_page = listing_page ~key:"/network" `Network
let index_markdown = listing_markdown ~key:"/" `Index
let papers_markdown = listing_markdown ~key:"/papers" `Papers
let notes_markdown = listing_markdown ~key:"/notes" `Notes
let ideas_markdown = listing_markdown ~key:"/ideas" `Ideas
let projects_markdown = listing_markdown ~key:"/projects" `Projects
let videos_markdown = listing_markdown ~key:"/videos" `Videos
let links_markdown = listing_markdown ~key:"/links" `Links
let network_markdown = listing_markdown ~key:"/network" `Network

(** {1 Entry pages} *)

(* A ".md" suffix asks for one entry as markdown. It is not negotiated, so it
   is not cached: an entry render is cheap next to a list page. *)
let entry_markdown (respond : Resp.respond @ local) env slug =
  match Render.entry_markdown ~ctx:env.E.ctx slug with
  | Some md -> Resp.media respond markdown_type md
  | None -> Resp.not_found respond ()

let entry_page ~prefix kind slug =
  let key = prefix ^ slug in
  Negotiate.v
    [
      ( `Html,
        fun env _req respond ->
          cached respond env ~key ~content_type:html_type (fun () ->
              Render.entry ~ctx:env.E.ctx kind slug `Html) );
      ( `Markdown,
        fun env _req respond ->
          cached respond env ~key:(md_key key) ~content_type:markdown_type
            (fun () -> Render.entry ~ctx:env.E.ctx kind slug `Markdown) );
    ]

(* One route serves every shape of a paper URL, since the extension is part of
   the slug segment rather than a path of its own. *)
let paper slug env req respond =
  if String.ends_with ~suffix:".pdf" slug then
    match env.E.read_paper slug with
    | Some body -> Resp.media respond (Mime.of_path slug) body
    | None -> Resp.not_found respond ()
  else if String.ends_with ~suffix:".bib" slug then
    match Render.paper_bib ~ctx:env.E.ctx (Filename.chop_extension slug) with
    | Some bib -> Resp.text respond bib
    | None -> Resp.not_found respond ()
  else if String.ends_with ~suffix:".md" slug then
    entry_markdown respond env (Filename.chop_extension slug)
  else entry_page ~prefix:"/papers/" `Paper slug env req respond

let entry_route ~prefix kind slug env req respond =
  if String.ends_with ~suffix:".md" slug then
    entry_markdown respond env (Filename.chop_extension slug)
  else entry_page ~prefix kind slug env req respond

let note slug = entry_route ~prefix:"/notes/" `Note slug
let idea slug = entry_route ~prefix:"/ideas/" `Idea slug
let project slug = entry_route ~prefix:"/projects/" `Project slug
let video slug = entry_route ~prefix:"/videos/" `Video slug

(** {1 Feeds} *)

(* Two paths serve the Atom feed and each names itself in the entries it
   writes, so the path is an argument. It comes from the route table and not
   from the request: a request target percent-encodes as it likes, the router
   matches on decoded segments, and a key taken from the target would let a
   client mint cache entries the cache never evicts. *)
let atom_feed path env _req respond =
  cached respond env ~key:("feed:" ^ path) ~content_type:atom_type (fun () ->
      env.E.feed (`Atom path))

let json_feed env _req respond =
  cached respond env ~key:"feed:/feed.json" ~content_type:json_type (fun () ->
      env.E.feed `Json)

let perma_atom env _req respond =
  cached respond env ~key:"feed:/perma.xml" ~content_type:atom_type (fun () ->
      env.E.feed `Perma_atom)

let perma_json env _req respond =
  cached respond env ~key:"feed:/perma.json" ~content_type:json_type (fun () ->
      env.E.feed `Perma_json)

(** {1 Redirect targets} *)

(* A capture arrives decoded, so it may hold a space, a '/', or a CR that
   would make [Resp.redirect] refuse the field. [`Unreserved] escapes every
   byte outside the unreserved set of RFC 3986 section 2.3, sub-delimiters
   included, which is stricter than [`Segment] and is what the redirect tests
   pin. Uriz is the vendored encoder, whose interface is mode-annotated, so a
   portable handler can call it. *)
let encode_segment s = Uriz.pct_encode ~component:`Unreserved s

(** {1 Machine-readable pages} *)

let sitemap env _req respond =
  Resp.media respond "application/xml" (Render.sitemap ~ctx:env.E.ctx)

let blogroll_opml env _req respond =
  Resp.media respond "text/x-opml+xml; charset=utf-8"
    (Render.blogroll ~ctx:env.E.ctx)

let robots_txt env _req respond =
  Resp.text respond
    (Printf.sprintf "User-agent: *\nAllow: /\n\nSitemap: %s/sitemap.xml\n"
       env.E.config.site.base_url)

let llms_txt env _req respond =
  Resp.media respond markdown_type (Render.llms_txt ~ctx:env.E.ctx)

let well_known key env _req respond =
  match
    List.find_opt
      (fun e -> String.equal e.Arod.Config.key key)
      env.E.config.well_known
  with
  | Some entry -> Resp.text respond entry.value
  | None -> Resp.not_found respond ()

(** {1 JSON APIs} *)

let int_param req name ~default ~lo ~hi =
  match Req.query_param req name with
  | None -> default
  | Some v -> (
    match int_of_string_opt v with
    | Some n -> min hi (max lo n)
    | None -> default)

let pagination_api env req respond =
  let offset =
    match Req.query_param req "offset" with
    | Some o -> ( match int_of_string_opt o with Some n -> max 0 n | None -> 0)
    | None -> 0
  in
  let limit = int_param req "limit" ~default:25 ~lo:1 ~hi:100 in
  let types =
    List.filter_map
      (fun (k, v) ->
        if String.equal k "type" then Some (Req.globalize v) else None)
      (Req.query req)
  in
  let collection =
    match Req.query_param req "collection" with
    | None -> None
    | Some collection -> Some (Req.globalize collection)
  in
  Resp.stream respond json_type
    (env.E.pagination
       ~collection
       ~offset ~limit ~types)

let sort_param req =
  match Req.query_param req "sort" with
  | Some "date" -> `Date
  | _ -> `Relevance

(* [search_api] and [search_page] read the same three parameters. Pure
   [Req] reads, so this stays portable. *)
let search_params req =
  let q =
    match Req.query_param req "q" with
    | Some q -> Req.globalize q
    | None -> ""
  in
  let limit = int_param req "limit" ~default:20 ~lo:1 ~hi:100 in
  let link_limit = int_param req "link_limit" ~default:12 ~lo:1 ~hi:100 in
  (q, limit, link_limit, sort_param req)

let search_api env req respond =
  let q, limit, link_limit, order = search_params req in
  env.E.log_search ~query:q ~limit ~results:None;
  let write, results = env.E.search ~q ~limit ~link_limit ~order in
  env.E.log_search ~query:q ~limit ~results:(Some results);
  Resp.stream respond json_type write

let search_page env req respond =
  let q, limit, link_limit, order = search_params req in
  let fragment = Req.query_param req "fragment" = Some "1" in
  Resp.html respond
    (env.E.search_page ~q ~limit ~link_limit ~order ~fragment)

(** {1 Files} *)

let image_file (segs @ local) env _req respond =
  let rec globalize (segs @ local) =
    match segs with
    | [] -> []
    | segment :: rest -> Req.globalize segment :: globalize rest
  in
  let segs = globalize segs in
  match env.E.read_image segs with
  | Some body ->
      Resp.media respond (Mime.of_path (String.concat "/" segs)) body
  | None -> Resp.not_found respond ()

let embedded_file path _env _req respond =
  match Arod_assets.read path with
  | Some body -> Resp.media respond (Mime.of_path path) body
  | None -> Resp.not_found respond ()

let embedded_file_immutable path _env _req respond =
  match Arod_assets.read path with
  | Some body ->
      Resp.media respond ~cache:immutable_cache (Mime.of_path path) body
  | None -> Resp.not_found respond ()

let js_file name _env _req respond =
  match List.assoc_opt name Arod_component.Scripts.by_name with
  | Some js ->
      Resp.media respond ~cache:immutable_cache "text/javascript" js
  | None -> Resp.not_found respond ()

(** {1 Stats dashboard} *)

let stats_auth ~password auth =
  match auth with
  | None -> false
  | Some header -> (
    let prefix = "Basic " in
    if not (String.starts_with ~prefix header) then false
    else
      let encoded =
        String.sub header (String.length prefix)
          (String.length header - String.length prefix)
      in
      (* RFC 7235 allows whitespace after the scheme token but not after the
         credentials. Trimming both sides is deliberately more lenient than
         that, since a trailing space cannot make one credential read as
         another. *)
      match Base64.decode (String.trim encoded) with
      | Error (`Msg _) -> false
      | Ok decoded -> (
        (* The credentials are "user:password" and only the password is
           checked, since the dashboard has one reader. *)
        match String.index_opt decoded ':' with
        | None -> false
        | Some i ->
          String.equal
            (String.sub decoded (i + 1) (String.length decoded - i - 1))
            password))

let stats_range req =
  match Req.query_param req "range" with
  | Some range -> Req.globalize range
  | None -> "7d"

let stats_dashboard env req respond =
  Resp.html respond (env.E.report `Dashboard ~range:(stats_range req))

let stats_overview env req respond =
  Resp.media respond json_type (env.E.report `Overview ~range:(stats_range req))

let stats_traffic env req respond =
  Resp.media respond json_type (env.E.report `Traffic ~range:(stats_range req))

let stats_recent env req respond =
  Resp.media respond json_type (env.E.report `Recent ~range:(stats_range req))
