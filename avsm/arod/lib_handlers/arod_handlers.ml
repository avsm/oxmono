(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Route handlers for arod, described as proffer responses. *)

module Env = Arod_env
module Render = Arod_render
module E = Env
open Proffer

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
let cached env ~key ~content_type gen =
  let hits_before, _ = Cache.stats env.E.cache in
  let body, etag = Cache.memoize env.E.cache ~now:(env.E.now ()) ~key gen in
  let hits_after, _ = Cache.stats env.E.cache in
  let status = if hits_after > hits_before then "hit" else "miss" in
  Resp.media ~etag content_type body |> Resp.add_headers [ ("X-Cache", status) ]

(* The markdown rendering of a page is cached under the HTML page's key with a
   suffix, so a ".md" URL and an Accept of text/markdown share one entry. *)
let md_key key = key ^ ":md"

(** {1 Content pages} *)

let listing_page ~key which =
  Negotiate.v
    [
      ( `Html,
        fun env _req ->
          cached env ~key ~content_type:html_type (fun () ->
              env.E.listing which `Html) );
      ( `Markdown,
        fun env _req ->
          cached env ~key:(md_key key) ~content_type:markdown_type (fun () ->
              env.E.listing which `Markdown) );
    ]

let listing_markdown ~key which env _req =
  cached env ~key:(md_key key) ~content_type:markdown_type (fun () ->
      env.E.listing which `Markdown)

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
let entry_markdown env slug =
  match env.E.entry_markdown slug with
  | Some md -> Resp.media markdown_type md
  | None -> Resp.not_found ()

let entry_page ~prefix kind slug =
  let key = prefix ^ slug in
  Negotiate.v
    [
      ( `Html,
        fun env _req ->
          cached env ~key ~content_type:html_type (fun () ->
              env.E.entry kind slug `Html) );
      ( `Markdown,
        fun env _req ->
          cached env ~key:(md_key key) ~content_type:markdown_type (fun () ->
              env.E.entry kind slug `Markdown) );
    ]

(* One route serves every shape of a paper URL, since the extension is part of
   the slug segment rather than a path of its own. *)
let paper slug env req =
  if String.ends_with ~suffix:".pdf" slug then
    match env.E.read_paper slug with
    | Some body -> Resp.media (Mime.of_path slug) body
    | None -> Resp.not_found ()
  else if String.ends_with ~suffix:".bib" slug then
    match env.E.paper_bib (Filename.chop_extension slug) with
    | Some bib -> Resp.text bib
    | None -> Resp.not_found ()
  else if String.ends_with ~suffix:".md" slug then
    entry_markdown env (Filename.chop_extension slug)
  else entry_page ~prefix:"/papers/" `Paper slug env req

let entry_route ~prefix kind slug env req =
  if String.ends_with ~suffix:".md" slug then
    entry_markdown env (Filename.chop_extension slug)
  else entry_page ~prefix kind slug env req

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
let atom_feed path env _req =
  cached env ~key:("feed:" ^ path) ~content_type:atom_type (fun () ->
      env.E.feed (`Atom path))

let json_feed env _req =
  cached env ~key:"feed:/feed.json" ~content_type:json_type (fun () ->
      env.E.feed `Json)

let perma_atom env _req =
  cached env ~key:"feed:/perma.xml" ~content_type:atom_type (fun () ->
      env.E.feed `Perma_atom)

let perma_json env _req =
  cached env ~key:"feed:/perma.json" ~content_type:json_type (fun () ->
      env.E.feed `Perma_json)

(** {1 Redirect targets} *)

(* A capture arrives decoded, so it may hold a space, a '/', or a CR that
   would make [Resp.redirect] refuse the field. Everything outside the
   unreserved set of RFC 3986 section 2.3 is encoded, which is always valid
   and leaves an ordinary slug untouched. Uri does the same job but carries no
   mode annotations, so a portable handler cannot call it. *)
let encode_segment s =
  let unreserved c =
    (c >= 'A' && c <= 'Z')
    || (c >= 'a' && c <= 'z')
    || (c >= '0' && c <= '9')
    || c = '-' || c = '.' || c = '_' || c = '~'
  in
  let hex n = "0123456789ABCDEF".[n] in
  let b = Buffer.create (String.length s) in
  String.iter
    (fun c ->
      if unreserved c then Buffer.add_char b c
      else (
        Buffer.add_char b '%';
        Buffer.add_char b (hex (Char.code c lsr 4));
        Buffer.add_char b (hex (Char.code c land 0xf))))
    s;
  Buffer.contents b

(** {1 Machine-readable pages} *)

let sitemap env _req = Resp.media "application/xml" (env.E.sitemap ())

let blogroll_opml env _req =
  Resp.media "text/x-opml+xml; charset=utf-8" (env.E.blogroll ())

let robots_txt env _req = Resp.text (env.E.robots ())

let well_known key env _req =
  match env.E.well_known key with
  | Some value -> Resp.text value
  | None -> Resp.not_found ()

(** {1 JSON APIs} *)

let int_param req name ~default ~lo ~hi =
  match Req.query_param req name with
  | None -> default
  | Some v -> (
    match int_of_string_opt v with
    | Some n -> min hi (max lo n)
    | None -> default)

let pagination_api env req =
  let offset =
    match Req.query_param req "offset" with
    | Some o -> ( match int_of_string_opt o with Some n -> max 0 n | None -> 0)
    | None -> 0
  in
  let limit = int_param req "limit" ~default:25 ~lo:1 ~hi:100 in
  let types =
    List.filter_map
      (fun (k, v) -> if String.equal k "type" then Some v else None)
      (Req.query req)
  in
  Resp.media json_type
    (env.E.pagination
       ~collection:(Req.query_param req "collection")
       ~offset ~limit ~types)

let search_api env req =
  let q = match Req.query_param req "q" with Some q -> q | None -> "" in
  let limit = int_param req "limit" ~default:20 ~lo:1 ~hi:100 in
  Resp.media json_type (env.E.search ~q ~limit)

(** {1 Files} *)

let image_file segs env _req =
  match env.E.read_image segs with
  | Some body -> Resp.media (Mime.of_path (String.concat "/" segs)) body
  | None -> Resp.not_found ()

let embedded_file path _env _req =
  match Arod_assets.read path with
  | Some body -> Resp.media (Mime.of_path path) body
  | None -> Resp.not_found ()

let embedded_file_immutable path _env _req =
  match Arod_assets.read path with
  | Some body -> Resp.media ~cache:immutable_cache (Mime.of_path path) body
  | None -> Resp.not_found ()

let js_file name _env _req =
  match List.assoc_opt name Arod_component.Scripts.by_name with
  | Some js -> Resp.media ~cache:immutable_cache "text/javascript" js
  | None -> Resp.not_found ()

(** {1 Stats dashboard} *)

(* Base64 is decoded here rather than through the [base64] library, whose
   interface carries no mode annotations and so cannot be called from the
   portable check {!Proffer.Site.with_auth} takes. Only the standard alphabet
   is accepted, which is all HTTP Basic sends. *)
let base64_decode s =
  let sextet c =
    if c >= 'A' && c <= 'Z' then Char.code c - Char.code 'A'
    else if c >= 'a' && c <= 'z' then Char.code c - Char.code 'a' + 26
    else if c >= '0' && c <= '9' then Char.code c - Char.code '0' + 52
    else if c = '+' then 62
    else if c = '/' then 63
    else -1
  in
  let n = String.length s in
  if n = 0 || n mod 4 <> 0 then None
  else
    let pad =
      if s.[n - 1] <> '=' then 0 else if s.[n - 2] <> '=' then 1 else 2
    in
    let out = Bytes.create ((n / 4 * 3) - pad) in
    let len = Bytes.length out in
    let bad = ref false in
    let o = ref 0 in
    let i = ref 0 in
    while (not !bad) && !i < n do
      let take j =
        let c = s.[!i + j] in
        if c = '=' then (
          (* Padding is only ever the last one or two characters. *)
          if !i + 4 <> n || j < 4 - pad then bad := true;
          0)
        else
          let v = sextet c in
          if v < 0 then (
            bad := true;
            0)
          else v
      in
      let a = take 0 in
      let b = take 1 in
      let c = take 2 in
      let d = take 3 in
      let word = (a lsl 18) lor (b lsl 12) lor (c lsl 6) lor d in
      if !o < len then Bytes.set out !o (Char.chr ((word lsr 16) land 0xff));
      if !o + 1 < len then
        Bytes.set out (!o + 1) (Char.chr ((word lsr 8) land 0xff));
      if !o + 2 < len then Bytes.set out (!o + 2) (Char.chr (word land 0xff));
      o := !o + 3;
      i := !i + 4
    done;
    if !bad then None else Some (Bytes.to_string out)

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
      match base64_decode (String.trim encoded) with
      | None -> false
      | Some decoded -> (
        (* The credentials are "user:password" and only the password is
           checked, since the dashboard has one reader. *)
        match String.index_opt decoded ':' with
        | None -> false
        | Some i ->
          String.equal
            (String.sub decoded (i + 1) (String.length decoded - i - 1))
            password))

let stats_range req =
  match Req.query_param req "range" with Some s -> s | None -> "7d"

let stats_dashboard env req =
  Resp.html (env.E.report `Dashboard ~range:(stats_range req))

let stats_overview env req =
  Resp.media json_type (env.E.report `Overview ~range:(stats_range req))

let stats_traffic env req =
  Resp.media json_type (env.E.report `Traffic ~range:(stats_range req))

let stats_recent env req =
  Resp.media json_type (env.E.report `Recent ~range:(stats_range req))
