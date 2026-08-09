(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Schema.org JSON-LD structured data generation.

    Generates JSON-LD strings for embedding in HTML pages as
    [<script type="application/ld+json">] blocks. Uses Printf.sprintf
    for simple, dependency-free JSON construction. *)

(** {1 Helpers} *)

let encode_json_string s =
  let buf = Buffer.create (String.length s + 8) in
  String.iter (fun c ->
    match c with
    | '"' -> Buffer.add_string buf {|\"|}
    | '\\' -> Buffer.add_string buf {|\\|}
    | '\n' -> Buffer.add_string buf {|\n|}
    | '\r' -> Buffer.add_string buf {|\r|}
    | '\t' -> Buffer.add_string buf {|\t|}
    | '<' -> Buffer.add_string buf {|\u003c|}
    | '>' -> Buffer.add_string buf {|\u003e|}
    | '/' -> Buffer.add_string buf {|\/|}
    | c when Char.code c < 0x20 ->
      Buffer.add_string buf (Printf.sprintf {|\u%04x|} (Char.code c))
    | c -> Buffer.add_char buf c
  ) s;
  Buffer.contents buf

let json_string s = Printf.sprintf {|"%s"|} (encode_json_string s)

let json_array items =
  "[" ^ String.concat ", " items ^ "]"

let ptime_to_iso (y, m, d) =
  Printf.sprintf "%04d-%02d-%02d" y m d

(** {1 WebSite} *)

let website_jsonld ~base_url ~site_name ~description =
  Printf.sprintf
    {|{"@context": "https://schema.org", "@type": "WebSite", "@id": "%s/#website", "url": %s, "name": %s, "description": %s}|}
    (encode_json_string base_url)
    (json_string base_url)
    (json_string site_name)
    (json_string description)

(** {1 Person} *)

let person_jsonld ~ctx =
  let module Contact = Sortal_schema.Contact in
  let config = Arod_ctx.config ctx in
  let base_url = config.site.base_url in
  match Arod_ctx.author ctx with
  | None -> Printf.sprintf
      {|{"@context": "https://schema.org", "@type": "Person", "name": %s, "url": %s}|}
      (json_string config.site.author_name)
      (json_string base_url)
  | Some author ->
    let name = Contact.name author in
    let entries = Arod_ctx.entries ctx in
    let image = match Bushel.Entry.contact_thumbnail entries author with
      | Some t -> Printf.sprintf {|, "image": %s|} (json_string (base_url ^ t))
      | None -> ""
    in
    let job_title, affiliation =
      match Contact.current_affiliation author with
      | Some (a : Contact.affiliation) ->
        let jt = match a.title with
          | Some t -> Printf.sprintf {|, "jobTitle": %s|} (json_string t)
          | None -> ""
        in
        let org_json =
          let org_url = match a.url with
            | Some u -> Printf.sprintf {|, "url": %s|} (json_string u)
            | None -> ""
          in
          Printf.sprintf {|{"@type": "Organization", "name": %s%s}|}
            (json_string a.org) org_url
        in
        let af = Printf.sprintf {|, "affiliation": %s|} org_json in
        (jt, af)
      | None -> ("", "")
    in
    (* Collect sameAs URLs *)
    let same_as_urls = List.filter_map Fun.id [
      Contact.url_on author (Simple Github);
      Contact.url_on author Atproto;
      Option.map Contact.Account.url (Contact.account_on author (Federated Mastodon));
      Option.map Contact.Account.url (Contact.account_on author (Simple LinkedIn));
      Contact.url_on author (Simple Twitter);
      Contact.url_on author (Simple Orcid);
      Option.map Contact.Account.url (Contact.account_on author (Federated PeerTube));
      Option.map Contact.Account.url (Contact.account_on author (Simple Threads));
    ] in
    let same_as_urls =
      let photo_urls =
        List.concat_map (Contact.accounts_on author)
          [ Simple Instagram; Simple Flickr ]
        |> List.map Contact.Account.url
      in
      same_as_urls @ photo_urls
    in
    let same_as = match same_as_urls with
      | [] -> ""
      | urls ->
        Printf.sprintf {|, "sameAs": %s|}
          (json_array (List.map json_string urls))
    in
    let orcid_id = match Contact.url_on author (Simple Orcid) with
      | Some url ->
        Printf.sprintf
          {|, "identifier": {"@type": "PropertyValue", "propertyID": "ORCID", "value": %s}|}
          (json_string url)
      | None -> ""
    in
    Printf.sprintf
      {|{"@context": "https://schema.org", "@type": "Person", "name": %s, "url": %s%s%s%s%s%s}|}
      (json_string name)
      (json_string base_url)
      image job_title affiliation same_as orcid_id

(** {1 ProfilePage} *)

let profile_page_jsonld ~ctx =
  let config = Arod_ctx.config ctx in
  let base_url = config.site.base_url in
  Printf.sprintf
    {|{"@context": "https://schema.org", "@type": "ProfilePage", "mainEntity": %s, "url": %s, "name": %s, "description": %s}|}
    (person_jsonld ~ctx)
    (json_string base_url)
    (json_string config.site.name)
    (json_string config.site.description)

(** {1 CollectionPage} *)

let collection_page_jsonld ~base_url ~url ~title ~description ~count () =
  Printf.sprintf
    {|{"@context": "https://schema.org", "@type": "CollectionPage", "name": %s, "description": %s, "url": %s, "mainEntity": {"@type": "ItemList", "numberOfItems": %d}}|}
    (json_string title)
    (json_string description)
    (json_string (base_url ^ url))
    count

(** {1 Article} *)

let article_jsonld ~base_url ~url ~title ~description ~author_name
    ~date ?modified ?image ?(tags=[]) () =
  let date_str = ptime_to_iso date in
  let modified_str = match modified with
    | Some d -> Printf.sprintf {|, "dateModified": %s|} (json_string (ptime_to_iso d))
    | None -> ""
  in
  let image_str = match image with
    | Some img -> Printf.sprintf {|, "image": %s|} (json_string img)
    | None -> ""
  in
  let tags_str = match tags with
    | [] -> ""
    | ts -> Printf.sprintf {|, "keywords": %s|} (json_array (List.map json_string ts))
  in
  Printf.sprintf
    {|{"@context": "https://schema.org", "@type": "Article", "headline": %s, "description": %s, "author": {"@type": "Person", "name": %s}, "datePublished": %s%s%s, "url": %s%s}|}
    (json_string title)
    (json_string description)
    (json_string author_name)
    (json_string date_str)
    modified_str
    image_str
    (json_string (base_url ^ url))
    tags_str

(** {1 ScholarlyArticle} *)

let scholarly_article_jsonld ~base_url ~url ~title ~description
    ~authors ~date ?doi ?image ?journal ?(tags=[]) () =
  let date_str = ptime_to_iso date in
  let author_objs = List.map (fun name ->
    Printf.sprintf {|{"@type": "Person", "name": %s}|} (json_string name)
  ) authors in
  let doi_str = match doi with
    | Some d ->
      Printf.sprintf {|, "identifier": {"@type": "PropertyValue", "propertyID": "DOI", "value": %s}|}
        (json_string d)
    | None -> ""
  in
  let image_str = match image with
    | Some img -> Printf.sprintf {|, "image": %s|} (json_string img)
    | None -> ""
  in
  let journal_str = match journal with
    | Some j ->
      Printf.sprintf {|, "isPartOf": {"@type": "Periodical", "name": %s}|}
        (json_string j)
    | None -> ""
  in
  let tags_str = match tags with
    | [] -> ""
    | ts -> Printf.sprintf {|, "keywords": %s|} (json_array (List.map json_string ts))
  in
  Printf.sprintf
    {|{"@context": "https://schema.org", "@type": "ScholarlyArticle", "headline": %s, "description": %s, "author": %s, "datePublished": %s%s%s%s%s, "url": %s}|}
    (json_string title)
    (json_string description)
    (json_array author_objs)
    (json_string date_str)
    doi_str image_str journal_str tags_str
    (json_string (base_url ^ url))

(** {1 SoftwareSourceCode (Projects)} *)

let project_jsonld ~base_url ~url ~title ~description ~date_start
    ?date_end ?(tags=[]) () =
  let date_end_str = match date_end with
    | Some y -> Printf.sprintf {|, "dateModified": %s|} (json_string (Printf.sprintf "%d" y))
    | None -> ""
  in
  let tags_str = match tags with
    | [] -> ""
    | ts -> Printf.sprintf {|, "keywords": %s|} (json_array (List.map json_string ts))
  in
  Printf.sprintf
    {|{"@context": "https://schema.org", "@type": "SoftwareSourceCode", "name": %s, "description": %s, "dateCreated": %s%s%s, "url": %s}|}
    (json_string title)
    (json_string description)
    (json_string (Printf.sprintf "%d" date_start))
    date_end_str tags_str
    (json_string (base_url ^ url))

(** {1 VideoObject} *)

let video_jsonld ~base_url ~url ~title ~description ~datetime ?image
    ~embed_url ?is_talk () =
  let date_str = Ptime.to_rfc3339 ~tz_offset_s:0 datetime in
  let description = if description = "" then title else description in
  let image_str = match image with
    | Some img -> Printf.sprintf {|, "thumbnailUrl": %s|} (json_string img)
    | None -> ""
  in
  let full_url = base_url ^ url in
  let url_str =
    if embed_url <> "" then
      Printf.sprintf {|, "embedUrl": %s, "contentUrl": %s|}
        (json_string embed_url) (json_string embed_url)
    else
      Printf.sprintf {|, "contentUrl": %s|} (json_string full_url)
  in
  let genre_str = match is_talk with
    | Some true -> {|, "genre": "Conference talk"|}
    | _ -> ""
  in
  Printf.sprintf
    {|{"@context": "https://schema.org", "@type": "VideoObject", "name": %s, "description": %s, "uploadDate": %s%s%s%s, "url": %s}|}
    (json_string title)
    (json_string description)
    (json_string date_str)
    image_str url_str genre_str
    (json_string full_url)

(** {1 BreadcrumbList} *)

let breadcrumb_jsonld ~base_url items =
  let list_items = List.mapi (fun i (name, item_url) ->
    let full_url = base_url ^ item_url in
    Printf.sprintf
      {|{"@type": "ListItem", "position": %d, "name": %s, "item": %s}|}
      (i + 1) (json_string name) (json_string full_url)
  ) items in
  Printf.sprintf
    {|{"@context": "https://schema.org", "@type": "BreadcrumbList", "itemListElement": %s}|}
    (json_array list_items)
