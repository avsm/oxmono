(*---------------------------------------------------------------------------
   Copyright (c) 2024 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

module Unknown = struct
  type t = Jsont.json

  let empty = Jsont.Object ([], Jsont.Meta.none)
  let is_empty = function Jsont.Object ([], _) -> true | _ -> false
end

type content = [ `Html of string | `Text of string | `Both of string * string ]

type t = {
  id : string;
  content : content;
  url : string option;
  external_url : string option;
  title : string option;
  summary : string option;
  image : string option;
  banner_image : string option;
  date_published : Ptime.t option;
  date_modified : Ptime.t option;
  authors : Author.t list option;
  tags : string list option;
  language : string option;
  attachments : Attachment.t list option;
  references : Reference.t list option;
  unknown : Unknown.t;
}

let create ~id ~content ?url ?external_url ?title ?summary ?image ?banner_image
    ?date_published ?date_modified ?authors ?tags ?language ?attachments
    ?references ?(unknown = Unknown.empty) () =
  {
    id;
    content;
    url;
    external_url;
    title;
    summary;
    image;
    banner_image;
    date_published;
    date_modified;
    authors;
    tags;
    language;
    attachments;
    references;
    unknown;
  }

let id t = t.id
let content t = t.content
let url t = t.url
let external_url t = t.external_url
let title t = t.title
let summary t = t.summary
let image t = t.image
let banner_image t = t.banner_image
let date_published t = t.date_published
let date_modified t = t.date_modified
let authors t = t.authors
let tags t = t.tags
let language t = t.language
let attachments t = t.attachments
let references t = t.references
let unknown t = t.unknown

let content_html t =
  match t.content with
  | `Html html -> Some html
  | `Text _ -> None
  | `Both (html, _) -> Some html

let content_text t =
  match t.content with
  | `Html _ -> None
  | `Text text -> Some text
  | `Both (_, text) -> Some text

let equal a b = a.id = b.id
let compare a b = Option.compare Ptime.compare a.date_published b.date_published

let pp ppf t =
  match (t.date_published, t.title) with
  | Some date, Some title ->
      let (y, m, d), _ = Ptime.to_date_time date in
      Format.fprintf ppf "[%04d-%02d-%02d] %s (%s)" y m d title t.id
  | Some date, None ->
      let (y, m, d), _ = Ptime.to_date_time date in
      Format.fprintf ppf "[%04d-%02d-%02d] %s" y m d t.id
  | None, Some title -> Format.fprintf ppf "%s (%s)" title t.id
  | None, None -> Format.fprintf ppf "%s" t.id

let pp_summary ppf t =
  Format.fprintf ppf "%s" (Option.value ~default:t.id t.title)

(* Jsont type *)

let jsont =
  let kind = "Item" in
  let doc = "A JSON Feed item" in

  (* Helper to construct item from JSON fields *)
  let make_from_json id content_html content_text url external_url title summary
      image banner_image date_published date_modified authors tags language
      attachments references _extensions unknown =
    (* Determine content from content_html and content_text *)
    let content =
      match (content_html, content_text) with
      | Some html, Some text -> `Both (html, text)
      | Some html, None -> `Html html
      | None, Some text -> `Text text
      | None, None ->
          Jsont.Error.msg Jsont.Meta.none
            "Item must have at least one of content_html or content_text"
    in
    {
      id;
      content;
      url;
      external_url;
      title;
      summary;
      image;
      banner_image;
      date_published;
      date_modified;
      authors;
      tags;
      language;
      attachments;
      references;
      unknown;
    }
  in

  Jsont.Object.map ~kind ~doc make_from_json
  |> Jsont.Object.mem "id" Jsont.string ~enc:id
  |> Jsont.Object.opt_mem "content_html" Jsont.string ~enc:content_html
  |> Jsont.Object.opt_mem "content_text" Jsont.string ~enc:content_text
  |> Jsont.Object.opt_mem "url" Jsont.string ~enc:url
  |> Jsont.Object.opt_mem "external_url" Jsont.string ~enc:external_url
  |> Jsont.Object.opt_mem "title" Jsont.string ~enc:title
  |> Jsont.Object.opt_mem "summary" Jsont.string ~enc:summary
  |> Jsont.Object.opt_mem "image" Jsont.string ~enc:image
  |> Jsont.Object.opt_mem "banner_image" Jsont.string ~enc:banner_image
  |> Jsont.Object.opt_mem "date_published" Rfc3339.jsont ~enc:date_published
  |> Jsont.Object.opt_mem "date_modified" Rfc3339.jsont ~enc:date_modified
  |> Jsont.Object.opt_mem "authors" (Jsont.list Author.jsont) ~enc:authors
  |> Jsont.Object.opt_mem "tags" (Jsont.list Jsont.string) ~enc:tags
  |> Jsont.Object.opt_mem "language" Jsont.string ~enc:language
  |> Jsont.Object.opt_mem "attachments"
       (Jsont.list Attachment.jsont)
       ~enc:attachments
  |> Jsont.Object.opt_mem "_references"
       (Jsont.list Reference.jsont)
       ~enc:references
  |> Jsont.Object.opt_mem "_extensions" Jsont.json_object ~enc:(fun _t -> None)
  |> Jsont.Object.keep_unknown Jsont.json_mems ~enc:unknown
  |> Jsont.Object.finish
