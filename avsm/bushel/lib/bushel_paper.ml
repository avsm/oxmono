(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Paper entry type for Bushel *)

(** Classification of paper type *)
type classification = Full | Short | Preprint

let string_of_classification = function
  | Full -> "full"
  | Short -> "short"
  | Preprint -> "preprint"

let classification_of_string = function
  | "full" -> Full
  | "short" -> Short
  | "preprint" -> Preprint
  | _ -> Full

type t = {
  slug : string;
  ver : string;
  title : string;
  authors : string list;
  year : int;
  month : int;
  bibtype : string;
  publisher : string;
  booktitle : string;
  journal : string;
  institution : string;
  pages : string;
  volume : string option;
  number : string option;
  doi : string option;
  url : string option;
  video : string option;
  isbn : string;
  editor : string;
  bib : string;
  tags : string list;
  projects : string list;
  slides : string list;
  abstract : string;
  latest : bool;
  selected : bool;
  classification : classification option;
  note : string option;
}

type ts = t list

(** {1 Accessors} *)

let slug { slug; _ } = slug
let title { title; _ } = title
let authors { authors; _ } = authors
let year { year; _ } = year
let bibtype { bibtype; _ } = bibtype
let publisher { publisher; _ } = publisher
let booktitle { booktitle; _ } = booktitle
let journal { journal; _ } = journal
let institution { institution; _ } = institution
let pages { pages; _ } = pages
let volume { volume; _ } = volume
let number { number; _ } = number
let doi { doi; _ } = doi
let url { url; _ } = url
let video { video; _ } = video
let isbn { isbn; _ } = isbn
let editor { editor; _ } = editor
let bib { bib; _ } = bib
let tags { tags; _ } = tags
let project_slugs { projects; _ } = projects
let slides { slides; _ } = slides
let abstract { abstract; _ } = abstract
let selected { selected; _ } = selected
let note { note; _ } = note
let classification { classification; bibtype; journal; booktitle; title; _ } =
  match classification with
  | Some c -> c
  | None ->
    (* Heuristic classification based on metadata *)
    let bibtype_lower = String.lowercase_ascii bibtype in
    let journal_lower = String.lowercase_ascii journal in
    let booktitle_lower = String.lowercase_ascii booktitle in
    let title_lower = String.lowercase_ascii title in
    let contains_any text patterns =
      List.exists (fun p ->
        try
          let re = Re.Perl.compile_pat ~opts:[`Caseless] p in
          Re.execp re text
        with _ -> false
      ) patterns
    in
    if contains_any journal_lower ["arxiv"] ||
       contains_any booktitle_lower ["arxiv"] ||
       bibtype_lower = "misc" || bibtype_lower = "techreport"
    then Preprint
    else if contains_any journal_lower ["workshop"; "wip"; "poster"; "demo"; "hotdep"; "short"] ||
            contains_any booktitle_lower ["workshop"; "wip"; "poster"; "demo"; "hotdep"; "short"] ||
            contains_any title_lower ["poster"]
    then Short
    else Full

let date { year; month; _ } = (year, month, 1)
let datetime p = Bushel_types.ptime_of_date_exn (date p)

(** {1 Comparison} *)

let compare p2 p1 =
  let d1 = try datetime p1 with _ -> Bushel_types.ptime_of_date_exn (1977, 1, 1) in
  let d2 = try datetime p2 with _ -> Bushel_types.ptime_of_date_exn (1977, 1, 1) in
  Ptime.compare d1 d2

(** {1 Lookup} *)

let slugs ts =
  List.fold_left (fun acc t -> if List.mem t.slug acc then acc else t.slug :: acc) [] ts

let lookup ts slug = List.find_opt (fun t -> t.slug = slug && t.latest) ts

let get_papers ~slug ts =
  List.filter (fun p -> p.slug = slug && p.latest <> true) ts |> List.sort compare

(** Convert bibtype to tag *)
let tag_of_bibtype bt =
  match String.lowercase_ascii bt with
  | "article" -> "journal"
  | "inproceedings" -> "conference"
  | "techreport" -> "report"
  | "misc" -> "preprint"
  | "book" -> "book"
  | x -> x

(** Compute version tracking *)
let tv (l : t list) =
  let h = Hashtbl.create 7 in
  List.iter (fun { slug; ver; _ } ->
    match Hashtbl.find_opt h slug with
    | None -> Hashtbl.add h slug [ ver ]
    | Some l ->
      let l = ver :: l in
      let l = List.sort String.compare l in
      Hashtbl.replace h slug l
  ) l;
  List.map (fun p ->
    let latest = Hashtbl.find h p.slug |> List.rev |> List.hd in
    let latest = p.ver = latest in
    { p with latest }
  ) l

let best_url p = url p

(** {1 Jsont Codec} *)

let month_of_string s =
  match String.lowercase_ascii s with
  | "jan" -> 1 | "feb" -> 2 | "mar" -> 3 | "apr" -> 4
  | "may" -> 5 | "jun" -> 6 | "jul" -> 7 | "aug" -> 8
  | "sep" -> 9 | "oct" -> 10 | "nov" -> 11 | "dec" -> 12
  | _ -> 1

let jsont : t Jsont.t =
  let open Jsont in
  let open Jsont.Object in
  let make title authors year month bibtype publisher booktitle journal institution
           pages volume number doi url video isbn editor bib tags projects slides
           selected classification note =
    { slug = ""; ver = ""; title; authors; year; month; bibtype; publisher; booktitle;
      journal; institution; pages; volume; number; doi; url; video; isbn; editor; bib;
      tags; projects; slides; abstract = ""; latest = false; selected;
      classification; note }
  in
  map ~kind:"Paper" make
  |> mem "title" string ~enc:(fun p -> p.title)
  |> mem "author" (list string) ~dec_absent:[] ~enc:(fun p -> p.authors)
  |> mem "year" (of_of_string ~kind:"year" (fun s -> Ok (int_of_string s)) ~enc:string_of_int)
       ~enc:(fun p -> p.year)
  |> mem "month" (of_of_string ~kind:"month" (fun s -> Ok (month_of_string s)) ~enc:(fun m ->
       match m with 1 -> "jan" | 2 -> "feb" | 3 -> "mar" | 4 -> "apr"
       | 5 -> "may" | 6 -> "jun" | 7 -> "jul" | 8 -> "aug"
       | 9 -> "sep" | 10 -> "oct" | 11 -> "nov" | 12 -> "dec" | _ -> "jan"))
       ~dec_absent:1 ~enc:(fun p -> p.month)
  |> mem "bibtype" string ~enc:(fun p -> p.bibtype)
  |> mem "publisher" string ~dec_absent:"" ~enc:(fun p -> p.publisher)
  |> mem "booktitle" string ~dec_absent:"" ~enc:(fun p -> p.booktitle)
  |> mem "journal" string ~dec_absent:"" ~enc:(fun p -> p.journal)
  |> mem "institution" string ~dec_absent:"" ~enc:(fun p -> p.institution)
  |> mem "pages" string ~dec_absent:"" ~enc:(fun p -> p.pages)
  |> mem "volume" Bushel_types.string_option_jsont ~dec_absent:None
       ~enc_omit:Option.is_none ~enc:(fun p -> p.volume)
  |> mem "number" Bushel_types.string_option_jsont ~dec_absent:None
       ~enc_omit:Option.is_none ~enc:(fun p -> p.number)
  |> mem "doi" Bushel_types.string_option_jsont ~dec_absent:None
       ~enc_omit:Option.is_none ~enc:(fun p -> p.doi)
  |> mem "url" Bushel_types.string_option_jsont ~dec_absent:None
       ~enc_omit:Option.is_none ~enc:(fun p -> p.url)
  |> mem "video" Bushel_types.string_option_jsont ~dec_absent:None
       ~enc_omit:Option.is_none ~enc:(fun p -> p.video)
  |> mem "isbn" string ~dec_absent:"" ~enc:(fun p -> p.isbn)
  |> mem "editor" string ~dec_absent:"" ~enc:(fun p -> p.editor)
  |> mem "bib" string ~dec_absent:"" ~enc:(fun p -> p.bib)
  |> mem "tags" (list string) ~dec_absent:[] ~enc:(fun p -> p.tags)
  |> mem "projects" (list string) ~dec_absent:[] ~enc:(fun p -> p.projects)
  |> mem "slides" (list string) ~dec_absent:[] ~enc:(fun p -> p.slides)
  |> mem "selected" bool ~dec_absent:false ~enc:(fun p -> p.selected)
  |> mem "classification" (option (of_of_string ~kind:"classification"
       (fun s -> Ok (classification_of_string s)) ~enc:string_of_classification))
       ~dec_absent:None ~enc_omit:Option.is_none ~enc:(fun p -> p.classification)
  |> mem "note" Bushel_types.string_option_jsont ~dec_absent:None
       ~enc_omit:Option.is_none ~enc:(fun p -> p.note)
  |> finish

(** {1 Parsing} *)

let of_frontmatter ~slug ~ver (fm : Frontmatter.t) : (t, string) result =
  match Frontmatter.decode jsont fm with
  | Error e -> Error e
  | Ok p ->
    (* Compute full tags including bibtype and projects *)
    let keywords = Frontmatter.find_strings "keywords" fm in
    let all_tags =
      List.flatten [p.tags; keywords; [tag_of_bibtype p.bibtype]; p.projects]
    in
    Ok { p with
         slug;
         ver;
         abstract = Frontmatter.body fm;
         tags = all_tags }

(** {1 Pretty Printing} *)

let pp ppf p =
  let open Fmt in
  pf ppf "@[<v>";
  pf ppf "%a: %a@," (styled `Bold string) "Type" (styled `Cyan string) "Paper";
  pf ppf "%a: %a@," (styled `Bold string) "Slug" string (slug p);
  pf ppf "%a: %a@," (styled `Bold string) "Version" string p.ver;
  pf ppf "%a: %a@," (styled `Bold string) "Title" string (title p);
  pf ppf "%a: @[<h>%a@]@," (styled `Bold string) "Authors" (list ~sep:comma string) (authors p);
  pf ppf "%a: %a@," (styled `Bold string) "Year" int (year p);
  pf ppf "%a: %a@," (styled `Bold string) "Bibtype" string (bibtype p);
  (match doi p with
   | Some d -> pf ppf "%a: %a@," (styled `Bold string) "DOI" string d
   | None -> ());
  (match url p with
   | Some u -> pf ppf "%a: %a@," (styled `Bold string) "URL" string u
   | None -> ());
  (match video p with
   | Some v -> pf ppf "%a: %a@," (styled `Bold string) "Video" string v
   | None -> ());
  let projs = project_slugs p in
  if projs <> [] then
    pf ppf "%a: @[<h>%a@]@," (styled `Bold string) "Projects" (list ~sep:comma string) projs;
  let sl = slides p in
  if sl <> [] then
    pf ppf "%a: @[<h>%a@]@," (styled `Bold string) "Slides" (list ~sep:comma string) sl;
  (match bibtype p with
   | "article" ->
     pf ppf "%a: %a@," (styled `Bold string) "Journal" string (journal p);
     (match volume p with
      | Some vol -> pf ppf "%a: %a@," (styled `Bold string) "Volume" string vol
      | None -> ());
     (match number p with
      | Some iss -> pf ppf "%a: %a@," (styled `Bold string) "Issue" string iss
      | None -> ());
     let pgs = pages p in
     if pgs <> "" then
       pf ppf "%a: %a@," (styled `Bold string) "Pages" string pgs;
   | "inproceedings" ->
     pf ppf "%a: %a@," (styled `Bold string) "Booktitle" string (booktitle p);
     let pgs = pages p in
     if pgs <> "" then
       pf ppf "%a: %a@," (styled `Bold string) "Pages" string pgs;
   | "techreport" ->
     pf ppf "%a: %a@," (styled `Bold string) "Institution" string (institution p);
     (match number p with
      | Some num -> pf ppf "%a: %a@," (styled `Bold string) "Number" string num
      | None -> ());
   | _ -> ());
  pf ppf "@,";
  pf ppf "%a:@," (styled `Bold string) "Abstract";
  pf ppf "%a@," (styled `Faint string) (abstract p);
  pf ppf "@]"
