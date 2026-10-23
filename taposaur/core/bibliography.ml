type t = {
  title : string option;
  authors : Author.t list;
  journal : string option;
  publisher : string option;
  published : string option;
  year : int option;
  abstract : string option;
  volume : string option;
  issue : string option;
  pages : string option;
  issn : string option;
  doi : string option;
  pmcid : string option;
  pmid : string option;
}

let empty = {
  title = None;
  authors = [];
  journal = None;
  publisher = None;
  published = None;
  year = None;
  abstract = None;
  volume = None;
  issue = None;
  pages = None;
  issn = None;
  doi = None;
  pmcid = None;
  pmid = None;
}

let of_index (i : Index.t) = { empty with
  title = i.title;
  authors = i.authors;
  journal = i.journal;
  publisher = i.publisher;
  published = i.published;
  year = i.year;
  abstract = i.abstract;
  doi = i.doi;
  pmcid = i.pmcid;
  pmid = i.pmid;
}

type presence = { title : bool; abstract : bool; publisher : bool }
let presence (b : t) =
  let available = function Some s -> String.trim s <> "" | None -> false in
  { title = available b.title; abstract = available b.abstract;
    publisher = available b.publisher }

let jsont =
  Jsont.Object.map
    (fun title authors journal publisher published year abstract volume
      issue pages issn doi pmcid pmid ->
      { title; authors; journal; publisher; published; year; abstract;
        volume; issue; pages; issn; doi; pmcid; pmid })
  |> Jsont.Object.opt_mem "title" Jsont.string
       ~enc:(fun (b : t) -> b.title)
  |> Jsont.Object.mem "authors" (Jsont.list Author.jsont)
       ~dec_absent:(fun () -> []) ~enc:(fun (b : t) -> b.authors)
  |> Jsont.Object.opt_mem "journal" Jsont.string
       ~enc:(fun (b : t) -> b.journal)
  |> Jsont.Object.opt_mem "publisher" Jsont.string
       ~enc:(fun (b : t) -> b.publisher)
  |> Jsont.Object.opt_mem "published" Jsont.string
       ~enc:(fun (b : t) -> b.published)
  |> Jsont.Object.opt_mem "year" Jsont.int
       ~enc:(fun (b : t) -> b.year)
  |> Jsont.Object.opt_mem "abstract" Jsont.string
       ~enc:(fun (b : t) -> b.abstract)
  |> Jsont.Object.opt_mem "volume" Jsont.string
       ~enc:(fun (b : t) -> b.volume)
  |> Jsont.Object.opt_mem "issue" Jsont.string
       ~enc:(fun (b : t) -> b.issue)
  |> Jsont.Object.opt_mem "pages" Jsont.string
       ~enc:(fun (b : t) -> b.pages)
  |> Jsont.Object.opt_mem "issn" Jsont.string
       ~enc:(fun (b : t) -> b.issn)
  |> Jsont.Object.opt_mem "doi" Jsont.string
       ~enc:(fun (b : t) -> b.doi)
  |> Jsont.Object.opt_mem "pmcid" Jsont.string
       ~enc:(fun (b : t) -> b.pmcid)
  |> Jsont.Object.opt_mem "pmid" Jsont.string
       ~enc:(fun (b : t) -> b.pmid)
  |> Jsont.Object.finish

let of_string = Jsont_bytesrw.decode_string jsont
let to_string = Jsont_bytesrw.encode_string jsont
