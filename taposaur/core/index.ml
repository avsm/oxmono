(* Copyright (c) 2026 Anil Madhavapeddy. SPDX-License-Identifier: ISC *)

type t = {
  uid : Uid.t;
  doi : string option;
  pmcid : string option;
  pmid : string option;
  pdf_path : string;
  pdf_size : int64;
  pdf_mtime : float;
  source : string option;
  has_xml : bool;
  has_s2orc : bool;
  tei_path : string;
  tei_alt_path : string option;
  tei_mtime : float;
  tei_source : string option;
  title : string option;
  authors : Author.t list;
  journal : string option;
  publisher : string option;
  published : string option;
  year : int option;
  abstract : string option;
  corrected : string list;
}

(* [scheme], [doi] and [pmcid] are written out beside [uid] so a
   consumer can filter on one namespace, or on a scheme's own
   identifier, without parsing the uid. The scheme must agree with it. *)
let jsont =
  Jsont.Object.map ~kind:"Paper"
    (fun uid scheme doi pmcid pmid pdf_path pdf_size pdf_mtime source
      has_xml has_s2orc tei_path tei_alt_path tei_mtime tei_source title
      authors journal publisher published year abstract corrected ->
      if scheme <> Uid.Scheme.to_string (Uid.scheme uid) then
        Jsont.Error.msg Jsont.Meta.none "Scheme disagrees with UID";
      if pdf_size < 0L then
        Jsont.Error.msg Jsont.Meta.none "Negative PDF size";
      { uid; doi; pmcid; pmid; pdf_path; pdf_size; pdf_mtime; source;
        has_xml; has_s2orc; tei_path; tei_alt_path; tei_mtime; tei_source;
        title; authors; journal; publisher; published; year; abstract;
        corrected })
  |> Jsont.Object.mem "uid" Uid.jsont ~enc:(fun e -> e.uid)
  |> Jsont.Object.mem "scheme" Jsont.string
    ~enc:(fun e -> Uid.Scheme.to_string (Uid.scheme e.uid))
  |> Jsont.Object.opt_mem "doi" Jsont.string ~enc:(fun e -> e.doi)
  |> Jsont.Object.opt_mem "pmcid" Jsont.string ~enc:(fun e -> e.pmcid)
  |> Jsont.Object.opt_mem "pmid" Jsont.string ~enc:(fun e -> e.pmid)
  |> Jsont.Object.mem "pdf_path" Jsont.string
    ~enc:(fun e -> e.pdf_path)
  |> Jsont.Object.mem "pdf_size" Jsont.int64
    ~enc:(fun e -> e.pdf_size)
  |> Jsont.Object.mem "pdf_mtime" Jsont.number
    ~enc:(fun e -> e.pdf_mtime)
  |> Jsont.Object.opt_mem "source" Jsont.string ~enc:(fun e -> e.source)
  |> Jsont.Object.mem "has_xml" Jsont.bool ~dec_absent:(fun () -> false)
    ~enc:(fun e -> e.has_xml) ~enc_omit:(fun b -> not b)
  |> Jsont.Object.mem "has_s2orc" Jsont.bool ~dec_absent:(fun () -> false)
    ~enc:(fun e -> e.has_s2orc) ~enc_omit:(fun b -> not b)
  |> Jsont.Object.mem "tei_path" Jsont.string
    ~enc:(fun e -> e.tei_path)
  |> Jsont.Object.opt_mem "tei_alt_path" Jsont.string
    ~enc:(fun e -> e.tei_alt_path)
  |> Jsont.Object.mem "tei_mtime" Jsont.number
    ~enc:(fun e -> e.tei_mtime)
  |> Jsont.Object.opt_mem "tei_source" Jsont.string
    ~enc:(fun e -> e.tei_source)
  |> Jsont.Object.opt_mem "title" Jsont.string ~enc:(fun e -> e.title)
  |> Jsont.Object.mem "authors" (Jsont.list Author.jsont)
    ~dec_absent:(fun () -> []) ~enc:(fun e -> e.authors) ~enc_omit:(fun l -> l = [])
  |> Jsont.Object.opt_mem "journal" Jsont.string ~enc:(fun e -> e.journal)
  |> Jsont.Object.opt_mem "publisher" Jsont.string
    ~enc:(fun e -> e.publisher)
  |> Jsont.Object.opt_mem "published" Jsont.string
    ~enc:(fun e -> e.published)
  |> Jsont.Object.opt_mem "year" Jsont.int ~enc:(fun e -> e.year)
  |> Jsont.Object.opt_mem "abstract" Jsont.string ~enc:(fun e -> e.abstract)
  (* Which of the fields above a person set rather than a document
     said, so a consumer can tell one from the other without reading
     the paper's manifest. Absent from a line nobody has corrected,
     which is nearly every line, and last in the object so a consumer
     reading the members it knows is untouched by it. *)
  |> Jsont.Object.mem "corrected" (Jsont.list Jsont.string) ~dec_absent:(fun () -> [])
    ~enc:(fun e -> e.corrected) ~enc_omit:(fun l -> l = [])
  |> Jsont.Object.finish

let of_string = Jsont_bytesrw.decode_string jsont
let to_string = Jsont_bytesrw.encode_string jsont
