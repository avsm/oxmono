(* Copyright (c) 2026 Anil Madhavapeddy. SPDX-License-Identifier: ISC *)

type id = Uid.t

(* Vocabularies *)

(* Each vocabulary spells its labels once and parses by searching that
   spelling, so a label and its parser cannot drift apart, and a JSON
   string outside the vocabulary is a decoding error rather than a
   value no reader knows. *)
let vocabulary (type a : immutable_data) ~(kind : string) ~(what : string) ~(all : a list)
    (to_string : (a -> string) @ portable) =
  let of_string s =
    match List.find_opt (fun v -> String.equal (to_string v) s) all with
    | Some v -> Ok v
    | None -> Error (Printf.sprintf "unknown %s %S" what s)
  in
  let dec s =
    match of_string s with
    | Ok v -> v
    | Error e -> Jsont.Error.msg Jsont.Meta.none e
  in
  (of_string, Jsont.map ~kind ~dec ~enc:to_string Jsont.string)

module Source = struct
  type t = Publisher | Oa | Core | Url | Tei | S2orc

  let to_string = function
    | Publisher -> "publisher"
    | Oa -> "oa"
    | Core -> "core"
    | Url -> "url"
    | Tei -> "tei"
    | S2orc -> "s2orc"

  let of_string, jsont =
    vocabulary ~kind:"Source" ~what:"artifact source"
      ~all:[ Publisher; Oa; Core; Url; Tei; S2orc ] to_string
end

module Format = struct
  type t = Xml | Pdf | Tei | Json

  let to_string = function
    | Xml -> "xml" | Pdf -> "pdf" | Tei -> "tei" | Json -> "json"

  let of_string, jsont =
    vocabulary ~kind:"Format" ~what:"artifact format"
      ~all:[ Xml; Pdf; Tei; Json ] to_string
end

module Version = struct
  type t = Published | Accepted | Preprint | Unknown

  let to_string = function
    | Published -> "published"
    | Accepted -> "accepted"
    | Preprint -> "preprint"
    | Unknown -> "unknown"

  let of_string, jsont =
    vocabulary ~kind:"Version" ~what:"artifact version"
      ~all:[ Published; Accepted; Preprint; Unknown ] to_string
end

module Confidence = struct
  type t = Observed | Assumed

  let to_string = function Observed -> "observed" | Assumed -> "assumed"

  let of_string, jsont =
    vocabulary ~kind:"Confidence" ~what:"provenance confidence"
      ~all:[ Observed; Assumed ] to_string
end

module Route = struct
  type t = Pub2tei | Grobid | S2orc

  (* Pub2TEI reads the article's own markup where GROBID reads a
     rendered page, so it leads wherever a paper offers both. The
     s2orc route rewrites Semantic Scholar's parse of a page we may
     not hold ourselves, which is why it ranks after both of ours. *)
  let all = [ Pub2tei; Grobid; S2orc ]

  let to_string = function
    | Pub2tei -> "pub2tei" | Grobid -> "grobid" | S2orc -> "s2orc"

  let of_string, jsont =
    vocabulary ~kind:"Route" ~what:"conversion route" ~all to_string


end

module Field = struct
  type t = Title | Authors | Journal | Publisher | Year | Abstract

  let all = [ Title; Authors; Journal; Publisher; Year; Abstract ]

  let to_string = function
    | Title -> "title"
    | Authors -> "authors"
    | Journal -> "journal"
    | Publisher -> "publisher"
    | Year -> "year"
    | Abstract -> "abstract"

  (* An identity field is refused by name rather than left to the
     unknown-field message: a paper with the wrong doi or pmcid is a
     different paper, and a reader has to be told that rather than left
     thinking the name was misspelled. *)
  let identity = [ "doi"; "pmcid"; "pmid"; "uid"; "id" ]

  let of_string s =
    let s = String.lowercase_ascii (String.trim s) in
    match List.find_opt (fun f -> String.equal (to_string f) s) all with
    | Some f -> Ok f
    | None when List.mem s identity ->
      Error
        (Printf.sprintf
           "%s is an identity field: a paper with a different identity is a \
            different paper, so it is moved between objects rather than \
            corrected"
           s)
    | None -> Error (Printf.sprintf "unknown correctable field %S" s)

  let jsont =
    let dec s =
      match of_string s with
      | Ok f -> f
      | Error e -> Jsont.Error.msg Jsont.Meta.none e
    in
    Jsont.map ~kind:"Field" ~dec ~enc:to_string Jsont.string

  (* The value is checked where the correction is made rather than
     where it is read: a year that is not a year must not reach a
     manifest that a consumer reads a number out of. *)

end

module Correction = struct
  type t = {
    field : Field.t;
    value : string;
    by : string;
    at : string;
    note : string option;
  }

  let jsont =
    Jsont.Object.map ~kind:"Correction" (fun field value by at note ->
        { field; value; by; at; note })
    |> Jsont.Object.mem "field" Field.jsont ~enc:(fun c -> c.field)
    |> Jsont.Object.mem "value" Jsont.string ~enc:(fun c -> c.value)
    |> Jsont.Object.mem "by" Jsont.string ~enc:(fun c -> c.by)
    |> Jsont.Object.mem "at" Jsont.string ~enc:(fun c -> c.at)
    |> Jsont.Object.opt_mem "note" Jsont.string ~enc:(fun c -> c.note)
    |> Jsont.Object.finish
end

module Artifact = struct
  type t = {
    file : string;
    format : Format.t;
    source : Source.t;
    version : Version.t option;
    converter : string option;
    from : string option;
    strategy : string option;
    status : int option;
    bytes : int64;
    sha256 : string option;
    at : string;
    url : string option;
    origin : string option;
    confidence : Confidence.t option;
  }

  let jsont =
    Jsont.Object.map ~kind:"Artifact"
      (fun file format source version converter from strategy url status bytes
           sha256 at origin confidence ->
        if bytes < 0L then
          Jsont.Error.msg Jsont.Meta.none "Negative artifact size";
        { file; format; source; version; converter; from; strategy; url;
          status; bytes; sha256; at; origin; confidence })
    |> Jsont.Object.mem "file" Json.filename ~enc:(fun a -> a.file)
    |> Jsont.Object.mem "format" Format.jsont ~enc:(fun a -> a.format)
    |> Jsont.Object.mem "source" Source.jsont ~enc:(fun a -> a.source)
    |> Jsont.Object.opt_mem "version" Version.jsont ~enc:(fun a -> a.version)
    |> Jsont.Object.opt_mem "converter" Jsont.string
         ~enc:(fun a -> a.converter)
    |> Jsont.Object.opt_mem "from" Json.filename ~enc:(fun a -> a.from)
    |> Jsont.Object.opt_mem "strategy" Jsont.string ~enc:(fun a -> a.strategy)
    |> Jsont.Object.opt_mem "url" Jsont.string ~enc:(fun a -> a.url)
    |> Jsont.Object.opt_mem "status" Jsont.int ~enc:(fun a -> a.status)
    |> Jsont.Object.mem "bytes" Jsont.int64 ~enc:(fun a -> a.bytes)
    |> Jsont.Object.opt_mem "sha256" Jsont.string ~enc:(fun a -> a.sha256)
    |> Jsont.Object.mem "at" Jsont.string ~enc:(fun a -> a.at)
    |> Jsont.Object.opt_mem "origin" Jsont.string ~enc:(fun a -> a.origin)
    |> Jsont.Object.opt_mem "confidence" Confidence.jsont
         ~enc:(fun a -> a.confidence)
    |> Jsont.Object.finish
end

(* A conversion that would not run, remembered so a sweep does not ask
   the same question of the same document for ever. It is a projection
   of the error events the log already holds, not a new kind of record,
   and it is dropped the moment that route produces a conversion. *)
module Failure = struct
  type t = {
    route : Route.t;
    at : string;  (** When the last attempt failed. *)
    attempts : int;  (** How many have failed, over the whole log. *)
    reason : string;  (** What the converter said, verbatim. *)
    permanent : bool;
        (** Whether the converter was refusing this document rather than
            failing at its own end. A permanent refusal is not asked
            again by an ordinary sweep, because asking spends the
            converter to be told the same thing; only a converter new
            enough to answer differently, named by [--retry-failed
            --since], gets another go. Absent in a manifest written
            before the distinction existed, which reads as false, so
            those are re-asked once and classified properly. *)
  }

  let jsont =
    Jsont.Object.map ~kind:"Failure"
      (fun route at attempts reason permanent ->
        { route; at; attempts; reason; permanent })
    |> Jsont.Object.mem "route" Route.jsont ~enc:(fun f -> f.route)
    |> Jsont.Object.mem "at" Jsont.string ~enc:(fun f -> f.at)
    |> Jsont.Object.mem "attempts" Jsont.int ~dec_absent:(fun () -> 1)
         ~enc:(fun f -> f.attempts)
    |> Jsont.Object.mem "reason" Jsont.string ~enc:(fun f -> f.reason)
    |> Jsont.Object.mem "permanent" Jsont.bool ~dec_absent:(fun () -> false)
         ~enc:(fun f -> f.permanent)
    |> Jsont.Object.finish
end

type t = {
  v : int;
  id : id;
  doi : string option;
  pmcid : string option;
  pmid : string option;
  title : string option;
  publisher : string option;
  year : int option;
  corrections : Correction.t list;
  artifacts : Artifact.t list;
  conversions_failed : Failure.t list;
  updated : string;
}

let jsont =
  Jsont.Object.map ~kind:"Paper"
    (fun v id doi pmcid pmid title publisher year corrections artifacts
         conversions_failed updated ->
      { v; id; doi; pmcid; pmid; title; publisher; year; corrections; artifacts;
        conversions_failed; updated })
  |> Jsont.Object.mem "v" Json.version ~enc:(fun m -> m.v)
  |> Jsont.Object.mem "id" Uid.jsont ~enc:(fun m -> m.id)
  |> Jsont.Object.opt_mem "doi" Jsont.string ~enc:(fun m -> m.doi)
  |> Jsont.Object.opt_mem "pmcid" Jsont.string ~enc:(fun m -> m.pmcid)
  |> Jsont.Object.opt_mem "pmid" Jsont.string ~enc:(fun m -> m.pmid)
  |> Jsont.Object.opt_mem "title" Jsont.string ~enc:(fun m -> m.title)
  |> Jsont.Object.opt_mem "publisher" Jsont.string ~enc:(fun m -> m.publisher)
  |> Jsont.Object.opt_mem "year" Jsont.int ~enc:(fun m -> m.year)
  (* Left out where a paper has none, so a manifest carrying this
     member is one a person has changed by hand. *)
  |> Jsont.Object.mem "corrections" (Jsont.list Correction.jsont)
       ~dec_absent:(fun () -> []) ~enc:(fun m -> m.corrections)
       ~enc_omit:(fun l -> l = [])
  (* A member of every manifest, empty array and all: a paper nothing
     has been fetched for says so rather than leaving the question
     open. *)
  |> Jsont.Object.mem "artifacts" (Jsont.list Artifact.jsont) ~enc:(fun m -> m.artifacts)
  (* Present only where a conversion has failed and not since
     succeeded, so a manifest carrying this member names work a sweep
     should leave alone until something changes. *)
  |> Jsont.Object.mem "conversions_failed" (Jsont.list Failure.jsont)
       ~dec_absent:(fun () -> []) ~enc:(fun m -> m.conversions_failed)
       ~enc_omit:(fun l -> l = [])
  |> Jsont.Object.mem "updated" Jsont.string ~enc:(fun m -> m.updated)
  |> Jsont.Object.finish


let of_string = Jsont_bytesrw.decode_string jsont
let to_string = Jsont_bytesrw.encode_string jsont

let correction m field =
  List.fold_left (fun found (c : Correction.t) ->
    if c.field = field then Some c.value else found) None m.corrections
