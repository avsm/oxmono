(** One row of the derived corpus index. Paths are provenance, not file
    access capabilities. Use the archive library to open artifacts. *)

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

val jsont : t Jsont.t @@ portable
val of_string : string -> (t, string) result
val to_string : t -> (string, string) result
(** [of_string s] decodes one index row. Omitted flags and arrays become
    false and empty. Identity, PDF and TEI presence fields are required. *)
