(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Versioned publication entries. *)

@@ portable

(** {1 Types} *)

type classification = Full | Short | Preprint
(** How substantial a publication is. *)

type t = {
  slug : string;  (** Shared by every version of one publication. *)
  ver : string;  (** Version identifier, ordered by [String.compare]. *)
  title : string;
  authors : string list;
  year : int;
  month : int;
  bibtype : string;  (** BibTeX entry type, such as ["inproceedings"]. *)
  publisher : string;
  booktitle : string;
  journal : string;
  institution : string;
  pages : string;
  volume : string option;
  number : string option;
  doi : string option;
  url : string option;
  video : string option;  (** UUID of the talk recording. *)
  isbn : string;
  editor : string;
  bib : string;  (** Raw BibTeX record. *)
  tags : string list;
  projects : string list;  (** Slugs of the owning projects. *)
  slides : string list;
  abstract : string;  (** Bushel markdown body. *)
  latest : bool;  (** Newest version of this slug, computed by {!tv}. *)
  selected : bool;  (** Curated highlight for index pages. *)
  classification : classification option;
      (** Recorded classification. When absent {!val-classification} guesses
          one from the other fields. *)
  note : string option;
  social : Bushel_types.social option;
}
(** One version of a publication. *)

type ts = t list
(** A list of papers, in load order. *)

val string_of_classification : classification -> string
(** [string_of_classification c] is ["full"], ["short"] or ["preprint"]. *)

(** {1 Accessors} *)

val slug : t -> string
(** [slug p] is the slug of [p], shared with its other versions. *)

val title : t -> string
(** [title p] is the title of [p]. *)

val authors : t -> string list
(** [authors p] is the authors of [p], in citation order. *)

val year : t -> int
(** [year p] is the year [p] was published. *)

val bibtype : t -> string
(** [bibtype p] is the BibTeX entry type of [p]. *)

val publisher : t -> string
(** [publisher p] is the publisher of [p]. *)

val booktitle : t -> string
(** [booktitle p] is the proceedings title of [p]. *)

val journal : t -> string
(** [journal p] is the journal title of [p]. *)

val institution : t -> string
(** [institution p] is the issuing institution of a technical report. *)

val pages : t -> string
(** [pages p] is the page range of [p]. *)

val volume : t -> string option
(** [volume p] is the journal volume of [p]. *)

val number : t -> string option
(** [number p] is the journal issue or report number of [p]. *)

val doi : t -> string option
(** [doi p] is the DOI of [p]. *)

val url : t -> string option
(** [url p] is the canonical URL of [p]. *)

val best_url : t -> string option
(** [best_url p] is the URL to send a reader to, which is {!val-url}. *)

val video : t -> string option
(** [video p] is the UUID of the talk recording of [p]. *)

val isbn : t -> string
(** [isbn p] is the ISBN of [p]. *)

val editor : t -> string
(** [editor p] is the editor of the volume containing [p]. *)

val bib : t -> string
(** [bib p] is the raw BibTeX record of [p]. *)

val tags : t -> string list
(** [tags p] is the tags of [p], which {!of_frontmatter} extends with the
    keywords, the bibtype tag and the project slugs. *)

val project_slugs : t -> string list
(** [project_slugs p] is the slugs of the projects owning [p]. *)

val slides : t -> string list
(** [slides p] is the slide decks that accompany [p]. *)

val abstract : t -> string
(** [abstract p] is the Bushel markdown body of [p]. *)

val selected : t -> bool
(** [selected p] is [true] if [p] is a curated highlight. *)

val note : t -> string option
(** [note p] is the editorial note attached to [p]. *)

val social : t -> Bushel_types.social option
(** [social p] is the discussion links recorded for [p]. *)

val classification : t -> classification
(** [classification p] is the recorded classification of [p]. When none was
    recorded it is inferred from the publication metadata. *)

val date : t -> int * int * int
(** [date p] is [(year, month, 1)]. A paper records no day. *)

(** {1 Ordering} *)

val compare : t -> t -> int
(** [compare a b] orders by date, most recent first. Invalid dates sort as
    1977-01-01. *)

(** {1 Versions and lookup} *)

val tv : t list -> t list
(** [tv ps] is [ps] with the [latest] field of each entry set, marking the
    greatest [ver] for each slug. *)

val lookup : t list -> string -> t option
(** [lookup ps slug] is the latest version of [slug] in [ps], or [None] if
    [ps] holds no latest version under that slug. *)

(** {1 Parsing and printing} *)

val of_frontmatter :
  slug:string -> ver:string -> Frontmatter.t -> (t, string) result
  @@ nonportable
(** [of_frontmatter ~slug ~ver fm] is the paper version [ver] of [slug]
    described by [fm]. The abstract comes from the body. *)

val pp : Format.formatter -> t -> unit @@ nonportable
(** [pp ppf p] prints [p] to [ppf] as a styled multi-line summary. *)
