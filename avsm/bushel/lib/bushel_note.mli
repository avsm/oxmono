(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Notes, weeknotes and link posts. *)

@@ portable

(** {1 Types} *)

type t = {
  title : string;
  date : Ptime.date;  (** Date of first publication. *)
  slug : string;
  body : string;  (** Bushel markdown body. *)
  tags : string list;
  draft : bool;
  updated : Ptime.date option;  (** Date of the last revision. *)
  sidebar : string option;  (** Bushel markdown shown beside the body. *)
  index_page : bool;  (** Section index rather than an ordinary note. *)
  perma : bool;  (** Permanent article that will receive a DOI. *)
  weeknote : bool;  (** Regular small update with ISO week numbering. *)
  featured : bool;  (** Curated highlight for index pages. *)
  doi : string option;  (** DOI identifier for permanent articles. *)
  synopsis : string option;
  titleimage : string option;  (** Slug of the header image. *)
  via : (string * string) option;  (** Label and URL for link-style notes. *)
  slug_ent : string option;  (** Slug of the entry this note is about. *)
  source : string option;  (** Source for news-style notes. *)
  url : string option;  (** External URL for news-style notes. *)
  author : string option;  (** Author for news-style notes. *)
  category : string option;  (** Category for news-style notes. *)
  standardsite : string option;  (** Standards body site reference. *)
  social : Bushel_types.social option;
      (** Discussion links on social platforms. *)
  source_file : string option;
      (** Path the note was loaded from, set by the loader rather than by
          frontmatter. *)
}
(** A note entry. *)

type ts = t list
(** A list of notes, in load order. *)

(** {1 Accessors} *)

val title : t -> string
(** [title n] is the title of [n]. Weeknote titles include the week number. *)

val slug : t -> string
(** [slug n] is the slug of [n]. *)

val body : t -> string
(** [body n] is the Bushel markdown body of [n]. *)

val tags : t -> string list
(** [tags n] is the tags of [n]. *)

val draft : t -> bool
(** [draft n] is [true] if [n] is unpublished. *)

val sidebar : t -> string option
(** [sidebar n] is the Bushel markdown shown beside the body of [n]. *)

val synopsis : t -> string option
(** [synopsis n] is the one-line summary of [n]. *)

val perma : t -> bool
(** [perma n] is [true] if [n] is a permanent article. *)

val weeknote : t -> bool
(** [weeknote n] is [true] if [n] is a weeknote. *)

val featured : t -> bool
(** [featured n] is [true] if [n] is a curated highlight. *)

val doi : t -> string option
(** [doi n] is the DOI of [n], if one has been minted. *)

val titleimage : t -> string option
(** [titleimage n] is the slug of the header image of [n]. *)

val slug_ent : t -> string option
(** [slug_ent n] is the slug of the entry [n] is about, if any. *)

val source : t -> string option
(** [source n] is the source of a news-style [n]. *)

val url : t -> string option
(** [url n] is the external URL of a news-style [n]. *)

val author : t -> string option
(** [author n] is the author of a news-style [n]. *)

val category : t -> string option
(** [category n] is the category of a news-style [n]. *)

val standardsite : t -> string option
(** [standardsite n] is the standards body site [n] refers to. *)

val social : t -> Bushel_types.social option
(** [social n] is the discussion links recorded for [n]. *)

val source_file : t -> string option
(** [source_file n] is the path [n] was loaded from. *)

val words : t -> int
(** [words n] is the number of words in the body of [n]. *)

(** {1 Dates} *)

val date : t -> Ptime.date
(** [date n] is the date of the last revision of [n], falling back to the
    date of first publication when there has been none. *)

val datetime : t -> Ptime.t
(** [datetime n] is {!date} as a timestamp at midnight UTC. *)

val origdate : t -> Ptime.t
(** [origdate n] is the first publication date of [n] at midnight UTC. *)

(** {1 Weeknotes} *)

val iso_week_number : Ptime.date -> int * int
(** [iso_week_number d] is the ISO 8601 [(year, week)] of [d]. The year
    may differ from the calendar year near a year boundary. *)

val week_number : t -> int * int
(** [week_number n] is [iso_week_number (date n)]. *)

val week_date_range_string : t -> string
(** [week_date_range_string n] is the Monday to Sunday span of the ISO week
    of [n], such as ["Feb 3rd–7th"] or ["Mar 28th–Apr 4th"]. *)

val adjacent_weeknotes : t list -> t -> t option * t option
(** [adjacent_weeknotes notes n] is the weeknote before and the weeknote
    after [n] among the weeknotes of [notes], ordered by ISO week. Either
    side is [None] at the ends of the run. *)

(** {1 Ordering} *)

val compare : t -> t -> int
(** [compare a b] orders by {!datetime}, most recent first. *)

(** {1 Link-style notes} *)

val link : t -> [> `Ext of string * string | `Local of string ]
(** [link n] is [`Ext (label, url)] if [n] has an empty body and a [via]
    field, and [`Local slug] otherwise.

    @raise Failure if [n] has an empty body and no [via] field, because such
    a note has nothing to point at. *)

(** {1 Parsing and printing} *)

val of_frontmatter : Frontmatter.t -> (t, string) result @@ nonportable
(** [of_frontmatter fm] is the note described by [fm]. The slug and the date
    default to the file name. A weeknote is dated on Sunday and its title is
    prefixed with the ISO week number. *)

val pp : Format.formatter -> t -> unit @@ nonportable
(** [pp ppf n] prints [n] to [ppf] as a styled multi-line summary. *)
