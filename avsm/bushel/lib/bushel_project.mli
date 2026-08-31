(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Project entry type for Bushel.

    A project is a named piece of work with a start year and an optional
    finish year. Everything here is portable except {!of_frontmatter} and
    {!pp}, which is what lets a renderer read a project from inside a
    function marked [portable]. *)

@@ portable

(** {1 Types} *)

type t = {
  slug : string;
  title : string;
  start : int;  (** Start year. *)
  finish : int option;  (** End year, or [None] while the project runs. *)
  tags : string list;
  ideas : string;  (** One line on what working on this project's ideas
                       involves, shown to a student on the ideas index. *)
  body : string;
  social : Bushel_types.social option;
}
(** A project entry. The record is public because the loader builds one field
    by field and the renderers read fields directly. *)

type ts = t list
(** A list of projects, in load order. *)

(** {1 Accessors} *)

val slug : t -> string
(** [slug p] is the slug of [p]. *)

val title : t -> string
(** [title p] is the title of [p]. *)

val start : t -> int
(** [start p] is the year [p] began. *)

val finish : t -> int option
(** [finish p] is the year [p] ended, or [None] if it is still running. *)

val tags : t -> string list
(** [tags p] is the tags of [p]. *)

val ideas : t -> string
(** [ideas p] is one line on what working on the ideas of [p] involves, or the
    empty string if [p] sets none. *)

val body : t -> string
(** [body p] is the Bushel markdown body of [p]. *)

val social : t -> Bushel_types.social option
(** [social p] is the discussion links recorded for [p]. *)

(** {1 Ordering} *)

val compare : t -> t -> int
(** [compare a b] orders a running project before a finished one, then by
    finish year descending, then by start year descending. *)

(** {1 Parsing and printing} *)

val of_frontmatter : Frontmatter.t -> (t, string) result @@ nonportable
(** [of_frontmatter fm] is the project described by [fm]. The slug comes from
    the file name and the start year from the [date] field. A field that is
    absent takes its empty value rather than failing. *)

val pp : Format.formatter -> t -> unit @@ nonportable
(** [pp ppf p] prints [p] to [ppf] as a styled multi-line summary. *)
