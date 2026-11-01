(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Research idea entries. *)

@@ portable

(** {1 Types} *)

type level = Any | PartII | MPhil | PhD | Postdoc
(** The academic level an idea is pitched at. *)

type status = Available | Discussion | Ongoing | Completed | Expired
(** How far an idea has got. *)

type t = {
  slug : string;
  title : string;
  level : level;
  project : string;  (** Slug of the owning project. *)
  status : status;
  month : int;
  year : int;
  supervisors : Sortal_schema.Contact.t list;
      (** Contacts resolved by {!resolve_all_contacts}. *)
  students : Sortal_schema.Contact.t list;
      (** Student contacts resolved by {!resolve_all_contacts}. *)
  supervisor_handles : string list;
  student_handles : string list;
  reading : string;  (** Bushel markdown reading list. *)
  body : string;  (** Bushel markdown body. *)
  url : string option;
  tags : string list;
  social : Bushel_types.social option;
}
(** A research idea. *)

type ts = t list
(** A list of ideas, in load order. *)

val level_to_string : level -> string
(** [level_to_string level] is the display name of [level]. *)

val status_to_string : status -> string
(** [status_to_string s] is the constructor name of [s]. *)

(** {1 Accessors} *)

val slug : t -> string
(** [slug i] is the slug of [i]. *)

val title : t -> string
(** [title i] is the title of [i]. *)

val level : t -> level
(** [level i] is the academic level of [i]. *)

val project : t -> string
(** [project i] is the slug of the project owning [i]. *)

val status : t -> status
(** [status i] is how far [i] has got. *)

val year : t -> int
(** [year i] is the year [i] was offered. *)

val month : t -> int
(** [month i] is the month [i] was offered. *)

val supervisors : t -> Sortal_schema.Contact.t list
(** [supervisors i] is the resolved supervisor contacts of [i]. *)

val students : t -> Sortal_schema.Contact.t list
(** [students i] is the resolved student contacts of [i]. *)

val supervisor_handles : t -> string list
(** [supervisor_handles i] is the supervisor handles as written. *)

val student_handles : t -> string list
(** [student_handles i] is the student handles as written. *)

val reading : t -> string
(** [reading i] is the Bushel markdown reading list of [i]. *)

val body : t -> string
(** [body i] is the Bushel markdown body of [i]. *)

val url : t -> string option
(** [url i] is the external URL of [i]. *)

val tags : t -> string list
(** [tags i] is the tags of [i]. *)

val social : t -> Bushel_types.social option
(** [social i] is the discussion links recorded for [i]. *)

(** {1 Ordering} *)

val compare : t -> t -> int
(** [compare a b] orders by status, level and reverse date. Completed ideas
    are ordered by reverse year only. *)

(** {1 Contact resolution} *)

val resolve_all_contacts :
  Sortal_schema.Contact.t list -> t list -> t list
(** [resolve_all_contacts contacts ideas] is [ideas] with the supervisor and
    student handles resolved against [contacts]. Unknown handles are dropped. *)

(** {1 Parsing and printing} *)

val of_frontmatter : Frontmatter.t -> (t, string) result @@ nonportable
(** [of_frontmatter fm] is the idea described by [fm]. The slug comes from the
    file name. Its date also comes from the file name when absent. *)

val pp : Format.formatter -> t -> unit @@ nonportable
(** [pp ppf i] prints [i] to [ppf] as a styled multi-line summary. *)
