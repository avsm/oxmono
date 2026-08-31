(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** CommonMark with Bushel link extensions.

    - [:slug] links to the entry with that slug.
    - [@handle] links to the contact with that handle.
    - [##tag] links to a tag, and [###kind] to an entry kind.

    Inline and reference links are accepted. Reference links require
    {!with_bushel_links} when parsing. *)

@@ portable

(** {1 Sidenotes} *)

type sidenote_data =
  | Contact_note of Sortal_schema.Contact.t * string
  | Paper_note of Bushel_paper.t * string
  | Idea_note of Bushel_idea.t * string
  | Note_note of Bushel_note.t * string
  | Project_note of Bushel_project.t * string
  | Video_note of Bushel_video.t * string
(** The entry and link text shown by a sidenote. *)

type Cmarkit.Inline.t += Side_note of sidenote_data
(** The inline node produced by {!make_sidenote_mapper}. *)

(** {1 Link Detection} *)

val is_bushel_slug : string -> bool
(** [is_bushel_slug l] is [true] if [l] starts with [":"]. *)

val is_tag_slug : string -> bool
(** [is_tag_slug l] is [true] if [l] starts with ["##"] but not ["###"]. *)

val is_kind_slug : string -> bool
(** [is_kind_slug l] is [true] if [l] starts with ["###"]. *)

val is_contact_slug : string -> bool
(** [is_contact_slug l] is [true] if [l] starts with ["@"]. *)

val strip_handle : string -> string
(** [strip_handle l] is [l] without its leading sigil, whichever of [":"],
    ["@"], ["##"] and ["###"] it carries. A string with no sigil is returned
    unchanged. *)

(** {1 Resolution} *)

val with_bushel_links : Cmarkit.Label.resolver
(** [with_bushel_links] is a resolver that answers an undefined reference
    label beginning with [":"], ["@"] or ["##"] as a Bushel link. Other labels
    use CommonMark resolution. *)

(** {1 Mappers} *)

val make_sidenote_mapper :
  Bushel_entry.t -> Cmarkit.Inline.t Cmarkit.Mapper.mapper
(** [make_sidenote_mapper es] is an inline mapper that rewrites a Bushel link
    into a {!Side_note}. Tag and kind links remain ordinary links. *)

val make_link_only_mapper :
  Bushel_entry.t -> Cmarkit.Inline.t Cmarkit.Mapper.mapper
(** [make_link_only_mapper es] is an inline mapper that rewrites a Bushel link
    into an ordinary site link. Contacts without URLs become plain text. *)

(** {1 Whole-document conversions} *)

val plain_text_of_markdown :
  ?contact_name:(string -> string option) -> string -> string
(** [plain_text_of_markdown md] is the prose of [md] with its markup removed,
    preserving paragraph and heading boundaries. [contact_name] may expand
    contact handles. *)

val to_markdown :
  ?base_url:string ->
  ?image_base:string ->
  entries:Bushel_entry.t ->
  string ->
  string
(** [to_markdown ~entries md] is [md] as standard markdown, with every Bushel
    link resolved below [base_url] and every image below [image_base]. The
    defaults are [""] and ["/images"]. *)

val extract_all_links : string -> string list @@ nonportable
(** [extract_all_links md] is every distinct link and image target in [md],
    sorted lexically. *)

(** {1 Validation} *)

val validate_references :
  Bushel_entry.t -> string -> string list * string list
(** [validate_references es md] is the Bushel slugs and the contact handles
    that [md] links to and [es] does not hold, each in unspecified order. Both
    are returned with their sigils. *)

(** {1 References} *)

type reference_source =
  | Paper  (** Cited as a source document. *)
  | Note  (** Cited as related. *)
  | External  (** Cited, with nothing more said. *)
(** Where a reference points, which decides the CiTO property a feed
    annotates it with. *)

val note_references :
  Bushel_entry.t ->
  Sortal_schema.Contact.t ->
  Bushel_note.t ->
  (string * string * reference_source) list @@ nonportable
(** [note_references es default_author n] is the works [n] cites, as
    [(doi, citation, source)] triples in document order. Duplicate and self
    citations are omitted. [default_author] supplies missing note authors. *)
