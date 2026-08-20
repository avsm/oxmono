(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Bushel markdown extensions.

    Bushel extends CommonMark with three link forms that name things inside
    the knowledge base rather than URLs.

    - [:slug] links to the entry with that slug.
    - [@handle] links to the contact with that handle.
    - [##tag] links to a tag, and [###kind] to an entry kind.

    Both the inline form [[text](:slug)] and the reference form [[text][:slug]]
    are recognised. The reference form only works if the document was parsed
    with {!with_bushel_links} as its resolver, because CommonMark would
    otherwise drop a reference with no definition.

    This module supplies the resolver and three mappers that rewrite those
    links. A mapper is chosen by what the output is for. {!make_sidenote_mapper}
    produces {!Side_note} nodes for the website, which renders them as hover
    previews. {!make_link_only_mapper} produces ordinary links for feeds and
    for search indexing. {!to_markdown} produces standard markdown.

    The link predicates and {!with_bushel_links} are portable, so a renderer
    running inside a function marked [portable] can parse with the resolver.
    The mappers and the whole-document conversions are not, and each says
    why. *)

@@ portable

(** {1 Sidenotes} *)

type sidenote_data =
  | Contact_note of Sortal_schema.Contact.t * string
  | Paper_note of Bushel_paper.t * string
  | Idea_note of Bushel_idea.t * string
  | Note_note of Bushel_note.t * string
  | Project_note of Bushel_project.t * string
  | Video_note of Bushel_video.t * string
  | Footnote_note of string * Cmarkit.Block.t * string
      (** Nothing constructs this. It is kept because removing a constructor
          from a public type is a wider change than this file. *)
(** What a sidenote shows. The second field of each case is the text that
    triggered it, which is the link text with a slug resolved to a title. *)

type Cmarkit.Inline.t += Side_note of sidenote_data
(** The inline node {!make_sidenote_mapper} writes in place of a Bushel link.
    A renderer that does not handle it falls through to the default
    rendering, which prints nothing. *)

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
    label starting with [":"], ["@"] or ["##"] with that same label tagged, so
    that the mappers below can tell a slug reference from a contact reference.
    It is a [Cmarkit.Label.t] and not a definition: it carries no destination,
    and a renderer that does not consume the tag reports the label as
    undefined. Any other label resolves as CommonMark says. Parse with this or
    the reference form of a Bushel link is dropped before a mapper ever sees
    it. *)

(** {1 Mappers}

    None of the three is portable, and the reason is the same for all three
    and worth stating once. Each writes its results with
    [Cmarkit.Mapper.default] and [Cmarkit.Mapper.ret]. [Mapper.default] is a
    module-level value of a polymorphic variant type, which crosses nothing,
    so a portable function that reads it may only return a [contended]
    result. [Cmarkit.Mapper.mapper] does not admit one, so such a mapper
    cannot be passed to [Cmarkit.Mapper.make] and the whole arrangement is
    unusable. Writing the literals [`Default] and [`Map (Some v)] in place of
    the two constants removes the read and the mappers become portable, which
    the compiler confirms on the version of this file that does so. *)

val make_sidenote_mapper :
  Bushel_entry.t -> Cmarkit.Inline.t Cmarkit.Mapper.mapper @@ nonportable
(** [make_sidenote_mapper es] is an inline mapper that rewrites a Bushel link
    into a {!Side_note} carrying the entry or contact it names. A tag or kind
    link stays an ordinary link, and a link to a slug that [es] does not hold
    becomes a link to the site path for that slug. An image whose target is a
    slug becomes an image under [/images], or a link under [/videos] if the
    slug names a video. *)

val make_link_only_mapper :
  Bushel_entry.t -> Cmarkit.Inline.t Cmarkit.Mapper.mapper @@ nonportable
(** [make_link_only_mapper es] is an inline mapper that rewrites a Bushel link
    into an ordinary link to the site path of what it names. A contact link
    becomes a link to that contact's best URL, or plain text if the contact
    has none. Use this where a sidenote cannot be shown, such as in a feed. *)

val make_bushel_link_only_mapper :
  'a -> Bushel_entry.t -> Cmarkit.Inline.t Cmarkit.Mapper.mapper
  @@ nonportable
(** [make_bushel_link_only_mapper defs es] is [make_link_only_mapper es].
    [defs] is ignored and is there for callers that hold a definition map. *)

(** {1 Whole-document conversions} *)

val plain_text_of_markdown :
  ?contact_name:(string -> string option) -> string -> string @@ nonportable
(** [plain_text_of_markdown md] is the prose of [md] with its markup removed,
    one paragraph or heading per line and a blank line before each heading. A
    Bushel link becomes its text, a contact link becomes the contact's name if
    [contact_name] maps its handle and the handle otherwise, and an image with
    a slug target is dropped.

    This is not portable because its mapper reads [Cmarkit.Mapper.delete], a
    module-level polymorphic variant constant that crosses nothing. *)

val to_markdown :
  ?base_url:string ->
  ?image_base:string ->
  entries:Bushel_entry.t ->
  string ->
  string @@ nonportable
(** [to_markdown ~entries md] is [md] as standard markdown, with every Bushel
    link resolved to a URL under [base_url], which defaults to the empty
    string, and every slug image resolved under [image_base], which defaults
    to ["/images"]. A video embed and an image carrying a placement directive
    become raw HTML, because markdown cannot express either.

    This is not portable because it rewrites a video watch URL to an embed URL
    through [Uri]. *)

val extract_all_links : string -> string list @@ nonportable
(** [extract_all_links md] is every link and image target in [md], including
    Bushel reference labels, sorted and without duplicates.

    This is not portable because it collects into a locally applied
    [Set.Make], whose module-level values cross nothing. *)

(** {1 Validation} *)

val validate_references :
  Bushel_entry.t -> string -> string list * string list @@ nonportable
(** [validate_references es md] is the Bushel slugs and the contact handles
    that [md] links to and [es] does not hold, each in unspecified order. Both
    are returned with their sigils.

    This is not portable because its mapper reads [Cmarkit.Mapper.default], a
    module-level polymorphic variant constant that crosses nothing. *)

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
    [(doi, citation, source)] triples in the order they were found. It gathers
    the entry [n] is about, the entries its body links to, the DOI URLs in its
    body and the publisher URLs in its body that the DOI cache of [es]
    resolves. A DOI already seen is not repeated, and the note's own DOI is
    left out. [default_author] supplies the author of a cited note that names
    none.

    This is not portable because it scans for URLs with [Re] and decodes them
    with [Uri]. *)
