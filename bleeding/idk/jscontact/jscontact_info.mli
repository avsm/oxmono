@@ portable

(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Relations, anniversaries, notes and personal information.

    The Card properties defined here say how Cards relate to each other and hold
    personal detail. They are [relatedTo]
    ({{:https://www.rfc-editor.org/rfc/rfc9553.html#section-2.1.8} RFC 9553
      Section 2.1.8}), [anniversaries], [notes] and [personalInfo]
    ({{:https://www.rfc-editor.org/rfc/rfc9553.html#section-2.8} Section 2.8}).

    The [keywords] property of Section 2.8.2 is a [String[Boolean]] set of free
    text and has no object type of its own. See
    {!Jscontact.Json.Map.string_set}.

    @canonical Jscontact.Info *)

(** Relations between Cards.

    The [relatedTo] property of a Card is a [String[Relation]] map whose keys
    are the [uid] of the related Card and whose values say how the two relate,
    as defined by
    {{:https://www.rfc-editor.org/rfc/rfc9553.html#section-2.1.8} RFC 9553
     Section 2.1.8}. *)
module Relation : sig
  (** The kinds of relation.

      The enumerated values of the [relation] set of a Relation. They are the
      IANA-registered [TYPE] parameter values of the vCard [RELATED] property of
      {{:https://www.rfc-editor.org/rfc/rfc6350.html#section-6.6.6} RFC 6350
       Section 6.6.6}. *)
  module Kind : sig
    type t =
      [ `Acquaintance
      | `Agent
      | `Child
      | `Co_resident
      | `Co_worker
      | `Colleague
      | `Contact
      | `Crush
      | `Date
      | `Emergency
      | `Friend
      | `Kin
      | `Me
      | `Met
      | `Muse
      | `Neighbor
      | `Parent
      | `Sibling
      | `Spouse
      | `Sweetheart
      | `Vendor of string  (** A vendor-specific relation type. *) ]
    (** The type for a relation type. *)

    include Jscontact_enum.S with type t := t
  end

  type t = {
    relation : Kind.t list;
        (** The relationship of the related Card to the Card, as a set of
            relation types. The relationship is undefined if the set is empty,
            which it is by default. *)
    unknown : Jscontact_unknown.t;  (** The members no property above names. *)
  }
  (** The type for a relation. *)

  val make : ?relation:Kind.t list -> ?unknown:Jscontact_unknown.t -> unit -> t
  (** [make ()] is a relation with the given properties. [relation] defaults to
      the empty set. *)

  val equal : t -> t -> bool
  (** [equal a b] is [true] if [a] and [b] have the same properties. *)

  val pp : Format.formatter -> t -> unit
  (** [pp ppf r] formats [r] on [ppf]. *)

  val validate : t -> t Jscontact_valid.t
  (** [validate r] checks the relation types of [r] and its unknown members. *)

  val jsont : t Jsont.t
  (** [jsont] is the codec for a Relation. An empty [relation] set is not
      written back, since Section 2.1.8 defaults it to the empty object. *)
end

(** Memorable dates and events.

    The [anniversaries] property of a Card is an [Id[Anniversary]] map, as
    defined by
    {{:https://www.rfc-editor.org/rfc/rfc9553.html#section-2.8.1} RFC 9553
     Section 2.8.1}. *)
module Anniversary : sig
  (** The kinds of anniversary.

      The enumerated values of the [kind] property of an Anniversary. *)
  module Kind : sig
    type t =
      [ `Birth  (** A birthday anniversary. *)
      | `Death  (** A deathday anniversary. *)
      | `Wedding  (** A wedding day anniversary. *)
      | `Vendor of string  (** A vendor-specific kind. *) ]
    (** The type for the kind of an anniversary. *)

    include Jscontact_enum.S with type t := t
  end

  type t = {
    kind : Kind.t;  (** The kind of anniversary. *)
    date : Jscontact_date.t;
        (** The date of the anniversary in the Gregorian calendar, either a
            whole or partial calendar date or a complete UTC timestamp. *)
    place : Jscontact_address.t option;
        (** An address associated with the anniversary, such as the place of
            birth or death. *)
    unknown : Jscontact_unknown.t;  (** The members no property above names. *)
  }
  (** The type for an anniversary. *)

  val make :
    ?place:Jscontact_address.t ->
    ?unknown:Jscontact_unknown.t ->
    Kind.t ->
    Jscontact_date.t ->
    t
  (** [make kind date] is the anniversary of kind [kind] on [date] with the
      given properties. [place] defaults to unset. *)

  val equal : t -> t -> bool
  (** [equal a b] is [true] if [a] and [b] have the same properties. *)

  val pp : Format.formatter -> t -> unit
  (** [pp ppf a] formats [a] on [ppf]. *)

  val validate : t -> t Jscontact_valid.t
  (** [validate a] checks the kind of [a] and its unknown members, and recurses
      into its date and its place. *)

  val jsont : t Jsont.t
  (** [jsont] is the codec for an Anniversary. Its [date] is coded by
      {!Jscontact.Date.jsont}, which implies PartialDate on an object with no
      [@type] member. *)
end

(** Free-text notes.

    The [notes] property of a Card is an [Id[Note]] map, as defined by
    {{:https://www.rfc-editor.org/rfc/rfc9553.html#section-2.8.3} RFC 9553
     Section 2.8.3}. *)
module Note : sig
  (** The authors of notes.

      An Author names the person or system that wrote a note, by name, by URI,
      or both. At least one property other than [@type] must be set. *)
  module Author : sig
    type t = {
      name : string option;  (** The name of the author. *)
      uri : string option;  (** The URI that identifies the author. *)
      unknown : Jscontact_unknown.t;  (** The members no property above names. *)
    }
    (** The type for an author. *)

    val make :
      ?name:string -> ?uri:string -> ?unknown:Jscontact_unknown.t -> unit -> t
    (** [make ()] is an author with the given properties. Every optional
        argument defaults to the property being unset. *)

    val equal : t -> t -> bool
    (** [equal a b] is [true] if [a] and [b] have the same properties. *)

    val pp : Format.formatter -> t -> unit
    (** [pp ppf a] formats [a] on [ppf]. *)

    val validate : t -> t Jscontact_valid.t
    (** [validate a] checks that [a] sets at least one property other than
        [@type], counting an unknown or vendor-specific member as one, checks
        its [uri] with {!Jscontact.Uri.validate}, and checks its unknown
        members. *)

    val jsont : t Jsont.t
    (** [jsont] is the codec for an Author. *)
  end

  type t = {
    note : string;  (** The free-text value of the note. *)
    created : Jscontact_date.Utc.t option;
        (** The date and time when the note was created. *)
    author : Author.t option;  (** The author of the note. *)
    unknown : Jscontact_unknown.t;  (** The members no property above names. *)
  }
  (** The type for a note. *)

  val make :
    ?created:Jscontact_date.Utc.t ->
    ?author:Author.t ->
    ?unknown:Jscontact_unknown.t ->
    string ->
    t
  (** [make note] is the note whose free-text value is [note], with the given
      properties. Every optional argument defaults to the property being unset.
  *)

  val equal : t -> t -> bool
  (** [equal a b] is [true] if [a] and [b] have the same properties. *)

  val pp : Format.formatter -> t -> unit
  (** [pp ppf n] formats [n] on [ppf]. *)

  val validate : t -> t Jscontact_valid.t
  (** [validate n] checks the unknown members of [n] and recurses into its
      author. *)

  val jsont : t Jsont.t
  (** [jsont] is the codec for a Note. *)
end

(** Personal information.

    The [personalInfo] property of a Card is an [Id[PersonalInfo]] map, as
    defined by
    {{:https://www.rfc-editor.org/rfc/rfc9553.html#section-2.8.4} RFC 9553
     Section 2.8.4}. *)
module Personal_info : sig
  (** The kinds of personal information.

      The enumerated values of the [kind] property of a PersonalInfo. *)
  module Kind : sig
    type t =
      [ `Expertise  (** A field of expertise or a credential. *)
      | `Hobby  (** A hobby. *)
      | `Interest  (** An interest. *)
      | `Vendor of string  (** A vendor-specific kind. *) ]
    (** The type for the kind of personal information. *)

    include Jscontact_enum.S with type t := t
  end

  (** The levels of expertise or engagement.

      The enumerated values of the [level] property of a PersonalInfo. *)
  module Level : sig
    type t =
      [ `High
      | `Medium
      | `Low
      | `Vendor of string  (** A vendor-specific level. *) ]
    (** The type for a level. *)

    include Jscontact_enum.S with type t := t
  end

  type t = {
    kind : Kind.t;  (** The kind of personal information. *)
    value : string;  (** The actual information. *)
    level : Level.t option;
        (** The level of expertise or of engagement in the hobby or interest. *)
    list_as : int option;
        (** The position of the information in the list of all PersonalInfo
            objects of the same kind in the Card. If set, it must be greater
            than zero. Entries may share a value or have none. How entries that
            do are then sorted is implementation-specific. *)
    label : string option;  (** A custom label for the information. *)
    unknown : Jscontact_unknown.t;  (** The members no property above names. *)
  }
  (** The type for personal information. *)

  val make :
    ?level:Level.t ->
    ?list_as:int ->
    ?label:string ->
    ?unknown:Jscontact_unknown.t ->
    Kind.t ->
    string ->
    t
  (** [make kind value] is the personal information [value] of kind [kind], with
      the given properties. Every optional argument defaults to the property
      being unset. *)

  val equal : t -> t -> bool
  (** [equal a b] is [true] if [a] and [b] have the same properties. *)

  val pp : Format.formatter -> t -> unit
  (** [pp ppf i] formats [i] on [ppf]. *)

  val validate : t -> t Jscontact_valid.t
  (** [validate i] checks the kind and level of [i], that its [list_as] is
      greater than zero, and its unknown members. *)

  val jsont : t Jsont.t
  (** [jsont] is the codec for a PersonalInfo. *)
end
