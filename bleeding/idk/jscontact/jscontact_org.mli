@@ portable

(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Organizations, titles and forms of address.

    The Card properties defined here say where the entity works and how to speak
    to it. They are [organizations] (Section 2.2.3), [speakToAs] (Section 2.2.4)
    and [titles] (Section 2.2.5).

    @canonical Jscontact.Org *)

(** Organizations.

    An Organization is a company or organization associated with the Card,
    optionally divided into units, as defined by
    {{:https://www.rfc-editor.org/rfc/rfc9553.html#section-2.2.3} RFC 9553
     Section 2.2.3}. The [organizations] property of a Card is an
    [Id[Organization]] map. *)
module Organization : sig
  (** Organizational units.

      An OrgUnit is one level of the division of an organization, as defined by
      {{:https://www.rfc-editor.org/rfc/rfc9553.html#section-2.2.3} RFC 9553
       Section 2.2.3}. *)
  module Org_unit : sig
    type t = {
      name : string;  (** The name of the organizational unit. *)
      sort_as : string option;
          (** The verbatim string to compare when sorting the unit against other
              units of the same level. The level is the index of the unit in the
              [units] of its Organization. If unset, sorting may use [name]. *)
      unknown : Jscontact_unknown.t;  (** The members no property above names. *)
    }
    (** The type for an organizational unit. *)

    val make : ?sort_as:string -> ?unknown:Jscontact_unknown.t -> string -> t
    (** [make name] is the unit named [name] with the given properties. Every
        optional argument defaults to the property being unset. *)

    val equal : t -> t -> bool
    (** [equal a b] is [true] if [a] and [b] have the same properties. *)

    val pp : Format.formatter -> t -> unit
    (** [pp ppf u] formats [u] on [ppf]. *)

    val validate : t -> t Jscontact_valid.t
    (** [validate u] checks the properties of [u] against Section 2.2.3. *)

    val jsont : t Jsont.t
    (** [jsont] is the codec for an OrgUnit. *)
  end

  type t = {
    name : string option;  (** The name of the organization. *)
    units : Org_unit.t list option;
        (** The organizational units, ordered as descending by hierarchy, so
            that a division sorts before a department within it. If set, the
            list has at least one entry. *)
    sort_as : string option;
        (** The verbatim string to compare when sorting the organization against
            other organizations by name. In absence of this property, [name] may
            be used. *)
    contexts : Jscontact_context.t list option;
        (** The contexts in which association with the organization applies.
            Membership in a choir, for one, may apply in a private context
            alone. *)
    unknown : Jscontact_unknown.t;  (** The members no property above names. *)
  }
  (** The type for an organization. At least one of [name] and [units] must be
      set. *)

  val make :
    ?name:string ->
    ?units:Org_unit.t list ->
    ?sort_as:string ->
    ?contexts:Jscontact_context.t list ->
    ?unknown:Jscontact_unknown.t ->
    unit ->
    t
  (** [make ()] is an organization with the given properties. Every optional
      argument defaults to the property being unset. *)

  val equal : t -> t -> bool
  (** [equal a b] is [true] if [a] and [b] have the same properties. *)

  val pp : Format.formatter -> t -> unit
  (** [pp ppf o] formats [o] on [ppf]. *)

  val validate : t -> t Jscontact_valid.t
  (** [validate o] checks that [o] sets at least one of [name] and [units], that
      its [units], if set, has at least one entry, and the properties Section
      2.2.3 states, recursing into the units. *)

  val jsont : t Jsont.t
  (** [jsont] is the codec for an Organization. *)
end

(** Job titles.

    A Title is a job title or functional position of the entity represented by
    the Card, as defined by
    {{:https://www.rfc-editor.org/rfc/rfc9553.html#section-2.2.5} RFC 9553
     Section 2.2.5}. The [titles] property of a Card is an [Id[Title]] map. *)
module Title : sig
  (** The kind of a title.

      Section 2.2.5 distinguishes a title, an organizational position, from a
      role, a more temporary assignment such as in project management. *)
  module Kind : sig
    type t =
      [ `Title  (** An organizational position. *)
      | `Role  (** A situational assignment. *)
      | `Vendor of string  (** A vendor-specific kind. *) ]
    (** The type for the kind of a title. *)

    include Jscontact_enum.S with type t := t
  end

  type t = {
    name : string;
        (** The title or role name of the entity represented by the Card. *)
    kind : Kind.t;
        (** The organizational or situational kind of the title. Section 2.2.5
            defaults this property to [`Title]. *)
    organization_id : Jscontact_id.t option;
        (** The key, in the [organizations] property of the Card, of the
            organization in which this title is held. *)
    unknown : Jscontact_unknown.t;  (** The members no property above names. *)
  }
  (** The type for a title. *)

  val make :
    ?kind:Kind.t ->
    ?organization_id:Jscontact_id.t ->
    ?unknown:Jscontact_unknown.t ->
    string ->
    t
  (** [make name] is the title named [name] with the given properties. [kind]
      defaults to [`Title]. [organization_id] defaults to unset. *)

  val equal : t -> t -> bool
  (** [equal a b] is [true] if [a] and [b] have the same properties. *)

  val pp : Format.formatter -> t -> unit
  (** [pp ppf t] formats [t] on [ppf]. *)

  val validate : t -> t Jscontact_valid.t
  (** [validate t] checks the properties of [t] against Section 2.2.5. It does
      not resolve [organization_id]. Whether the Card has an organization of
      that key is a rule of the Card, not of the Title. *)

  val jsont : t Jsont.t
  (** [jsont] is the codec for a Title. The [kind] member is omitted when it is
      the default [`Title]. *)
end

(** Forms of address.

    A SpeakToAs directs how to address, speak to, or refer to the entity
    represented by the Card, as defined by
    {{:https://www.rfc-editor.org/rfc/rfc9553.html#section-2.2.4} RFC 9553
     Section 2.2.4}. The [speakToAs] property of a Card is one such object. *)
module Speak_to_as : sig
  (** Grammatical genders.

      Section 2.2.4 uses the grammatical gender in salutations and other
      grammatical constructs, such as the German "Sehr geehrte" and "Sehr
      geehrter". It does not allow inferring the gender identity or assigned sex
      of the contact. *)
  module Grammatical_gender : sig
    type t =
      [ `Animate  (** The animate gender. *)
      | `Common  (** The common gender. *)
      | `Feminine  (** The feminine gender. *)
      | `Inanimate  (** The inanimate gender. *)
      | `Masculine  (** The masculine gender. *)
      | `Neuter  (** The neuter gender. *)
      | `Vendor of string  (** A vendor-specific gender. *) ]
    (** The type for a grammatical gender. *)

    include Jscontact_enum.S with type t := t
  end

  (** Pronouns.

      A Pronouns object holds pronouns the contact chooses to use for itself, as
      defined by
      {{:https://www.rfc-editor.org/rfc/rfc9553.html#section-2.2.4} RFC 9553
       Section 2.2.4}. *)
  module Pronouns : sig
    type t = {
      pronouns : string;
          (** The pronouns. Any value or form is allowed. English examples
              include ["she/her"] and ["they/them/theirs"]. The value may be
              overridden in the [localizations] property of the Card. *)
      contexts : Jscontact_context.t list option;
          (** The contexts in which to use the pronouns. *)
      pref : int option;
          (** The preference of the pronouns in relation to other pronouns in
              the same context, in the range 1 to 100, 1 being most preferred.
          *)
      unknown : Jscontact_unknown.t;  (** The members no property above names. *)
    }
    (** The type for pronouns. *)

    val make :
      ?contexts:Jscontact_context.t list ->
      ?pref:int ->
      ?unknown:Jscontact_unknown.t ->
      string ->
      t
    (** [make pronouns] is the pronouns [pronouns] with the given properties.
        Every optional argument defaults to the property being unset. *)

    val equal : t -> t -> bool
    (** [equal a b] is [true] if [a] and [b] have the same properties. *)

    val pp : Format.formatter -> t -> unit
    (** [pp ppf p] formats [p] on [ppf]. *)

    val validate : t -> t Jscontact_valid.t
    (** [validate p] checks the [contexts] and [pref] of [p] against Sections
        1.5.1 and 1.5.3. *)

    val jsont : t Jsont.t
    (** [jsont] is the codec for a Pronouns object. *)
  end

  type t = {
    grammatical_gender : Grammatical_gender.t option;
        (** The grammatical gender to use in salutations and other grammatical
            constructs. *)
    pronouns : (Jscontact_id.t * Pronouns.t) list option;
        (** The pronouns the contact chooses to use for itself, keyed by Id. *)
    unknown : Jscontact_unknown.t;  (** The members no property above names. *)
  }
  (** The type for a form of address. At least one of [grammatical_gender] and
      [pronouns] must be set. *)

  val make :
    ?grammatical_gender:Grammatical_gender.t ->
    ?pronouns:(Jscontact_id.t * Pronouns.t) list ->
    ?unknown:Jscontact_unknown.t ->
    unit ->
    t
  (** [make ()] is a form of address with the given properties. Every optional
      argument defaults to the property being unset. *)

  val equal : t -> t -> bool
  (** [equal a b] is [true] if [a] and [b] have the same properties. *)

  val pp : Format.formatter -> t -> unit
  (** [pp ppf s] formats [s] on [ppf]. *)

  val validate : t -> t Jscontact_valid.t
  (** [validate s] checks that [s] sets at least one of [grammatical_gender] and
      [pronouns], and the properties Section 2.2.4 states, recursing into the
      pronouns. *)

  val jsont : t Jsont.t
  (** [jsont] is the codec for a SpeakToAs. *)
end
