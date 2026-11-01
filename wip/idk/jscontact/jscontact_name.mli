@@ portable

(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Names and nicknames.

    A Card names the entity it represents with a Name object, which is a full
    name written as one string, a list of components, or both, and with a map of
    Nickname objects.

    {{:https://www.rfc-editor.org/rfc/rfc9553.html#section-2.2.1} RFC 9553
     Section 2.2.1} defines the [name] property and
    {{:https://www.rfc-editor.org/rfc/rfc9553.html#section-2.2.2} Section 2.2.2}
    the [nicknames] property.

    @canonical Jscontact.Name *)

(** Name components.

    A NameComponent is one part of a name, such as a given name or a surname, as
    defined by
    {{:https://www.rfc-editor.org/rfc/rfc9553.html#section-2.2.1.2} RFC 9553
     Section 2.2.1.2}. *)
module Component : sig
  (** The kind of a name component.

      The values are those Section 2.2.1.2 enumerates for the [kind] property.
      {{:https://www.rfc-editor.org/rfc/rfc9553.html#section-1.7.5} Section
       1.7.5} also admits a vendor-specific value. *)
  module Kind : sig
    type t =
      [ `Title
        (** An honorific title or prefix, such as ["Mr."], ["Ms."] or ["Dr."].
        *)
      | `Given  (** A given name, also known as a first or personal name. *)
      | `Given2
        (** A name between the given name and the surname, such as a middle or
            patronymic name. *)
      | `Surname  (** A surname, also known as a last or family name. *)
      | `Surname2
        (** A secondary surname, also known as a maternal surname, used in some
            cultures. *)
      | `Credential
        (** A credential, also known as an accreditation qualifier or honorific
            suffix, such as ["B.A."] or ["Esq."]. *)
      | `Generation
        (** A generation marker or qualifier, such as ["Jr."] or ["III"]. *)
      | `Separator
        (** A formatting separator between two ordered non-separator components.
            The [value] of the component is the verbatim separator, which may be
            the empty string, and takes precedence over the [default_separator]
            of the name. Two separator components must not follow one another. A
            single one holding the combined value is used instead. This kind
            must not be set if the name is not ordered. *)
      | `Vendor of string  (** A vendor-specific kind. *) ]
    (** The type for the kind of a name component. *)

    include Jscontact_enum.S with type t := t
  end

  type t = {
    kind : Kind.t;  (** The kind of the name component. *)
    value : string;
        (** The value of the name component. It can be one word or several, such
            as ["Poe"] or ["van Gogh"]. *)
    phonetic : string option;
        (** The pronunciation of the name component. If it is set, then at least
            one of the [phonetic_script] and [phonetic_system] properties of the
            name holding the component must be set. See
            {{:https://www.rfc-editor.org/rfc/rfc9553.html#section-1.5.4}
             Section 1.5.4}. *)
    unknown : Jscontact_unknown.t;  (** The members no property above names. *)
  }
  (** The type for a name component, as defined by
      {{:https://www.rfc-editor.org/rfc/rfc9553.html#section-2.2.1.2} Section
       2.2.1.2}. *)

  val make :
    ?phonetic:string -> ?unknown:Jscontact_unknown.t -> Kind.t -> string -> t
  (** [make kind value] is the component of kind [kind] whose value is [value].
      Every optional argument defaults to the property being unset. *)

  val equal : t -> t -> bool
  (** [equal a b] is [true] if [a] and [b] have the same properties. *)

  val pp : Format.formatter -> t -> unit
  (** [pp ppf c] formats [c] on [ppf]. *)

  val validate : t -> t Jscontact_valid.t
  (** [validate c] checks the rules Section 2.2.1.2 states of [c] on its own.
      The two rules that relate a component to the name holding it, that a
      [phonetic] requires a phonetic script or system and that a [`Separator]
      requires an ordered name, are checked by {!Jscontact.Name.validate}. *)

  val jsont : t Jsont.t
  (** [jsont] is the codec for a NameComponent. *)
end

(** Nicknames.

    A Nickname is one of the nicknames of the entity a Card represents, as
    defined by
    {{:https://www.rfc-editor.org/rfc/rfc9553.html#section-2.2.2} RFC 9553
     Section 2.2.2}. The [nicknames] property of a Card is a map from
    {!Jscontact.Id.t} to values of this type. *)
module Nickname : sig
  type t = {
    name : string;  (** The nickname. *)
    contexts : Jscontact_context.t list option;
        (** The contexts in which to use the nickname. See
            {{:https://www.rfc-editor.org/rfc/rfc9553.html#section-1.5.1}
             Section 1.5.1}. *)
    pref : int option;
        (** The preference of the nickname in relation to other nicknames. See
            {{:https://www.rfc-editor.org/rfc/rfc9553.html#section-1.5.3}
             Section 1.5.3}. *)
    unknown : Jscontact_unknown.t;  (** The members no property above names. *)
  }
  (** The type for a nickname. *)

  val make :
    ?contexts:Jscontact_context.t list ->
    ?pref:int ->
    ?unknown:Jscontact_unknown.t ->
    string ->
    t
  (** [make name] is the nickname [name] with the given properties. Every
      optional argument defaults to the property being unset. *)

  val equal : t -> t -> bool
  (** [equal a b] is [true] if [a] and [b] have the same properties. *)

  val pp : Format.formatter -> t -> unit
  (** [pp ppf n] formats [n] on [ppf]. *)

  val validate : t -> t Jscontact_valid.t
  (** [validate n] checks the [contexts] and [pref] of [n], which Sections 1.5.1
      and 1.5.3 define. *)

  val jsont : t Jsont.t
  (** [jsont] is the codec for a Nickname. *)
end

type t = {
  components : Component.t list option;
      (** The components making up this name. It must be set if [full] is not
          set, and should be set otherwise. At least one component must have a
          kind other than [`Separator]. Components should be ordered so that
          joining their values yields a valid full name of the entity. If they
          are, [is_ordered] must be [true]. *)
  is_ordered : bool;
      (** Whether the components are ordered. Defaults to [false]. *)
  default_separator : string option;
      (** The separator to insert between component values when joining them
          into a single string. A [`Separator] component takes precedence over
          it. It must not be set if [is_ordered] is [false] or if [components]
          is not set. *)
  full : string option;
      (** The full name, such as ["Mr. John Q. Public, Esq."]. It must be set if
          [components] is not set. *)
  sort_as : (Component.Kind.t * string) list option;
      (** The values to sort this name by, keyed by the kind of component sorted
          on. A kind absent from the map should not be sorted on, and sorting by
          a missing kind, or when the property is not set at all, is
          implementation-specific. Each key must be a kind that some entry of
          [components] has, and the property must not be set if [components] is
          not set. *)
  phonetic_script : string option;
      (** The script the [phonetic] of a component is written in, a script
          subtag as defined by
          {{:https://www.rfc-editor.org/rfc/rfc5646.html#section-2.2.3} RFC 5646
           Section 2.2.3}. See
          {{:https://www.rfc-editor.org/rfc/rfc9553.html#section-1.5.4} Section
           1.5.4}. *)
  phonetic_system : Jscontact_phonetic.t option;
      (** The phonetic system the [phonetic] of a component is written in. *)
  unknown : Jscontact_unknown.t;  (** The members no property above names. *)
}
(** The type for a name, as defined by
    {{:https://www.rfc-editor.org/rfc/rfc9553.html#section-2.2.1.1} RFC 9553
     Section 2.2.1.1}. *)

val make :
  ?components:Component.t list ->
  ?is_ordered:bool ->
  ?default_separator:string ->
  ?full:string ->
  ?sort_as:(Component.Kind.t * string) list ->
  ?phonetic_script:string ->
  ?phonetic_system:Jscontact_phonetic.t ->
  ?unknown:Jscontact_unknown.t ->
  unit ->
  t
(** [make ()] is a name with the given properties. Every optional argument
    defaults to the property being unset, except [is_ordered], which defaults to
    [false] as Section 2.2.1.1 does. *)

val equal : t -> t -> bool
(** [equal a b] is [true] if [a] and [b] have the same properties. *)

val pp : Format.formatter -> t -> unit
(** [pp ppf n] formats [n] on [ppf]. *)

val validate : t -> t Jscontact_valid.t
(** [validate n] checks the rules Section 2.2.1.1 states of [n], together with
    the rules of Section 2.2.1.2 that relate a component to the name holding it.
    It checks that [full] is set if [components] is not set. It checks that
    [default_separator] is not set if [components] is not set, or if
    [is_ordered] is [false]. When [components] is set, it checks that at least
    one component has a kind other than [`Separator], that no two [`Separator]
    components are consecutive, that a [`Separator] component is set only if
    [is_ordered] is [true], and that a component with a [phonetic] set has
    [phonetic_script] or [phonetic_system] set on [n]. It checks that each key
    of [sort_as] is a kind some entry of [components] has, and that [sort_as] is
    not set if [components] is not set. It recurses into each component of
    [components]. *)

val jsont : t Jsont.t
(** [jsont] is the codec for a Name. The [isOrdered] member is written only when
    it is [true]. Section 2.2.1.1 defaults it to [false]. *)
