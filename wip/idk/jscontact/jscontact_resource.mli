@@ portable

(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Resources.

    {{:https://www.rfc-editor.org/rfc/rfc9553.html#section-1.4.4} RFC 9553
     Section 1.4.4} defines the Resource data type. A resource is associated
    with the entity the Card represents and is identified by a URI. Resource is
    not itself a JSContact object type. It is the set of properties that the
    concrete resource types of
    {{:https://www.rfc-editor.org/rfc/rfc9553.html#section-2.6} Section 2.6}
    share. {!type-base} defines that set once, and the four types of Section 2.6
    build on it. {!Jscontact.Calendar} defines the fifth.

    @canonical Jscontact.Resource *)

(** {1:base The Resource properties} *)

type 'kind base = {
  kind : 'kind;
      (** The kind of the resource. Each concrete type enumerates its own values
          and states whether the property is mandatory. The field is an
          enumeration for a type that makes the property mandatory, and an
          option for a type that leaves it optional. *)
  uri : string option;
      (** The resource value, a URI as defined by
          {{:https://www.rfc-editor.org/rfc/rfc3986.html#section-3} RFC 3986
           Section 3}.

          Section 1.4.4 makes the property mandatory and {!val-validate}
          enforces it. The codec leaves it optional.
          {{:https://www.rfc-editor.org/rfc/rfc9610.html#section-3} RFC 9610
           Section 3} has a JMAP server return a Media with a [blobId] and no
          [uri], in place of the [data:] URI it stands for, and such a Media
          still decodes. *)
  media_type : string option;
      (** The media type of the resource the [uri] identifies, as defined by
          {{:https://www.rfc-editor.org/rfc/rfc2046.html} RFC 2046}. *)
  contexts : Jscontact_context.t list option;
      (** The contexts in which to use the resource. *)
  pref : int option;
      (** The preference of the resource in relation to other resources of the
          same property, from 1 to 100. *)
  label : string option;  (** A custom label for the resource. *)
  unknown : Jscontact_unknown.t;  (** The members no property above names. *)
}
(** The type for the properties Section 1.4.4 gives every resource, with the
    kind left to the concrete type ['kind]. A resource never carries the [@type]
    value ["Resource"]. Section 1.4.4 requires the name of the concrete type
    instead. *)

val make :
  ?uri:string ->
  ?media_type:string ->
  ?contexts:Jscontact_context.t list ->
  ?pref:int ->
  ?label:string ->
  ?unknown:Jscontact_unknown.t ->
  'kind ->
  'kind base
(** [make kind] is the resource of kind [kind] with the given properties. *)

val equal : ('kind -> 'kind -> bool) -> 'kind base -> 'kind base -> bool
(** [equal eq a b] is [true] if [a] and [b] have the same properties, kinds
    compared with [eq]. *)

val pp :
  (Format.formatter -> 'kind -> unit) -> Format.formatter -> 'kind base -> unit
(** [pp pp_kind ppf r] formats [r] on [ppf], using [pp_kind] to format the kind,
    followed by the uri. *)

val validate :
  type_name:string ->
  kind:('kind -> 'kind Jscontact_valid.t) ->
  'kind base ->
  'kind base Jscontact_valid.t
(** [validate ~type_name ~kind r] checks the properties Section 1.4.4 gives
    every resource. The kind is checked with [kind], the uri is required and
    checked with {!Jscontact.Uri.validate}, the contexts with
    {!Jscontact.Context.validate_set}, the preference with
    {!Jscontact.Pref.validate} and the unknown members with
    {!Jscontact.Unknown.validate}. [type_name] names the concrete type in the
    error message. A type that adds properties of its own checks them itself. *)

val mems :
  base:('o -> 'kind base) @ portable ->
  ( 'o,
    string option ->
    string option ->
    Jscontact_context.t list option ->
    int option ->
    string option ->
    'a )
  Jsont.Object.map ->
  ('o, 'a) Jsont.Object.map
(** [mems ~base map] adds to [map] the [uri], [mediaType], [contexts], [pref]
    and [label] members of Section 1.4.4, in that order, and takes them in that
    order from the constructor of [map]. [base] views the object being coded as
    a resource. It is [Fun.id] for a type that is a {!type-base} and projects
    out the Resource properties for a type that is not.

    [mems] does not add the [kind] member. Section 1.4.4 leaves its values and
    its mandatoriness to each concrete type, so a codec that uses [mems] adds
    [kind] to [map] before [mems], preceded by [@type] added with
    {!Jscontact.Json.type_mem}. [mems] does not add the unknown members either,
    so a codec that uses it can add a property of its own, such as the [listAs]
    of a {!Directory.t}, after [label], and ends with
    [Jsont.Object.keep_unknown] and [Jsont.Object.finish]. *)

(** {1:types Resource types} *)

(** Cryptographic resources.

    {{:https://www.rfc-editor.org/rfc/rfc9553.html#section-2.6.1} RFC 9553
     Section 2.6.1} defines the CryptoKey object, the value type of the
    [cryptoKeys] property of a Card. A CryptoKey is a public key or certificate,
    referred to by its URI or embedded in a [data:] URI. *)
module Crypto_key : sig
  type t = string option base
  (** The type for a cryptographic resource. Section 2.6.1 adds no kind values
      to those of Section 1.4.4, which are none. The kind is therefore an
      unconstrained optional string. *)

  val make :
    ?kind:string ->
    ?uri:string ->
    ?media_type:string ->
    ?contexts:Jscontact_context.t list ->
    ?pref:int ->
    ?label:string ->
    ?unknown:Jscontact_unknown.t ->
    unit ->
    t
  (** [make ()] is a cryptographic resource with the given properties. *)

  val equal : t -> t -> bool
  (** [equal a b] is [true] if [a] and [b] have the same properties. *)

  val pp : Format.formatter -> t -> unit
  (** [pp ppf k] formats [k] on [ppf]. *)

  val validate : t -> t Jscontact_valid.t
  (** [validate k] is {!Jscontact.Resource.validate} on [k]. Section 2.6.1 adds
      no rule of its own. *)

  val jsont : t Jsont.t
  (** [jsont] is the codec for a CryptoKey. *)
end

(** Directories.

    {{:https://www.rfc-editor.org/rfc/rfc9553.html#section-2.6.2} RFC 9553
     Section 2.6.2} defines the Directory object, the value type of the
    [directories] property of a Card. A Directory is a directory service the
    entity is part of, or the entry of the entity within one. *)
module Directory : sig
  (** The kinds of directory. *)
  module Kind : sig
    type t =
      [ `Directory
        (** A directory service the entity represented by the Card is a part of,
            typically an organizational directory that also holds associated
            entities. *)
      | `Entry
        (** The directory entry of the entity represented by the Card, that is
            its specific URI within a directory. *)
      | `Vendor of string  (** A vendor-specific kind. *) ]
    (** The type for the kind of a directory. *)

    include Jscontact_enum.S with type t := t
  end

  type t = {
    kind : Kind.t;  (** The kind of the directory. Mandatory. *)
    uri : string option;
        (** The resource value, a URI. See {!type-base} for why it is optional.
        *)
    media_type : string option;  (** The media type of the resource. *)
    contexts : Jscontact_context.t list option;
        (** The contexts in which to use the resource. *)
    pref : int option;
        (** The preference of the resource in relation to other directories. *)
    label : string option;  (** A custom label for the resource. *)
    list_as : int option;
        (** The position of the resource in the list of the directories of the
            Card that have the same kind. If set, it must be higher than zero.
            Directories may share a value or have none. How directories that do
            are then ordered is implementation-specific. *)
    unknown : Jscontact_unknown.t;  (** The members no property above names. *)
  }
  (** The type for a directory. Section 2.6.2 adds [listAs] to the properties
      Section 1.4.4 gives every resource, which the record names again rather
      than nesting a {!type-base}. *)

  val make :
    ?uri:string ->
    ?media_type:string ->
    ?contexts:Jscontact_context.t list ->
    ?pref:int ->
    ?label:string ->
    ?list_as:int ->
    ?unknown:Jscontact_unknown.t ->
    Kind.t ->
    t
  (** [make kind] is the directory of kind [kind] with the given properties. *)

  val equal : t -> t -> bool
  (** [equal a b] is [true] if [a] and [b] have the same properties. *)

  val pp : Format.formatter -> t -> unit
  (** [pp ppf d] formats [d] on [ppf]. *)

  val validate : t -> t Jscontact_valid.t
  (** [validate d] is {!Jscontact.Resource.validate} on the Resource properties
      of [d], and checks that its [list_as], if set, is higher than zero. *)

  val jsont : t Jsont.t
  (** [jsont] is the codec for a Directory. The [kind] member is mandatory. *)
end

(** Links.

    {{:https://www.rfc-editor.org/rfc/rfc9553.html#section-2.6.3} RFC 9553
     Section 2.6.3} defines the Link object, the value type of the [links]
    property of a Card. A Link is a resource that fits none of the other
    resource properties. *)
module Link : sig
  (** The kinds of link. *)
  module Kind : sig
    type t =
      [ `Contact
        (** A URI by which the entity represented by the Card may be contacted,
            including a web form or another medium that needs user interaction.
        *)
      | `Vendor of string  (** A vendor-specific kind. *) ]
    (** The type for the kind of a link. *)

    include Jscontact_enum.S with type t := t
  end

  type t = Kind.t option base
  (** The type for a link. Section 2.6.3 leaves the kind optional. *)

  val make :
    ?kind:Kind.t ->
    ?uri:string ->
    ?media_type:string ->
    ?contexts:Jscontact_context.t list ->
    ?pref:int ->
    ?label:string ->
    ?unknown:Jscontact_unknown.t ->
    unit ->
    t
  (** [make ()] is a link with the given properties. *)

  val equal : t -> t -> bool
  (** [equal a b] is [true] if [a] and [b] have the same properties. *)

  val pp : Format.formatter -> t -> unit
  (** [pp ppf l] formats [l] on [ppf]. *)

  val validate : t -> t Jscontact_valid.t
  (** [validate l] is {!Jscontact.Resource.validate} on [l]. Section 2.6.3 adds
      no rule of its own. *)

  val jsont : t Jsont.t
  (** [jsont] is the codec for a Link. *)
end

(** Media resources.

    {{:https://www.rfc-editor.org/rfc/rfc9553.html#section-2.6.4} RFC 9553
     Section 2.6.4} defines the Media object, the value type of the [media]
    property of a Card. A Media is a photograph, a sound or a logo associated
    with the entity. *)
module Media : sig
  (** The kinds of media resource. *)
  module Kind : sig
    type t =
      [ `Photo  (** A photograph or avatar. *)
      | `Sound
        (** Audio media, such as the pronunciation of the contents of the name
            property. *)
      | `Logo
        (** A graphic image or logo associated with the entity represented by
            the Card. *)
      | `Vendor of string  (** A vendor-specific kind. *) ]
    (** The type for the kind of a media resource. *)

    include Jscontact_enum.S with type t := t
  end

  type t = Kind.t base
  (** The type for a media resource. Section 2.6.4 makes the kind mandatory. *)

  val make :
    ?uri:string ->
    ?media_type:string ->
    ?contexts:Jscontact_context.t list ->
    ?pref:int ->
    ?label:string ->
    ?unknown:Jscontact_unknown.t ->
    Kind.t ->
    t
  (** [make kind] is the media resource of kind [kind] with the given
      properties. *)

  val equal : t -> t -> bool
  (** [equal a b] is [true] if [a] and [b] have the same properties. *)

  val pp : Format.formatter -> t -> unit
  (** [pp ppf m] formats [m] on [ppf]. *)

  val validate : t -> t Jscontact_valid.t
  (** [validate m] is {!Jscontact.Resource.validate} on [m]. Section 2.6.4 adds
      no rule of its own. *)

  val jsont : t Jsont.t
  (** [jsont] is the codec for a Media. The [kind] member is mandatory. *)
end
