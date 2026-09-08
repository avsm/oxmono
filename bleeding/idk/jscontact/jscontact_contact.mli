@@ portable

(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Contact properties.

    {{:https://www.rfc-editor.org/rfc/rfc9553.html#section-2.3} RFC 9553 Section
     2.3} defines the properties by which the entity a Card represents is
    contacted. They are the email addresses of the [emails] property, the online
    services of the [onlineServices] property, the phone numbers of the [phones]
    property, and the languages of the [preferredLanguages] property. Each of
    those Card properties is a map from {!Jscontact.Id.t} to one of the object
    types defined here.

    @canonical Jscontact.Contact *)

(** Email addresses.

    An EmailAddress object is a value of the [emails] property of a Card, as
    defined by
    {{:https://www.rfc-editor.org/rfc/rfc9553.html#section-2.3.1} RFC 9553
     Section 2.3.1}. *)
module Email_address : sig
  type t = {
    address : string;
        (** The email address, an [addr-spec] as defined by
            {{:https://www.rfc-editor.org/rfc/rfc5322.html#section-3.4.1} RFC
             5322 Section 3.4.1}. Mandatory. *)
    contexts : Jscontact_context.t list option;
        (** The contexts in which to use this email address. *)
    pref : int option;
        (** The preference of this email address in relation to the other email
            addresses of the Card, 1 to 100, 1 being most preferred. *)
    label : string option;  (** A custom label for the address. *)
    unknown : Jscontact_unknown.t;  (** The members no property above names. *)
  }
  (** The type for an email address. *)

  val make :
    ?contexts:Jscontact_context.t list ->
    ?pref:int ->
    ?label:string ->
    ?unknown:Jscontact_unknown.t ->
    string ->
    t
  (** [make address] is the email address [address] with the given properties.
      Every optional argument defaults to the property being unset. *)

  val equal : t -> t -> bool
  (** [equal a b] is [true] if [a] and [b] have the same properties. *)

  val pp : Format.formatter -> t -> unit
  (** [pp ppf e] formats [e] on [ppf] by its [address]. *)

  val validate : t -> t Jscontact_valid.t
  (** [validate e] checks the [address], [contexts] and [pref] of [e] and the
      names of its unknown members. The [address] check is syntactic and loose.
      It asks for a non-empty local part and domain either side of the last
      commercial at, and for no whitespace or control character, rather than the
      whole of the {{:https://www.rfc-editor.org/rfc/rfc5322.html} RFC 5322}
      [addr-spec] grammar. *)

  val jsont : t Jsont.t
  (** [jsont] is the codec for an EmailAddress, as defined by
      {{:https://www.rfc-editor.org/rfc/rfc9553.html#section-2.3.1} Section
       2.3.1}. Decoding errors on an absent [address] member. *)
end

(** Online services.

    An OnlineService object is a value of the [onlineServices] property of a
    Card, as defined by
    {{:https://www.rfc-editor.org/rfc/rfc9553.html#section-2.3.2} RFC 9553
     Section 2.3.2}. It records a messaging service, a social media profile or
    another service at which the entity the Card represents has an identity. *)
module Online_service : sig
  type t = {
    service : string option;
        (** The name of the online service or protocol, such as ["GitHub"] or
            ["Mastodon"]. It may be capitalized as the service does, but two
            names that match case-insensitively are the same service. *)
    uri : string option;
        (** The identifier of the entity at the service, a URI as defined by
            {{:https://www.rfc-editor.org/rfc/rfc3986.html#section-3} RFC 3986
             Section 3}. *)
    user : string option;
        (** The name of the entity at the service, any free text. The [service]
            property should be set alongside it. *)
    contexts : Jscontact_context.t list option;
        (** The contexts in which to use the service. *)
    pref : int option;
        (** The preference of this service in relation to the other services of
            the Card, 1 to 100, 1 being most preferred. *)
    label : string option;  (** A custom label for the service. *)
    unknown : Jscontact_unknown.t;  (** The members no property above names. *)
  }
  (** The type for an online service. At least one of [uri] and [user] must be
      set. *)

  val make :
    ?service:string ->
    ?uri:string ->
    ?user:string ->
    ?contexts:Jscontact_context.t list ->
    ?pref:int ->
    ?label:string ->
    ?unknown:Jscontact_unknown.t ->
    unit ->
    t
  (** [make ()] is an online service with the given properties. Every optional
      argument defaults to the property being unset. *)

  val equal : t -> t -> bool
  (** [equal a b] is [true] if [a] and [b] have the same properties. *)

  val pp : Format.formatter -> t -> unit
  (** [pp ppf s] formats [s] on [ppf] by its [uri], or by its [user] if it has
      no [uri]. *)

  val validate : t -> t Jscontact_valid.t
  (** [validate s] checks that at least one of the [uri] and [user] properties
      of [s] is set, which Section 2.3.2 requires, and checks its [uri],
      [contexts] and [pref] and the names of its unknown members. The [uri]
      check is syntactic. It asks for an
      {{:https://www.rfc-editor.org/rfc/rfc3986.html} RFC 3986} scheme and a
      colon and does not parse what follows. *)

  val jsont : t Jsont.t
  (** [jsont] is the codec for an OnlineService, as defined by
      {{:https://www.rfc-editor.org/rfc/rfc9553.html#section-2.3.2} Section
       2.3.2}. The object has no mandatory member. That one of [uri] and [user]
      is set is left to {!validate}. *)
end

(** Phone numbers.

    A Phone object is a value of the [phones] property of a Card, as defined by
    {{:https://www.rfc-editor.org/rfc/rfc9553.html#section-2.3.3} RFC 9553
     Section 2.3.3}. *)
module Phone : sig
  (** The contact features a phone number may be used for.

      The values are those enumerated by
      {{:https://www.rfc-editor.org/rfc/rfc9553.html#section-2.3.3} Section
       2.3.3}.
      {{:https://www.rfc-editor.org/rfc/rfc9553.html#section-1.7.5} Section
       1.7.5} lets a vendor add values of its own. *)
  module Feature : sig
    type t =
      [ `Mobile  (** The number is for a mobile phone. *)
      | `Voice  (** The number supports calling by voice. *)
      | `Text  (** The number supports text messages (SMS). *)
      | `Video  (** The number supports video conferencing. *)
      | `Main_number
        (** The number is a main phone number, such as the front desk of a
            company, as opposed to the direct-dial number of an employee. Its
            wire spelling is ["main-number"]. *)
      | `Textphone
        (** The number is for a device for people with hearing or speech
            difficulties. *)
      | `Fax  (** The number supports sending faxes. *)
      | `Pager  (** The number is for a pager or beeper. *)
      | `Vendor of string  (** A vendor-specific feature. *) ]
    (** The type for a phone feature. *)

    include Jscontact_enum.S with type t := t
  end

  type t = {
    number : string;
        (** The phone number, either a URI, typically in the ["tel"] or ["sip"]
            scheme, or free text. Mandatory. *)
    features : Feature.t list option;
        (** The contact features the number may be used for. *)
    contexts : Jscontact_context.t list option;
        (** The contexts in which to use the number. *)
    pref : int option;
        (** The preference of this number in relation to the other numbers of
            the Card, 1 to 100, 1 being most preferred. *)
    label : string option;  (** A custom label for the number. *)
    unknown : Jscontact_unknown.t;  (** The members no property above names. *)
  }
  (** The type for a phone number. *)

  val make :
    ?features:Feature.t list ->
    ?contexts:Jscontact_context.t list ->
    ?pref:int ->
    ?label:string ->
    ?unknown:Jscontact_unknown.t ->
    string ->
    t
  (** [make number] is the phone number [number] with the given properties.
      Every optional argument defaults to the property being unset. *)

  val equal : t -> t -> bool
  (** [equal a b] is [true] if [a] and [b] have the same properties. *)

  val pp : Format.formatter -> t -> unit
  (** [pp ppf p] formats [p] on [ppf] by its [number]. *)

  val validate : t -> t Jscontact_valid.t
  (** [validate p] checks the [features], [contexts] and [pref] of [p] and the
      names of its unknown members. Section 2.3.3 places no grammar constraint
      on [number]. It may be free text. *)

  val jsont : t Jsont.t
  (** [jsont] is the codec for a Phone, as defined by
      {{:https://www.rfc-editor.org/rfc/rfc9553.html#section-2.3.3} Section
       2.3.3}. Decoding errors on an absent [number] member and on a [features]
      member that maps a feature to [false]. *)
end

(** Preferred languages.

    A LanguagePref object is a value of the [preferredLanguages] property of a
    Card, as defined by
    {{:https://www.rfc-editor.org/rfc/rfc9553.html#section-2.3.4} RFC 9553
     Section 2.3.4}. *)
module Language_pref : sig
  type t = {
    language : string;
        (** The preferred language, a language tag as defined by
            {{:https://www.rfc-editor.org/rfc/rfc5646.html} RFC 5646}.
            Mandatory. *)
    contexts : Jscontact_context.t list option;
        (** The contexts in which to use the language. *)
    pref : int option;
        (** The preference of this language in relation to the other languages
            of the same contexts, 1 to 100, 1 being most preferred. *)
    unknown : Jscontact_unknown.t;  (** The members no property above names. *)
  }
  (** The type for a language preference. *)

  val make :
    ?contexts:Jscontact_context.t list ->
    ?pref:int ->
    ?unknown:Jscontact_unknown.t ->
    string ->
    t
  (** [make language] is the preference for [language] with the given
      properties. Every optional argument defaults to the property being unset.
  *)

  val equal : t -> t -> bool
  (** [equal a b] is [true] if [a] and [b] have the same properties. *)

  val pp : Format.formatter -> t -> unit
  (** [pp ppf l] formats [l] on [ppf] by its [language]. *)

  val validate : t -> t Jscontact_valid.t
  (** [validate l] checks the [language] of [l] with
      {!Jscontact.Language.validate}, checks its [contexts] and [pref], and
      checks the names of its unknown members. *)

  val jsont : t Jsont.t
  (** [jsont] is the codec for a LanguagePref, as defined by
      {{:https://www.rfc-editor.org/rfc/rfc9553.html#section-2.3.4} Section
       2.3.4}. Decoding errors on an absent [language] member. Section 2.3.4
      defines no [label] property for this type. *)
end
