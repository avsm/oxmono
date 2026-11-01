@@ portable

(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Cards.

    A Card stores the contact information of a person, organization or company.
    It is the topmost object of JSContact data, as defined in
    {{:https://www.rfc-editor.org/rfc/rfc9553.html#section-2} RFC 9553 Section
     2}. Its media type is [application/jscontact+json].

    Every property but [version] and [uid] is optional. The repeated properties
    are maps keyed by {!Jscontact.Id.t}. A {!Jscontact.Patch.t} can then address
    one entry, and a Title can name the Organization it is held in.

    @canonical Jscontact.Card *)

(** The kind of entity a Card represents.

    {{:https://www.rfc-editor.org/rfc/rfc9553.html#section-2.1.4} Section 2.1.4}
    enumerates the values and makes ["individual"] the default. *)
module Kind : sig
  type t =
    [ `Individual  (** A single person. *)
    | `Group  (** A group of people or entities. *)
    | `Org  (** An organization. *)
    | `Location  (** A named location. *)
    | `Device  (** An appliance, a computer, or a network element. *)
    | `Application  (** A software application. *)
    | `Vendor of string  (** A vendor-specific kind. *) ]
  (** The type for the kind of a Card. *)

  include Jscontact_enum.S with type t := t
end

type t = {
  version : string;
      (** The JSContact version this Card is written against, mandatory. Section
          1.9.2 registers ["1.0"], which is {!version_1_0}. *)
  created : Jscontact_date.Utc.t option;  (** When the Card was created. *)
  kind : Kind.t;  (** The kind of entity the Card represents. *)
  language : string option;
      (** The language tag that best describes the text in the Card. *)
  members : string list option;
      (** The uids of the Cards that are members of this group Card. If set,
          {!kind} must be [`Group]. A group Card need not set it. Its members
          may be known empirically. *)
  prod_id : string option;
      (** The identifier of the product that created the Card. *)
  related_to : (string * Jscontact_info.Relation.t) list option;
      (** The Cards that relate to this one, keyed by their uid. *)
  uid : string;
      (** The identifier that associates this Card as the same across systems,
          address books and views, mandatory. It should be a URN in the "uuid"
          namespace. *)
  updated : Jscontact_date.Utc.t option;
      (** When the data in the Card was last modified. *)
  name : Jscontact_name.t option;
      (** The name of the entity the Card represents. *)
  nicknames : (Jscontact_id.t * Jscontact_name.Nickname.t) list option;
      (** The nicknames of the entity. *)
  organizations : (Jscontact_id.t * Jscontact_org.Organization.t) list option;
      (** The organizations and units associated with the Card. *)
  speak_to_as : Jscontact_org.Speak_to_as.t option;
      (** How to address, speak to, or refer to the entity. *)
  titles : (Jscontact_id.t * Jscontact_org.Title.t) list option;
      (** The job titles or functional positions of the entity. *)
  emails : (Jscontact_id.t * Jscontact_contact.Email_address.t) list option;
      (** The email addresses at which to contact the entity. *)
  online_services :
    (Jscontact_id.t * Jscontact_contact.Online_service.t) list option;
      (** The online services associated with the entity. *)
  phones : (Jscontact_id.t * Jscontact_contact.Phone.t) list option;
      (** The phone numbers at which to contact the entity. *)
  preferred_languages :
    (Jscontact_id.t * Jscontact_contact.Language_pref.t) list option;
      (** The languages preferred for contacting the entity. *)
  calendars : (Jscontact_id.t * Jscontact_calendar.t) list option;
      (** The calendaring resources of the entity. *)
  scheduling_addresses :
    (Jscontact_id.t * Jscontact_calendar.Scheduling_address.t) list option;
      (** The addresses at which the entity receives scheduling invitations. *)
  addresses : (Jscontact_id.t * Jscontact_address.t) list option;
      (** The postal addresses and geographic locations of the entity. *)
  crypto_keys : (Jscontact_id.t * Jscontact_resource.Crypto_key.t) list option;
      (** The public keys and certificates of the entity. *)
  directories : (Jscontact_id.t * Jscontact_resource.Directory.t) list option;
      (** The directories holding information about the entity. *)
  links : (Jscontact_id.t * Jscontact_resource.Link.t) list option;
      (** The resources that fit no other resource property. *)
  media : (Jscontact_id.t * Jscontact_resource.Media.t) list option;
      (** The photographs, avatars and sounds associated with the entity. *)
  localizations : (string * Jscontact_patch.t) list option;
      (** The property values localized to languages other than {!language},
          keyed by language tag. See {!localize}. *)
  anniversaries : (Jscontact_id.t * Jscontact_info.Anniversary.t) list option;
      (** The memorable dates and events for the entity. *)
  keywords : string list option;
      (** The free-text keywords, also known as tags. *)
  notes : (Jscontact_id.t * Jscontact_info.Note.t) list option;
      (** The free-text notes associated with the Card. *)
  personal_info : (Jscontact_id.t * Jscontact_info.Personal_info.t) list option;
      (** The personal information of the entity. *)
  unknown : Jscontact_unknown.t;
      (** The members that no property above names. Sections 1.7.4 and 1.8.1
          require them to be preserved. *)
}
(** The type for a Card. *)

val version_1_0 : string
(** [version_1_0] is ["1.0"], the JSContact version this library implements,
    registered by
    {{:https://www.rfc-editor.org/rfc/rfc9553.html#section-1.9.2} Section 1.9.2}.
*)

val make :
  ?version:string ->
  ?created:Jscontact_date.Utc.t ->
  ?kind:Kind.t ->
  ?language:string ->
  ?members:string list ->
  ?prod_id:string ->
  ?related_to:(string * Jscontact_info.Relation.t) list ->
  ?updated:Jscontact_date.Utc.t ->
  ?name:Jscontact_name.t ->
  ?nicknames:(Jscontact_id.t * Jscontact_name.Nickname.t) list ->
  ?organizations:(Jscontact_id.t * Jscontact_org.Organization.t) list ->
  ?speak_to_as:Jscontact_org.Speak_to_as.t ->
  ?titles:(Jscontact_id.t * Jscontact_org.Title.t) list ->
  ?emails:(Jscontact_id.t * Jscontact_contact.Email_address.t) list ->
  ?online_services:(Jscontact_id.t * Jscontact_contact.Online_service.t) list ->
  ?phones:(Jscontact_id.t * Jscontact_contact.Phone.t) list ->
  ?preferred_languages:(Jscontact_id.t * Jscontact_contact.Language_pref.t) list ->
  ?calendars:(Jscontact_id.t * Jscontact_calendar.t) list ->
  ?scheduling_addresses:
    (Jscontact_id.t * Jscontact_calendar.Scheduling_address.t) list ->
  ?addresses:(Jscontact_id.t * Jscontact_address.t) list ->
  ?crypto_keys:(Jscontact_id.t * Jscontact_resource.Crypto_key.t) list ->
  ?directories:(Jscontact_id.t * Jscontact_resource.Directory.t) list ->
  ?links:(Jscontact_id.t * Jscontact_resource.Link.t) list ->
  ?media:(Jscontact_id.t * Jscontact_resource.Media.t) list ->
  ?localizations:(string * Jscontact_patch.t) list ->
  ?anniversaries:(Jscontact_id.t * Jscontact_info.Anniversary.t) list ->
  ?keywords:string list ->
  ?notes:(Jscontact_id.t * Jscontact_info.Note.t) list ->
  ?personal_info:(Jscontact_id.t * Jscontact_info.Personal_info.t) list ->
  ?unknown:Jscontact_unknown.t ->
  string ->
  t
(** [make uid] is the Card identified by [uid] with the given properties. Every
    optional argument defaults to the property being unset, except [version],
    which defaults to {!version_1_0}, and [kind], which defaults to
    [`Individual]. *)

val equal : t -> t -> bool
(** [equal a b] is [true] if [a] and [b] have the same properties. *)

val pp : Format.formatter -> t -> unit
(** [pp ppf c] formats the uid and kind of [c] on [ppf]. *)

val validate : t -> t Jscontact_valid.t
(** [validate c] holds [c] and everything it contains to the rules of
    {{:https://www.rfc-editor.org/rfc/rfc9553.html#section-1.7} Section 1.7}.
    The version is well formed. [members] is set only on a group Card. The
    language tags are well formed. No localization patches the [localizations]
    property itself. Each contained object validates. *)

val jsont : t Jsont.t
(** [jsont] is the codec for a Card. Section 2.1.1 makes the [@type] property of
    a Card mandatory, since a Card is not the value of a property. Decoding
    requires it and encoding writes it. *)

val partial_jsont : t Jsont.t
(** [partial_jsont] is {!jsont} for a context that returns a subset of a Card's
    properties rather than a whole Card.
    {{:https://www.rfc-editor.org/rfc/rfc8620.html#section-5.1} RFC 8620 Section
     5.1} lets a JMAP [/get] name the properties it wants. A server then returns
    those alone, so a ContactCard may arrive with none of [@type], [version] and
    [uid], even though Section 2.1 makes all three mandatory.

    Decoding accepts all three as absent. It leaves {!field-version} at
    {!version_1_0} and {!field-uid} empty. Such a Card is not a valid Card, and
    {!validate} reports it as such, since Section 2.1.9 requires a uid. A caller
    should test [uid <> ""] before treating a decoded Card as whole. Encoding
    always writes [@type] and [version], and omits an empty [uid] rather than
    writing one no Card may have. *)

val localize : t -> language:string -> (t option, string) result
(** [localize c ~language] is the variant of [c] in [language], or [None] if [c]
    has no localization for that language tag.

    {{:https://www.rfc-editor.org/rfc/rfc9553.html#section-2.7.1} Section 2.7.1}
    defines the algorithm. It takes the PatchObject stored under [language],
    copies the Card without its [localizations] property, applies every patch of
    the PatchObject to the copy, and sets the copy's [language]. [localize]
    checks that the patch does not target the [localizations] property itself.
    The error holds a message when a patch cannot be applied, or when the
    patched Card no longer decodes. Section 1.4.3 requires the localization to
    be rejected in its entirety rather than applied in part. *)
