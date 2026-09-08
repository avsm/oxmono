@@ portable

(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Contact cards.

    {{:https://www.rfc-editor.org/rfc/rfc9610#section-3} RFC 9610 Section 3}
    defines the ContactCard object: a JSContact Card, as defined by
    {{:https://www.rfc-editor.org/rfc/rfc9553#section-2} RFC 9553 Section 2},
    carrying two further properties, [id] and [addressBookIds].

    @canonical Jmap.Proto.Contact_card *)

(** {1 Contact cards} *)

type t = {
  id : Proto_id.t option;
      (** The id of the ContactCard, immutable and server-set. It may differ
          from the [uid] of the Card. [None] on an object built for a [/set]
          [create], and on one returned by a [/get] that did not ask for the
          property. *)
  address_book_ids : (Proto_id.t * bool) list option;
      (** The AddressBooks the ContactCard belongs to, as an [Id[Boolean]] map
          whose every value must be [true]. Section 3 requires a card to belong
          to at least one AddressBook until it is destroyed. [None] when the
          [/get] did not ask for the property. *)
  card : Jscontact.Card.t;  (** The JSContact body of the ContactCard. *)
}
(** The type for ContactCard objects. *)

val make :
  ?id:Proto_id.t ->
  ?address_book_ids:(Proto_id.t * bool) list ->
  Jscontact.Card.t ->
  t
(** [make card] is the ContactCard whose body is [card]. [id] and
    [address_book_ids] are unset unless given. A [/set] [create] leaves [id]
    out, the server assigning it, and gives [address_book_ids] the AddressBooks
    the card is to belong to. *)

val creation : string -> t Proto_id.creation
(** [creation s] is {!Jmap.Proto.Id.val-creation} [s] as the creation id of a
    ContactCard. Binding it here rather than through [Id.creation] fixes the
    type of the record it names at the binding, so a creation id defined before
    the [/set] that uses it needs no annotation. *)

val jsont : t Jsont.t
(** [jsont] is the codec for a ContactCard.

    It maps over {!Jscontact.Card.partial_jsont} rather than
    {!Jscontact.Card.jsont}, since
    {{:https://www.rfc-editor.org/rfc/rfc8620#section-5.1} RFC 8620 Section 5.1}
    lets a [ContactCard/get] name the [properties] it wants and a server then
    returns those alone: [@type], [version] and [uid] may all be absent even
    though RFC 9553 Section 2.1 makes them mandatory. Test [card.uid <> ""]
    before treating a decoded card as whole.

    A Card keeps the members its codec does not define, so decoding lifts [id]
    and [addressBookIds] out of them into {!field-id} and
    {!field-address_book_ids}, and encoding puts them back. Neither is left in
    [card.unknown]. An absent member decodes to [None] rather than an error,
    which a [/set] [create] and a partial [/get] both need. *)

(** {1 Queries} *)

(** Filter conditions for a [ContactCard/query]. *)
module Filter_condition : sig
  type t = {
    in_address_book : Proto_id.t option;
        (** Keep the cards in this AddressBook. *)
    uid : string option;  (** Keep the cards whose [uid] is exactly this. *)
    has_member : string option;
        (** Keep the group cards whose [members] holds this uid. *)
    kind : string option;  (** Keep the cards whose [kind] is exactly this. *)
    created_before : Proto_date.t option;
        (** Keep the cards whose [created] date is before this time. *)
    created_after : Proto_date.t option;
        (** Keep the cards whose [created] date is at or after this time. *)
    updated_before : Proto_date.t option;
        (** Keep the cards whose [updated] date is before this time. *)
    updated_after : Proto_date.t option;
        (** Keep the cards whose [updated] date is at or after this time. *)
    text : string option;  (** Keep the cards whose text holds this text. *)
    name : string option;
        (** Keep the cards a NameComponent or the [full] of whose [name] matches
            this text. *)
    name_given : string option;
        (** Keep the cards a NameComponent of kind ["given"] of whose [name]
            matches this text. The wire name is ["name/given"], whose solidus is
            part of the member name and not a nesting. *)
    name_surname : string option;
        (** Keep the cards a NameComponent of kind ["surname"] of whose [name]
            matches this text. The wire name is ["name/surname"], whose solidus
            is part of the member name and not a nesting. *)
    name_surname2 : string option;
        (** Keep the cards a NameComponent of kind ["surname2"] of whose [name]
            matches this text. The wire name is ["name/surname2"], whose solidus
            is part of the member name and not a nesting. *)
    nickname : string option;
        (** Keep the cards the [name] of one of whose [nicknames] matches this
            text. *)
    organization : string option;
        (** Keep the cards the [name] of one of whose [organizations] matches
            this text. *)
    email : string option;
        (** Keep the cards the [address] or [label] of one of whose [emails]
            matches this text. *)
    phone : string option;
        (** Keep the cards the [number] or [label] of one of whose [phones]
            matches this text. *)
    online_service : string option;
        (** Keep the cards the [service], [uri], [user] or [label] of one of
            whose [onlineServices] matches this text. *)
    address : string option;
        (** Keep the cards an AddressComponent or the [full] of one of whose
            [addresses] matches this text. *)
    note : string option;
        (** Keep the cards the [note] of one of whose [notes] matches this text.
        *)
  }
  (** The type for the FilterCondition of a [ContactCard/query] (RFC 9610
      Section 3.3.1). A field of [None] does not filter. *)

  val empty : t
  (** [empty] is the condition with every field unset, which Section 3.3.1 makes
      true for every ContactCard. Build a condition from it with record update
      syntax, as in [{ empty with kind = Some "group" }]. *)

  val jsont : t Jsont.t
  (** [jsont] is the codec for a ContactCard FilterCondition. *)
end

type filter = Filter_condition.t Proto_filter.filter
(** The type for the [filter] argument of a [ContactCard/query]. *)

val filter_jsont : filter Jsont.t
(** [filter_jsont] is the codec for the [filter] argument of a
    [ContactCard/query]. *)

val filter :
  ?in_address_book:Proto_id.t ->
  ?uid:string ->
  ?has_member:string ->
  ?kind:string ->
  ?created_before:Proto_date.t ->
  ?created_after:Proto_date.t ->
  ?updated_before:Proto_date.t ->
  ?updated_after:Proto_date.t ->
  ?text:string ->
  ?name:string ->
  ?name_given:string ->
  ?name_surname:string ->
  ?name_surname2:string ->
  ?nickname:string ->
  ?organization:string ->
  ?email:string ->
  ?phone:string ->
  ?online_service:string ->
  ?address:string ->
  ?note:string ->
  unit ->
  filter
(** [filter ()] is the {!type-filter} of one {!Filter_condition} keeping the
    ContactCards that satisfy every argument given, as one condition of RFC 9610
    Section 3.3.1. An argument left out sets no field and so filters nothing.
    [filter ()] keeps every ContactCard. *)

(** The properties a [ContactCard/query] sorts on. *)
module Sort : sig
  type t = [ `Created | `Updated | `Name_given | `Name_surname | `Name_surname2 ]
  (** The type for the [property] field of a Comparator on a
      [ContactCard/query], listed by
      {{:https://www.rfc-editor.org/rfc/rfc9610#section-3.3.2} RFC 9610 Section
       3.3.2}. A server must support [`Created] and [`Updated], and should
      support the three name sorts. *)

  val to_string : t -> string
  (** [to_string p] is the wire name of [p], such as ["name/given"].
      {!Jmap.Proto.Filter.val-comparator} takes the property as a string, so
      this is what a caller passes it. *)

  val of_string : string -> t option
  (** [of_string s] is the property whose wire name is [s], or [None] if there
      is none. The comparison is by octet. *)

  val jsont : t Jsont.t
  (** [jsont] is the codec for a sort property. *)
end
