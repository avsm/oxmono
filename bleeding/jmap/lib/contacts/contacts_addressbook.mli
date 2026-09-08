@@ portable

(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Address books.

    {{:https://datatracker.ietf.org/doc/html/rfc9610#section-2} RFC 9610 Section
     2} defines the AddressBook object, a named collection of ContactCards.
    Every ContactCard belongs to at least one AddressBook.

    @canonical Jmap.Proto.Address_book *)

(** {1 Properties} *)

type property =
  [ `Id
  | `Name
  | `Description
  | `Sort_order
  | `Is_default
  | `Is_subscribed
  | `Share_with
  | `My_rights ]
(** The type for the properties an [AddressBook/get] may ask for. *)

val property_to_string : [< property ] -> string
(** [property_to_string p] is the wire name of [p], such as ["sortOrder"]. *)

val property_of_string : string -> property option
(** [property_of_string s] is the property whose wire name is [s], or [None] if
    there is none. The comparison is by octet. *)

(** {1 Rights} *)

(** The rights the user has on an AddressBook. *)
module Rights : sig
  type t = {
    may_read : bool;
        (** [true] if the user may fetch the ContactCards of the AddressBook. *)
    may_write : bool;
        (** [true] if the user may create, modify or destroy any ContactCard of
            the AddressBook, or move a ContactCard into or out of it. *)
    may_share : bool;
        (** [true] if the user may modify the [shareWith] property of the
            AddressBook. *)
    may_delete : bool;
        (** [true] if the user may destroy the AddressBook itself. *)
  }
  (** The type for AddressBookRights objects. *)

  val jsont : t Jsont.t
  (** [jsont] is the codec for an AddressBookRights. *)
end

(** {1 Address books} *)

type t = {
  id : Proto_id.t option;  (** The server assigned id of the AddressBook. *)
  name : string option;  (** The user visible name of the AddressBook. *)
  description : string option;
      (** The long form description of the AddressBook, which gives context
          where the name alone does not. [None] means no description, or that
          the property was not asked for. *)
  sort_order : int64 option;
      (** The position of the AddressBook when the address books are shown,
          lower first. Address books of equal order sort by name. *)
  is_default : bool option;
      (** [true] if the AddressBook is the one a client picks when it has no
          other information on which to choose. At most one AddressBook of an
          account has it. *)
  is_subscribed : bool option;
      (** [true] if the user has indicated they wish to see the AddressBook in
          their client. *)
  share_with : (Proto_id.t * Rights.t) list option;
      (** The rights each Principal the AddressBook is shared with has on it,
          keyed by the Principal id of
          {{:https://datatracker.ietf.org/doc/html/rfc9670#section-2} RFC 9670
           Section 2}. [None] means shared with nobody or a server without RFC
          9670, or that the property was not asked for. The Principal owning the
          AddressBook is never in the map. *)
  my_rights : Rights.t option;
      (** The rights the user has on the AddressBook. *)
}
(** The type for AddressBook objects. A property is [None] when the
    [AddressBook/get] did not ask for it, and for the properties RFC 9610
    Section 2 types [T|null] it also covers the [null]. *)

val empty : t
(** [empty] is the AddressBook with every property unset. *)

val creation : string -> t Proto_id.creation
(** [creation s] is {!Jmap.Proto.Id.val-creation} [s] as the creation id of an
    AddressBook. Binding it here rather than through [Id.creation] fixes the
    type of the record it names at the binding, so a creation id defined before
    the [/set] that uses it needs no annotation. *)

val create :
  name:string ->
  ?description:string ->
  ?sort_order:int64 ->
  ?is_subscribed:bool ->
  ?share_with:(Proto_id.t * Rights.t) list ->
  unit ->
  (t, string) result
(** [create ~name ()] is the object of an [AddressBook/set] [create] entry. Only
    the client settable properties of RFC 9610 Section 2 are taken, the server
    set [id], [is_default] and [my_rights] being left unset.

    The error holds a human readable message when the arguments break Section 2,
    which requires [name] to "NOT be the empty string" and to "NOT be greater
    than 255 octets in size when encoded as UTF-8", and [sort_order] to "be an
    integer in the range 0 <= sortOrder < 2^31". [name] is also checked for
    being valid UTF-8 free of Unicode noncharacters, which the I-JSON profile of
    RFC 8620 Section 1.5 demands of every string.

    Section 2 lets [share_with] be set only by a user holding the [may_share]
    right, which the client cannot know before the server answers, so it is not
    checked here. A server refuses such a [/set] with a [forbidden] SetError. *)

val create_exn :
  name:string ->
  ?description:string ->
  ?sort_order:int64 ->
  ?is_subscribed:bool ->
  ?share_with:(Proto_id.t * Rights.t) list ->
  unit ->
  t
(** [create_exn ~name ()] is {!create}.

    @raise Invalid_argument if the arguments break RFC 9610 Section 2. *)

val jsont : t Jsont.t
(** [jsont] is the codec for an AddressBook. [description] and [shareWith] are
    always present and encode as an explicit [null] when they are [None], RFC
    9610 Section 2 typing them [String|null] and [Id[AddressBookRights]|null]
    and giving [null] the meanings "no description" and "shared with nobody". *)
