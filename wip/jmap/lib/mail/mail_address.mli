@@ portable

(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Email addresses.

    {{:https://datatracker.ietf.org/doc/html/rfc8621#section-4.1.2.3} RFC 8621
     Section 4.1.2.3} defines the EmailAddress object of the [asAddresses]
    header form, and Section 4.1.2.4 the EmailAddressGroup of the
    [asGroupedAddresses] form.

    @canonical Jmap.Proto.Email_address *)

(** {1 Addresses} *)

type t = {
  name : string option;
      (** The display name, from the RFC 5322 phrase before the address. *)
  email : string;  (** The address, an RFC 5322 addr-spec. *)
}
(** The type for email addresses. *)

val create : ?name:string -> string -> t
(** [create ~name email] is the address [email] displayed as [name]. [name]
    defaults to absent. *)

val equal : t -> t -> bool
(** [equal a b] is [true] if [a] and [b] have the same {!field-email}. The
    comparison is by octet and ignores {!field-name}, so two addresses that
    differ only in display name are equal. Case differences in [email] remain
    significant. *)

val pp : Format.formatter -> t -> unit
(** [pp ppf a] prints [a] on [ppf] as [Name <addr>], or as the bare address when
    [a] has no display name. Control bytes are escaped. *)

val jsont : t Jsont.t
(** [jsont] is the codec for an EmailAddress. An absent [name] and an explicit
    [null] both decode to [None]. *)

(** {1 Groups} *)

(** Named groups of addresses. *)
module Group : sig
  type address = t
  (** The type for the addresses of a group. *)

  type t = {
    name : string option;
        (** The group name, or [None] for addresses that are not in a group. *)
    addresses : address list;  (** The addresses of the group. *)
  }
  (** The type for EmailAddressGroup objects. *)

  val create : ?name:string -> address list -> t
  (** [create ~name addresses] is the group of [addresses] called [name]. [name]
      defaults to absent, which stands for addresses given outside any group. *)

  val jsont : t Jsont.t
  (** [jsont] is the codec for an EmailAddressGroup. *)
end
