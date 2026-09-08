@@ portable

(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Identities.

    {{:https://datatracker.ietf.org/doc/html/rfc8621#section-6} RFC 8621 Section
     6} defines the Identity object, an address the user may send from together
    with the defaults to apply to a message sent from it.

    @canonical Jmap.Proto.Identity *)

(** {1 Properties} *)

type property =
  [ `Id
  | `Name
  | `Email
  | `Reply_to
  | `Bcc
  | `Text_signature
  | `Html_signature
  | `May_delete ]
(** The type for the properties an [Identity/get] may ask for. *)

val property_to_string : [< property ] -> string
(** [property_to_string p] is the wire name of [p], such as ["textSignature"].
*)

val property_of_string : string -> property option
(** [property_of_string s] is the property whose wire name is [s], or [None] if
    there is none. The comparison is by octet. *)

(** {1 Identities} *)

type t = {
  id : Proto_id.t option;  (** The server assigned id of the Identity. *)
  name : string option;
      (** The display name to put in the From field of a message sent from this
          Identity. *)
  email : string option;
      (** The address to put in the From field. It may start with ["*@"], which
          stands for any local part at that domain. *)
  reply_to : Mail_address.t list option;
      (** The addresses to put in the Reply-To field. *)
  bcc : Mail_address.t list option;
      (** The addresses to put in the Bcc field. *)
  text_signature : string option;
      (** The signature to append to the plain text body. *)
  html_signature : string option;
      (** The signature to append to the HTML body. *)
  may_delete : bool option;  (** [true] if the user may destroy this Identity. *)
}
(** The type for Identity objects. A property is [None] when the [Identity/get]
    did not ask for it. *)

val v :
  ?id:Proto_id.t ->
  ?name:string ->
  ?email:string ->
  ?reply_to:Mail_address.t list ->
  ?bcc:Mail_address.t list ->
  ?text_signature:string ->
  ?html_signature:string ->
  ?may_delete:bool ->
  unit ->
  t
(** [v ()] is an Identity with only the properties given set, every other one
    being [None] and left out of the JSON. An [Identity/set] [create] object is
    built the same way. RFC 8621 Section 6 makes [id] and [mayDelete] server set
    and [email] immutable, so a create carries no more than [name], [email],
    [reply_to], [bcc], [text_signature] and [html_signature]. *)

val id : t -> Proto_id.t option
(** [id t] is the id of the Identity [t], or [None] if the [Identity/get] did
    not ask for it. *)

val sending_address : local_part:string -> t -> string option
(** [sending_address ~local_part i] is the address a message sent from [i]
    carries in its From field. RFC 8621 Section 6 lets [email] be ["*@domain"],
    standing for any local part at that domain, in which case it is [local_part]
    at that domain. It is [None] when [i] has no address and when the address is
    ["*"] or ["*@"], neither of which names a domain. It is also [None] when
    [email] is ["*@domain"] and [local_part] is empty or holds an ["@"], since
    RFC 8620 Section 2 lets the session username a caller passes here be a full
    address rather than a local part. *)

val creation : string -> t Proto_id.creation
(** [creation s] is {!Jmap.Proto.Id.val-creation} [s] as the creation id of an
    Identity. Binding it here rather than through [Id.creation] fixes the type
    of the record it names at the binding, so a creation id defined before the
    [/set] that uses it needs no annotation. *)

val jsont : t Jsont.t
(** [jsont] is the codec for an Identity. *)
