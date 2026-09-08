@@ portable

(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Push notifications.

    {{:https://datatracker.ietf.org/doc/html/rfc8620#section-7} RFC 8620 Section
     7} lets a server tell a client that data has changed, either over an event
    source connection or by POSTing to a URL the client registers as a
    PushSubscription.

    @canonical Jmap.Proto.Push *)

(** {1 StateChange} *)

(** The object a server pushes when data changes, defined by RFC 8620 Section
    7.1. *)
module State_change : sig
  type type_state = {
    type_name : string;
        (** The data type that changed, such as ["Email"] or ["Mailbox"]. *)
    state : string;  (** The new state string of that data type. *)
  }
  (** The type for the new state of one data type. *)

  type t = {
    changed : (Proto_id.t * type_state list) list;
        (** The new states, keyed by account id. *)
  }
  (** The type for StateChange objects. *)

  val type_name : string
  (** [type_name] is ["StateChange"], the value RFC 8620 Section 7.1 requires in
      the [@type] member. *)

  val v : (Proto_id.t * type_state list) list -> t
  (** [v changed] is the StateChange reporting [changed]. *)

  val jsont : t Jsont.t
  (** [jsont] is the codec for a StateChange. Encoding writes the [@type] member
      and decoding rejects any value of it but {!val-type_name}. *)
end

(** {1 PushVerification} *)

(** The object a server POSTs to a new subscription URL to verify it, defined by
    RFC 8620 Section 7.2.2. The client must update the push subscription with
    this verification code before the server makes any further request to the
    subscription URL. *)
module Push_verification : sig
  type t = {
    push_subscription_id : string;
        (** The id of the push subscription that was created. *)
    verification_code : string;
        (** The code to put in the [verificationCode] property of that
            subscription. *)
  }
  (** The type for PushVerification objects. *)

  val type_name : string
  (** [type_name] is ["PushVerification"], the value RFC 8620 Section 7.2.2
      requires in the [@type] member. *)

  val v : push_subscription_id:string -> verification_code:string -> t
  (** [v ~push_subscription_id ~verification_code] is the PushVerification for
      [push_subscription_id]. *)

  val jsont : t Jsont.t
  (** [jsont] is the codec for a PushVerification. Encoding writes the [@type]
      member and decoding rejects any value of it but {!type_name}. *)
end

(** {1 PushSubscription} *)

type push_keys = {
  p256dh : string;  (** The P-256 ECDH public key, as URL safe base64. *)
  auth : string;  (** The authentication secret, as URL safe base64. *)
}
(** The type for the Web Push encryption keys of a subscription, defined by RFC
    8291. *)

val push_keys : p256dh:string -> auth:string -> (push_keys, string) result
(** [push_keys ~p256dh ~auth] is the Web Push key pair encoded with unpadded
    base64url. [p256dh] decodes to a 65-octet uncompressed P-256 point and
    [auth] decodes to 16 octets. The error holds a human readable message when a
    value carries base64 padding, is not base64url, or does not decode to the
    length its member requires. *)

val push_keys_jsont : push_keys Jsont.t
(** [push_keys_jsont] is the codec for push keys, checked as {!val-push_keys}
    checks them on both decode and encode. *)

type t = {
  id : Proto_id.t;  (** The server assigned subscription id. *)
  device_client_id : string option;
      (** The client provided device identifier, when returned. RFC 8620 Section
          7.2 types it plain [String], so [None] means the member was absent and
          an explicit [null] is rejected on decode. *)
  url : string option;
      (** The push endpoint URL. A conformant server never returns it, and, like
          {!field-device_client_id}, it is absent rather than [null] when not
          returned. *)
  keys : push_keys option;
      (** The encryption keys. A conformant server never returns them. *)
  verification_code : string option;
      (** The code the client must echo back to confirm ownership of the
          endpoint. *)
  expires : Ptime.t option;
      (** When the subscription expires and the server stops using it. *)
  types : string list option;
      (** The data types to notify about. [None] means every type. *)
}
(** The type for PushSubscription records, defined by RFC 8620 Section 7.2.
    [device_client_id] and [url] are optional because a server must not return
    [url] or [keys], which may hold data private to a device, and a
    [PushSubscription/set] response echoes only the properties the server set.
    Use {!val-create_args} to create a subscription, where both are required. *)

val jsont : t Jsont.t
(** [jsont] is the codec for a PushSubscription. *)

(** {1 PushSubscription/get}

    RFC 8620 Section 7.2.1 makes this a standard [/get] except that it neither
    takes nor returns an [accountId] argument, and does not return [state]. *)

type get_args = {
  ids : Proto_id.t list option;
      (** The ids to fetch. [None] means every subscription and is written out
          as an explicit ["ids": null]. *)
  properties : string list option;
      (** The properties to return. Asking for [url] or [keys] explicitly must
          be answered with a [forbidden] error. *)
}
(** The type for the arguments of a [PushSubscription/get] call. *)

val get_args :
  ?ids:Proto_id.t list -> ?properties:string list -> unit -> get_args
(** [get_args ()] is the arguments of a [PushSubscription/get] call, with [ids]
    and [properties] [None] unless given. *)

val get_args_jsont : get_args Jsont.t
(** [get_args_jsont] is the codec for the arguments of a [PushSubscription/get]
    call. *)

type get_response = {
  list : t list;  (** The subscriptions fetched. *)
  not_found : Proto_id.t list;  (** The requested ids that do not exist. *)
}
(** The type for the response of a [PushSubscription/get] call. *)

val get_response_jsont : get_response Jsont.t
(** [get_response_jsont] is the codec for the response of a
    [PushSubscription/get] call. *)

(** {1 PushSubscription/set}

    RFC 8620 Section 7.2.2 makes this a standard [/set] except that it neither
    takes nor returns an [accountId] argument, does not take [ifInState] and
    returns neither [oldState] nor [newState]. *)

type create_args = {
  device_client_id : string;
      (** An id that identifies the client and the device it runs on. It must
          not carry an unobfuscated device id. *)
  url : string;
      (** The absolute endpoint to POST push data to. It must begin with
          ["https://"], have a host and, if present, have a numeric port in the
          range 1--65535. *)
  keys : push_keys option;
      (** The client generated encryption keys. When they are given the server
          must encrypt everything it sends to the subscription with them, per
          RFC 8291. *)
  verification_code : string option;
      (** The code confirming ownership of the endpoint. It must be absent on
          create, and is set on a later update with the code from the
          {!Push_verification} object the server pushes. *)
  expires : Ptime.t option;
      (** The requested expiry time. [None] lets the server choose it. *)
  types : string list option;
      (** The data types to notify about. [None] means every type. *)
}
(** The type for a PushSubscription as a client creates it. *)

val create_args :
  device_client_id:string ->
  url:string ->
  ?keys:push_keys ->
  ?expires:Ptime.t ->
  ?types:string list ->
  unit ->
  (create_args, string) result
(** [create_args ~device_client_id ~url ()] is the object of a
    [PushSubscription/set] [create] entry. [keys], [expires], and [types] are
    [None] unless given. [verification_code] is always [None], which RFC 8620
    Section 7.2.2 requires on create. The error holds a human readable message
    if the arguments would break
    {{:https://datatracker.ietf.org/doc/html/rfc8620#section-7.2} RFC 8620
     Section 7.2}, where [url] "MUST begin with [https://]" and must name a
    host. URL syntax is validated by {!Httpz_uri}. An explicit port must be
    numeric and in the range 1--65535. [device_client_id] "uniquely identifies
    the client + device it is running on" and so may not be empty. [keys], when
    given, is checked as {!val-push_keys} checks it. *)

val create_args_jsont : create_args Jsont.t
(** [create_args_jsont] is the codec for a created PushSubscription. *)

type set_args = {
  create : (create_args Proto_id.creation * create_args) list option;
      (** The subscriptions to create, keyed by creation id. *)
  update : (Proto_id.t * Proto_patch.t) list option;
      (** The patches to apply, keyed by subscription id. *)
  destroy : Proto_id.t list option;
      (** The subscription ids to destroy. An entry may be a creation reference
          to a subscription created in the same call, per RFC 8620 Section 5.3.
      *)
}
(** The type for the arguments of a [PushSubscription/set] call. *)

val set_args :
  ?create:(create_args Proto_id.creation * create_args) list ->
  ?update:(Proto_id.t * Proto_patch.t) list ->
  ?destroy:Proto_id.t list ->
  unit ->
  set_args
(** [set_args ()] is the arguments of a [PushSubscription/set] call, with every
    member [None] unless given. *)

val set_args_jsont : set_args Jsont.t
(** [set_args_jsont] is the codec for the arguments of a [PushSubscription/set]
    call. *)

type update_response = {
  expires : Ptime.t option;
      (** The expiry time set by the server, or [None] if it returned no
          [expires] property. *)
}
(** The type for properties returned after a subscription update. *)

type set_response = {
  created : (Proto_id.t * t) list option;
      (** The properties the server set on each created subscription, keyed by
          creation id. *)
  updated : (Proto_id.t * update_response option) list option;
      (** The subscriptions updated. The value is [None] when the server has no
          further property to report. *)
  destroyed : Proto_id.t list option;
      (** The ids of the subscriptions destroyed. *)
  not_created : (Proto_id.t * Proto_error.Set_error.t) list option;
      (** Why each subscription that was not created failed. *)
  not_updated : (Proto_id.t * Proto_error.Set_error.t) list option;
      (** Why each subscription that was not updated failed. *)
  not_destroyed : (Proto_id.t * Proto_error.Set_error.t) list option;
      (** Why each subscription that was not destroyed failed. *)
}
(** The type for the response of a [PushSubscription/set] call. *)

val set_response_jsont : set_response Jsont.t
(** [set_response_jsont] is the codec for the response of a
    [PushSubscription/set] call. *)
