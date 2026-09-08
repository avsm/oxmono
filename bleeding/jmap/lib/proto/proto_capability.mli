@@ portable

(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** JMAP capabilities.

    A capability is named by a URI and carries an object of server settings.
    {{:https://datatracker.ietf.org/doc/html/rfc8620#section-2} RFC 8620 Section
     2} puts the server wide object under [capabilities] in the session and the
    per account object under [accountCapabilities] in each account.

    @canonical Jmap.Proto.Capability *)

(** {1 Capability URIs} *)

val core : string
(** [core] is [urn:ietf:params:jmap:core], the RFC 8620 core capability. *)

val mail : string
(** [mail] is [urn:ietf:params:jmap:mail], the RFC 8621 mail capability. *)

val submission : string
(** [submission] is [urn:ietf:params:jmap:submission], the RFC 8621 email
    submission capability. *)

val vacation_response : string
(** [vacation_response] is [urn:ietf:params:jmap:vacationresponse], the RFC 8621
    vacation response capability. *)

val contacts : string
(** [contacts] is [urn:ietf:params:jmap:contacts], the
    {{:https://www.rfc-editor.org/rfc/rfc9610#section-1.4.1} RFC 9610 Section
     1.4.1} contacts capability, which represents support for the AddressBook
    and ContactCard data types. *)

(** {1 Capability objects} *)

(** The core capability object of RFC 8620 Section 2. *)
module Core : sig
  type t = {
    max_size_upload : int64;
        (** The maximum size in octets of a single blob upload. *)
    max_concurrent_upload : int64;
        (** The maximum number of concurrent upload requests. *)
    max_size_request : int64;
        (** The maximum size in octets of a single request. *)
    max_concurrent_requests : int64;
        (** The maximum number of concurrent requests. *)
    max_calls_in_request : int64;
        (** The maximum number of method calls in a single request. *)
    max_objects_in_get : int64;
        (** The maximum number of objects in a single [/get] request. *)
    max_objects_in_set : int64;
        (** The maximum number of objects in a single [/set] request. *)
    collation_algorithms : string list;
        (** The collation algorithms the server supports for sorting. *)
  }
  (** The type for core capability objects. *)

  val create :
    max_size_upload:int64 ->
    max_concurrent_upload:int64 ->
    max_size_request:int64 ->
    max_concurrent_requests:int64 ->
    max_calls_in_request:int64 ->
    max_objects_in_get:int64 ->
    max_objects_in_set:int64 ->
    collation_algorithms:string list ->
    t
  (** [create ~max_size_upload ...] is the core capability object with the given
      limits. *)

  val jsont : t Jsont.t
  (** [jsont] is the codec for a core capability object. Every member is
      mandatory. *)
end

(** The mail capability object of RFC 8621 Section 1.3.1.

    The record uses options so that one type covers both scopes. In a decoded
    Session, {!Mail.session_jsont} requires the empty object the session scope
    gives this capability and {!Mail.account_jsont} requires all six
    account-scoped members. *)
module Mail : sig
  type t = {
    max_mailboxes_per_email : int64 option;
        (** The maximum number of Mailboxes an Email can belong to. RFC 8621
            Section 1.3.1 types this [UnsignedInt|null], where [null] means no
            limit. [None] means the server stated no limit, either by sending
            [null] or by omitting the member as it does at session scope. *)
    max_mailbox_depth : int64 option;
        (** The maximum depth of the Mailbox hierarchy, on the same terms as
            {!field-max_mailboxes_per_email}. *)
    max_size_mailbox_name : int64 option;
        (** The maximum size of a Mailbox name in octets. *)
    max_size_attachments_per_email : int64 option;
        (** The maximum total size of the attachments of one Email. *)
    email_query_sort_options : string list option;
        (** The properties [Email/query] can sort on. *)
    may_create_top_level_mailbox : bool option;
        (** Whether the user may create a Mailbox with no parent. *)
  }
  (** The type for mail capability objects. *)

  val create :
    ?max_mailboxes_per_email:int64 ->
    ?max_mailbox_depth:int64 ->
    ?max_size_mailbox_name:int64 ->
    ?max_size_attachments_per_email:int64 ->
    ?email_query_sort_options:string list ->
    ?may_create_top_level_mailbox:bool ->
    unit ->
    t
  (** [create ()] is the mail capability object with every member absent unless
      given. *)

  val session_jsont : unit Jsont.t
  (** [session_jsont] accepts exactly the empty object required for this
      capability in a session's [capabilities] map. *)

  val account_jsont : t Jsont.t
  (** [account_jsont] is the account-scoped codec. All six members are mandatory
      and the first two may be JSON [null]. It additionally enforces the RFC
      minimum of one for [maxMailboxesPerEmail] and 100 octets for
      [maxSizeMailboxName]. A value decoded with this codec has [Some] for every
      non-nullable field. Encoding fails if such a field is [None]. *)
end

(** The submission capability object of RFC 8621 Section 1.3.2.

    The record uses options so that one type covers both scopes. In a decoded
    Session, {!Submission.session_jsont} requires the empty object the session
    scope gives this capability and {!Submission.account_jsont} requires both
    account-scoped members. *)
module Submission : sig
  type t = {
    max_delayed_send : int64 option;
        (** The maximum delay in seconds the server accepts for delayed sending.
            Zero means delayed sending is not supported. *)
    submission_extensions : (string * string list) list option;
        (** The SMTP extensions the server supports, each with its parameters.
        *)
  }
  (** The type for submission capability objects. *)

  val create :
    ?max_delayed_send:int64 ->
    ?submission_extensions:(string * string list) list ->
    unit ->
    t
  (** [create ()] is the submission capability object with both members absent
      unless given. *)

  val session_jsont : unit Jsont.t
  (** [session_jsont] accepts exactly the empty object required for this
      capability in a session's [capabilities] map. *)

  val account_jsont : t Jsont.t
  (** [account_jsont] is the account-scoped codec. Both members are mandatory,
      and a decoded value therefore has [Some] for both. Encoding fails if
      either field is [None]. *)
end

(** {1 Capability values} *)

(** The contacts capability object of RFC 9610 Section 1.4.1. *)
module Contacts : sig
  type t = {
    max_address_books_per_card : int64 option;
        (** The maximum number of AddressBooks a single ContactCard may be
            assigned to. RFC 9610 Section 1.4.1 types this [UnsignedInt|null],
            where [null] means the only limit is the number of AddressBooks in
            the account. [None] means the server stated no limit, either by
            sending [null] or by omitting the member as it does at session
            scope. *)
    may_create_address_book : bool option;
        (** Whether the user may create an AddressBook in this account. [None]
            at session scope, where the object is empty. *)
  }
  (** The type for contacts capability objects. *)

  val create :
    ?max_address_books_per_card:int64 ->
    ?may_create_address_book:bool ->
    unit ->
    t
  (** [create ()] is the contacts capability object with every member absent
      unless given. *)

  val is_empty : t -> bool
  (** [is_empty c] is [true] if every member of [c] is absent, which is what
      Section 1.4.1 requires of the session scoped object. *)

  val encode_jsont : t Jsont.t
  (** [encode_jsont] is the codec that writes every member the value sets. *)

  val session_jsont : unit Jsont.t
  (** [session_jsont] is the codec for the session scoped object, which Section
      1.4.1 requires to be empty. *)

  val account_jsont : t Jsont.t
  (** [account_jsont] is the codec for the account scoped object, whose two
      members Section 1.4.1 makes mandatory. *)
end

(** The type for the value of one capability of a session or an account. *)
type capability =
  | Core of Core.t
  | Mail of Mail.t
  | Submission of Submission.t
  | Vacation_response
      (** The vacation response capability. RFC 8621 Section 1.3.3 defines no
          settings for it, so its object is empty. *)
  | Contacts of Contacts.t
  | Unknown of Jsont.json
      (** A capability this library does not model, kept verbatim. *)

val session_capability_of_json :
  string -> Jsont.json -> (capability, string) result
(** [session_capability_of_json uri json] decodes a capability in a session's
    [capabilities] map. It requires mail, submission, and vacation-response
    values to be empty objects and applies the server-scoped core codec. A URI
    this library does not model is [Unknown json], which keeps the value
    verbatim so that {!capability_to_json} round trips it. The error holds a
    human readable message when a modelled value does not decode. *)

val account_capability_of_json :
  string -> Jsont.json -> (capability, string) result
(** [account_capability_of_json uri json] decodes a capability in an account's
    [accountCapabilities] map. Mail and submission use their mandatory
    account-scoped members, vacation response must be empty, and core remains
    opaque because RFC 8620 defines no account-scoped core settings. A URI this
    library does not model is [Unknown json], kept verbatim. The error holds a
    human readable message when a modelled value does not decode. *)

val capability_to_json : string * capability -> string * Jsont.json
(** [capability_to_json (uri, cap)] is [uri] paired with the settings of [cap]
    as JSON. A [Mail] value with every member absent is written as the empty
    object the session scope requires. Any other [Mail] value is written with
    [maxMailboxesPerEmail] and [maxMailboxDepth] present, as an explicit [null]
    when absent, as the account scope requires.

    @raise Invalid_argument
      if [cap] holds a value outside the range its codec allows, such as a limit
      that is not an [Int53]. *)
