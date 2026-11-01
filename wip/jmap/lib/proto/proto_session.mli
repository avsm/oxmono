@@ portable

(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** The JMAP session resource.

    A client fetches the session resource before anything else. It names the
    accounts the credentials reach, the capabilities the server supports and the
    URLs of the API, upload, download and event source endpoints, as defined by
    {{:https://datatracker.ietf.org/doc/html/rfc8620#section-2} RFC 8620 Section
     2}.

    @canonical Jmap.Proto.Session *)

(** {1 Accounts} *)

(** An account the credentials give access to. *)
module Account : sig
  type t = {
    name : string;  (** A human readable name for the account. *)
    is_personal : bool;
        (** Whether the account belongs to the user the credentials name. *)
    is_read_only : bool;  (** Whether the account forbids every change. *)
    account_capabilities : (string * Jsont.json) list;
        (** The capabilities available on this account, keyed by capability URI.
            RFC 8620 Section 2 types this [String[Object]], so decoding rejects
            a value that is not an object. When the mail or submission
            capability is present, the members it defines are type- and
            range-checked. *)
    unknown : Proto_unknown.t;
        (** The members not defined above, kept verbatim so that a re-encoded
            account carries them on. RFC 8620 Section 2 says "the client MUST
            ignore any properties it does not understand". *)
  }
  (** The type for accounts. *)

  val unknown_member : t -> string -> Jsont.json option
  (** [unknown_member a name] is the value of the extension member [name] of
      [a], or [None] if [a] has no such member. *)

  val jsont : t Jsont.t
  (** [jsont] is the codec for an account. *)
end

(** {1 Sessions} *)

type t = {
  capabilities : (string * Jsont.json) list;
      (** The capabilities the server supports, keyed by capability URI. RFC
          8620 Section 2 requires the core capability and types this
          [String[Object]], so decoding rejects a missing core entry or a value
          that is not an object. *)
  accounts : (Proto_id.t * Account.t) list;
      (** The accounts the credentials reach, keyed by account id. *)
  primary_accounts : (string * Proto_id.t) list;
      (** The primary account id for each capability URI. *)
  username : string;  (** The username the credentials belong to. *)
  api_url : string;  (** The URL to POST a request to. *)
  download_url : string;
      (** The URI template for downloading a blob. See
          {!Jmap.Proto.Blob.expand_download_url}. *)
  upload_url : string;  (** The URI template for uploading a blob. *)
  event_source_url : string;  (** The URI template for the push event source. *)
  state : string;
      (** An opaque string that changes whenever anything else in the session
          changes. *)
  unknown : Proto_unknown.t;
      (** The members not defined above, kept verbatim. RFC 8620 Section 2 says
          "other properties MAY be included on the Session object. Clients MUST
          ignore any properties they are not expecting." *)
}
(** The type for session resources. *)

val unknown_member : t -> string -> Jsont.json option
(** [unknown_member s name] is the value of the extension member [name] of [s],
    or [None] if [s] has no such member. *)

val jsont : t Jsont.t
(** [jsont] is the codec for a session resource. *)

(** {1 Lookups} *)

val find_account : Proto_id.t -> t -> Account.t option
(** [find_account id s] is the account of [s] with id [id], or [None] if [s] has
    no such account. *)

val primary_account_for : string -> t -> Proto_id.t option
(** [primary_account_for uri s] is the id of the primary account of [s] for the
    capability [uri], or [None] if [s] names none. *)

val has_capability : string -> t -> bool
(** [has_capability uri s] is [true] if the server offers the capability [uri].
*)

val core_capability : t -> Proto_capability.Core.t option
(** [core_capability s] is the core capability object of [s]. {!jsont} rejects a
    session that omits [urn:ietf:params:jmap:core] or gives it a malformed
    value. A manually constructed record can still violate that invariant, in
    which case this returns [None]. *)

val mail_capability : Account.t -> Proto_capability.Mail.t option
(** [mail_capability a] is the mail capability object of the account [a], or
    [None] if [a] does not offer [urn:ietf:params:jmap:mail]. {!Account.jsont}
    rejects a malformed advertised value; a manually constructed record can
    still violate that invariant. RFC 8621 Section 1.3.1 puts the mail limits in
    the account capabilities and leaves the session wide value empty. *)

val submission_capability : Account.t -> Proto_capability.Submission.t option
(** [submission_capability a] is the submission capability object of the account
    [a], or [None] if [a] does not offer [urn:ietf:params:jmap:submission].
    {!Account.jsont} rejects a malformed advertised value; a manually
    constructed record can still violate that invariant. RFC 8621 Section 1.3.2
    puts the submission settings in the account capabilities and leaves the
    session wide value empty. *)

val contacts_capability : Account.t -> Proto_capability.Contacts.t option
(** [contacts_capability a] is the contacts capability object of the account
    [a], or [None] if [a] does not offer [urn:ietf:params:jmap:contacts].
    {{:https://www.rfc-editor.org/rfc/rfc9610#section-1.4.1} RFC 9610 Section
     1.4.1} puts [maxAddressBooksPerCard] and [mayCreateAddressBook] in the
    account capabilities and leaves the session wide value empty. *)
