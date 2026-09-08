@@ portable

(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Email submissions.

    {{:https://datatracker.ietf.org/doc/html/rfc8621#section-7} RFC 8621 Section
     7} defines the EmailSubmission object, which records the sending of an
    Email. Sending a message is creating an EmailSubmission for it.

    @canonical Jmap.Proto.Submission *)

(** {1 Properties} *)

type property =
  [ `Id
  | `Identity_id
  | `Email_id
  | `Thread_id
  | `Envelope
  | `Send_at
  | `Undo_status
  | `Delivery_status
  | `Dsn_blob_ids
  | `Mdn_blob_ids ]
(** The type for the properties an [EmailSubmission/get] may ask for. *)

val property_to_string : [< property ] -> string
(** [property_to_string p] is the wire name of [p], such as ["identityId"]. *)

val property_of_string : string -> property option
(** [property_of_string s] is the property whose wire name is [s], or [None] if
    there is none. The comparison is by octet. *)

(** {1 Envelopes} *)

(** SMTP envelope addresses. *)
module Address : sig
  type t = {
    email : string;  (** The address, without angle brackets. *)
    parameters : (string * string option) list option;
        (** The ESMTP parameters to send the address with. [None] is no
            parameters, and an entry whose value is [None] is a parameter that
            takes no value (RFC 8621 Section 7). *)
  }
  (** The type for the addresses of an Envelope. *)

  val v : ?parameters:(string * string option) list -> string -> t
  (** [v ~parameters email] is the address [email]. [parameters] defaults to
      absent, which encodes as an omitted member rather than an explicit [null],
      the two meaning the same thing. *)

  val jsont : t Jsont.t
  (** [jsont] is the codec for an envelope address. *)
end

(** SMTP envelopes. *)
module Envelope : sig
  type t = {
    mail_from : Address.t;  (** The address of the SMTP MAIL FROM command. *)
    rcpt_to : Address.t list;  (** The addresses of the SMTP RCPT TO commands. *)
  }
  (** The type for Envelope objects. *)

  val v : mail_from:Address.t -> rcpt_to:Address.t list -> t
  (** [v ~mail_from ~rcpt_to] is an envelope. Giving one on a create overrides
      the envelope the server would otherwise derive from the Sender, From, To,
      Cc and Bcc fields of the Email (RFC 8621 Section 7). *)

  val jsont : t Jsont.t
  (** [jsont] is the codec for an Envelope. *)
end

(** {1 Delivery status} *)

(** The delivery status of a submission to one recipient. *)
module Delivery_status : sig
  type delivered = [ `Queued | `Yes | `No | `Unknown | `Other of string ]
  (** The type for how far a message has got towards a recipient. [`Other s]
      preserves a spelling outside the set RFC 8621 Section 7 defines. *)

  type displayed = [ `Unknown | `Yes | `Other of string ]
  (** The type for whether a message has been displayed to a recipient, as
      reported by an MDN. [`Other s] preserves an unrecognised spelling. *)

  type t = {
    smtp_reply : string;
        (** The SMTP reply the recipient's server gave, as in
            ["250 2.1.5 Recipient OK"]. *)
    delivered : delivered;  (** How far the message has got. *)
    displayed : displayed;
        (** Whether the recipient has displayed the message. *)
  }
  (** The type for DeliveryStatus objects. *)

  val v : smtp_reply:string -> delivered:delivered -> displayed:displayed -> t
  (** [v ~smtp_reply ~delivered ~displayed] is a delivery status. *)

  val delivered_to_string : delivered -> string
  (** [delivered_to_string d] is the wire spelling of [d], such as ["queued"].
  *)

  val delivered_of_string : string -> delivered option
  (** [delivered_of_string s] is the [delivered] value spelled [s], or [None]
      for a spelling outside the four RFC 8621 Section 7 lists. *)

  val delivered_of_string_exn : string -> delivered
  (** [delivered_of_string_exn s] is {!delivered_of_string} of [s]. Use it where
      the client chooses the value. The codec is deliberately more lenient, see
      {!jsont}.

      @raise Invalid_argument
        if [s] is not one of the values RFC 8621 Section 7 lists. *)

  val displayed_to_string : displayed -> string
  (** [displayed_to_string d] is the wire spelling of [d]. *)

  val displayed_of_string : string -> displayed option
  (** [displayed_of_string s] is the [displayed] value spelled [s], or [None]
      for a spelling outside the two RFC 8621 Section 7 lists. *)

  val displayed_of_string_exn : string -> displayed
  (** [displayed_of_string_exn s] is {!displayed_of_string} of [s].

      @raise Invalid_argument
        if [s] is not one of the values RFC 8621 Section 7 lists. *)

  val jsont : t Jsont.t
  (** [jsont] is the codec for a DeliveryStatus.

      Decoding is forward-compatible about [delivered] and [displayed]. A value
      outside the sets RFC 8621 Section 7 lists becomes [`Other s], preserving
      the spelling and the rest of the [EmailSubmission/get] response. It
      round-trips unchanged and remains distinct from the RFC's ["unknown"]
      status. Where the client picks the value, use {!delivered_of_string_exn}
      or {!displayed_of_string_exn}, which reject anything outside the listed
      sets. *)
end

(** {1 Submissions} *)

type undo_status = [ `Pending | `Final | `Canceled ]
(** The type for how far a submission has got. [`Pending] is still cancellable,
    [`Final] has been sent and [`Canceled] was withdrawn before it was. *)

val undo_status_to_string : undo_status -> string
(** [undo_status_to_string u] is the wire spelling of [u], one of ["pending"],
    ["final"] and ["canceled"]. *)

val undo_status_of_string : string -> undo_status option
(** [undo_status_of_string s] is the [undoStatus] value spelled [s], or [None]
    for a spelling outside the three RFC 8621 Section 7 lists. *)

val undo_status_jsont : undo_status Jsont.t
(** [undo_status_jsont] is the codec for an [undoStatus]. Decoding a spelling
    outside the three raises [Jsont.Error]. *)

type t = {
  id : Proto_id.t option;  (** The server assigned id of the EmailSubmission. *)
  identity_id : Proto_id.t option;
      (** The Identity the message was sent from. *)
  email_id : Proto_id.t option;  (** The Email that was sent. *)
  thread_id : Proto_id.t option;  (** The Thread of that Email. *)
  envelope : Envelope.t option;
      (** The envelope the message was sent with, or [None] if the server
          derived it from the header fields. *)
  send_at : Ptime.t option;
      (** The time the message was handed to the SMTP server, or is to be. *)
  undo_status : undo_status option;  (** How far the submission has got. *)
  delivery_status : (string * Delivery_status.t) list option;
      (** The delivery status per recipient, keyed by envelope address, or
          [None] while the server has none. *)
  dsn_blob_ids : Proto_id.t list option;
      (** The blobs of the delivery status notifications received for this
          submission. *)
  mdn_blob_ids : Proto_id.t list option;
      (** The blobs of the message disposition notifications received for this
          submission. *)
}
(** The type for EmailSubmission objects. A property is [None] when the
    [EmailSubmission/get] did not ask for it. *)

val v :
  ?id:Proto_id.t ->
  ?identity_id:Proto_id.t ->
  ?email_id:Proto_id.t ->
  ?thread_id:Proto_id.t ->
  ?envelope:Envelope.t ->
  ?send_at:Ptime.t ->
  ?undo_status:undo_status ->
  ?delivery_status:(string * Delivery_status.t) list ->
  ?dsn_blob_ids:Proto_id.t list ->
  ?mdn_blob_ids:Proto_id.t list ->
  unit ->
  t
(** [v ()] is an EmailSubmission with only the properties given set, every other
    one being [None] and left out of the JSON. *)

val create :
  ?envelope:Envelope.t ->
  identity_id:Proto_id.t ->
  email_id:Proto_id.t ->
  unit ->
  t
(** [create ~identity_id ~email_id ()] is the object of an [EmailSubmission/set]
    [create] entry, which is how an Email is sent (RFC 8621 Section 7.5). It
    sets only the three client settable properties, with [id], [thread_id],
    [send_at], [undo_status], [delivery_status], [dsn_blob_ids] and
    [mdn_blob_ids] all being server set.

    [email_id] may be a creation reference such as ["#draft1"] naming an Email
    created earlier in the same request (RFC 8620 Section 5.3). With [envelope]
    left out the server derives the envelope from the header fields of the
    Email. *)

val id : t -> Proto_id.t option
(** [id s] is the id of the EmailSubmission [s], or [None] if the
    [EmailSubmission/get] did not ask for it. *)

val creation : string -> t Proto_id.creation
(** [creation s] is {!Jmap.Proto.Id.val-creation} [s] as the creation id of an
    EmailSubmission. Binding it here rather than through [Id.creation] fixes the
    type of the record it names at the binding, so a creation id defined before
    the [/set] that uses it needs no annotation. *)

val jsont : t Jsont.t
(** [jsont] is the codec for an EmailSubmission. *)

(** {1 Queries} *)

(** Filter conditions for an [EmailSubmission/query]. *)
module Filter_condition : sig
  type t = {
    identity_ids : Proto_id.t list option;
        (** Keep the submissions sent from one of these Identities. *)
    email_ids : Proto_id.t list option;
        (** Keep the submissions of one of these Emails. *)
    thread_ids : Proto_id.t list option;
        (** Keep the submissions of an Email in one of these Threads. *)
    undo_status : undo_status option;
        (** Keep the submissions in this state. *)
    before : Ptime.t option;
        (** Keep the submissions whose [sendAt] is before this time. *)
    after : Ptime.t option;
        (** Keep the submissions whose [sendAt] is at or after this time. *)
  }
  (** The type for the FilterCondition of an [EmailSubmission/query] (RFC 8621
      Section 7.3). A field of [None] does not filter. *)

  val empty : t
  (** [empty] is the condition with every field unset, which Section 7.3 makes
      "automatically true for all objects". Build a condition from it with
      record update syntax, as in [{ empty with email_ids = Some [ id ] }]. *)

  val jsont : t Jsont.t
  (** [jsont] is the codec for an EmailSubmission FilterCondition. *)
end

type filter = Filter_condition.t Proto_filter.filter
(** The type for the [filter] argument of an [EmailSubmission/query]. *)

val filter_jsont : filter Jsont.t
(** [filter_jsont] is the codec for the [filter] argument of an
    [EmailSubmission/query]. *)

val filter :
  ?identity_ids:Proto_id.t list ->
  ?email_ids:Proto_id.t list ->
  ?thread_ids:Proto_id.t list ->
  ?undo_status:undo_status ->
  ?before:Ptime.t ->
  ?after:Ptime.t ->
  unit ->
  filter
(** [filter ()] is the {!type-filter} of one {!Filter_condition} keeping the
    EmailSubmissions that satisfy every argument given, as one condition of RFC
    8621 Section 7.3. An argument left out sets no field and so filters nothing.
    [filter ()] keeps every EmailSubmission. *)

type sort_property = [ `Email_id | `Thread_id | `Sent_at ]
(** The type for the properties an [EmailSubmission/query] sorts on, which are
    the three RFC 8621 Section 7.3 requires a server to support. [`Sent_at] is
    named [sentAt] there, where the property of the object itself is [sendAt].
*)

val sort :
  ?ascending:bool ->
  ?collation:string ->
  sort_property ->
  Proto_filter.comparator
(** [sort p] is the comparator ordering an [EmailSubmission/query] on [p].
    [ascending] defaults to [true], and only [false] makes the comparator
    descending. [collation] is left to the server unless given. *)
