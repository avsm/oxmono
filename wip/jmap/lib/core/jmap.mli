@@ portable

(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** JMAP for OCaml.

    JMAP is the JSON Meta Application Protocol of
    {{:https://datatracker.ietf.org/doc/html/rfc8620} RFC 8620}, whose mail data
    model is {{:https://datatracker.ietf.org/doc/html/rfc8621} RFC 8621}.
    {!Proto} holds one module per protocol object, each with the type of the
    object, the functions that build and inspect it and its JSON codec. {!Chain}
    builds a request whose method calls take their arguments from the results of
    earlier calls in the same request. *)

(** Protocol and mail objects.

    Every module of this sheet is an alias for a module of the [jmap] library.
    The aliases are the names to use. *)
module Proto : sig
  (** {1 Protocol (RFC 8620)} *)

  module Id = Proto_id
  (** Record identifiers. *)

  module Int53 = Proto_int53
  (** Integers a JSON number represents exactly. *)

  module Json = Proto_json
  (** JMAP JSON text encoding and decoding. *)

  module Date = Proto_date
  (** Dates and times. *)

  module Json_map = Proto_json_map
  (** Objects whose member names are record identifiers. *)

  module Template = Proto_template
  (** URI template expansion. *)

  module Unknown = Proto_unknown
  (** The members of a decoded object no field of this library holds. *)

  module Error = Proto_error
  (** Request level, method level and [/set] errors. *)

  module Capability = Proto_capability
  (** The capabilities a server and an account advertise. *)

  module Filter = Proto_filter
  (** Query filters and sort comparators. *)

  module Method = Proto_method
  (** The arguments and responses of the standard [/get], [/changes], [/set],
      [/copy], [/query] and [/queryChanges] methods. *)

  module Patch = Proto_patch
  (** PatchObjects, the values of a [/set] [update] map. *)

  module Invocation = Proto_invocation
  (** Method invocations and result references. *)

  module Request = Proto_request
  (** Requests. *)

  module Response = Proto_response
  (** Responses. *)

  module Session = Proto_session
  (** The session resource. *)

  module Push = Proto_push
  (** Push subscriptions and state change notifications. *)

  module Blob = Proto_blob
  (** Blob upload, download and copy. *)

  (** {1 Mail (RFC 8621)} *)

  module Keyword = Mail_keyword
  (** Email keywords. *)

  module Email_address = Mail_address
  (** The addresses of an Email header field. *)

  module Email_header = Mail_header
  (** Email header fields and the forms they are read in. *)

  module Email_body = Mail_body
  (** Email body parts and body values. *)

  module Mailbox = Mail_mailbox
  (** Mailboxes. *)

  module Thread = Mail_thread
  (** Threads. *)

  module Email = Mail_email
  (** Emails. *)

  module Search_snippet = Mail_snippet
  (** Search snippets. *)

  module Identity = Mail_identity
  (** The identities a user may send mail from. *)

  module Submission = Mail_submission
  (** Email submissions. *)

  module Vacation = Mail_vacation
  (** The vacation response. *)

  module Address_book = Contacts_addressbook
  module Contact_card = Contacts_card

  (** {1 Calendars (draft-ietf-jmap-calendars-28)} *)

  module Calendar_types = Calendar_types
  (** Shared JSCalendar types, including recurrence rules and alerts. *)
  module Calendar = Calendar_calendar
  module Calendar_event = Calendar_event
  module Participant_identity = Calendar_participant_identity
end

module Chain = Chain
(** Requests whose method calls refer to each other's results. *)

module Mirror = Mirror
(** Storage-independent, restartable synchronisation. *)
