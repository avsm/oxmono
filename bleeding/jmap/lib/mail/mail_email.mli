@@ portable

(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Emails.

    {{:https://datatracker.ietf.org/doc/html/rfc8621#section-4} RFC 8621 Section
     4} defines the Email object, which is a message in the mail store seen at
    once as metadata, as parsed header fields and as a MIME body structure.

    @canonical Jmap.Proto.Email *)

(** {1 Properties} *)

type metadata_property =
  [ `Id
  | `Blob_id
  | `Thread_id
  | `Mailbox_ids
  | `Keywords
  | `Size
  | `Received_at ]
(** The type for the properties of RFC 8621 Section 4.1.1, which describe the
    message in the mail store rather than the message itself. *)

type header_convenience_property =
  [ `Message_id
  | `In_reply_to
  | `References
  | `Sender
  | `From
  | `To
  | `Cc
  | `Bcc
  | `Reply_to
  | `Subject
  | `Sent_at
  | `Headers ]
(** The type for the properties of RFC 8621 Section 4.1.3, each a shorthand for
    one [header:{name}:{form}] property. [`Subject] is [header:Subject:asText],
    [`Sent_at] is [header:Date:asDate], and [`Headers] is every field in raw
    form. *)

type body_property =
  [ `Body_structure
  | `Body_values
  | `Text_body
  | `Html_body
  | `Attachments
  | `Has_attachment
  | `Preview ]
(** The type for the properties of RFC 8621 Section 4.1.4, which describe the
    body of the message. *)

type standard_property =
  [ metadata_property | header_convenience_property | body_property ]
(** The type for the properties RFC 8621 gives the Email object a name for. *)

type header_property = [ `Header of Mail_header.header_property ]
(** The type for a [header:{name}:{form}:all] property. Build one with
    {!Jmap.Proto.Email_header.raw} and its siblings. *)

type property = [ standard_property | header_property ]
(** The type for the properties an [Email/get] may ask for. *)

val property_to_string : [< property ] -> string
(** [property_to_string p] is the wire name of [p], such as ["from"]. *)

val property_of_string : string -> property option
(** [property_of_string s] is the property whose wire name is [s], or [None] if
    there is none. Both a standard property and a [header:*] one are recognised.
*)

val standard_property_of_string : string -> standard_property option
(** [standard_property_of_string s] is {!property_of_string} restricted to the
    standard properties, so a [header:*] name is [None]. *)

(** {1 Body part properties} *)

type standard_body_part_property =
  [ `Part_id
  | `Blob_id
  | `Size
  | `Part_headers
  | `Name
  | `Type
  | `Charset
  | `Disposition
  | `Cid
  | `Language
  | `Location
  | `Sub_parts ]
(** The type for the EmailBodyPart properties of RFC 8621 Section 4.1.4.
    [`Part_headers] is the property named [headers] on the wire. *)

type body_part_property = [ standard_body_part_property | header_property ]
(** The type for the properties the [bodyProperties] argument of an [Email/get]
    may ask for. RFC 8621 Section 4.1.4 also allows properties naming individual
    header fields of the part, "following the same syntax and semantics as for
    the Email object", such as [header:Content-Type]. *)

val standard_body_part_property_to_string :
  [< standard_body_part_property ] -> string
(** [standard_body_part_property_to_string p] is the wire name of [p], such as
    ["subParts"]. *)

val standard_body_part_property_of_string :
  string -> standard_body_part_property option
(** [standard_body_part_property_of_string s] is the standard body part property
    whose wire name is [s], or [None] if there is none. *)

val body_part_property_to_string : [< body_part_property ] -> string
(** [body_part_property_to_string p] is the wire name of [p]. *)

val body_part_property_of_string : string -> body_part_property option
(** [body_part_property_of_string s] is the body part property whose wire name
    is [s], or [None] if there is none. A [header:*] name is recognised. *)

(** {1 Emails} *)

type t = {
  id : Proto_id.t option;  (** The server assigned id of the Email. *)
  blob_id : Proto_id.t option;
      (** The blob holding the raw RFC 5322 message. *)
  thread_id : Proto_id.t option;  (** The Thread the Email belongs to. *)
  size : int64 option;  (** The size of the raw message in octets. *)
  received_at : Ptime.t option;
      (** The time the message arrived at the mail store. It is set by the
          client on creation and is immutable thereafter. *)
  mailbox_ids : (Proto_id.t * bool) list option;
      (** The Mailboxes the Email is in, each mapped to [true]. Build one with
          {!of_mailboxes}. *)
  keywords : (Mail_keyword.t * bool) list option;
      (** The keywords set on the Email, each mapped to [true]. Build one with
          {!Jmap.Proto.Keyword.of_list}. *)
  message_id : string list option;
      (** The Message-ID field, its angle brackets removed. *)
  in_reply_to : string list option;  (** The In-Reply-To field. *)
  references : string list option;  (** The References field. *)
  sender : Mail_address.t list option;  (** The Sender field. *)
  from : Mail_address.t list option;  (** The From field. *)
  to_ : Mail_address.t list option;  (** The To field. *)
  cc : Mail_address.t list option;  (** The Cc field. *)
  bcc : Mail_address.t list option;  (** The Bcc field. *)
  reply_to : Mail_address.t list option;  (** The Reply-To field. *)
  subject : string option;  (** The Subject field, decoded. *)
  sent_at : Ptime.t option;  (** The Date field. *)
  headers : Mail_header.t list option;
      (** Every header field of the message, in the order they appear. *)
  body_structure : Mail_body.Part.t option;
      (** The MIME structure of the message as a tree of parts. *)
  body_values : (string * Mail_body.Value.t) list option;
      (** The content of the parts the [Email/get] asked to have fetched, keyed
          by their [part_id]. *)
  text_body : Mail_body.Part.t list option;
      (** The parts to display as the plain text body. *)
  html_body : Mail_body.Part.t list option;
      (** The parts to display as the HTML body. *)
  attachments : Mail_body.Part.t list option;
      (** The parts to present as attachments. *)
  has_attachment : bool option;
      (** [true] if the message has at least one part the server considers an
          attachment. *)
  preview : string option;
      (** An extract of the beginning of the body, for a message list. *)
  dynamic_headers : (string * Jsont.json) list;
      (** The values of the [header:*] properties the [Email/get] asked for,
          keyed by the full property name, such as ["header:X-Custom:asText"].
          Read one with {!find_header} and its siblings. *)
  unknown : Proto_unknown.t;
      (** The members not defined above and not naming a [header:*] property,
          kept verbatim. See {!unknown_member}. *)
}
(** The type for Email objects. A property is [None] when the [Email/get] did
    not ask for it, and, for the header fields, also when the message does not
    carry the field. *)

val v :
  ?id:Proto_id.t ->
  ?blob_id:Proto_id.t ->
  ?thread_id:Proto_id.t ->
  ?size:int64 ->
  ?received_at:Ptime.t ->
  ?mailbox_ids:(Proto_id.t * bool) list ->
  ?keywords:(Mail_keyword.t * bool) list ->
  ?message_id:string list ->
  ?in_reply_to:string list ->
  ?references:string list ->
  ?sender:Mail_address.t list ->
  ?from:Mail_address.t list ->
  ?to_:Mail_address.t list ->
  ?cc:Mail_address.t list ->
  ?bcc:Mail_address.t list ->
  ?reply_to:Mail_address.t list ->
  ?subject:string ->
  ?sent_at:Ptime.t ->
  ?headers:Mail_header.t list ->
  ?body_structure:Mail_body.Part.t ->
  ?body_values:(string * Mail_body.Value.t) list ->
  ?text_body:Mail_body.Part.t list ->
  ?html_body:Mail_body.Part.t list ->
  ?attachments:Mail_body.Part.t list ->
  ?has_attachment:bool ->
  ?preview:string ->
  ?dynamic_headers:(string * Jsont.json) list ->
  ?unknown:Proto_unknown.t ->
  unit ->
  t
(** [v ()] is an Email with only the properties given set, every other one being
    [None], which {!jsont} omits rather than encoding as [null].
    [dynamic_headers] defaults to the empty list and [unknown] to
    {!Jmap.Proto.Unknown.empty}.

    An [Email/set] create object is built this way (RFC 8621 Section 4.6). "The
    client MUST omit any properties that may only be set by the server", so
    leave [id], [blob_id], [thread_id], [size], [has_attachment] and [preview]
    unset there. The body of a new message is a [body_values] entry keyed by a
    part id together with a [text_body] part naming it.
    {[
    Email.v
      ~mailbox_ids:(Email.of_mailboxes [ drafts_id ])
      ~keywords:(Keyword.of_list [ `Draft ])
      ~subject:"Hello" ~from:[ me ] ~to_:[ you ]
      ~body_values:[ ("1", Email_body.Value.v "Hello, world.") ]
      ~text_body:[ Email_body.Part.v ~part_id:"1" ~type_:"text/plain" () ]
      ()
    ]}

    @raise Invalid_argument
      if a key of [dynamic_headers] does not start with ["header:"], since it
      would then encode as a second member of a name the object already defines.
*)

val empty : t
(** [empty] is {!v} with every property unset. *)

val id : t -> Proto_id.t option
(** [id e] is the id of the Email [e], or [None] if the [Email/get] did not ask
    for it. *)

val creation : string -> t Proto_id.creation
(** [creation s] is {!Jmap.Proto.Id.val-creation} [s] as the creation id of an
    Email. Binding it here rather than through [Id.creation] fixes the type of
    the record it names at the binding, so a creation id defined before the
    [/set] that uses it needs no annotation. *)

val of_mailboxes : Proto_id.t list -> (Proto_id.t * bool) list
(** [of_mailboxes ids] is the [Id[Boolean]] map of RFC 8621 Section 4.1.1 that
    puts an Email in exactly the Mailboxes [ids]. *)

val create :
  mailbox_ids:Proto_id.t list ->
  ?keywords:Mail_keyword.t list ->
  ?sender:Mail_address.t list ->
  ?from:Mail_address.t list ->
  ?to_:Mail_address.t list ->
  ?cc:Mail_address.t list ->
  ?bcc:Mail_address.t list ->
  ?reply_to:Mail_address.t list ->
  ?subject:string ->
  ?sent_at:Ptime.t ->
  ?in_reply_to:string list ->
  ?references:string list ->
  ?text_body:string ->
  ?html_body:string ->
  unit ->
  t
(** [create ~mailbox_ids ()] is the Email of an [Email/set] create. Every other
    property is absent unless given.

    Only client settable properties are exposed, RFC 8620 Section 5.3 requiring
    that "the client MUST omit any properties that may only be set by the
    server". A [text_body] or [html_body] becomes the [bodyValues] entry and the
    matching [textBody] or [htmlBody] part that RFC 8621 Section 4.6 asks for.
    Build the record with {!v} for anything richer, such as an attachment or a
    header field [create] does not name.

    @raise Stdlib.exception-Invalid_argument
      if [mailbox_ids] is empty, or if a keyword fails
      {!Jmap.Proto.Keyword.validate} or duplicates another keyword without
      regard to case. *)

val keyword_list : t -> Mail_keyword.t list
(** [keyword_list e] is the keywords [e] carries, which are those its [keywords]
    map sends to [true]. It is [[]] when the [Email/get] did not ask for the
    property. *)

val has_keyword : Mail_keyword.t -> t -> bool
(** [has_keyword k e] is [true] if [keyword_list e] holds [k]. Keywords are
    compared with {!Jmap.Proto.Keyword.equal}, which ignores case. *)

val mailbox_list : t -> Proto_id.t list
(** [mailbox_list e] is the Mailboxes [e] is in, which are those its
    [mailboxIds] map sends to [true]. It is [[]] when the [Email/get] did not
    ask for the property. *)

val in_mailbox : Proto_id.t -> t -> bool
(** [in_mailbox id e] is [true] if [mailbox_list e] holds [id]. *)

val body_value : t -> Mail_body.Part.t -> Mail_body.Value.t option
(** [body_value e p] is the content of the body part [p] of [e], the entry of
    the [body_values] of [e] under the [part_id] of [p]. It is [None] if [p] has
    no [part_id], or if the [Email/get] did not fetch the value of [p]. RFC 8621
    Section 4.1.4 fetches a value only when [fetchTextBodyValues],
    [fetchHTMLBodyValues] or [fetchAllBodyValues] asks for it. *)

val unknown_member : t -> string -> Jsont.json option
(** [unknown_member e name] is the value of the member [name] of [e] that is
    neither one of the properties above nor a [header:*] property, or [None] if
    [e] has no such member. *)

(** {1 Dynamic header values} *)

val decode_header_value :
  string -> Jsont.json -> Mail_header.header_value option
(** [decode_header_value name json] is the value [json] read in the form the
    property [name] asks for, or [None] if [name] is not a well formed
    [header:*] property name or [json] does not have the shape that form gives
    it. *)

val find_header : t -> string -> Mail_header.header_value option
(** [find_header e name] is the value of the [header:*] property [name] of [e],
    or [None] if [e] has no such property or its value could not be read. [name]
    must be given exactly as it was asked for, as in
    [find_header e "header:X-Custom:asText"]. *)

val find_header_string : t -> string -> string option
(** [find_header_string e name] is {!find_header} of [name] when it was asked
    for in the raw or [asText] form and the message carries the field, and
    [None] otherwise. *)

val find_header_addresses : t -> string -> Mail_address.t list option
(** [find_header_addresses e name] is {!find_header} of [name] when it was asked
    for in the [asAddresses] form of RFC 8621 Section 4.1.2.3 and the message
    carries the field, and [None] otherwise. *)

val find_header_grouped_addresses :
  t -> string -> Mail_address.Group.t list option
(** [find_header_grouped_addresses e name] is {!find_header} of [name] when it
    was asked for in the [asGroupedAddresses] form of RFC 8621 Section 4.1.2.4
    and the message carries the field, and [None] otherwise. *)

val find_header_strings : t -> string -> string list option
(** [find_header_strings e name] is {!find_header} of [name] when it was asked
    for in the [asMessageIds] form of RFC 8621 Section 4.1.2.5 or the [asURLs]
    form of Section 4.1.2.7 and the message carries the field, and [None]
    otherwise. The two forms share one decoded shape, the message ids having
    "the surrounding angle brackets [...] removed". *)

val find_header_date : t -> string -> Ptime.t option
(** [find_header_date e name] is {!find_header} of [name] when it was asked for
    in the [asDate] form of RFC 8621 Section 4.1.2.6 and the message carries the
    field, and [None] otherwise. *)

val find_header_all : t -> string -> string list option
(** [find_header_all e name] is {!find_header} of [name] when it was asked for
    in the raw or [asText] form with the [:all] suffix of RFC 8621 Section
    4.1.3, which "returns all instances of the header [...] rather than just the
    last one", and [None] otherwise. A field the message does not carry is
    [Some []]. *)

val find_header_text : t -> string -> string option
(** [find_header_text e name] is {!Jmap.Proto.Email_header.value_to_string} of
    {!find_header} of [name], whatever form it was asked for in. It is the
    accessor to reach for when a program only wants to show the value. The form
    specific ones above are for programs that need the parsed data. *)

val jsont : t Jsont.t
(** [jsont] is the codec for an Email. Every property may be absent, since a
    client chooses the [properties] of an [Email/get], and every one that RFC
    8621 types [T|null] accepts an explicit [null] and decodes it to [None].
    Members naming a [header:*] property are kept in {!field-dynamic_headers}
    and any other member it does not define in {!field-unknown}. *)

(** {1 Queries} *)

(** Filter conditions for an [Email/query]. *)
module Filter_condition : sig
  type t = {
    in_mailbox : Proto_id.t option;  (** Keep the Emails in this Mailbox. *)
    in_mailbox_other_than : Proto_id.t list option;
        (** Keep the Emails that are in at least one Mailbox outside these. *)
    before : Ptime.t option;
        (** Keep the Emails whose [receivedAt] is before this time. *)
    after : Ptime.t option;
        (** Keep the Emails whose [receivedAt] is at or after this time. *)
    min_size : int64 option;
        (** Keep the Emails whose [size] is this or more. *)
    max_size : int64 option;  (** Keep the Emails whose [size] is below this. *)
    all_in_thread_have_keyword : Mail_keyword.t option;
        (** Keep the Emails whose Thread has this keyword on every Email. *)
    some_in_thread_have_keyword : Mail_keyword.t option;
        (** Keep the Emails whose Thread has this keyword on at least one Email.
        *)
    none_in_thread_have_keyword : Mail_keyword.t option;
        (** Keep the Emails whose Thread has this keyword on no Email. *)
    has_keyword : Mail_keyword.t option;
        (** Keep the Emails with this keyword. *)
    not_keyword : Mail_keyword.t option;
        (** Keep the Emails without this keyword. *)
    has_attachment : bool option;
        (** Keep the Emails that have an attachment, or those that have none. *)
    text : string option;
        (** Keep the Emails whose header fields or body hold this text. *)
    from : string option;
        (** Keep the Emails whose From field holds this text. *)
    to_ : string option;  (** Keep the Emails whose To field holds this text. *)
    cc : string option;  (** Keep the Emails whose Cc field holds this text. *)
    bcc : string option;
        (** Keep the Emails whose Bcc field holds this text. *)
    subject : string option;
        (** Keep the Emails whose Subject field holds this text. *)
    body : string option;  (** Keep the Emails whose body holds this text. *)
    header : (string * string option) option;
        (** Keep the Emails that carry this header field, and, when a value is
            given, whose field holds it. *)
  }
  (** The type for the FilterCondition of an [Email/query] (RFC 8621 Section
      4.4.1). A field of [None] does not filter. *)

  val empty : t
  (** [empty] is the condition with every field unset, which Section 4.4.1 makes
      true for every Email. Build a condition from it with record update syntax,
      as in [{ empty with subject = Some "x" }]. *)

  val jsont : t Jsont.t
  (** [jsont] is the codec for an Email FilterCondition. *)
end

type filter = Filter_condition.t Proto_filter.filter
(** The type for the [filter] argument of an [Email/query]. *)

val filter_jsont : filter Jsont.t
(** [filter_jsont] is the codec for the [filter] argument of an [Email/query].
*)

val filter :
  ?in_mailbox:Proto_id.t ->
  ?in_mailbox_other_than:Proto_id.t list ->
  ?before:Ptime.t ->
  ?after:Ptime.t ->
  ?min_size:int64 ->
  ?max_size:int64 ->
  ?all_in_thread_have_keyword:Mail_keyword.t ->
  ?some_in_thread_have_keyword:Mail_keyword.t ->
  ?none_in_thread_have_keyword:Mail_keyword.t ->
  ?has_keyword:Mail_keyword.t ->
  ?not_keyword:Mail_keyword.t ->
  ?has_attachment:bool ->
  ?text:string ->
  ?from:string ->
  ?to_:string ->
  ?cc:string ->
  ?bcc:string ->
  ?subject:string ->
  ?body:string ->
  ?header:string * string option ->
  unit ->
  filter
(** [filter ()] is the {!type-filter} of one {!Filter_condition} keeping the
    Emails that satisfy every argument given, as one condition of RFC 8621
    Section 4.4.1. An argument left out sets no field and so filters nothing.
    [filter ()] keeps every Email. *)

type sort_property =
  [ `Received_at
  | `Size
  | `From
  | `To
  | `Subject
  | `Sent_at
  | `Has_keyword of Mail_keyword.t
  | `All_in_thread_have_keyword of Mail_keyword.t
  | `Some_in_thread_have_keyword of Mail_keyword.t ]
(** The type for the properties an [Email/query] sorts on, listed by RFC 8621
    Section 4.4.2. A server must support [`Received_at] and should support the
    rest. The [emailQuerySortOptions] field of the mail capability of the
    account names the ones it does support. *)

val sort :
  ?ascending:bool ->
  ?collation:string ->
  sort_property ->
  Proto_filter.comparator
(** [sort p] is the comparator ordering an [Email/query] on [p]. [ascending]
    defaults to [true], and only [false] makes the comparator descending.
    [collation] is left to the server unless given. A keyword sort carries its
    keyword in the [keyword] member of the comparator, which RFC 8621 Section
    4.4.2 requires of it. *)

(** {1 Email/get} *)

type get_args_extra = {
  body_properties : body_part_property list option;
      (** The properties to return for each body part. [None] leaves the server
          to return the default set of RFC 8621 Section 4.2. *)
  fetch_text_body_values : bool;
      (** [true] to fetch the content of every text part of {!field-text_body}.
      *)
  fetch_html_body_values : bool;
      (** [true] to fetch the content of every text part of {!field-html_body}.
      *)
  fetch_all_body_values : bool;
      (** [true] to fetch the content of every text part of the message. *)
  max_body_value_bytes : int64 option;
      (** The number of octets to truncate a fetched value at. [None] fetches it
          whole. *)
}
(** The type for the arguments an [Email/get] takes beyond those of a standard
    [/get] call (RFC 8621 Section 4.2). The [properties] argument of the
    standard call names {!property} values through {!property_to_string}. *)

val get_args_extra :
  ?body_properties:body_part_property list ->
  ?fetch_text_body_values:bool ->
  ?fetch_html_body_values:bool ->
  ?fetch_all_body_values:bool ->
  ?max_body_value_bytes:int64 ->
  unit ->
  get_args_extra
(** [get_args_extra ()] is the extra arguments given. The three fetch flags
    default to [false] and the other two to absent. *)

val get_args_extra_jsont : get_args_extra Jsont.t
(** [get_args_extra_jsont] is the codec for the extra arguments of an
    [Email/get]. A fetch flag that is [false] is omitted. *)

(** {1 Email/import}

    {{:https://datatracker.ietf.org/doc/html/rfc8621#section-4.8} RFC 8621
     Section 4.8}: "The [Email/import] method adds messages [RFC5322] to the set
    of Emails in an account. [...] The messages must first be uploaded as blobs
    using the standard upload mechanism." *)

(** The arguments and response of an [Email/import] call. *)
module Import : sig
  type email = {
    blob_id : Proto_id.t;  (** The blob holding the raw message. *)
    mailbox_ids : (Proto_id.t * bool) list;
        (** The Mailboxes to file the Email in. At least one must be given. *)
    keywords : (Mail_keyword.t * bool) list;
        (** The keywords to set on the Email. *)
    received_at : Ptime.t option;
        (** The time to record the Email as having arrived at. [None] leaves the
            server to use "the time of most recent Received header, or time of
            import on server if none". *)
  }
  (** The type for EmailImport objects. *)

  val email :
    blob_id:Proto_id.t ->
    mailbox_ids:Proto_id.t list ->
    ?keywords:Mail_keyword.t list ->
    ?received_at:Ptime.t ->
    unit ->
    email
  (** [email ~blob_id ~mailbox_ids ()] is an EmailImport with every Mailbox and
      keyword given mapped to [true]. [keywords] defaults to the empty list and
      [received_at] to absent.

      @raise Stdlib.exception-Invalid_argument
        if [mailbox_ids] is empty or holds the same id twice, or if a keyword
        fails {!Jmap.Proto.Keyword.validate} or duplicates another keyword
        without regard to case. *)

  val email_jsont : email Jsont.t
  (** [email_jsont] is the codec for an EmailImport. An empty [keywords] is
      omitted, RFC 8621 Section 4.8 giving it a default of [{}]. *)

  type args = {
    account_id : Proto_id.t;  (** The account to import into. *)
    if_in_state : string option;
        (** The state the account must be in for the call to go ahead. "The
            string must match the current state of the account [...]; otherwise,
            the method will be aborted and a [stateMismatch] error returned." *)
    emails : (t Proto_id.creation * email) list;
        (** "A map of creation id (client specified) to EmailImport objects." *)
  }
  (** The type for the arguments of an [Email/import] call. *)

  val args :
    account_id:Proto_id.t ->
    ?if_in_state:string ->
    emails:(t Proto_id.creation * email) list ->
    unit ->
    args
  (** [args ~account_id ~emails ()] is the arguments of an [Email/import] call.
      [if_in_state] defaults to absent, which imports whatever state the account
      is in. *)

  val args_jsont : args Jsont.t
  (** [args_jsont] is the codec for the arguments of an [Email/import] call. *)

  type response = {
    account_id : Proto_id.t;  (** The account the call was made on. *)
    old_state : string option;
        (** The state the account was in before the call. *)
    new_state : string;  (** The state the account is in after the call. *)
    created : (Proto_id.t * t) list option;
        (** The Emails that were imported, keyed by creation id, each carrying
            its [id], [blobId], [threadId] and [size], or [None] if the server
            sent no map. An empty map decodes as [Some []]. *)
    not_created : (Proto_id.t * Proto_error.Set_error.t) list option;
        (** The imports that failed, keyed by creation id. An [alreadyExists]
            error carries the id of the existing Email in the [existing_id]
            field of {!Jmap.Proto.Error.Set_error.t}. *)
  }
  (** The type for the response arguments of an [Email/import] call. *)

  val response_jsont : response Jsont.t
  (** [response_jsont] is the codec for the response arguments of an
      [Email/import] call. *)

  val created : response -> t Proto_id.creation -> t option
  (** [created r c] is the Email imported under the creation id [c], or [None]
      if [c] names no imported Email. *)

  val not_created :
    response -> t Proto_id.creation -> Proto_error.Set_error.t option
  (** [not_created r c] is the error for the creation id [c], or [None] if the
      import under [c] did not fail. *)
end

(** {1 Email/parse}

    {{:https://datatracker.ietf.org/doc/html/rfc8621#section-4.9} RFC 8621
     Section 4.9}: "This method allows you to parse blobs as messages [RFC5322]
    to get Email objects. [...] This can be used to parse and display attached
    messages without having to import them as top-level Email objects in the
    mail store in their own right."

    The [id], [mailboxIds], [keywords] and [receivedAt] properties of a parsed
    Email are null if asked for, and so decode as [None]. *)

(** The arguments and response of an [Email/parse] call. *)
module Parse : sig
  type args = {
    account_id : Proto_id.t;  (** The account to parse in. *)
    blob_ids : Proto_id.t list;  (** The blobs to parse. *)
    properties : string list option;
        (** The properties to return for each Email. "If supplied, only the
            properties listed in the array are returned for each Email object."
        *)
    extra : get_args_extra;
        (** The body arguments, which [Email/parse] takes as [Email/get] does.
        *)
  }
  (** The type for the arguments of an [Email/parse] call. *)

  val args :
    account_id:Proto_id.t ->
    blob_ids:Proto_id.t list ->
    ?properties:string list ->
    ?body_properties:body_part_property list ->
    ?fetch_text_body_values:bool ->
    ?fetch_html_body_values:bool ->
    ?fetch_all_body_values:bool ->
    ?max_body_value_bytes:int64 ->
    unit ->
    args
  (** [args ~account_id ~blob_ids ()] is the arguments of an [Email/parse] call.
      The body arguments default as they do in {!val-get_args_extra}. *)

  val args_jsont : args Jsont.t
  (** [args_jsont] is the codec for the arguments of an [Email/parse] call. *)

  type response = {
    account_id : Proto_id.t;  (** The account the call was made on. *)
    parsed : (Proto_id.t * t) list option;
        (** The Emails parsed, keyed by blob id. *)
    not_parsable : Proto_id.t list option;
        (** The blobs that "could not be parsed as Emails". *)
    not_found : Proto_id.t list option;
        (** "A list of blob ids given that could not be found." *)
  }
  (** The type for the response arguments of an [Email/parse] call. Every map
      and list is [T|null], so each is [None] when the server sent an explicit
      [null] rather than an empty map or list. *)

  val response_jsont : response Jsont.t
  (** [response_jsont] is the codec for the response arguments of an
      [Email/parse] call. *)

  val parsed : response -> Proto_id.t -> t option
  (** [parsed r blob_id] is the Email parsed from [blob_id], or [None] if
      [blob_id] was not parsed. *)
end

(** {1 Patches}

    RFC 8621 Section 4.1.1 types [keywords] [String[Boolean]] and [mailboxIds]
    [Id[Boolean]], so one keyword or one Mailbox membership is patched without
    resending the whole map. *)

module Patch : sig
  (** The entries of a PatchObject on an Email. *)

  val set_keyword : Mail_keyword.t -> Proto_patch.entry
  (** [set_keyword k] is the patch setting ["keywords/" ^ k] to [true].

      @raise Invalid_argument if [k] fails {!Jmap.Proto.Keyword.validate}. *)

  val remove_keyword : Mail_keyword.t -> Proto_patch.entry
  (** [remove_keyword k] is the patch setting ["keywords/" ^ k] to [null], which
      RFC 8620 Section 5.3 makes a removal, and "if the key is not present in
      the parent, this a no-op".

      @raise Invalid_argument if [k] fails {!Jmap.Proto.Keyword.validate}. *)

  val set_keywords : Mail_keyword.t list -> Proto_patch.entry
  (** [set_keywords ks] is the patch replacing the whole [keywords] map, so that
      the Email ends up with exactly the keywords [ks].

      @raise Invalid_argument
        if a keyword of [ks] fails {!Jmap.Proto.Keyword.validate}. *)

  val add_to_mailbox : Proto_id.t -> Proto_patch.entry
  (** [add_to_mailbox id] is the patch setting ["mailboxIds/" ^ id] to [true].
  *)

  val remove_from_mailbox : Proto_id.t -> Proto_patch.entry
  (** [remove_from_mailbox id] is the patch setting ["mailboxIds/" ^ id] to
      [null].

      RFC 8621 Section 4.1.1 says an Email "MUST belong to at least one
      Mailbox", so a patch that empties [mailboxIds] is rejected by the server
      with an [invalidProperties] SetError. Pair the removal with an
      {!add_to_mailbox} in the same patch to move a message. *)

  val set_mailboxes : Proto_id.t list -> Proto_patch.entry
  (** [set_mailboxes ids] is the patch replacing the whole [mailboxIds] map, so
      that the Email ends up in exactly the Mailboxes [ids].

      @raise Invalid_argument if [ids] contains the same identifier twice. *)
end
