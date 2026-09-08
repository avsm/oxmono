@@ portable

(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** JMAP errors.

    A JMAP failure is reported at one of three levels. A request level error is
    an HTTP error status with an RFC 7807 problem details body, defined by
    {{:https://datatracker.ietf.org/doc/html/rfc8620#section-3.6.1} RFC 8620
     Section 3.6.1}. A method level error replaces the response of a single
    method call, per
    {{:https://datatracker.ietf.org/doc/html/rfc8620#section-3.6.2} Section
     3.6.2}. A SetError reports the failure of one record within a [/set],
    [/copy] or [/import] response, per
    {{:https://datatracker.ietf.org/doc/html/rfc8620#section-5.3} Section 5.3}.

    The three modules have the same shape. Each has a [type_] of error types, a
    record [t], a checked constructor [v], a [validate], a [jsont] codec and a
    printer. Decoding is tolerant throughout. It never applies [validate], so an
    error a nonconformant server sends is still readable.

    @canonical Jmap.Proto.Error *)

val pp_escaped : Format.formatter -> string -> unit
(** [pp_escaped ppf s] prints [s], preserving printable bytes and UTF-8 but
    rendering C0, DEL, and C1 control bytes visibly. It is suitable for
    server-supplied text in a one-line diagnostic. *)

(** Request level errors. *)
module Request_error : sig
  type type_ =
    [ `Unknown_capability
      (** [urn:ietf:params:jmap:error:unknownCapability]. The client named a
          capability in [using] that the server does not support. *)
    | `Not_json
      (** [urn:ietf:params:jmap:error:notJSON]. The content type was not
          [application/json] or the body was not valid JSON. *)
    | `Not_request
      (** [urn:ietf:params:jmap:error:notRequest]. The body was valid JSON but
          not a valid JMAP Request object. *)
    | `Limit
      (** [urn:ietf:params:jmap:error:limit]. A server defined limit was
          reached. *)
    | `Other of string  (** A URN outside the set above. *) ]
  (** The type for request level error types. *)

  val type_to_string : type_ -> string
  (** [type_to_string t] is the URN of [t]. *)

  val type_of_string : string -> type_
  (** [type_of_string s] is the request level error type whose URN is [s], and
      [`Other s] if [s] is not one of the four defined URNs. *)

  type t = {
    type_ : type_;  (** The error type URN. *)
    status : int option;
        (** The HTTP status code. RFC 7807 makes every member of a problem
            details object optional. *)
    title : string option;  (** A short human readable summary. *)
    detail : string option;  (** A longer human readable explanation. *)
    limit : string option;
        (** The name of the limit that was exceeded, for a [`Limit] error. *)
    unknown : Proto_unknown.t;
        (** The members not defined above, kept verbatim. RFC 7807 Section 3.1
            lets a problem type "extend the problem details object with
            additional members", so they re-encode unchanged. *)
  }
  (** The type for request level errors. *)

  val v :
    ?status:int ->
    ?title:string ->
    ?detail:string ->
    ?limit:string ->
    ?unknown:Proto_unknown.t ->
    type_ ->
    (t, string) result
  (** [v type_] is the request level error [type_], with [status], [title],
      [detail], [limit] and [unknown] absent unless given. [unknown] defaults to
      {!Jmap.Proto.Unknown.empty}. A [`Other] URN that names one of the four
      defined types is normalised to that type. The error holds a human readable
      message if [status] is outside the range 0 to 999, or if {!validate}
      rejects the result. *)

  val validate : t -> (t, string) result
  (** [validate e] is [e] if [e] meets the requirements RFC 8620 Section 3.6.1
      places on a problem details object, and a human readable message
      otherwise. The only such requirement is that a
      [urn:ietf:params:jmap:error:limit] error carry a [limit] member, "the name
      of the limit being applied". *)

  val unknown_member : t -> string -> Jsont.json option
  (** [unknown_member e name] is the value of the extension member [name] of
      [e], or [None] if [e] has no such member. *)

  val jsont : t Jsont.t
  (** [jsont] is the codec for a problem details object. Decoding takes an
      absent [type] member as [`Other "about:blank"], the type RFC 7807 Section
      4.2 implies, and accepts an explicit [null] for [status], [title],
      [detail] and [limit]. It does not apply {!validate}. *)

  val pp : Format.formatter -> t -> unit
  (** [pp ppf e] prints the type URN of [e] on [ppf], followed in parentheses by
      the [detail] of [e], or by its [title] when [detail] is absent. Control
      bytes in these server-supplied strings are escaped. *)

  val to_string : t -> string
  (** [to_string e] is {!pp} as a string. *)
end

(** Method level errors. *)
module Method_error : sig
  type type_ =
    [ `Server_unavailable  (** The server is temporarily unavailable. *)
    | `Server_fail  (** An unexpected error occurred. *)
    | `Server_partial_fail  (** Some, but not all, of the changes were made. *)
    | `Unknown_method  (** The method name is not recognised. *)
    | `Invalid_arguments  (** One or more arguments are invalid. *)
    | `Invalid_result_reference
      (** A result reference could not be resolved. *)
    | `Forbidden  (** The method and its arguments are valid but forbidden. *)
    | `Account_not_found  (** The [accountId] names no account. *)
    | `Account_not_supported_by_method
      (** The account does not support this method. *)
    | `Account_read_only  (** The account is read only. *)
    | `Cannot_calculate_changes
      (** RFC 8620 Sections 5.2 and 5.6. The server cannot calculate the changes
          from the state string given. The client must invalidate its cache. *)
    | `Request_too_large
      (** RFC 8620 Sections 5.1 and 5.3. The number of ids or objects asked for
          exceeds the server maximum. *)
    | `State_mismatch
      (** RFC 8620 Sections 5.3 and 5.4. An [ifInState] or [ifFromInState]
          argument was given and does not match the current state. *)
    | `Anchor_not_found
      (** RFC 8620 Section 5.5. An [anchor] was given and is not in the results.
      *)
    | `Unsupported_sort
      (** RFC 8620 Section 5.5. The server does not support the sort. *)
    | `Unsupported_filter
      (** RFC 8620 Section 5.5. The server does not support the filter. *)
    | `Too_many_changes
      (** RFC 8620 Section 5.6. More changes than [maxChanges] have occurred. *)
    | `From_account_not_found
      (** RFC 8620 Sections 5.4 and 6.3. The [fromAccountId] names no account.
      *)
    | `From_account_not_supported_by_method
      (** RFC 8620 Section 5.4. The [fromAccountId] account does not support
          this data type. *)
    | `Other of string  (** A type outside the set above. *) ]
  (** The type for method level error types. *)

  val type_to_string : type_ -> string
  (** [type_to_string t] is the name of [t]. *)

  val type_of_string : string -> type_
  (** [type_of_string s] is the method level error type named [s], and
      [`Other s] if [s] is not one of the defined names. *)

  type t = {
    type_ : type_;  (** The error type. *)
    description : string option;
        (** A human readable description of the error. *)
    unknown : Proto_unknown.t;
        (** The members not defined above, kept verbatim. RFC 8620 Section 3.6.2
            lets an error type define further properties. *)
  }
  (** The type for method level errors. *)

  val v :
    ?description:string ->
    ?unknown:Proto_unknown.t ->
    type_ ->
    (t, string) result
  (** [v type_] is the method level error [type_], with [description] absent
      unless given and [unknown] defaulting to {!Jmap.Proto.Unknown.empty}. A
      [`Other] name that names one of the defined types is normalised to that
      type. The error holds a human readable message if {!validate} rejects the
      result. *)

  val validate : t -> (t, string) result
  (** [validate e] is [e]. RFC 8620 Section 3.6.2 places no requirement on a
      method level error beyond its [type]. *)

  val unknown_member : t -> string -> Jsont.json option
  (** [unknown_member e name] is the value of the extension member [name] of
      [e], or [None] if [e] has no such member. *)

  val jsont : t Jsont.t
  (** [jsont] is the codec for a method level error. Decoding accepts an
      explicit [null] for [description]. It does not apply {!validate}. *)

  val pp : Format.formatter -> t -> unit
  (** [pp ppf e] prints the type name of [e] on [ppf], followed by the
      [description] of [e] in parentheses when it has one, such as
      [cannotCalculateChanges] or [invalidArguments (unknown sort)]. Control
      bytes in server-supplied strings are escaped. *)

  val to_string : t -> string
  (** [to_string e] is {!pp} as a string. *)
end

(** Record level errors of a [/set], [/copy] or [/import] response. *)
module Set_error : sig
  type type_ =
    [ `Forbidden  (** The operation is not permitted. *)
    | `Over_quota  (** The maximum server quota has been reached. *)
    | `Too_large  (** The object is too large. *)
    | `Rate_limit
      (** Too many objects of this type have been created recently. *)
    | `Not_found  (** The id does not exist. *)
    | `Invalid_patch  (** The PatchObject is invalid. *)
    | `Will_destroy
      (** Another operation in the request will destroy the object. *)
    | `Invalid_properties  (** Some properties were invalid. *)
    | `Singleton  (** Only one object of this type can exist. *)
    | `Already_exists
      (** RFC 8620 Section 5.4. The server forbids duplicates and the record
          already exists in the target account. {!field-existing_id} must name
          the existing record. *)
    | `Mailbox_has_child
      (** RFC 8621 Section 2.5. The Mailbox still has at least one child
          Mailbox. *)
    | `Mailbox_has_email
      (** RFC 8621 Section 2.5. The Mailbox has at least one Email assigned to
          it. *)
    | `Address_book_has_contents
      (** RFC 9610 Section 2.3. The AddressBook has at least one ContactCard
          assigned to it and the [onDestroyRemoveContents] argument was false.
      *)
    | `Blob_not_found
      (** RFC 8621 Section 4.6. At least one blob id given for an EmailBodyPart
          does not exist. {!field-not_found} must list every such blob id. *)
    | `Too_many_keywords
      (** RFC 8621 Section 4.6. The change would exceed the server maximum
          number of keywords on an Email. *)
    | `Too_many_mailboxes
      (** RFC 8621 Section 4.6. The change would exceed the server maximum
          number of Mailboxes an Email may be in. *)
    | `Invalid_email
      (** RFC 8621 Section 7.5. The Email to be sent is invalid.
          {!field-properties} should list every invalid Email property. *)
    | `Too_many_recipients
      (** RFC 8621 Section 7.5. The envelope has more recipients than the server
          allows. {!field-max_recipients} must be present. *)
    | `No_recipients
      (** RFC 8621 Section 7.5. The envelope has no [rcptTo] addresses. *)
    | `Invalid_recipients
      (** RFC 8621 Section 7.5. At least one [rcptTo] value is not a valid
          address to send to. {!field-invalid_recipients} must be present. *)
    | `Cannot_unsend
      (** RFC 8621 Section 7.5. The client tried to move [undoStatus] from
          pending to canceled and the message cannot be unsent. *)
    | `Forbidden_mail_from
      (** RFC 8621. The server does not permit the user to send from the
          address. *)
    | `Forbidden_from
      (** RFC 8621. The server does not permit the user to send a message with
          this From header. *)
    | `Forbidden_to_send
      (** RFC 8621. The user does not have permission to send at all. *)
    | `Other of string  (** A type outside the set above. *) ]
  (** The type for SetError types. *)

  val type_to_string : type_ -> string
  (** [type_to_string t] is the name of [t]. *)

  val type_of_string : string -> type_
  (** [type_of_string s] is the SetError type named [s], and [`Other s] if [s]
      is not one of the defined names. *)

  type t = {
    type_ : type_;  (** The error type. *)
    description : string option;  (** A human readable description. *)
    properties : string list option;
        (** The names of the invalid properties, for [`Invalid_properties] and
            [`Invalid_email]. *)
    existing_id : Proto_id.t option;
        (** The id of the existing record in the target account, for
            [`Already_exists]. *)
    not_found : Proto_id.t list option;
        (** Every referenced [blobId] the server could not find, for
            [`Blob_not_found]. *)
    max_size : int64 option;
        (** The maximum size in octets of a message that may be sent, for
            [`Too_large] on [EmailSubmission/set]. *)
    max_recipients : int64 option;
        (** The maximum number of allowed recipients, for
            [`Too_many_recipients]. *)
    invalid_recipients : string list option;
        (** The addresses that cannot be sent to, for [`Invalid_recipients]. *)
    unknown : Proto_unknown.t;
        (** The members not defined above, kept verbatim. A SetError type may
            define further properties and extensions add their own. *)
  }
  (** The type for SetErrors. *)

  val v :
    ?description:string ->
    ?properties:string list ->
    ?existing_id:Proto_id.t ->
    ?not_found:Proto_id.t list ->
    ?max_size:int64 ->
    ?max_recipients:int64 ->
    ?invalid_recipients:string list ->
    ?unknown:Proto_unknown.t ->
    type_ ->
    (t, string) result
  (** [v type_] is the SetError [type_], with every member absent unless given
      and [unknown] defaulting to {!Jmap.Proto.Unknown.empty}. A [`Other] name
      that names one of the defined types is normalised to that type. The error
      holds a human readable message if [max_size] or [max_recipients] is
      outside the [UnsignedInt] range of 0 to 2{^ 53}-1, or if {!validate}
      rejects the result. *)

  val validate : t -> (t, string) result
  (** [validate e] is [e] if [e] carries the member the specification requires
      for its type, and a human readable message otherwise. Those requirements
      are [existing_id] for [`Already_exists], [not_found] for
      [`Blob_not_found], [max_recipients] for [`Too_many_recipients] and
      [invalid_recipients] for [`Invalid_recipients]. *)

  val unknown_member : t -> string -> Jsont.json option
  (** [unknown_member e name] is the value of the extension member [name] of
      [e], or [None] if [e] has no such member. *)

  val jsont : t Jsont.t
  (** [jsont] is the codec for a SetError. Decoding accepts an explicit [null]
      for [description]. It does not apply {!validate}. *)

  val pp : Format.formatter -> t -> unit
  (** [pp ppf e] prints the type name of [e] on [ppf], followed by the
      [description] of [e] in parentheses when it has one. Control bytes in
      server-supplied strings are escaped. *)

  val to_string : t -> string
  (** [to_string e] is {!pp} as a string. *)
end
