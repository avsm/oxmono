@@ portable

(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Requests whose method calls refer to each other's results.

    A chain is a computation that adds method calls to a request and hands back
    a handle for each of them. A handle names the call it stands for, so a later
    call can take its ids from an earlier result with a result reference, and a
    response can be decoded with the codec of the method that was asked for.
    Method call ids are assigned by the chain and never chosen by the caller.

    A chain ends in the handles whose responses are wanted, gathered in a
    {!Handles.t} written [Handles.[ ... ]]. {!parse_all} reads a response into a
    {!Results.t} of the same length, whose entries have the types the handles
    decode to, and which is taken apart by a [Results.[ ... ]] pattern.

    The example of
    {{:https://datatracker.ietf.org/doc/html/rfc8620#section-3.7} RFC 8620
     Section 3.7} fetches the sender, date and subject of every Email in the ten
    most recent Threads of a Mailbox, in one request.
    {[
    open Jmap.Proto
    module Chain = Jmap.Chain
    module Results = Jmap.Chain.Results

    let recent ~account_id ~inbox =
      Chain.build
        ~capabilities:[ Capability.core; Capability.mail ]
        Chain.(
          let* threads_of =
            email_query ~account_id
              ~filter:(Email.filter ~in_mailbox:inbox ())
              ~sort:[ Email.sort ~ascending:false `Received_at ]
              ~collapse_threads:true ~limit:10L ()
          in
          let* heads =
            email_get ~account_id ~ids:(from_query threads_of)
              ~properties:[ `Thread_id ] ()
          in
          let* threads =
            thread_get ~account_id ~ids:(from_get_field heads Thread_id) ()
          in
          let+ messages =
            email_get ~account_id
              ~ids:(from_get_field threads Email_ids)
              ~properties:[ `From; `Received_at; `Subject ]
              ()
          in
          Handles.[ threads_of; threads; messages ])

    let report response handles =
      match Chain.parse_all handles response with
      | Error e -> Format.printf "%a@." Chain.pp_parse_error e
      | Ok Results.[ threads_of; threads; messages ] ->
          Format.printf "%d of %d Threads, %d Emails@."
            (List.length threads.list)
            (List.length threads_of.ids)
            (List.length messages.list)
    ]}

    @canonical Jmap.Chain *)

(** {1 Handles} *)

type query
(** The type for the kind of a [/query] handle. *)

type get
(** The type for the kind of a [/get] handle. *)

type snippet_get
(** The type for the kind of a [SearchSnippet/get] handle. RFC 8621 Section 5.1
    gives a SearchSnippet no [id], so its response admits none of the references
    a [/get] response does. *)

type changes
(** The type for the kind of a [/changes] handle. *)

type set
(** The type for the kind of a [/set] handle. *)

type query_changes
(** The type for the kind of a [/queryChanges] handle. *)

type copy
(** The type for the kind of a [/copy] handle. *)

type import
(** The type for the kind of an [Email/import] handle. *)

type parse
(** The type for the kind of an [Email/parse] handle. *)

type mailbox_changes_response = Mail_mailbox.changes_response = {
  changes : Proto_method.changes_response;
      (** The standard [/changes] response arguments. *)
  updated_properties : string list option;
      (** The properties that may have changed, or [None] if the server cannot
          tell that only counts have changed. *)
}
(** The type for the response of a [Mailbox/changes], which RFC 8621 Section 2.2
    gives one argument more than a standard [/changes] response. It is
    {!Jmap.Proto.Mailbox.changes_response}, repeated here so that the field
    names are in scope beside {!mailbox_changes}. *)

type (_, _) handle
(** The type for handles on a method call. The first parameter is the kind of
    the method, which decides the result references that may be built from it.
    The second is the type its response decodes to. *)

val call_id : (_, _) handle -> string
(** [call_id h] is the method call id the chain gave the call [h] stands for. *)

val method_name : (_, _) handle -> string
(** [method_name h] is the name of the method [h] stands for, such as
    ["Email/query"]. *)

val attempt :
  ('k, 'r) handle -> ('k, ('r, Proto_error.Method_error.t) result) handle
(** [attempt h] is [h] whose response is [Error e] when the server answered the
    call with the method error [e], and [Ok v] when it answered with the
    response [v]. A response that does not decode, and a response that is
    missing altogether, remain a {!Json_error} of {!val-parse}.

    It is for a call that may fail without failing the read of the rest of the
    request, such as a [/changes] that can answer [cannotCalculateChanges]. It
    takes the handle a builder produced, so a chain binds the call first and
    wraps the handle where it is read, as in
    [let* c = email_changes ... in Handles.[ attempt c ]]. *)

(** {1 Sources of ids} *)

(** The type for the ids an argument takes. A {!Ref} is sent under the argument
    name prefixed with ["#"], which RFC 8620 Section 3.7 has the server resolve
    before it runs the method. *)
type id_source =
  | Ids of Proto_id.t list  (** Ids the client already holds. *)
  | Ref of Proto_invocation.result_reference
      (** A reference to a value in the result of an earlier call. *)

val ids : Proto_id.t list -> id_source
(** [ids l] is the ids [l]. *)

val id : Proto_id.t -> id_source
(** [id i] is the single id [i]. *)

val from_query : (query, _) handle -> id_source
(** [from_query h] is the [ids] of the response to [h]. *)

val from_get_ids : (get, _) handle -> id_source
(** [from_get_ids h] is the id of every record in the [list] of the response to
    [h]. *)

(** The type for the properties of a record whose value is an Id or an array of
    Ids. RFC 8620 Section 3.7 lets the [*] token of a result reference map
    through an array of records, so these are the only properties a later call
    can take its ids from. Each constructor is indexed by the record that
    carries the property. *)
type _ id_property =
  | Thread_id : Mail_email.t id_property
      (** The [threadId] of an Email, RFC 8621 Section 4.1.1. *)
  | Email_ids : Mail_thread.t id_property
      (** The [emailIds] of a Thread, RFC 8621 Section 3. *)
  | Submission_email_id : Mail_submission.t id_property
      (** The [emailId] of an EmailSubmission, RFC 8621 Section 7. *)
  | Submission_identity_id : Mail_submission.t id_property
      (** The [identityId] of an EmailSubmission, RFC 8621 Section 7. *)
  | Submission_thread_id : Mail_submission.t id_property
      (** The [threadId] of an EmailSubmission, RFC 8621 Section 7. *)

val from_get_field :
  (get, 'r Proto_method.get_response) handle -> 'r id_property -> id_source
(** [from_get_field h p] is the property [p] of every record in the [list] of
    the response to [h], such as {!Thread_id} of an [Email/get] or {!Email_ids}
    of a [Thread/get].

    @raise Invalid_argument
      if [h] was given a list of properties that does not name [p]. RFC 8620
      Section 5.1 has a [/get] return only the properties it was asked for, so
      the server would answer the reference with an [invalidResultReference]
      error rather than with the ids. *)

val from_get_field_raw : (get, _) handle -> string -> id_source
(** [from_get_field_raw h field] is [field] of every record in the [list] of the
    response to [h], for a property this module has no {!id_property} for, such
    as one an extension capability defines. ["id"] is accepted whatever [h] was
    asked for, RFC 8620 Section 5.1 having a [/get] always return it.

    @raise Invalid_argument
      if [h] was given a list of properties that does not name [field], for the
      reason {!from_get_field} gives, or if [field] is not valid UTF-8. *)

val from_changes_created : (changes, _) handle -> id_source
(** [from_changes_created h] is the [created] ids of the response to [h]. *)

val from_changes_updated : (changes, _) handle -> id_source
(** [from_changes_updated h] is the [updated] ids of the response to [h]. *)

val from_changes_destroyed : (changes, _) handle -> id_source
(** [from_changes_destroyed h] is the [destroyed] ids of the response to [h].

    There is no reference to the records a [/set], [/copy] or [/import] call
    creates.
    {{:https://datatracker.ietf.org/doc/html/rfc8620#section-3.7} RFC 8620
     Section 3.7} lets the [*] token map through an array only, and the
    [created] member of those responses is an object keyed by creation id. A
    later call of the same request names such a record by its creation
    reference, {!Jmap.Proto.Id.creation_ref}, as RFC 8620 Section 5.3 intends.
*)

val from_changes_updated_properties :
  (changes, mailbox_changes_response) handle ->
  Proto_invocation.result_reference
(** [from_changes_updated_properties h] is the [updatedProperties] of the
    response to the [Mailbox/changes] [h].

    {{:https://datatracker.ietf.org/doc/html/rfc8621#section-2.2} RFC 8621
     Section 2.2}: "The [updatedProperties] array may be used directly via a
    back-reference in a subsequent [Mailbox/get] call in the same request, so
    only these properties are returned if nothing else has changed." It is a
    list of property names rather than of ids, so it is passed to {!mailbox_get}
    as [?properties_ref]. *)

val from_query_changes_removed : (query_changes, _) handle -> id_source
(** [from_query_changes_removed h] is the [removed] ids of the response to [h].
*)

val from_query_changes_added : (query_changes, _) handle -> id_source
(** [from_query_changes_added h] is the id of every item [h] adds to the query
    results. *)

(** {1 The chain}

    A chain is written inside [Chain.( ... )] or under [let open Chain in],
    which brings the builder names of this module into scope for the duration. A
    binding of the program's own under one of those names is shadowed there, so
    a state string is bound as [since_email] rather than as [email_state]. *)

type 'a t
(** The type for chains producing a value of type ['a], which is usually a
    handle or a {!Handles.t} of them. *)

val return : 'a -> 'a t
(** [return x] is the chain producing [x] and adding no method call. *)

val bind : 'a t -> ('a -> 'b t) -> 'b t
(** [bind m f] is the chain running [m] and then [f] on its value. The calls of
    [m] come first in the request. *)

val map : ('a -> 'b) -> 'a t -> 'b t
(** [map f m] is the chain producing [f] applied to the value of [m]. *)

val both : 'a t -> 'b t -> ('a * 'b) t
(** [both a b] is the chain producing the values of [a] and [b], with the calls
    of [a] first in the request. *)

val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
(** [( let* )] is {!bind}. *)

val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t
(** [( let+ )] is {!map} with its arguments exchanged. *)

val ( and* ) : 'a t -> 'b t -> ('a * 'b) t
(** [( and* )] is {!both}. *)

val ( and+ ) : 'a t -> 'b t -> ('a * 'b) t
(** [( and+ )] is {!both}. *)

val build : capabilities:string list -> 'a t -> Proto_request.t * 'a
(** [build ~capabilities m] is the request of the calls [m] adds, using
    [capabilities], together with the value of [m]. Every builder raises while
    [m] runs, so an [Invalid_argument] a builder documents is raised by this
    function rather than by the builder. *)

val build_request : capabilities:string list -> 'a t -> Proto_request.t
(** [build_request ~capabilities m] is {!build} with the value of [m] dropped.
*)

val build_handles : capabilities:string list -> 'a t -> 'a
(** [build_handles ~capabilities m] is {!build} with the request dropped, which
    is the handles [m] ends in. *)

val attempt_call :
  ('k, 'r) handle t -> ('k, ('r, Proto_error.Method_error.t) result) handle t
(** [attempt_call c] is the one call chain [c] with its handle passed through
    {!attempt}, for a call sent on its own whose error the caller acts on. *)

(** {1 Core methods} *)

val blob_copy :
  from_account_id:Proto_id.t ->
  account_id:Proto_id.t ->
  blob_ids:id_source ->
  unit ->
  (copy, Proto_blob.copy_response) handle t
(** [blob_copy ~from_account_id ~account_id ~blob_ids ()] adds a [Blob/copy]
    call copying [blob_ids] between accounts.

    @raise Stdlib.exception-Invalid_argument if the accounts are equal. *)

val push_subscription_get :
  ?ids:id_source ->
  ?properties:string list ->
  unit ->
  (get, Proto_push.get_response) handle t
(** [push_subscription_get ()] adds a [PushSubscription/get] call. [ids] and
    [properties] are omitted unless given. *)

val push_subscription_set :
  ?create:
    (Proto_push.create_args Proto_id.creation * Proto_push.create_args) list ->
  ?update:(Proto_id.t * Proto_patch.t) list ->
  ?destroy:id_source ->
  unit ->
  (set, Proto_push.set_response) handle t
(** [push_subscription_set ()] adds a [PushSubscription/set] call. Every
    argument is omitted unless given. *)

(** {1 Email methods} *)

val email_query :
  account_id:Proto_id.t ->
  ?filter:Mail_email.filter ->
  ?sort:Proto_filter.comparator list ->
  ?position:int64 ->
  ?anchor:Proto_id.t ->
  ?anchor_offset:int64 ->
  ?limit:int64 ->
  ?calculate_total:bool ->
  ?collapse_threads:bool ->
  unit ->
  (query, Proto_method.query_response) handle t
(** [email_query ~account_id ()] adds an [Email/query] call on [account_id].
    Every optional argument is omitted unless given, which leaves the server its
    default. [collapse_threads] is the extra argument of RFC 8621 Section 4.4,
    which returns one Email per Thread.

    @raise Invalid_argument
      if [position], [anchor_offset], [limit] or a filter value is outside the
      range RFC 8620 Section 1.3 allows. *)

val email_get :
  account_id:Proto_id.t ->
  ?ids:id_source ->
  ?properties:Mail_email.property list ->
  ?properties_raw:string list ->
  ?body_properties:Mail_email.body_part_property list ->
  ?body_properties_raw:string list ->
  ?fetch_text_body_values:bool ->
  ?fetch_html_body_values:bool ->
  ?fetch_all_body_values:bool ->
  ?max_body_value_bytes:int64 ->
  unit ->
  (get, Mail_email.t Proto_method.get_response) handle t
(** [email_get ~account_id ()] adds an [Email/get] call on [account_id]. An
    omitted [ids] asks for every Email, which RFC 8620 Section 5.1 lets a server
    refuse.

    [properties] names the properties of
    {{:https://datatracker.ietf.org/doc/html/rfc8621#section-4.1} RFC 8621
     Section 4.1} as typed variants, so that ["recievedAt"] cannot be typed by
    mistake and a header field is spelled
    [`Header (Jmap.Proto.Email_header.addresses `Sender)] rather than
    ["header:Sender:asAddresses"].
    {[
    email_get ~account_id ~ids
      ~properties:[ `Id; `Subject; `From; `Received_at ]
      ()
    ]}
    [body_properties] does the same for the EmailBodyPart properties of Section
    4.1.4. [properties_raw] and [body_properties_raw] name a property this
    library has no variant for, such as one an extension capability adds.
    Entries of both are sent in the one argument, the typed ones first, and
    omitting both asks for every property.

    The properties sent are remembered by the handle, so {!from_get_field}
    refuses a reference to a property the call did not ask for.

    @raise Invalid_argument
      if [max_body_value_bytes] is negative or above 2{^ 53}-1. *)

val email_state : account_id:Proto_id.t -> (unit, string) handle t
(** [email_state ~account_id] adds an [Email/get] of no ids whose response is
    read as its [state] string. RFC 8620 Section 5.1 has a [/get] return the
    state of the data type in the account whatever the ids asked for, so this is
    the cheapest way to learn the state to hand to {!email_changes} later.

    A local open of this module shadows a binding of the caller's called
    [email_state], so a program holds the string it reads here under a name of
    its own such as [since_email]. {!mailbox_state} and {!thread_state} are the
    same. *)

val email_changes :
  account_id:Proto_id.t ->
  since_state:string ->
  ?max_changes:int64 ->
  unit ->
  (changes, Proto_method.changes_response) handle t
(** [email_changes ~account_id ~since_state ()] adds an [Email/changes] call
    reporting what changed since [since_state]. [max_changes] is unset unless
    given, which leaves the server to choose how much to return.

    @raise Stdlib.exception-Invalid_argument
      if [max_changes] is not positive or is above 2{^ 53}-1. *)

val email_query_changes :
  account_id:Proto_id.t ->
  since_query_state:string ->
  ?filter:Mail_email.filter ->
  ?sort:Proto_filter.comparator list ->
  ?max_changes:int64 ->
  ?up_to_id:Proto_id.t ->
  ?calculate_total:bool ->
  ?collapse_threads:bool ->
  unit ->
  (query_changes, Proto_method.query_changes_response) handle t
(** [email_query_changes ~account_id ~since_query_state ()] adds an
    [Email/queryChanges] call reporting how the results of a query changed since
    [since_query_state]. [filter], [sort] and [collapse_threads] must repeat the
    values the {!email_query} being tracked was given, per
    {{:https://datatracker.ietf.org/doc/html/rfc8621#section-4.5} RFC 8621
     Section 4.5}.

    @raise Invalid_argument if [max_changes] is negative or above 2{^ 53}-1. *)

val email_set :
  account_id:Proto_id.t ->
  ?if_in_state:string ->
  ?create:(Mail_email.t Proto_id.creation * Mail_email.t) list ->
  ?update:(Proto_id.t * Proto_patch.t) list ->
  ?destroy:id_source ->
  unit ->
  (set, Mail_email.t Proto_method.set_response) handle t
(** [email_set ~account_id ()] adds an [Email/set] call on [account_id]. An
    omitted argument is not sent.

    [create] maps a creation id to the record to create, which
    {!Jmap.Proto.Email.create} builds. A key is a {!Jmap.Proto.Id.type-creation}
    token, which RFC 8620 Section 5.3 writes as the map key without a ["#"]. The
    record is encoded with {!Jmap.Proto.Email.jsont}, so a property the library
    has no field for is given in its [unknown] field. Name the record the call
    creates in a later call of the same request with
    {!Jmap.Proto.Id.creation_ref} of the same token, and read the result with
    {!Jmap.Proto.Method.val-created}.

    [update] maps the id of an Email to the PatchObject of RFC 8620 Section 5.3
    to apply to it. {!Jmap.Proto.Patch.of_json} turns a patch already built as
    JSON into one, including the whole record form.
    {[
    email_set ~account_id
      ~update:
        [
          ( email_id,
            Patch.v
              [
                Email.Patch.set_keyword `Seen;
                Email.Patch.add_to_mailbox archive;
              ] );
        ]
      ()
    ]}
    [if_in_state] is the state the account must be in for the call to go ahead.

    @raise Invalid_argument if a record of [create] cannot be encoded. *)

val email_import :
  account_id:Proto_id.t ->
  ?if_in_state:string ->
  emails:(Mail_email.t Proto_id.creation * Mail_email.Import.email) list ->
  unit ->
  (import, Mail_email.Import.response) handle t
(** [email_import ~account_id ~emails ()] adds an [Email/import] call creating
    one Email per entry of [emails], keyed by creation id.

    {{:https://datatracker.ietf.org/doc/html/rfc8621#section-4.8} RFC 8621
     Section 4.8}: the messages "must first be uploaded as blobs using the
    standard upload mechanism", so an {!Jmap.Proto.Email.Import.type-email}
    names the uploaded blob together with the Mailboxes and keywords to give the
    new Email. Read the results with {!Jmap.Proto.Email.Import.val-created} and
    {!Jmap.Proto.Email.Import.val-not_created}, or name an imported Email in a
    later call of the same request by {!Jmap.Proto.Id.creation_ref} of its
    token. [if_in_state] is the state the account must be in for the call to go
    ahead. *)

val email_parse :
  account_id:Proto_id.t ->
  blob_ids:id_source ->
  ?properties:Mail_email.property list ->
  ?properties_raw:string list ->
  ?body_properties:Mail_email.body_part_property list ->
  ?body_properties_raw:string list ->
  ?fetch_text_body_values:bool ->
  ?fetch_html_body_values:bool ->
  ?fetch_all_body_values:bool ->
  ?max_body_value_bytes:int64 ->
  unit ->
  (parse, Mail_email.Parse.response) handle t
(** [email_parse ~account_id ~blob_ids ()] adds an [Email/parse] call reading
    each blob of [blob_ids] as an RFC 5322 message.

    {{:https://datatracker.ietf.org/doc/html/rfc8621#section-4.9} RFC 8621
     Section 4.9}: this "can be used to parse and display attached messages
    without having to import them as top-level Email objects in the mail store
    in their own right". [blob_ids] may be a reference, for instance to the
    [blobId] of the [message/rfc822] parts an [Email/get] in the same request
    returns. Read a result with {!Jmap.Proto.Email.Parse.val-parsed}.
    [properties] and [body_properties] are those of {!email_get}.

    @raise Invalid_argument
      if [max_body_value_bytes] is negative or above 2{^ 53}-1. *)

val email_copy :
  from_account_id:Proto_id.t ->
  account_id:Proto_id.t ->
  ?if_from_in_state:string ->
  ?if_in_state:string ->
  create:
    (Mail_email.t Proto_id.creation * Proto_id.t * (string * Jsont.Json.t) list)
    list ->
  ?on_success_destroy_original:bool ->
  ?destroy_from_if_in_state:string ->
  unit ->
  (copy, Mail_email.t Proto_method.copy_response) handle t
(** [email_copy ~from_account_id ~account_id ~create ()] adds an [Email/copy]
    call copying Emails of [from_account_id] into [account_id].

    An entry [(creation, source_id, overrides)] of [create] copies the Email
    [source_id] under the creation id [creation], with each property of
    [overrides] replacing the one the source Email has. Only [mailboxIds],
    [keywords] and [receivedAt] may be named in [overrides].
    {{:https://datatracker.ietf.org/doc/html/rfc8620#section-5.4} RFC 8620
     Section 5.4} keys the map by creation id and has the object carry the id of
    the record to copy, which this builder writes.

    [on_success_destroy_original] asks for the source Emails to be destroyed
    once the copy succeeds, and [destroy_from_if_in_state] is the state
    [from_account_id] must be in for that destruction to go ahead.

    @raise Stdlib.exception-Invalid_argument
      if the accounts are equal, [create] is empty, an override is not
      permitted, or a map contains a duplicate property. *)

(** {1 Thread methods} *)

val thread_get :
  account_id:Proto_id.t ->
  ?ids:id_source ->
  ?properties:Mail_thread.property list ->
  ?properties_raw:string list ->
  unit ->
  (get, Mail_thread.t Proto_method.get_response) handle t
(** [thread_get ~account_id ()] adds a [Thread/get] call on [account_id].
    {{:https://datatracker.ietf.org/doc/html/rfc8621#section-3} RFC 8621 Section
     3} gives a Thread two properties, [`Id] and [`Email_ids], so [properties]
    is rarely worth giving and omitting it asks for both. See {!email_get} for
    [properties_raw]. *)

val thread_state : account_id:Proto_id.t -> (unit, string) handle t
(** [thread_state ~account_id] is {!email_state} for the Thread data type,
    adding a [Thread/get] of no ids and reading its [state] string. *)

val thread_changes :
  account_id:Proto_id.t ->
  since_state:string ->
  ?max_changes:int64 ->
  unit ->
  (changes, Proto_method.changes_response) handle t
(** [thread_changes ~account_id ~since_state ()] adds a [Thread/changes] call
    reporting what changed since [since_state]. See {!email_changes} for
    [max_changes].

    @raise Stdlib.exception-Invalid_argument
      if [max_changes] is not positive or is above 2{^ 53}-1. *)

(** {1 Mailbox methods} *)

val mailbox_query :
  account_id:Proto_id.t ->
  ?filter:Mail_mailbox.filter ->
  ?sort:Proto_filter.comparator list ->
  ?position:int64 ->
  ?anchor:Proto_id.t ->
  ?anchor_offset:int64 ->
  ?limit:int64 ->
  ?calculate_total:bool ->
  ?sort_as_tree:bool ->
  ?filter_as_tree:bool ->
  unit ->
  (query, Proto_method.query_response) handle t
(** [mailbox_query ~account_id ()] adds a [Mailbox/query] call on [account_id].

    [sort_as_tree] and [filter_as_tree] are the extra arguments of
    {{:https://datatracker.ietf.org/doc/html/rfc8621#section-2.3} RFC 8621
     Section 2.3}, both [false] at the server when omitted. With
    [~sort_as_tree:true] an ancestor always sorts before its descendants, and
    with [~filter_as_tree:true] "a Mailbox is only included in the query if all
    its ancestors are also included in the query according to the filter".

    @raise Invalid_argument
      if [position], [anchor_offset] or [limit] is outside the range RFC 8620
      Section 1.3 allows. *)

val mailbox_get :
  account_id:Proto_id.t ->
  ?ids:id_source ->
  ?properties:Mail_mailbox.property list ->
  ?properties_raw:string list ->
  ?properties_ref:Proto_invocation.result_reference ->
  unit ->
  (get, Mail_mailbox.t Proto_method.get_response) handle t
(** [mailbox_get ~account_id ()] adds a [Mailbox/get] call on [account_id].
    [properties] names the RFC 8621 Section 2 Mailbox properties as typed
    variants, as in [~properties:[ `Id; `Name; `Role ]]. See {!email_get} for
    [properties_raw].

    [properties_ref] sends [#properties] in place of [properties], for the
    Section 2.2 pattern of feeding the [updatedProperties] of a
    [Mailbox/changes] straight into the [Mailbox/get] that follows it. See
    {!from_changes_updated_properties}. The properties are then chosen by the
    server, so {!from_get_field} checks nothing against them.

    @raise Invalid_argument
      if [properties_ref] is given together with [properties] or
      [properties_raw], which RFC 8620 Section 3.7 requires a server to reject
      with an [invalidArguments] error. The chain runs when the request is
      built, so {!build} raises rather than this function. *)

val mailbox_state : account_id:Proto_id.t -> (unit, string) handle t
(** [mailbox_state ~account_id] is {!email_state} for the Mailbox data type,
    adding a [Mailbox/get] of no ids and reading its [state] string. *)

val mailbox_by_role :
  account_id:Proto_id.t ->
  ?properties:Mail_mailbox.property list ->
  Mail_mailbox.role ->
  (get, Mail_mailbox.t Proto_method.get_response) handle t
(** [mailbox_by_role ~account_id role] adds a [Mailbox/query] filtering
    [account_id] on [role] followed by a [Mailbox/get] of the ids it returns,
    and is the handle of the [Mailbox/get]. [properties] is the property list of
    the [Mailbox/get], and omitting it asks for every property, as
    {!mailbox_get} does.

    RFC 8621 Section 2 gives an account at most one Mailbox per role, so the
    [list] of the response holds one Mailbox, or none when the account has no
    Mailbox with [role]. The two calls go in one request, the [Mailbox/get]
    taking its ids by result reference. *)

val mailbox_changes :
  account_id:Proto_id.t ->
  since_state:string ->
  ?max_changes:int64 ->
  unit ->
  (changes, mailbox_changes_response) handle t
(** [mailbox_changes ~account_id ~since_state ()] adds a [Mailbox/changes] call
    reporting what changed since [since_state]. Its response carries the RFC
    8621 Section 2.2 [updatedProperties] argument as well as the standard
    [/changes] fields. See {!mailbox_changes_response}.

    @raise Stdlib.exception-Invalid_argument
      if [max_changes] is not positive or is above 2{^ 53}-1. *)

val mailbox_query_changes :
  account_id:Proto_id.t ->
  since_query_state:string ->
  ?filter:Mail_mailbox.filter ->
  ?sort:Proto_filter.comparator list ->
  ?max_changes:int64 ->
  ?up_to_id:Proto_id.t ->
  ?calculate_total:bool ->
  unit ->
  (query_changes, Proto_method.query_changes_response) handle t
(** [mailbox_query_changes ~account_id ~since_query_state ()] adds a
    [Mailbox/queryChanges] call. [filter] and [sort] must repeat the values the
    {!mailbox_query} being tracked was given.

    @raise Invalid_argument if [max_changes] is negative or above 2{^ 53}-1. *)

val mailbox_set :
  account_id:Proto_id.t ->
  ?if_in_state:string ->
  ?create:(Mail_mailbox.t Proto_id.creation * Mail_mailbox.t) list ->
  ?update:(Proto_id.t * Proto_patch.t) list ->
  ?destroy:id_source ->
  ?on_destroy_remove_emails:bool ->
  unit ->
  (set, Mail_mailbox.t Proto_method.set_response) handle t
(** [mailbox_set ~account_id ()] adds a [Mailbox/set] call on [account_id]. See
    {!email_set} for [create], [update] and [if_in_state].
    [on_destroy_remove_emails] is the extra argument of RFC 8621 Section 2.5,
    which destroys the Emails a destroyed Mailbox held rather than failing. A
    record of [create] is built with {!Jmap.Proto.Mailbox.create}.

    @raise Invalid_argument if a record of [create] cannot be encoded. *)

(** {1 Identity methods} *)

val identity_get :
  account_id:Proto_id.t ->
  ?ids:id_source ->
  ?properties_raw:string list ->
  unit ->
  (get, Mail_identity.t Proto_method.get_response) handle t
(** [identity_get ~account_id ()] adds an [Identity/get] call on [account_id].
    The
    {{:https://datatracker.ietf.org/doc/html/rfc8621#section-6} RFC 8621 Section
     6} Identity properties have no typed list, so only their wire names are
    taken and omitting them asks for every property. *)

val identity_changes :
  account_id:Proto_id.t ->
  since_state:string ->
  ?max_changes:int64 ->
  unit ->
  (changes, Proto_method.changes_response) handle t
(** [identity_changes ~account_id ~since_state ()] adds an [Identity/changes]
    call reporting what changed since [since_state].

    @raise Stdlib.exception-Invalid_argument
      if [max_changes] is not positive or is above 2{^ 53}-1. *)

val identity_set :
  account_id:Proto_id.t ->
  ?if_in_state:string ->
  ?create:(Mail_identity.t Proto_id.creation * Mail_identity.t) list ->
  ?update:(Proto_id.t * Proto_patch.t) list ->
  ?destroy:id_source ->
  unit ->
  (set, Mail_identity.t Proto_method.set_response) handle t
(** [identity_set ~account_id ()] adds an [Identity/set] call on [account_id].
    See {!email_set} for [create], [update] and [if_in_state]. A record of
    [create] is built with {!Jmap.Proto.Identity.v}.

    @raise Invalid_argument if a record of [create] cannot be encoded. *)

(** {1 EmailSubmission methods} *)

val email_submission_query :
  account_id:Proto_id.t ->
  ?filter:Mail_submission.filter ->
  ?sort:Proto_filter.comparator list ->
  ?position:int64 ->
  ?anchor:Proto_id.t ->
  ?anchor_offset:int64 ->
  ?limit:int64 ->
  ?calculate_total:bool ->
  unit ->
  (query, Proto_method.query_response) handle t
(** [email_submission_query ~account_id ()] adds an [EmailSubmission/query] call
    on [account_id].

    @raise Invalid_argument
      if [position], [anchor_offset] or [limit] is outside the range RFC 8620
      Section 1.3 allows. *)

val email_submission_get :
  account_id:Proto_id.t ->
  ?ids:id_source ->
  ?properties:Mail_submission.property list ->
  ?properties_raw:string list ->
  unit ->
  (get, Mail_submission.t Proto_method.get_response) handle t
(** [email_submission_get ~account_id ()] adds an [EmailSubmission/get] call on
    [account_id]. [properties] names the
    {{:https://datatracker.ietf.org/doc/html/rfc8621#section-7} RFC 8621 Section
     7} properties as typed variants, as in
    [~properties:[ `Email_id; `Undo_status ]]. See {!email_get} for
    [properties_raw]. *)

val email_submission_changes :
  account_id:Proto_id.t ->
  since_state:string ->
  ?max_changes:int64 ->
  unit ->
  (changes, Proto_method.changes_response) handle t
(** [email_submission_changes ~account_id ~since_state ()] adds an
    [EmailSubmission/changes] call reporting what changed since [since_state].

    @raise Stdlib.exception-Invalid_argument
      if [max_changes] is not positive or is above 2{^ 53}-1. *)

val email_submission_query_changes :
  account_id:Proto_id.t ->
  since_query_state:string ->
  ?filter:Mail_submission.filter ->
  ?sort:Proto_filter.comparator list ->
  ?max_changes:int64 ->
  ?up_to_id:Proto_id.t ->
  ?calculate_total:bool ->
  unit ->
  (query_changes, Proto_method.query_changes_response) handle t
(** [email_submission_query_changes ~account_id ~since_query_state ()] adds an
    [EmailSubmission/queryChanges] call. [filter] and [sort] must repeat the
    values the {!email_submission_query} being tracked was given.

    @raise Invalid_argument if [max_changes] is negative or above 2{^ 53}-1. *)

val email_submission_set :
  account_id:Proto_id.t ->
  ?if_in_state:string ->
  ?create:(Mail_submission.t Proto_id.creation * Mail_submission.t) list ->
  ?update:(Proto_id.t * Proto_patch.t) list ->
  ?destroy:id_source ->
  ?on_success_update_email:(Proto_id.t * Proto_patch.t) list ->
  ?on_success_destroy_email:Proto_id.t list ->
  unit ->
  (set, Mail_submission.t Proto_method.set_response) handle t
(** [email_submission_set ~account_id ()] adds an [EmailSubmission/set] call on
    [account_id]. See {!email_set} for [create], [update] and [if_in_state].

    [on_success_update_email] is the argument of
    {{:https://datatracker.ietf.org/doc/html/rfc8621#section-7.5} RFC 8621
     Section 7.5}, "a map of EmailSubmission id to an object containing
    properties to update on the Email object referenced by the EmailSubmission
    if the create/update/destroy succeeds". Its keys, like those of
    [on_success_destroy_email], are EmailSubmission ids or the creation
    references {!Jmap.Proto.Id.creation_ref} builds. The usual patch takes
    [$draft] off the message and moves it from Drafts to Sent. A record of
    [create] is built with {!Jmap.Proto.Submission.create}, whose [email_id] may
    be the creation reference of an Email created earlier in the request.
    {[
    let draft = Id.creation "draft" in
    email_submission_set ~account_id
      ~create:
        [
          ( Id.creation "s1",
            Submission.create ~identity_id ~email_id:(Id.creation_ref draft) ()
          );
        ]
      ()
    ]}

    @raise Invalid_argument if a record of [create] cannot be encoded. *)

(** {1 SearchSnippet methods} *)

val search_snippet_get :
  account_id:Proto_id.t ->
  filter:Mail_email.filter ->
  email_ids:id_source ->
  unit ->
  (snippet_get, Mail_snippet.get_response) handle t
(** [search_snippet_get ~account_id ~filter ~email_ids ()] adds a
    [SearchSnippet/get] call returning a snippet of each Email of [email_ids]
    for [filter]. [filter] must repeat the one the {!email_query} that produced
    [email_ids] was given.

    The response is a {!Jmap.Proto.Search_snippet.get_response} rather than a
    standard [/get] response, since
    {{:https://datatracker.ietf.org/doc/html/rfc8621#section-5.1} RFC 8621
     Section 5.1} gives a SearchSnippet no [id], sends no [state] string and
    types [notFound] as [Id[]|null].

    @raise Invalid_argument if [filter] cannot be encoded. *)

(** {1 VacationResponse methods} *)

val vacation_response_get :
  account_id:Proto_id.t ->
  ?properties_raw:string list ->
  unit ->
  (get, Mail_vacation.t Proto_method.get_response) handle t
(** [vacation_response_get ~account_id ()] adds a [VacationResponse/get] call on
    [account_id]. Like {!identity_get} it takes only wire property names, the
    RFC 8621 Section 8 VacationResponse having no typed list. The account has
    one VacationResponse, whose id is ["singleton"]. *)

val vacation_response_set :
  account_id:Proto_id.t ->
  ?if_in_state:string ->
  update:Proto_patch.t ->
  unit ->
  (set, Mail_vacation.t Proto_method.set_response) handle t
(** [vacation_response_set ~account_id ~update ()] adds a [VacationResponse/set]
    call applying [update] to the one VacationResponse of [account_id]. RFC 8621
    Section 8.2 makes it a singleton, so the record cannot be created or
    destroyed and the id is always ["singleton"]. [if_in_state] is the state the
    account must be in for the call to go ahead. *)

(** {1 AddressBook methods} *)

val address_book_get :
  account_id:Proto_id.t ->
  ?ids:id_source ->
  ?properties:Contacts_addressbook.property list ->
  ?properties_raw:string list ->
  unit ->
  (get, Contacts_addressbook.t Proto_method.get_response) handle t
(** [address_book_get ~account_id ()] adds an [AddressBook/get] call on
    [account_id]. [properties] names the
    {{:https://www.rfc-editor.org/rfc/rfc9610#section-2} RFC 9610 Section 2}
    AddressBook properties as typed variants, as in
    [~properties:[ `Id; `Name; `Is_default ]]. See {!email_get} for
    [properties_raw]. Section 2.1 lets [ids] be null to fetch every AddressBook
    of the account at once, which is what omitting it does. *)

val address_book_state : account_id:Proto_id.t -> (unit, string) handle t
(** [address_book_state ~account_id] is {!email_state} for the AddressBook data
    type, adding an [AddressBook/get] of no ids and reading its [state] string.
*)

val address_book_changes :
  account_id:Proto_id.t ->
  since_state:string ->
  ?max_changes:int64 ->
  unit ->
  (changes, Proto_method.changes_response) handle t
(** [address_book_changes ~account_id ~since_state ()] adds an
    [AddressBook/changes] call reporting what changed since [since_state].

    @raise Invalid_argument
      if [max_changes] is not positive or is above 2{^ 53}-1. *)

val address_book_set :
  account_id:Proto_id.t ->
  ?if_in_state:string ->
  ?create:
    (Contacts_addressbook.t Proto_id.creation * Contacts_addressbook.t) list ->
  ?update:(Proto_id.t * Proto_patch.t) list ->
  ?destroy:id_source ->
  ?on_destroy_remove_contents:bool ->
  ?on_success_set_is_default:Proto_id.t ->
  unit ->
  (set, Contacts_addressbook.t Proto_method.set_response) handle t
(** [address_book_set ~account_id ()] adds an [AddressBook/set] call on
    [account_id]. See {!email_set} for [create], [update] and [if_in_state]. A
    record of [create] is built with {!Jmap.Proto.Address_book.create}.

    [on_destroy_remove_contents] is the extra argument of RFC 9610 Section 2.3.
    Left unset or [false], destroying an AddressBook that still holds a
    ContactCard fails with an [addressBookHasContents] SetError. Set [true], the
    cards are taken out of it instead, and a card that belongs to no other
    AddressBook is destroyed.

    [on_success_set_is_default] names the AddressBook to make the default once
    every create, update and destroy of the call has succeeded. It may be a
    creation reference naming an AddressBook created in the same call. Section
    2.3 makes the server ignore it silently if the id is not found or the change
    is not permitted, so a caller that must know reads [isDefault] back rather
    than looking for an error.

    @raise Invalid_argument if a record of [create] cannot be encoded. *)

(** {1 ContactCard methods} *)

val contact_card_get :
  account_id:Proto_id.t ->
  ?ids:id_source ->
  ?properties:string list ->
  unit ->
  (get, Contacts_card.t Proto_method.get_response) handle t
(** [contact_card_get ~account_id ()] adds a [ContactCard/get] call on
    [account_id].

    [properties] takes wire names rather than typed variants, unlike
    {!address_book_get}. The properties of a ContactCard are those of a
    JSContact Card, which RFC 9553 Section 3.5 registers at IANA, which RFC 9555
    Section 5.3 extends and which a vendor may extend further, so a closed
    variant would be wrong. ["id"] and ["addressBookIds"] are the two RFC 9610
    Section 3 adds.

    A server that is given [properties] returns those alone, so the [@type],
    [version] and [uid] that RFC 9553 Section 2.1 makes mandatory may all be
    absent from the response. {!Jmap.Proto.Contact_card.jsont} reads such a card
    through [Jscontact.Card.partial_jsont] and the card it yields is not a whole
    Card; see {!Jmap.Proto.Contact_card.t}. *)

val contact_card_state : account_id:Proto_id.t -> (unit, string) handle t
(** [contact_card_state ~account_id] is {!email_state} for the ContactCard data
    type. *)

val contact_card_changes :
  account_id:Proto_id.t ->
  since_state:string ->
  ?max_changes:int64 ->
  unit ->
  (changes, Proto_method.changes_response) handle t
(** [contact_card_changes ~account_id ~since_state ()] adds a
    [ContactCard/changes] call reporting what changed since [since_state].

    @raise Invalid_argument
      if [max_changes] is not positive or is above 2{^ 53}-1. *)

val contact_card_query :
  account_id:Proto_id.t ->
  ?filter:Contacts_card.filter ->
  ?sort:Proto_filter.comparator list ->
  ?position:int64 ->
  ?anchor:Proto_id.t ->
  ?anchor_offset:int64 ->
  ?limit:int64 ->
  ?calculate_total:bool ->
  unit ->
  (query, Proto_method.query_response) handle t
(** [contact_card_query ~account_id ()] adds a [ContactCard/query] call.
    [filter] is built with {!Jmap.Proto.Contact_card.filter} and [sort] with
    {!Jmap.Proto.Contact_card.Sort}, whose [to_string] gives the [property] of a
    comparator. RFC 9610 Section 3.3.2 requires a server to sort on [created]
    and [updated] and asks it to sort on the three name components. *)

val contact_card_query_changes :
  account_id:Proto_id.t ->
  since_query_state:string ->
  ?filter:Contacts_card.filter ->
  ?sort:Proto_filter.comparator list ->
  ?max_changes:int64 ->
  ?up_to_id:Proto_id.t ->
  ?calculate_total:bool ->
  unit ->
  (query_changes, Proto_method.query_changes_response) handle t
(** [contact_card_query_changes ~account_id ~since_query_state ()] adds a
    [ContactCard/queryChanges] call. [filter] and [sort] must repeat the values
    the {!contact_card_query} being tracked was given.

    @raise Invalid_argument if [max_changes] is negative or above 2{^ 53}-1. *)

val contact_card_set :
  account_id:Proto_id.t ->
  ?if_in_state:string ->
  ?create:(Contacts_card.t Proto_id.creation * Contacts_card.t) list ->
  ?update:(Proto_id.t * Proto_patch.t) list ->
  ?destroy:id_source ->
  unit ->
  (set, Contacts_card.t Proto_method.set_response) handle t
(** [contact_card_set ~account_id ()] adds a [ContactCard/set] call on
    [account_id]. See {!email_set} for [create], [update] and [if_in_state].

    RFC 9610 Section 3 requires a card to belong to at least one AddressBook at
    all times, so a created record must set [addressBookIds], and RFC 9553
    Section 2.1 requires it to carry [@type], [version] and [uid].

    @raise Invalid_argument if a record of [create] cannot be encoded. *)

val contact_card_copy :
  from_account_id:Proto_id.t ->
  account_id:Proto_id.t ->
  ?if_from_in_state:string ->
  ?if_in_state:string ->
  create:
    (Contacts_card.t Proto_id.creation
    * Proto_id.t
    * (string * Jsont.Json.t) list)
    list ->
  ?on_success_destroy_original:bool ->
  ?destroy_from_if_in_state:string ->
  unit ->
  (copy, Contacts_card.t Proto_method.copy_response) handle t
(** [contact_card_copy ~from_account_id ~account_id ~create ()] adds a
    [ContactCard/copy] call, moving cards from [from_account_id] into
    [account_id]. See {!email_copy} for the shape of [create]: each entry is a
    creation id, the id of the card in the source account, and the properties to
    override on the copy.

    RFC 9610 Section 3 requires a card to belong to at least one AddressBook, so
    an entry overrides [addressBookIds] with a book of the destination account;
    the source account's book ids mean nothing there. That is the only property
    a copied ContactCard may override.

    Both accounts must be visible to the one session, which on a server that
    implements JMAP Sharing means the owner of the source has shared an
    AddressBook with the user through its [shareWith] property.

    @raise Invalid_argument
      if the two accounts are the same or if [create] is empty, both of which
      RFC 8620 Section 5.4 rejects. *)

(** {1 Other methods} *)

val echo : Jsont.Json.t -> (unit, Jsont.Json.t) handle t
(** [echo args] adds a [Core/echo] call.
    {{:https://datatracker.ietf.org/doc/html/rfc8620#section-4} RFC 8620 Section
     4} has the server return [args] unchanged, which tests that a request
    reaches it and comes back. *)

val invocation :
  name:string -> arguments:Jsont.Json.t -> 'r Jsont.t -> (unit, 'r) handle t
(** [invocation ~name ~arguments codec] adds a call of [name] with [arguments]
    whose response decodes with [codec], for a method this module has no builder
    for. The kind of the handle is [unit], so no result reference is built from
    it.

    @raise Invalid_argument
      if [arguments] carries the same argument in both normal and referenced
      form. See {!check_arguments}. *)

val raw_invocation :
  name:string -> arguments:Jsont.Json.t -> (unit, Jsont.Json.t) handle t
(** [raw_invocation ~name ~arguments] is {!invocation} of [name] and [arguments]
    with the codec [Jsont.json], so its response is handed back as JSON.

    @raise Invalid_argument
      if [arguments] carries the same argument in both normal and referenced
      form. *)

val check_arguments : Jsont.Json.t -> (unit, string) result
(** [check_arguments args] is [Ok ()] if [args] is an object with unique members
    and no normal and referenced form of the same argument. The error is a human
    readable message.

    {{:https://datatracker.ietf.org/doc/html/rfc8620#section-3.7} RFC 8620
     Section 3.7}: "If an arguments object contains the same argument name in
    normal and referenced form (e.g., [foo] and [#foo]), the method MUST return
    an [invalidArguments] error." No builder of this module can produce such a
    pair, so the check is for an arguments object built by the caller for
    {!invocation}. *)

(** {1 Reading responses} *)

(** The type for the reasons {!val-parse} produced no response.

    {{:https://datatracker.ietf.org/doc/html/rfc8620#section-3.6.2} RFC 8620
     Section 3.6.2}: when a method call fails "the response name is set to
    'error', and the arguments are the error object". That is a well formed
    answer rather than malformed JSON, and worth telling apart. A client retries
    or reports a {!Method_error}, whereas a {!Json_error} means the server and
    this library disagree about the wire format. *)
type parse_error =
  | Method_error of Proto_error.Method_error.t
      (** The server answered the call with an [error] response in place of the
          response the method would have returned. *)
  | Json_error of Jsont.Error.t
      (** No response carried the method call id and name of the handle, or the
          arguments of the one that did do not decode as the response of the
          method. *)

val pp_parse_error : Format.formatter -> parse_error -> unit
(** [pp_parse_error ppf e] prints a one line description of [e] on [ppf]. *)

val parse_error_to_string : parse_error -> string
(** [parse_error_to_string e] is {!pp_parse_error} of [e] as a string. *)

exception Parse_error of parse_error
(** The exception {!parse_exn} raises. It carries the reason no response was
    decoded, so a program that reports the failure and stops needs no handler of
    its own beyond printing it with {!pp_parse_error}. *)

val method_error :
  (_, _) handle -> Proto_response.t -> Proto_error.Method_error.t option
(** [method_error h r] is the error the server returned for the call [h] stands
    for, and [None] if the call succeeded, if [r] carries no response for it, or
    if the error object does not decode.

    RFC 8620 Section 3.6.2 has an error response share the method call id of the
    call that failed, so this asks whether a call succeeded without committing
    to decoding its response. Use it for the errors a client acts on rather than
    reports, such as [cannotCalculateChanges] from a [/changes] or
    [unsupportedSort] from a [/query], and leave the rest to {!val-parse}. *)

val parse : (_, 'a) handle -> Proto_response.t -> ('a, parse_error) result
(** [parse h r] is the response of [r] to the call [h] stands for, decoded with
    the codec of the method [h] names.

    RFC 8620 Section 3.2 lets one call produce several responses, all carrying
    its method call id, so the response taken is the one whose name is the
    method name of [h] or ["error"]. An error for the call is reported as a
    {!Method_error} rather than decoded as a response, so a caller that does
    nothing but report failures needs no {!method_error} of its own. *)

val parse_exn : (_, 'a) handle -> Proto_response.t -> 'a
(** [parse_exn h r] is {!val-parse} of [h] and [r].

    @raise Parse_error if there is no response to decode. *)

(** Lists of handles.

    The constructors are in a module of their own so that a list literal written
    inside an [open Jmap.Chain] still means an ordinary list. A chain ends in
    [Handles.[ h1; h2 ]]. *)
module Handles : sig
  (** The type for lists of handles, indexed by the responses they decode to. *)
  type _ t = [] : unit t | ( :: ) : (_, 'r) handle * 'rs t -> ('r * 'rs) t
end

(** The responses of a {!Handles.t}.

    A read is [let Results.[ v1; v2 ] = ...], with the constructors in a module
    of their own for the reason {!Handles} gives. The pattern is read in the
    order of the handle list rather than by name, so naming its variables after
    the handles, in that order, is what keeps the response of one call from
    being read as the response of another of the same type. *)
module Results : sig
  (** The type for the decoded responses of a {!Handles.t}, in the same order.
  *)
  type _ t = [] : unit t | ( :: ) : 'r * 'rs t -> ('r * 'rs) t
end

val parse_all :
  'rs Handles.t -> Proto_response.t -> ('rs Results.t, parse_error) result
(** [parse_all hs r] is {!val-parse} of [r] for each handle of [hs], in the same
    order. It is the first error of the list if any handle fails to read. *)

val parse_all_exn : 'rs Handles.t -> Proto_response.t -> 'rs Results.t
(** [parse_all_exn hs r] is {!parse_all} of [hs] and [r].

    @raise Parse_error if a handle of [hs] has no response to decode. *)
