(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Test harness for running the JMAP client against a real server.

    The oracle is a Cyrus IMAP test server started by [scripts/oracle-up.sh]
    (see [test/oracle/README.md]). Every test is skipped unless
    [JMAP_ORACLE_URL] is set, so [dune runtest] stays hermetic.

    Environment variables:
    - [JMAP_ORACLE_URL] session or well-known URL, e.g.
      [http://localhost:18080/.well-known/jmap]
    - [JMAP_ORACLE_USER] login name (default [user1])
    - [JMAP_ORACLE_PASSWORD] password (default [x]; Cyrus accepts anything)
    - [JMAP_ORACLE_DOMAIN] mail domain of the users (default [example.com])
    - [JMAP_ORACLE_LMTP] [host:port] of the LMTP listener used to inject test
      mail (default [localhost:18024]) *)

type t = {
  env : Eio_unix.Stdenv.base;
  sw : Eio.Switch.t;
      (** The switch the client was connected under. A test that connects a
          second client of its own hands it to {!connect_with}. *)
  client : Jmap_eio.Client.t;
  account_id : Jmap.Proto.Id.t;
  user : string;  (** Login name. *)
  address : string;  (** Delivery address, [user@domain]. *)
}

val configured : unit -> bool
(** [configured ()] is [true] when [JMAP_ORACLE_URL] is set. *)

val url : unit -> string
(** [url ()] is [JMAP_ORACLE_URL], failing the test when it is unset. *)

val user : unit -> string
(** [user ()] is [JMAP_ORACLE_USER], default [user1]. *)

val password : unit -> string
(** [password ()] is [JMAP_ORACLE_PASSWORD], default [x]. *)

val lmtp : unit -> string * int
(** [lmtp ()] is the host and port from [JMAP_ORACLE_LMTP], default
    [localhost:18024]. A host without a port uses port [24]. An empty host or a
    port outside [1..65535] fails the test with a configuration diagnostic. *)

val other_user : unit -> string
(** [other_user ()] is the login of a second account on the oracle,
    [JMAP_ORACLE_USER2] or ["user2"]. A [/copy] test needs two accounts visible
    to one session. *)

val auth : unit -> Jmap_eio.Auth.t
(** [auth ()] is the credential the oracle expects, {!Jmap_eio.Auth.basic} over
    {!val-user} and {!password}. Cyrus answers [401] to a bearer token whatever
    it holds, so basic is the only scheme the oracle accepts. *)

val connect_with :
  sw:Eio.Switch.t ->
  ?auth:Jmap_eio.Auth.t ->
  ?timeout:float ->
  Eio_unix.Stdenv.base ->
  (Jmap_eio.Client.t, Jmap_eio.Client.error) result
(** [connect_with ~sw env] is {!Jmap_eio.Client.connect_env} against the oracle,
    with [~allow_insecure:true] because the test server is plain http on
    localhost. It is what {!test_case} uses, exposed so that a test can vary the
    credential or the timeout and inspect the failure rather than aborting on
    it. [auth] defaults to {!auth}. *)

val connect_other : sw:Eio.Switch.t -> t -> Jmap_eio.Client.t * Jmap.Proto.Id.t
(** [connect_other ~sw t] logs in as {!other_user} and is its client together
    with the id of its primary contacts account. It fails the test if the login
    or the session fails.

    RFC 8620 Section 5.4 requires both accounts of a [/copy] to be visible to
    one session, and a JMAP server shows a second account only once its owner
    has shared something; see {!Jmap.Proto.Address_book.t} and its [shareWith]
    property. *)

val test_case : string -> (t -> unit) -> unit Alcotest.test_case
(** [test_case name f] is a quick Alcotest case that connects to the oracle and
    runs [f]. It is skipped when the oracle is not configured. *)

val run : string -> (string * unit Alcotest.test_case list) list -> unit
(** [run name suites] is [Alcotest.run name suites]. *)

(** {1 Requests} *)

val capabilities : string list
(** [capabilities] is the list sent in [using] for every call this harness
    makes, core, mail and submission. *)

val contacts_capabilities : string list
(** [contacts_capabilities] is core and
    {{:https://www.rfc-editor.org/rfc/rfc9610#section-1.4.1} the RFC 9610
     contacts capability}. A server that does not implement RFC 9610 answers a
    request naming it with [unknownCapability], so a contacts test passes this
    to {!val-call} rather than widening {!capabilities} for every other test. *)

val request : t -> Jmap.Proto.Request.t -> Jmap.Proto.Response.t
(** [request t req] executes the request [req] and fails the test with the
    client's error on failure. It is for a test that reads the response itself;
    {!val-call} and {!run_all} are the chain forms. *)

val call :
  ?client:Jmap_eio.Client.t ->
  ?capabilities:string list ->
  t ->
  (_, 'r) Jmap.Chain.handle Jmap.Chain.t ->
  'r
(** [call t c] is {!Jmap_eio.Client.val-call} of the chain [c] with
    {!capabilities}, failing the test on a transport failure, a method error or
    a response that does not decode. [client] defaults to that of [t], and is
    given by a test that holds a second login. [capabilities] defaults to
    {!capabilities}; a contacts test passes {!contacts_capabilities}. *)

val run_all :
  ?client:Jmap_eio.Client.t ->
  ?capabilities:string list ->
  t ->
  'rs Jmap.Chain.Handles.t Jmap.Chain.t ->
  'rs Jmap.Chain.Results.t
(** [run_all t c] is {!val-call} for a chain ending in several handles. It is
    named apart from {!val-run}, which is the Alcotest entry point. *)

(** {1 Test mail} *)

val unique : string -> string
(** [unique prefix] is [prefix] followed by a token unique to this process and
    call, for subjects and mailbox names that must not collide. *)

val message :
  ?from:string ->
  ?to_:string ->
  ?subject:string ->
  ?body:string ->
  ?headers:string list ->
  unit ->
  string * string
(** [message ()] is [(subject, raw)], an RFC 5322 message with a unique subject
    and Message-ID, CRLF line endings, and any extra [headers], each a complete
    field. *)

val query_by_subject :
  ?client:Jmap_eio.Client.t ->
  ?account_id:Jmap.Proto.Id.t ->
  t ->
  string ->
  Jmap.Proto.Id.t list
(** [query_by_subject t subject] is the ids of at most ten Emails of the account
    whose subject is [subject], which is how a test finds the messages it
    delivered. [client] and [account_id] default to those of [t]. *)

val deliver : t -> ?from:string -> ?to_:string -> string -> unit
(** [deliver t raw] injects [raw] over LMTP to [to_] (default [t.address]). It
    fails the test if the server rejects it. *)

val wait_for_email :
  t -> ?timeout:float -> subject:string -> unit -> Jmap.Proto.Id.t
(** [wait_for_email t ~subject ()] polls [Email/query] with a [subject] filter
    until one message matches. [timeout] defaults to 45 seconds; LMTP
    acknowledgement on the Cyrus test server can take over 30 seconds under
    concurrent load. *)

val deliver_and_wait :
  t ->
  ?from:string ->
  ?subject:string ->
  ?body:string ->
  ?headers:string list ->
  unit ->
  Jmap.Proto.Id.t * string
(** [deliver_and_wait t ()] builds a {!message}, delivers it and waits for it.
    It is [(email_id, subject)]. *)

val email :
  t ->
  properties:Jmap.Proto.Email.property list ->
  Jmap.Proto.Id.t ->
  Jmap.Proto.Email.t
(** [email t ~properties id] is the Email [id] with [properties] fetched. It
    fails the test unless the [Email/get] returned exactly that one Email. *)

(** {1 Session helpers} *)

val mailbox_with_role : t -> Jmap.Proto.Mailbox.role -> Jmap.Proto.Mailbox.t
(** [mailbox_with_role t `Inbox] is the mailbox of [t]'s account whose role is
    that one. RFC 8621 Section 2 gives an account at most one Mailbox per role,
    and this fails the test if the account has none with it. *)
