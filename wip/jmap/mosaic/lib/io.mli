(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** The JMAP side of the client.

    A value of type {!t} is one run of the client. It holds the connection the
    login screen asked for and performs one {!Model.type-action} against it,
    reporting the outcome as {!Model.type-msg} values. It is where every method
    call of the client lives.

    Listing a mailbox or smart search is an [Email/query] and the [Email/get] of
    its ids in one request, and two requests on a server whose
    [maxCallsInRequest] is one. Sending a reply uses one chained [Email/set] and
    [EmailSubmission/set] under the same condition, and otherwise sends them
    sequentially; a final [Email/set] marks the message being answered.

    Every string a response contributes to the model is escaped as
    {!Jmap_eio.Cli.terminal_text} escapes it, since the view draws the model
    unaltered. A body keeps its line endings and each of its lines is escaped on
    its own. *)

val capabilities : string list
(** [capabilities] is the [using] array for Mosaic's combined send operation:
    the core, mail and submission capabilities of RFC 8620 and RFC 8621.
    Mail-only reads and changes advertise only core and mail internally, so an
    account without submission support can still be browsed. *)

type t
(** The type for the connection of one run of the client. *)

val default_timeout : float
(** [default_timeout] is [60.], the deadline for each JMAP exchange and the
    idle-read timeout for a blob body. *)

val create :
  sw:Eio.Switch.t ->
  ?account:string ->
  ?timeout:float ->
  ?transport:Jmap_eio.Transport.t ->
  ?allow_insecure:bool ->
  Eio_unix.Stdenv.base ->
  t
(** [create ~sw ~account ~timeout ~transport ~allow_insecure env] is a value
    with no connection yet, which {!Model.Connect} makes. [sw] is the switch the
    client belongs to and it must outlive the client. [env] is the environment
    the default transport and UI clock are reached through. [account] is the id
    of the account to act on and defaults to absent, in which case the primary
    mail account of the session is used. Connecting rejects an id absent from
    the Session or an account without the mail capability.

    [timeout] is applied by {!Jmap_eio.Client} to the session fetch and every
    later exchange, and as an idle-read timeout for a blob body. It defaults to
    {!default_timeout}. [transport] defaults to a standard transport using
    Mosaic's short retry backoff. A supplied transport must carry a clock.
    [allow_insecure] permits credentials over cleartext HTTP and defaults to
    [false].

    @raise Invalid_argument
      if [timeout] is not finite and non-negative or [transport] carries no
      clock. *)

val connection : t -> (Jmap_eio.Client.t * Jmap.Proto.Id.t) option
(** [connection t] is the client of [t] and the account it resolved, or [None]
    until {!Model.Connect} has succeeded. *)

val perform : t -> Model.action -> (Model.msg -> unit) -> unit
(** [perform t a dispatch] makes the request [a] asks for and calls [dispatch]
    with each message it produces. It blocks until the exchange finishes, so it
    is called from a fiber of its own.

    {!Model.Connect} fetches the session, resolves the account and keeps both in
    [t], and is answered with {!Model.Connected} or {!Model.Login_failed}. Every
    other action needs a connection and is answered with {!Model.Failed} without
    a request when [t] has none, which the model never asks for.

    A failed exchange, a response that does not decode, an incomplete [/set]
    outcome, an unsupported submission operation, a change the server refused
    and an argument no server would accept each become one {!Model.Failed} (or
    {!Model.Send_failed} for a send), so the update loop never has to handle
    those exceptions. Cancellation and an exception raised by [dispatch]
    propagate unchanged.

    {!Model.Send} creates the draft before the server judges the submission, so
    it answers by what the submission outcome proves. A refusal proves nothing
    was submitted, and is {!Model.Send_failed} after the draft has been
    destroyed. An outcome the server did not report, and a submission response
    that does not decode, prove nothing either way, and are
    {!Model.Sent_with_warning}, which the model does not offer a retry from,
    since retrying could deliver the reply twice. *)
