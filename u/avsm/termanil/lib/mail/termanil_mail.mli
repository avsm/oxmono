(* Copyright (c) 2026 Anil Madhavapeddy. SPDX-License-Identifier: ISC *)
module Html_text = Html_text
module Search = Search

type t

val create : client:Jmap_eio.Client.t -> service:string -> account:string -> t
(** [create ~client ~service ~account] binds every request and result to one
    service and account. The caller owns the client's Eio switch. *)

val mailboxes : t -> Termanil_model.mailbox list

val messages :
  t ->
  mailbox:string option ->
  query:string ->
  position:int ->
  Termanil_model.page
(** [messages t ~mailbox ~query ~position] reads a bounded page, preserving
    query order even when Email/get returns another order. *)

val read : t -> Termanil_model.email_ref -> Termanil_model.message * string

val set_keyword :
  t -> Termanil_model.email_ref -> [ `Seen | `Flagged ] -> bool -> unit
(** [set_keyword t source keyword enabled] patches only the named keyword,
    conditional on a freshly read Email state. It checks per-object errors.
    Transport failures are not retried here. *)

val message :
  service:string ->
  account:string ->
  Jmap.Proto.Email.t ->
  Termanil_model.message

val body : Jmap.Proto.Email.t -> string

val conversation :
  t -> Termanil_model.email_ref -> (Termanil_model.message * string) list
(** [conversation t source] reads up to fifty emails in received order,
    retaining [source]. The query listing collapses each thread to one matching
    result. *)

val reply_target :
  t -> Termanil_model.email_ref -> string * string * string list
(** [reply_target t source] returns the thread, reply subject and Reply-To
    addresses (falling back to From). *)

val prepare_reply :
  t ->
  identity:string option ->
  Termanil_model.draft ->
  before_submit:(string -> unit) ->
  string
(** [prepare_reply t ~identity draft] validates the sending identity and reply
    headers without writing. The returned operation creates and submits one
    Email, recording its id through [before_submit] before EmailSubmission/set.
*)

val submission_for_email : t -> Termanil_model.email_ref -> string option
(** [submission_for_email t source] finds a pending or final submission for the
    exact remote Email. Absence is not proof that the Email was never sent. *)

val signature : t -> identity:string option -> override:string option -> string
(** [signature t ~identity ~override] uses the override if present, otherwise
    the selected identity's text signature, with HTML converted to text as a
    fallback. An empty override disables signatures. *)

val identity_signature : Jmap.Proto.Identity.t -> string
(** [identity_signature identity] prefers nonempty text to HTML, converting HTML
    with the same renderer used for message bodies. *)

val archive : t -> Termanil_model.email_ref -> unit
(** [archive t source] removes Inbox membership and adds Archive in one
    conditional patch, preserving all other mailbox memberships and keywords. *)
