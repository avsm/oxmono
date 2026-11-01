(* Copyright (c) 2026 Anil Madhavapeddy. SPDX-License-Identifier: ISC *)
val list : string -> Termanil_model.draft list * string list
(** [list root] reads agent-editable Markdown replies. Changed files lose their
    queued status. Invalid files are reported and retained. *)

val save :
  string ->
  source:Termanil_model.email_ref ->
  thread_id:string ->
  subject:string ->
  recipients:string list ->
  body:string ->
  expected:string option ->
  Termanil_model.draft
(** [save root ...] atomically saves a reply if its on-disk revision still
    matches [expected]. Unknown frontmatter and comments survive body edits. *)

val queue : string -> Termanil_model.draft -> bool -> Termanil_model.draft
(** [queue root draft ready] binds the queue decision to the exact file
    revision. *)

val send :
  string ->
  Termanil_model.draft list ->
  prepare:(Termanil_model.draft -> before_submit:(string -> unit) -> string) ->
  Termanil_model.draft list * string list
(** [send root drafts ~prepare] checks every reviewed revision before sending.
    [prepare] performs read-only validation and returns the submission
    operation. An uncertain submission is recorded durably and is never retried
    implicitly. [before_submit] records the remote Email id before sending it.
*)

val verify :
  string ->
  lookup:(Termanil_model.email_ref -> string option) ->
  Termanil_model.draft list * string list
(** [verify root ~lookup] resolves uncertain receipts only when the server
    positively identifies an accepted submission. It never retries a send. *)
