(** The live activity page and its JSON feed. *)
val page : jobs:bool -> string
(** [page ~jobs] renders the sections the client script fills from the
    feed, noting whether the server can read the queue. *)
val feed : web:Activity.entry list -> web_cursor:int ->
  jobs:(Duraml.event * Taposaur_jobs.Inspect.job option) list ->
  running:Taposaur_jobs.Inspect.job list -> jobs_cursor:int64 ->
  older:int64 option -> counts:int * int -> string
(** [feed ...] encodes new website requests as lines and queue events as
    one summary per job: its current state, lineage and the attempts the
    events describe, with claimed and settled transitions of one attempt
    collapsed into a single span. [running] adds jobs still running whose
    events fall outside the window. [older] is the cursor for the next
    older page and [counts] the queued and running totals. *)
