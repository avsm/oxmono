(** Read-only job queue presentation. No mutation controls are rendered. *)
module J = Taposaur_jobs.Inspect
val index : (string * string) list -> J.counts list -> J.job list -> string
(** [index params counts jobs] renders queue totals and the selected page of
    jobs, with links to papers and to the next page. *)
val job : J.job -> J.job list -> Taposaur_audit.event list -> string
(** [job j children audit] renders one job with its timestamps, outcome,
    payload, parent link and the successors it staged, followed by its queue
    timeline and the tree of audit operations its handlers recorded. [audit]
    holds the paper's audit records, newest first; those stamped with this
    job's identifier, bridged queue events included, are selected, together
    with every operation nested beneath them. *)
