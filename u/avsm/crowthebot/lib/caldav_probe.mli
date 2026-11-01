(** Operator diagnostics through the read-only calendar capability. *)

val run :
  emit:(string -> unit) -> (string * (unit -> Caldav_source.t)) list -> bool
(** [run ~emit sources] checks every named connection, continuing after errors.
    It authenticates, discovers calendars, reads one sync report or inventory
    per calendar and reads at most one sample object per calendar. Output has
    counts and safe error categories, never calendar contents or credentials. No
    mirror, cursor or cron job is stored. [true] means all checks passed, or no
    CalDAV sources were configured. *)
