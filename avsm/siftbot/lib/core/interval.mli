type t

val parse : string -> t
(** [parse text] accepts a positive number followed by [d], [w], [m] or [y], or
    an RFC3339 timestamp. Invalid input raises [Invalid_argument]. *)

val since : now:Ptime.t -> t -> Ptime.t
(** [since ~now interval] resolves a start before [now]. Months and years are
    calendar intervals in UTC, clamped to the last valid day of the target
    month. Days and weeks are multiples of 24 hours. Out-of-range or future
    starts raise [Invalid_argument]. *)
