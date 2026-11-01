(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Reading back the times the journal writes.

    {!Journal.rfc3339} writes them and this reads them, so a [wake] record's due
    time can be given back to {!Schedule.poll} and a [--since] on the command
    line can be compared with a record. *)

val of_rfc3339 : string -> float option
(** [of_rfc3339 s] is the POSIX time [s] names, where [s] is
    [YYYY-MM-DDTHH:MM:SSZ] as the journal writes it, and [None] for anything
    else. *)

val of_since : string -> (float, string) result
(** [of_since s] is the POSIX time a [--since] means. It accepts a whole
    timestamp, a [YYYY-MM-DD] date, which is midnight UTC on that day, and a
    duration back from now such as [2h] or [7d]. The error says which forms
    those are. *)
