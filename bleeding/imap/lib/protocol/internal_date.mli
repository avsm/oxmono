(** IMAP INTERNALDATE and APPEND date-times.

    A value is an RFC 9051 [date-time] checked for calendar validity. It
    keeps its explicit numeric zone, including the distinction between
    [+0000] and [-0000], and involves no host-local time conversion. *)

type t
(** The type for validated date-times. *)

val of_string : string -> (t, string) result
(** [of_string s] is the unquoted 26-byte date-time [s], such as
    [" 7-Feb-1994 21:52:25 -0800"]. The day may be space-padded or
    zero-padded, and the month name is matched case-insensitively. The
    error covers bad syntax, a year of 0, an impossible calendar date, an
    hour, minute or zone field out of range, and a second of 60 that does
    not fall at 23:59:60 UTC once the zone is applied. *)

val to_string : t -> string
(** [to_string t] is the canonical unquoted form of [t], with a
    space-padded day and a capitalised month name. *)

val to_wire : t -> string
(** [to_wire t] is [to_string t] in double quotes, the APPEND argument
    form. *)

val equal_instant : t -> t -> bool
(** [equal_instant a b] is [true] if [a] and [b] denote the same instant,
    whatever their zones. A leap second equals only another leap second. *)

val of_unix_seconds : int64 -> (t, string) result
(** [of_unix_seconds s] is the POSIX timestamp [s] as a date-time in zone
    [+0000]. The error covers an [s] whose UTC year is outside 1 to
    9999. *)

val to_unix_seconds : t -> (int64, string) result
(** [to_unix_seconds t] is the whole-second POSIX timestamp of [t]. The
    error covers a leap second, and an instant outside the range
    {!of_unix_seconds} accepts, which a local time in year 1 or year 9999
    can reach through its zone. *)
