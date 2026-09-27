(** Validated IMAP INTERNALDATE / APPEND date-time (RFC 9051).

    The value retains its explicit numeric zone, including the distinction
    between [+0000] and [-0000]. It contains no host-local time conversion. *)

type t

val of_string : string -> (t, string) result
(** [of_string s] parses the unquoted 26-byte date-time value [s]. It rejects
    impossible calendar dates, invalid clock fields, malformed zones and
    non-ASCII bytes. Second 60 is accepted only when it falls at 23:59:60
    UTC after applying the zone. *)

val to_string : t -> string
(** Return the canonical unquoted IMAP value. *)

val to_wire : t -> string
(** Return the quoted date-time argument for APPEND. *)

val equal_instant : t -> t -> bool
(** Compare the represented instant across numeric timezone offsets. A leap
    second compares only with another explicit leap second. *)

val of_unix_seconds : int64 -> (t, string) result
(** Convert a whole-second POSIX timestamp into a UTC IMAP date-time without
    relying on the process timezone. Reject years outside 1..9999. *)

val to_unix_seconds : t -> (int64, string) result
(** [to_unix_seconds t] is the whole-second POSIX timestamp of [t]. It is
    [Error] for an explicit leap second, and for an instant outside the range
    {!of_unix_seconds} accepts, which a year-1 or year-9999 local time can
    reach through its zone. *)
