(** Conversions between Maildir modification times and IMAP INTERNALDATE. *)

val of_mtime : float -> (Imap.Internal_date.t, string) result
(** [of_mtime mtime] is the UTC INTERNALDATE of the modification time
    [mtime] in POSIX seconds, rounded down to a whole second. A time that is
    not finite or lies outside 10{^12} seconds of the epoch is an error. *)

val of_occurrence : Maildir.occurrence -> (Imap.Internal_date.t, string) result
(** [of_occurrence o] is [of_mtime o.mtime]. *)

val to_mtime : Imap.Internal_date.t -> (float, string) result
(** [to_mtime date] is the modification time in POSIX seconds that
    represents [date]. A leap second and an instant outside the range
    {!Imap.Internal_date.of_unix_seconds} accepts are errors. *)
