(** Safe rendering of untrusted diagnostics. *)

val sanitize : string -> string @@ portable
(** [sanitize s] renders C0, DEL, C1, Unicode bidi controls and line separators
    as visible ASCII escapes, preserving other well-formed UTF-8. *)
