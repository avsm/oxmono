open! Core

(** General utf8-friendly xpath parser. *)
val parse_utf8_exn : string -> Types.Expression.t

(** Ascii-only xpath parser, faster than [parse_utf8], but less general. Only call if you
    know your data is ascii only. *)
val parse_ascii_exn : string -> Types.Expression.t
