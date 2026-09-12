(** Byte-preserving edits using native YAML parser spans. *)

val update : string -> Common.value -> string
(** Patch changed values, retaining untouched bytes and comments. Reparse and
    check the complete result before returning it. *)
