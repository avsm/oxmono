(** This module provides portable byte-class scans and token classification.

    {!Scan} is a narrowed alias of this module, selected when no faster
    architecture-specific implementation is configured.

    Scans are unchecked. The caller must ensure [0 <= pos <= limit <= Bytes.length buf]. *)

(** [find_cr buf ~pos ~limit] is the first CR offset from [pos] up to but not including
    [limit], or [limit]. *)
val find_cr : local_ bytes -> pos:int -> limit:int -> int @@ portable

(** [find_sp_or_cr buf ~pos ~limit] is the first SP or CR offset from [pos] up to but not
    including [limit], or [limit]. *)
val find_sp_or_cr : local_ bytes -> pos:int -> limit:int -> int @@ portable

(** [is_token_char byte] is [true] when [byte] is an HTTP token character. *)
val is_token_char : char# -> bool @@ portable

(** [skip_token buf ~pos ~limit] is the first offset not accepted by {!is_token_char}, or
    [limit]. *)
val skip_token : local_ bytes -> pos:int -> limit:int -> int @@ portable
