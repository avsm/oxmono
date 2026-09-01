(** This module provides portable implementations of {!Scan} and token classification.

    Scans are unchecked. The caller must ensure [0 <= pos <= limit <= Bytes.length buf]. *)

(** [find_cr buf ~pos ~limit] is the first CR offset from [pos] up to but not including
    [limit], or [limit]. *)
val find_cr : local_ bytes -> pos:int -> limit:int -> int @@ portable

(** [find_sp_or_cr buf ~pos ~limit] is the first SP or CR offset from [pos] up to but not
    including [limit], or [limit]. *)
val find_sp_or_cr : local_ bytes -> pos:int -> limit:int -> int @@ portable

(** [is_token_char byte] is [true] when [byte] is an HTTP token character. *)
val is_token_char : char# -> bool @@ portable

(** [tchar_table] is a map from each byte value to nonzero exactly when {!is_token_char}
    accepts it. *)
val tchar_table : string @@ portable

(** [skip_token buf ~pos ~limit] is the first offset not accepted by {!is_token_char}, or
    [limit]. *)
val skip_token : local_ bytes -> pos:int -> limit:int -> int @@ portable
