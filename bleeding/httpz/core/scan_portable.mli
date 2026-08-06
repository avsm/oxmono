(** Byte-class scans without SIMD, eight bytes at a time.

    Semantics are identical to {!Scan}; see that module for the contract. This
    module is the fallback used when [ocaml_simd] is unavailable, and is also
    used by the SSE2 implementation for the final sub-16-byte tail. *)

val find_cr : local_ bytes -> pos:int -> limit:int -> int @@ portable
(** [find_cr buf ~pos ~limit] is the index of the first CR in
    [\[pos, limit)], or [limit] if there is none. *)

val find_sp_or_cr : local_ bytes -> pos:int -> limit:int -> int @@ portable
(** [find_sp_or_cr buf ~pos ~limit] is the index of the first SP or CR in
    [\[pos, limit)], or [limit] if there is none. *)

val is_token_char : char# -> bool @@ portable
(** [is_token_char c] is [true] if [c] is an RFC 7230 [tchar]. *)

val tchar_table : string @@ portable
(** One byte per code point, non-zero exactly for the characters accepted by
    {!is_token_char}. *)

val skip_token : local_ bytes -> pos:int -> limit:int -> int @@ portable
(** [skip_token buf ~pos ~limit] is the index of the first byte in
    [\[pos, limit)] not accepted by {!is_token_char}, or [limit] if there is
    none. *)
