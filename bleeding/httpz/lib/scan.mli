(** This module provides unchecked byte-class scans over parse buffers.

    Each function examines bytes from [pos] up to but not including [limit] and returns a
    matching offset or [limit]. The caller must ensure
    [0 <= pos <= limit <= Bytes.length buf].

    On amd64 with [ocaml_simd] installed these examine sixteen bytes per step using SSE2,
    falling back to eight-byte SWAR and then a byte loop for the tail. Otherwise
    {!Scan_portable} is used throughout. Both implementations are exactly equivalent to
    the corresponding byte loop. *)

(** [find_cr buf ~pos ~limit] is the first CR offset from [pos] up to but not including
    [limit], or [limit]. *)
val find_cr : local_ bytes -> pos:int -> limit:int -> int @@ portable

(** [find_sp_or_cr buf ~pos ~limit] is the first SP or CR offset from [pos] up to but not
    including [limit], or [limit]. *)
val find_sp_or_cr : local_ bytes -> pos:int -> limit:int -> int @@ portable

(** [skip_token buf ~pos ~limit] is the first offset that is not an HTTP token character,
    or [limit]. HTTP tokens are defined by
    {{:https://www.rfc-editor.org/rfc/rfc9110.html#section-5.6.2} RFC 9110, Section 5.6.2}. *)
val skip_token : local_ bytes -> pos:int -> limit:int -> int @@ portable
