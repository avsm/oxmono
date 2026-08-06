(** Byte-class scans over a parse buffer.

    Each function scans [buf] from [pos] (inclusive) towards [limit]
    (exclusive) and returns the index of the first matching byte, or [limit] if
    there is none. Callers must ensure [0 <= pos <= limit <= Bytes.length buf];
    no bounds checking is performed.

    On amd64 with [ocaml_simd] installed these examine sixteen bytes per step
    using SSE2, falling back to eight-byte SWAR and then a byte loop for the
    tail. Otherwise {!Scan_portable} is used throughout. Both implementations
    are exactly equivalent to the corresponding byte loop. *)

val find_cr : local_ bytes -> pos:int -> limit:int -> int @@ portable
(** [find_cr buf ~pos ~limit] is the index of the first CR in
    [\[pos, limit)], or [limit] if there is none. *)

val find_sp_or_cr : local_ bytes -> pos:int -> limit:int -> int @@ portable
(** [find_sp_or_cr buf ~pos ~limit] is the index of the first SP or CR in
    [\[pos, limit)], or [limit] if there is none.

    This is the end of an HTTP request target: SP separates it from the
    version, and CR ends a request line with no version. *)

val skip_token : local_ bytes -> pos:int -> limit:int -> int @@ portable
(** [skip_token buf ~pos ~limit] is the index of the first byte in
    [\[pos, limit)] that is not an RFC 7230 [tchar], or [limit] if there is
    none.

    This is the end of a method or header-field name. *)
