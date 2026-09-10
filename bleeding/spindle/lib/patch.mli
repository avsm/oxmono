(** Bounded, content-addressed pull-request patch loading. *)

val revision : Eio_unix.Stdenv.base -> string -> string
(** [revision system compressed] validates gzip input and extracts the final
    format-patch commit. Malformed input raises [Json.Invalid]. *)

val pull : Network.t -> Eio_unix.Stdenv.base -> string -> Jsont.json -> string
(** [pull network system author record] fetches the last round's gzip blob,
    verifies its CID and returns its final format-patch commit SHA. Compressed
    input is limited to 16 MiB and decompressed input to 64 MiB. *)
