(** Scoped provisional files for IMAP body transfers. *)

val with_spool :
  _ Eio.Path.t -> (Eio.File.rw_ty Eio.Resource.t -> 'a) -> 'a
(** [with_spool path f] is [f file] for a mode-0600 file exclusively created
    at [path]. [f] may reopen [path] to read the written bytes. The file is
    closed and removed when [f] returns or raises, including on cancellation.
    When [f] raises, a failure to remove the file is ignored and [f]'s
    exception propagates. An existing file at [path] is never removed. [f]
    must not rename or replace the file, or let [file] escape. *)

val hash_file : _ Eio.Path.t -> int64 * string
(** [hash_file path] is the byte count and lowercase SHA-256 digest of the
    file at [path], read to end of file with a fixed-size buffer. The caller
    must ensure that the file is not modified concurrently. *)
