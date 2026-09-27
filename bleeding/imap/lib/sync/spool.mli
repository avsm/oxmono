(** Scoped provisional files for IMAP body transfers. *)

val with_spool :
  _ Eio.Path.t -> (Eio.File.rw_ty Eio.Resource.t -> 'a) -> 'a
(** Exclusively create a mode-0600 spool and pass its open file to the
    callback. The callback may reopen the path to consume the written bytes.
    The file is closed and removed when the callback returns or raises,
    including cancellation. An existing path is never removed when creation
    fails. The callback must not rename or replace the spool, or let its file
    resource escape the callback lifetime. *)

val hash_file : _ Eio.Path.t -> int64 * string
(** Read the file to EOF with a fixed-size buffer, returning the actual byte
    count and lowercase SHA-256 digest. The file descriptor is scoped to this
    call. The caller must ensure that the file is not concurrently modified. *)
