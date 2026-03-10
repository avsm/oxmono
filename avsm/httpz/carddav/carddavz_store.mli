(** File-backed address book store using Eio.

    Stores vCards as individual [.vcf] files in a directory hierarchy:
    {v /addressbooks/<name>/*.vcf v}

    Implements {!Webdavz.STORE} for use with the WebDAV handler. *)

type t
(** The file-backed store. *)

val create : Eio.Fs.dir_ty Eio.Path.t -> t
(** [create root] creates a store rooted at [root].
    The directory should already exist. *)

(** {1 STORE Implementation} *)

include Webdavz.STORE with type t := t
