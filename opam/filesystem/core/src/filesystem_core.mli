open! Core

(** @inline *)
include Filesystem.S with module IO := Monad.Ident and type Fd.t = Core_unix.File_descr.t
