open! Core

(*_ Make sure everything gets tested. *)
include Filesystem.S with module IO := Monad.Ident
