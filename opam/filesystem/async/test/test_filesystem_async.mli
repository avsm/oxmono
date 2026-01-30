open! Core
open! Async

(*_ Make sure everything gets tested. *)
include Filesystem.S with module IO := Deferred
