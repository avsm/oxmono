open! Core
open! Async

(** Implementation of [csv destructure ...], which converts ‘nice’ representations of
    OCaml types to more simple numbers that can be used by other programs, e.g. changing
    [3m2s] to [182]. *)

val command : unit -> Command.t
