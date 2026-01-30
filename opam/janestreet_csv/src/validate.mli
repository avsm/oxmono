open! Core
open Async

val run : ?sep:char -> Reader.t -> (unit, string) result Deferred.t
