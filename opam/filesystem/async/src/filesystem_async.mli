open! Core
open! Async_kernel
open! Async_unix

(** @inline *)
include Filesystem.S with module IO := Deferred and type Fd.t = Unix.Fd.t
