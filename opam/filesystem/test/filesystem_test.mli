open! Core
open! Async

(** We test [Filesystem.S] implementations via functor. Because of restrictions on expect
    tests in functors, we instantiate the tests here rather than in client libraries. *)

module Test_filesystem_async : Filesystem.S with module IO := Deferred
module Test_filesystem_core : Filesystem.S with module IO := Monad.Ident
