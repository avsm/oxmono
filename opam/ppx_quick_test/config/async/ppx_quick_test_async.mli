module Ppx_quick_test_config :
  Ppx_quick_test_runtime_lib.S with type 'a IO.t = 'a Async_kernel.Deferred.t
(** @inline *)
