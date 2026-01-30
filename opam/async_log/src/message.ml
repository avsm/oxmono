module Stable = struct
  module V2 = struct
    type t = Time_float_unix.t Async_log_kernel.Message.Stable.T1.V2.t
    [@@deriving bin_io, sexp]
  end
end

open! Core
open! Import

include (
  Async_log_kernel.Message :
    module type of Async_log_kernel.Message
    with module Stable := Async_log_kernel.Message.Stable)

type t = Time_float_unix.t Async_log_kernel.Message.T1.t [@@deriving sexp_of]

let to_write_only_text ?(zone = force Time_float_unix.Zone.local) t =
  to_write_only_text t zone
;;
