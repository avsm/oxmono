open! Core

module type S = sig
  type 'return t =
    | Call of (exn -> 'return)
    | Ignore
    | Log
    | Raise
  [@@deriving sexp_of, typed_variants]

  val on_cleanup_error
    :  'return t
    -> backtrace:Backtrace.t option
    -> exn:Exn.t
    -> log_s:(Sexp.t -> 'return)
    -> path:File_path.Absolute.t
    -> return:(unit -> 'return)
    -> 'return
end

module type On_cleanup_error = sig
  module type S = S

  include S
end
