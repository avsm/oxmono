open! Core
open Expect_test_helpers_base

module type IO = sig
  type 'a t

  val return : 'a -> 'a t
  val bind : 'a t -> f:('a -> 'b t) -> 'b t
  val map : 'a t -> f:('a -> 'b) -> 'b t
end

module type S = sig
  module IO : IO

  val run_quick_test
    :  here_pos:Source_code_position.t
    -> config:Base_quickcheck.Test.Config.t option
         (** default is [Base_quickcheck.Test.default_config] *)
    -> cr:CR.t option (** default is [CR] *)
    -> hide_positions:bool option (** default is [false] when [cr=CR], [true] otherwise *)
    -> examples:'tup list
    -> trials:int option (** if provided, overrides the number of trials in [config] *)
    -> sexp_examples:Sexp_examples.t
    -> params:('tup, 'fn, unit IO.t) Params.t
    -> filename_rel_to_project_root:string
    -> 'fn
    -> unit IO.t
end

module type Arg = sig
  module IO : IO

  val run
    :  here_pos:Lexing.position
    -> config:Base_quickcheck.Test.Config.t option
    -> trials:int option (** if provided, overrides the number of trials in [config] *)
    -> examples:'a list
    -> (module Base_quickcheck.Test.S with type t = 'a)
    -> f:('a -> unit Or_error.t IO.t)
    -> (unit, 'a * Error.t) result IO.t
end
