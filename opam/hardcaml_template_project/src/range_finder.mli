(** An example design that takes a series of input values and calculates the range between
    the largest and smallest one. *)

open! Core
open! Hardcaml

val num_bits : int

(*_ The module interface exports the same I/O records. Note that the widths don't need to
    be specified in the interface. *)
module I : sig
  type 'a t =
    { clock : 'a
    ; clear : 'a
    ; start : 'a
    ; finish : 'a
    ; data_in : 'a
    ; data_in_valid : 'a
    }
  [@@deriving hardcaml]
end

module O : sig
  type 'a t = { range : 'a With_valid.t } [@@deriving hardcaml]
end

val hierarchical : Scope.t -> Signal.t I.t -> Signal.t O.t
