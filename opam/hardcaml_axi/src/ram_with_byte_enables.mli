(** RAM with per byte enable and configurable size. *)

open! Base
open! Hardcaml

include Ram_with_byte_enables_intf.Ram_with_byte_enables (** @inline *)
