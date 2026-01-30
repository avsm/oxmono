(** Arbitrate between two Ibus masters, where one has priority. This ensures that, when
    the primary bus starts a transaction, the other bus finishes its current transaction
    before relinquishing control *)

open! Core
open! Hardcaml

module Make (Ibus : Internal_bus.S) : sig
  module I : sig
    type 'a t =
      { clocking : 'a Types.Clocking.t
      ; primary_master : 'a Ibus.Master_to_slave.t
      ; secondary_master : 'a Ibus.Master_to_slave.t
      ; slave : 'a Ibus.Slave_to_master.t
      }
    [@@deriving hardcaml]
  end

  module O : sig
    type 'a t =
      { master : 'a Ibus.Master_to_slave.t
      ; primary_slave : 'a Ibus.Slave_to_master.t
      ; secondary_slave : 'a Ibus.Slave_to_master.t
      }
    [@@deriving hardcaml]
  end

  val create : Scope.t -> Interface.Create_fn(I)(O).t
  val hierarchical : ?instance:string -> Scope.t -> Interface.Create_fn(I)(O).t
end
