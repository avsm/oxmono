(** Arbitrate between two interfaces, ensuring that, when either interface wishes to
    starts a transaction, the other finishes its current transaction before relinquishing
    control.

    By default, the primary interface controls the bus and the secondary interface can
    only gain access if the primary interface is not currently in, and does not wish to
    start a transaction.

    A transaction could be (for example):
    - Ibus read/write
    - AXIS packet transfer *)

open! Base
open! Hardcaml
open Signal

module Config = struct
  module type S = sig
    module Source : Hardcaml.Interface.S
    module Dest : Hardcaml.Interface.S

    val start_transaction : t Source.t -> t
    val finished_transaction : t Source.t -> t Dest.t -> t
  end
end

module type Transaction_arbiter = sig
  module Config = Config

  module Make (Config : Config.S) : sig
    open Config

    module I : sig
      type 'a t =
        { clocking : 'a Types.Clocking.t
        ; primary_source : 'a Source.t
        ; secondary_source : 'a Source.t
        ; dest : 'a Dest.t
        }
      [@@deriving hardcaml]
    end

    module O : sig
      type 'a t =
        { source : 'a Source.t
        ; primary_dest : 'a Dest.t
        ; secondary_dest : 'a Dest.t
        ; first : 'a
        }
      [@@deriving hardcaml]
    end

    val create : Scope.t -> Interface.Create_fn(I)(O).t
    val hierarchical : Scope.t -> Interface.Create_fn(I)(O).t
  end
end
