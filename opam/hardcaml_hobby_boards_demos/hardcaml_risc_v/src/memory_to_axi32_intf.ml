(** This module is similar to the Memory_to_packet8 module in external/hardcaml_risc_v.
    The module takes an enable signal with a length and address and writes out the memory
    byte by byte to a 32-bit output stream. When connected to a Ethernet_tx this should
    allow us to send ethernet frames. The address must be aligned otherwise we ignore it
    and don't read anything. *)

open! Core
open Hardcaml
open Hardcaml_axi
open Hardcaml_memory_controller

module M (Memory : Memory_bus_intf.S) (Axi : Stream.S) = struct
  module type S = sig
    module Input : sig
      type 'a t =
        { length : 'a
        ; address : 'a
        }
      [@@deriving hardcaml]

      module With_valid : With_valid.Wrap.S with type 'a value := 'a t
    end

    module I : sig
      type 'a t =
        { clock : 'a
        ; clear : 'a
        ; read_input : 'a Input.With_valid.t
        ; output_packet : 'a Axi.Dest.t
        ; memory : 'a Memory.Read_bus.Dest.t
        ; memory_response : 'a Memory.Read_response.With_valid.t
        }
      [@@deriving hardcaml]
    end

    module O : sig
      type 'a t =
        { ready : 'a
        ; output_packet : 'a Axi.Source.t
        ; memory : 'a Memory.Read_bus.Source.t
        }
      [@@deriving hardcaml]
    end

    val hierarchical : Scope.t -> Signal.t I.t -> Signal.t O.t
  end
end
