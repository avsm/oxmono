(** Prepend a memory address to the start of an axi stream input. This memory address
    starts at a configurable value and increments by bytes_per_word for every tvalid seen.
    The prepended memory address must be less than the max_address. If a data stream
    reaches or overflows past the max_address, the prepend address is reset to the
    start_address for the next data stream *)

open! Core
open Hardcaml
open Hardcaml_axi

module M (Axi : Stream.S) = struct
  module type S = sig
    module Config : sig
      type 'a t =
        { start_address : 'a
        ; max_address : 'a
        }
      [@@deriving hardcaml]
    end

    module I : sig
      type 'a t =
        { clock : 'a
        ; clear : 'a
        ; config : 'a Config.t
        ; input_data_stream : 'a Axi.Source.t
        ; output_data_stream_ready : 'a Axi.Dest.t
        }
      [@@deriving hardcaml]
    end

    module O : sig
      type 'a t =
        { ready : 'a Axi.Dest.t
        ; output_data_stream : 'a Axi.Source.t
        }
      [@@deriving hardcaml]
    end

    val hierarchical : Scope.t -> Signal.t I.t -> Signal.t O.t
  end
end
