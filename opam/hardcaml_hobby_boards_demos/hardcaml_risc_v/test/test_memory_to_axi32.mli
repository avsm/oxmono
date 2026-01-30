open! Hardcaml

module Packet : sig
  type t =
    { contents : String.t
    ; dma_address : int
    ; dma_length : int
    }
end

val test : packets:Packet.t list -> unit
