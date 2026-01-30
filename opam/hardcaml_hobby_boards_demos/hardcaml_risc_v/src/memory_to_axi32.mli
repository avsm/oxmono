open! Core
open Hardcaml_axi
open Hardcaml_memory_controller

module Make (Memory : Memory_bus_intf.S) (Axi : Stream.S) :
  Memory_to_axi32_intf.M(Memory)(Axi).S
