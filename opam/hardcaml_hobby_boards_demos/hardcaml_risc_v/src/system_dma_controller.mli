open! Core
open Hardcaml_memory_controller
open Hardcaml_io_controller
open Hardcaml_risc_v

module Make
    (General_config : System_intf.Config)
    (Memory_config : System_intf.Memory_config)
    (Memory : Memory_bus_intf.S)
    (Memory_to_packet8 : Memory_to_packet8_intf.M(Memory)(Axi8).S) :
  System_dma_controller_intf.M(Memory)(Memory_to_packet8).S
