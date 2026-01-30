(** This module constructs an entire system, including harts, a memory controller and an
    IO controller. The system is defined by three parts, a general config which includes
    the uart_config, a memory config that describes the shape of the memory controller,
    and a hart config which configures each hart. All harts must be homogeneous. It is
    similar to the external/hardcaml_risc_v system but it uses the hobby board IO
    components *)
open! Core

open Hardcaml
open Hardcaml_memory_controller
open Hardcaml_risc_v_hart

module Make
    (Hart_config : Hart_config_intf.S)
    (Memory_config : System_intf.Memory_config)
    (General_config : System_intf.Config)
    (Video_config : Framebuffer_expander.Config)
    (Axi4 : Axi4.S) : sig
  module Registers : Registers_intf.S
  module Memory_bus : Memory_bus_intf.S

  module I : sig
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; uart_rx : 'a
      ; eth_crsdv : 'a
      ; eth_rxerr : 'a
      ; eth_rxd : 'a
      ; memory : 'a Axi4.I.t
      }
    [@@deriving hardcaml]
  end

  module O : sig
    type 'a t =
      { registers : 'a Registers.t list
      ; ethernet_txen : 'a
      ; ethernet_txd : 'a
      ; uart_tx : 'a
      ; uart_rx_valid : 'a
      ; vga_red : 'a
      ; vga_green : 'a
      ; vga_blue : 'a
      ; vga_hsync : 'a
      ; vga_vsync : 'a
      ; memory : 'a Axi4.O.t
      }
    [@@deriving hardcaml]
  end

  val create : build_mode:Build_mode.t -> Scope.t -> Signal.t I.t -> Signal.t O.t
  val hierarchical : build_mode:Build_mode.t -> Scope.t -> Signal.t I.t -> Signal.t O.t
end
