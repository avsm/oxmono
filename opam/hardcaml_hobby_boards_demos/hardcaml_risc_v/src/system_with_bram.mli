(** This module extends system with a BRAM based memory for a simple core. *)
open! Core

open Hardcaml
open Hardcaml_memory_controller
open Hardcaml_risc_v_hart

module Make
    (Hart_config : Hart_config_intf.S)
    (Memory_config : System_intf.Memory_config)
    (General_config : System_intf.Config)
    (Video_config : Framebuffer_expander.Config) : sig
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
      }
    [@@deriving hardcaml]
  end

  val create
    :  build_mode:Build_mode.t
    -> read_latency:int
    -> Scope.t
    -> Signal.t I.t
    -> Signal.t O.t

  val hierarchical
    :  build_mode:Build_mode.t
    -> read_latency:int
    -> Scope.t
    -> Signal.t I.t
    -> Signal.t O.t
end
