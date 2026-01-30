open! Core
open Hardcaml
open Hardcaml_memory_controller
open Hardcaml_io_controller
open Hardcaml_risc_v
open Hardcaml_hobby_boards_kernel

(** System dma controller that is similar to the external/hardcaml_risc_v system DMA
    controller. It instantiates the hobby boards UART instead of the
    external/hardcaml_risc_v/hardcaml_uart one *)

module M
    (Memory : Memory_bus_intf.S)
    (Memory_to_packet8 : Memory_to_packet8_intf.M(Memory)(Axi8).S) =
struct
  module type S = sig
    module I : sig
      type 'a t =
        { clock : 'a
        ; clear : 'a
        ; uart_rx : 'a
        ; eth_crsdv : 'a
        ; eth_rxerr : 'a
        ; eth_rxd : 'a
        ; tx_input : 'a Memory_to_packet8.Input.With_valid.t
        ; uart_read_request : 'a Memory.Read_bus.Dest.t
        ; ethernet_read_request : 'a Memory.Read_bus.Dest.t
        ; uart_write_request : 'a Memory.Write_bus.Dest.t
        ; ethernet_write_request : 'a Memory.Write_bus.Dest.t
        ; uart_read_response : 'a Memory.Read_response.With_valid.t
        ; ethernet_read_response : 'a Memory.Read_response.With_valid.t
        ; uart_write_response : 'a Memory.Write_response.With_valid.t
        ; ethernet_write_response : 'a Memory.Write_response.With_valid.t
        }
      [@@deriving hardcaml]
    end

    module O : sig
      type 'a t =
        { uart_rx_valid : 'a
        ; uart_tx : 'a
        ; ethernet_txen : 'a
        ; ethernet_txd : 'a
        ; dma_tx_ready : 'a
        ; uart_read_request : 'a Memory.Read_bus.Source.t
        ; ethernet_read_request : 'a Memory.Read_bus.Source.t
        ; uart_write_request : 'a Memory.Write_bus.Source.t
        ; ethernet_write_request : 'a Memory.Write_bus.Source.t
        ; frame_buffer_write_done : 'a
        ; clear_message : 'a
        }
      [@@deriving hardcaml, fields ~getters]
    end

    val create
      :  uart_config:Signal.t Uart.Config.t
      -> Scope.t
      -> Signal.t I.t
      -> Signal.t O.t

    val hierarchical
      :  ?instance:string
      -> uart_config:Signal.t Uart.Config.t
      -> Scope.t
      -> Signal.t I.t
      -> Signal.t O.t
  end
end
