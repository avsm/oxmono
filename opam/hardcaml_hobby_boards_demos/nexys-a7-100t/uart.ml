open! Core
open Hardcaml
open Signal

include struct
  open Hardcaml_hobby_boards_kernel
  module Board = Board
  module Utils = Utils
  module Nexys = Nexys_a7_100t
  module Uart_types = Uart_types
  module Uart = Uart
end

let create () =
  let scope = Scope.create ~flatten_design:false () in
  let board = Board.create () in
  let clocking = Nexys.Clock_and_reset.create board in
  let clear = Utils.generate_clear clocking in
  let uart = Nexys.Uart.create board in
  let config =
    { Uart.Config.data_bits = Uart_types.Data_bits.Enum.of_enum (module Signal) Eight
    ; parity = Uart_types.Parity.Enum.of_enum (module Signal) Even
    ; stop_bits = Uart_types.Stop_bits.Enum.of_enum (module Signal) One
    ; clocks_per_bit =
        of_unsigned_int
          ~width:Uart.Config.port_widths.clocks_per_bit
          (100_000_000 / 115_200)
    }
  in
  let uart_rx =
    Uart.Rx.create
      ~align_rxdata_to_lsb:true
      scope
      { Uart.Rx.I.clocking = { clock = clocking.clock_100; clear }
      ; enable = vdd
      ; config
      ; rxd = uart.rxd
      }
  in
  let uart_tx =
    Uart.Tx.create
      scope
      { Uart.Tx.I.clocking = { clock = clocking.clock_100; clear }
      ; config
      ; data_in = uart_rx.data_out
      ; data_in_valid = uart_rx.data_out_valid
      }
  in
  Nexys.Uart.complete board { Nexys.Uart.O.rts = gnd; txd = uart_tx.txd };
  board
;;
