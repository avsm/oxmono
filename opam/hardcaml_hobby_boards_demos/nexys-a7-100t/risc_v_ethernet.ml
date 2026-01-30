open! Core
open Hardcaml
open Signal

include struct
  open Hardcaml_hobby_boards_kernel
  module Board = Board
  module Utils = Utils
  module Nexys = Nexys_a7_100t
  module Uart = Uart
  module Uart_types = Uart_types
  module Vga = Hardcaml_hobby_boards_kernel.Vga
  module Hardcaml_risc_v_hart = Hardcaml_risc_v_hart
  module Risc_v = Hardcaml_hobby_boards_hardcaml_risc_v
  module BUFR = Clock_utils.BUFR

  (* Divide the 100MHz board clock by 2 to create a 50MHz clock for the design to run at
     and output as the refclk *)
  module BUFR_COMP = BUFR.Make (struct
      include BUFR.P

      let bufr_divide = "2"
    end)
end

let design_frequency = 50_000_000

let uart_config =
  Uart.Config.
    { data_bits = Uart_types.Data_bits.Enum.of_enum (module Signal) Eight
    ; parity = Uart_types.Parity.Enum.of_enum (module Signal) None
    ; stop_bits = Uart_types.Stop_bits.Enum.of_enum (module Signal) One
    ; clocks_per_bit =
        of_unsigned_int ~width:port_widths.clocks_per_bit (design_frequency / 115_200)
    }
;;

module Cpu =
  Risc_v.System_with_bram.Make
    (struct
      let register_width = Hardcaml_risc_v_hart.Register_width.B32
      let num_registers = 32
      let design_frequency = design_frequency
    end)
    (struct
      let capacity_in_bytes = 65536
      let ethernet_start_address = 0
      let frame_buffer_bytes = capacity_in_bytes
    end)
    (struct
      let num_harts = 1
      let io_controller = uart_config
      let fpga_mac_address = of_string "48'h0018_3e04_c882"
    end)
    (struct
      let input_width = 2
      let input_height = 3
      let output_width = 4
      let output_height = 6
      let start_address = 64
      let vga_spec = Vga.Spec.testing
      let vga_clock_div = design_frequency / vga_spec.clock_hz
    end)

let generate_clear clock_50 reset_n =
  let reset_chain =
    reg_fb
      (Reg_spec.create ~clock:clock_50 ~reset:reset_n ~reset_edge:Falling ())
      ~reset_to:(Bits.ones 16)
      ~width:16
      ~f:(fun d -> sll d ~by:1)
  in
  reg (Reg_spec.create ~clock:clock_50 ()) (msb reset_chain)
;;

let create () =
  let scope = Scope.create ~flatten_design:true () in
  let board = Board.create () in
  let clocking = Nexys.Clock_and_reset.create board in
  let clear_100 = Utils.generate_clear clocking in
  let clock_inputs = BUFR_COMP.I.{ ce = vdd; clr = clear_100; i = clocking.clock_100 } in
  let _ethernet_inputs = Nexys.Ethernet.create board in
  let%tydi { o = clock_50 } = BUFR_COMP.create clock_inputs in
  let clear_50 = generate_clear clock_50 clocking.reset_n in
  let uart = Nexys.Uart.create board in
  let { Cpu.O.registers = _; uart_tx = cpu_uart_tx; ethernet_txen; ethernet_txd; _ } =
    Cpu.create
      ~read_latency:2
      ~build_mode:Synthesis
      scope
      { clock = clock_50
      ; clear = clear_50
      ; uart_rx = uart.rxd
      ; eth_crsdv = gnd
      ; eth_rxerr = gnd
      ; eth_rxd = zero 2
      }
  in
  Nexys.Ethernet.complete
    board
    { Nexys.Ethernet.O.txen = ethernet_txen
    ; txd = ethernet_txd
    ; rstn = clocking.reset_n
    ; refclk = clock_50
    };
  Nexys.Uart.complete board { Nexys.Uart.O.rts = gnd; txd = cpu_uart_tx };
  board
;;
