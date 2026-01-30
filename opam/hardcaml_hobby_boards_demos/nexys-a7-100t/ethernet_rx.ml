open! Core
open Hardcaml
open! Signal

include struct
  open Hardcaml_hobby_boards_kernel
  module Board = Board
  module Utils = Utils
  module Nexys = Nexys_a7_100t

  module Udp_packet_decoder = Udp_packet_decoder.Make (struct
      let filter_config = Udp_packet_decoder_intf.Filter_config.No_filter
    end)

  module Ethernet = Ethernet
  module BUFR = Clock_utils.BUFR

  (* Divide the 100MHz board clock by 2 to create a 50MHz clock for the design to run at
     and output as the refclk *)
  module BUFR_COMP = BUFR.Make (struct
      include BUFR.P

      let bufr_divide = "2"
    end)
end

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
  let scope = Scope.create () in
  let board = Board.create () in
  let clocking = Nexys.Clock_and_reset.create board in
  let clear = Utils.generate_clear clocking in
  let inputs = Nexys.Ethernet.create board in
  let clock_inputs = BUFR_COMP.I.{ ce = vdd; clr = clear; i = clocking.clock_100 } in
  let%tydi { o = clock_50 } = BUFR_COMP.create clock_inputs in
  let clear_50 = generate_clear clock_50 clocking.reset_n in
  let spec = Reg_spec.create ~clock:clock_50 ~clear:clear_50 () in
  let ethernet_rx_output =
    Ethernet.Rx.create
      scope
      { Ethernet.Rx.I.clocking = { clock = clock_50; clear = clear_50 }
      ; crsdv = inputs.crsdv
      ; rxerr = inputs.rxerr
      ; rxd = inputs.rxd
      }
  in
  let udp_decoder_output =
    Udp_packet_decoder.create
      scope
      { Udp_packet_decoder.I.clocking = { clock = clock_50; clear = clear_50 }
      ; axi_rx = ethernet_rx_output.axi_tx
      }
  in
  let error_counter =
    reg_fb spec ~width:8 ~f:(fun d -> d +: uresize ethernet_rx_output.rx_error ~width:8)
  in
  let udp_counter =
    reg_fb spec ~width:8 ~f:(fun d ->
      d
      +: uresize
           (lsb udp_decoder_output.axi_tx.tuser &: udp_decoder_output.axi_tx.tvalid)
           ~width:8)
  in
  Nexys.Leds.complete board (error_counter @: udp_counter);
  Nexys.Ethernet.complete
    board
    { Nexys.Ethernet.O.txen = gnd
    ; txd = zero 2
    ; rstn = clocking.reset_n
    ; refclk = clock_50
    };
  board
;;
