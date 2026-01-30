open! Core
open Hardcaml
open! Signal

include struct
  open Hardcaml_hobby_boards_kernel
  module Board = Board
  module Utils = Utils
  module Nexys = Nexys_a7_100t
  module Udp_packet_generator = Udp_packet_generator
  module Udp_packet_stream = Udp_packet_stream
  module Ethernet = Ethernet
  module Ethernet_types = Ethernet_types
  module Axi = Ethernet.Axi32
  module BUFR = Clock_utils.BUFR

  (* Divide the 100MHz board clock by 2 to create a 50MHz clock for the design to run at
     and output as the refclk *)
  module BUFR_COMP = BUFR.Make (struct
      include BUFR.P

      let bufr_divide = "2"
    end)

  module Ethernet_tx = Ethernet.Tx.Make (struct
      let max_packets = 4
      let average_packet_size = 128
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
  let clear_100 = Utils.generate_clear clocking in
  let clock_inputs = BUFR_COMP.I.{ ce = vdd; clr = clear_100; i = clocking.clock_100 } in
  let inputs = Nexys.Ethernet.create board in
  let%tydi { o = clock_50 } = BUFR_COMP.create clock_inputs in
  let clear_50 = generate_clear clock_50 clocking.reset_n in
  let spec = Reg_spec.create ~clock:clock_50 ~clear:clear_50 () in
  let config =
    Udp_packet_generator.Config.
      { fpga_mac = of_string "48'h0018_3e04_c882"
      ; fpga_ip = of_string "32'hc0a8_0164"
      ; fpga_port = of_int_trunc ~width:Ethernet_types.Udp.port_widths.src_port 49152
      ; host_mac = of_string "48'h607d09a40854"
      ; host_ip = of_string "32'hc0a8_0165"
      ; host_port = of_int_trunc ~width:Ethernet_types.Udp.port_widths.dst_port 49152
      ; ip_header_checksum = of_string "16'hf699"
      }
  in
  let ethernet_rx_output =
    Ethernet.Rx.create
      scope
      { Ethernet.Rx.I.clocking = { clock = clock_50; clear = clear_50 }
      ; crsdv = inputs.crsdv
      ; rxerr = inputs.rxerr
      ; rxd = inputs.rxd
      }
  in
  let udp_packet_stream_output =
    Udp_packet_stream.create
      scope
      { Udp_packet_stream.I.clocking = { clock = clock_50; clear = clear_50 }
      ; config = Udp_packet_stream.Config.{ dst_mac = config.fpga_mac }
      ; axi_rx = ethernet_rx_output.axi_tx
      }
  in
  let udp_packet =
    Udp_packet_generator.create
      scope
      { Udp_packet_generator.I.clocking = { clock = clock_50; clear = clear_50 }; config }
  in
  let switches = Nexys.Switches.create board in
  let sending_data = wire 1 in
  let sending_data_reg = reg spec sending_data in
  (* Wait to finish sending the ethernet data stream to the ethernet tx module before
     switching to a different mode *)
  let%hw sending_state =
    reg_fb spec ~width:1 ~f:(fun d ->
      mux2 (d <>: lsb switches &: ~:sending_data) (lsb switches) d)
  in
  let%hw data_stream =
    mux2
      sending_state
      (Axi.Source.Of_signal.pack udp_packet.axi_tx)
      (Axi.Source.Of_signal.pack udp_packet_stream_output.axi_udp_packet)
  in
  let data_stream_axi = Axi.Source.Of_signal.unpack data_stream in
  sending_data
  <-- mux2
        (data_stream_axi.tvalid &: lsb data_stream_axi.tuser)
        vdd
        (mux2 (data_stream_axi.tvalid &: data_stream_axi.tlast) gnd sending_data_reg);
  let ethernet_tx =
    Ethernet_tx.create
      scope
      { Ethernet_tx.I.clocking = { clock = clock_50; clear = clear_50 }
      ; data_stream = data_stream_axi
      }
  in
  let outputs =
    Nexys.Ethernet.O.
      { txen = ethernet_tx.txen
      ; txd = ethernet_tx.txd
      ; rstn = clocking.reset_n
      ; refclk = clock_50
      }
  in
  let error_counter =
    reg_fb spec ~width:8 ~f:(fun d -> d +: uresize ethernet_rx_output.rx_error ~width:8)
  in
  let udp_counter =
    reg_fb spec ~width:8 ~f:(fun d ->
      d +: uresize (lsb data_stream_axi.tuser &: data_stream_axi.tvalid) ~width:8)
  in
  Nexys.Leds.complete board (error_counter @: udp_counter);
  Nexys.Ethernet.complete board outputs;
  board
;;
