open! Core
open Hardcaml
open Hardcaml_memory_controller
open Hardcaml_io_framework
open Hardcaml_io_controller
open Hardcaml_circuits
open Hardcaml_hobby_boards_kernel
open Hardcaml_risc_v
open Signal

module Make
    (General_config : System_intf.Config)
    (Memory_config : System_intf.Memory_config)
    (Memory : Memory_bus_intf.S)
    (Memory_to_packet8 : Memory_to_packet8_intf.M(Memory)(Axi8).S) =
struct
  module Write_dpath = Datapath_register.Make (Memory.Write)
  module Memory_to_axi32 = Memory_to_axi32.Make (Memory) (Ethernet.Axi32)
  module Prepend_address = Prepend_address.Make (Ethernet.Axi32)

  module Udp_packet_decoder = Udp_packet_decoder.Make (struct
      let filter_config =
        Udp_packet_decoder_intf.Filter_config.Destination_mac_filter
          General_config.fpga_mac_address
      ;;
    end)

  module Ethernet_tx = Ethernet.Tx.Make (struct
      let max_packets = 4
      let average_packet_size = 128 / (Ethernet.Config.data_bits / 8)
    end)

  module I = struct
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; uart_rx : 'a
      ; eth_crsdv : 'a
      ; eth_rxerr : 'a
      ; eth_rxd : 'a [@bits 2]
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

  module O = struct
    type 'a t =
      { uart_rx_valid : 'a
      ; uart_tx : 'a
      ; ethernet_txen : 'a
      ; ethernet_txd : 'a [@bits 2]
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

  let create
    ~uart_config
    scope
    { I.clock
    ; clear
    ; uart_rx
    ; eth_crsdv
    ; eth_rxerr
    ; eth_rxd
    ; tx_input
    ; uart_read_request
    ; ethernet_read_request
    ; uart_write_request
    ; ethernet_write_request
    ; uart_read_response
    ; ethernet_read_response
    ; uart_write_response
    ; ethernet_write_response
    }
    =
    let module Uart_packet_to_memory = Packet_to_memory.Make (Memory) (Axi8) in
    let module Udp_packet_to_memory = Packet_to_memory.Make (Memory) (Ethernet.Axi32) in
    let module Serial_buffer =
      Serial_buffer.Make (struct
        let serial_input_width = 8
      end)
    in
    let module Serial_to_packet =
      Serial_to_packet.Make
        (struct
          let header = 'Q'
        end)
        (Axi8)
    in
    let module Router =
      Router.Make
        (struct
          let num_tags = 2
        end)
        (Axi8)
    in
    let module Pulse = Pulse.Make (Axi8) in
    let { Uart.Rx.O.data_out_valid = uart_rx_valid; data_out = uart_rx_data; error = _ } =
      Uart.Rx.create
        ~align_rxdata_to_lsb:true
        scope
        { Uart.Rx.I.clocking = { clock; clear }
        ; enable = vdd
        ; config = uart_config
        ; rxd = uart_rx
        }
    in
    let serial_to_packet_ready = wire 1 in
    let { Serial_buffer.O.out_valid = serial_buffer_valid; out_data = serial_buffer_data }
      =
      Serial_buffer.hierarchical
        ~capacity:8096
        scope
        { Serial_buffer.I.clock
        ; clear
        ; in_valid = uart_rx_valid
        ; in_data = sel_bottom ~width:8 uart_rx_data
        ; out_ready = serial_to_packet_ready
        }
    in
    let dpath_ready = wire 1 in
    let { Serial_to_packet.O.dn; up_ready = serial_to_packet_ready' } =
      Serial_to_packet.hierarchical
        scope
        { Serial_to_packet.I.clock
        ; clear
        ; in_valid = serial_buffer_valid
        ; in_data = serial_buffer_data
        ; dn = { tready = dpath_ready }
        }
    in
    let router_ready = wire 1 in
    let { Axi8.Datapath_register.IO.source = dn; dest = dpath_ready' } =
      Axi8.Datapath_register.hierarchical
        scope
        { clock
        ; clear
        ; i = { Axi8.Datapath_register.IO.source = dn; dest = { tready = router_ready } }
        }
    in
    dpath_ready <-- dpath_ready'.tready;
    serial_to_packet_ready <-- serial_to_packet_ready';
    let dma_dpath_ready = wire 1 in
    let pulse_ready = wire 1 in
    let router =
      Router.hierarchical
        scope
        { Router.I.clock
        ; clear
        ; up = dn
        ; dns = [ { tready = dma_dpath_ready }; { tready = pulse_ready } ]
        }
    in
    router_ready <-- router.up.tready;
    let uart_dma_ready = wire 1 in
    let { Axi8.Datapath_register.IO.source = uart_dma; dest = dpath_ready' } =
      Axi8.Datapath_register.hierarchical
        scope
        { clock
        ; clear
        ; i =
            { Axi8.Datapath_register.IO.source = List.nth_exn router.dns 0
            ; dest = { tready = uart_dma_ready }
            }
        }
    in
    let { Ethernet.Rx.O.axi_tx = ethernet_rx_axi; _ } =
      Ethernet.Rx.hierarchical
        scope
        { Ethernet.Rx.I.clocking = { clock; clear }
        ; crsdv = eth_crsdv
        ; rxerr = eth_rxerr
        ; rxd = eth_rxd
        }
    in
    let { Udp_packet_decoder.O.axi_tx = udp_dma; _ } =
      Udp_packet_decoder.hierarchical
        scope
        { Udp_packet_decoder.I.clocking = { clock; clear }; axi_rx = ethernet_rx_axi }
    in
    let { Prepend_address.O.output_data_stream = udp_dma_with_address; _ } =
      Prepend_address.hierarchical
        scope
        { Prepend_address.I.clock
        ; clear
        ; config =
            { start_address = of_int_trunc ~width:32 Memory_config.ethernet_start_address
            ; max_address =
                of_int_trunc ~width:32 Memory_config.ethernet_start_address
                +:. Memory_config.frame_buffer_bytes
            }
        ; input_data_stream = udp_dma
        ; output_data_stream_ready = { tready = vdd }
        }
    in
    let ethernet_dma_write_dpath_ready = wire 1 in
    let ethernet_dma =
      Udp_packet_to_memory.hierarchical
        scope
        { Udp_packet_to_memory.I.clock
        ; clear
        ; in_ = udp_dma_with_address
        ; out = { ready = ethernet_dma_write_dpath_ready }
        ; out_ack = ethernet_write_response
        }
    in
    let ethernet_dma_write_dpath_reg =
      Write_dpath.hierarchical
        scope
        { Write_dpath.I.clock
        ; clear
        ; i =
            { valid = ethernet_dma.out.valid
            ; data = ethernet_dma.out.data
            ; ready = ethernet_write_request.ready
            }
        }
    in
    ethernet_dma_write_dpath_ready <-- ethernet_dma_write_dpath_reg.ready;
    dma_dpath_ready <-- dpath_ready'.tready;
    let dma_write_dpath_ready = wire 1 in
    let uart_dma =
      Uart_packet_to_memory.hierarchical
        scope
        { Uart_packet_to_memory.I.clock
        ; clear
        ; in_ = uart_dma
        ; out = { ready = dma_write_dpath_ready }
        ; out_ack = uart_write_response
        }
    in
    uart_dma_ready <-- uart_dma.in_.tready;
    let dma_write_dpath_reg =
      Write_dpath.hierarchical
        scope
        { Write_dpath.I.clock
        ; clear
        ; i =
            { valid = uart_dma.out.valid
            ; data = uart_dma.out.data
            ; ready = uart_write_request.ready
            }
        }
    in
    let%hw uart_write_valid = dma_write_dpath_reg.valid in
    dma_write_dpath_ready <-- dma_write_dpath_reg.ready;
    let pulse =
      Pulse.hierarchical scope { Pulse.I.clock; clear; up = List.nth_exn router.dns 1 }
    in
    pulse_ready <-- pulse.up.tready;
    let uart_tx_ready = wire 1 in
    let dma_out_packet8 =
      Memory_to_packet8.hierarchical
        scope
        { Memory_to_packet8.I.clock
        ; clear
        ; enable = tx_input
        ; memory = uart_read_request
        ; memory_response = uart_read_response
        ; output_packet = { tready = uart_tx_ready }
        }
    in
    let dma_out_uart_tx =
      Uart.Tx.hierarchical
        scope
        { Uart.Tx.I.clocking = { clock; clear }
        ; config = uart_config
        ; data_in_valid = dma_out_packet8.output_packet.tvalid
        ; data_in = uextend ~width:9 dma_out_packet8.output_packet.tdata
        }
    in
    let ethernet_tx_ready = wire 1 in
    let dma_out_axi32 =
      Memory_to_axi32.hierarchical
        scope
        { Memory_to_axi32.I.clock
        ; clear
        ; read_input =
            { valid = tx_input.valid
            ; value = { length = tx_input.value.length; address = tx_input.value.address }
            }
        ; memory = ethernet_read_request
        ; memory_response = ethernet_read_response
        ; output_packet = { tready = ethernet_tx_ready }
        }
    in
    let dma_out_ethernet_tx =
      Ethernet_tx.hierarchical
        scope
        { Ethernet_tx.I.clocking = { clock; clear }
        ; data_stream =
            { dma_out_axi32.output_packet with
              tdata = bswap dma_out_axi32.output_packet.tdata
            ; tkeep = reverse dma_out_axi32.output_packet.tkeep
            }
        }
    in
    uart_tx_ready <-- dma_out_uart_tx.data_in_ready;
    ethernet_tx_ready <-- dma_out_ethernet_tx.ready.tready;
    { O.uart_write_request = { valid = uart_write_valid; data = dma_write_dpath_reg.data }
    ; ethernet_write_request =
        { valid = ethernet_dma_write_dpath_reg.valid
        ; data = ethernet_dma_write_dpath_reg.data
        }
    ; uart_read_request = dma_out_packet8.memory
    ; ethernet_read_request = dma_out_axi32.memory
    ; uart_tx = dma_out_uart_tx.txd
    ; ethernet_txen = dma_out_ethernet_tx.txen
    ; ethernet_txd = dma_out_ethernet_tx.txd
    ; uart_rx_valid
    ; clear_message = pulse.signal
    ; dma_tx_ready = dma_out_packet8.ready
    ; frame_buffer_write_done = udp_dma_with_address.tlast &: udp_dma_with_address.tvalid
    }
  ;;

  let hierarchical ?instance ~uart_config (scope : Scope.t) (input : Signal.t I.t) =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical
      ?instance
      ~scope
      ~name:"system_dma_controller"
      (create ~uart_config)
      input
  ;;
end
