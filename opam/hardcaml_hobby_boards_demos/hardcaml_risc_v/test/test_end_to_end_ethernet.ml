open! Core
open Hardcaml
open Hardcaml_test_harness
open Hardcaml_io_controller
open Hardcaml_io_framework_test
open Hardcaml_memory_controller
open Hardcaml_hobby_boards_kernel
open Hardcaml_hobby_boards_hardcaml_risc_v
open Ethernet_utils.For_testing
open! Bits

let debug = false

module Memory_config = struct
  let capacity_in_bytes = 256
  let num_read_channels = 1
  let num_write_channels = 1
  let address_width = 32
  let data_bus_width = 32
end

module Memory_controller = Bram_memory_controller.Make (Memory_config)

module Ethernet_tx = Ethernet.Tx.Make (struct
    let max_packets = 4
    let average_packet_size = 128
  end)

module Dma = Packet_to_memory.Make (Memory_controller.Memory_bus) (Ethernet.Axi32)
module Prepend_address = Prepend_address.Make (Ethernet.Axi32)

module Memory_to_axi32 =
  Memory_to_axi32.Make (Memory_controller.Memory_bus) (Ethernet.Axi32)

module Machine = struct
  open Signal
  open Always
  open Memory_controller.Memory_bus

  module I = struct
    type 'a t =
      { clocking : 'a Types.Clocking.t
      ; crsdv : 'a
      ; rxerr : 'a
      ; rxd : 'a [@bits 2]
      ; dma_out_enable : 'a
      ; dma_out_address : 'a [@bits 32]
      ; dma_out_length : 'a [@bits 16]
      }
    [@@deriving hardcaml]
  end

  module O = Ethernet_tx.O

  let create
    ~start_address
    (scope : Scope.t)
    { I.clocking; crsdv; rxerr; rxd; dma_out_enable; dma_out_address; dma_out_length }
    =
    let { Ethernet.Rx.O.axi_tx; _ } =
      Ethernet.Rx.hierarchical scope { Ethernet.Rx.I.clocking; crsdv; rxerr; rxd }
    in
    let dma_ready = wire 1 in
    let { Prepend_address.O.output_data_stream; _ } =
      Prepend_address.hierarchical
        scope
        { Prepend_address.I.clock = clocking.clock
        ; clear = clocking.clear
        ; config =
            { start_address = of_int_trunc ~width:32 start_address
            ; max_address = of_int_trunc ~width:32 Memory_config.capacity_in_bytes
            }
        ; input_data_stream = axi_tx
        ; output_data_stream_ready = { tready = dma_ready }
        }
    in
    let dma_to_memory_controller = Write_bus.Dest.Of_always.wire zero in
    let memory_controller_to_dma = Write_response.With_valid.Of_always.wire zero in
    let dma =
      Dma.hierarchical
        scope
        { Dma.I.clock = clocking.clock
        ; clear = clocking.clear
        ; in_ = output_data_stream
        ; out = Write_bus.Dest.Of_always.value dma_to_memory_controller
        ; out_ack = Write_response.With_valid.Of_always.value memory_controller_to_dma
        }
    in
    Signal.(dma_ready <-- dma.in_.tready);
    let dma_out_to_memory_controller = Read_bus.Dest.Of_always.wire zero in
    let memory_controller_to_dma_out = Read_response.With_valid.Of_always.wire zero in
    let ethernet_tx_ready = wire 1 in
    let dma_out =
      Memory_to_axi32.hierarchical
        scope
        { Memory_to_axi32.I.clock = clocking.clock
        ; clear = clocking.clear
        ; read_input =
            { valid = dma_out_enable
            ; value = { address = dma_out_address; length = dma_out_length }
            }
        ; memory = Read_bus.Dest.Of_always.value dma_out_to_memory_controller
        ; memory_response =
            Read_response.With_valid.Of_always.value memory_controller_to_dma_out
        ; output_packet = { tready = ethernet_tx_ready }
        }
    in
    let dma_out_ethernet_tx =
      Ethernet_tx.hierarchical
        scope
        { Ethernet_tx.I.clocking
        ; data_stream =
            { dma_out.output_packet with
              tdata = bswap dma_out.output_packet.tdata
            ; tkeep = reverse dma_out.output_packet.tkeep
            }
        }
    in
    Signal.(ethernet_tx_ready <-- dma_out_ethernet_tx.ready.tready);
    let controller =
      Memory_controller.hierarchical
        ~build_mode:Simulation
        ~priority_mode:Priority_order
        ~read_latency:1
        scope
        { Memory_controller.I.clock = clocking.clock
        ; clear = clocking.clear
        ; read_to_controller = [ dma_out.memory ]
        ; write_to_controller = [ dma.out ]
        }
    in
    compile
      [ Write_bus.Dest.Of_always.assign
          dma_to_memory_controller
          (List.nth_exn controller.write_to_controller 0)
      ; Write_response.With_valid.Of_always.assign
          memory_controller_to_dma
          (List.nth_exn controller.write_response 0)
      ; Read_bus.Dest.Of_always.assign
          dma_out_to_memory_controller
          (List.nth_exn controller.read_to_controller 0)
      ; Read_response.With_valid.Of_always.assign
          memory_controller_to_dma_out
          (List.nth_exn controller.read_response 0)
      ];
    { O.txd = dma_out_ethernet_tx.txd
    ; txen = dma_out_ethernet_tx.txen
    ; ready = dma_out_ethernet_tx.ready
    }
  ;;
end

module Harness = Cyclesim_harness.Make (Machine.I) (Machine.O)

let test ~verbose ~packets ~start_address =
  Harness.run
    ~waves_config:
      (if debug then Waves_config.to_home_subdirectory () else Waves_config.no_waves)
    ~trace:`All_named
    ~create:(Machine.create ~start_address)
    (fun ~inputs ~outputs sim ->
      inputs.clocking.clear := vdd;
      Cyclesim.cycle sim;
      inputs.clocking.clear := gnd;
      Cyclesim.cycle sim;
      let current_address = ref start_address in
      List.iter
        ~f:(fun packet ->
          let ethernet_rx_inputs =
            { Ethernet.Rx.I.clocking = inputs.clocking
            ; rxd = inputs.rxd
            ; crsdv = inputs.crsdv
            ; rxerr = inputs.rxerr
            }
          in
          sim_preamble_sfd sim ethernet_rx_inputs;
          sim_packet sim ~packet ethernet_rx_inputs;
          if verbose
          then (
            printf "Printing ram (not DMA response):\n";
            Test_util.print_ram sim;
            printf "Doing a DMA read:\n");
          let issue_read ~address ~length =
            inputs.dma_out_enable := vdd;
            inputs.dma_out_address := of_unsigned_int ~width:32 address;
            inputs.dma_out_length := of_unsigned_int ~width:16 length;
            Cyclesim.cycle sim;
            inputs.dma_out_enable := gnd;
            let data = ref [] in
            let store_outputs () =
              if to_bool !(outputs.txen) then data := !(outputs.txd) :: !data else ()
            in
            let count = ref 0 in
            while
              !count <> 10_000
              && List.length !data
                 < (String.length packet * (8 / width !(inputs.rxd)))
                   + ((Ethernet_types.fcs_bits + Ethernet_types.preamble_sfd_bits)
                      / width !(inputs.rxd))
            do
              Cyclesim.cycle sim;
              store_outputs ();
              Int.incr count
            done;
            let data = !data in
            let packet_bits =
              String.to_list packet
              |> List.map ~f:Char.to_int
              |> List.map ~f:(of_int_trunc ~width:8)
              |> concat_msb
            in
            check_tx_data
              ~output_data:data
              ~expected_data:packet_bits
              ~print_data_as_string:verbose
              ();
            if verbose then printf "Cycles: %i\n" !count else ()
          in
          issue_read ~address:!current_address ~length:(String.length packet);
          current_address
          := !current_address + Int.round_up ~to_multiple_of:4 (String.length packet))
        packets)
;;

let%expect_test "test" =
  test ~packets:[ "Hi"; "Goodbye" ] ~start_address:0 ~verbose:true;
  [%expect
    {|
    Printing ram (not DMA response):
    ("00000000  48 69 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |Hi..............|"
     "00000010  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "00000020  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "00000030  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "00000040  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "00000050  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "00000060  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "00000070  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "00000080  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "00000090  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "000000a0  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "000000b0  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "000000c0  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "000000d0  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "000000e0  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "000000f0  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|")
    Doing a DMA read:
    Hi
    Cycles: 59
    Printing ram (not DMA response):
    ("00000000  48 69 00 00 47 6f 6f 64  62 79 65 00 00 00 00 00  |Hi..Goodbye.....|"
     "00000010  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "00000020  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "00000030  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "00000040  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "00000050  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "00000060  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "00000070  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "00000080  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "00000090  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "000000a0  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "000000b0  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "000000c0  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "000000d0  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "000000e0  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "000000f0  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|")
    Doing a DMA read:
    Goodbye
    Cycles: 82
    |}];
  test
    ~packets:[ "Hello world!"; "What's going on!"; "Goodbye world"; ":(" ]
    ~start_address:8
    ~verbose:true;
  [%expect
    {|
    Printing ram (not DMA response):
    ("00000000  00 00 00 00 00 00 00 00  48 65 6c 6c 6f 20 77 6f  |........Hello wo|"
     "00000010  72 6c 64 21 00 00 00 00  00 00 00 00 00 00 00 00  |rld!............|"
     "00000020  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "00000030  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "00000040  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "00000050  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "00000060  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "00000070  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "00000080  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "00000090  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "000000a0  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "000000b0  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "000000c0  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "000000d0  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "000000e0  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "000000f0  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|")
    Doing a DMA read:
    Hello world!
    Cycles: 105
    Printing ram (not DMA response):
    ("00000000  00 00 00 00 00 00 00 00  48 65 6c 6c 6f 20 77 6f  |........Hello wo|"
     "00000010  72 6c 64 21 57 68 61 74  27 73 20 67 6f 69 6e 67  |rld!What's going|"
     "00000020  20 6f 6e 21 00 00 00 00  00 00 00 00 00 00 00 00  | on!............|"
     "00000030  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "00000040  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "00000050  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "00000060  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "00000070  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "00000080  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "00000090  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "000000a0  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "000000b0  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "000000c0  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "000000d0  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "000000e0  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "000000f0  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|")
    Doing a DMA read:
    What's going on!
    Cycles: 124
    Printing ram (not DMA response):
    ("00000000  00 00 00 00 00 00 00 00  48 65 6c 6c 6f 20 77 6f  |........Hello wo|"
     "00000010  72 6c 64 21 57 68 61 74  27 73 20 67 6f 69 6e 67  |rld!What's going|"
     "00000020  20 6f 6e 21 47 6f 6f 64  62 79 65 20 77 6f 72 6c  | on!Goodbye worl|"
     "00000030  64 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |d...............|"
     "00000040  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "00000050  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "00000060  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "00000070  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "00000080  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "00000090  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "000000a0  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "000000b0  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "000000c0  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "000000d0  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "000000e0  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "000000f0  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|")
    Doing a DMA read:
    Goodbye world
    Cycles: 112
    Printing ram (not DMA response):
    ("00000000  00 00 00 00 00 00 00 00  48 65 6c 6c 6f 20 77 6f  |........Hello wo|"
     "00000010  72 6c 64 21 57 68 61 74  27 73 20 67 6f 69 6e 67  |rld!What's going|"
     "00000020  20 6f 6e 21 47 6f 6f 64  62 79 65 20 77 6f 72 6c  | on!Goodbye worl|"
     "00000030  64 00 00 00 3a 28 00 00  00 00 00 00 00 00 00 00  |d...:(..........|"
     "00000040  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "00000050  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "00000060  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "00000070  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "00000080  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "00000090  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "000000a0  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "000000b0  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "000000c0  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "000000d0  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "000000e0  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "000000f0  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|")
    Doing a DMA read:
    :(
    Cycles: 59
    |}]
;;

let%expect_test "fuzz" =
  Quickcheck.test
    ~trials:10
    (String.gen_with_length (Random.int_incl 64 100) Char.gen_alpha)
    ~f:(fun test_str ->
      test ~packets:[ test_str; test_str ] ~start_address:0 ~verbose:false);
  [%expect {| |}]
;;
