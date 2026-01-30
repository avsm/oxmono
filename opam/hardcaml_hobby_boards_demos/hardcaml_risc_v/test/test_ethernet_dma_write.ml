open! Core
open Hardcaml
open Hardcaml_test_harness
open Hardcaml_io_controller
open Hardcaml_io_framework_test
open Hardcaml_memory_controller
open Hardcaml_hobby_boards_kernel
open Hardcaml_hobby_boards_hardcaml_risc_v
open Ethernet_utils.For_testing
open Bits

let debug = false

module Memory_config = struct
  let capacity_in_bytes = 128
  let num_read_channels = 1
  let num_write_channels = 1
  let address_width = 32
  let data_bus_width = 32
end

module Memory_controller = Bram_memory_controller.Make (Memory_config)
open Memory_controller.Memory_bus
module Dma = Packet_to_memory.Make (Memory_controller.Memory_bus) (Ethernet.Axi32)
module Prepend_address = Prepend_address.Make (Ethernet.Axi32)

module Machine = struct
  module I = Ethernet.Rx.I

  module O = struct
    type 'a t =
      { write_response : 'a Write_response.With_valid.t
      ; read_response : 'a Read_response.With_valid.t
      }
    [@@deriving hardcaml]
  end

  let create (scope : Scope.t) ~address { I.clocking; crsdv; rxerr; rxd } =
    let { Ethernet.Rx.O.axi_tx; _ } =
      Ethernet.Rx.hierarchical scope { Ethernet.Rx.I.clocking; crsdv; rxerr; rxd }
    in
    let dma_ready = Signal.wire 1 in
    let { Prepend_address.O.output_data_stream; _ } =
      Prepend_address.hierarchical
        scope
        { Prepend_address.I.clock = clocking.clock
        ; clear = clocking.clear
        ; config =
            { start_address = Signal.of_int_trunc ~width:address_width address
            ; max_address = Signal.of_int_trunc ~width:address_width 112
            }
        ; input_data_stream = axi_tx
        ; output_data_stream_ready = { tready = dma_ready }
        }
    in
    let dma_to_memory_controller = Write_bus.Dest.Of_always.wire Signal.zero in
    let memory_controller_to_dma = Write_response.With_valid.Of_always.wire Signal.zero in
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
    let controller =
      Memory_controller.hierarchical
        ~build_mode:Simulation
        ~priority_mode:Priority_order
        ~read_latency:1
        scope
        { Memory_controller.I.clock = clocking.clock
        ; clear = clocking.clear
        ; read_to_controller = [ Read_bus.Source.Of_signal.zero () ]
        ; write_to_controller = [ dma.out ]
        }
    in
    Always.compile
      [ Write_bus.Dest.Of_always.assign
          dma_to_memory_controller
          (List.nth_exn controller.write_to_controller 0)
      ; Write_response.With_valid.Of_always.assign
          memory_controller_to_dma
          (List.nth_exn controller.write_response 0)
      ];
    (* We echo the read and write responses to avoid dead code elimination deleting the
       entire BRAM *)
    { O.write_response = List.nth_exn controller.write_response 0
    ; read_response = List.nth_exn controller.read_response 0
    }
  ;;
end

module Harness = Cyclesim_harness.Make (Machine.I) (Machine.O)

let test ~address ~packets =
  Harness.run
    ~waves_config:
      (if debug then Waves_config.to_home_subdirectory () else Waves_config.no_waves)
    ~trace:`All_named
    ~create:(Machine.create ~address)
    (fun ~inputs ~outputs sim ->
      ignore outputs;
      inputs.clocking.clear := vdd;
      Cyclesim.cycle sim;
      inputs.clocking.clear := gnd;
      Cyclesim.cycle sim;
      List.iter packets ~f:(fun packet ->
        sim_preamble_sfd sim inputs;
        sim_packet sim ~packet inputs);
      Test_util.print_ram sim)
;;

let%expect_test "test" =
  test ~address:0 ~packets:[ "Hio" ];
  [%expect
    {|
    ("00000000  48 69 6f 00 00 00 00 00  00 00 00 00 00 00 00 00  |Hio.............|"
     "00000010  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "00000020  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "00000030  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "00000040  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "00000050  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "00000060  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "00000070  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|")
    |}];
  test ~address:32 ~packets:[ "Hello world!" ];
  [%expect
    {|
    ("00000000  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "00000010  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "00000020  48 65 6c 6c 6f 20 77 6f  72 6c 64 21 00 00 00 00  |Hello world!....|"
     "00000030  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "00000040  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "00000050  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "00000060  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "00000070  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|")
    |}];
  test
    ~address:64
    ~packets:
      [ "This is a long string that will overflow the max address"
      ; "New string that will overwrite the last string"
      ];
  [%expect
    {|
    ("00000000  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "00000010  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "00000020  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "00000030  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "00000040  4e 65 77 20 73 74 72 69  6e 67 20 74 68 61 74 20  |New string that |"
     "00000050  77 69 6c 6c 20 6f 76 65  72 77 72 69 74 65 20 74  |will overwrite t|"
     "00000060  68 65 20 6c 61 73 74 20  73 74 72 69 6e 67 00 00  |he last string..|"
     "00000070  20 61 64 64 72 65 73 73  00 00 00 00 00 00 00 00  | address........|")
    |}];
  test
    ~address:64
    ~packets:
      [ "This is a string that reaches the max address   "
      ; "New string that will overwrite the last string"
      ];
  [%expect
    {|
    ("00000000  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "00000010  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "00000020  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "00000030  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "00000040  4e 65 77 20 73 74 72 69  6e 67 20 74 68 61 74 20  |New string that |"
     "00000050  77 69 6c 6c 20 6f 76 65  72 77 72 69 74 65 20 74  |will overwrite t|"
     "00000060  68 65 20 6c 61 73 74 20  73 74 72 69 6e 67 00 00  |he last string..|"
     "00000070  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|")
    |}]
;;
