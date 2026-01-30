open! Core
open Hardcaml
open Hardcaml_test_harness
open Hardcaml_io_controller_test
open Hardcaml_memory_controller
open Hardcaml_hobby_boards_hardcaml_risc_v
open! Bits

let debug = false

module Packet = struct
  type t =
    { contents : String.t
    ; dma_address : int
    ; dma_length : int
    }
end

module Memory_controller = Bram_memory_controller.Make (struct
    let capacity_in_bytes = 256
    let num_write_channels = 1
    let num_read_channels = 1
    let address_width = 32
    let data_bus_width = 32
  end)

open Memory_controller.Memory_bus
module Memory_to_axi32 = Memory_to_axi32.Make (Memory_controller.Memory_bus) (Axi32)

module Machine = struct
  module I = struct
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; enable : 'a
      ; address : 'a [@bits Memory_controller.Memory_bus.address_width]
      ; length : 'a [@bits 16]
      }
    [@@deriving hardcaml]
  end

  module O = Memory_to_axi32.O

  let create (scope : Scope.t) { I.clock; clear; enable; address; length } =
    let ch_to_controller = Read_bus.Source.Of_always.wire Signal.zero in
    let controller =
      Memory_controller.hierarchical
        ~build_mode:Simulation
        ~read_latency:1
        ~priority_mode:Priority_order
        scope
        { Memory_controller.I.clock
        ; clear
        ; read_to_controller = [ Read_bus.Source.Of_always.value ch_to_controller ]
        ; write_to_controller = [ Write_bus.Source.Of_signal.zero () ]
        }
    in
    let output =
      Memory_to_axi32.hierarchical
        scope
        { Memory_to_axi32.I.clock
        ; clear
        ; read_input = { valid = enable; value = { address; length } }
        ; output_packet = { tready = Signal.vdd }
        ; memory = List.nth_exn controller.read_to_controller 0
        ; memory_response = List.nth_exn controller.read_response 0
        }
    in
    Always.compile [ Read_bus.Source.Of_always.assign ch_to_controller output.memory ];
    output
  ;;
end

module Harness = Cyclesim_harness.Make (Machine.I) (Machine.O)

let test ~packets =
  Harness.run
    ~create:Machine.create
    ~trace:`All_named
    ~waves_config:
      (if debug then Waves_config.to_home_subdirectory () else Waves_config.no_waves)
    (fun ~inputs ~outputs sim ->
      List.iter
        ~f:(fun { Packet.contents = load_memory; dma_address; dma_length } ->
          Test_util.write_packet_to_memory ~packet:load_memory sim;
          let data = ref "" in
          let store_outputs () =
            if to_bool !(outputs.output_packet.tvalid)
            then
              List.iter2_exn
                (split_msb ~exact:true ~part_width:8 !(outputs.output_packet.tdata)
                 |> List.rev)
                (bits_lsb !(outputs.output_packet.tkeep))
                ~f:(fun byte valid ->
                  if to_bool valid
                  then data := String.concat [ !data; to_char byte |> Char.to_string ]
                  else ())
            else ()
          in
          let issue_read ~address ~length =
            inputs.enable := vdd;
            inputs.address := of_unsigned_int ~width:32 address;
            inputs.length := of_unsigned_int ~width:16 length;
            Cyclesim.cycle sim;
            store_outputs ();
            inputs.enable := gnd;
            let count = ref 0 in
            while !count <> 1000 && not (to_bool !(outputs.output_packet.tlast)) do
              Cyclesim.cycle sim;
              store_outputs ();
              Int.incr count
            done;
            print_s [%message "" ~_:(!data : String.Hexdump.t)];
            printf "Cycles: %i\n" !count
          in
          inputs.clear := vdd;
          Cyclesim.cycle sim;
          inputs.clear := gnd;
          issue_read ~address:dma_address ~length:dma_length)
        packets)
;;

let%expect_test "test" =
  let test_str = "The quick brown fox jumps over the lazy dog" in
  test
    ~packets:
      [ { Packet.contents = test_str; dma_address = 3; dma_length = 9 }
      ; { Packet.contents = test_str; dma_address = 4; dma_length = 10 }
      ; { Packet.contents = test_str
        ; dma_address = 0
        ; dma_length = String.length test_str
        }
      ; { Packet.contents = test_str; dma_address = 12; dma_length = 1 }
      ; { Packet.contents = test_str; dma_address = 8; dma_length = 12 }
      ];
  [%expect
    {|
    ()
    Cycles: 1000
    ("00000000  71 75 69 63 6b 20 62 72  6f 77                    |quick brow|")
    Cycles: 8
    ("00000000  54 68 65 20 71 75 69 63  6b 20 62 72 6f 77 6e 20  |The quick brown |"
     "00000010  66 6f 78 20 6a 75 6d 70  73 20 6f 76 65 72 20 74  |fox jumps over t|"
     "00000020  68 65 20 6c 61 7a 79 20  64 6f 67                 |he lazy dog|")
    Cycles: 32
    ("00000000  6f                                                |o|")
    Cycles: 2
    ("00000000  6b 20 62 72 6f 77 6e 20  66 6f 78 20              |k brown fox |")
    Cycles: 8
    |}]
;;
