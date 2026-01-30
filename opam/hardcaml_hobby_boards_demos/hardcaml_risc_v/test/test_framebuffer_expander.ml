open! Core
open Hardcaml
open Hardcaml_memory_controller
open Hardcaml_hobby_boards_kernel
open Hardcaml_hobby_boards_hardcaml_risc_v
open Hardcaml_test_harness
open! Bits

module Memory_controller = Bram_memory_controller.Make (struct
    let capacity_in_bytes = 256
    let num_read_channels = 1
    let num_write_channels = 1
    let address_width = 32
    let data_bus_width = 32
  end)

module Machine = struct
  module Make (Framebuffer_config : Framebuffer_expander.Config) = struct
    open Memory_controller.Memory_bus

    module Framebuffer_expander =
      Framebuffer_expander.Make (Framebuffer_config) (Memory_controller.Memory_bus)

    module I = struct
      type 'a t =
        { clock : 'a
        ; clear : 'a
        }
      [@@deriving hardcaml]
    end

    module O = struct
      type 'a t =
        { red : 'a [@bits 4]
        ; green : 'a [@bits 4]
        ; blue : 'a [@bits 4]
        }
      [@@deriving hardcaml]
    end

    let create (scope : Scope.t) { I.clock; clear } =
      let request_ack = Read_bus.Dest.Of_signal.wires () in
      let response = Read_response.With_valid.Of_signal.wires () in
      let frame =
        Framebuffer_expander.hierarchical
          ~build_mode:Simulation
          scope
          { Framebuffer_expander.I.clock
          ; clear
          ; memory_request = request_ack
          ; memory_response = response
          }
      in
      let controller =
        Memory_controller.hierarchical
          ~build_mode:Simulation
          ~priority_mode:Priority_order
          ~read_latency:1
          scope
          { Memory_controller.I.clock
          ; clear
          ; read_to_controller = [ frame.memory_request ]
          ; write_to_controller = [ Write_bus.Source.Of_signal.zero () ]
          }
      in
      Read_bus.Dest.Of_signal.(request_ack <-- List.hd_exn controller.read_to_controller);
      Read_response.With_valid.Of_signal.(
        response <-- List.hd_exn controller.read_response);
      { O.red = frame.red; green = frame.green; blue = frame.blue }
    ;;
  end
end

(*=RED        GREEN       BLUE
| A | B |   | 1 | 2 |   | 0 | 1 |
| C | D |   | 3 | 4 |   | 1 | 0 |
| E | F |   | 5 | 6 |   | 0 | 1 |
*)

let write_frame_buffer_data_to_memory sim =
  let ram = Cyclesim.lookup_mem_by_name sim "main_memory_bram" |> Option.value_exn in
  let data_bits_list =
    [ of_string "0000_0000_0000_0000_0000_0000_0001_1010"
    ; of_string "0000_0000_0000_0000_0000_0001_0010_1011"
    ; of_string "0000_0000_0000_0000_0000_0001_0011_1100"
    ; of_string "0000_0000_0000_0000_0000_0000_0100_1101"
    ; of_string "0000_0000_0000_0000_0000_0000_0101_1110"
    ; of_string "0000_0000_0000_0000_0000_0001_0110_1111"
    ]
  in
  for ram_address = 0 to 5 do
    let start = ram_address * 4 in
    let data_bits =
      List.nth_exn data_bits_list ram_address |> split_lsb ~exact:true ~part_width:8
    in
    for i = 0 to 3 do
      Cyclesim.Memory.of_bits ~address:(start + i) ram (List.nth_exn data_bits i)
    done
  done
;;

let debug = false

module Make (Framebuffer_config : Framebuffer_expander.Config) = struct
  let test ~num_framebuffers =
    let module Machine = Machine.Make (Framebuffer_config) in
    let module Harness = Cyclesim_harness.Make (Machine.I) (Machine.O) in
    Harness.run
      ~trace:`All_named (* Needed so the BRAM doesn't get optimized away. *)
      ~create:Machine.create
      ~waves_config:
        (if debug then Waves_config.to_home_subdirectory () else Waves_config.no_waves)
      (fun ~inputs ~outputs sim ->
        inputs.clear := vdd;
        Cyclesim.cycle sim;
        inputs.clear := gnd;
        for i = 0 to num_framebuffers - 1 do
          write_frame_buffer_data_to_memory sim;
          let count = ref 0 in
          let get_framebuffer_data () =
            while
              to_bool
                (no_bits_set (!(outputs.red) @: !(outputs.green) @: !(outputs.blue)))
              && !count < 10000
            do
              Cyclesim.cycle sim;
              Int.incr count
            done;
            let result = [ !(outputs.red); !(outputs.green); !(outputs.blue) ] in
            Cyclesim.cycle sim;
            result
          in
          let frame_buffer =
            Array.init
              ~f:(fun _ -> get_framebuffer_data ())
              (Framebuffer_config.output_width * Framebuffer_config.output_height)
          in
          printf "Video Frame %i\n" i;
          Sequence.range 0 Framebuffer_config.output_height
          |> Sequence.iter ~f:(fun y ->
            Sequence.range 0 Framebuffer_config.output_width
            |> Sequence.iter ~f:(fun x ->
              let pixel =
                Array.get frame_buffer ((y * Framebuffer_config.output_width) + x)
              in
              printf "(%x, " (to_int_trunc (List.nth_exn pixel 0));
              printf "%x, " (to_int_trunc (List.nth_exn pixel 1));
              printf "%x)" (to_int_trunc (List.nth_exn pixel 2)));
            printf "\n")
        done)
  ;;
end

let%expect_test "test" =
  let module Test =
    Make (struct
      let input_width = 2
      let input_height = 3
      let output_width = 4
      let output_height = 6
      let start_address = 0
      let vga_spec = Vga.Spec.testing
      let vga_clock_div = 1
    end)
  in
  Test.test ~num_framebuffers:3;
  [%expect
    {|
    Video Frame 0
    (a, 1, 0)(a, 1, 0)(b, 2, 1)(b, 2, 1)
    (a, 1, 0)(a, 1, 0)(b, 2, 1)(b, 2, 1)
    (c, 3, 1)(c, 3, 1)(d, 4, 0)(d, 4, 0)
    (c, 3, 1)(c, 3, 1)(d, 4, 0)(d, 4, 0)
    (e, 5, 0)(e, 5, 0)(f, 6, 1)(f, 6, 1)
    (e, 5, 0)(e, 5, 0)(f, 6, 1)(f, 6, 1)
    Video Frame 1
    (a, 1, 0)(a, 1, 0)(b, 2, 1)(b, 2, 1)
    (a, 1, 0)(a, 1, 0)(b, 2, 1)(b, 2, 1)
    (c, 3, 1)(c, 3, 1)(d, 4, 0)(d, 4, 0)
    (c, 3, 1)(c, 3, 1)(d, 4, 0)(d, 4, 0)
    (e, 5, 0)(e, 5, 0)(f, 6, 1)(f, 6, 1)
    (e, 5, 0)(e, 5, 0)(f, 6, 1)(f, 6, 1)
    Video Frame 2
    (a, 1, 0)(a, 1, 0)(b, 2, 1)(b, 2, 1)
    (a, 1, 0)(a, 1, 0)(b, 2, 1)(b, 2, 1)
    (c, 3, 1)(c, 3, 1)(d, 4, 0)(d, 4, 0)
    (c, 3, 1)(c, 3, 1)(d, 4, 0)(d, 4, 0)
    (e, 5, 0)(e, 5, 0)(f, 6, 1)(f, 6, 1)
    (e, 5, 0)(e, 5, 0)(f, 6, 1)(f, 6, 1)
    |}]
;;

let%expect_test "test_no_scaling" =
  let module Test =
    Make (struct
      let input_width = 2
      let input_height = 3
      let output_width = 2
      let output_height = 3
      let start_address = 0
      let vga_spec = Vga.Spec.testing_no_scaling
      let vga_clock_div = 1
    end)
  in
  Test.test ~num_framebuffers:3;
  [%expect
    {|
    Video Frame 0
    (a, 1, 0)(b, 2, 1)
    (c, 3, 1)(d, 4, 0)
    (e, 5, 0)(f, 6, 1)
    Video Frame 1
    (a, 1, 0)(b, 2, 1)
    (c, 3, 1)(d, 4, 0)
    (e, 5, 0)(f, 6, 1)
    Video Frame 2
    (a, 1, 0)(b, 2, 1)
    (c, 3, 1)(d, 4, 0)
    (e, 5, 0)(f, 6, 1)
    |}]
;;

let%expect_test "test_scale_4x4y" =
  let module Test =
    Make (struct
      let input_width = 2
      let input_height = 3
      let output_width = 8
      let output_height = 12
      let start_address = 0
      let vga_spec = Vga.Spec.testing_4x4y
      let vga_clock_div = 1
    end)
  in
  Test.test ~num_framebuffers:3;
  [%expect
    {|
    Video Frame 0
    (a, 1, 0)(a, 1, 0)(a, 1, 0)(a, 1, 0)(b, 2, 1)(b, 2, 1)(b, 2, 1)(b, 2, 1)
    (a, 1, 0)(a, 1, 0)(a, 1, 0)(a, 1, 0)(b, 2, 1)(b, 2, 1)(b, 2, 1)(b, 2, 1)
    (a, 1, 0)(a, 1, 0)(a, 1, 0)(a, 1, 0)(b, 2, 1)(b, 2, 1)(b, 2, 1)(b, 2, 1)
    (a, 1, 0)(a, 1, 0)(a, 1, 0)(a, 1, 0)(b, 2, 1)(b, 2, 1)(b, 2, 1)(b, 2, 1)
    (c, 3, 1)(c, 3, 1)(c, 3, 1)(c, 3, 1)(d, 4, 0)(d, 4, 0)(d, 4, 0)(d, 4, 0)
    (c, 3, 1)(c, 3, 1)(c, 3, 1)(c, 3, 1)(d, 4, 0)(d, 4, 0)(d, 4, 0)(d, 4, 0)
    (c, 3, 1)(c, 3, 1)(c, 3, 1)(c, 3, 1)(d, 4, 0)(d, 4, 0)(d, 4, 0)(d, 4, 0)
    (c, 3, 1)(c, 3, 1)(c, 3, 1)(c, 3, 1)(d, 4, 0)(d, 4, 0)(d, 4, 0)(d, 4, 0)
    (e, 5, 0)(e, 5, 0)(e, 5, 0)(e, 5, 0)(f, 6, 1)(f, 6, 1)(f, 6, 1)(f, 6, 1)
    (e, 5, 0)(e, 5, 0)(e, 5, 0)(e, 5, 0)(f, 6, 1)(f, 6, 1)(f, 6, 1)(f, 6, 1)
    (e, 5, 0)(e, 5, 0)(e, 5, 0)(e, 5, 0)(f, 6, 1)(f, 6, 1)(f, 6, 1)(f, 6, 1)
    (e, 5, 0)(e, 5, 0)(e, 5, 0)(e, 5, 0)(f, 6, 1)(f, 6, 1)(f, 6, 1)(f, 6, 1)
    Video Frame 1
    (a, 1, 0)(a, 1, 0)(a, 1, 0)(a, 1, 0)(b, 2, 1)(b, 2, 1)(b, 2, 1)(b, 2, 1)
    (a, 1, 0)(a, 1, 0)(a, 1, 0)(a, 1, 0)(b, 2, 1)(b, 2, 1)(b, 2, 1)(b, 2, 1)
    (a, 1, 0)(a, 1, 0)(a, 1, 0)(a, 1, 0)(b, 2, 1)(b, 2, 1)(b, 2, 1)(b, 2, 1)
    (a, 1, 0)(a, 1, 0)(a, 1, 0)(a, 1, 0)(b, 2, 1)(b, 2, 1)(b, 2, 1)(b, 2, 1)
    (c, 3, 1)(c, 3, 1)(c, 3, 1)(c, 3, 1)(d, 4, 0)(d, 4, 0)(d, 4, 0)(d, 4, 0)
    (c, 3, 1)(c, 3, 1)(c, 3, 1)(c, 3, 1)(d, 4, 0)(d, 4, 0)(d, 4, 0)(d, 4, 0)
    (c, 3, 1)(c, 3, 1)(c, 3, 1)(c, 3, 1)(d, 4, 0)(d, 4, 0)(d, 4, 0)(d, 4, 0)
    (c, 3, 1)(c, 3, 1)(c, 3, 1)(c, 3, 1)(d, 4, 0)(d, 4, 0)(d, 4, 0)(d, 4, 0)
    (e, 5, 0)(e, 5, 0)(e, 5, 0)(e, 5, 0)(f, 6, 1)(f, 6, 1)(f, 6, 1)(f, 6, 1)
    (e, 5, 0)(e, 5, 0)(e, 5, 0)(e, 5, 0)(f, 6, 1)(f, 6, 1)(f, 6, 1)(f, 6, 1)
    (e, 5, 0)(e, 5, 0)(e, 5, 0)(e, 5, 0)(f, 6, 1)(f, 6, 1)(f, 6, 1)(f, 6, 1)
    (e, 5, 0)(e, 5, 0)(e, 5, 0)(e, 5, 0)(f, 6, 1)(f, 6, 1)(f, 6, 1)(f, 6, 1)
    Video Frame 2
    (a, 1, 0)(a, 1, 0)(a, 1, 0)(a, 1, 0)(b, 2, 1)(b, 2, 1)(b, 2, 1)(b, 2, 1)
    (a, 1, 0)(a, 1, 0)(a, 1, 0)(a, 1, 0)(b, 2, 1)(b, 2, 1)(b, 2, 1)(b, 2, 1)
    (a, 1, 0)(a, 1, 0)(a, 1, 0)(a, 1, 0)(b, 2, 1)(b, 2, 1)(b, 2, 1)(b, 2, 1)
    (a, 1, 0)(a, 1, 0)(a, 1, 0)(a, 1, 0)(b, 2, 1)(b, 2, 1)(b, 2, 1)(b, 2, 1)
    (c, 3, 1)(c, 3, 1)(c, 3, 1)(c, 3, 1)(d, 4, 0)(d, 4, 0)(d, 4, 0)(d, 4, 0)
    (c, 3, 1)(c, 3, 1)(c, 3, 1)(c, 3, 1)(d, 4, 0)(d, 4, 0)(d, 4, 0)(d, 4, 0)
    (c, 3, 1)(c, 3, 1)(c, 3, 1)(c, 3, 1)(d, 4, 0)(d, 4, 0)(d, 4, 0)(d, 4, 0)
    (c, 3, 1)(c, 3, 1)(c, 3, 1)(c, 3, 1)(d, 4, 0)(d, 4, 0)(d, 4, 0)(d, 4, 0)
    (e, 5, 0)(e, 5, 0)(e, 5, 0)(e, 5, 0)(f, 6, 1)(f, 6, 1)(f, 6, 1)(f, 6, 1)
    (e, 5, 0)(e, 5, 0)(e, 5, 0)(e, 5, 0)(f, 6, 1)(f, 6, 1)(f, 6, 1)(f, 6, 1)
    (e, 5, 0)(e, 5, 0)(e, 5, 0)(e, 5, 0)(f, 6, 1)(f, 6, 1)(f, 6, 1)(f, 6, 1)
    (e, 5, 0)(e, 5, 0)(e, 5, 0)(e, 5, 0)(f, 6, 1)(f, 6, 1)(f, 6, 1)(f, 6, 1)
    |}]
;;

let%expect_test "test_scale_2x4y" =
  let module Test =
    Make (struct
      let input_width = 2
      let input_height = 3
      let output_width = 4
      let output_height = 12
      let start_address = 0
      let vga_spec = Vga.Spec.testing_2x4y
      let vga_clock_div = 1
    end)
  in
  Test.test ~num_framebuffers:3;
  [%expect
    {|
    Video Frame 0
    (a, 1, 0)(a, 1, 0)(b, 2, 1)(b, 2, 1)
    (a, 1, 0)(a, 1, 0)(b, 2, 1)(b, 2, 1)
    (a, 1, 0)(a, 1, 0)(b, 2, 1)(b, 2, 1)
    (a, 1, 0)(a, 1, 0)(b, 2, 1)(b, 2, 1)
    (c, 3, 1)(c, 3, 1)(d, 4, 0)(d, 4, 0)
    (c, 3, 1)(c, 3, 1)(d, 4, 0)(d, 4, 0)
    (c, 3, 1)(c, 3, 1)(d, 4, 0)(d, 4, 0)
    (c, 3, 1)(c, 3, 1)(d, 4, 0)(d, 4, 0)
    (e, 5, 0)(e, 5, 0)(f, 6, 1)(f, 6, 1)
    (e, 5, 0)(e, 5, 0)(f, 6, 1)(f, 6, 1)
    (e, 5, 0)(e, 5, 0)(f, 6, 1)(f, 6, 1)
    (e, 5, 0)(e, 5, 0)(f, 6, 1)(f, 6, 1)
    Video Frame 1
    (a, 1, 0)(a, 1, 0)(b, 2, 1)(b, 2, 1)
    (a, 1, 0)(a, 1, 0)(b, 2, 1)(b, 2, 1)
    (a, 1, 0)(a, 1, 0)(b, 2, 1)(b, 2, 1)
    (a, 1, 0)(a, 1, 0)(b, 2, 1)(b, 2, 1)
    (c, 3, 1)(c, 3, 1)(d, 4, 0)(d, 4, 0)
    (c, 3, 1)(c, 3, 1)(d, 4, 0)(d, 4, 0)
    (c, 3, 1)(c, 3, 1)(d, 4, 0)(d, 4, 0)
    (c, 3, 1)(c, 3, 1)(d, 4, 0)(d, 4, 0)
    (e, 5, 0)(e, 5, 0)(f, 6, 1)(f, 6, 1)
    (e, 5, 0)(e, 5, 0)(f, 6, 1)(f, 6, 1)
    (e, 5, 0)(e, 5, 0)(f, 6, 1)(f, 6, 1)
    (e, 5, 0)(e, 5, 0)(f, 6, 1)(f, 6, 1)
    Video Frame 2
    (a, 1, 0)(a, 1, 0)(b, 2, 1)(b, 2, 1)
    (a, 1, 0)(a, 1, 0)(b, 2, 1)(b, 2, 1)
    (a, 1, 0)(a, 1, 0)(b, 2, 1)(b, 2, 1)
    (a, 1, 0)(a, 1, 0)(b, 2, 1)(b, 2, 1)
    (c, 3, 1)(c, 3, 1)(d, 4, 0)(d, 4, 0)
    (c, 3, 1)(c, 3, 1)(d, 4, 0)(d, 4, 0)
    (c, 3, 1)(c, 3, 1)(d, 4, 0)(d, 4, 0)
    (c, 3, 1)(c, 3, 1)(d, 4, 0)(d, 4, 0)
    (e, 5, 0)(e, 5, 0)(f, 6, 1)(f, 6, 1)
    (e, 5, 0)(e, 5, 0)(f, 6, 1)(f, 6, 1)
    (e, 5, 0)(e, 5, 0)(f, 6, 1)(f, 6, 1)
    (e, 5, 0)(e, 5, 0)(f, 6, 1)(f, 6, 1)
    |}]
;;

let%expect_test "test_scale_4x2y" =
  let module Test =
    Make (struct
      let input_width = 2
      let input_height = 3
      let output_width = 8
      let output_height = 6
      let start_address = 0
      let vga_spec = Vga.Spec.testing_4x2y
      let vga_clock_div = 1
    end)
  in
  Test.test ~num_framebuffers:3;
  [%expect
    {|
    Video Frame 0
    (a, 1, 0)(a, 1, 0)(a, 1, 0)(a, 1, 0)(b, 2, 1)(b, 2, 1)(b, 2, 1)(b, 2, 1)
    (a, 1, 0)(a, 1, 0)(a, 1, 0)(a, 1, 0)(b, 2, 1)(b, 2, 1)(b, 2, 1)(b, 2, 1)
    (c, 3, 1)(c, 3, 1)(c, 3, 1)(c, 3, 1)(d, 4, 0)(d, 4, 0)(d, 4, 0)(d, 4, 0)
    (c, 3, 1)(c, 3, 1)(c, 3, 1)(c, 3, 1)(d, 4, 0)(d, 4, 0)(d, 4, 0)(d, 4, 0)
    (e, 5, 0)(e, 5, 0)(e, 5, 0)(e, 5, 0)(f, 6, 1)(f, 6, 1)(f, 6, 1)(f, 6, 1)
    (e, 5, 0)(e, 5, 0)(e, 5, 0)(e, 5, 0)(f, 6, 1)(f, 6, 1)(f, 6, 1)(f, 6, 1)
    Video Frame 1
    (a, 1, 0)(a, 1, 0)(a, 1, 0)(a, 1, 0)(b, 2, 1)(b, 2, 1)(b, 2, 1)(b, 2, 1)
    (a, 1, 0)(a, 1, 0)(a, 1, 0)(a, 1, 0)(b, 2, 1)(b, 2, 1)(b, 2, 1)(b, 2, 1)
    (c, 3, 1)(c, 3, 1)(c, 3, 1)(c, 3, 1)(d, 4, 0)(d, 4, 0)(d, 4, 0)(d, 4, 0)
    (c, 3, 1)(c, 3, 1)(c, 3, 1)(c, 3, 1)(d, 4, 0)(d, 4, 0)(d, 4, 0)(d, 4, 0)
    (e, 5, 0)(e, 5, 0)(e, 5, 0)(e, 5, 0)(f, 6, 1)(f, 6, 1)(f, 6, 1)(f, 6, 1)
    (e, 5, 0)(e, 5, 0)(e, 5, 0)(e, 5, 0)(f, 6, 1)(f, 6, 1)(f, 6, 1)(f, 6, 1)
    Video Frame 2
    (a, 1, 0)(a, 1, 0)(a, 1, 0)(a, 1, 0)(b, 2, 1)(b, 2, 1)(b, 2, 1)(b, 2, 1)
    (a, 1, 0)(a, 1, 0)(a, 1, 0)(a, 1, 0)(b, 2, 1)(b, 2, 1)(b, 2, 1)(b, 2, 1)
    (c, 3, 1)(c, 3, 1)(c, 3, 1)(c, 3, 1)(d, 4, 0)(d, 4, 0)(d, 4, 0)(d, 4, 0)
    (c, 3, 1)(c, 3, 1)(c, 3, 1)(c, 3, 1)(d, 4, 0)(d, 4, 0)(d, 4, 0)(d, 4, 0)
    (e, 5, 0)(e, 5, 0)(e, 5, 0)(e, 5, 0)(f, 6, 1)(f, 6, 1)(f, 6, 1)(f, 6, 1)
    (e, 5, 0)(e, 5, 0)(e, 5, 0)(e, 5, 0)(f, 6, 1)(f, 6, 1)(f, 6, 1)(f, 6, 1)
    |}]
;;

let%expect_test "test_scale_3x5y" =
  let module Test =
    Make (struct
      let input_width = 2
      let input_height = 3
      let output_width = 6
      let output_height = 15
      let start_address = 0
      let vga_spec = Vga.Spec.testing_3x5y
      let vga_clock_div = 1
    end)
  in
  Test.test ~num_framebuffers:3;
  [%expect
    {|
    Video Frame 0
    (a, 1, 0)(a, 1, 0)(a, 1, 0)(b, 2, 1)(b, 2, 1)(b, 2, 1)
    (a, 1, 0)(a, 1, 0)(a, 1, 0)(b, 2, 1)(b, 2, 1)(b, 2, 1)
    (a, 1, 0)(a, 1, 0)(a, 1, 0)(b, 2, 1)(b, 2, 1)(b, 2, 1)
    (a, 1, 0)(a, 1, 0)(a, 1, 0)(b, 2, 1)(b, 2, 1)(b, 2, 1)
    (a, 1, 0)(a, 1, 0)(a, 1, 0)(b, 2, 1)(b, 2, 1)(b, 2, 1)
    (c, 3, 1)(c, 3, 1)(c, 3, 1)(d, 4, 0)(d, 4, 0)(d, 4, 0)
    (c, 3, 1)(c, 3, 1)(c, 3, 1)(d, 4, 0)(d, 4, 0)(d, 4, 0)
    (c, 3, 1)(c, 3, 1)(c, 3, 1)(d, 4, 0)(d, 4, 0)(d, 4, 0)
    (c, 3, 1)(c, 3, 1)(c, 3, 1)(d, 4, 0)(d, 4, 0)(d, 4, 0)
    (c, 3, 1)(c, 3, 1)(c, 3, 1)(d, 4, 0)(d, 4, 0)(d, 4, 0)
    (e, 5, 0)(e, 5, 0)(e, 5, 0)(f, 6, 1)(f, 6, 1)(f, 6, 1)
    (e, 5, 0)(e, 5, 0)(e, 5, 0)(f, 6, 1)(f, 6, 1)(f, 6, 1)
    (e, 5, 0)(e, 5, 0)(e, 5, 0)(f, 6, 1)(f, 6, 1)(f, 6, 1)
    (e, 5, 0)(e, 5, 0)(e, 5, 0)(f, 6, 1)(f, 6, 1)(f, 6, 1)
    (e, 5, 0)(e, 5, 0)(e, 5, 0)(f, 6, 1)(f, 6, 1)(f, 6, 1)
    Video Frame 1
    (a, 1, 0)(a, 1, 0)(a, 1, 0)(b, 2, 1)(b, 2, 1)(b, 2, 1)
    (a, 1, 0)(a, 1, 0)(a, 1, 0)(b, 2, 1)(b, 2, 1)(b, 2, 1)
    (a, 1, 0)(a, 1, 0)(a, 1, 0)(b, 2, 1)(b, 2, 1)(b, 2, 1)
    (a, 1, 0)(a, 1, 0)(a, 1, 0)(b, 2, 1)(b, 2, 1)(b, 2, 1)
    (a, 1, 0)(a, 1, 0)(a, 1, 0)(b, 2, 1)(b, 2, 1)(b, 2, 1)
    (c, 3, 1)(c, 3, 1)(c, 3, 1)(d, 4, 0)(d, 4, 0)(d, 4, 0)
    (c, 3, 1)(c, 3, 1)(c, 3, 1)(d, 4, 0)(d, 4, 0)(d, 4, 0)
    (c, 3, 1)(c, 3, 1)(c, 3, 1)(d, 4, 0)(d, 4, 0)(d, 4, 0)
    (c, 3, 1)(c, 3, 1)(c, 3, 1)(d, 4, 0)(d, 4, 0)(d, 4, 0)
    (c, 3, 1)(c, 3, 1)(c, 3, 1)(d, 4, 0)(d, 4, 0)(d, 4, 0)
    (e, 5, 0)(e, 5, 0)(e, 5, 0)(f, 6, 1)(f, 6, 1)(f, 6, 1)
    (e, 5, 0)(e, 5, 0)(e, 5, 0)(f, 6, 1)(f, 6, 1)(f, 6, 1)
    (e, 5, 0)(e, 5, 0)(e, 5, 0)(f, 6, 1)(f, 6, 1)(f, 6, 1)
    (e, 5, 0)(e, 5, 0)(e, 5, 0)(f, 6, 1)(f, 6, 1)(f, 6, 1)
    (e, 5, 0)(e, 5, 0)(e, 5, 0)(f, 6, 1)(f, 6, 1)(f, 6, 1)
    Video Frame 2
    (a, 1, 0)(a, 1, 0)(a, 1, 0)(b, 2, 1)(b, 2, 1)(b, 2, 1)
    (a, 1, 0)(a, 1, 0)(a, 1, 0)(b, 2, 1)(b, 2, 1)(b, 2, 1)
    (a, 1, 0)(a, 1, 0)(a, 1, 0)(b, 2, 1)(b, 2, 1)(b, 2, 1)
    (a, 1, 0)(a, 1, 0)(a, 1, 0)(b, 2, 1)(b, 2, 1)(b, 2, 1)
    (a, 1, 0)(a, 1, 0)(a, 1, 0)(b, 2, 1)(b, 2, 1)(b, 2, 1)
    (c, 3, 1)(c, 3, 1)(c, 3, 1)(d, 4, 0)(d, 4, 0)(d, 4, 0)
    (c, 3, 1)(c, 3, 1)(c, 3, 1)(d, 4, 0)(d, 4, 0)(d, 4, 0)
    (c, 3, 1)(c, 3, 1)(c, 3, 1)(d, 4, 0)(d, 4, 0)(d, 4, 0)
    (c, 3, 1)(c, 3, 1)(c, 3, 1)(d, 4, 0)(d, 4, 0)(d, 4, 0)
    (c, 3, 1)(c, 3, 1)(c, 3, 1)(d, 4, 0)(d, 4, 0)(d, 4, 0)
    (e, 5, 0)(e, 5, 0)(e, 5, 0)(f, 6, 1)(f, 6, 1)(f, 6, 1)
    (e, 5, 0)(e, 5, 0)(e, 5, 0)(f, 6, 1)(f, 6, 1)(f, 6, 1)
    (e, 5, 0)(e, 5, 0)(e, 5, 0)(f, 6, 1)(f, 6, 1)(f, 6, 1)
    (e, 5, 0)(e, 5, 0)(e, 5, 0)(f, 6, 1)(f, 6, 1)(f, 6, 1)
    (e, 5, 0)(e, 5, 0)(e, 5, 0)(f, 6, 1)(f, 6, 1)(f, 6, 1)
    |}]
;;
