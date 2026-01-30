open Core
open Hardcaml
open Signal
open Hardcaml_memory_controller
open Hardcaml_hobby_boards_kernel
open Hardcaml_xilinx

module type Config = sig
  val input_width : int
  val input_height : int
  val output_width : int
  val output_height : int
  val start_address : int
  val vga_spec : Vga.Spec.t
  val vga_clock_div : int
end

module Make (Config : Config) (Memory : Memory_bus_intf.S) = struct
  open Config
  module Memory_to_axi32 = Memory_to_axi32.Make (Memory) (Ethernet.Axi32)
  module Var = Always.Variable

  module I = struct
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; memory_request : 'a Memory.Read_bus.Dest.t
      ; memory_response : 'a Memory.Read_response.With_valid.t
      }
    [@@deriving hardcaml]
  end

  module O = struct
    type 'a t =
      { red : 'a [@bits 4]
      ; green : 'a [@bits 4]
      ; blue : 'a [@bits 4]
      ; hsync : 'a
      ; vsync : 'a
      ; memory_request : 'a Memory.Read_bus.Source.t
      }
    [@@deriving hardcaml]
  end

  module Pixel = struct
    type 'a t =
      { red : 'a [@bits 4]
      ; green : 'a [@bits 4]
      ; blue : 'a [@bits 4]
      }
    [@@deriving hardcaml]
  end

  let () =
    if input_width > output_width || input_height > output_height
    then raise_s [%message "inputs must be smaller than outputs"]
  ;;

  let scaling_factor_x, scaling_factor_y =
    output_width / input_width, output_height / input_height
  ;;

  let num_pixels = input_height * input_width
  let pixel_counter_width = address_bits_for num_pixels
  let word_in_bytes = I.port_widths.memory_response.value.read_data / 8

  let create scope ~build_mode ({ clock; clear; memory_request; memory_response } : _ I.t)
    =
    let spec = Reg_spec.create ~clock ~clear () in
    let frame_buffer_data =
      Memory_to_axi32.hierarchical
        scope
        { Memory_to_axi32.I.clock
        ; clear
        ; read_input =
            { valid = vdd
            ; value =
                { length =
                    of_int_trunc ~width:16 (input_width * input_height * word_in_bytes)
                ; address =
                    of_int_trunc
                      ~width:Memory.Read_bus.Source.port_widths.data.address
                      start_address
                }
            }
        ; memory = memory_request
        ; memory_response
        ; output_packet = { tready = vdd }
        }
    in
    let%hw next_frame = wire 1 in
    let%hw next_pixel = wire 1 in
    let%hw frame_buffer_first =
      frame_buffer_data.output_packet.tvalid &: frame_buffer_data.output_packet.tuser.:(0)
    in
    let frame_buffer_pixel =
      Pixel.Of_signal.(
        reg spec (unpack (sel_bottom ~width:12 frame_buffer_data.output_packet.tdata)))
    in
    let%hw frame_buffer_tvalid = reg spec frame_buffer_data.output_packet.tvalid in
    let%hw frame_buffer_counter =
      reg_fb
        spec
        ~clear:(clear |: frame_buffer_first)
        ~width:pixel_counter_width
        ~f:(fun d ->
          mux2
            (d ==:. num_pixels)
            (zero pixel_counter_width)
            (d
             +: uextend ~width:pixel_counter_width frame_buffer_data.output_packet.tvalid
            ))
    in
    let%hw_var row_ctr =
      Var.reg ~clear:(clear |: next_frame) ~width:(address_bits_for input_width) spec
    in
    let%hw_var y_px_ctr =
      Var.reg ~clear:(clear |: next_frame) ~width:(address_bits_for scaling_factor_y) spec
    in
    let%hw_var x_px_ctr =
      Var.reg ~clear:(clear |: next_frame) ~width:(address_bits_for scaling_factor_x) spec
    in
    let read_address =
      Var.reg ~clear:(clear |: next_frame) ~width:pixel_counter_width spec
    in
    let row_start_address =
      Var.reg ~clear:(clear |: next_frame) ~width:pixel_counter_width spec
    in
    Always.(
      compile
        [ when_
            next_pixel
            [ incr x_px_ctr
            ; when_
                (x_px_ctr.value ==:. scaling_factor_x - 1)
                [ x_px_ctr <--. 0
                ; incr read_address
                ; incr row_ctr
                ; when_
                    (row_ctr.value ==:. input_width - 1)
                    [ incr y_px_ctr
                    ; row_ctr <--. 0
                    ; if_
                        (y_px_ctr.value ==:. scaling_factor_y - 1)
                        [ incr read_address
                        ; row_start_address <-- row_start_address.value +:. input_width
                        ; y_px_ctr <--. 0
                        ]
                        [ read_address <-- row_start_address.value ]
                    ]
                ]
            ]
        ]);
    let%hw red_data =
      Simple_dual_port_ram.create
        ~simulation_name:"vga_red_memory_bram"
        ~arch:(Blockram Read_before_write)
        ~address_collision_protection:Mux_output_ports
        ~address_collision_model:Lfsr
        ~size:num_pixels
        ~build_mode
        ~clock
        ~clear:(clear |: next_frame)
        ~write_enable:frame_buffer_tvalid
        ~write_address:frame_buffer_counter
        ~data:frame_buffer_pixel.red
        ~read_enable:next_pixel
        ~read_address:read_address.value
        ~read_latency:1
        ()
    in
    let%hw green_data =
      Simple_dual_port_ram.create
        ~simulation_name:"vga_green_memory_bram"
        ~arch:(Blockram Read_before_write)
        ~address_collision_protection:Mux_output_ports
        ~address_collision_model:Lfsr
        ~size:num_pixels
        ~build_mode
        ~clock
        ~clear:(clear |: next_frame)
        ~write_enable:frame_buffer_tvalid
        ~write_address:frame_buffer_counter
        ~data:frame_buffer_pixel.green
        ~read_enable:next_pixel
        ~read_address:read_address.value
        ~read_latency:1
        ()
    in
    let%hw blue_data =
      Simple_dual_port_ram.create
        ~simulation_name:"vga_blue_memory_bram"
        ~arch:(Blockram Read_before_write)
        ~address_collision_protection:Mux_output_ports
        ~address_collision_model:Lfsr
        ~size:num_pixels
        ~build_mode
        ~clock
        ~clear
        ~write_enable:frame_buffer_tvalid
        ~write_address:frame_buffer_counter
        ~data:frame_buffer_pixel.blue
        ~read_enable:next_pixel
        ~read_address:read_address.value
        ~read_latency:1
        ()
    in
    let%hw read_current_pixel = wire 1 in
    let%hw next_pixel_reg = reg spec next_pixel in
    let%hw.Fifo.Of_signal pixel_buffer =
      Fifo.create
        ~showahead:true
        ~clock
        ~clear:(clear |: next_frame)
        ~capacity:4
        ~wr:next_pixel_reg
        ~d:(blue_data @: green_data @: red_data)
        ~rd:read_current_pixel
        ()
    in
    let pixel = Pixel.Of_signal.unpack pixel_buffer.q in
    let%hw enable =
      if vga_clock_div = 1 then vdd else reg_fb spec ~width:1 ~f:(fun d -> d +:. 1) ==:. 0
    in
    let%hw.Vga.Scan.O.Of_signal hscan =
      Vga.Scan.create
        (Scope.sub_scope scope "hscan")
        spec
        ~enable
        vga_spec.horizontal_timing
    in
    let vscan =
      Vga.Scan.create
        (Scope.sub_scope scope "vscan")
        spec
        ~enable:(enable &: hscan.last &: hscan.is_front_porch)
        vga_spec.vertical_timing
    in
    let red =
      reg
        spec
        (mux2 (hscan.is_active &: vscan.is_active) pixel.red (of_unsigned_int ~width:4 0))
    in
    let green =
      reg
        spec
        (mux2
           (hscan.is_active &: vscan.is_active)
           pixel.green
           (of_unsigned_int ~width:4 0))
    in
    let blue =
      reg
        spec
        (mux2
           (hscan.is_active &: vscan.is_active)
           pixel.blue
           (of_unsigned_int ~width:4 0))
    in
    next_pixel
    <-- (vscan.last
         &: vscan.is_sync
         |: (vscan.last &: vscan.is_back_porch)
         |: (hscan.is_active &: vscan.is_active)
         &: enable);
    read_current_pixel <-- (hscan.is_active &: vscan.is_active &: enable);
    next_frame <-- (hscan.last &: vscan.last &: vscan.is_front_porch &: enable);
    O.
      { red
      ; green
      ; blue
      ; hsync = reg spec ~:(hscan.is_sync)
      ; vsync = reg spec ~:(vscan.is_sync)
      ; memory_request = frame_buffer_data.memory
      }
  ;;

  let hierarchical ~build_mode (scope : Scope.t) (input : Signal.t I.t) =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical ~scope ~name:"framebuffer_expander" (create ~build_mode) input
  ;;
end
