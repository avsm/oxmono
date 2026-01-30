open! Core
open Hardcaml
open Hardcaml_risc_v_hart
open Hardcaml_waveterm
open Hardcaml_hobby_boards_kernel
open Hardcaml_hobby_boards_hardcaml_risc_v
open Hardcaml_risc_v_test
open Hardcaml_io_framework_test
open Ethernet_utils.For_testing
open! Bits

let debug = false

let uart_config =
  Uart.Config.
    { data_bits = Uart_types.Data_bits.Enum.of_enum (module Signal) Eight
    ; parity = Uart_types.Parity.Enum.of_enum (module Signal) None
    ; stop_bits = Uart_types.Stop_bits.Enum.of_enum (module Signal) One
    ; clocks_per_bit =
        Signal.of_unsigned_int ~width:port_widths.clocks_per_bit (100_000 / 50_000)
    }
;;

let frame_buffer_start_address = 64
let frame_buffer_input_width = 2
let frame_buffer_input_height = 3
let vga_spec = Vga.Spec.testing
let design_frequency = 100
let video_output_width = vga_spec.horizontal_timing.active
let video_output_height = vga_spec.vertical_timing.active
let frame_buffer_size = frame_buffer_input_width * frame_buffer_input_height
let ethernet_memory_address = 32
let pixel_width = 12

module Cpu_with_dma_memory =
  System_with_bram.Make
    (struct
      let register_width = Register_width.B32
      let num_registers = 32
      let design_frequency = design_frequency
    end)
    (struct
      let capacity_in_bytes = 256
      let ethernet_start_address = ethernet_memory_address
      let frame_buffer_bytes = frame_buffer_input_height * frame_buffer_input_width
    end)
    (struct
      let num_harts = 1
      let io_controller = uart_config
      let fpga_mac_address = Signal.of_string "48'h0018_3e04_c882"
    end)
    (struct
      let vga_spec = vga_spec
      let input_width = frame_buffer_input_width
      let input_height = frame_buffer_input_height
      let output_width = video_output_width
      let output_height = video_output_height
      let start_address = frame_buffer_start_address
      let vga_clock_div = 1
    end)

module With_transmitter = struct
  module I = struct
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; data_in_valid : 'a
      ; data_in : 'a [@bits 8]
      ; crsdv : 'a
      ; rxerr : 'a
      ; rxd : 'a [@bits 2]
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O = struct
    type 'a t =
      { ethernet_txen : 'a
      ; ethernet_txd : 'a [@bits 2]
      ; uart_data_out_valid : 'a
      ; uart_data_out : 'a [@bits 8]
      ; vga_red : 'a [@bits 4]
      ; vga_green : 'a [@bits 4]
      ; vga_blue : 'a [@bits 4]
      ; registers : 'a Cpu_with_dma_memory.Registers.t list [@length 1]
      }
    [@@deriving sexp_of, hardcaml]
  end

  let create scope { I.clock; clear; data_in_valid; data_in; crsdv; rxerr; rxd } =
    let { Uart.Tx.O.txd; _ } =
      Uart.Tx.hierarchical
        scope
        { Uart.Tx.I.clocking = { clock; clear }
        ; config = uart_config
        ; data_in_valid
        ; data_in = Signal.uextend ~width:9 data_in
        }
    in
    let { Cpu_with_dma_memory.O.registers
        ; ethernet_txen
        ; ethernet_txd
        ; uart_tx = cpu_uart_tx
        ; vga_red
        ; vga_green
        ; vga_blue
        ; _
        }
      =
      Cpu_with_dma_memory.hierarchical
        ~read_latency:1
        ~build_mode:Simulation
        scope
        { clock
        ; clear
        ; uart_rx = txd
        ; eth_crsdv = crsdv
        ; eth_rxerr = rxerr
        ; eth_rxd = rxd
        }
    in
    let { Uart.Rx.O.data_out_valid; data_out; _ } =
      Uart.Rx.hierarchical
        ~align_rxdata_to_lsb:true
        scope
        { Uart.Rx.I.clocking = { clock; clear }
        ; enable = Signal.vdd
        ; config = uart_config
        ; rxd = cpu_uart_tx
        }
    in
    { O.registers
    ; ethernet_txen
    ; ethernet_txd
    ; uart_data_out_valid = data_out_valid
    ; uart_data_out = Signal.sel_bottom ~width:8 data_out
    ; vga_red
    ; vga_green
    ; vga_blue
    }
  ;;
end

module Sim = Cyclesim.With_interface (With_transmitter.I) (With_transmitter.O)

type sim =
  { sim : Sim.t
  ; waveform : Waveform.t
  ; name : string
  }

let create_sim name : sim =
  let sim =
    Sim.create
      ~config:Cyclesim.Config.trace_all
      (With_transmitter.create
         (Scope.create ~auto_label_hierarchical_ports:true ~flatten_design:true ()))
  in
  let waveform, sim = Waveform.create sim in
  { sim; waveform; name }
;;

let finalize_sim sim =
  if debug
  then
    Waveform.Serialize.marshall
      sim.waveform
      ("~/waves/dma_" ^ sim.name ^ ".hardcamlwaveform")
  else ()
;;

let clear_registers ~(inputs : t ref With_transmitter.I.t) sim =
  inputs.clear := one 1;
  Cyclesim.cycle sim;
  inputs.clear := zero 1
;;

let send_dma_message ~address ~packet (sim : Sim.t) =
  let inputs = Cyclesim.inputs sim in
  let whole_packet = Opcode_helper.dma_packet ~address packet in
  (* Send the DMA message through byte by byte. Uart_tx will transmit a byte once every
     ~10 cycles (this is dependent on the number of stop bits and the parity bit. *)
  let rec loop_for n =
    if n = 0
    then ()
    else (
      Cyclesim.cycle sim;
      loop_for (n - 1))
  in
  List.iter
    ~f:(fun input ->
      inputs.data_in_valid := vdd;
      inputs.data_in := of_int_trunc ~width:8 input;
      Cyclesim.cycle sim;
      inputs.data_in_valid := of_int_trunc ~width:1 0;
      loop_for 100)
    whole_packet
;;

let test ~num_cycles ~data { sim; waveform = _; name = _ } =
  let inputs = Cyclesim.inputs sim in
  (* Send a clear signal to initialize any CPU IO controller state back to default so
     we're ready to receive. *)
  clear_registers ~inputs sim;
  send_dma_message ~address:0 ~packet:data sim;
  for _ = 0 to 1000 do
    Cyclesim.cycle sim
  done;
  let outputs = Cyclesim.outputs sim in
  clear_registers ~inputs sim;
  let data = ref [] in
  let rec loop_for cycles =
    if cycles = 0
    then ()
    else (
      Cyclesim.cycle sim;
      if to_bool !(outputs.ethernet_txen) then data := !(outputs.ethernet_txd) :: !data;
      if to_bool !(outputs.uart_data_out_valid)
      then (
        let char = to_char !(outputs.uart_data_out) in
        printf "%c" (if Char.is_print char then char else '_'));
      loop_for (cycles - 1))
  in
  printf "RECEIVED FROM CPU VIA UART: ";
  loop_for num_cycles;
  printf "\n";
  printf "RECEIVED FROM CPU VIA ETHERNET: ";
  let data = !data in
  let packet_bits =
    String.to_list "Hello world!"
    |> List.map ~f:Char.to_int
    |> List.map ~f:(of_int_trunc ~width:8)
    |> concat_msb
  in
  Ethernet_utils.For_testing.check_tx_data
    ~output_data:data
    ~expected_data:packet_bits
    ~print_data_as_string:true
    ();
  match outputs.registers with
  | [ outputs ] ->
    let outputs =
      Cpu_with_dma_memory.Registers.map ~f:(fun t -> to_int_trunc !t) outputs
    in
    print_s [%message "" ~_:(outputs : int Cpu_with_dma_memory.Registers.t)];
    Test_util.print_ram sim
  | _ -> raise_s [%message "BUG: Unexpected number of harts"]
;;

let%expect_test "hello world via DMA round-trip" =
  let hello_world_program = Opcode_helper.hello_world_program in
  let sim = create_sim "test_dma_hello_world_ethernet" in
  test ~num_cycles:5000 ~data:hello_world_program sim;
  finalize_sim sim;
  [%expect
    {|
    RECEIVED FROM CPU VIA UART: D__Hello world!
    RECEIVED FROM CPU VIA ETHERNET: Hello world!
    ((pc 16)
     (general
      (0 0 0 0 0 1 16 12 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)))
    ("00000000  93 02 00 00 13 03 00 01  93 03 c0 00 73 00 00 00  |............s...|"
     "00000010  48 65 6c 6c 6f 20 77 6f  72 6c 64 21 00 00 00 00  |Hello world!....|"
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
    |}]
;;

let send_udp_packet ~packet sim =
  let inputs : _ With_transmitter.I.t = Cyclesim.inputs sim in
  let ethernet_rx_inputs =
    { Ethernet.Rx.I.clocking = { clock = inputs.clock; clear = inputs.clear }
    ; rxd = inputs.rxd
    ; crsdv = inputs.crsdv
    ; rxerr = inputs.rxerr
    }
  in
  sim_preamble_sfd sim ethernet_rx_inputs;
  sim_udp_packet sim ~packet ethernet_rx_inputs
;;

let read_memory_and_write_frame_buffer
  ~address
  ~frame_buffer_size
  ~frame_buffer_start_address
  ~sim
  =
  let ram = Cyclesim.lookup_mem_by_name sim "main_memory_bram" |> Option.value_exn in
  let memory_data = ram |> Cyclesim.Memory.read_all |> Array.to_list in
  let frame_buffer_bytes = frame_buffer_size * pixel_width / 8 in
  let memory_data_bits = 32 in
  let memory_data_bytes = memory_data_bits / 8 in
  let data_bits_list =
    List.slice memory_data address (address + frame_buffer_bytes)
    |> Bits.concat_msb
    |> Bits.split_msb ~exact:true ~part_width:pixel_width
    |> List.map ~f:(uextend ~width:memory_data_bits)
  in
  for ram_address = 0 to frame_buffer_size - 1 do
    let start = frame_buffer_start_address + (ram_address * memory_data_bytes) in
    let data_bits =
      List.nth_exn data_bits_list ram_address |> split_lsb ~exact:true ~part_width:8
    in
    for i = 0 to 3 do
      Cyclesim.Memory.of_bits ~address:(start + i) ram (List.nth_exn data_bits i)
    done
  done;
  Cyclesim.cycle ~n:50 sim
;;

let test_ethernet ~data { sim; waveform = _; name = _ } =
  let inputs : _ With_transmitter.I.t = Cyclesim.inputs sim in
  (* Send a clear signal to initialize any CPU IO controller state back to default so
     we're ready to receive. *)
  clear_registers ~inputs sim;
  send_udp_packet ~packet:data sim;
  read_memory_and_write_frame_buffer
    ~address:ethernet_memory_address
    ~frame_buffer_size
    ~frame_buffer_start_address
    ~sim;
  let outputs : _ With_transmitter.O.t = Cyclesim.outputs sim in
  let get_video_out_pixel () =
    let count = ref 0 in
    while to_bool (no_bits_set !(outputs.vga_red)) && !count < 5000 do
      Cyclesim.cycle sim;
      count := !count + 1
    done;
    let result = [ !(outputs.vga_red); !(outputs.vga_green); !(outputs.vga_blue) ] in
    Cyclesim.cycle sim;
    result
  in
  let video_frame =
    Array.init
      ~f:(fun _ -> get_video_out_pixel ())
      (video_output_width * video_output_height)
  in
  printf "Video frame\n";
  Sequence.range 0 video_output_height
  |> Sequence.iter ~f:(fun y ->
    Sequence.range 0 video_output_width
    |> Sequence.iter ~f:(fun x ->
      let px = Array.get video_frame ((y * video_output_width) + x) in
      printf "(%x, " (to_int_trunc (List.nth_exn px 0));
      printf "%x, " (to_int_trunc (List.nth_exn px 1));
      printf "%x)" (to_int_trunc (List.nth_exn px 2)));
    printf "\n");
  match outputs.registers with
  | [ outputs ] ->
    let outputs =
      Cpu_with_dma_memory.Registers.map ~f:(fun t -> to_int_trunc !t) outputs
    in
    print_s [%message "" ~_:(outputs : int Cpu_with_dma_memory.Registers.t)];
    Test_util.print_ram sim
  | _ -> raise_s [%message "BUG: Unexpected number of harts"]
;;

let%expect_test "test ethernet vga output" =
  let sim = create_sim "test_dma_hello_world_ethernet_vga" in
  test_ethernet ~data:"GoodbyeHello" sim;
  finalize_sim sim;
  [%expect
    {|
    Video frame
    (6, 7, 4)(6, 7, 4)(f, 6, f)(f, 6, f)
    (6, 7, 4)(6, 7, 4)(f, 6, f)(f, 6, f)
    (6, 4, 6)(6, 4, 6)(9, 7, 2)(9, 7, 2)
    (6, 4, 6)(6, 4, 6)(9, 7, 2)(9, 7, 2)
    (4, 5, 6)(4, 5, 6)(5, 6, 8)(5, 6, 8)
    (4, 5, 6)(4, 5, 6)(5, 6, 8)(5, 6, 8)
    ((pc 0)
     (general (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)))
    ("00000000  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "00000010  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "00000020  47 6f 6f 64 62 79 65 48  65 6c 6c 6f 00 00 00 00  |GoodbyeHello....|"
     "00000030  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
     "00000040  76 04 00 00 6f 0f 00 00  46 06 00 00 79 02 00 00  |v...o...F...y...|"
     "00000050  54 06 00 00 65 08 00 00  00 00 00 00 00 00 00 00  |T...e...........|"
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
    |}]
;;
