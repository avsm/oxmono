open! Core
open Hardcaml
open Hardcaml_memory_controller
open Hardcaml_io_controller
open Hardcaml_risc_v_hart
open Hardcaml_risc_v
open Signal

module Make
    (Hart_config : Hart_config_intf.S)
    (Memory_config : System_intf.Memory_config)
    (General_config : System_intf.Config)
    (Video_config : Framebuffer_expander.Config)
    (Axi4 : Axi4.S) =
struct
  module Registers = Registers.Make (Hart_config)
  module Decoded_instruction = Decoded_instruction.Make (Hart_config) (Registers)

  let required_read_channels_per_hart = Hart.required_read_channels
  let required_write_channels_per_hart = Hart.required_write_channels
  let system_non_hart_read_memory_channels = 3
  let system_non_hart_write_memory_channels = 2

  module Memory_controller =
    Memory_controller.Make
      (struct
        let capacity_in_bytes = Memory_config.capacity_in_bytes

        let num_read_channels =
          system_non_hart_read_memory_channels
          + (General_config.num_harts * Hart.required_read_channels)
        ;;

        let num_write_channels =
          system_non_hart_write_memory_channels
          + (General_config.num_harts * Hart.required_write_channels)
        ;;

        let address_width = Register_width.bits Hart_config.register_width
        let data_bus_width = Axi4.data_width
      end)
      (Axi4)

  module Memory_bus = Memory_controller.Memory_bus
  include Memory_bus

  module Video_out =
    Framebuffer_expander.Make (Video_config) (Memory_controller.Memory_bus)

  let write_ch_start_offset which_hart =
    system_non_hart_write_memory_channels + (which_hart * required_write_channels_per_hart)
  ;;

  let read_ch_start_offset which_hart =
    system_non_hart_read_memory_channels + (which_hart * required_read_channels_per_hart)
  ;;

  let select arr ch sz =
    let start = List.drop arr ch in
    List.take start sz
  ;;

  let select_rd_chs_for_hart which_hart arr =
    select arr (read_ch_start_offset which_hart) required_read_channels_per_hart
  ;;

  let select_wr_chs_for_hart which_hart arr =
    select arr (write_ch_start_offset which_hart) required_write_channels_per_hart
  ;;

  module Transaction = Transaction.Make (Hart_config) (Memory_controller.Memory_bus)

  let register_width = Register_width.bits Hart_config.register_width

  module Hart =
    Hart.Make (Hart_config) (Memory_controller.Memory_bus) (Registers)
      (Decoded_instruction)
      (Transaction)

  module Memory_to_packet8 =
    Memory_to_packet8.Make
      (struct
        let header = Some 'D'
      end)
      (Memory_controller.Memory_bus)
      (Axi8)

  module Dma =
    System_dma_controller.Make (General_config) (Memory_config)
      (Memory_controller.Memory_bus)
      (Memory_to_packet8)

  module I = struct
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; uart_rx : 'a
      ; eth_crsdv : 'a
      ; eth_rxerr : 'a
      ; eth_rxd : 'a [@bits 2]
      ; memory : 'a Axi4.I.t [@rtlprefix "axi_i$"]
      }
    [@@deriving hardcaml]
  end

  module O = struct
    type 'a t =
      { registers : 'a Registers.t list [@length General_config.num_harts]
      ; ethernet_txen : 'a
      ; ethernet_txd : 'a [@bits 2]
      ; uart_tx : 'a
      ; uart_rx_valid : 'a
      ; vga_red : 'a [@bits 4]
      ; vga_green : 'a [@bits 4]
      ; vga_blue : 'a [@bits 4]
      ; vga_hsync : 'a
      ; vga_vsync : 'a
      ; memory : 'a Axi4.O.t [@rtlprefix "axi_o$"]
      }
    [@@deriving hardcaml]
  end

  let default_transaction (hart : _ Hart.O.t) =
    { With_valid.valid = vdd
    ; value =
        { Transaction.set_rd = vdd
        ; new_rd = zero register_width
        ; new_pc = hart.registers.pc +:. 4
        ; error = gnd
        }
    }
  ;;

  let assign_non_io_ecall ((hart : _ Hart.O.t), transaction) =
    (* Default the remainders *)
    Transaction.With_valid.Of_signal.(transaction <-- default_transaction hart)
  ;;

  let uart_dma_read_slot = 0
  let ethernet_dma_read_slot = 1
  let vga_read_slot = 2
  let uart_dma_write_slot = 0
  let ethernet_dma_write_slot = 1

  let assign_dma_io_ecall
    ~clock
    (harts : _ Hart.O.t list)
    hart_ecall_transactions
    ~tx_input
    ~tx_ready
    scope
    =
    (* For now we only allow Hart0 to do IO. This isn't necessary, but it makes it easier
       to stop them both issuing commands at the same time and entering a weird state. *)
    let reg_spec_no_clear = Reg_spec.create ~clock () in
    let hart0 = List.nth_exn harts 0 in
    (* Delay the ecall by a cycle so we can register all the relevant registers, reducing
       routing pressure. *)
    let%hw delayed_r5 =
      reg reg_spec_no_clear (List.nth_exn hart0.registers.general 5 ==:. 0)
    in
    let%hw delayed_r6 = reg reg_spec_no_clear (List.nth_exn hart0.registers.general 6) in
    let%hw delayed_r7 =
      reg
        reg_spec_no_clear
        (List.nth_exn hart0.registers.general 7
         |> uresize ~width:Memory_to_packet8.Input.port_widths.length)
    in
    let%hw is_dma_write = hart0.is_ecall &: delayed_r5 in
    let%hw next_pc = reg reg_spec_no_clear (hart0.registers.pc +:. 4) in
    Memory_to_packet8.Input.With_valid.Of_signal.(
      tx_input
      <-- { valid = is_dma_write &: tx_ready
          ; value = { address = delayed_r6; length = delayed_r7 }
          });
    (* Assign the Hart0 transaction. *)
    Transaction.With_valid.Of_signal.(
      List.hd_exn hart_ecall_transactions
      <-- { With_valid.valid = vdd
          ; value =
              { Transaction.set_rd = vdd
              ; new_rd = uextend ~width:register_width (is_dma_write &: tx_ready)
              ; new_pc = next_pc
              ; error = gnd
              }
          });
    (* Default the remainders *)
    List.zip_exn harts hart_ecall_transactions
    |> List.tl_exn
    |> List.iter ~f:assign_non_io_ecall
  ;;

  let assign_ecalls ~clock ~tx_input ~tx_ready harts hart_ecall_transactions scope =
    (* If a DMA controller is in the design then wire up DMA related ecalls otherwise do
       not include any ecalls *)
    assign_dma_io_ecall ~clock ~tx_input ~tx_ready harts hart_ecall_transactions scope
  ;;

  let initialize_harts
    ~io_clear
    ~hart_ecall_transactions
    ~read_bus_per_hart
    ~write_bus_per_hart
    ~(memory_controller : _ Memory_controller.O.t)
    scope
    { I.clock; clear; _ }
    =
    let reg_spec_no_clear = Reg_spec.create ~clock () in
    let harts =
      List.init
        ~f:(fun which_hart ->
          let hart =
            Hart.hierarchical
              ~instance:[%string "hart_%{which_hart#Int}"]
              scope
              { Hart.I.clock
              ; clear = pipeline ~n:2 reg_spec_no_clear (io_clear |: clear)
              ; read_bus =
                  select_rd_chs_for_hart which_hart memory_controller.read_to_controller
              ; write_bus =
                  select_wr_chs_for_hart which_hart memory_controller.write_to_controller
              ; read_response =
                  select_rd_chs_for_hart which_hart memory_controller.read_response
              ; write_response =
                  select_wr_chs_for_hart which_hart memory_controller.write_response
              ; ecall_transaction = List.nth_exn hart_ecall_transactions which_hart
              }
          in
          hart)
        General_config.num_harts
    in
    List.iter2_exn
      ~f:(List.iter2_exn ~f:Read_bus.Source.Of_signal.( <-- ))
      read_bus_per_hart
      (List.map ~f:Hart.O.read_bus harts);
    List.iter2_exn
      ~f:(List.iter2_exn ~f:Write_bus.Source.Of_signal.( <-- ))
      write_bus_per_hart
      (List.map ~f:Hart.O.write_bus harts);
    harts
  ;;

  let create ~build_mode scope (i : _ I.t) =
    (* Initialize the DMA controller with a bunch of unconnected wires. These will be
       wired to the memory controller and harts below. *)
    let tx_input = Memory_to_packet8.Input.With_valid.Of_signal.wires () in
    let dma_read_request_uart = Read_bus.Dest.Of_signal.wires () in
    let dma_read_request_ethernet = Read_bus.Dest.Of_signal.wires () in
    let read_request_vga = Read_bus.Dest.Of_signal.wires () in
    let dma_write_request_uart = Write_bus.Dest.Of_signal.wires () in
    let dma_write_request_ethernet = Write_bus.Dest.Of_signal.wires () in
    let dma_read_response_uart = Read_response.With_valid.Of_signal.wires () in
    let dma_read_response_ethernet = Read_response.With_valid.Of_signal.wires () in
    let read_response_vga = Read_response.With_valid.Of_signal.wires () in
    let dma_write_response_uart = Write_response.With_valid.Of_signal.wires () in
    let dma_write_response_ethernet = Write_response.With_valid.Of_signal.wires () in
    let dma_controller =
      Dma.create
        ~uart_config:General_config.io_controller
        scope
        { Dma.I.clock = i.clock
        ; clear = i.clear
        ; uart_rx = i.uart_rx
        ; eth_crsdv = i.eth_crsdv
        ; eth_rxerr = i.eth_rxerr
        ; eth_rxd = i.eth_rxd
        ; tx_input
        ; uart_read_request = dma_read_request_uart
        ; ethernet_read_request = dma_read_request_ethernet
        ; uart_write_request = dma_write_request_uart
        ; ethernet_write_request = dma_write_request_ethernet
        ; uart_read_response = dma_read_response_uart
        ; ethernet_read_response = dma_read_response_ethernet
        ; uart_write_response = dma_write_response_uart
        ; ethernet_write_response = dma_write_response_ethernet
        }
    in
    (* Initialize the memory controller and allocate some wires for channels. Any non-hart
       memory channels will occupy the start of the memory controller, followed by the
       memory channels of each hart. *)
    let read_bus_per_hart =
      List.init
        ~f:(fun _which_hart ->
          List.init
            ~f:(fun _i -> Read_bus.Source.Of_signal.wires ())
            required_read_channels_per_hart)
        General_config.num_harts
    in
    let write_bus_per_hart =
      List.init
        ~f:(fun _which_hart ->
          List.init
            ~f:(fun _i -> Write_bus.Source.Of_signal.wires ())
            required_write_channels_per_hart)
        General_config.num_harts
    in
    let video_out_read_request = Read_bus.Source.Of_signal.wires () in
    let controller =
      Memory_controller.hierarchical
        ~priority_mode:Priority_order
        scope
        { Memory_controller.I.clock = i.clock
        ; clear = i.clear
        ; read_to_controller =
            [ dma_controller.uart_read_request
            ; dma_controller.ethernet_read_request
            ; video_out_read_request
            ]
            @ List.concat read_bus_per_hart
        ; write_to_controller =
            [ dma_controller.uart_write_request; dma_controller.ethernet_write_request ]
            @ List.concat write_bus_per_hart
        ; memory = i.memory
        }
    in
    let hart_ecall_transactions =
      List.init
        ~f:(fun _ -> Transaction.With_valid.Of_signal.wires ())
        General_config.num_harts
    in
    let harts =
      initialize_harts
        ~io_clear:dma_controller.clear_message
          (* Clear harts when the DMA core pulses clear in addition to the global clear if
             a DMA controller is attached. *)
        ~hart_ecall_transactions
        ~write_bus_per_hart
        ~read_bus_per_hart
        ~memory_controller:controller
        scope
        i
    in
    assign_ecalls
      ~clock:i.clock
      ~tx_input
      ~tx_ready:dma_controller.dma_tx_ready
      harts
      hart_ecall_transactions
      scope;
    Read_bus.Dest.Of_signal.(
      dma_read_request_uart
      <-- List.nth_exn controller.read_to_controller uart_dma_read_slot);
    Read_response.With_valid.Of_signal.(
      dma_read_response_uart <-- List.nth_exn controller.read_response uart_dma_read_slot);
    Read_bus.Dest.Of_signal.(
      dma_read_request_ethernet
      <-- List.nth_exn controller.read_to_controller ethernet_dma_read_slot);
    Read_response.With_valid.Of_signal.(
      dma_read_response_ethernet
      <-- List.nth_exn controller.read_response ethernet_dma_read_slot);
    Read_bus.Dest.Of_signal.(
      read_request_vga <-- List.nth_exn controller.read_to_controller vga_read_slot);
    Read_response.With_valid.Of_signal.(
      read_response_vga <-- List.nth_exn controller.read_response vga_read_slot);
    Write_bus.Dest.Of_signal.(
      dma_write_request_uart
      <-- List.nth_exn controller.write_to_controller uart_dma_write_slot);
    Write_bus.Dest.Of_signal.(
      dma_write_request_ethernet
      <-- List.nth_exn controller.write_to_controller ethernet_dma_write_slot);
    Write_response.With_valid.Of_signal.(
      dma_write_response_uart
      <-- List.nth_exn controller.write_response uart_dma_write_slot);
    Write_response.With_valid.Of_signal.(
      dma_write_response_ethernet
      <-- List.nth_exn controller.write_response ethernet_dma_write_slot);
    let video_out =
      Video_out.hierarchical
        ~build_mode
        scope
        { Video_out.I.clock = i.clock
        ; clear = i.clear
        ; memory_request = read_request_vga
        ; memory_response = read_response_vga
        }
    in
    Read_bus.Source.Of_signal.(video_out_read_request <-- video_out.memory_request);
    { O.registers = List.map ~f:(fun o -> o.registers) harts
    ; ethernet_txen = dma_controller.ethernet_txen
    ; ethernet_txd = dma_controller.ethernet_txd
    ; uart_tx = dma_controller.uart_tx
    ; uart_rx_valid = dma_controller.uart_rx_valid
    ; vga_red = video_out.red
    ; vga_green = video_out.green
    ; vga_blue = video_out.blue
    ; vga_hsync = video_out.hsync
    ; vga_vsync = video_out.vsync
    ; memory = controller.memory
    }
  ;;

  let hierarchical ~build_mode (scope : Scope.t) (input : Signal.t I.t) =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical ~scope ~name:"system" (create ~build_mode) input
  ;;
end
