open! Core
open Hardcaml
open Hardcaml_axi
open Hardcaml_memory_controller
open Signal
open Always

module Make (Memory : Memory_bus_intf.S) (Axi : Stream.S) = struct
  let address_width = Memory.address_width
  let data_width = Memory.data_bus_width
  let bytes_per_word = data_width / 8

  module Input = struct
    module T = struct
      type 'a t =
        { length : 'a [@bits 16]
        ; address : 'a [@bits address_width]
        }
      [@@deriving hardcaml]
    end

    include T
    module With_valid = With_valid.Wrap.Make (T)
  end

  module I = struct
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; read_input : 'a Input.With_valid.t
      ; output_packet : 'a Axi.Dest.t
      ; memory : 'a Memory.Read_bus.Dest.t
      ; memory_response : 'a Memory.Read_response.With_valid.t
      }
    [@@deriving hardcaml]
  end

  module O = struct
    type 'a t =
      { ready : 'a
      ; output_packet : 'a Axi.Source.t [@rtlprefix "output$"]
      ; memory : 'a Memory.Read_bus.Source.t [@rtlprefix "memory$"]
      }
    [@@deriving hardcaml]
  end

  let () =
    if Axi.Source.port_widths.tdata <> data_width || Axi.Source.port_widths.tdata <> 32
    then
      raise_s
        [%message
          "BUG: Only a memory with a 32-bit data bus to an axi32 stream is currently \
           supported. "]
  ;;

  module State = struct
    type t =
      | Idle
      | Reading_data
      | Writing_data
    [@@deriving sexp, enumerate, compare ~localize]
  end

  let create (scope : Scope.t) (i : _ I.t) =
    let read_valid = i.read_input.valid in
    let%tydi { length = read_length; address = read_address } = i.read_input.value in
    let reg_spec = Reg_spec.create ~clock:i.clock ~clear:i.clear () in
    let reg_spec_no_clear = Reg_spec.create ~clock:i.clock () in
    let%hw.State_machine state = State_machine.create (module State) reg_spec in
    let%hw_var do_read = Variable.reg ~width:1 reg_spec in
    let first_byte = Variable.reg ~width:1 reg_spec in
    let%hw_var length = Variable.reg ~width:(width read_length) reg_spec_no_clear in
    let%hw_var address = Variable.reg ~width:(width read_address) reg_spec_no_clear in
    let%hw_var read_data =
      Variable.reg ~width:(width i.memory_response.value.read_data) reg_spec_no_clear
    in
    let enter_reading_data = proc [ state.set_next Reading_data; do_read <-- vdd ] in
    let reset = proc [ do_read <-- gnd; first_byte <-- vdd; state.set_next Idle ] in
    let aligned_address = Memory.byte_address_to_memory_address read_address in
    let last_bytes = state.is Writing_data &: (length.value <=:. bytes_per_word) in
    let tkeep =
      mux_init (sel_bottom ~width:2 length.value) bytes_per_word ~f:(fun t ->
        let width = bytes_per_word in
        if t = 0 then ones width else concat_msb [ zero (width - t); ones t ])
    in
    compile
      [ state.switch
          [ ( State.Idle
            , [ reset
              ; when_
                  (* Ignore read addresses that are not aligned *)
                  (read_valid &: (read_length <>:. 0) &: aligned_address.valid)
                  [ length <-- read_length
                  ; address <-- aligned_address.value
                  ; enter_reading_data
                  ]
              ] )
          ; ( Reading_data
            , [ when_ i.memory.ready [ do_read <-- gnd ]
              ; when_
                  i.memory_response.valid
                  [ read_data <-- i.memory_response.value.read_data
                  ; state.set_next Writing_data
                  ]
              ] )
          ; ( Writing_data
            , [ when_
                  i.output_packet.tready
                  [ length <-- length.value -:. bytes_per_word
                  ; first_byte <-- gnd
                  ; incr address
                  ; enter_reading_data
                  ; (* If this was the last write, reset the entire state machine to idle. *)
                    when_ (length.value <=:. bytes_per_word) [ reset ]
                  ]
              ] )
          ]
      ];
    { O.ready = state.is State.Idle
    ; output_packet =
        { tvalid = state.is Writing_data &: i.output_packet.tready
        ; tdata = read_data.value
        ; tlast = last_bytes
        ; tkeep = mux2 last_bytes tkeep (ones bytes_per_word)
        ; tstrb = ones bytes_per_word
        ; tuser = zero 1 @: first_byte.value
        }
    ; memory = { valid = do_read.value; data = { address = address.value } }
    }
  ;;

  let hierarchical (scope : Scope.t) (input : Signal.t I.t) =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical ~scope ~name:"dma_memory_to_axi32" create input
  ;;
end
