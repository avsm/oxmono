open! Core
open Hardcaml
open Hardcaml_axi
open Signal

module Make (Axi : Stream.S) = struct
  let data_width = Axi.Source.port_widths.tdata
  let bytes_per_word = data_width / 8

  module Config = struct
    type 'a t =
      { start_address : 'a [@bits data_width]
      ; max_address : 'a [@bits data_width]
      }
    [@@deriving hardcaml]
  end

  module I = struct
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; config : 'a Config.t
      ; input_data_stream : 'a Axi.Source.t
      ; output_data_stream_ready : 'a Axi.Dest.t
      }
    [@@deriving hardcaml]
  end

  module O = struct
    type 'a t =
      { ready : 'a Axi.Dest.t
      ; output_data_stream : 'a Axi.Source.t
      }
    [@@deriving hardcaml]
  end

  let create
    (scope : Scope.t)
    ({ I.clock; clear; config; input_data_stream; output_data_stream_ready } : _ I.t)
    =
    let spec = Reg_spec.create () ~clock ~clear in
    let%hw_var address_overflow = Always.Variable.reg spec ~width:1 in
    let address_spec =
      Reg_spec.create () ~clock ~clear:(clear |: address_overflow.value)
    in
    let tfirst = input_data_stream.tuser.:(0) in
    let axi_reg = Axi.Source.Of_signal.pipeline spec ~n:1 input_data_stream in
    let%hw current_address =
      reg_fb address_spec ~width:data_width ~clear_to:config.start_address ~f:(fun d ->
        d
        +: mux2
             input_data_stream.tvalid
             (of_int_trunc ~width:data_width bytes_per_word)
             (zero data_width))
    in
    Always.(
      compile
        [ address_overflow
          <-- (axi_reg.tlast &: axi_reg.tvalid &: (current_address >=: config.max_address))
        ]);
    { O.output_data_stream =
        { Axi.Source.tvalid = input_data_stream.tvalid &: tfirst |: axi_reg.tvalid
        ; tdata = mux2 tfirst current_address axi_reg.tdata
        ; tkeep = axi_reg.tkeep
        ; tstrb = axi_reg.tstrb
        ; tlast = axi_reg.tlast
        ; tuser = axi_reg.tuser.:(1) @: tfirst
        }
    ; ready = { tready = output_data_stream_ready.tready }
    }
  ;;

  let hierarchical (scope : Scope.t) (input : Signal.t I.t) =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical ~scope ~name:"prepend_address" create input
  ;;
end
