open Base
open Hardcaml
include Stream_intf
module Source_untyped = Stream_untyped.Source
module Dest_untyped = Stream_untyped.Dest

module Make (X : Config) = struct
  module Dest = struct
    type 'a t = 'a Dest_untyped.t = { tready : 'a } [@@deriving hardcaml]

    let of_untyped { Dest_untyped.tready } = { tready }
    let to_untyped { tready } = { Stream_untyped.Dest.tready }
  end

  module Source = struct
    type 'a t = 'a Source_untyped.t =
      { tvalid : 'a
      ; tdata : 'a [@bits X.data_bits]
      ; tkeep : 'a [@bits X.data_bits / 8]
      ; tstrb : 'a [@bits X.data_bits / 8]
      ; tlast : 'a
      ; tuser : 'a [@bits X.user_bits]
      }
    [@@deriving hardcaml, compare ~localize]

    let get_valid (t : Signal.t t) = t.tvalid
    let set_valid t ~valid:tvalid = { t with tvalid }

    let of_untyped { Source_untyped.tvalid; tdata; tkeep; tstrb; tlast; tuser } =
      let x = { tvalid; tdata; tkeep; tstrb; tlast; tuser } in
      Of_signal.assert_widths x;
      x
    ;;

    let to_untyped { tvalid; tdata; tkeep; tstrb; tlast; tuser } =
      { Stream_untyped.Source.tvalid; tdata; tkeep; tstrb; tlast; tuser }
    ;;

    module Clocked = struct
      let of_untyped { Source_untyped.tvalid; tdata; tkeep; tstrb; tlast; tuser } =
        let x = { tvalid; tdata; tkeep; tstrb; tlast; tuser } in
        Of_clocked_signal.assert_widths x;
        x
      ;;
    end
  end

  let add_properties
    ?(clear = Signal.gnd)
    ?(prefix = "")
    ~(source : _ Source.t)
    ~(dest : _ Dest.t)
    scope
    =
    if Scope.trace_properties scope
    then (
      let open Signal in
      let open Property.LTL in
      let make_ap name signal = Scope.make_ltl_ap scope (prefix ^ name) signal in
      let ap_tvalid = make_ap "tvalid" source.tvalid in
      let ap_tready = make_ap "tready" dest.tready in
      let ap_clear = make_ap "tclear" clear in
      let ap_tlast = ap_tvalid &: make_ap "tlast" source.tlast in
      let make_bits_aps name signal =
        List.range 0 (width signal)
        |> List.map ~f:(fun n -> make_ap (name ^ Int.to_string n) signal.:(n))
      in
      let aps_tdata = make_bits_aps "tdata" source.tdata in
      let aps_tkeep = make_bits_aps "tkeep" source.tkeep in
      let aps_tstrb = make_bits_aps "tstrb" source.tstrb in
      Scope.add_ltl_property
        scope
        "tvalid until tready"
        (g (ap_tvalid ==>: r ap_tready ap_tvalid));
      Scope.add_ltl_property
        scope
        "tlast remains until ready"
        (g (ap_tlast ==>: r ap_tready ap_tlast));
      (* Generates one LTL property per bit to ensure that bit doesn't change when tvalid
         is high until after we see a tready. Can't be just a single LTL property as that
         would require 2^number_of_bits states *)
      List.concat [ [ ap_tlast ]; aps_tdata; aps_tkeep; aps_tstrb ]
      |> List.iter ~f:(fun ap ->
        let prop =
          g (ap_tvalid &: ap ==>: (r ap_tready ap |: u ap ap_clear))
          &: g (ap_tvalid &: ~:ap ==>: (r ap_tready ~:ap |: u ~:ap ap_clear))
        in
        let signal_name signal = List.hd_exn (Signal.names signal) in
        let ap_name = Property.LTL.to_string ~name:signal_name ap in
        Scope.add_ltl_property
          scope
          [%string "tvalid waits for tready without %{ap_name} changing"]
          prop))
  ;;

  module Chain = struct
    module Transform = struct
      type t = (Signal.t Source.t, Signal.t Source.t) Handshake.t
    end

    module Transform_constructor = struct
      type t = clock:Signal.t -> clear:Signal.t -> Scope.t -> Transform.t
    end

    let transform f ~clock ~clear scope =
      Handshake.component (fun (io : _ Hardcaml_handshake.IO.t) ->
        let ((src, dst) : Signal.t Source.t * Signal.t Dest.t) =
          f ~clock ~clear scope io.data { Dest.tready = io.ack }
        in
        { Hardcaml_handshake.IO.data = src; ack = dst.tready })
    ;;

    type t = (Signal.t Source.t, Signal.t Source.t) Handshake.t

    let ( >>> ) = Handshake.( >>> )

    let run (t : t) (up : Signal.t Source.t) (dn : Signal.t Dest.t) =
      let handshake = Handshake.run t { data = up; ack = dn.tready } in
      handshake.data, { Dest.tready = handshake.ack }
    ;;
  end

  module Datapath_register = struct
    module Datapath_register_base = Hardcaml_circuits.Datapath_register.Make (Source)

    module T = struct
      module IO = struct
        type 'a t =
          { source : 'a Source.t
          ; dest : 'a Dest.t
          }
        [@@deriving hardcaml ~rtlmangle:false]
      end

      module I = struct
        type 'a t =
          { clock : 'a
          ; clear : 'a
          ; i : 'a IO.t [@rtlprefix "i_"]
          }
        [@@deriving hardcaml ~rtlmangle:false]
      end

      let create_io ?attributes spec ({ source; dest } : _ IO.t) =
        let base_o =
          Datapath_register_base.create_io
            ?attributes
            spec
            { data = source; valid = source.tvalid; ready = dest.tready }
        in
        { IO.source = { base_o.data with tvalid = base_o.valid }
        ; dest = { tready = base_o.ready }
        }
      ;;

      let create ?attributes _scope (i : _ I.t) =
        let spec = Signal.Reg_spec.create () ~clock:i.clock ~clear:i.clear in
        create_io ?attributes spec i.i
      ;;

      let hierarchical ?instance ?attributes scope i =
        let module Scoped = Hierarchy.In_scope (I) (IO) in
        Scoped.hierarchical
          ~scope
          ~name:("axi_datapath_reg_" ^ Int.to_string X.data_bits)
          ?instance
          (create ?attributes)
          i
      ;;
    end

    include T

    (* Pipeline code *)
    module Reg_for_pipeline = struct
      module Config = struct
        type t = unit
      end

      let hierarchical ?instance ~config:_ scope ~clock ~clear ~slave_dn ~master_up =
        let%tydi { source = master_dn; dest = slave_up } =
          T.hierarchical
            ?instance
            scope
            { clock; clear; i = { source = master_up; dest = slave_dn } }
        in
        slave_up, master_dn
      ;;
    end

    module Pipeline_stage_descr = Build_register_pipeline.Pipeline_stage_descr

    include struct
      module B = Build_register_pipeline.Make (Source) (Dest) (Reg_for_pipeline)

      let convert_input
        ({ clock; clear; i = { source = master_up; dest = slave_dn } } : _ I.t)
        =
        { B.I.clock; clear; slave_dn; master_up }
      ;;

      let convert_output { B.O.slave_up = dest; master_dn = source } = { IO.source; dest }

      let pipeline_simple ?instance_name ~n scope i =
        B.pipeline_simple ?instance:instance_name ~config:() ~n scope (convert_input i)
        |> convert_output
      ;;

      let pipeline_expert ~pipeline_stages ~scope ~clock ~(io : _ IO.t) =
        B.pipeline_expert
          ~config:()
          ~pipeline_stages
          scope
          (convert_input { I.clock; clear = Signal.gnd; i = io })
        |> convert_output
      ;;
    end

    let handshake_simple ?instance_name ?(n = 1) =
      Chain.transform (fun ~clock ~clear scope src dst ->
        let i = { I.clock; clear; i = { source = src; dest = dst } } in
        let o = pipeline_simple ?instance_name ~n scope i in
        o.source, o.dest)
    ;;
  end
end

module type S_untyped =
  S with type 'a Source.t = 'a Source_untyped.t and type 'a Dest.t = 'a Dest_untyped.t

module Make_untyped = Make
