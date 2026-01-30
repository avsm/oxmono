open Core
open Hardcaml
open Signal

module Make
    (Master_to_slave : Internal_bus_ports.Master_to_slave)
    (Slave_to_master : Internal_bus_ports.Slave_to_master) =
struct
  module T = struct
    module I = struct
      type 'a t =
        { clock : 'a
        ; clear : 'a
        ; slave_dn : 'a Slave_to_master.t [@rtlprefix "slave_dn$"]
        ; master_up : 'a Master_to_slave.t [@rtlprefix "master_up$"]
        }
      [@@deriving hardcaml ~rtlmangle:false]
    end

    module O = struct
      type 'a t =
        { slave_up : 'a Slave_to_master.t [@rtlprefix "slave_up$"]
        ; master_dn : 'a Master_to_slave.t [@rtlprefix "master_dn$"]
        ; timeout_cnt : 'a [@bits 32]
        }
      [@@deriving hardcaml ~rtlmangle:false]
    end

    module State = struct
      type t =
        | Idle
        | Write
        | Read_dn (* Sending the read downstream *)
        | Read_up (* Returning read data upstream *)
        | Timed_out
      [@@deriving enumerate, compare ~localize, sexp_of]
    end

    module Timeout = struct
      type t =
        { count : int
        ; return_value : int32
        }
    end

    let create ?(timeout : Timeout.t option) ~supports_wready scope (i : _ I.t) =
      let ( -- ) = Scope.naming scope in
      let reg_spec = Reg_spec.create ~clock:i.clock ~clear:i.clear () in
      let open Always in
      let sm = State_machine.create (module State) reg_spec in
      ignore (sm.current -- "STATE" : Signal.t);
      (* Output is registered except for ready signals to upstream. This is to guard
         against the case we see a valid write or read but it is de-asserted before the
         handshake is finished. *)
      let o = O.Of_always.reg reg_spec in
      let slave_up_write_ready =
        if supports_wready
        then Variable.reg reg_spec ~width:1
        else Variable.wire ~default:gnd ()
      in
      let%hw_var timeout_count =
        Variable.reg
          reg_spec
          ~width:
            (num_bits_to_represent
               (Option.value_map timeout ~f:(fun t -> t.count) ~default:1))
      in
      let maybe_timeout =
        Option.value_map timeout ~default:[] ~f:(fun timeout ->
          [ timeout_count <-- timeout_count.value +:. 1
          ; when_
              (timeout_count.value ==:. timeout.count)
              [ when_
                  o.master_dn.read_valid.value
                  [ o.slave_up.read_data <-- of_int32_trunc ~width:32 timeout.return_value
                  ; o.slave_up.read_ready <-- vdd
                  ]
              ; Master_to_slave.(Of_always.assign o.master_dn (Of_signal.zero ()))
              ; sm.set_next Timed_out
              ]
          ])
        |> proc
      in
      compile
        [ Slave_to_master.(Of_always.assign o.slave_up (Of_signal.zero ()))
        ; (* The _first signals don't wait for ready before de-asserting. *)
          o.master_dn.read_first <-- gnd
        ; o.master_dn.write_first <-- gnd
        ; sm.switch
            [ ( Idle
              , [ Master_to_slave.(Of_always.assign o.master_dn (Of_signal.zero ()))
                ; timeout_count <--. 0
                ; if_
                    i.master_up.write_valid
                    [ (if supports_wready then [ sm.set_next Write ] else []) |> proc
                    ; slave_up_write_ready <-- vdd
                    ; Master_to_slave.Of_always.assign
                        o.master_dn
                        { i.master_up with
                          read_valid = gnd
                        ; read_first = gnd
                        ; write_first = vdd
                        }
                    ]
                  @@ elif
                       i.master_up.read_valid
                       [ sm.set_next Read_dn
                       ; Master_to_slave.Of_always.assign
                           o.master_dn
                           { i.master_up with
                             write_valid = gnd
                           ; write_first = gnd
                           ; read_first = vdd
                           }
                       ]
                  @@ []
                ] )
            ; ( Write
              , [ (if supports_wready then slave_up_write_ready <--. 0 else proc [])
                ; when_
                    i.slave_dn.write_ready
                    [ sm.set_next Idle
                    ; Master_to_slave.(Of_always.assign o.master_dn (Of_signal.zero ()))
                    ]
                ; maybe_timeout
                ] )
            ; ( Read_dn
              , [ when_
                    i.slave_dn.read_ready
                    [ o.slave_up.read_data <-- i.slave_dn.read_data
                    ; (* Guard against case that upstream lowers its read valid
                         mid-handshake for whatever reason. *)
                      when_ i.master_up.read_valid [ o.slave_up.read_ready <-- vdd ]
                    ; Master_to_slave.(Of_always.assign o.master_dn (Of_signal.zero ()))
                    ; sm.set_next Read_up
                    ]
                ; maybe_timeout
                ] )
            ; Read_up, [ o.slave_up.read_ready <-- gnd; sm.set_next Idle ]
            ; Timed_out, [ incr o.timeout_cnt; sm.set_next Idle ]
            ]
        ];
      let o = O.Of_always.value o in
      { o with slave_up = { o.slave_up with write_ready = slave_up_write_ready.value } }
    ;;

    let hierarchical_inner ?timeout ?instance ~supports_wready scope i =
      let module Scoped = Hierarchy.In_scope (I) (O) in
      Scoped.hierarchical
        ?instance
        ~scope
        ~name:"ibus_register"
        (create ?timeout ~supports_wready)
        i
    ;;
  end

  include T

  (* Pipeline code *)
  module Reg_for_pipeline = struct
    module Config = struct
      type t =
        { supports_wready : bool
        ; timeout : Timeout.t option
        ; timeout_cnt : Signal.t option ref
        }
    end

    let hierarchical
      ?instance
      ~(config : Config.t)
      scope
      ~clock
      ~clear
      ~slave_dn
      ~master_up
      =
      let%tydi { slave_up; master_dn; timeout_cnt = timeout_cnt' } =
        T.hierarchical_inner
          ?instance
          ?timeout:config.timeout
          ~supports_wready:config.supports_wready
          scope
          { clock; clear; slave_dn; master_up }
      in
      (* We only expose the timeout counter for the first Ibus register when used in a
         pipeline. This is OK as if one of the registers timeout, it would imply that all
         in the chain have (and should be counted as a single timeout). *)
      if Option.is_none !(config.timeout_cnt) then config.timeout_cnt := Some timeout_cnt';
      slave_up, master_dn
    ;;
  end

  include struct
    module B =
      Build_register_pipeline.Make (Master_to_slave) (Slave_to_master) (Reg_for_pipeline)

    let create
      ?(n = 1)
      ?timeout
      ~supports_wready
      scope
      ({ clock; clear; slave_dn; master_up } : _ I.t)
      =
      let timeout_cnt = ref None in
      let%tydi { slave_up; master_dn } =
        B.pipeline_simple
          ~config:{ supports_wready; timeout; timeout_cnt }
          ~n
          scope
          { clock; clear; slave_dn; master_up }
      in
      { O.slave_up; master_dn; timeout_cnt = Option.value_exn !timeout_cnt }
    ;;

    let hierarchical ?timeout ?instance ?n ~supports_wready scope i =
      let module Scoped = Hierarchy.In_scope (I) (O) in
      Scoped.hierarchical
        ?instance
        ~scope
        ~name:"ibus_register_pipeline"
        (create ?timeout ?n ~supports_wready)
        i
    ;;
  end
end
