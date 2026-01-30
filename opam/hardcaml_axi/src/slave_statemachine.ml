open Base
open Hardcaml

module Make
    (Master_to_slave : Lite_ports.Master_to_slave)
    (Slave_to_master : Lite_ports.Slave_to_master)
    (Internal_bus : Internal_bus.S) =
struct
  open Signal

  module State = struct
    type t =
      | Wait_transaction_start
      | Wait_for_axi_write_address
      | Wait_for_axi_write_data
      | Wait_for_internal_write_done
      | Wait_for_axi_write_done
      | Wait_for_internal_read_done
      | Wait_for_axi_read_done
    [@@deriving compare ~localize, enumerate, sexp_of, variants]

    let to_string t = t |> sexp_of_t |> Sexp.to_string_hum

    let of_int_exn =
      let map =
        all
        |> List.map ~f:(fun v -> Variants.to_rank v, v)
        |> Map.of_alist_exn (module Int)
      in
      fun int -> Map.find_exn map int
    ;;
  end

  module I = struct
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; axi_master : 'a Master_to_slave.t
      ; int_slave : 'a Internal_bus.Slave_to_master.t
      }
    [@@deriving hardcaml ~rtlmangle:false]
  end

  module O = struct
    type 'a t =
      { axi_slave : 'a Slave_to_master.t
      ; int_master : 'a Internal_bus.Master_to_slave.t
      }
    [@@deriving hardcaml ~rtlmangle:false]
  end

  let create _scope (i : _ I.t) =
    (* AXI-channels
       {v
         ==========================
         awaddr  |--->|               write address
         awvalid |--->|
         awprot  |--->|
                 |<---| awready
         ==========================
         wdata   |--->|               write data
         wstrb   |--->|
         wvalid  |--->|
                 |<---| wready
         ==========================
         bready  |--->|               write response
                 |<---| bresp
                 |<---| bvalid
         ==========================
         araddr  |--->|               read address
         arvalid |--->|
         arprot  |--->|
                 |<---| arready
         ==========================
         rready  |--->|               read response
                 |<---| rdata
                 |<---| rresp
                 |<---| rvalid
       v}
    *)
    let reg_spec = Reg_spec.create ~clock:i.clock ~clear:i.clear () in
    let state = Always.State_machine.create (module State) reg_spec ~enable:vdd in
    ignore @@ (state.current -- "STATE");
    let int_master =
      Internal_bus.Master_to_slave.(map port_widths) ~f:(fun width ->
        Always.Variable.reg reg_spec ~enable:vdd ~width)
    in
    let axi_slave =
      Slave_to_master.(map port_widths) ~f:(fun width ->
        Always.Variable.reg reg_spec ~enable:vdd ~width)
    in
    Always.(
      compile
        [ axi_slave.arready <--. 0
        ; int_master.write_first <--. 0
        ; int_master.read_first <--. 0
        ; state.switch
            [ (* wait for an AXI transaction to start. In the initial state we immediately
                 acknowledge write address/data transactions with no delay. *)
              ( Wait_transaction_start
              , [ axi_slave.awready <--. 1
                ; axi_slave.wready <--. 1
                ; int_master.address <-- i.axi_master.awaddr
                ; int_master.write_data <-- i.axi_master.wdata
                ; int_master.write_byte_en <-- i.axi_master.wstrb
                ; if_
                    i.axi_master.awvalid
                    [ if_
                        i.axi_master.wvalid
                        [ (* got and responded to awvalid and wvalid *)
                          axi_slave.awready <--. 0
                        ; axi_slave.wready <--. 0
                        ; int_master.write_valid <--. 1
                        ; int_master.write_first <--. 1
                        ; state.set_next Wait_for_internal_write_done
                        ]
                        [ (* wait for wvalid, responded to awvalid *)
                          axi_slave.awready <--. 0
                        ; state.set_next Wait_for_axi_write_data
                        ]
                    ]
                  @@ elif
                       i.axi_master.wvalid
                       [ (* wait for awvalid, responded to wvalid *)
                         axi_slave.wready <--. 0
                       ; state.set_next Wait_for_axi_write_address
                       ]
                  @@ elif
                       i.axi_master.arvalid
                       [ (* respond to arvalid *)
                         axi_slave.arready <--. 1
                       ; axi_slave.awready <--. 0
                       ; axi_slave.wready <--. 0
                       ; int_master.read_valid <--. 1
                       ; int_master.read_first <--. 1
                       ; int_master.address <-- i.axi_master.araddr
                       ; state.set_next Wait_for_internal_read_done
                       ]
                       []
                ] )
            ; (* complete the address phase for the write transaction *)
              ( Wait_for_axi_write_address
              , [ int_master.address <-- i.axi_master.awaddr
                ; when_
                    i.axi_master.awvalid
                    [ axi_slave.awready <--. 0
                    ; int_master.write_valid <--. 1
                    ; int_master.write_first <--. 1
                    ; state.set_next Wait_for_internal_write_done
                    ]
                ] )
            ; (* complete the data phase for the write transaction *)
              ( Wait_for_axi_write_data
              , [ int_master.write_data <-- i.axi_master.wdata
                ; int_master.write_byte_en <-- i.axi_master.wstrb
                ; when_
                    i.axi_master.wvalid
                    [ axi_slave.wready <--. 0
                    ; int_master.write_valid <--. 1
                    ; int_master.write_first <--. 1
                    ; state.set_next Wait_for_internal_write_done
                    ]
                ] )
            ; (* perform the internal write *)
              ( Wait_for_internal_write_done
              , [ when_
                    i.int_slave.write_ready
                    [ int_master.write_valid <--. 0
                    ; axi_slave.bvalid <--. 1
                    ; axi_slave.bresp <--. 0
                    ; (* support anything else? *)
                      state.set_next Wait_for_axi_write_done
                    ]
                ] )
            ; (* wait for write confirmation response from the axi_master. *)
              ( Wait_for_axi_write_done
              , [ when_
                    i.axi_master.bready
                    [ axi_slave.bvalid <--. 0
                    ; axi_slave.awready <--. 1
                    ; axi_slave.wready <--. 1
                    ; state.set_next Wait_transaction_start
                    ]
                ] )
            ; (* perform the internal read *)
              ( Wait_for_internal_read_done
              , [ when_
                    i.int_slave.read_ready
                    [ int_master.read_valid <--. 0
                    ; axi_slave.rdata <-- i.int_slave.read_data
                    ; axi_slave.rresp <--. 0
                    ; (* support anything else? *)
                      axi_slave.rvalid <--. 1
                    ; state.set_next Wait_for_axi_read_done
                    ]
                ] )
            ; (* wait for read response from the axi_master *)
              ( Wait_for_axi_read_done
              , [ when_
                    i.axi_master.rready
                    [ axi_slave.rvalid <--. 0
                    ; axi_slave.awready <--. 1
                    ; axi_slave.wready <--. 1
                    ; state.set_next Wait_transaction_start
                    ]
                ] )
            ]
        ]);
    { O.axi_slave = Slave_to_master.map axi_slave ~f:Always.Variable.value
    ; int_master = Internal_bus.Master_to_slave.map int_master ~f:Always.Variable.value
    }
  ;;

  let hierarchical scope =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical ~scope ~name:"axi_statemachine" create
  ;;

  let with_slave_statemachine
    ?(hierarchical_instance = false)
    scope
    ~reg_spec
    ~axi_master
    ~create_fn
    =
    let int_slave = Internal_bus.Slave_to_master.Of_signal.wires () in
    let { O.axi_slave; int_master } =
      (if hierarchical_instance then hierarchical else create)
        scope
        { clock = Reg_spec.clock reg_spec
        ; clear = Reg_spec.clear_exn reg_spec
        ; axi_master
        ; int_slave
        }
    in
    let { Slave_with_data.slave = int_slave_s; data } = create_fn int_master in
    Internal_bus.Slave_to_master.iter2 int_slave int_slave_s ~f:( <-- );
    { Slave_with_data.slave = axi_slave; data }
  ;;
end
