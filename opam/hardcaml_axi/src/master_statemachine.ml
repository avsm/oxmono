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
      | Idle
      | Write
      | Read
    [@@deriving compare ~localize, enumerate, sexp_of, variants]
  end

  module I = struct
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; int_master : 'a Internal_bus.Master_to_slave.t
      ; axi_slave : 'a Slave_to_master.t
      }
    [@@deriving hardcaml ~rtlmangle:false]
  end

  module O = struct
    type 'a t =
      { int_slave : 'a Internal_bus.Slave_to_master.t
      ; axi_master : 'a Master_to_slave.t
      }
    [@@deriving hardcaml ~rtlmangle:false]
  end

  let create _scope (i : _ I.t) =
    let reg_spec = Reg_spec.create ~clock:i.clock ~clear:i.clear () in
    let sm = Always.State_machine.create (module State) reg_spec ~enable:vdd in
    ignore @@ (sm.current -- "STATE");
    let int_slave = Internal_bus.Slave_to_master.Of_always.reg reg_spec in
    let axi_master = Master_to_slave.Of_always.reg reg_spec in
    let open Always in
    compile
      [ when_ i.axi_slave.arready [ axi_master.arvalid <--. 0 ]
      ; when_ i.axi_slave.awready [ axi_master.awvalid <--. 0 ]
      ; when_ i.axi_slave.wready [ axi_master.wvalid <--. 0 ]
      ; sm.switch
          [ ( Idle
            , [ (* Assumes the Ibus will not raise both read and write valid in the same
                   beat. *)
                when_
                  i.int_master.read_valid
                  [ axi_master.arvalid <--. 1
                  ; axi_master.araddr
                    <-- uresize
                          i.int_master.address
                          ~width:Master_to_slave.port_widths.araddr
                  ; axi_master.rready <--. 1
                  ; sm.set_next Read
                  ]
              ; when_
                  i.int_master.write_valid
                  [ axi_master.awvalid <--. 1
                  ; axi_master.wvalid <--. 1
                  ; axi_master.wstrb
                    <-- ones (Internal_bus.Master_to_slave.port_widths.write_data / 8)
                  ; axi_master.awaddr
                    <-- uresize
                          i.int_master.address
                          ~width:Master_to_slave.port_widths.awaddr
                  ; axi_master.wdata <-- i.int_master.write_data
                  ; axi_master.bready <--. 1
                  ; sm.set_next Write
                  ]
              ] )
          ; ( Read
            , [ when_
                  i.axi_slave.rvalid
                  [ int_slave.read_data <-- i.axi_slave.rdata
                  ; int_slave.read_ready <--. 1
                  ; Master_to_slave.(Of_always.assign axi_master (Of_signal.zero ()))
                  ]
              ; when_
                  (int_slave.read_ready.value &: i.int_master.read_valid)
                  [ Internal_bus.Slave_to_master.(
                      Of_always.assign int_slave (Of_signal.zero ()))
                  ; sm.set_next Idle
                  ]
              ] )
          ; ( Write
            , [ when_
                  i.axi_slave.bvalid
                  [ int_slave.write_ready <--. 1
                  ; Master_to_slave.(Of_always.assign axi_master (Of_signal.zero ()))
                  ]
              ; when_
                  (int_slave.write_ready.value &: i.int_master.write_valid)
                  [ Internal_bus.Slave_to_master.(
                      Of_always.assign int_slave (Of_signal.zero ()))
                  ; sm.set_next Idle
                  ]
              ] )
          ]
      ];
    { O.axi_master = Master_to_slave.Of_always.value axi_master
    ; int_slave = Internal_bus.Slave_to_master.Of_always.value int_slave
    }
  ;;

  let hierarchical ?(name = "master_statemachine") scope =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical ~scope ~name create
  ;;
end
