open Base
open Hardcaml
include Lite_intf

module Make (X : Master_slave_bus_config.S) = struct
  module Internal_bus = Internal_bus.Make (X)
  module Master_to_slave = Lite_ports.Master_to_slave (X)
  module Slave_to_master = Lite_ports.Slave_to_master (X)

  module Slave_statemachine =
    Slave_statemachine.Make (Master_to_slave) (Slave_to_master) (Internal_bus)

  module Master_statemachine =
    Master_statemachine.Make (Master_to_slave) (Slave_to_master) (Internal_bus)

  module Demultiplexer = struct
    type 'a t =
      ( Signal.t Slave_to_master.t
        , Signal.t Internal_bus.Master_to_slave.t list )
        Slave_with_data.t
    [@@deriving sexp_of]

    let create
      scope
      ~reg_spec
      ~address_offset
      ~(axi_master : Signal.t Master_to_slave.t)
      ~(int_slaves : Signal.t Internal_bus.Slave_to_master.t list)
      =
      Slave_statemachine.with_slave_statemachine
        scope
        ~reg_spec
        ~axi_master
        ~create_fn:(fun master ->
          Internal_bus.Demultiplexer.create
            scope
            ~address_offset
            ~master
            ~slaves:int_slaves)
    ;;
  end

  module Ram_with_byte_enables = struct
    type t = Signal.t Slave_to_master.t [@@deriving sexp_of]

    let create scope ~reg_spec ~axi_master ~size =
      let x =
        Slave_statemachine.with_slave_statemachine
          scope
          ~reg_spec
          ~axi_master
          ~create_fn:(fun master ->
            Internal_bus.Ram_with_byte_enables.create ~reg_spec ~master ~size)
      in
      x.slave
    ;;
  end

  module Register_bank = struct
    type 'a t =
      (Signal.t Slave_to_master.t, Internal_bus.Register_bank.result) Slave_with_data.t
    [@@deriving sexp_of]

    let create scope ~reg_spec ~axi_master ~write_modes ~read_values : _ t =
      Slave_statemachine.with_slave_statemachine
        scope
        ~reg_spec
        ~axi_master
        ~create_fn:(fun master ->
          Internal_bus.Register_bank.create
            reg_spec
            ~master
            ~write_modes
            ~read_values
            ~clear_write_values:Signal.gnd)
    ;;

    module With_interface (Read : Interface.S) (Write : Interface.S) = struct
      type t =
        (Signal.t Slave_to_master.t, Signal.t With_valid.t Write.t) Slave_with_data.t
      [@@deriving sexp_of]

      module Internal = Internal_bus.Register_bank.With_interface (Read) (Write)

      let write_addresses = Internal.write_addresses
      let read_addresses = Internal.read_addresses

      let create scope ~reg_spec ~axi_master ~write_modes ~read_values =
        Slave_statemachine.with_slave_statemachine
          scope
          ~reg_spec
          ~axi_master
          ~create_fn:(fun master ->
            let { Internal.O.slave; write_values; read_enable = _ } =
              Internal.create
                scope
                ~write_modes
                { Internal.I.clock = Signal.Reg_spec.clock reg_spec
                ; clear = Signal.Reg_spec.clear_exn reg_spec
                ; master
                ; read_values
                ; clear_write_values = Signal.gnd
                }
            in
            { Slave_with_data.slave; data = write_values })
      ;;
    end
  end
end
