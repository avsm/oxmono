open Base
open Hardcaml

module type Master_to_slave = Lite_ports.Master_to_slave
module type Slave_to_master = Lite_ports.Slave_to_master

module type Lite = sig
  module type Master_to_slave = Master_to_slave
  module type Slave_to_master = Slave_to_master

  module Make (X : Master_slave_bus_config.S) : sig
    (** A simplified protocol for reading from and writing to slave space within a
        hardware design. Reads and writes are framed by [_valid] and [_first] signals. The
        [_valid] signal will be high throughout the transaction, while [_first] will
        toggle high only on the first cycle of a transaction. The [_ready] signals, driven
        by the slave, indicate completion of a transaction. *)
    module Internal_bus : Internal_bus.S

    module Master_to_slave : Master_to_slave
    module Slave_to_master : Slave_to_master

    (** Statemachine for conversion between AXI transfers, and the simplified protocol
        defined by [Internal_bus] *)
    module Slave_statemachine :
        module type of
          Slave_statemachine.Make (Master_to_slave) (Slave_to_master) (Internal_bus)

    (** Statemachine for conversion from Ibus to AXI lite, with the Ibus as the master. *)
    module Master_statemachine :
        module type of
          Master_statemachine.Make (Master_to_slave) (Slave_to_master) (Internal_bus)

    (** Convert a single AXI address range into multiple interface slave spaces. *)
    module Demultiplexer : sig
      type 'a t =
        ( Signal.t Slave_to_master.t
          , Signal.t Internal_bus.Master_to_slave.t list )
          Slave_with_data.t
      [@@deriving sexp_of]

      val create
        :  Scope.t
        -> reg_spec:Signal.Reg_spec.t
        -> address_offset:int
        -> axi_master:Signal.t Master_to_slave.t
        -> int_slaves:Signal.t Internal_bus.Slave_to_master.t list
        -> Signal.t t
    end

    (** RAM with per byte enable and configurable size *)
    module Ram_with_byte_enables : sig
      type t = Signal.t Slave_to_master.t [@@deriving sexp_of]

      val create
        :  Scope.t
        -> reg_spec:Signal.Reg_spec.t
        -> axi_master:Signal.t Master_to_slave.t
        -> size:int
        -> t
    end

    (** Register bank attached directly to the AXI bus. Under the hood, this uses
        {!Internal_bus.Register_bank} *)
    module Register_bank : sig
      type 'a t =
        (Signal.t Slave_to_master.t, Internal_bus.Register_bank.result) Slave_with_data.t
      [@@deriving sexp_of]

      val create
        :  Scope.t
        -> reg_spec:Signal.Reg_spec.t
        -> axi_master:Signal.t Master_to_slave.t
        -> write_modes:Register_mode.t list
        -> read_values:Signal.t list
        -> Signal.t t

      module With_interface (Read : Interface.S) (Write : Interface.S) : sig
        type t =
          (Signal.t Slave_to_master.t, Signal.t With_valid.t Write.t) Slave_with_data.t
        [@@deriving sexp_of]

        val write_addresses : int Write.t
        val read_addresses : int Read.t

        val create
          :  Scope.t
          -> reg_spec:Signal.Reg_spec.t
          -> axi_master:Signal.t Master_to_slave.t
          -> write_modes:Register_mode.t Write.t
          -> read_values:Signal.t Read.t
          -> t
      end
    end
  end
end
