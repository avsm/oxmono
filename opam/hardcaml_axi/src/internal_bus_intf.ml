open! Base
open! Hardcaml

module type S = sig
  (** A simpler master to slave interface decoded from AXI requests. *)
  module Master_to_slave : Internal_bus_ports.Master_to_slave

  (** A simpler slave to master interface to be encoded to AXI response. *)
  module Slave_to_master : Internal_bus_ports.Slave_to_master

  (** Demultiplex one master across one or more slaves. *)
  module Demultiplexer : sig
    (** @inline *)
    include
      Demultiplexer.S
      with module Master_to_slave := Master_to_slave
       and module Slave_to_master := Slave_to_master
  end

  (** RAM with per byte enable and configurable size. *)
  module Ram_with_byte_enables : sig
    (** @inline *)
    include
      Ram_with_byte_enables.S
      with module Master_to_slave := Master_to_slave
       and module Slave_to_master := Slave_to_master
  end

  (** Bank of read/write registers connected to a Master interface. *)
  module Register_bank : sig
    (** @inline *)
    include
      Register_bank.S
      with module Master_to_slave := Master_to_slave
       and module Slave_to_master := Slave_to_master
  end

  (** A simple state machine to register the datapath of an Ibus. *)
  module Register :
      module type of Internal_bus_register.Make (Master_to_slave) (Slave_to_master)
end

module type Internal_bus = sig
  module type S = S

  module Make (X : Master_slave_bus_config.S) : S

  (** With 32 bit data and address bus *)
  module W32 : S
end
