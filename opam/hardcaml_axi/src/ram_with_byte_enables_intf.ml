open Base
open Hardcaml

module type S = sig
  module Master_to_slave : Internal_bus_ports.Master_to_slave
  module Slave_to_master : Internal_bus_ports.Slave_to_master

  type t = (Signal.t Slave_to_master.t, unit) Slave_with_data.t [@@deriving sexp_of]

  val create
    :  reg_spec:Signal.Reg_spec.t
    -> master:Signal.t Master_to_slave.t
    -> size:int
    -> t
end

module type Ram_with_byte_enables = sig
  module type S = S

  module Make
      (Master_to_slave : Internal_bus_ports.Master_to_slave)
      (Slave_to_master : Internal_bus_ports.Slave_to_master) :
    S
    with module Master_to_slave := Master_to_slave
     and module Slave_to_master := Slave_to_master
end
