open Hardcaml

(** A simple statemachine that can be used to drive a AXI lite slave from a Ibus master. *)

module Make
    (Master_to_slave : Lite_ports.Master_to_slave)
    (Slave_to_master : Lite_ports.Slave_to_master)
    (Internal_bus : Internal_bus.S) : sig
  module I : sig
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; int_master : 'a Internal_bus.Master_to_slave.t
      ; axi_slave : 'a Slave_to_master.t
      }
    [@@deriving hardcaml]
  end

  module O : sig
    type 'a t =
      { int_slave : 'a Internal_bus.Slave_to_master.t
      ; axi_master : 'a Master_to_slave.t
      }
    [@@deriving hardcaml]
  end

  val hierarchical : ?name:string -> Scope.t -> Interface.Create_fn(I)(O).t
end
