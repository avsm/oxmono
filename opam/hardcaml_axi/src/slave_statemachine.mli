open Base
open Hardcaml

module Make
    (Master_to_slave : Lite_ports.Master_to_slave)
    (Slave_to_master : Lite_ports.Slave_to_master)
    (Internal_bus : Internal_bus.S) : sig
  module I : sig
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; axi_master : 'a Master_to_slave.t
      ; int_slave : 'a Internal_bus.Slave_to_master.t
      }
    [@@deriving hardcaml]
  end

  module O : sig
    type 'a t =
      { axi_slave : 'a Slave_to_master.t
      ; int_master : 'a Internal_bus.Master_to_slave.t
      }
    [@@deriving hardcaml]
  end

  val create : Scope.t -> Interface.Create_fn(I)(O).t
  val hierarchical : Scope.t -> Interface.Create_fn(I)(O).t

  val with_slave_statemachine
    :  ?hierarchical_instance:bool (** Default is [false] *)
    -> Scope.t
    -> reg_spec:Signal.Reg_spec.t
    -> axi_master:Signal.t Master_to_slave.t
    -> create_fn:
         (Signal.t Internal_bus.Master_to_slave.t
          -> (Signal.t Internal_bus.Slave_to_master.t, 'return_value) Slave_with_data.t)
    -> (Signal.t Slave_to_master.t, 'return_value) Slave_with_data.t

  module State : sig
    type t [@@deriving sexp_of]

    val of_int_exn : int -> t
    val to_string : t -> string
  end
end
