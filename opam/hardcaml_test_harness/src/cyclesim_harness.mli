open Hardcaml

module Make (I : Interface.S) (O : Interface.S) : sig
  module Sim : module type of Cyclesim.With_interface (I) (O)

  (** Runs the provided simulation with input refs, output refs, and the cyclesim. *)
  val run
    : (?timeout:int
       -> create:(Scope.t -> Signal.t I.t -> Signal.t O.t)
       -> (inputs:Bits.t ref I.t -> outputs:Bits.t ref O.t -> Sim.t -> 'a)
       -> 'a)
        Harness_base.with_test_config

  (** Provides only the cyclesim as a single argument. *)
  val run_advanced
    : (?timeout:int
       -> create:(Scope.t -> Signal.t I.t -> Signal.t O.t)
       -> (Sim.t -> 'a)
       -> 'a)
        Harness_base.with_test_config
end
