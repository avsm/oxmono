open Hardcaml

(** Description of a single register in the full pipeline. *)
module Pipeline_stage_descr = struct
  type t =
    { instance_name : string option
    ; clear : Signal.t
    }
end

(** The underlying register to use when constructing this pipeline. *)
module type Reg = sig
  module Master : Interface.S
  module Slave : Interface.S

  (** General config type to specify any extra parameters to be passed to the underlying
      registers. *)
  module Config : sig
    type t
  end

  val hierarchical
    :  ?instance:string
    -> config:Config.t
    -> Scope.t
    -> clock:Signal.t
    -> clear:Signal.t
    -> slave_dn:Signal.t Slave.t
    -> master_up:Signal.t Master.t
    -> Signal.t Slave.t * Signal.t Master.t
end

(** General functor to construct pipelines of protocol-level registers - i.e. for AXI
    Stream or Internal bus. *)
module M
    (Master : Interface.S)
    (Slave : Interface.S)
    (Reg : Reg with module Master := Master and module Slave := Slave) =
struct
  open Reg

  module type S = sig
    module I : sig
      type 'a t =
        { clock : 'a
        ; clear : 'a
        ; slave_dn : 'a Slave.t
        ; master_up : 'a Master.t
        }
      [@@deriving hardcaml]
    end

    module O : sig
      type 'a t =
        { slave_up : 'a Slave.t
        ; master_dn : 'a Master.t
        }
      [@@deriving hardcaml]
    end

    (** Expert pipeline function that constructs a pipeline of registers corresponding to
        the list [pipeline_stages] of [Pipeline_stage_descr.t]

        NOTE: This module does NOT use the [clear] signal that is passed in the [I.t]! It
        uses the individual [clear]s passed for each element of [pipeline_stages] *)
    val pipeline_expert
      :  config:Config.t
      -> pipeline_stages:Pipeline_stage_descr.t list
      -> Scope.t
      -> Signal.t I.t
      -> Signal.t O.t

    (** Simple pipeline function that constructs a pipeline of [n] registers that all get
        the same [Pipeline_stage_descr] (i.e. they all get the same [clear]). *)
    val pipeline_simple
      :  ?instance:string
      -> config:Config.t
      -> n:int (** The number of pipeline stages to construct. *)
      -> Scope.t
      -> Signal.t I.t
      -> Signal.t O.t
  end
end

module type Build_register_pipeline = sig
  module Pipeline_stage_descr = Pipeline_stage_descr
  module M = M

  module type Reg = Reg

  module Make
      (Master : Interface.S)
      (Slave : Interface.S)
      (Reg : Reg with module Master := Master and module Slave := Slave) :
    M(Master)(Slave)(Reg).S
end
