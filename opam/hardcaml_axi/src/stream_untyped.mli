(** An untyped AXI Stream interface. See [Stream.Make_untyped] for more information. *)

open Hardcaml

module Source : sig
  type 'a t =
    { tvalid : 'a
    ; tdata : 'a
    ; tkeep : 'a
    ; tstrb : 'a
    ; tlast : 'a
    ; tuser : 'a
    }

  val get_valid : Signal.t t -> Signal.t
  val set_valid : Signal.t t -> valid:Signal.t -> Signal.t t

  include Hardcaml.Interface.Pre_partial with type 'a t := 'a t
end

module Dest : sig
  type 'a t = { tready : 'a }

  include Hardcaml.Interface.Pre_partial with type 'a t := 'a t
end
