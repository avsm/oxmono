open! Base
open! Hardcaml

module Mode = struct
  type t =
    | Toggle_low
    | Toggle_high
    | Hold
  [@@deriving sexp_of]
end

type t =
  { internal_clear : bool
  ; clear_to : int
  ; mode : Mode.t
  }
[@@deriving sexp_of, fields ~getters]

let create ?(internal_clear = false) ?(clear_to = 0) mode =
  { internal_clear; clear_to; mode }
;;

let hold = create ~internal_clear:false Hold
let hold_with_internal_clear = create ~internal_clear:true Hold
let toggle_high = create ~clear_to:(-1) Toggle_high
let toggle_low = create Toggle_low
