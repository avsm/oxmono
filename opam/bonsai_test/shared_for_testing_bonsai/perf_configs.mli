open! Core
open Bonsai_bench_scenario

module Dynamic_num : sig
  type t =
    | Let_arr
    | Map
    | Assoc_simple
    | Assoc
  [@@deriving compare, sexp_of, enumerate]

  val all_computations
    : (string * (float Opaque_map.t, float) compare_computation) list Lazy.t

  val startup_inputs : (string * float Opaque_map.t) list lazy_t
  val scenarios : (float Opaque_map.t, Nothing.t) Scenario.t list lazy_t
end

module Switch : sig
  type t =
    | Arr_then_match of
        { uses_state : bool
        ; two_inputs : bool
        }
    | Match_sub of
        { uses_state : bool
        ; two_inputs : bool
        }
  [@@deriving compare, sexp_of, enumerate]

  val all_computations : (string * (bool, string) compare_computation) list Lazy.t
  val startup_inputs : (string * bool) list
  val scenarios : (bool, Nothing.t) Scenario.t list
end
