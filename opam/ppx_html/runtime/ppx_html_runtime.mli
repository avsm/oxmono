open! Base

(** This library is ppx_html's runtime library that exposes functions that ppx_html would
    like to make the code it generates call.

    For instance, we would like to call, List.map, but since we are a ppx, List.map may be
    shadowed, so exposing List.map in a runtime library makes this resilient against
    people shadowing List.map. *)

module List : sig
  val map : 'a list -> f:local_ ('a -> 'b) -> 'b list
end
