open Core

(** ['a list t] represents a list temporarily stored in the reverse of its logical order.

    e.g. to represent the sequence 1 then 2 we may store [2 :: 1 :: []] *)
type +'a t [@@deriving sexp_of, compare, equal]

(** The empty list.

    [to_list empty == []] *)
val empty : 'a list t

(** /O(1)/. Add an item to the logical end of the list.

    [to_list (snoc xs x) == to_list xs @ [x]] *)
val snoc : 'a list t -> 'a -> 'a list t

val singleton : 'a -> 'a list t

(** /O(n)/. Convert from a list, preserving the logical order. *)
val of_list : 'a list -> 'a list t

(** /O(n)/. Convert to a list, preserving the logical order. *)
val to_list : 'a list t -> 'a list

(** /O(1)/ Convert from a list, reversing the logical order. *)
val of_list_rev : 'a list -> 'a list t

(** /O(1)/ Convert to a list, reversing the logical order. *)
val to_list_rev : 'a list t -> 'a list

(** /O(n)/. Perform a map while converting to a list.

    /O(n)/ where /n/ is the length of [x].

    [map_to_list x ~f ~tail:y == List.map (to_list x) ~f @ y]

    The default [~tail] is [[]]. *)
val map_to_list : ?tail:'b list -> 'a list t -> f:('a -> 'b) -> 'b list

(** /O(n)/ Map and append values from a list, preserving logical order.

    /O(n)/ where /n/ is the length of [y].

    [map_append x y ~f == List.fold (List.map y ~f) ~init:x ~f:snoc] *)
val map_append : 'b list t -> 'a list -> f:('a -> 'b) -> 'b list t

include Quickcheckable.S1 with type 'a t := 'a t

module Stable : sig
  module V1 : Stable1 with type 'a t = 'a t
end
