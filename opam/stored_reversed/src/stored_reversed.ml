module Stable = struct
  module V1 = struct
    type +'a t = 'a [@@deriving sexp, bin_io, compare, equal]

    let map x ~f = f x
  end
end

open Core
include Stable.V1

let empty = []
let snoc xs x = x :: xs
let singleton x = [ x ]
let of_list_rev x = x
let to_list_rev x = x
let of_list = List.rev
let to_list = List.rev
let map_to_list ?(tail = []) head ~f = List.rev_map_append head ~f tail
let map_append tail head ~f = List.rev_map_append head ~f tail

include struct
  open Quickcheck

  let quickcheck_generator : 'a Generator.t -> 'a t Generator.t = Fn.id
  let quickcheck_observer : 'a Observer.t -> 'a t Observer.t = Fn.id
  let quickcheck_shrinker : 'a Shrinker.t -> 'a t Shrinker.t = Fn.id
end
