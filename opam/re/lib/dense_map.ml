open Import

let make : ('a : value mod contended portable). size:int -> f:(int -> 'a) -> (int -> 'a) @ portable =
  fun ~size ~f ->
  let cache = Iarray.unsafe_of_array__promise_no_mutation (Array.init size f) in
  fun i -> cache.:(i)
;;
