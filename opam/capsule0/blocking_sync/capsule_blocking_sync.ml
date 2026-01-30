module Capsule = Capsule_expert

let yield = (Basement.Blocking_sync.yield [@alert "-deprecated"])

external unsafe_key : unit -> 'k Capsule.Key.t @ unique @@ portable = "%unbox_unit"

external unsafe_password
  :  unit
  -> 'k Capsule.Password.t @ local
  @@ portable
  = "%unbox_unit"

module Mutex = struct
  module M = Basement.Blocking_sync.Mutex [@alert "-deprecated"]

  type mutex = M.t
  type 'k t = M.t
  type packed = P : 'k t -> packed [@@unboxed]

  let[@inline] create _ = M.create ()

  exception Poisoned = M.Poisoned

  let[@inline] with_lock
    : type (a : value_or_null) k.
      (k t
       -> f:(k Capsule.Password.t @ local -> a @ once unique) @ local once
       -> a @ once unique) @ portable
    =
    fun t ~f ->
    M.with_lock t ~f:(fun () ->
      let pw : k Capsule.Password.t = unsafe_password () in
      f pw [@nontail])
    [@nontail]
  ;;

  let[@inline] with_key
    : type (a : value_or_null) k.
      (k t
       -> f:(k Capsule.Key.t @ unique -> #(a * k Capsule.Key.t) @ once unique)
          @ local once
       -> a @ once unique) @ portable
    =
    fun t ~f ->
    M.with_lock t ~f:(fun () ->
      let key : k Capsule.Key.t = unsafe_key () in
      let #(x, _key) = f key in
      x)
    [@nontail]
  ;;

  let[@inline] destroy t =
    M.destroy t;
    unsafe_key ()
  ;;
end

module Condition = struct
  module C = Basement.Blocking_sync.Condition [@alert "-deprecated"]

  type 'k t = C.t

  let[@inline] create () = C.create ()

  let[@inline] wait t ~mutex key =
    C.wait t ~mutex;
    key
  ;;

  let[@inline] signal t = C.signal t
  let[@inline] broadcast t = C.broadcast t
end
