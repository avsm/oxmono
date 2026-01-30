module Cross = struct
  module Portable = struct
    type 'a t = T

    let[@inline] cross T x = Stdlib_shim.Obj.magic_portable x
    let infer = T
    let[@inline] option T = T
    let[@inline] list T = T
    let[@inline] array T = T
    let[@inline] iarray T = T
    let[@inline] tuple2 T T = T
    let[@inline] tuple3 T T T = T
    let extension_constructor = T
    let random_state = T
    let magic = T
  end

  module Contended = struct
    type 'a t = T

    let[@inline] cross T x = Stdlib_shim.Obj.magic_uncontended x
    let infer = T
    let[@inline] option T = T
    let[@inline] list T = T
    let[@inline] iarray T = T
    let[@inline] tuple2 T T = T
    let[@inline] tuple3 T T T = T
    let extension_constructor = T
    let magic = T
  end
end

external magic_portable__needs_base_and_core
  :  'a
  -> 'a @ portable
  @@ portable
  = "%identity"

external magic_uncontended__needs_base_and_core
  :  'a @ contended
  -> 'a
  @@ portable
  = "%identity"

external magic_portable__needs_portable_functors
  :  'a
  -> 'a @ portable
  @@ portable
  = "%identity"

external magic_uncontended__promise_deeply_immutable
  :  'a @ contended
  -> 'a
  @@ portable
  = "%identity"

external magic_uncontended__promise_deeply_immutable_module
  :  'a @ contended
  -> 'a
  @@ portable
  = "%identity"
