include Dynamic_intf

module Fiber_dynamic : Dynamic = struct
  (* Stacks are represented as tagged pointers, so do not keep the fiber alive. We must
     not enter the GC between the creation and use of a [stack]. *)
  type (-'a, +'b) stack : immediate
  type last_fiber : immediate
  type (-'a, +'b) cont

  (** Interface to Dynamic implemented in the runtime. The runtime implementation of
      [Dynamic.set_root] sets the per-domain root value, but we want the root value to be
      global, so we wrap the runtime version in our own version with a global atomic for
      the root value. *)
  module Runtime_dynamic = struct
    type ('a : value_or_null) t : value mod contended portable

    external reperform
      :  'a Stdlib.Effect.t
      -> ('a, 'b) cont @ unique
      -> last_fiber
      -> 'b @ unique
      @@ portable
      = "%reperform"

    module Must_not_enter_gc = struct
      external alloc_stack_dyn
        : 'a 'b 'c ('d : value_or_null mod contended).
        ('a -> 'b)
        -> (exn -> 'b)
        -> ('c Effect.t -> ('c, 'b) cont @ unique -> last_fiber -> 'b @ unique)
        -> 'd t
        -> 'd @ portable
        -> ('a, 'b) stack
        @@ portable
        = "basement_alloc_stack_bind"

      external runstack
        :  ('a, 'b) stack
        -> ('c -> 'a) @ once
        -> 'c
        -> 'b @ unique
        @@ portable
        = "%runstack"

      (* Allocate a stack and immediately run [f x] on it with [d] bound to [v]. We must
         not enter the GC between [alloc_stack_dyn] and [runstack]. [with_stack_dyn] is
         marked as [@inline never] to avoid reordering. *)
      let[@inline never] with_stack_dyn valuec exnc effc d v f x =
        runstack (alloc_stack_dyn valuec exnc effc d v) f x
      ;;
    end

    external make
      : ('a : value_or_null mod contended).
      'a @ portable -> 'a t
      @@ portable
      = "basement_dynamic_make"

    external get
      : ('a : value_or_null mod contended).
      'a t -> 'a @ portable
      @@ portable
      = "basement_dynamic_get"

    let with_temporarily d v ~f =
      let effc eff k last_fiber = reperform eff k last_fiber in
      Must_not_enter_gc.with_stack_dyn (fun x -> x) (fun e -> raise e) effc d v f ()
    ;;
  end

  type 'a inner =
    { mutable root : 'a Modes.Portable.t [@atomic]
    ; current : 'a or_null Runtime_dynamic.t
    }

  type 'a t = 'a inner Modes.Contended.t

  let make a : _ t =
    { contended = { root = { portable = a }; current = Runtime_dynamic.make Null } }
  ;;

  let set_root (type a : value mod contended) (t : a t) v =
    Atomic.Loc.set [%atomic.loc t.contended.root] { portable = v }
  ;;

  let get (t : _ t) =
    match Runtime_dynamic.get t.contended.current with
    | This r -> r
    | Null -> (Atomic.Loc.get [%atomic.loc t.contended.root]).portable
  ;;

  let with_temporarily (t : _ t) v ~f =
    Runtime_dynamic.with_temporarily t.contended.current (This v) ~f
  ;;
end

external dynamic_supported : unit -> bool = "basement_dynamic_supported"

include
  (val if dynamic_supported ()
       then (module Fiber_dynamic : Dynamic)
       else (module Non_fiber_dynamic : Dynamic))
