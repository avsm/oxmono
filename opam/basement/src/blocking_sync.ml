external yield : unit -> unit @@ portable = "caml_thread_yield"

(* Like [Stdlib.raise], but [portable], and the value it never returns is also
   [portable unique] *)
external reraise
  : ('a : value_or_null).
  exn -> 'a @ portable unique
  @@ portable
  = "%reraise"

module M = struct
  type t : value mod contended portable

  external create : unit -> t @@ portable = "caml_blocking_mutex_new"
  external lock : t @ local -> unit @@ portable = "caml_blocking_mutex_lock"
  external unlock : t @ local -> unit @@ portable = "caml_blocking_mutex_unlock"
end

module Mutex = struct
  type t : value mod contended portable =
    { mutex : M.t
    ; mutable poisoned : bool
    }
  [@@unsafe_allow_any_mode_crossing
    "Unsafe mode crossing by design. The mutable [poisoned] field is protected by the \
     [mutex]. "]

  let[@inline] create () = { mutex = M.create (); poisoned = false }

  exception Poisoned

  let[@inline never] poison_and_reraise t ~exn =
    t.poisoned <- true;
    M.unlock t.mutex;
    reraise exn
  ;;

  let[@inline] with_lock t ~f =
    M.lock t.mutex;
    match t.poisoned with
    | true ->
      M.unlock t.mutex;
      reraise Poisoned
    | false ->
      (match f () with
       | x ->
         M.unlock t.mutex;
         x
       | exception exn -> poison_and_reraise t ~exn [@nontail])
  ;;

  let[@inline] destroy t =
    M.lock t.mutex;
    match t.poisoned with
    | true ->
      M.unlock t.mutex;
      reraise Poisoned
    | false ->
      t.poisoned <- true;
      M.unlock t.mutex
  ;;
end

module Condition = struct
  type t : value mod contended portable

  external create : unit -> t @@ portable = "caml_blocking_condition_new"
  external wait : t -> M.t -> unit @@ portable = "caml_blocking_condition_wait"
  external signal : t -> unit @@ portable = "caml_blocking_condition_signal"
  external broadcast : t -> unit @@ portable = "caml_blocking_condition_broadcast"

  let[@inline] wait t ~(mutex : Mutex.t) =
    (* Check that the mutex is not poisoned. It's safe to do so without locking: either we
       hold the [key] because it's locked, or because it's poisoned. *)
    match mutex.poisoned with
    | true -> reraise Mutex.Poisoned
    | false ->
      wait t mutex.mutex;
      (* Check that the mutex wasn't poisoned again while we were waiting. If it was, we
         can't return the key. *)
      (match mutex.poisoned with
       | true -> reraise Mutex.Poisoned
       | false -> ())
  ;;
end
