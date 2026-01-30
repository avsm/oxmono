open! Basement

module Bench (D : sig
    type 'a t

    val make : ('a : value mod contended). 'a @ portable -> 'a t
    val get : ('a : value mod contended). 'a t -> 'a @ portable
    val set_root : ('a : value mod contended). 'a t -> 'a @ portable -> unit

    val with_temporarily
      : ('a : value mod contended) 'b.
      'a t -> 'a @ portable -> f:(unit -> 'b) @ unyielding -> 'b
  end) =
struct
  let%bench_fun "get root value" =
    let v = D.make true in
    fun () -> ignore (Sys.opaque_identity (D.get v))
  ;;

  let%bench_fun "set_root" =
    let v = D.make true in
    fun () -> Sys.opaque_identity (D.set_root v false)
  ;;

  let%bench_fun "with_temporarily and shallow get" =
    let v = D.make true in
    fun () ->
      D.with_temporarily v false ~f:(fun () -> ignore (Sys.opaque_identity (D.get v)))
  ;;

  let%bench_fun "two nested with_temporarilys and get" =
    let v1 = D.make true in
    let v2 = D.make 1 in
    fun () ->
      D.with_temporarily v1 false ~f:(fun () ->
        D.with_temporarily v2 7 ~f:(fun () ->
          ignore (Sys.opaque_identity (D.get v1, D.get v2))))
  ;;
end

module%bench Ref = Bench (struct
    type 'a t = 'a Modes.Portable.t ref

    let make a = ref { Modes.Portable.portable = a }
    let get t = !t.Modes.Portable.portable
    let set_root t a = t := { Modes.Portable.portable = a }

    let with_temporarily t a ~f =
      let restore_to = !t in
      t := { Modes.Portable.portable = a };
      match f () with
      | res ->
        t := restore_to;
        res
      | exception exn ->
        t := restore_to;
        raise exn
    ;;
  end)

module%bench Atomic = Bench (struct
    type 'a t = 'a Portable_atomic.t

    let make a = Portable_atomic.make a
    let get t = Portable_atomic.get t
    let set_root t a = Portable_atomic.set t a

    let with_temporarily t a ~f =
      let restore_to = Portable_atomic.get t in
      Portable_atomic.set t a;
      match f () with
      | res ->
        Portable_atomic.set t restore_to;
        res
      | exception exn ->
        Portable_atomic.set t restore_to;
        raise exn
    ;;
  end)

module%bench Dynamic = Bench (Dynamic)
