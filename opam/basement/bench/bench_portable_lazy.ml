external opaque_identity_contended : 'a @ contended -> 'a @ contended = "%opaque"
external opaque_identity_portable : 'a @ portable -> 'a @ portable = "%opaque"

module Bench (L : sig
    type 'a t

    val from_val : 'a @ portable -> 'a t
    val from_fun : (unit -> 'a @ portable) @ portable -> 'a t
    val force : 'a t -> 'a @ contended
  end) =
struct
  let%bench_fun "construct using [from_val]" =
    let value = opaque_identity_portable [ 1 ] in
    fun () -> ignore (Sys.opaque_identity (L.from_val value))
  ;;

  let%bench_fun "construct using [from_fun]" =
    let make = opaque_identity_portable (fun () -> [ 1 ]) in
    fun () -> ignore (Sys.opaque_identity (L.from_fun make))
  ;;

  let%bench_fun "[force] when constructed via [from_val]" =
    let lz = Sys.opaque_identity (L.from_val [ 1 ]) in
    fun () -> ignore_contended (opaque_identity_contended (L.force lz))
  ;;

  let%bench_fun "[force] when constructed via [from_fun]" =
    let lz =
      Sys.opaque_identity (L.from_fun (opaque_identity_portable (fun () -> [ 1 ])))
    in
    fun () -> ignore_contended (opaque_identity_contended (L.force lz))
  ;;

  let%bench_fun "[force] when constructed via [from_fun] and already forced" =
    let lz =
      Sys.opaque_identity (L.from_fun (opaque_identity_portable (fun () -> [ 1 ])))
    in
    let f () = ignore_contended (opaque_identity_contended (L.force lz)) in
    f ();
    f
  ;;
end

module%bench Lazy = Bench (Lazy)
module%bench Portable_lazy = Bench (Basement.Portable_lazy)
