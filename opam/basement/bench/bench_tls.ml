open! Basement

module Bench (Storage : sig
    type 'a t

    val new_key : (unit -> 'a) @ portable -> 'a t
    val get : ('a : value mod portable). 'a t -> 'a @ contended
    val set : ('a : value mod contended). 'a t -> 'a @ portable -> unit @ portable
  end) =
struct
  let key = Storage.new_key (fun () -> ())
  let%bench "get" = ignore (Sys.opaque_identity (Storage.get key))
  let%bench "set" = ignore (Sys.opaque_identity (Storage.set key ()))
end

module%bench DLS = Bench (struct
    type 'a t = 'a Domain.Safe.DLS.key

    let new_key a = Domain.Safe.DLS.new_key a
    let get = Domain.Safe.DLS.get
    let set = Domain.Safe.DLS.set
  end)

module%bench TLS = Bench (struct
    type 'a t = 'a Domain.Safe.TLS.key

    let new_key a = Domain.Safe.TLS.new_key a
    let get = Domain.Safe.TLS.get
    let set = Domain.Safe.TLS.set
  end)
