module type Dynamic = sig @@ portable
  (** This module is reexported, with documentation, as [Base.Dynamic]; see that module
      for documentation on this interface. *)

  type 'a t : value mod contended portable

  val make : ('a : value mod contended). 'a @ portable -> 'a t
  val get : ('a : value mod contended). 'a t -> 'a @ portable
  val set_root : ('a : value mod contended). 'a t -> 'a @ portable -> unit

  val with_temporarily
    : ('a : value mod contended) 'b.
    'a t -> 'a @ portable -> f:(unit -> 'b) @ once -> 'b
end
