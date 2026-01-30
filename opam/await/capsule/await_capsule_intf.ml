open! Base
open Portable_kernel
open Await_kernel
open Await_sync

module Definitions = struct
  module type Module_with_mutex = sig
    type k

    val mutex : k Mutex.t
  end

  module type Module_with_rwlock = sig
    type k

    val rwlock : k Rwlock.t
  end
end

module type Capsule = sig @@ portable
  include module type of struct
    include Capsule
  end

  include module type of struct
    include Definitions
  end

  module Mutex : sig
    type 'k t = 'k Mutex.t
    type packed = P : 'k t -> packed

    (** Creates a mutex with a fresh existential key type. *)
    val create : unit -> packed

    (** Like [create]. Useful in module definitions, where GADTs cannot be unpacked. *)
    module Create () : Module_with_mutex

    val with_lock
      : 'k 'a ('b : value_or_null).
      Await.t @ local
      -> 'k t
      -> f:('k Capsule.Access.t -> 'a @ contended portable) @ local once portable
      -> 'a @ contended portable
  end

  module With_mutex : sig
    type ('a, 'k) inner : value mod contended portable =
      { data : ('a, 'k) Capsule.Data.t
      ; mutex : 'k Mutex.t
      }

    (** An ['a Capsule.With_mutex.t] is a value of type ['a] in its own capsule, protected
        by a mutex *)
    type 'a t : value mod contended portable = P : ('a, 'k) inner -> 'a t [@@unboxed]

    (** [create f] runs [f] within a fresh capsule, and creates a [Capsule.With_mutex.t]
        containing the result *)
    val create : (unit -> 'a) @ local once portable -> 'a t

    (** [of_isolated isolated] creates a [Capsule.With_mutex.t] from a value in an
        isolated capsule, consuming the isolated capsule. *)
    val of_isolated : 'a Capsule.Isolated.t @ unique -> 'a t

    (** [with_lock t ~f] locks the mutex associated with [t] and calls [f] on the
        protected value, returning the result. *)
    val with_lock
      : 'a ('b : value_or_null).
      Await.t @ local
      -> 'a t
      -> f:('a -> 'b @ contended portable) @ local once portable
      -> 'b @ contended portable

    (** [iter t ~f] is [with_lock t ~f], specialised to a function that returns [unit] *)
    val iter : Await.t @ local -> 'a t -> f:('a -> unit) @ local once portable -> unit

    (** [map t ~f] locks the mutex associated with [t] and calls [f] on the protected
        value, returning a new [With_mutex.t] containing the result in the same capsule,
        and protected by the same mutex. *)
    val map : Await.t @ local -> 'a t -> f:('a -> 'b) @ local once portable -> 'b t

    (** [destroy t] poisons the mutex associated with [t], merging the protected value
        into the current capsule and returning it. *)
    val destroy : Await.t @ local -> 'a t -> 'a
  end

  module Rwlock : sig
    type 'k t = 'k Rwlock.t
    type packed = P : 'k t -> packed

    (** Creates a reader-writer lock with a fresh existential key type. *)
    val create : unit -> packed

    (** Like [create]. Useful in module definitions, where GADTs cannot be unpacked. *)
    module Create () : Module_with_rwlock

    val with_write
      : 'k 'a ('b : value_or_null).
      Await.t @ local
      -> 'k t
      -> f:('k Capsule.Access.t -> 'a @ contended portable) @ local once portable
      -> 'a @ contended portable

    val with_read
      : 'k 'a ('b : value_or_null).
      Await.t @ local
      -> 'k t
      -> f:('k Capsule.Access.t @ shared -> 'a @ contended portable) @ local once portable
      -> 'a @ contended portable
  end

  module With_rwlock : sig
    type ('a, 'k) inner : value mod contended portable =
      { data : ('a, 'k) Capsule.Data.t
      ; rwlock : 'k Rwlock.t
      }

    (** An ['a Capsule.With_rwlock.t] is a value of type ['a] in its own capsule,
        protected by a reader-writer lock *)
    type 'a t : value mod contended portable = P : ('a, 'k) inner -> 'a t [@@unboxed]

    (** [create f] runs [f] within a fresh capsule, and creates a [Capsule.With_rwlock.t]
        containing the result *)
    val create : (unit -> 'a) @ local once portable -> 'a t

    (** [of_isolated isolated] creates a [Capsule.With_rwlock.t] from a value in an
        isolated capsule, consuming the isolated capsule. *)
    val of_isolated : 'a Capsule.Isolated.t @ unique -> 'a t

    (** [with_write t ~f] locks the reader-writer lock associated with [t] for writing and
        calls [f] on the protected value, returning the result. *)
    val with_write
      : 'a ('b : value_or_null).
      Await.t @ local
      -> 'a t
      -> f:('a -> 'b @ contended portable) @ local once portable
      -> 'b @ contended portable

    (** [with_read t ~f] locks the reader-writer lock associated with [t] for reading and
        calls [f] on the protected value, returning the result. *)
    val with_read
      : ('a : value mod portable) ('b : value_or_null).
      Await.t @ local
      -> 'a t
      -> f:('a @ shared -> 'b @ contended portable) @ local once portable
      -> 'b @ contended portable

    (** [iter_write t ~f] is [with_write t ~f], specialised to a function that returns
        [unit] *)
    val iter_write
      :  Await.t @ local
      -> 'a t
      -> f:('a -> unit) @ local once portable
      -> unit

    (** [iter_read t ~f] is [with_read t ~f], specialised to a function that returns
        [unit] *)
    val iter_read
      : ('a : value mod portable).
      Await.t @ local -> 'a t -> f:('a @ shared -> unit) @ local once portable -> unit
  end
end
