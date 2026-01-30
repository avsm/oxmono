(** Capsules are a mechanism for safely having [uncontended] access to mutable data from
    multiple threads. The interface in this module ensures that only one thread can have
    [uncontended] access to that data at a time.

    We consider every piece of mutable data in the program to live inside of some capsule.
    This might be an explicit capsule created by the user using this interface, or an
    implicit capsule created when a new thread is started. Whenever some thread is
    executing a function it has uncontended access to a single capsule and any new mutable
    data it creates is created within that capsule. We say that the function is "running
    within" the capsule.

    Capsules are only ever associated with one thread at a time, which ensures there are
    no data races in the program. The implicit capsules created when a new thread is
    started are only ever associated with that thread. Explicit capsules can change which
    threads have uncontended access to them using synchronization primitives.

    Each explicit capsule is associated with a type "brand" -- written ['k] throughout
    this interface -- which allows us to statically reason about access to the capsule
    within the type system. In the documentation of this interface we will often use ['k]
    to refer to the capsule associated with that brand.

    When you create a new capsule ['k] via [create], the library returns a unique
    ['k Key.t] that grants you exclusive ownership of that capsule. You can temporarily
    hand out a [Password.t] (or [Password.Shared.t]) to functions or threads that need to
    access the capsule. This ensures that mutable data is only ever accessed in accordance
    with the capsule's concurrency rules.

    Using a ['k Key.t] more than once turns it into an [aliased] key. Aliased global keys
    indicate that the corresponding capsule has been permanently shared across threads
    with read-only access.

    This module only provides interfaces that statically rule out data races. The [Await]
    library augments capsules with various synchronization primitives that prevent races
    at runtime.

    {1 Exceptions}

    Currently, it is possible to break the soundness guarantees of the capsule API by
    defining an exception which contains mutable state or nonportable functions, and
    "smuggling" values out of a capsule by raising that exception out of one of the
    callbacks in this module. In the medium-term, we plan to distinguish portable
    exception constructors, whose contents cross portability and contention, from
    nonportable exception constructors. In the meantime we are consciously leaving this
    soundness gap for the sake of improved ergonomics over encapsulating exceptions. *)

(** An [Access.t] allows wrapping and unwrapping [Data.t] values from the current capsule. *)
module Access : sig
  (** ['k t] represents access to the current capsule, allowing wrapping and unwrapping
      [Data.t] values. An [uncontended] ['k t] indicates that ['k] is the current capsule.
      A [shared] ['k t] indicates that ['k] is the current capsule but that it may be
      shared with other threads.

      ['k t]s captured in a [portable] closure become [contended], which prevents sharing
      them with other threads. *)
  type 'k t : void mod aliased external_ global many portable

  (** [packed] is the type of access to some unknown capsule. Unpacking one provides a
      ['k t] together with a fresh existential type brand for ['k]. *)
  type packed = P : 'k t -> packed [@@unboxed]

  (** A boxed version of [Access.t] for places where you need a type with layout [value]. *)
  type 'k boxed : value mod aliased external_ global many portable

  val box : 'k t -> 'k boxed @@ portable
  val unbox : 'k boxed -> 'k t @@ portable

  (** [equality_witness a b] returns a witness that the brands of [a] and [b] are equal.
      This must be true since they are both present uncontended within the same capsule. *)
  val equality_witness : 'k t -> 'j t -> ('k, 'j) Type.eq @@ portable
end

(** Obtain an [Access.t] for the current capsule. Since we do not know the brand for the
    current capsule, we receive a fresh one. *)
val current : unit -> Access.packed @@ portable

(** The brand for the initial capsule. *)
type initial

(** An [Access.t] for the initial capsule *)
val initial : initial Access.boxed

(** [access_initial ~f] calls [f (This initial)] if run on the initial thread, or [f Null]
    otherwise. *)
val access_initial
  :  (initial Access.boxed option @ local -> 'a @ contended local portable unique)
     @ local once portable unyielding
  -> 'a @ contended local portable unique
  @@ portable

(** [access_initial_domain ~f] calls [f (This initial)] if run on the initial domain, or
    [f Null] otherwise. *)
val access_initial_domain
  :  (initial Access.boxed option @ local -> 'a @ contended local portable unique)
     @ local once portable unyielding
  -> 'a @ contended local portable unique
  @@ portable

(** Passwords represent permission to get access to a capsule. *)
module Password : sig
  (** ['k t] is the type of "passwords" representing permission for the current thread to
      have [uncontended] access to the capsule ['k]. They are only ever available locally,
      so cannot move between threads.

      Obtaining a ['k t] requires exclusive access to ['k], either through a
      ['k Key.t @ unique] or indirectly through acquiring a mutex or rw-lock associated
      with ['k]. The mode system prevents retaining the ['k t] after releasing access to
      the capsule. This guarantees that uncontended access to the capsule is only granted
      to one thread at a time. *)
  type 'k t : void mod contended external_ portable unyielding

  (** A boxed version of [Password.t] for places where you need a type with layout
      [value]. *)
  type 'k boxed : value mod contended external_ portable unyielding

  val box : 'k t @ local -> 'k boxed @ local @@ portable
  val unbox : 'k boxed @ local -> 'k t @ local @@ portable

  (** Shared passwords represent permission to get shared access to a capsule. *)
  module Shared : sig
    (** ['k t] is the type of "shared passwords" representing permission for the current
        thread to have [shared] access to the capsule ['k]. They are only ever available
        locally, and so cannot move between threads.

        Obtaining a ['k t] requires a ['k Key.t @ aliased] or read-acquiring the
        reader-writer lock associated with ['k]. *)
    type 'k t : void mod contended external_ portable unyielding

    (** A boxed version of [Password.Shared.t] for places where you need a type with
        layout [value]. *)
    type 'k boxed : value mod contended external_ portable unyielding

    val box : 'k t @ local -> 'k boxed @ local @@ portable
    val unbox : 'k boxed @ local -> 'k t @ local @@ portable

    (** [borrow t f] calls [f] with the shared password [t] upgraded to [forkable]. The
        function [f] is itself [forkable], so cannot close over passwords. This allows the
        borrowed password to be safely closed over in parallel tasks, which receive
        temporary read-only access to the capsule.

        Note [f] cannot return the password, as the result is [global]. *)
    val borrow
      : ('a : value_or_null) 'k.
      'k t @ local
      -> ('k t @ forkable local -> 'a @ unique) @ forkable local once
      -> 'a @ unique
      @@ portable
  end

  (** [shared t] downgrades a ['k] password to a ['k] shared password. *)
  val shared : 'k t @ local -> 'k Shared.t @ local @@ portable

  (** [with_current k f] calls [f] with a password for the current capsule [k].

      Note [f] cannot return the [unforkable] password, as the result is [forkable]. *)
  val with_current
    : ('a : value_or_null) 'k.
    'k Access.t
    -> ('k t @ local -> 'a @ forkable local once unique) @ local once
    -> 'a @ forkable local once unique
    @@ portable
end

(** Keys represent the ownership of the capsule. *)
module Key : sig
  (** ['k t @ unique] represents the exclusive ownership of the capsule ['k]. The
      [unique]ness of ['k t] guarantees that only one thread can access the capsule at a
      time.

      Obtaining a unique ['k t] requires either calling [Capsule.create] or acquiring a
      synchronization primitive associated with ['k]. Such primitives are provided by the
      [Await] library.

      ['k t @ aliased] indicates that the key has been permanently shared, since it's
      avaiable [aliased] and therefore not available [unique]ly. Therefore, we can allow
      all threads read access to ['k]. *)
  type 'k t : void mod contended external_ forkable many portable unyielding

  type packed = P : 'k t -> packed [@@unboxed]

  (** A boxed version of [Key.t] for places where you need a type with layout [value]. *)
  type 'k boxed : value mod contended external_ forkable many portable

  val box : 'k t @ unique -> 'k boxed @ unique @@ portable
  val unbox : 'k boxed @ unique -> 'k t @ unique @@ portable
  val box_aliased : 'k t -> 'k boxed @@ portable
  val unbox_aliased : 'k boxed -> 'k t @@ portable

  (** [with_password k ~f] runs [f], providing it a password for ['k], and returns the
      result of [f] together with the key.

      If [f] raises an exception, the key is destroyed, leaking the contents of the
      capsule. *)
  val with_password
    : ('a : value_or_null) 'k.
    'k t @ unique
    -> f:('k Password.t @ local -> 'a @ unique) @ local once
    -> #('a * 'k t) @ unique
    @@ portable

  (** [with_password_local k ~f] runs [f], providing it a password for ['k], and returns
      the result of [f]. The key is destroyed, but the local password can be returned to
      provide access to the capsule. *)
  val with_password_local
    : ('a : value_or_null) 'k.
    'k t @ unique -> f:('k Password.t @ local -> 'a @ local) @ local once -> 'a @ local
    @@ portable

  (** [with_password_shared k ~f] runs [f], providing it a shared password for ['k], and
      returns the result of [f]. *)
  val with_password_shared
    : ('a : value_or_null) 'k.
    'k t -> f:('k Password.Shared.t @ local -> 'a @ unique) @ local once -> 'a @ unique
    @@ portable

  (** As [with_password_shared], but returns a local value. *)
  val with_password_shared_local
    : ('a : value_or_null) 'k.
    'k t -> f:('k Password.Shared.t @ local -> 'a @ local) @ local once -> 'a @ local
    @@ portable

  (** [access k ~f] runs [f], providing it access to the capsule ['k], and returns the
      result of [f] together with the key.

      If [f] raises an exception, the key is destroyed, leaking the contents of the
      capsule, and the exception is reraised. *)
  val access
    : ('a : value_or_null) 'k.
    'k t @ unique
    -> f:('k Access.t -> 'a @ contended once portable unique) @ local once portable
    -> #('a * 'k t) @ contended once portable unique
    @@ portable

  (** As [access], but local. *)
  val access_local
    : ('a : value_or_null) 'k.
    'k t @ unique
    -> f:('k Access.t -> 'a @ contended local once portable unique) @ local once portable
    -> #('a * 'k t) @ contended local once portable unique
    @@ portable

  (** [access_shared k ~f] runs [f], providing it a shared access to ['k], and returns the
      result of [f]. Exceptions raised from [f] are re-raised. *)
  val access_shared
    : ('a : value_or_null) 'k.
    'k t
    -> f:('k Access.t @ shared -> 'a @ contended once portable unique)
       @ local once portable
    -> 'a @ contended once portable unique
    @@ portable

  (** As [access_shared], but returns a local value. *)
  val access_shared_local
    : ('a : value_or_null) 'k.
    'k t
    -> f:('k Access.t @ shared -> 'a @ contended local once portable unique)
       @ local once portable
    -> 'a @ contended local once portable unique
    @@ portable

  (** [globalize_unique k] promotes a local unique key to a global one. *)
  val globalize_unique : 'k t @ local unique -> 'k t @ unique @@ portable

  (** [destroy k] returns ['k Access.t] for ['k], merging it with the current capsule. The
      key is destroyed. *)
  val destroy : 'k t @ local unique -> 'k Access.t @@ portable

  (** [unsafe_mk ()] unsafely makes a unique key for an arbitrary capsule. *)
  val unsafe_mk : unit -> 'k t @ unique @@ portable
end

(** [create ()] creates a new capsule with an associated key. *)
val create : unit -> Key.packed @ unique @@ portable

(** [access ~password ~f] runs [f] within the capsule ['k], providing it with an
    {!Access.t} for ['k]. The result is within ['k] so it must be [portable] and it is
    marked [contended]. *)
val access
  : ('a : value_or_null) 'k.
  password:'k Password.t @ local
  -> f:('k Access.t -> 'a @ contended once portable unique) @ local once portable
  -> 'a @ contended once portable unique
  @@ portable

(** As [access], but returns a local value. *)
val access_local
  : ('a : value_or_null) 'k.
  password:'k Password.t @ local
  -> f:('k Access.t -> 'a @ contended local portable unique) @ local once portable
  -> 'a @ contended local portable unique
  @@ portable

(** [shared_access ~password ~f] runs [f] within the capsule ['k], providing it with a
    shared {!Access.t} for ['k]. The result is within ['k] so it must be [portable] and it
    is marked [contended]. *)
val access_shared
  : ('a : value_or_null) 'k.
  password:'k Password.Shared.t @ local
  -> f:('k Access.t @ shared -> 'a @ contended portable) @ local once portable
  -> 'a @ contended portable
  @@ portable

(** As [access_shared], but returns a local value. *)
val access_shared_local
  : ('a : value_or_null) 'k.
  password:'k Password.Shared.t @ local
  -> f:('k Access.t @ shared -> 'a @ contended local portable) @ local once portable
  -> 'a @ contended local portable
  @@ portable

(** Pointers to data within a capsule. *)
module Data : sig
  (** [('a, 'k) t] is the type of ['a]s within the capsule ['k]. It can be passed between
      threads. Operations on [('a, 'k) t] require a ['k Password.t] associated with the
      capsule ['k]. *)
  type ('a, 'k) t : value mod everything with 'a @@ contended portable

  (** [wrap ~access v] returns a pointer to the value [v], which lives in the capsule
      ['k]. ['k] is always the current capsule. *)
  val wrap : access:'k Access.t -> 'a -> ('a, 'k) t @@ portable

  (** [unwrap ~access t] returns the value of [t], which lives in the capsule ['k]. ['k]
      is always the current capsule. *)
  val unwrap : access:'k Access.t -> ('a, 'k) t -> 'a @@ portable

  (** Like [wrap], but for unique values. *)
  val wrap_unique : access:'k Access.t -> 'a @ unique -> ('a, 'k) t @ unique @@ portable

  (** Like [unwrap], but for unique values. *)
  val unwrap_unique : access:'k Access.t -> ('a, 'k) t @ unique -> 'a @ unique @@ portable

  (** Like [wrap], but for once values. *)
  val wrap_once : access:'k Access.t -> 'a @ once -> ('a, 'k) t @ once @@ portable

  (** Like [unwrap], but for once values. *)
  val unwrap_once : access:'k Access.t -> ('a, 'k) t @ once -> 'a @ once @@ portable

  (** Like [wrap], but for once unique values. *)
  val wrap_once_unique
    :  access:'k Access.t
    -> 'a @ once unique
    -> ('a, 'k) t @ once unique
    @@ portable

  (** Like [unwrap], but for once unique values. *)
  val unwrap_once_unique
    :  access:'k Access.t
    -> ('a, 'k) t @ once unique
    -> 'a @ once unique
    @@ portable

  (** [unwrap_shared ~access t] returns the shared value of [t], which lives in the
      capsule ['k]. ['k] is always the current capsule. Since ['a] may have been shared
      with other threads, ['a] must cross portability. *)
  val unwrap_shared
    : ('a : value mod portable) 'k.
    access:'k Access.t @ shared -> ('a, 'k) t -> 'a @ shared
    @@ portable

  (** [create f] runs [f] within the capsule ['k] and returns a pointer to the result of
      [f]. *)
  val create : (unit -> 'a) @ local once portable -> ('a, 'k) t @@ portable

  (** Like [create], but for once values. *)
  val create_once
    :  (unit -> 'a @ once) @ local once portable
    -> ('a, 'k) t @ once
    @@ portable

  (** [create_unique f] runs [f] within the capsule ['k] and returns a pointer to the
      result of [f]. *)
  val create_unique
    :  (unit -> 'a @ unique) @ local once portable
    -> ('a, 'k) t @ unique
    @@ portable

  (** [map ~password ~f t] applies [f] to the value of [p] within the capsule ['k] and
      returns a pointer to the result. *)
  val map
    :  password:'k Password.t @ local
    -> f:('a -> 'b) @ local once portable
    -> ('a, 'k) t
    -> ('b, 'k) t
    @@ portable

  (** [both t1 t2] is a pointer to a pair of the values of [t1] and [t2]. *)
  val both : ('a, 'k) t -> ('b, 'k) t -> ('a * 'b, 'k) t @@ portable

  (** [fst t] gives a pointer to the first value inside [t] *)
  val fst : ('a * 'b, 'k) t -> ('a, 'k) t @@ portable

  (** [snd t] gives a pointer to the second value inside [t] *)
  val snd : ('a * 'b, 'k) t -> ('b, 'k) t @@ portable

  (** [extract ~password ~f t] applies [f] to the value of [t] within the capsule ['k] and
      returns the result. The result is within ['k] so must be [portable] and is marked
      [contended]. *)
  val extract
    :  password:'k Password.t @ local
    -> f:('a -> 'b @ contended once portable unique) @ local once portable
    -> ('a, 'k) t
    -> 'b @ contended once portable unique
    @@ portable

  (** [inject v] creates a pointer to a value [v] injected into the capsule ['k]. It's a
      specialization of [create] to values that are always [uncontended]. *)
  val inject : ('a : value mod contended) 'k. 'a @ portable -> ('a, 'k) t @@ portable

  (** [project t] returns the value of [t]. The result is within ['k], so is marked
      [contended]. The value is required to always be [portable], so unlike [extract],
      [project] does not require permission to access ['k]. This is safe because all
      accesses to the value happen only after it's marked [contended]. *)
  val project : ('a : value mod portable) 'k. ('a, 'k) t -> 'a @ contended @@ portable

  (** [project_shared ~key t] is like [project t], but since [t] is a capsule associated
      with a [key @ aliased global], the contents can be returned at [shared]. *)
  val project_shared
    : ('a : value mod portable) 'k.
    key:'k Key.t -> ('a, 'k) t -> 'a @ shared
    @@ portable

  val project_shared_unique
    : ('a : value mod portable) 'k.
    key:'k Key.t -> ('a, 'k) t @ unique -> 'a @ shared unique
    @@ portable

  (** [bind ~password ~f t] is [project (map ~password ~f t)]. *)
  val bind
    :  password:'k Password.t @ local
    -> f:('a -> ('b, 'j) t) @ local once portable
    -> ('a, 'k) t
    -> ('b, 'j) t
    @@ portable

  (** [iter] is [extract] with result type specialized to [unit]. *)
  val iter
    :  password:'k Password.t @ local
    -> f:('a -> unit) @ local once portable
    -> ('a, 'k) t
    -> unit
    @@ portable

  (** [map_shared ~password ~f t] applies [f] to the shared parts of [t] within the
      capsule ['k] and returns a pointer to the result. Since ['a] may have been shared
      with other threads, ['a] must cross portability. *)
  val map_shared
    : ('a : value mod portable) 'b 'k.
    password:'k Password.Shared.t @ local
    -> f:('a @ shared -> 'b) @ local once portable
    -> ('a, 'k) t
    -> ('b, 'k) t
    @@ portable

  (** [extract_shared ~password ~f t] applies [f] to the shared parts of [t] within the
      capsule ['k] and returns the result. The result is within ['k] so must be portable
      and is marked contended. Since ['a] may have been shared with other threads, ['a]
      must cross portability. *)
  val extract_shared
    : ('a : value mod portable) 'b 'k.
    password:'k Password.Shared.t @ local
    -> f:('a @ shared -> 'b @ contended once portable unique) @ local once portable
    -> ('a, 'k) t
    -> 'b @ contended once portable unique
    @@ portable

  module Shared : sig
    type ('a, 'k) data = ('a, 'k) t

    (** [('a, 'k) t] is the type of ['a]s in a "sub-capsule" of ['k] that has a [shared]
        view of ['k] and [uncontended] access to the sub-capsule enclosing ['a].

        Both read and write operations only require ['k Access.t @ shared]. However,
        unlike [('a, 'k) Data.t], [('a, 'k) Data.Shared.t] does not cross contention. *)
    type ('a, 'k) t : value mod portable

    (** [wrap ~access v] returns a pointer to the value [v], which lives in the
        sub-capsule of ['k]. ['k] is always the current capsule. *)
    val wrap : access:'k Access.t @ shared -> 'a -> ('a, 'k) t @@ portable

    (** [unwrap ~access t] returns the value of [t], which lives in the sub-capsule of
        ['k]. ['k] is always the current capsule. *)
    val unwrap : access:'k Access.t @ shared -> ('a, 'k) t -> 'a @@ portable

    (** A ['k Key.t @ global aliased] indicates that all capsules have permanent read-only
        access to ['k]. Therefore, [('a, 'k) t] can be safely [expose]d. *)
    val expose : key:'k Key.t -> ('a, 'k) t -> 'a @@ portable

    (** [create f] runs [f] within the sub-capsule of ['k] and returns a pointer to the
        result. *)
    val create : (unit -> 'a) @ local once portable -> ('a, 'k) t @@ portable

    (** [map ~password ~f t] applies [f] to the value of [p] within the sub-capsule of
        ['k] and returns a pointer to the result. *)
    val map
      :  password:'k Password.Shared.t @ local
      -> f:('a -> 'b) @ local once portable
      -> ('a, 'k) t
      -> ('b, 'k) t
      @@ portable

    (** [both t1 t2] is a pointer to a pair of the values of [t1] and [t2]. *)
    val both : ('a, 'k) t -> ('b, 'k) t -> ('a * 'b, 'k) t @@ portable

    (** [fst t] gives a pointer to the first value inside [t] *)
    val fst : ('a * 'b, 'k) t -> ('a, 'k) t @@ portable

    (** [snd t] gives a pointer to the second value inside [t] *)
    val snd : ('a * 'b, 'k) t -> ('b, 'k) t @@ portable

    (** [extract ~password ~f t] applies [f] to the value of [t] within the sub-capsule of
        ['k] and returns the result. The result has access to ['k] so must be [portable]
        and is marked [contended]. *)
    val extract
      :  password:'k Password.Shared.t @ local
      -> f:('a -> 'b @ contended once portable unique) @ local once portable
      -> ('a, 'k) t
      -> 'b @ contended once portable unique
      @@ portable

    (** [inject v] is a pointer to an value [v] injected into the capsule ['k]. It's a
        specialization of [create] to values that are always [uncontended]. *)
    val inject : ('a : value mod contended) 'k. 'a @ portable -> ('a, 'k) t @@ portable

    (** [project t] returns the value of [t]. The result is within ['k], so must be
        [portable] and is marked [contended]. Since it's always [portable], unlike with
        [extract], we don't need exclusive access to ['k]: all accesses to the value
        happen only after it's marked [contended]. *)
    val project : ('a : value mod portable) 'k. ('a, 'k) t -> 'a @ contended @@ portable

    (** [bind ~password ~f t] is [project (map ~password ~f t)]. *)
    val bind
      :  password:'k Password.Shared.t @ local
      -> f:('a -> ('b, 'j) t) @ local once portable
      -> ('a, 'k) t
      -> ('b, 'j) t
      @@ portable

    (** [iter] is [extract] with result type specialized to [unit]. *)
    val iter
      :  password:'k Password.Shared.t @ local
      -> f:('a -> unit) @ local once portable
      -> ('a, 'k) t
      -> unit
      @@ portable

    (** [map_into] is like [Capsule.map_shared] but returns a [Shared.t]. *)
    val map_into
      : ('a : value mod portable) 'b 'k.
      password:'k Password.Shared.t @ local
      -> f:('a @ shared -> 'b) @ local once portable
      -> ('a, 'k) data
      -> ('b, 'k) t
      @@ portable

    (** Functions to work with [('a, 'k) t @ local]. *)
    module Local : sig
      (** [wrap ~access v] returns a pointer to the local value [v], which lives in the
          sub-capsule of ['k]. ['k] is always the current capsule. *)
      val wrap
        :  access:'k Access.t @ shared
        -> 'a @ local
        -> ('a, 'k) t @ local
        @@ portable

      (** [unwrap ~access t] returns the value of [t], which lives in the sub-capsule of
          ['k]. ['k] is always the current capsule. *)
      val unwrap
        :  access:'k Access.t @ shared
        -> ('a, 'k) t @ local
        -> 'a @ local
        @@ portable

      (** [create f] runs [f] within the sub-capsule of ['k] and returns a local pointer
          to the result. *)
      val create
        :  (unit -> 'a @ local) @ local once portable
        -> ('a, 'k) t @ local
        @@ portable

      (** [map ~pasword ~f t] applies [f] to the value of [p] within the sub-capsule of
          ['k] and returns a local pointer to the result. *)
      val map
        :  password:'k Password.Shared.t @ local
        -> f:('a @ local -> 'b @ local) @ local once portable
        -> ('a, 'k) t @ local
        -> ('b, 'k) t @ local
        @@ portable

      (** [both t1 t2] is a pointer to a pair of the values of [t1] and [t2]. *)
      val both
        :  ('a, 'k) t @ local
        -> ('b, 'k) t @ local
        -> ('a * 'b, 'k) t @ local
        @@ portable

      (** [fst t] gives a pointer to the first value inside [t] *)
      val fst : ('a * 'b, 'k) t @ local -> ('a, 'k) t @ local @@ portable

      (** [snd t] gives a pointer to the second value inside [t] *)
      val snd : ('a * 'b, 'k) t @ local -> ('b, 'k) t @ local @@ portable

      (** [extract ~pasword ~f t] applies [f] to the value of [t] within the sub-capsule
          of ['k] and returns the result. The result has access to ['k] so must be
          [portable] and is marked [contended]. *)
      val extract
        :  password:'k Password.Shared.t @ local
        -> f:('a @ local -> 'b @ contended local once portable unique)
           @ local once portable
        -> ('a, 'k) t @ local
        -> 'b @ contended local once portable unique
        @@ portable

      (** [inject v] is a pointer to an value [v] injected into the capsule ['k]. It's a
          specialization of [create] to values that are always [uncontended]. *)
      val inject
        : ('a : value mod contended) 'k.
        'a @ local portable -> ('a, 'k) t @ local
        @@ portable

      (** [project t] returns the value of [t]. The result is within ['k], so must be
          [portable] and is marked [contended]. Since it's always [portable], unlike with
          [extract], we don't need exclusive access to ['k]: all accesses to the value
          happen only after it's marked [contended]. *)
      val project
        : ('a : value mod portable) 'k.
        ('a, 'k) t @ local -> 'a @ contended local
        @@ portable

      (** [bind ~password ~f t] is [project (map ~password ~f t)]. *)
      val bind
        :  password:'k Password.Shared.t @ local
        -> f:('a @ local -> ('b, 'j) t @ local) @ local once portable
        -> ('a, 'k) t @ local
        -> ('b, 'j) t @ local
        @@ portable

      (** [iter] is [extract] with result type specialized to [unit]. *)
      val iter
        :  password:'k Password.Shared.t @ local
        -> f:('a @ local -> unit) @ local once portable
        -> ('a, 'k) t @ local
        -> unit
        @@ portable

      (** [map_into] is like [Capsule.map_shared] but returns a [Shared.t]. *)
      val map_into
        : ('a : value mod portable) 'b 'k.
        password:'k Password.Shared.t @ local
        -> f:('a @ local shared -> 'b @ local) @ local once portable
        -> ('a, 'k) data @ local
        -> ('b, 'k) t @ local
        @@ portable
    end
  end

  (** Functions to work with [('a, 'k) t @ local]. *)
  module Local : sig
    (** [wrap ~access v] returns a pointer to the local value [v], which lives in the
        capsule ['k]. ['k] is always the current capsule. *)
    val wrap : access:'k Access.t -> 'a @ local -> ('a, 'k) t @ local @@ portable

    (** [unwrap ~access t] returns the value of [t], which lives in the capsule ['k]. ['k]
        is always the current capsule. *)
    val unwrap : access:'k Access.t -> ('a, 'k) t @ local -> 'a @ local @@ portable

    (** Like [wrap], but for unique values. *)
    val wrap_unique
      :  access:'k Access.t
      -> 'a @ local unique
      -> ('a, 'k) t @ local unique
      @@ portable

    (** Like [unwrap], but for unique values. *)
    val unwrap_unique
      :  access:'k Access.t
      -> ('a, 'k) t @ local unique
      -> 'a @ local unique
      @@ portable

    (** Like [wrap], but for once values. *)
    val wrap_once
      :  access:'k Access.t
      -> 'a @ local once
      -> ('a, 'k) t @ local once
      @@ portable

    (** Like [unwrap], but for once values. *)
    val unwrap_once
      :  access:'k Access.t
      -> ('a, 'k) t @ local once
      -> 'a @ local once
      @@ portable

    (** [unwrap_shared ~access t] returns the shared value of [t], which lives in the
        capsule ['k]. ['k] is always the current capsule. Since ['a] may have been shared
        with other threads, ['a] must cross portability. *)
    val unwrap_shared
      : ('a : value mod portable) 'k.
      access:'k Access.t @ shared -> ('a, 'k) t @ local -> 'a @ local shared
      @@ portable

    (** [create f] runs [f] within the capsule ['k] and returns a local pointer to the
        result of [f]. *)
    val create
      :  (unit -> 'a @ local) @ local once portable
      -> ('a, 'k) t @ local
      @@ portable

    (** [map ~password ~f t] applies [f] to the value of [p] within the capsule ['k] and
        returns a local pointer to the result. *)
    val map
      :  password:'k Password.t @ local
      -> f:('a @ local -> 'b @ local) @ local once portable
      -> ('a, 'k) t @ local
      -> ('b, 'k) t @ local
      @@ portable

    (** [both t1 t2] is a pointer to a pair of the values of [t1] and [t2]. *)
    val both
      :  ('a, 'k) t @ local
      -> ('b, 'k) t @ local
      -> ('a * 'b, 'k) t @ local
      @@ portable

    (** [fst t] gives a pointer to the first value inside [t] *)
    val fst : ('a * 'b, 'k) t @ local -> ('a, 'k) t @ local @@ portable

    (** [snd t] gives a pointer to the second value inside [t] *)
    val snd : ('a * 'b, 'k) t @ local -> ('b, 'k) t @ local @@ portable

    (** [extract ~password ~f t] applies [f] to the value of [t] within the capsule ['k]
        and returns the result. The result is within ['k] so must be [portable] and is
        marked [contended]. *)
    val extract
      :  password:'k Password.t @ local
      -> f:('a @ local -> 'b @ contended local once portable unique) @ local once portable
      -> ('a, 'k) t @ local
      -> 'b @ contended local once portable unique
      @@ portable

    (** [inject v] is a pointer to an value [v] injected into the capsule ['k]. It's a
        specialization of [create] to values that are always [uncontended]. *)
    val inject
      : ('a : value mod contended) 'k.
      'a @ local portable -> ('a, 'k) t @ local
      @@ portable

    (** [project t] returns the value of [t]. The result is within ['k], so must be
        [portable] and is marked [contended]. Since it's always [portable], unlike with
        [extract], we don't need exclusive access to ['k]: all accesses to the value
        happen only after it's marked [contended]. *)
    val project
      : ('a : value mod portable) 'k.
      ('a, 'k) t @ local -> 'a @ contended local
      @@ portable

    (** [project_shared ~key t] is like [project t], but since [t] is a capsule associated
        with a [key @ aliased global], the contents can be returned at [shared]. *)
    val project_shared
      : ('a : value mod portable) 'k.
      key:'k Key.t @ local -> ('a, 'k) t @ local -> 'a @ local shared
      @@ portable

    (** [bind ~password ~f t] is [project (map ~password ~f t)]. *)
    val bind
      :  password:'k Password.t @ local
      -> f:('a @ local -> ('b, 'j) t @ local) @ local once portable
      -> ('a, 'k) t @ local
      -> ('b, 'j) t @ local
      @@ portable

    (** [iter] is [extract] with result type specialized to [unit]. *)
    val iter
      :  password:'k Password.t @ local
      -> f:('a @ local -> unit) @ local once portable
      -> ('a, 'k) t @ local
      -> unit
      @@ portable

    (** [map_shared ~password ~f t] applies [f] to the shared parts of [t] within the
        capsule ['k] and returns a pointer to the result. Since ['a] may have been shared
        with other threads, ['a] must cross portability. *)
    val map_shared
      : ('a : value mod portable) 'b 'k.
      password:'k Password.Shared.t @ local
      -> f:('a @ local shared -> 'b @ local) @ local once portable
      -> ('a, 'k) t @ local
      -> ('b, 'k) t @ local
      @@ portable

    (** [extract_shared ~password ~f t] applies [f] to the shared parts of [t] within the
        capsule ['k] and returns the result. The result is within ['k] so must be portable
        and is marked contended. Since ['a] may have been shared with other threads, ['a]
        must cross portability. *)
    val extract_shared
      : ('a : value mod portable) 'b 'k.
      password:'k Password.Shared.t @ local
      -> f:('a @ local shared -> 'b @ contended local once portable unique)
         @ local once portable
      -> ('a, 'k) t @ local
      -> 'b @ contended local once portable unique
      @@ portable
  end

  module Or_null : sig
    type ('a : value_or_null
         , 'k)
         t :
         value_or_null mod everything with 'a @@ contended portable

    (** [wrap ~access v] returns a pointer to the value [v], which lives in the capsule
        ['k]. ['k] is always the current capsule. *)
    val wrap : ('a : value_or_null) 'k. access:'k Access.t -> 'a -> ('a, 'k) t @@ portable

    (** [unwrap ~access t] returns the value of [t], which lives in the capsule ['k]. ['k]
        is always the current capsule. *)
    val unwrap
      : ('a : value_or_null) 'k.
      access:'k Access.t -> ('a, 'k) t -> 'a
      @@ portable

    (** [create f] runs [f] within the capsule ['k] and returns a pointer to the result of
        [f]. *)
    val create
      : ('a : value_or_null) 'k.
      (unit -> 'a) @ local once portable -> ('a, 'k) t
      @@ portable

    (** [project t] returns the value of [t]. The result is within ['k], so is marked
        [contended]. The value is required to always be [portable], so unlike [extract],
        [project] does not require permission to access ['k]. This is safe because all
        accesses to the value happen only after it's marked [contended]. *)
    val project
      : ('a : value_or_null mod portable) 'k.
      ('a, 'k) t -> 'a @ contended
      @@ portable
  end
end
