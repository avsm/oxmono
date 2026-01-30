(** Jane Street extensions add some new functions to the OCaml standard library. To remain
    compatible with upstream OCaml, we swap out their implementation when releasing the
    open-source version of our libraries.

    This file provides the common interface between the two different implementations. *)

(** Detect whether we are using the OCaml 5 runtime. *)
external runtime5 : unit -> bool @@ portable = "%runtime5"

(** Like {!ignore}, but takes a [contended] value. This is technically strictly stronger
    than [ignore], but changing [ignore] in place causes backwards compatibility issues
    due to type inference. *)
external ignore_contended
  : ('a : value_or_null).
  'a @ contended -> unit
  @@ portable
  = "%ignore"

external raise : exn -> 'a @ portable unique @@ portable = "%reraise"
external raise_notrace : exn -> 'a @ portable unique @@ portable = "%raise_notrace"
val failwith : string -> 'a @ portable unique @@ portable

module Atomic : sig @@ portable
  type ('a : value_or_null) t := 'a Stdlib.Atomic.t

  external get_contended
    : ('a : value_or_null).
    'a t @ contended local -> 'a @ contended
    = "%atomic_load"

  module Local : sig
    external make : 'a -> ('a t[@local_opt]) = "%makemutable"
    external make_contended : 'a -> ('a t[@local_opt]) = "caml_atomic_make_contended"
    external get : 'a t @ local -> 'a = "%atomic_load"
    external set : 'a t @ local -> 'a -> unit = "%atomic_set"
    external exchange : 'a t @ local -> 'a -> 'a = "%atomic_exchange"
    external compare_and_set : 'a t @ local -> 'a -> 'a -> bool = "%atomic_cas"

    external compare_exchange
      :  'a t @ local
      -> 'a
      -> 'a
      -> 'a
      = "%atomic_compare_exchange"

    external fetch_and_add : int t @ contended local -> int -> int = "%atomic_fetch_add"
    external add : int t @ contended local -> int -> unit = "%atomic_add"
    external sub : int t @ contended local -> int -> unit = "%atomic_sub"
    external logand : int t @ contended local -> int -> unit = "%atomic_land"
    external logor : int t @ contended local -> int -> unit = "%atomic_lor"
    external logxor : int t @ contended local -> int -> unit = "%atomic_lxor"
    val incr : int t @ contended local -> unit
    val decr : int t @ contended local -> unit
  end

  module Expert : sig
    external fenceless_get : 'a t @ local -> 'a = "%field0"
    external fenceless_set : 'a t @ local -> 'a -> unit = "%setfield0"

    module Contended : sig
      external fenceless_get
        : ('a : value mod contended).
        'a t @ contended local -> 'a
        = "%field0"

      external fenceless_set
        : ('a : value mod portable).
        'a t @ contended local -> 'a -> unit
        = "%setfield0"
    end
  end
end

module Backoff : sig @@ portable
  (** Type of backoff values. *)
  type t : immediate

  (** Logarithm of the maximum allowed value for wait. *)
  val max_wait_log : int

  (** [create] creates a backoff value. [upper_wait_log], [lower_wait_log] override the
      logarithmic upper and lower bound on the number of spins executed by {!once}. *)
  val create : ?lower_wait_log:int -> ?upper_wait_log:int -> unit -> t

  (** [default] is equivalent to [create ()]. *)
  val default : t

  (** [once b] executes one random wait and returns a new backoff with logarithm of the
      current maximum value incremented unless it is already at [upper_wait_log] of [b].

      Note that this uses the default Stdlib [Random] per-domain generator. *)
  val once : t -> t

  (** [reset b] returns a backoff equivalent to [b] except with current value set to the
      [lower_wait_log] of [b]. *)
  val reset : t -> t

  (** [get_wait_log b] returns logarithm of the maximum value of wait for next {!once}. *)
  val get_wait_log : t -> int
end

module Callback : sig
  module Safe : sig
    (** Like {!register}, but is safe to use in the presence of multiple domains. The
        provided value must be [portable] as registered values may be looked up from any
        domain. *)
    val register : string -> 'a @ portable -> unit @@ portable

    (** Like {!register_exception}, but is safe to use in the presence of multiple
        domains. The provided exception must be [portable] as registered exceptions may be
        looked up from any domain. *)
    val register_exception : string -> exn @ portable -> unit @@ portable
  end
end

module Domain : sig
  (*_ Do not use a default modality for [Domain]; it uses unsafe implementations
      internally, so each item should be considered and marked individually. *)

  type id = private int

  (** If busy-waiting, calling cpu_relax () between iterations will improve performance on
      some CPU architectures. When poll insertion is disabled, this is a polling point. *)
  val cpu_relax : unit -> unit @@ portable

  val self : unit -> id @@ portable

  module Safe : sig
    module DLS : sig
      type 'a key : value mod contended portable = 'a Domain.DLS.key

      val new_key
        :  ?split_from_parent:('a -> (unit -> 'a) @ once portable) @ portable
        -> (unit -> 'a) @ portable
        -> 'a key
        @@ portable

      val get : ('a : value mod portable). 'a key -> 'a @ contended @@ portable
      val set : ('a : value mod contended). 'a key -> 'a @ portable -> unit @@ portable
    end

    module TLS : sig
      type 'a key : value mod contended portable

      val new_key
        :  ?split_from_parent:('a -> (unit -> 'a) @ once portable) @ portable
        -> (unit -> 'a) @ portable
        -> 'a key
        @@ portable

      val get : ('a : value mod portable). 'a key -> 'a @ contended @@ portable
      val set : ('a : value mod contended). 'a key -> 'a @ portable -> unit @@ portable
    end

    val at_exit : (unit -> unit) @ portable -> unit @@ portable
  end
end

module Ephemeron : sig @@ portable
  module K1 : sig
    module MakePortable (H : sig
      @@ portable
        include Hashtbl.HashedType
      end) : sig
      @@ portable
      include Ephemeron.S with type key = H.t
    end

    module MakeSeededPortable (H : sig
      @@ portable
        include Hashtbl.SeededHashedType
      end) : sig
      @@ portable
      include Ephemeron.SeededS with type key = H.t
    end
  end

  module K2 : sig
    module MakePortable
        (H1 : sig
         @@ portable
           include Hashtbl.HashedType
         end)
        (H2 : sig
         @@ portable
           include Hashtbl.HashedType
         end) : sig
      @@ portable
      include Ephemeron.S with type key = H1.t * H2.t
    end

    module MakeSeededPortable
        (H1 : sig
         @@ portable
           include Hashtbl.SeededHashedType
         end)
        (H2 : sig
         @@ portable
           include Hashtbl.SeededHashedType
         end) : sig
      @@ portable
      include Ephemeron.SeededS with type key = H1.t * H2.t
    end
  end

  module Kn : sig
    module MakePortable (H : sig
      @@ portable
        include Hashtbl.HashedType
      end) : sig
      @@ portable
      include Ephemeron.S with type key = H.t array
    end

    module MakeSeededPortable (H : sig
      @@ portable
        include Hashtbl.SeededHashedType
      end) : sig
      @@ portable
      include Ephemeron.SeededS with type key = H.t array
    end
  end
end

module Format : sig @@ portable
  type formatter := Format.formatter

  (** Submodule containing non-backwards-compatible functions which enforce thread safety
      via modes. *)
  module Safe : sig
    (** Like {!make_synchronized_formatter}, but can be called from any domain.

        The provided closures must be [portable] as they will be called from other domains
        that access the returned [Domain.Safe.DLS.key]. *)
    val make_synchronized_formatter
      :  (string -> int -> int -> unit) @ portable
      -> (unit -> unit) @ portable
      -> formatter Domain.Safe.DLS.key
  end
end

module Hashtbl : sig @@ portable
  (** Like [Make], but takes a portable [hash] function to portable [Hashtbl] operations. *)
  module MakePortable (H : sig
    @@ portable
      include Hashtbl.HashedType
    end) : sig
    @@ portable
    include Hashtbl.S with type key = H.t
  end

  (** Like [MakeSeeded], but takes a portable [seeded_hash] function to portable [Hashtbl]
      operations. *)
  module MakeSeededPortable (H : sig
    @@ portable
      include Hashtbl.SeededHashedType
    end) : sig
    @@ portable
    include Hashtbl.SeededS with type key = H.t
  end
end

module Map : sig @@ portable
  (** Like [Make], but takes a portable [compare] function to portable [Map] operations. *)
  module MakePortable (Ord : sig
    @@ portable
      include Map.OrderedType
    end) : sig
    @@ portable
    include Map.S with type key = Ord.t
  end
end

module Modes : sig
  module Global : sig
    type ('a : value_or_null) t = { global_ global : 'a } [@@unboxed]
  end

  module Portable : sig
    type ('a : value_or_null) t : value_or_null mod portable =
      { portable : 'a @@ portable }
    [@@unboxed]
  end

  module Contended : sig
    type ('a : value_or_null) t : value_or_null mod contended =
      { contended : 'a @@ contended }
    [@@unboxed]
  end

  module Portended : sig
    type ('a : value_or_null) t : value_or_null mod contended portable =
      { portended : 'a @@ contended portable }
    [@@unboxed]
  end

  module Aliased : sig
    type ('a : value_or_null) t : value_or_null mod aliased = { aliased : 'a @@ aliased }
    [@@unboxed]
  end

  module Many : sig
    type ('a : value_or_null) t : value_or_null mod many = { many : 'a @@ many }
    [@@unboxed]
  end
end

module Obj : sig @@ portable
  external magic_portable
    : ('a : any).
    ('a[@local_opt]) -> ('a[@local_opt]) @ portable
    = "%identity"
  [@@layout_poly]

  external magic_uncontended
    : ('a : any).
    ('a[@local_opt]) @ contended -> ('a[@local_opt])
    = "%identity"
  [@@layout_poly]

  external magic_read_write_uncontended
    : ('a : any).
    ('a[@local_opt]) @ immutable -> ('a[@local_opt])
    = "%identity"
  [@@layout_poly]

  external magic_unique
    : ('a : any).
    ('a[@local_opt]) -> ('a[@local_opt]) @ unique
    = "%identity"
  [@@layout_poly]

  external magic_many
    : ('a : any).
    ('a[@local_opt]) @ once -> ('a[@local_opt])
    = "%identity"
  [@@layout_poly]

  external magic_unyielding
    : ('a : any).
    'a @ local yielding -> 'a @ local unyielding
    = "%identity"
  [@@layout_poly]

  module Extension_constructor : sig
    val of_val : 'a @ immutable -> extension_constructor
  end
end

module Printexc : sig @@ portable
  (** Submodule containing non-backwards-compatible functions which enforce thread safety
      via modes. *)
  module Safe : sig
    (** Like {!register_printer}, but is safe to use in the presence of multiple domains.
        The provided closure must be [portable] as exception printers may be called from
        any domain, not just the one that it's registered on. *)
    val register_printer : (exn -> string option) @ portable -> unit

    (** Like {!set_uncaught_exception_handler}, but is safe to use in the presence of
        multiple domains. The provided closure must be [portable] as exception handlers
        may be called from any domain, not just the one that it's registered on. *)
    val set_uncaught_exception_handler
      :  (exn -> Printexc.raw_backtrace -> unit) @ portable
      -> unit
  end
end

(** Submodule containing non-backwards-compatible functions which enforce thread safety
    via modes. *)
module Safe : sig @@ portable
  (** Like {!at_exit}, but can be called from any domain. The provided closure must be
      [portable] as it might be called from another domain. In particular, the primary
      domain may call {!exit}, thus calling the provided closure even if it came from a
      secondary domain. *)
  val at_exit : (unit -> unit) @ portable -> unit
end

module Set : sig @@ portable
  (** Like [Make], but takes a portable [compare] function to portable [Set] operations. *)
  module MakePortable (Ord : sig
    @@ portable
      include Set.OrderedType
    end) : sig
    @@ portable
    include Set.S with type elt = Ord.t
  end
end

module Sys : sig @@ portable
  (** Submodule containing non-backwards-compatible functions which enforce thread safety
      via modes. *)
  module Safe : sig
    (** Like {!signal}, but is safe to call in the presence of multiple domains. The
        provided [signal_behavior] must be [portable] as it is shared between all domains. *)
    external signal
      :  int
      -> Sys.signal_behavior @ portable
      -> Sys.signal_behavior @ portable
      = "caml_install_signal_handler"

    (** Like {!set_signal}, but is safe to call in the presence of multiple domains. The
        provided [signal_behavior] must be [portable] as it is shared between all domains. *)
    val set_signal : int -> Sys.signal_behavior @ portable -> unit
  end
end

module MoreLabels : sig @@ portable
  module Hashtbl : sig
    (** Like {!Make}, but takes a portable [hash] function to portable [Hashtbl]
        operations. *)
    module MakePortable (H : sig
      @@ portable
        include MoreLabels.Hashtbl.HashedType
      end) : sig
      @@ portable
      include
        MoreLabels.Hashtbl.S
        with type key = H.t
         and type 'a t = 'a Hashtbl.MakePortable(H).t
    end

    (** Like {!MakeSeeded}, but takes a portable [seeded_hash] function to portable
        [Hashtbl] operations. *)
    module MakeSeededPortable (H : sig
      @@ portable
        include MoreLabels.Hashtbl.SeededHashedType
      end) : sig
      @@ portable
      include
        MoreLabels.Hashtbl.SeededS
        with type key = H.t
         and type 'a t = 'a Hashtbl.MakeSeededPortable(H).t
    end
  end

  module Map : sig
    (** Like {!Make}, but takes a portable [compare] function to portable [Map]
        operations. *)
    module MakePortable (Ord : sig
      @@ portable
        include MoreLabels.Map.OrderedType
      end) : sig
      @@ portable
      include
        MoreLabels.Map.S with type key = Ord.t and type 'a t = 'a Map.MakePortable(Ord).t
    end
  end

  module Set : sig
    (** Like {!Make}, but takes a portable [compare] function to portable [Set]
        operations. *)
    module MakePortable (Ord : sig
      @@ portable
        include MoreLabels.Set.OrderedType
      end) : sig
      @@ portable
      include MoreLabels.Set.S with type elt = Ord.t and type t = Set.MakePortable(Ord).t
    end
  end
end
