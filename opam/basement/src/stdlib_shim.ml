external runtime5 : unit -> bool @@ portable = "%runtime5"

external ignore_contended
  : ('a : value_or_null).
  'a @ contended -> unit
  @@ portable
  = "%ignore"

external raise : exn -> 'a @ portable unique @@ portable = "%reraise"
external raise_notrace : exn -> 'a @ portable unique @@ portable = "%raise_notrace"

let failwith s = raise (Failure s)

module Atomic = struct
  type ('a : value_or_null) t = 'a Stdlib.Atomic.t

  external get_contended
    : ('a : value_or_null).
    'a t @ contended local -> 'a @ contended
    @@ portable
    = "%atomic_load"

  module Local = struct
    external make : 'a -> ('a t[@local_opt]) @@ portable = "%makemutable"

    external make_contended
      :  'a
      -> ('a t[@local_opt])
      @@ portable
      = "caml_atomic_make_contended"

    external get : 'a t @ local -> 'a @@ portable = "%atomic_load"
    external set : 'a t @ local -> 'a -> unit @@ portable = "%atomic_set"
    external exchange : 'a t @ local -> 'a -> 'a @@ portable = "%atomic_exchange"

    external compare_and_set
      :  'a t @ local
      -> 'a
      -> 'a
      -> bool
      @@ portable
      = "%atomic_cas"

    external compare_exchange
      :  'a t @ local
      -> 'a
      -> 'a
      -> 'a
      @@ portable
      = "%atomic_compare_exchange"

    external fetch_and_add
      :  int t @ contended local
      -> int
      -> int
      @@ portable
      = "%atomic_fetch_add"

    external add : int t @ contended local -> int -> unit @@ portable = "%atomic_add"
    external sub : int t @ contended local -> int -> unit @@ portable = "%atomic_sub"
    external logand : int t @ contended local -> int -> unit @@ portable = "%atomic_land"
    external logor : int t @ contended local -> int -> unit @@ portable = "%atomic_lor"
    external logxor : int t @ contended local -> int -> unit @@ portable = "%atomic_lxor"

    let incr r = add r 1
    let decr r = sub r 1
  end

  module Expert = struct
    external fenceless_get : 'a t @ local -> 'a @@ portable = "%field0"
    external fenceless_set : 'a t @ local -> 'a -> unit @@ portable = "%setfield0"

    module Contended = struct
      external fenceless_get
        : ('a : value mod contended).
        'a t @ contended local -> 'a
        @@ portable
        = "%field0"

      external fenceless_set
        : ('a : value mod portable).
        'a t @ contended local -> 'a -> unit
        @@ portable
        = "%setfield0"
    end
  end
end

module Backoff = Backoff

module Callback = struct
  module Safe = Callback.Safe
end

module Domain = Domain

module Ephemeron = struct
  module K1 = Ephemeron.K1
  module K2 = Ephemeron.K2
  module Kn = Ephemeron.Kn
end

module Format = Format

module Hashtbl = struct
  module MakePortable = Hashtbl.MakePortable
  module MakeSeededPortable = Hashtbl.MakeSeededPortable
end

module Map = struct
  module MakePortable = Map.MakePortable
end

module Modes = Modes

module MoreLabels = struct
  module Hashtbl = struct
    module MakePortable = MoreLabels.Hashtbl.MakePortable
    module MakeSeededPortable = MoreLabels.Hashtbl.MakeSeededPortable
  end

  module Map = struct
    module MakePortable = MoreLabels.Map.MakePortable
  end

  module Set = struct
    module MakePortable = MoreLabels.Set.MakePortable
  end
end

module Obj = struct
  external magic_portable
    : ('a : any).
    ('a[@local_opt]) -> ('a[@local_opt]) @ portable
    @@ portable
    = "%identity"
  [@@layout_poly]

  external magic_uncontended
    : ('a : any).
    ('a[@local_opt]) @ contended -> ('a[@local_opt])
    @@ portable
    = "%identity"
  [@@layout_poly]

  external magic_read_write_uncontended
    : ('a : any).
    ('a[@local_opt]) @ immutable -> ('a[@local_opt])
    @@ portable
    = "%identity"
  [@@layout_poly]

  external magic_unique
    : ('a : any).
    ('a[@local_opt]) -> ('a[@local_opt]) @ unique
    @@ portable
    = "%identity"
  [@@layout_poly]

  external magic_many
    : ('a : any).
    ('a[@local_opt]) @ once -> ('a[@local_opt])
    @@ portable
    = "%identity"
  [@@layout_poly]

  external magic_unyielding
    : ('a : any).
    'a @ local yielding -> 'a @ local unyielding
    @@ portable
    = "%identity"
  [@@layout_poly]

  module Extension_constructor = struct
    let of_val x =
      Stdlib.Obj.Extension_constructor.of_val (magic_read_write_uncontended x)
    ;;
  end
end

module Printexc = struct
  module Safe = Printexc.Safe
end

module Safe = Safe

module Set = struct
  module MakePortable = Set.MakePortable
end

module Sys = struct
  module Safe = Sys.Safe
end
