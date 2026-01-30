open! Core
open! Import

module type S = sig
  type index

  [%%template:
  [@@@kind.default
    k
    = ( float32
      , bits64
      , value
      , immediate64
      , value & value
      , immediate64 & immediate64
      , value & value & value
      , immediate64 & immediate64 & immediate64
      , value & value & value & value
      , immediate64 & immediate64 & immediate64 & immediate64 )]

  type ('a : k) t : mutable_data with 'a
  [@@deriving compare ~localize, equal ~localize, sexp]

  val invariant : ('a -> unit) -> ('a t[@kind k]) Invariant.t
  val create : ?initial_capacity:int -> unit -> ('a t[@kind k])

  (** [init n ~f] returns a fresh vector of length [n], with element number [i]
      initialized to the result of [f i]. In other words, [init n ~f] tabulates the
      results of [f] applied to the integers [0] to [n-1].

      Raise Invalid_argument if [n < 0]. *)
  val init : int -> f:local_ (int -> 'a) -> ('a t[@kind k])

  (** Raises if the index is invalid. *)
  val get : ('a t[@kind k]) -> index -> 'a
  [@@zero_alloc]]

  val maybe_get : 'a t -> index -> 'a option
  val maybe_get_local : 'a t -> index -> local_ 'a Modes.Global.t option [@@zero_alloc]
  val maybe_get_or_null : 'a t -> index -> 'a or_null [@@zero_alloc]
  val last : 'a t -> 'a or_null

  [%%template:
  [@@@kind.default
    k
    = ( float32
      , bits64
      , value
      , immediate64
      , value & value
      , immediate64 & immediate64
      , value & value & value
      , immediate64 & immediate64 & immediate64
      , value & value & value & value
      , immediate64 & immediate64 & immediate64 & immediate64 )]

  (** Raises if the index is invalid. *)
  val set : ('a t[@kind k]) -> index -> 'a -> unit
  [@@zero_alloc]]

  val set_imm : ('a : immediate64). 'a t -> index -> 'a -> unit [@@zero_alloc]

  include Container.S1 with type 'a t := 'a t
  include Blit.S1 with type 'a t := 'a t

  [%%template:
  [@@@kind.default
    k
    = ( float32
      , bits64
      , immediate64
      , value & value
      , immediate64 & immediate64
      , value & value & value
      , immediate64 & immediate64 & immediate64
      , value & value & value & value
      , immediate64 & immediate64 & immediate64 & immediate64 )]

  val mem : ('a t[@kind k]) -> 'a -> equal:local_ ('a -> 'a -> bool) -> bool
  val length : (_ t[@kind k]) -> int [@@zero_alloc]
  val is_empty : (_ t[@kind k]) -> bool [@@zero_alloc]
  val iter : ('a t[@kind k]) -> f:local_ ('a -> unit) -> unit
  val fold : ('a t[@kind k]) -> init:'acc -> f:local_ ('acc -> 'a -> 'acc) -> 'acc
  val exists : ('a t[@kind k]) -> f:local_ ('a -> bool) -> bool
  val for_all : ('a t[@kind k]) -> f:local_ ('a -> bool) -> bool
  val count : ('a t[@kind k]) -> f:local_ ('a -> bool) -> int

  val sum
    :  (module Container.Summable with type t = 'sum)
    -> ('a t[@kind k])
    -> f:local_ ('a -> 'sum)
    -> 'sum

  val sub : ('a t[@kind k]) -> pos:int -> len:int -> ('a t[@kind k])

  val blit
    :  src:('a t[@kind k])
    -> src_pos:int
    -> dst:('a t[@kind k])
    -> dst_pos:int
    -> len:int
    -> unit]

  val%template to_array : ('a t[@kind float32]) -> 'a F32.Array.t [@@kind k = float32]
  val%template to_array : ('a t[@kind bits64]) -> 'a I64.Array.t [@@kind k = bits64]

  [%%template:
  [@@@kind.default
    k
    = ( float32
      , bits64
      , value
      , immediate64
      , value & value
      , immediate64 & immediate64
      , value & value & value
      , immediate64 & immediate64 & immediate64
      , value & value & value & value
      , immediate64 & immediate64 & immediate64 & immediate64 )]

  (** Finds the first 'a for which f is true *)
  val find_exn : ('a t[@kind k]) -> f:local_ ('a -> bool) -> 'a]

  (** [sort] uses constant heap space. To sort only part of the array, specify [pos] to be
      the index to start sorting from and [len] indicating how many elements to sort. *)
  val sort : ?pos:int -> ?len:int -> 'a t -> compare:('a -> 'a -> int) -> unit

  [%%template:
  [@@@kind.default
    k
    = ( float32
      , bits64
      , value
      , immediate64
      , value & value
      , immediate64 & immediate64
      , value & value & value
      , immediate64 & immediate64 & immediate64
      , value & value & value & value
      , immediate64 & immediate64 & immediate64 & immediate64 )]

  val is_sorted : ('a t[@kind k]) -> compare:local_ ('a -> 'a -> int) -> bool
  val is_sorted_strictly : ('a t[@kind k]) -> compare:local_ ('a -> 'a -> int) -> bool]

  include Binary_searchable.S1 with type 'a t := 'a t

  [%%template:
  [@@@kind.default
    k
    = ( float32
      , bits64
      , value
      , immediate64
      , value & value
      , immediate64 & immediate64
      , value & value & value
      , immediate64 & immediate64 & immediate64
      , value & value & value & value
      , immediate64 & immediate64 & immediate64 & immediate64 )]

  val next_free_index : ('a t[@kind k]) -> index
  val push_back : ('a t[@kind k]) -> 'a -> unit
  val push_back_index : ('a t[@kind k]) -> 'a -> index]

  val push_back_imm : ('a : immediate64). 'a t -> 'a -> unit
  val push_back_index_imm : ('a : immediate64). 'a t -> 'a -> index

  [%%template:
  [@@@kind.default
    k
    = ( float32
      , bits64
      , value
      , immediate64
      , value & value
      , immediate64 & immediate64
      , value & value & value
      , immediate64 & immediate64 & immediate64
      , value & value & value & value
      , immediate64 & immediate64 & immediate64 & immediate64 )]

  (** Grows the vec to the specified length if it is currently shorter. Sets all new
      indices to [default]. *)
  val grow_to : ('a t[@kind k]) -> len:int -> default:'a -> unit

  (** Equivalent to [grow_to t (index + 1) ~default]. *)
  val grow_to_include : ('a t[@kind k]) -> index -> default:'a -> unit

  (** Grows the vec to the specified length if it is currently shorter. Sets all new
      indices to [default idx]. *)
  val grow_to' : ('a t[@kind k]) -> len:int -> default:(index -> 'a) -> unit

  (** Equivalent to [grow_to' t (index + 1) ~default]. *)
  val grow_to_include' : ('a t[@kind k]) -> index -> default:(index -> 'a) -> unit

  (** Shortens the vec to the specified length if it is currently longer. Raises if
      [len < 0]. *)
  val shrink_to : ('a t[@kind k]) -> len:int -> unit
  [@@zero_alloc]

  (** [remove vec i] Removes the i-th element of the vector. This is not a fast
      implementation, and runs in O(N) time. (ie: it calls caml_modify under the hood) *)
  val remove_exn : ('a t[@kind k]) -> int -> unit]

  (** Find the first element that satisfies [f]. If exists, remove the element from the
      vector and return it. This is not a fast implementation, and runs in O(N) time. *)
  val find_and_remove : 'a t -> f:local_ ('a -> bool) -> 'a option

  val findi : 'a t -> f:local_ ('a -> bool) -> (index * 'a) option
  val pop_back_imm_exn : ('a : immediate64). 'a t -> 'a [@@zero_alloc]
  val pop_back_unit_imm_exn : ('a : immediate64). 'a t -> unit [@@zero_alloc]
  val peek_back : 'a t -> 'a or_null
  val pop_back : 'a t -> 'a or_null

  [%%template:
  [@@@kind.default
    k
    = ( float32
      , bits64
      , value
      , immediate64
      , value & value
      , immediate64 & immediate64
      , value & value & value
      , immediate64 & immediate64 & immediate64
      , value & value & value & value
      , immediate64 & immediate64 & immediate64 & immediate64 )]

  val pop_back_exn : ('a t[@kind k]) -> 'a
  val pop_back_unit_exn : ('a t[@kind k]) -> unit
  val peek_back_exn : ('a t[@kind k]) -> 'a

  val foldi
    :  ('a t[@kind k])
    -> init:'accum
    -> f:local_ (index -> 'accum -> 'a -> 'accum)
    -> 'accum

  val foldi_local_accum
    :  ('a t[@kind k])
    -> init:local_ 'accum
    -> f:local_ (index -> local_ 'accum -> 'a -> local_ 'accum)
    -> local_ 'accum

  val foldi_until
    :  ('a t[@kind k])
    -> init:'accum
    -> f:local_ (index -> 'accum -> 'a -> ('accum, 'b) Continue_or_stop.t)
    -> finish:('accum -> 'b)
    -> 'b

  val iteri : ('a t[@kind k]) -> f:local_ (index -> 'a -> unit) -> unit]

  val to_list : 'a t -> 'a list
  val to_local_list : 'a t -> local_ 'a list
  val to_local_iarray : 'a t -> local_ 'a iarray
  val to_iarray : 'a t -> 'a iarray
  val to_alist : 'a t -> (index * 'a) list

  (** The input vec is copied internally so that future modifications of it do not change
      the sequence. *)
  val to_sequence : 'a t -> 'a Sequence.t

  (** The input vec is shared with the sequence and modifications of it will result in
      modification of the sequence. *)
  val to_sequence_mutable : 'a t -> 'a Sequence.t

  val of_list : 'a list -> 'a t
  val of_array : 'a array -> 'a t
  val of_sequence : 'a Sequence.t -> 'a t

  [%%template:
  [@@@kind.default
    k
    = ( float32
      , bits64
      , value
      , immediate64
      , value & value
      , immediate64 & immediate64
      , value & value & value
      , immediate64 & immediate64 & immediate64
      , value & value & value & value
      , immediate64 & immediate64 & immediate64 & immediate64 )]

  (** [take_while t ~f] returns a fresh vec containing the longest prefix of [t] for which
      [f] is [true]. *)
  val take_while : ('a t[@kind k]) -> f:local_ ('a -> bool) -> ('a t[@kind k])]

  module Inplace : sig
    [%%template:
    [@@@kind.default
      k
      = ( float32
        , bits64
        , value
        , immediate64
        , value & value
        , immediate64 & immediate64
        , value & value & value
        , immediate64 & immediate64 & immediate64
        , value & value & value & value
        , immediate64 & immediate64 & immediate64 & immediate64 )]

    (** [sub] is like [Blit.sub], but modifies the vec in place. *)
    val sub : ('a t[@kind k]) -> pos:index -> len:int -> unit

    (** [take_while t ~f] shortens the vec in place to the longest prefix of [t] for which
        [f] is [true]. *)
    val take_while : ('a t[@kind k]) -> f:local_ ('a -> bool) -> unit

    (** Remove all elements from [t] that don't satisfy [f]. Shortens the vec in place. *)
    val filter : ('a t[@kind k]) -> f:local_ ('a -> bool) -> unit

    (** Modifies a vec in place, applying [f] to every element of the vec. *)
    val map : ('a t[@kind k]) -> f:local_ ('a -> 'a) -> unit

    (** Same as [map], but [f] also takes the index. *)
    val mapi : ('a t[@kind k]) -> f:local_ (index -> 'a -> 'a) -> unit

    (** Removes adjacent duplicates in-place. The relative order of the other elements is
        unaffected. The element kept from a run of duplicates is determined by
        [which_to_keep], which defaults to [`First]. *)
    val remove_consecutive_duplicates
      :  ?which_to_keep:[ `First | `Last ]
      -> ('a t[@kind k])
      -> equal:local_ ('a -> 'a -> bool)
      -> unit]
  end

  [%%template:
  [@@@kind.default
    k
    = ( float32
      , bits64
      , value
      , immediate64
      , value & value
      , immediate64 & immediate64
      , value & value & value
      , immediate64 & immediate64 & immediate64
      , value & value & value & value
      , immediate64 & immediate64 & immediate64 & immediate64 )]

  (** The number of elements we can hold without growing. *)
  val capacity : (_ t[@kind k]) -> int
  [@@zero_alloc]

  (** [clear t] discards all elements from [t] in O(length) time. *)
  val clear : (_ t[@kind k]) -> unit
  [@@zero_alloc]]

  (** [clear_imm t] discards all elements from ['a t] in O(1) time if ['a] is immediate. *)
  val clear_imm : ('a : immediate64). 'a t -> unit
  [@@zero_alloc]

  [%%template:
  [@@@kind.default
    k
    = ( float32
      , bits64
      , value
      , immediate64
      , value & value
      , immediate64 & immediate64
      , value & value & value
      , immediate64 & immediate64 & immediate64
      , value & value & value & value
      , immediate64 & immediate64 & immediate64 & immediate64 )]

  (** [copy t] returns a copy of [t], that is, a fresh vec containing the same elements as
      [t]. *)
  val copy : ('a t[@kind k]) -> ('a t[@kind k])

  (** swap the values at the provided indices *)
  val swap : (_ t[@kind k]) -> index -> index -> unit
  [@@zero_alloc]

  (** [swap_to_last_and_pop t i] is equivalent to
      [swap t i (length t - 1); pop_back_exn t]. It raises if [i] is out of bounds. *)
  val swap_to_last_and_pop : ('a t[@kind k]) -> index -> 'a]

  val swap_to_last_and_pop_imm : ('a : immediate64). 'a t -> index -> 'a

  module With_structure_details : sig
    (** [[%sexp_of : t]] above only prints the elements. This gives various data structure
        details. *)
    type%template nonrec 'a t = ('a t[@kind k])
    [@@kind
      k
      = ( float32
        , bits64
        , value
        , immediate64
        , value & value
        , immediate64 & immediate64
        , value & value & value
        , immediate64 & immediate64 & immediate64
        , value & value & value & value
        , immediate64 & immediate64 & immediate64 & immediate64 )]
    [@@deriving sexp_of]
  end

  [%%template:
  [@@@kind.default
    k
    = ( float32
      , bits64
      , value
      , immediate64
      , value & value
      , immediate64 & immediate64
      , value & value & value
      , immediate64 & immediate64 & immediate64
      , value & value & value & value
      , immediate64 & immediate64 & immediate64 & immediate64 )]

  val unsafe_get : ('a t[@kind k]) -> index -> 'a
  val unsafe_set : ('a t[@kind k]) -> index -> 'a -> unit
  val unsafe_set_length : (_ t[@kind k]) -> int -> unit]

  val unsafe_set_imm : ('a : immediate64). 'a t -> index -> 'a -> unit

  module Stable : sig
    module V1 : sig
      type nonrec 'a t = 'a t [@@deriving bin_io, compare ~localize, sexp, stable_witness]
    end
  end

  module For_testing : sig
    val%template zero_unused_capacity : ('a t[@kind bits64]) -> unit
  end
end

module type Vec = sig @@ portable
  (** A growable array of ['a]. Designed for efficiency and simplicity.

      This interface is generated lazily: if you need a standard function we haven't
      added, feel free to add or ping the authors.

      By default, [Vec] operations use integers as indices. The functor [Make] can be used
      to create a specialized version from any module implementing [Intable.S]. *)
  include S with type index := int

  module type S = S

  (** Generate a specialised version of [Vec] with a custom index type. *)
  module%template.portable Make (M : Intable.S) : S with type index := M.t
end
