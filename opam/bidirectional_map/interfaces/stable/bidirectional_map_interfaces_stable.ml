open! Core
open Bidirectional_map_interfaces

module type With_bin_io = sig
  type t

  include Binable.S with type t := t
  include Comparator.S with type t := t
end

module type With_stable_witness = sig
  type t

  val stable_witness : t Stable_witness.t

  include Comparator.S with type t := t
end

module type S = sig
  type ('l, 'lc, 'r, 'rc) t

  include Deriving_shared with type ('l, 'lc, 'r, 'rc) t := ('l, 'lc, 'r, 'rc) t

  (** Used by [@@deriving bin_io] *)
  val bin_shape_m__t
    :  (module With_bin_io with type t = 'l)
    -> (module With_bin_io with type t = 'r)
    -> Bin_shape.t

  (** Used by [@@deriving bin_io] *)
  val bin_size_m__t
    :  (module With_bin_io with type t = 'l)
    -> (module With_bin_io with type t = 'r)
    -> ('l, _, 'r, _) t
    -> int

  (** Used by [@@deriving bin_io] *)
  val bin_write_m__t
    :  (module With_bin_io with type t = 'l)
    -> (module With_bin_io with type t = 'r)
    -> pos:int
    -> Bin_prot.Common.buf @ local
    -> ('l, _, 'r, _) t
    -> int

  (** Used by [@@deriving bin_io] *)
  val bin_read_m__t
    :  (module With_bin_io with type t = 'l and type comparator_witness = 'lc)
    -> (module With_bin_io with type t = 'r and type comparator_witness = 'rc)
    -> pos_ref:int ref @ local
    -> Bin_prot.Common.buf @ local
    -> ('l, 'lc, 'r, 'rc) t

  (** Used by [@@deriving bin_io] *)
  val __bin_read_m__t__
    :  (module With_bin_io with type t = 'l and type comparator_witness = 'lc)
    -> (module With_bin_io with type t = 'r and type comparator_witness = 'rc)
    -> pos_ref:int ref @ local
    -> Bin_prot.Common.buf @ local
    -> int
    -> ('l, 'lc, 'r, 'rc) t

  (** Used by [@@deriving stable_witness] *)
  val stable_witness_m__t
    :  (module With_stable_witness with type t = 'l and type comparator_witness = 'lc)
    -> (module With_stable_witness with type t = 'r and type comparator_witness = 'rc)
    -> ('l, 'lc, 'r, 'rc) t Stable_witness.t

  module Provide_bin_io (Left : With_bin_io) (Right : With_bin_io) :
    Binable.S with type t := M(Left)(Right).t
end
