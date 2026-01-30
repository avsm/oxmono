open Core

module Basic_info = struct
  type t =
    { maximum_value : int
    ; brand : string
    }
end

module type S = sig
  type t

  val canonical_identifier : t -> string
  val maximum_leaf : t -> int
  val kind : t Type_equal.Id.t
  val create : Basic_info.t -> t
end
