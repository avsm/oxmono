module type S = sig
  type t

  val create
    :  ?time_source:Async_kernel.Synchronous_time_source.t
    -> Core.Time_float.Zone.t
    -> t

  (** For any rotation scheme that renames logs on rotation, this defines how to do the
      renaming. *)
  val rotate_one : t -> t

  val to_string_opt : t -> string option
  val of_string_opt : string option -> t option
  val cmp_newest_first : t -> t -> int
end

module type Rotation_id = sig
  module type S = S
end
