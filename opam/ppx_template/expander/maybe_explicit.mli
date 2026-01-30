(** [Maybe_explicit] is used by the mangling scheme to decide whether to enumerate all
    manglers, or drop an axis if all manglers for that axis is a default value. *)

type explicitness =
  | Explicit
  | Drop_axis_if_all_defaults

type 'a t = explicitness * 'a

val map : 'a t -> f:('a -> 'b) -> 'b t
val map_result : 'a t -> f:('a -> ('b, 'c) result) -> ('b t, 'c) result
val ok : ('a, 'b) result t -> ('a t, 'b) result

module Both : sig
  type 'a maybe_explicit := 'a t

  type 'a t = private
    { explicit : 'a
    ; drop_axis_if_all_defaults : 'a
    }

  type 'a opt_map_result =
    | Neither
    | One of 'a maybe_explicit
    | Both of 'a t

  val create : (explicitness -> 'a) -> 'a t
  val extract_list : 'a t -> 'a list
  val all : 'a t list -> 'a list t
  val opt_map : 'a t -> f:('a -> 'b option) -> 'b opt_map_result

  val opt_fold_map
    :  'a t
    -> init:'b
    -> f:('b -> 'a -> 'b * 'c option)
    -> 'b * 'c opt_map_result
end
