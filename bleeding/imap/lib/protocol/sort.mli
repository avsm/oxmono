(** RFC 5256 SORT keys and RFC 5267 ESORT return options. *)

type key = Arrival | Cc | Date | From | Size | Subject | To

type order = Ascending | Descending
(** [Descending] sends the key with the REVERSE modifier. *)

type return =
  | Min
  | Max
  | Count
  | All
  | Partial of (int64 * int64)
      (** RFC 9394 result positions, each in 1..4294967295. *)

val key_to_wire : key -> string
(** [key_to_wire k] is the uppercase key name of [k]. *)

val criterion_to_wire : key * order -> string
(** [criterion_to_wire (k, o)] is [k] on the wire, preceded by [REVERSE]
    when [o] is [Descending]. *)

val return_to_wire : return -> string
(** [return_to_wire r] is the return option [r], with [Partial (a, b)] as
    [PARTIAL a:b]. It does not check the range. *)

val equal_return : return -> return -> bool
(** [equal_return a b] holds when [a] and [b] are the same option with the
    same range. *)
