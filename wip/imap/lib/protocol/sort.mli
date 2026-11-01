@@ portable

(** SORT keys and ESORT return options.

    The vocabulary of RFC 5256 SORT and RFC 5267 ESORT. *)

type key = Arrival | Cc | Date | From | Size | Subject | To
(** The type for sort keys. *)

type order = Ascending | Descending
(** The type for sort directions. [Descending] sends the key with the
    REVERSE modifier. *)

type return =
  | Min
  | Max
  | Count
  | All
  | Partial of (int64 * int64)
      (** RFC 9394 result positions [(first, last)].
          {!Command.uid_sort_extended} requires each in 1 to
          4294967295. *)
(** The type for ESORT return options. *)

val key_to_wire : key -> string
(** [key_to_wire k] is the uppercase key name of [k]. *)

val criterion_to_wire : key * order -> string
(** [criterion_to_wire (k, o)] is [k] on the wire, preceded by [REVERSE]
    when [o] is [Descending]. *)

val return_to_wire : return -> string
(** [return_to_wire r] is the return option [r], with [Partial (a, b)] as
    [PARTIAL a:b]. It does not check the range. *)

val equal_return : return -> return -> bool
(** [equal_return a b] is [true] if [a] and [b] are the same option with
    the same range. *)
