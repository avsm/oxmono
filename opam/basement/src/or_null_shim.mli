(** Nullable values.

    Nullable values explicitly indicate the presence or absence of a value, representing
    present values as themselves and absent ones as null pointers. Unlike the regular
    [option], the [or_null] type can't be nested.

    This module mirrors [Stdlib.Option], except for [join] which can't be defined. *)

(** The type of nullable values. Either [Null] or a value [This v]. ['a or_null] has a
    non-standard layout [value_or_null], preventing the type constructor from being
    nested. *)
type 'a t = 'a or_null [@@or_null_reexport]

(** [null] is [Null]. *)
val null : 'a t

(** [this v] is [This v]. *)
val this : 'a -> 'a t

(** [value o ~default] is [v] if [o] is [This v] and [default] otherwise. *)
val value : 'a t -> default:'a -> 'a

(** [get o] is [v] if [o] is [This v] and raise otherwise. *)
val get : 'a t -> 'a

(** [bind o f] is [f v] if [o] is [This v] and [Null] if [o] is [Null]. *)
val bind : 'a t -> ('a -> 'b t) -> 'b t

(** [map f o] is [Null] if [o] is [Null] and [This v] if [o] is [This v]. *)
val map : ('a -> 'b) -> 'a t -> 'b t

(** [fold ~null ~this o] is [null] if [o] is [Null] and [this v] if [o] is [This v]. *)
val fold : null:'a -> this:('b -> 'a) -> 'b t -> 'a

(** [iter f o] is [f v] if [o] is [This v] and [()] otherwise. *)
val iter : ('a -> unit) -> 'a t -> unit

(** [is_null o] is [true] if [o] is [Null] and [false] otherwise. *)
val is_null : 'a t -> bool

(** [is_this o] is [true] if [o] is [This v] and [false] otherwise. *)
val is_this : 'a t -> bool

(** [equal eq o0 o1] is [true] if and only if [o0] and [o1] are both [Null] or if they are
    [This v0] and [This v1] and [eq v0 v1] is [true]. *)
val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool

(** [compare f o o'] is a total order on [t] using [cmp] to compare values wrapped by
    [This _]. [Null] is smaller than [This _] values. *)
val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int

(** [to_result ~null o] is [Ok v] if [o] is [This v] and [Error null] otherwise. *)
val to_result : null:'e -> 'a t -> ('a, 'e) result

(** [to_list] is [[]] if [o] is [Null] and [[v]] if [o] is [This v]. *)
val to_list : 'a t -> 'a list

(** [to_seq o] is [o] as a sequence. [Null] is the empty sequence and [This v] is the
    singleton sequence containing [v]. *)
val to_seq : 'a t -> 'a Seq.t

(** [to_option o] is [Some v] if [o] is [This v] and [None] otherwise. *)
val to_option : 'a t -> 'a option

(** [of_option o] is [This v] if [o] is [Some v] and [Null] otherwise. *)
val of_option : 'a option -> 'a t
