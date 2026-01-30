@@ portable

(** This module is reexported, with documentation, as [Base.Portable_lazy]; see that
    module for documentation this interface. *)

type (+'a : value_or_null) t : value mod contended portable

val from_val : ('a : value_or_null). 'a @ contended portable -> 'a t

val from_fun
  : ('a : value_or_null).
  (unit -> 'a @ contended portable) @ once portable -> 'a t

val from_fun_fixed
  : ('a : value_or_null).
  ('a t -> 'a @ contended portable) @ once portable -> 'a t

val force : ('a : value_or_null). 'a t @ local -> 'a @ contended portable

val map
  : ('a : value_or_null) ('b : value_or_null).
  'a t -> f:('a @ contended portable -> 'b @ contended portable) @ once portable -> 'b t

val bind
  : ('a : value_or_null) ('b : value_or_null).
  'a t -> f:('a @ contended portable -> 'b t) @ once portable -> 'b t

val compare : ('a : value_or_null mod contended). ('a -> 'a -> int) -> 'a t -> 'a t -> int

val compare__local
  : ('a : value_or_null mod contended).
  ('a @ local -> 'a @ local -> int) -> 'a t @ local -> 'a t @ local -> int

val equal : ('a : value_or_null mod contended). ('a -> 'a -> bool) -> 'a t -> 'a t -> bool

val equal__local
  : ('a : value_or_null mod contended).
  ('a @ local -> 'a @ local -> bool) -> 'a t @ local -> 'a t @ local -> bool

val globalize : ('a : value_or_null) ('b : value_or_null). 'b -> 'a t @ local -> 'a t
val is_val : ('a : value_or_null). 'a t -> bool
val peek : ('a : value). 'a t -> 'a Or_null_shim.t @ contended portable
val peek_opt : ('a : value_or_null). 'a t -> 'a option @ contended portable
