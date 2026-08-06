@@ portable

(* An abstract type. The underlying representation ([int]) crosses every
   axis, but the abstraction hides that. *)
type t
val v : t
val get : t -> int
