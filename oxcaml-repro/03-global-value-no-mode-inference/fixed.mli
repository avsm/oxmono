@@ portable

(* Declaring the kind in the library's interface fixes it, because the
   implementation can prove it. Only the library author can do this. *)
type t : immediate
val v : t
val get : t -> int
