@@ portable

(** The result of a call to {!Atomic.compare_and_set}. See the documentation of that
    function for more information. *)
type t =
  | Compare_failed
  | Set_here
