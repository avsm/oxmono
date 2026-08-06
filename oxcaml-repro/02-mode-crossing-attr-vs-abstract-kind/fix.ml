(* Repeating the kind on the declaration makes everything work. *)
type t : value mod portable contended = { g : (int -> int) array }
[@@unsafe_allow_any_mode_crossing]
