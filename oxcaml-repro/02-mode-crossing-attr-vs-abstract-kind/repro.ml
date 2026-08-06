(* [@@unsafe_allow_any_mode_crossing] alone does not change the declared
   kind, so matching the abstract kind in the interface fails. *)
type t = { g : (int -> int) array }
[@@unsafe_allow_any_mode_crossing]
