(** Scan the leds left and right with a small tail. This models a Cylons (or, if you
    prefer, Kits) eye. If you dont know what either of those are it's because you are
    young. *)

open Hardcaml

val cylon_eye_top : scope:Scope.t -> enable_rate:int -> clock:Signal.t -> Signal.t list
val create : unit -> Hardcaml_hobby_boards_kernel.Board.t
