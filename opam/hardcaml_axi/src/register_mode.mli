(** Write configuration of a register from the core interface. *)

module Mode : sig
  type t =
    | Toggle_low (** Register toggles back to [0] after a write *)
    | Toggle_high (** Register toggles back to all [1]'s after a write *)
    | Hold (** Register holds its value after a write *)
  [@@deriving sexp_of]
end

type t [@@deriving sexp_of]

(** {2 Creation of write modes} *)

(** Create a configuration for a write register. [internal_clear] allows the register to
    be cleared by user logic. [clear_to] is the initial value of the register after clear. *)
val create : ?internal_clear:bool -> ?clear_to:int -> Mode.t -> t

(** Register holds it's value after a write. *)
val hold : t

(** Like [hold] expect the register may be cleared by user logic. *)
val hold_with_internal_clear : t

(** Register takes the written value for 1 cycle, then sets itself high. *)
val toggle_high : t

(** Register takes the written value for 1 cycle, then sets itself low. *)
val toggle_low : t

(** {2 Fields} *)

val internal_clear : t -> bool
val clear_to : t -> int
val mode : t -> Mode.t
