(** System-model availability. *)

type t =
  [ `Available
  | `Device_not_eligible
  | `Apple_intelligence_not_enabled
  | `Model_not_ready
  | `Unavailable of string ]
(** Whether Apple's default system language model can accept a request. *)

val get : unit -> t
(** [get ()] reports the current
    {{:https://developer.apple.com/documentation/foundationmodels/systemlanguagemodel/availability-swift.property}
      model availability}. *)

val pp : Format.formatter -> t -> unit
(** [pp formatter availability] prints [availability]. *)
