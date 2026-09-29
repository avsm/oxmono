(** Serializable Foundation Models conversations. *)

type t = Apple_fm_base.Transcript.t
(** A complete Apple
    {{:https://developer.apple.com/documentation/foundationmodels/transcript}
      transcript}. *)

val of_json : string -> (t, string) result
(** [of_json text] checks and retains a transcript's JSON representation. *)

val to_json : t -> string
(** [to_json transcript] returns Apple's Codable JSON representation. *)

val pp : Format.formatter -> t -> unit
(** [pp formatter transcript] prints [transcript] as JSON. *)
