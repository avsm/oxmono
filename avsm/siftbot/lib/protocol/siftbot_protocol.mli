val max_bytes : int
(** Bounded, versioned process messages. *)

val request : Siftbot_model.request -> string
val response : Siftbot_model.response -> string
val parse_request : string -> (Siftbot_model.request, string) result
val parse_response : string -> (Siftbot_model.response, string) result

val read : in_channel -> string
(** [read channel] reads one bounded line, rejecting oversized input. *)
