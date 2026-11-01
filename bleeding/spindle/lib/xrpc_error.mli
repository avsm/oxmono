(** XRPC errors with the status and tag the reference spindle answers. *)

exception E of Httpz.Res.status * string * string
(** [E (status, error, message)] answers [status] with
    [{"error": error, "message": message}]. *)

val fail : Httpz.Res.status -> string -> string -> 'a
