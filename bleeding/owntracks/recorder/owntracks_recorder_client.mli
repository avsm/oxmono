type t
(** Streaming OwnTracks Recorder HTTP client. *)

type error = Http_status of int | Invalid_response of string

val v :
  ?max_response:int -> ?auth:string * string -> Fetch.plain -> url:string -> t
(** [v http ~url] uses an injected Fetch client. [auth] supplies HTTP Basic
    credentials scoped to the Recorder URL, including explicitly chosen HTTP
    URLs. Responses default to a 16 MiB limit. Invalid settings raise
    [Invalid_argument]. The caller bounds request duration with Eio timeouts. *)

val list_users : t -> (string list, error) result
val list_devices : t -> user:string -> (string list, error) result

val locations :
  t ->
  user:string ->
  device:string ->
  from_date:string ->
  to_date:string ->
  (Owntracks.Location.t list, error) result
(** [locations t ~user ~device ~from_date ~to_date] queries a date interval. All
    query values are URL encoded. HTTP and JSON failures return errors.
    Transport errors and cancellation propagate. *)

val pp_error : Format.formatter -> error -> unit
