(** Internet Archive Wayback CDX queries using a caller-supplied Fetch client.

    Each call makes one bounded query. Transport failures propagate as Fetch
    exceptions. HTTP and malformed response errors return [Error]. *)
type version = {
  capture : Memento.capture;
  original_uri : string;
  status : int option;
  media_type : string;
}
(** A [version] is one indexed capture. [status] is absent when the index
    records no HTTP status. A capture may be unavailable at playback time. *)
val versions :
  ?from:string -> ?until:string -> ?latest:bool -> ?limit:int ->
  _ Fetch.t -> string -> (version list, string) result
(** [versions client url] lists captures of an absolute HTTP(S) [url].
    [from] and [until] are inclusive CDX timestamps with one to fourteen
    digits. [latest] requests the last results instead of the first.
    [limit] defaults to 100 and must be between 1 and 10000. All recorded
    statuses are included. Invalid arguments return [Error] without a request.
    The decoded response is bounded at 4 MiB. *)
val urls :
  ?limit:int -> _ Fetch.t -> string -> (string list, string) result
(** [urls client prefix] lists archived original URLs under the absolute
    HTTP(S) [prefix], collapsed by CDX URL key. [limit] defaults to 100 and
    must be between 1 and 10000. The response is bounded at 4 MiB. This
    queries the archive index and does not fetch any original URL. *)
