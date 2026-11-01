(** Memento clients using a caller-supplied Fetch transport.

    Client restrictions, logging, connection checks, timeouts and retries
    remain the caller's Fetch policy. Transport failures and cancellation
    propagate as exceptions. Protocol and decoding failures return [Error]. *)
type discovery = {
  url : string;
  status : int;
  metadata : Memento.Headers.metadata;
  capture : Memento.capture option;
}
(** A [discovery] describes the final response URL after bounded redirects.
    [capture] uses Content-Location when a TimeGate serves a Memento directly.
    It is absent when a negotiating resource has no distinct Memento URI. *)
val discover : ?redirects:int -> _ Fetch.t -> string -> (discovery, string) result
(** [discover client uri] reads metadata with HEAD, including error responses.
    [redirects] defaults to five. *)
val negotiate :
  ?redirects:int -> _ Fetch.t -> datetime:Memento.Datetime.t -> string ->
  (discovery, string) result
(** [negotiate client ~datetime timegate] sends Accept-Datetime with HEAD,
    follows redirects and requires a successful Memento response. It returns
    metadata without downloading the archived representation. For 200
    TimeGates with Content-Location, [capture] identifies the selected URI. *)
val find :
  ?redirects:int -> ?timegate:string -> _ Fetch.t ->
  datetime:Memento.Datetime.t -> string -> (discovery, string) result
(** [find client ~datetime original] discovers a TimeGate and negotiates.
    [timegate] bypasses the original entirely, allowing lookup of broken
    links through an archive endpoint supplied by the caller. *)
type timemap = Json of Memento.Timemap.t | Links of Memento.Link.t list
(** A [timemap] preserves the received serialization's structure. *)
val timemap :
  ?limit:int -> ?redirects:int -> _ Fetch.t -> string -> (timemap, string) result
(** [timemap client uri] fetches one JSON or link-format TimeMap. [limit]
    defaults to 16 MiB and bounds body bytes. JSON nesting is bounded at 128.
    Page and index links are returned without automatic traversal. Link
    targets are preserved as received. A negative limit raises Invalid_argument. *)
val captures : timemap -> (Memento.capture list, string) result
(** [captures t] extracts the captures on this page. *)
