(** Read-only Nominatim tools for any Agentkit model adapter.

    Link [agentkit-osm]. Pass [tools client] to an adapter, or use [definitions]
    and [invoke] when the host owns dispatch. See the package guide for JSON
    arguments, output fields and a complete integration example. *)

type t
(** A configured address service. Construction performs no I/O. *)

val create :
  ?nominatim_url:string ->
  ?max_result_bytes:int ->
  fetch:_ Fetch.t ->
  clock:_ Eio.Time.Mono.t ->
  unit ->
  t
(** [create ~fetch ~clock ()] uses [http://marpe:8888/nominatim/] by default.
    Requests are GET-only and restricted to the configured base URL. Each call
    has a 30-second deadline, a 1 MiB response limit and an 8192-byte result
    limit by default. [max_result_bytes] must be at least 256. Model arguments
    cannot select endpoints or credentials. The Fetch client owns retries and
    credentials. Scope restrictions also apply to redirected requests. Invalid
    configuration raises [Invalid_argument]. *)

val names : string list
(** [names] is [address_search] followed by [address_reverse]. *)

val is_tool : string -> bool
(** [is_tool name] tests membership in [names], case-sensitively. *)

val definitions : Agentkit.Agent.Tool.t list
(** [definitions] contains names, descriptions and JSON schemas without
    invocation handlers. Use [tools] for bound implementations. *)

val system_prompt : string
(** [system_prompt] explains attribution, ambiguity and reverse-geocoding
    limits. Append it to the host model instructions. *)

val invoke : t -> string -> string -> (string, string) result
(** [invoke t name arguments] performs one address search or reverse lookup.
    Arguments and outputs are JSON. Results include candidate addresses,
    coordinates, OSM source links and attribution. Invalid inputs and service
    failures return [Error]. An unmatched search returns an empty result list.
    Arguments are limited to 4096 bytes. [address_search] requires [query]
    (1..512 bytes) and accepts [limit] (1..5, default 3). [address_reverse]
    requires numeric [latitude] and [longitude] in geographic range. Both accept
    [language] (1..64 bytes). Blank text and unknown fields are rejected. Output
    has [results], [attribution] and [attribution_url]. Each result has
    [display_name], [latitude], [longitude], [source_url] and [address].
    Oversized results fail without truncation. Timeout and transport failures
    return [Error]. Cancellation propagates. Callers enforce authorization,
    audit privacy and tool budgets. *)

val tools : t -> Agentkit.Agent.Tool.t list
(** [tools t] binds definitions to their implementation for direct Agentkit use.
    Failures are JSON objects with an [error] member. *)
