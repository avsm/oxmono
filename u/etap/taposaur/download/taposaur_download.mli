(** Single-paper acquisition. The caller supplies HTTP, clock, service
    configuration and a staging writer. No environment variables are read. *)

type t
type capability = { publisher : string; formats : string; access : string;
  requirements : string }
val publisher_capabilities : capability list
(** Implemented acquisition strategies and their CLI prerequisites.
    This inventory does not assert live credentials or publisher access. *)
val strategies : string list
(** [strategies] names the implemented publisher routes: [elsevier],
    [springer], [plos], [wiley], [tandf] and [cup]. Queue names and job
    payloads use these stable names. *)
val strategy_name : string -> string option
(** [strategy_name publisher] is the implemented strategy that acquisition
    would route [publisher] to, or [None] for unknown publishers. *)
val prefix_publisher : string -> string option
(** [prefix_publisher doi] names the implemented publisher that owns the
    Crossref prefix of [doi] (Elsevier 10.1016; Springer 10.1007 and
    10.1023; PLOS 10.1371; Wiley 10.1002, 10.1111 and 10.1046; Taylor &
    Francis 10.1080; Cambridge 10.1017), or [None]. Planning uses it for a
    member with no publisher evidence; the strategy still resolves the DOI
    itself and refuses content that is not the article. *)
val publisher_capability : string -> capability option
(** [publisher_capability publisher] uses the same publisher routing as
    acquisition. A match identifies an implemented strategy, not verified
    credentials, entitlement or availability for a particular paper. *)
type failure = Http of int | Timeout | Invalid_content of string
  | Transport | Unavailable of string
type attempt = { source : string; failure : failure; retry_after : float option }
(** [retry_after] is the server's [Retry-After] delay in seconds, when a
    busy or unavailable response supplied one as a delay. HTTP-date forms
    are not interpreted. *)
type result = {
  uid : Taposaur.Uid.t;
  artifacts : Taposaur.Manifest.Artifact.t list;
  attempts : attempt list;
}
val failure_message : failure -> string
val retry_after : Fetch.response -> float option
(** [retry_after response] is the [Retry-After] header as a nonnegative
    delay in seconds, or [None] when absent or given as an HTTP date. *)
val create :
  ?audit:Taposaur_audit.session ->
  ?timeout:float -> ?max_bytes:int ->
  ?lookup_timeout:float -> ?busy_retries:int -> ?busy_wait:float ->
  ?cup:Taposaur_services.Cup.t ->
  ?plos:Taposaur_services.Plos.t ->
  ?tandf:Taposaur_services.Tandf.t ->
  ?springer_pdf:Taposaur_services.Springer.t ->
  ?glutton:Taposaur_services.Glutton.t ->
  ?datacite:Taposaur_services.Datacite.t ->
  ?elsevier:Taposaur_services.Elsevier.t ->
  ?springer:Taposaur_services.Springer.t ->
  ?wiley:Taposaur_services.Wiley.t ->
  ?core:Taposaur_services.Core.t ->
  clock:_ Eio.Time.clock -> _ Fetch.t -> t
(** [create ~clock client] configures acquisition with a whole-transfer
    timeout (default 120 seconds) and artifact limit (default 128 MiB).
    Metadata defaults to at most 15 seconds. Publisher HTTP 429/500/502/503/504
    responses are retried three times with 300-second waits outside each
    transfer deadline. Set [busy_retries=0] to disable. Pacing belongs on
    supplied clients. Springer PDF entitlement is tried only when
    [springer_pdf] is supplied. *)
val fetch : ?audit:Taposaur_audit.session -> t -> Taposaur_store.t ->
  Taposaur.Uid.t -> result
(** [fetch t store uid] resolves an identity through Glutton, with DataCite
    fallback for a missing DOI record. It tries a configured publisher,
    then an open-access PDF, then CORE. XML success also attempts a PDF.
    Artifacts are committed independently. Expected remote failures are
    returned and logged. Cancellation and storage failures propagate.
    [artifacts=[]] means acquisition failed. [audit] replaces the session
    given at creation, so a caller's own operation can enclose the
    acquisition's records. *)
val fetch_url :
  ?audit:Taposaur_audit.session -> t -> Taposaur_store.t -> Taposaur.Uid.t ->
  format:Taposaur.Manifest.Format.t -> string -> result
(** [fetch_url t store uid ~format url] fetches one explicit PDF or XML URL
    without metadata discovery. It uses source [Url]. Other formats are
    refused. PDF signatures and EOF markers, or a complete non-HTML XML
    document, are required before publication. *)
