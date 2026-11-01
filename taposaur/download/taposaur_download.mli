(** Single-paper acquisition. The caller supplies HTTP, clock, service
    configuration and a staging writer. No environment variables are read. *)

type t
type capability = { publisher : string; formats : string; access : string;
  requirements : string }
val publisher_capabilities : capability list
(** Implemented acquisition strategies and their CLI prerequisites.
    This inventory does not assert live credentials or publisher access. *)
val publisher_capability : string -> capability option
(** [publisher_capability publisher] uses the same publisher routing as
    acquisition. A match identifies an implemented strategy, not verified
    credentials, entitlement or availability for a particular paper. *)
type failure = Http of int | Timeout | Invalid_content of string
  | Transport | Unavailable of string
type attempt = { source : string; failure : failure }
type result = {
  uid : Taposaur.Uid.t;
  artifacts : Taposaur.Manifest.Artifact.t list;
  attempts : attempt list;
}
val failure_message : failure -> string
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
val fetch : t -> Taposaur_store.t -> Taposaur.Uid.t -> result
(** [fetch t store uid] resolves an identity through Glutton, with DataCite
    fallback for a missing DOI record. It tries a configured publisher,
    then an open-access PDF, then CORE. XML success also attempts a PDF.
    Artifacts are committed independently. Expected remote failures are
    returned and logged. Cancellation and storage failures propagate.
    [artifacts=[]] means acquisition failed. *)
val fetch_url :
  t -> Taposaur_store.t -> Taposaur.Uid.t ->
  format:Taposaur.Manifest.Format.t -> string -> result
(** [fetch_url t store uid ~format url] fetches one explicit PDF or XML URL
    without metadata discovery. It uses source [Url]. Other formats are
    refused. PDF signatures and EOF markers, or a complete non-HTML XML
    document, are required before publication. *)
