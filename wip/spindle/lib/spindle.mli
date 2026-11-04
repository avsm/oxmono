(** Tangled CI with trusted OCaml workflows, durable events and service auth. *)

module Job = Job
module Executor = Executor
module Demo = Demo
module Service_auth = Service_auth
module Operations = Operations

module Catalog : sig
  type admission = Catalog.admission =
    | Open  (** Any owner may use the spindle. *)
    | Closed  (** Only admins and allow-listed owners may. *)
end

module Settings : sig
  type defaults = Settings.defaults = {
    repo : (string * int) list;
    user : (string * int) list;
  }
  (** Default limits by resource, for repositories and for their owners. A
      missing or zero limit is unlimited. *)
end

type config = {
  hostname : string;
  owner : string;
  admins : string list;
  admission : Catalog.admission;
  repo : (string * string) option;
  plc : string;
  state_dir : string;
  port : int;
  jobs : Job.t list;
  jetstream : string option;
  allow_http : bool;
  executor : Executor.t;
      (** Runs each workflow. The spindle runs no workflow commands itself. *)
  workflow_timeout : float;  (** Seconds a workflow may run. *)
  quotas : Settings.defaults;  (** Default limits. *)
  private_webhooks : bool;
      (** Let webhooks reach private addresses, on a development network. *)
  cors_origins : string list;
      (** Origins whose browser pages may call XRPC methods, or ["*"] for any.
          The empty list sends no CORS fields. *)
  contact_email : string option;  (** An address [describeSpindle] reports. *)
  privacy_policy : string option;  (** A URL [describeSpindle] links to. *)
  terms_of_service : string option;  (** A URL [describeSpindle] links to. *)
  concurrency : int;
      (** Workflows that may run at once. An executor that waits on remote
          builders can allow many. *)
  repository_workflows : bool;
      (** Whether each repository's [.tangled/workflows] also run. A spindle
          whose executor runs only its own [jobs] turns them off. *)
  web : bool;
      (** Whether to serve the web pages of {!Web} at [/ui], for the spindle's
          users to log in to with atproto. They need the spindle to be reached
          at [https://hostname]. *)
}
(** [config] serves repositories whose knot manifest names this spindle and
    whose owner is admitted. [owner] and [admins] administer the spindle and its
    allow-list. [admission] admits every owner when [Open]. [repo] optionally
    maps a static repository DID to a Git source. [jetstream] follows pull
    requests. [plc] selects the PLC directory. No live ATP endpoints are
    implicit. [allow_http] permits cleartext HTTP and WS for development
    networks. *)

val run :
  ?addr:string ->
  ?operations:Operations.t ->
  Eio_unix.Stdenv.base ->
  config ->
  unit
(** [run system config] serves Tangled CI XRPC through Proffer. It hands the
    OCaml [jobs], and each repository's [.tangled/workflows] YAML when
    [repository_workflows] is set, to [config.executor]. State, event cursors,
    JWT nonces and logs persist in SQLite. A workflow's log may hold at most 1
    MiB. *)
