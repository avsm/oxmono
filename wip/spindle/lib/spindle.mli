(** Tangled CI with trusted OCaml workflows, durable events and service auth. *)

module Job = Job
module Executor = Executor
module Service_auth = Auth
module Operations = Operations
module Catalog = Catalog
module Settings = Settings

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
}
(** [config] serves repositories whose knot manifest names this spindle and
    whose owner is admitted. [owner] and [admins] administer the spindle and
    its allow-list. [admission] admits every owner when [Open]. [repo]
    optionally maps a static repository DID to a Git source. [jetstream]
    follows pull requests. [plc] selects the PLC directory. No live ATP
    endpoints are implicit. [allow_http] permits cleartext HTTP and WS for
    development networks. *)

val run :
  ?addr:string ->
  ?operations:Operations.t ->
  Eio_unix.Stdenv.base ->
  config ->
  unit
(** [run system config] serves Tangled CI XRPC through Proffer. It hands the
    OCaml [jobs] and each repository's [.tangled/workflows] YAML to
    [config.executor]. State, event cursors, JWT nonces and logs persist in
    SQLite. A workflow's log may hold at most 1 MiB. *)
