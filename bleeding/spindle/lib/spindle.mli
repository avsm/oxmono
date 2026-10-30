(** Tangled CI with trusted OCaml workflows, durable events and service auth. *)

module Job = Job
module Service_auth = Auth
module Operations = Operations
module Catalog = Catalog

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
(** [run system config] serves Tangled CI XRPC through Proffer. Workflow
    definitions are trusted OCaml values; repository code does not configure the
    runner. State, event cursors, JWT nonces and logs persist in SQLite. Jobs
    have a 60-second deadline and a 1 MiB log limit per workflow. *)
