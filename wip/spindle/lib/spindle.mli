(** Tangled CI with trusted OCaml workflows, durable events and service auth. *)

module Job = Job
module Service_auth = Auth
module Operations = Operations

type config = {
  hostname : string;
  owner : string;
  repo : (string * string) option;
  plc : string;
  state_dir : string;
  port : int;
  jobs : Job.t list;
  jetstream : string option;
  allow_http : bool;
}
(** [config] discovers member repositories through the explicitly configured
    [jetstream]. [repo] optionally maps a static repository DID to a Git source.
    [plc] selects the PLC directory; no live ATP endpoints are implicit.
    [allow_http] permits cleartext HTTP and WS for development networks. *)

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
