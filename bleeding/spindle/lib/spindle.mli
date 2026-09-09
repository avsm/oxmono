(** A single-owner Tangled spindle with trusted OCaml jobs. *)

module Job = Job
module Service_auth = Auth

type config = {
  hostname : string;
  owner : string;
  repo : string;
  source : string;
  plc : string;
  state_dir : string;
  port : int;
  job : Job.t;
}
(** [config] maps one repository DID to a server-configured Git source and
    one authorized owner DID. [plc] is the explicitly configured PLC HTTP(S)
    origin. [state_dir] stores pipeline state, logs and temporary checkouts. *)

val run : ?addr:string -> Eio_unix.Stdenv.base -> config -> unit
(** [run system config] serves CI XRPC with Proffer. [addr] defaults to
    loopback. Manual triggers require an ES256K service JWT issued for the
    configured owner, spindle DID and method. Jobs run in child processes
    with a 60-second deadline and 1 MiB log limit. *)
