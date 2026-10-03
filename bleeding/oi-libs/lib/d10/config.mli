(** D10 configuration.

    Bundles the Eio capabilities and cache parameters needed by all D10
    operations. A single {!t} is constructed at startup and threaded through the
    layer, prefix, and index APIs.

    The [root] path points to the cache directory (typically [~/.cache/oi/]) and
    is used to derive layer and prefix paths. The [os_key] partitions the cache
    by platform so that layers built on different OS/arch combinations never
    collide. *)

type clk = [ `Clock of float ] Eio.Resource.t
(** Monotonic clock capability, used for layer creation timestamps. *)

type t = {
  sys : Sysops.t;
      (** System operations context (subprocess execution, tool paths). *)
  fs : Eio.Fs.dir_ty Eio.Path.t;  (** Filesystem capability for all I/O. *)
  clock : clk;  (** Clock for recording layer creation times. *)
  root : Eio.Fs.dir_ty Eio.Path.t;
      (** Cache root directory (e.g. [~/.cache/oi/]). *)
  os_key : string;  (** Platform key (e.g. ["macos~26~arm64"]), see {!Os_key}. *)
}

val pp : t Fmt.t
(** [pp] renders a one-line debug summary showing the cache root and OS key —
    enough to disambiguate a {!t} in a log line. *)
