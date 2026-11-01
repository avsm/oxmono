(** Eio capabilities and cache parameters shared by d10 operations.

    The caller selects [root] and [os_key]. Platform keys partition layers and
    prefixes. Use {!Os_key.of_platform} for distribution and version keys. *)

type clk = [ `Clock of float ] Eio.Resource.t
(** Wall-clock capability, used for layer creation timestamps. *)

type t = {
  sys : Sysops.t;
      (** System operations context (subprocess execution, tool paths). *)
  fs : Eio.Fs.dir_ty Eio.Path.t;  (** Filesystem capability for all I/O. *)
  clock : clk;  (** Clock for recording layer creation times. *)
  root : Eio.Fs.dir_ty Eio.Path.t;  (** Caller-selected cache root directory. *)
  os_key : string;  (** Platform key (e.g. ["macos~26~arm64"]), see {!Os_key}. *)
}

val pp : t Fmt.t
(** [pp] renders a one-line debug summary showing the cache root and OS key
    enough to disambiguate a {!t} in a log line. *)
