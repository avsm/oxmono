(** Platform key for cache directory partitioning.

    Layers and prefixes are partitioned by platform so that binaries compiled on
    different OS/architecture combinations never collide. The key is serialised
    as [{distro}~{os_version}~{arch}] with [~] as separator (e.g.
    ["macos~26~arm64"] or ["ubuntu~24.04~x86_64"]).

    This is more granular than GNU autoconf triples ([aarch64-apple-darwin]) or
    Rust targets ([aarch64-apple-darwin]) which don't distinguish Linux
    distributions or versions. Binary compatibility on Linux varies by distro
    (glibc vs musl, different shared library versions), so the key must capture
    the distribution and version to avoid cache collisions.

    The components correspond to opam's platform variables: [os-distribution],
    [os-version], and [arch]. On macOS, the version is truncated to the major
    version. Alpine uses major.minor. These are cache partitioning conventions,
    not guarantees that arbitrary binaries are compatible. *)

type t = {
  os : string;  (** OS family (e.g. ["macos"], ["linux"]). *)
  distro : string;  (** Distribution name (e.g. ["ubuntu"], ["macos"]). *)
  os_version : string;  (** OS version (e.g. ["26"], ["24.04"]). *)
  arch : string;  (** CPU architecture (e.g. ["arm64"], ["x86_64"]). *)
}

val of_platform : Osrel.t -> t
(** [of_platform p] constructs a key from the detected {!Osrel.t} platform. *)

val to_string : t -> string
(** [to_string t] serialises as [{distro}~{os_version}~{arch}]. *)

val of_string : string -> t
(** [of_string s] parses three tilde-separated components. macOS aliases produce
    [os = "macos"]. Other three-component keys produce [os = "linux"].
    Unrecognized shapes are retained as [distro = s] with [os = "unknown"] and
    empty version and architecture. This is a permissive decoder, not validation
    or a round trip for non-Linux, non-macOS platforms. *)

val pp : t Fmt.t
(** [pp] renders a key in {!to_string} form ([distro~os_version~arch]). *)
