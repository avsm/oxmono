(** OS and platform detection via Eio.

    Detects the current CPU architecture, operating system, distribution,
    version, package manager, and available parallelism by running system
    commands and/or reading [{/etc,/usr/lib}/os-release]. *)

(** {1 Architecture}

    CPU architecture as reported by [uname -m]. Normalises common aliases (e.g.
    [amd64] to [x86_64], [aarch64] to [arm64]) and recognises ARM variants
    ([armv7l], [earmv6hf], etc.) as [`Arm32]. *)

module Arch : sig
  type t =
    [ `X86_64
    | `X86_32
    | `Arm64
    | `Arm32
    | `Ppc64
    | `Ppc32
    | `S390x
    | `Riscv64
    | `Unknown of string ]

  val of_string : string -> t
  (** [of_string s] parses an architecture string. Case-insensitive. *)

  val to_string : t -> string
  (** [to_string t] is the canonical lowercase name (e.g. ["arm64"]). *)

  val pp : t Fmt.t
  (** [pp ppf t] prints the canonical architecture name. *)
end

(** {1 Operating system}

    Detects the OS kernel. The {!OS.type-kind} type captures the full detail
    (e.g. [`Linux `Ubuntu]), while {!OS.os_to_string} gives the broad OS name
    (["linux"]) and {!OS.kind_to_string} gives the specific distribution
    (["ubuntu"]). *)

module OS : sig
  type linux =
    [ `Alpine
    | `Android
    | `Arch
    | `CentOS
    | `Debian
    | `Fedora
    | `Gentoo
    | `NixOS
    | `OpenSUSE
    | `RHEL
    | `Ubuntu
    | `Other of string ]
  (** Linux distribution, identified from the [ID] field in [os-release]. *)

  type macos = [ `Homebrew | `MacPorts | `None ]
  (** macOS package manager detected from standard paths or PATH. Homebrew takes
      precedence when both managers are found. *)

  type kind =
    [ `Linux of linux
    | `MacOS of macos
    | `FreeBSD
    | `OpenBSD
    | `NetBSD
    | `DragonFly
    | `Win32
    | `Cygwin
    | `Unknown of string ]
  (** Full OS identification including distribution/package manager. *)

  type t = {
    kind : kind;  (** Detailed OS identification. *)
    version : string;  (** OS version (e.g. ["24.04"], ["15.2"]). *)
    family : string;
        (** On Linux, the first space-separated word of [ID_LIKE], lowercased,
            or the distribution name when absent. Else [kind_to_string kind]. *)
  }

  val kind_to_string : kind -> string
  (** [kind_to_string kind] is its distribution or package-manager name, such as
      ["ubuntu"], ["homebrew"] or ["freebsd"]. *)

  val os_to_string : kind -> string
  (** [os_to_string kind] is its broad OS name, such as ["linux"], ["macos"] or
      ["freebsd"]. *)

  val to_string : t -> string
  (** [to_string t] is [os_to_string t.kind]. *)

  val pp : t Fmt.t
  (** [pp ppf t] prints the broad OS name, as {!to_string}. *)

  val pp_kind : kind Fmt.t
  (** [pp_kind ppf kind] prints [kind_to_string kind]. *)
end

(** {1 Platform}

    Combines architecture, OS, and available parallelism into a single record.
    Detected once at startup via {!detect}. *)

type t = {
  arch : Arch.t;  (** CPU architecture. *)
  os : OS.t;  (** Operating system with distribution and version. *)
  jobs : int;
      (** Number of available CPUs (from [getconf _NPROCESSORS_ONLN], defaults
          to 4 if detection fails). *)
}

val detect : proc_mgr:_ Eio.Process.mgr -> fs:_ Eio.Path.t -> t
(** [detect ~proc_mgr ~fs] probes the system for architecture, OS, distribution,
    version, and CPU count. Reads [/etc/os-release] for Linux distribution
    identification, falling back to [/usr/lib/os-release]. Unknown architectures
    and kernels retain their reported names. Missing version data yields
    ["unknown"]. *)

val pp : t Fmt.t
(** [pp ppf t] prints architecture, OS, version, family and job count. *)
