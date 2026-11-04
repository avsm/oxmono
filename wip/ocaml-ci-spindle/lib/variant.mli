(** The builds to try for a project. *)

type arch = X86_64 | Arm64 | Ppc64le | Riscv64

type platform =
  | Debian of arch  (** Debian 13, on the [linux-ARCH] pool. *)
  | Windows  (** Windows Server 2025 with mingw-w64, on [windows-x86_64]. *)

type os = Linux | Windows

type t = {
  name : string;
      (** The workflow's name, such as [OCaml 5.4.1 on Debian 13 (x86_64)]. *)
  image : string;  (** The Docker image the build starts from. *)
  pool : string;  (** The OCluster pool the build runs on. *)
  os : os;
  vars : Solve.platform;  (** The pool's opam variables. *)
  compiler : string * string;
      (** The compiler package and version the solve fixes. *)
  requires : string list;
      (** Packages the solution must also hold, such as the Windows C toolchain
          the image has. *)
  oxcaml : bool;  (** Whether the OxCaml repository and switch are used. *)
}

val platforms : (string * platform) list
(** [platforms] names each platform for the command line: [x86_64], [arm64],
    [ppc64le], [riscv64] and [windows]. *)

val v : ?platform:platform -> string -> t
(** [v ~platform version] builds with OCaml [version], such as [5.4.1], on the
    image for its minor version. [platform] is [Debian X86_64] by default. *)

val oxcaml : t
(** [oxcaml] builds with [oxcaml-compiler.5.2.0minus39] on Debian 13, as oxmono
    does. *)

val version : t -> string
(** [version t] is the version of [t]'s compiler package. *)

val choose : packages:string -> string list -> t list
(** [choose ~packages minors] is a Debian 13 x86_64 build for the newest release
    of each OCaml minor version in [minors], newest first, as opam-repository's
    [packages] lists them. [["auto"]] picks 4.14 and the two newest minors. *)
