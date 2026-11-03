(** The builds to try for a project. *)

type t = {
  name : string;  (** The workflow's name, such as [debian-12-ocaml-5.4]. *)
  image : string;  (** The Docker image the build starts from. *)
  compiler : string * string;
      (** The compiler package and version the solve fixes. *)
  oxcaml : bool;  (** Whether the OxCaml repository and switch are used. *)
}

val ocaml : string -> string -> t
(** [ocaml minor version] builds with OCaml [version] on the [minor] image. *)

val oxcaml : t
(** [oxcaml] builds with [oxcaml-compiler.5.2.0minus39], as oxmono does. *)

val choose : packages:string -> string list -> t list
(** [choose ~packages minors] is a build for the newest release of each OCaml
    minor version in [minors], as opam-repository's [packages] lists them.
    [["auto"]] picks 4.14 and the two newest minors. *)
