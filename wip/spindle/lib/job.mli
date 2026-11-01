(** Trusted OCaml workflows and event selection. *)

type kind = Push | Pull_request | Manual

type context = {
  repo : string;
  actor : string;
  commit : string;
  kind : kind;
  ref_ : string option;
  changed_files : string list;
  default_ref : bool;
  request : Jsont.json;
}

type step =
  | Metadata
  | Command of string list
      (** Commands run directly, without a shell, in a detached checkout. *)

type t = private { name : string; steps : step list; accepts : context -> bool }

val v : ?accepts:(context -> bool) -> string -> step list -> t
(** [v ~accepts name steps] defines a workflow in trusted OCaml. [accepts]
    selects events using repository, trigger, branch and changed paths. *)

val inspect : t
