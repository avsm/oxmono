(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** What a dune workspace is made of.

    [dune describe workspace] reports every library and executable dune can see,
    the ones built here and the ones installed alongside, each with its object
    directories, its compiled artefacts and a hash for a name. This module keeps
    the part that answers where code lives and what it may refer to: the
    components of the workspace, the directory each is built from, the modules
    it is made of and the libraries it requires, with paths written as the
    source tree writes them.

    The map is taken by running dune in a private build directory, so it may be
    taken while a {!Session} session holds the workspace's own build lock. *)

type module_ = {
  name : string;  (** The module name, capitalised as OCaml writes it. *)
  impl : string option;  (** The implementation, relative to the root. *)
  intf : string option;  (** The interface, for a module that has one. *)
}

type component = {
  kind : [ `Library | `Executable ];  (** Which stanza dune built it from. *)
  name : string;
      (** The library name, or the executables' names separated by a space. *)
  local : bool;  (** Whether it is built from this workspace. *)
  requires : string list;
      (** The libraries it depends on, by name, in the order dune reports. Dune
          names a dependency by a hash, and one whose hash belongs to no library
          in the report is left out. *)
  source_dir : string;
      (** The directory it is built from, relative to the root, and empty for
          the root itself. It is dune's own directory for an installed library.
      *)
  modules : module_ list;
      (** The modules it is made of, empty for a component that is not local. *)
}

type t = {
  root : string;  (** The workspace root, as an absolute path. *)
  components : component list;
}

val describe :
  ?trace:(string -> unit) ->
  proc:_ Eio.Process.mgr ->
  root:Eio.Fs.dir_ty Eio.Path.t ->
  unit ->
  (t, string) result
(** [describe ~proc ~root ()] is the map of the workspace at [root], read from
    [dune describe workspace --format csexp] run there.

    Dune runs with a build directory of its own, made under the system temporary
    directory and removed afterwards, so the call does not wait on the lock a
    watching server holds and does not disturb what that server has built. It
    costs a fresh load of the workspace each time, which on a large one is
    seconds.

    The error names what failed, and carries what dune wrote on its standard
    error when dune is what failed.

    [trace] is called with one short line when the call starts and one for what
    it found, so that a caller can show that a slow load is under way. It
    defaults to {!ignore}, and an exception it raises is swallowed. *)

val to_text : t -> string
(** [to_text t] is the map as lines for a model to read: one block for each
    local component naming its directory, what it requires and its modules with
    their files, then the installed libraries named once at the end. The text is
    bounded, and says so where it stops. *)

val module_text : t -> name:string -> string
(** [module_text t ~name] is the component that holds the module [name], its
    directory and what it requires, with that one module and its files. A module
    of that name in more than one component gives one block for each, which is
    how a module built into two libraries is told apart.

    [name] may be written as OCaml writes it, ["Report"], or as the file that
    holds it, ["okit/report.ml"]. A name no component holds says so rather than
    answering with nothing, since a module that is not built is a different
    thing from a workspace with nothing in it. *)
