(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** What merlin knows about a piece of OCaml source.

    Merlin answers the questions a compiler does not: the type of the expression
    under a position, where a name is defined, what a file declares. Each query
    here runs [ocamlmerlin single], hands it the source on standard input and
    reads the one JSON reply, so a query needs no authority over the file system
    and nothing is kept between calls. The source may be text that was never
    written to disk.

    Merlin reads the build configuration of the directory a query runs in, so
    the answers are as good as that configuration. In a workspace dune has
    built, a name from another library resolves. In one it has not, the modules
    it cannot find are reported as errors and the rest is still answered. *)

type t
(** A merlin answering about one workspace. *)

val find :
  ?trace:(string -> unit) ->
  proc:_ Eio.Process.mgr ->
  root:Eio.Fs.dir_ty Eio.Path.t ->
  unit ->
  t option
(** [find ~proc ~root ()] is a merlin for the workspace at [root], or [None]
    when no ocamlmerlin binary answers on PATH. Every query runs with [root] as
    its working directory, which is where merlin looks for the configuration.

    [trace] is called with one short line for each query this merlin runs and
    for what it answered, so that a caller can show what a query is waiting on.
    A query is named before it begins rather than once it has returned. It
    defaults to {!ignore}, and an exception it raises is swallowed. *)

val trace : t -> string -> unit
(** [trace t s] passes [s] to the callback {!find} was given, guarded as the
    queries' own steps are. A tool built on a merlin names itself through this,
    so that what a caller shows is one sequence rather than two. *)

type pos = { line : int; col : int }
(** A position in a file. [line] counts from 1 and [col] counts from 0, which is
    how merlin and the OCaml compiler both write them. *)

type outline_item = {
  name : string;  (** The name declared. *)
  kind : string;
      (** What kind of declaration it is, in merlin's words: ["Value"],
          ["Type"], ["Module"], ["Exception"] and so on. *)
  typ : string option;
      (** The type, for a declaration that has one to state. *)
  pos : pos;  (** Where the declaration begins. *)
  children : outline_item list;
      (** What it declares in turn, such as the contents of a module. *)
}
(** One declaration of a file, and what it contains. *)

val outline :
  t -> path:string -> source:string -> (outline_item list, string) result
(** [outline t ~path ~source] is what [source] declares, in source order,
    outermost first.

    [path] is relative to the workspace root and need not name a file that
    exists. It tells merlin which configuration applies and whether to read
    [source] as an implementation or an interface. *)

val type_at :
  t -> path:string -> source:string -> pos -> (string, string) result
(** [type_at t ~path ~source p] is the type of the smallest expression enclosing
    [p], as merlin prints it. A position that no expression covers is an error
    saying so. *)

val locate :
  t ->
  path:string ->
  source:string ->
  pos ->
  ([ `Found of string * pos | `Not_found of string ], string) result
(** [locate t ~path ~source p] is where the name at [p] is defined.

    [`Found (file, pos)] gives an absolute path, which is outside the workspace
    when the definition is in an installed library. [`Not_found why] carries
    merlin's account of why there is nowhere to go, such as being at the
    definition already or standing on something that is not a name. *)

type problem = {
  at : pos option;
      (** Where it starts, and [None] for a syntax error the parser could not
          place. *)
  warning : bool;  (** Whether it is a warning rather than something fatal. *)
  message : string;  (** What merlin has to say, as the compiler would say it. *)
}
(** Something merlin objects to. *)

val errors : t -> path:string -> source:string -> (problem list, string) result
(** [errors t ~path ~source] is what merlin makes of [source], in the order it
    reports them. It type-checks [source] against what the workspace has already
    built, so a module changed but not rebuilt is checked against the last build
    of it. *)

type place = {
  file : string;
      (** The file, as merlin writes it. An occurrence gives an absolute path. A
          search result gives the path from the workspace root, except for one
          in the file the query itself was made from, which is the bare file
          name, as is one in an installed library, whose source merlin has no
          path to. *)
  pos : pos;  (** Where in it. *)
}
(** Somewhere merlin found. *)

val occurrences :
  t -> path:string -> source:string -> pos -> (place list, string) result
(** [occurrences t ~path ~source p] is every use of the name at [p], across the
    whole project, in the order merlin reports them. The definition is one of
    them, and a name declared in an interface and defined in an implementation
    has both.

    Merlin reads the uses outside [source] from the index dune writes for the
    [@ocaml-index] alias. Where that index is missing or stale, the answer is
    the uses in [source] alone, and merlin does not say so. Build the alias
    first. *)

type hit = {
  name : string;  (** The qualified name, such as ["List.map"]. *)
  typ : string;  (** Its type. *)
  place : place;  (** Where it is declared. *)
}
(** A value merlin found by its type. *)

val search :
  t ->
  path:string ->
  source:string ->
  query:string ->
  limit:int ->
  (hit list, string) result
(** [search t ~path ~source ~query ~limit] is at most [limit] values whose type
    is close to [query], best match first. [query] is a type expression such as
    ["int -> string"], and a value whose arguments are in another order or which
    takes more of them still matches.

    Everything the build configuration of [path] makes visible is searched,
    which is this workspace and every library it may refer to, not only what
    [source] has opened. [source] supplies that configuration and nothing else,
    so it need not be the file the answer lies in.

    Merlin reports a value once per path it can be reached by, so the same name
    and type may come back more than once. *)

type completion = {
  name : string;  (** The name, without the prefix that was asked for. *)
  kind : string;
      (** What it is, in merlin's words: ["Value"], ["Module"], ["Type"],
          ["Constructor"] and so on. *)
  typ : string;
      (** Its type, and empty for a completion with none to state, such as a
          module. *)
}
(** Something that can be named at a position. *)

val complete :
  t ->
  path:string ->
  source:string ->
  pos ->
  prefix:string ->
  (completion list, string) result
(** [complete t ~path ~source p ~prefix] is everything that can be named at [p]
    whose name begins with [prefix], in the order merlin reports them, which is
    alphabetical within a module.

    [prefix] is qualified as it would be written at [p], so ["List."] is what
    that module offers and ["fo"] is what is in scope beginning with those
    letters. This reaches an installed library, whose source is not in the
    workspace and so has no file to outline. *)
