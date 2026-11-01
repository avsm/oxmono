type t
(** A tool's operator configuration term. It is never a model tool. *)

val v : name:string -> doc:string -> (unit -> Jsont.json) Cmdliner.Term.t -> t

val command :
  profile:string Cmdliner.Term.t ->
  run:(profile:string -> (Secret_store.t -> unit) -> int) ->
  t ->
  int Cmdliner.Cmd.t
(** [command ~profile ~run t] composes a tool's configuration term with named
    add, set, list, remove, rename and select subcommands. Secret input is read
    only when executing add or set, after argument parsing. *)

val secret : label:string -> string option -> string
val endpoint : allow_http:bool -> string -> string
val encode : 'a Jsont.t -> 'a -> Jsont.json
val decode : 'a Jsont.t -> Jsont.json -> 'a
