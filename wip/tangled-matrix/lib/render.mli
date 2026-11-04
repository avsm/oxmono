(** A pipeline as a Matrix message. *)

type t = { plain : string; html : string }
(** A message's [body] and its [org.matrix.custom.html] [formatted_body]. *)

val pipeline : ?appview:string -> name:string -> Tangled.Watch.pipeline -> t
(** [pipeline ~appview ~name p] says how [p] of the repository called [name],
    such as [alice.example/demo], is going: a headline with its status, trigger,
    commit and a link to its page on [appview], then one line per workflow with
    any error. *)

val icon : string -> string
(** [icon status] is the emoji shown for a workflow or pipeline [status]. *)
