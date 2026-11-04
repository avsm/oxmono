(** The repositories a poster reports, and the rooms each is reported in. *)

type t = {
  repo : Tangled.Api.repository;
  rooms : string list;  (** Room IDs, without repeats. *)
}

val read :
  Tangled.Api.t ->
  accounts:string list ->
  bot:string ->
  ?on_error:(string -> exn -> unit) ->
  unit ->
  t list
(** [read api ~accounts ~bot ()] is every repository that one of [accounts],
    given as DIDs, has bound to a room for the Matrix user [bot] with
    [tangled pipeline matrix set]. A binding counts only when its author owns
    the repository or the repository's knot records them as a collaborator, and
    the repository names a spindle. An account or binding that cannot be read is
    passed to [on_error], which logs it by default, and left out. *)

(**/**)

val rooms :
  repo:string ->
  owner:string ->
  collaborators:string list Lazy.t ->
  (string * (string * string)) list ->
  string list
(** [rooms ~repo ~owner ~collaborators bindings] is the rooms that [bindings],
    given as [(repository, (author, room))], bind [repo] to with an author who
    is [owner] or one of [collaborators]. *)
