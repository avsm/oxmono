(** Which Matrix message reports which pipeline, kept across restarts.

    Each pipeline has one message per room. Once sent, the message is edited in
    place, so the store keeps the event to replace and what it last said. *)

type entry = {
  repo : string;  (** The repository's DID. *)
  pipeline : string;
  room : string;
  mutable event : string option;  (** The message's event ID, once sent. *)
  mutable finished : bool;  (** Whether it reports a finished pipeline. *)
  mutable shown : string;  (** The plain text the room last received. *)
}

type t

val load : _ Eio.Path.t -> t
(** [load path] reads the store at [path], or is empty if there is none. A file
    that does not parse is moved aside to [path.bad], so the poster starts
    afresh rather than not at all. *)

val save : t -> unit
(** [save t] writes [t] back atomically, readable only by its owner. *)

val find : t -> repo:string -> pipeline:string -> room:string -> entry option

val add : t -> entry -> unit
(** [add t e] records [e]. At most 1000 entries are kept, dropping the oldest
    finished ones first. *)

val unfinished : t -> (string * string) list
(** [unfinished t] is the [(repository DID, pipeline)] of every entry whose
    pipeline had not finished, to resume after a restart. *)

val entries : t -> entry list
