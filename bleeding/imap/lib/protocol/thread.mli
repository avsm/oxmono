(** RFC 5256 THREAD algorithms. Names compare case-insensitively. *)

type algorithm =
  | Orderedsubject
  | References
  | Other of string
      (** An algorithm this library does not know, by name. *)

val of_wire : string -> algorithm
(** [of_wire s] is the algorithm named [s], read case-insensitively. An
    unknown name is [Other] in uppercase. *)

val to_wire : algorithm -> string
(** [to_wire a] is the uppercase name of [a]. *)

val equal : algorithm -> algorithm -> bool
(** [equal a b] holds when [a] and [b] have the same name ignoring case.
    [Other "references"] equals [References]. *)

val pp : Format.formatter -> algorithm -> unit
(** [pp] prints {!to_wire}. *)
