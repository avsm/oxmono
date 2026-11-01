@@ portable

(** RFC 5256 THREAD algorithms.

    Algorithm names compare case-insensitively. *)

type algorithm =
  | Orderedsubject
  | References
  | Other of string
      (** An algorithm this library does not know, by name. *)
(** The type for threading algorithms. *)

val of_wire : string -> algorithm
(** [of_wire s] is the algorithm named [s], read case-insensitively. An
    unknown name is [Other] in uppercase. *)

val to_wire : algorithm -> string
(** [to_wire a] is the uppercase name of [a]. *)

val equal : algorithm -> algorithm -> bool
(** [equal a b] is [true] if [a] and [b] have the same name ignoring case.
    [Other "references"] equals [References]. *)

val pp : Format.formatter -> algorithm -> unit
(** [pp ppf a] prints [to_wire a] on [ppf]. *)
