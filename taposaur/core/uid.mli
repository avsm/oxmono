(** Canonical archive identities. *)

module Scheme : sig
  type t = Doi | Pmc
  val to_string : t -> string @@ portable
  val of_string : string -> (t, string) result @@ portable
end

type t = private Doi of Doi.t | Pmc of int

val of_string : string -> (t, string) result @@ portable
(** [of_string s] parses a scheme-prefixed UID. *)
val of_input : string -> (t, string) result @@ portable
(** [of_input text] accepts a scheme-prefixed UID, bare DOI, DOI URL or
    tagged PMCID. Bare numbers are not treated as PMC identifiers. *)

val of_id : Scheme.t -> string -> (t, string) result @@ portable
(** [of_id scheme s] normalises an identifier of [scheme]. *)
val to_string : t -> string @@ portable
val scheme : t -> Scheme.t @@ portable
val id : t -> string @@ portable
(** [id t] is the identifier without its scheme. *)
val doi : t -> Doi.t option @@ portable
val pmcid : t -> string option @@ portable
val compare : t -> t -> int @@ portable

val jsont : t Jsont.t @@ portable
