(** Common module for XML parsing. *)

(** The type for the optional {{:http://www.w3.org/TR/REC-xml/#dt-doctype}DTD}. *)
type dtd = string option

type pos = Xmlm.pos
type tag = Xmlm.tag

(** A XML tree. *)
type t = Node of pos * tag * t list | Data of pos * string

val resolve : xmlbase:Uriz.t option -> Uriz.t -> Uriz.t
(** [resolve base uri] resolve the [uri] against the possible base. *)

val uri_of_string : string -> Uriz.t
(** [uri_of_string s] parses [s] as a URI reference, repairing it if it is not
    one. It never raises and never fails, because a feed carries whatever its
    publisher wrote and a parser must not stop there.

    [s] is first parsed as it stands. If that fails, the bytes that no
    component of a URI reference admits are percent-encoded and the result is
    parsed again. That pass encodes a space, a control character, a raw UTF-8
    byte, a bracket outside the authority, a ['%'] that does not start a
    triplet, and a second ['#']. It leaves the scheme, the authority and the
    segment boundaries where they are, so a href that is merely unencoded
    comes back as the URI its publisher meant.

    If the repaired text is still not a reference, as happens for a
    non-numeric port or a first segment holding a colon, the whole of [s] is
    percent-encoded down to the unreserved set. The result is then a single
    path segment carrying the original text, which is a valid relative
    reference but no longer names anything. Compare it, do not follow it. *)

val get_position : t -> pos
val input_of_channel : in_channel -> Xmlm.input

val of_xmlm : Xmlm.input -> dtd * t
(** [of_xmlm doc] converts an XML document [doc] into a DTD and a tree
    representing the document. *)

val make_output :
  ?ns_prefix:(string -> string option) -> Xmlm.dest -> Xmlm.output

val to_xmlm : ?dtd:string -> t -> Xmlm.output -> unit
val to_string : ?ns_prefix:(string -> string option) -> t -> string
val to_buffer : ?ns_prefix:(string -> string option) -> t -> Buffer.t -> unit
