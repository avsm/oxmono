(** HTTP request methods.

    Represents the nine standard HTTP/1.1 methods defined in
    {{:https://datatracker.ietf.org/doc/html/rfc7231#section-4}RFC 7231 Section 4}.

    {2 Method Semantics}

    - {!Get}, {!Head}: Safe, cacheable, no body expected
    - {!Post}: Not safe, not idempotent, body expected
    - {!Put}, {!Delete}: Idempotent, body optional
    - {!Options}: Safe, cacheable
    - {!Patch}: Not safe, not idempotent, body expected
    - {!Connect}, {!Trace}: Special purpose

    {2 Unknown Methods}

    This library only accepts the standard methods. Custom or extension
    methods will cause parsing to fail with {!Buf_read.Invalid_method}. *)

(** {1 Types} *)

type t =
  | Get      (** GET - retrieve a resource *)
  | Head     (** HEAD - retrieve headers only *)
  | Post     (** POST - submit data for processing *)
  | Put      (** PUT - replace a resource *)
  | Delete   (** DELETE - remove a resource *)
  | Connect  (** CONNECT - establish a tunnel *)
  | Options  (** OPTIONS - describe communication options *)
  | Trace    (** TRACE - echo the request *)
  | Patch    (** PATCH - partial resource modification *)
(** HTTP request method. *)

(** {1 Conversion} *)

val to_string : t -> string
(** [to_string meth] returns the uppercase method name.

    {[
      Method.to_string Get   (* "GET" *)
      Method.to_string Post  (* "POST" *)
    ]} *)

(** {1 Pretty Printing} *)

val pp : Stdlib.Format.formatter -> t -> unit
(** Pretty-print method. *)
