(** HTTP protocol version.

    This library supports HTTP/1.0 and HTTP/1.1. The version affects
    default connection semantics:

    - HTTP/1.0: Connection closes after each request (unless [Connection: keep-alive])
    - HTTP/1.1: Connection persists (unless [Connection: close])

    See {{:https://datatracker.ietf.org/doc/html/rfc7230#section-2.6}RFC 7230 Section 2.6}. *)

(** {1 Types} *)

type t =
  | Http_1_0  (** HTTP/1.0 *)
  | Http_1_1  (** HTTP/1.1 *)
(** HTTP version. *)

(** {1 Conversion} *)

val to_string : t -> string
(** [to_string version] returns the version string.

    {[
      Version.to_string Http_1_0  (* "HTTP/1.0" *)
      Version.to_string Http_1_1  (* "HTTP/1.1" *)
    ]} *)

(** {1 Pretty Printing} *)

val pp : Stdlib.Format.formatter -> t -> unit
(** Pretty-print version. *)
