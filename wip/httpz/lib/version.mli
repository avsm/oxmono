(** This module represents HTTP versions supported by Httpz.

    HTTP/1.1 connections are persistent by default; HTTP/1.0 connections close by default.
    A Connection field can override either default. See
    {{:https://www.rfc-editor.org/rfc/rfc9112.html#section-9.3} RFC 9112, Section 9.3}. *)

(** A [t] is a supported HTTP version. *)
type t =
  | Http_1_0 (** [Http_1_0] means HTTP/1.0. *)
  | Http_1_1 (** [Http_1_1] means HTTP/1.1 or a compatible higher HTTP/1 minor version. *)

(** [to_string version] is ["HTTP/1.0"] or ["HTTP/1.1"]. *)
val to_string : t -> string @@ portable

(** [pp formatter version] is the formatter operation that prints [version]'s wire
    spelling. *)
val pp : Stdlib.Format.formatter -> t -> unit @@ portable
