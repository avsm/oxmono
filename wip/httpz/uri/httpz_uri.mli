(** RFC 3986 URI references, RFC 6570 templates and IP address classification.

    URI references use canonical percent-encoded text. {!Scanner} scans the
    caller's original bytes without copying component strings, {!Template}
    expands URI templates, and {!Ip} recognizes resolver-style IP literals. *)

include module type of Uri

module Template : module type of Uri_template with type uri := t
(** RFC 6570 URI templates and expansion into URI references. *)

module Ip : module type of Ip
(** IP literal recognition and IPv4 canonicalization. *)
