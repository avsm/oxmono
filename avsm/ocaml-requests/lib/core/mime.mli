(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** MIME type handling *)

(** Log source for MIME type operations *)
val src : Logs.Src.t

type t
(** Abstract MIME type *)

val of_string : string -> t
(** Parse MIME type from string (e.g., "text/html; charset=utf-8") *)

val to_string : t -> string
(** Convert MIME type to string representation *)

val pp : Format.formatter -> t -> unit
(** Pretty printer for MIME types *)

(** Common MIME types *)
val json : t
val text : t
val html : t
val xml : t
val form : t
val octet_stream : t
val multipart_form : t

val make : string -> string -> t
(** [make type subtype] creates a MIME type *)

val with_charset : string -> t -> t
(** Add or update charset parameter *)

val with_param : string -> string -> t -> t
(** [with_param key value t] adds or updates a parameter in the MIME type.
    Example: [with_param "boundary" "----WebKit123" multipart_form]
    produces "multipart/form-data; boundary=----WebKit123" *)

val charset : t -> string option
(** Extract charset parameter if present *)
