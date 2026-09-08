@@ portable

(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** URI template expansion through {!Httpz_uri.Template}.

    {{:https://datatracker.ietf.org/doc/html/rfc8620#section-2} RFC 8620 Section
     2} gives the session [downloadUrl], [uploadUrl] and [eventSourceUrl] as URI
    templates in the sense of
    {{:https://datatracker.ietf.org/doc/html/rfc6570} RFC 6570}. JMAP only uses
    Level 1 simple string expansion; parsing, UTF-8 validation, percent
    encoding, and expansion are the implementation shared with Httpz.

    @canonical Jmap.Proto.Template *)

val expand :
  vars:(string * string) list ->
  string ->
  (string, Httpz_uri.Template.error) result
(** [expand ~vars template] parses and expands [template]. A name bound by
    [vars] more than once takes its first binding, and an unbound name is
    undefined and omitted. String values are UTF-8 validated and percent encoded
    according to the template operator. A malformed template, an invalid value,
    and an expansion that is not an RFC 3986 URI reference are errors. *)

val expand_template :
  vars:(string * string) list ->
  Httpz_uri.Template.t ->
  (string, Httpz_uri.Template.error) result
(** [expand_template ~vars template] is {!expand} on an already parsed
    [template]. *)
