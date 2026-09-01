(*
 * Copyright (c) 2026 Anil Madhavapeddy <anil@recoil.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

(** RFC 6570 URI Templates.

    Templates support Level 1 through Level 4 expressions: simple, reserved,
    fragment, label, path, path-parameter, query and query-continuation,
    including prefix and explode modifiers.

    Templates and values are UTF-8 strings. Expansion percent-encodes their
    UTF-8 octets as required by RFC 6570. No Unicode normalization is applied;
    callers accepting human input should normalize it before binding it.

    Variable values encode bytes outside the RFC 3986 unreserved set.
    Reserved [{+var}] and fragment [{#var}] expressions also preserve reserved
    characters and valid percent triplets, allowing values to change URI
    structure. *)

type t : immutable_data
(** A validated Level 4 URI Template. *)

type value =
  [ `String of string
  | `List of string list
  | `Assoc of (string * string) list ]
(** A variable value. An empty list or association is undefined for expansion;
    an empty string is defined. Association order and duplicate names are
    preserved. *)

type error = private {
  offset : int;
  message : string;
}
(** A template parse or expansion error at a byte offset. *)

val pp_error : Format.formatter -> error -> unit @@ portable

val of_string : string -> (t, error) result @@ portable
(** [of_string s] parses the complete RFC 6570 Level 4 template [s]. Reserved
    extension operators ([=], [,], [!], [@], and [|]) are rejected because RFC
    6570 assigns them no expansion semantics. *)

val of_string_exn : string -> t @@ portable
(** [of_string_exn s] is [of_string s], raising [Invalid_argument] on error. *)

val to_string : t -> string @@ portable
(** [to_string t] is the original template text. *)

val pp : Format.formatter -> t -> unit @@ portable
[@@ocaml.toplevel_printer]

val variables : t -> string list @@ portable
(** [variables t] lists variable names once each, in first-use order. Percent
    triplets in names retain their template spelling and are not decoded. *)

type level = [ `Level_1 | `Level_2 | `Level_3 | `Level_4 ]
(** The minimum RFC 6570 feature level needed by a template's syntax. This
    classifies operators, multiple-variable expressions, and modifiers. A
    caller supplying a composite value still needs Level 4 semantics even when
    the expression itself has Level 1 syntax. *)

val level : t -> level @@ portable
(** [level t] is the minimum RFC 6570 feature level used by [t]. A template
    containing no expressions is Level 1. *)

val expand :
  t -> (string -> value option) -> (string, error) result @@ portable
(** [expand t lookup] expands [t], consulting [lookup] once for each distinct
    variable name. [None] is an undefined variable. A prefix modifier applied
    to a composite value, or a value that is not valid UTF-8, is an error. *)

val expand_assoc :
  t -> (string * value) list -> (string, error) result @@ portable
(** [expand_assoc t bindings] expands using the first binding for each name. *)

val expand_uri :
  t -> (string -> value option) -> (Uriz.t, error) result @@ portable
(** [expand_uri] expands and parses the result as an RFC 3986 URI reference.
    It can fail even for a valid expansion when an unrestricted reserved value
    is inappropriate at its position in the surrounding URI. *)

val expand_uri_assoc :
  t -> (string * value) list -> (Uriz.t, error) result @@ portable
(** [expand_uri_assoc] is [expand_uri] with association-list bindings. *)

val expand_resolve :
  base:Uriz.t @ local ->
  t ->
  (string -> value option) ->
  (Uriz.t, error) result
  @@ portable
(** [expand_resolve ~base template lookup] expands [template] with the actual
    bindings, parses the result as a URI reference, and resolves that reference
    against [base] according to RFC 3986 Section 5.2. [base] should be absolute. *)

val expand_resolve_assoc :
  base:Uriz.t @ local ->
  t ->
  (string * value) list ->
  (Uriz.t, error) result
  @@ portable
(** [expand_resolve_assoc] is {!expand_resolve} with association-list bindings. *)
