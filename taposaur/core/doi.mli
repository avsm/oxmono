(* Copyright (c) 2026 Anil Madhavapeddy. SPDX-License-Identifier: ISC *)

(** Digital Object Identifiers
    ({{:https://www.iso.org/standard/88862.html}ISO 26324}).

    A {!type-t} holds a validated DOI name in the case-folded form the
    standard defines as equivalent. A name is a prefix of digit
    components and a registrant-chosen suffix, separated by the first
    ["/"]. *)

type t : immutable_data
(** A validated DOI name. *)

val of_string : string -> (t, string) result @@ portable
(** [of_string s] parses [s] as a DOI name, or gives the reason it is
    not one. [s] may be bare (["10.1000/182"]), carry the [doi:] label,
    or be a [doi.org] or [dx.doi.org] proxy URL. The label and URL
    forms are percent-decoded. The result is folded to lowercase ASCII,
    which the standard defines as the same name. *)

val to_string : t -> string @@ portable
(** [to_string t] is the bare DOI name, such as ["10.1000/182"]. *)

val equal : t -> t -> bool @@ portable
(** [equal a b] is [true] if [a] and [b] are the same DOI name. *)

val prefix : t -> string @@ portable
(** [prefix t] is the part before the first ["/"], such as
    ["10.1000"]. *)

val registrant : t -> string option @@ portable
(** [registrant t] is the registrant code, the part of the prefix after
    its directory indicator, or [None] when the prefix is a bare
    directory indicator. *)

val suffix : t -> string @@ portable
(** [suffix t] is the registrant-chosen part after the first ["/"]. *)

val to_url : t -> string @@ portable
(** [to_url t] is the HTTP proxy form ["https://doi.org/…"], with the
    prefix and suffix percent-encoded as the standard specifies,
    including any ["/"] or [","] within the suffix. *)

val path : t -> string @@ portable
(** [path t] is the name percent-encoded for embedding in a URL path
    with its ["/"] separators kept, which is the form publisher APIs
    expect. *)

val pp : t Fmt.t
(** [pp] formats a DOI name as {!to_string} does. *)
