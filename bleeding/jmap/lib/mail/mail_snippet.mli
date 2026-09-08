@@ portable

(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Search snippets.

    {{:https://datatracker.ietf.org/doc/html/rfc8621#section-5} RFC 8621 Section
     5} defines the SearchSnippet object, which holds the part of an Email that
    matched an [Email/query] filter, with the matching words marked up for
    display.

    @canonical Jmap.Proto.Search_snippet *)

(** {1 Snippets} *)

type t = {
  email_id : Proto_id.t;  (** The Email the snippet is for. *)
  subject : string option;
      (** The subject with the matching words wrapped in [<mark>] elements, or
          [None] if the subject did not match. *)
  preview : string option;
      (** An extract of the body with the matching words wrapped in [<mark>]
          elements, or [None] if the body did not match. *)
}
(** The type for SearchSnippet objects. The marked up values are HTML with every
    other character escaped. *)

val jsont : t Jsont.t
(** [jsont] is the codec for a SearchSnippet. [subject] and [preview] are always
    present and encode as an explicit [null] when they are [None]. *)

(** {1 SearchSnippet/get} *)

type get_response = {
  account_id : Proto_id.t;  (** The account the call was made on. *)
  list : t list;  (** The snippets, in any order. *)
  not_found : Proto_id.t list option;
      (** The Email ids that could not be found, or [None] if they all were. *)
}
(** The type for the response arguments of a [SearchSnippet/get] call (RFC 8621
    Section 5.1). It is not a standard [/get] response. A SearchSnippet has no
    [id] property, the response carries no [state] string, "there is no state
    string or update mechanism needed", and [notFound] is [Id[]|null] rather
    than [Id[]]. *)

val get_response_jsont : get_response Jsont.t
(** [get_response_jsont] is the codec for the response arguments of a
    [SearchSnippet/get] call. [notFound] is always present and encodes as an
    explicit [null] when it is [None]. *)
