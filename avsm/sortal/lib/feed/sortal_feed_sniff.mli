(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Feed format detection from document bytes.

    The recorded feed type and the HTTP [Content-Type] header can both be
    wrong: a server may serve Atom labelled [rss] with a [Content-Type] of
    [application/rss+xml]. Detection looks at the document itself. *)

type t =
  | Atom
  | Rss
  | Json
  | Unknown of string
      (** the body's first bytes, truncated, when nothing recognisable was
          found *)

val detect : string -> t
(** [detect body] is [body]'s apparent format. It skips an optional
    leading XML declaration and whitespace, then looks at the first
    element: [<feed] is Atom, [<rss] or [<rdf:RDF] is RSS, and a body
    whose first non-whitespace byte is ['{'] is JSON Feed. Anything else
    ahead of the root element, such as an HTML-escaped XML declaration, is
    not tolerated: detection does not skip past it, so the result is
    [Unknown]. *)
