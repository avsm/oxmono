(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** {1 Zotero Translation Server Client}

    OCaml client for the {{:https://github.com/zotero/translation-server}Zotero
    Translation Server}, which provides DOI/URL resolution and bibliographic
    format export. *)

module Bibtex = Bibtex
(** BibTeX parsing and formatting without filesystem access. *)

(** {1 Session} *)

type t
(** A Zotero Translation Server client. *)

val of_fetch : base_url:string -> ?max_response_bytes:int -> _ Fetch.t -> t
(** [of_fetch ~base_url fetch] uses [fetch] to access the translation server
    at [base_url]. The caller owns the backend and its lifetime. The client
    narrows this capability to POST requests beneath [base_url]. Redirects
    are disabled. No network or filesystem access occurs during construction.

    [max_response_bytes] defaults to 16 MiB. Error bodies are bounded by the
    smaller of this limit and 64 KiB. Oversized error bodies are replaced by
    a diagnostic while preserving the HTTP status in {!Api_error}.

    @raise Invalid_argument if [base_url] is not an absolute HTTP(S) URL,
    contains credentials, a query or a fragment, or the limit is negative. *)

val base_url : t -> string
(** [base_url t] is the canonical base URL, including a trailing slash. *)

val http_session : t -> Fetch.plain
(** [http_session t] is the narrowed Fetch capability used by [t]. *)

(** {1 Export Formats} *)

type format =
  | Bibtex
  | Biblatex
  | Bookmarks
  | Coins
  | Csljson
  | Csv
  | Endnote_xml
  | Evernote
  | Mods
  | Rdf_bibliontology
  | Rdf_dc
  | Rdf_zotero
  | Refer
  | Refworks_tagged
  | Ris
  | Tei
  | Wikipedia
(** Export formats supported by the Zotero Translation Server. *)

val format_to_string : format -> string
(** [format_to_string fmt] returns the string representation of [fmt]. *)

val format_of_string : string -> format option
(** [format_of_string s] parses [s] into a format, or [None] if invalid. *)

(** {1 Logging and Errors} *)

val log_src : Logs.src
(** Log source for the Zotero Translation client. *)

exception Api_error of int * string
(** [Api_error (status, body)] reports a response outside the 2xx range.
    [body] is bounded as described in {!of_fetch}. Transport, redirect,
    policy and decoding failures propagate as [Eio.Io] with [Fetch.E].
    Cancellation propagates unchanged. Bound a whole operation with
    [Eio.Time.Mono.with_timeout] and the caller's monotonic clock. *)

(** {1 API Operations} *)

val resolve_doi : t -> string -> Jsont.json
(** [resolve_doi t doi] resolves a DOI to bibliographic metadata.
    @raise Api_error if the request fails *)

val resolve_url : t -> string -> Jsont.json
(** [resolve_url t url] resolves a URL to bibliographic metadata.
    @raise Api_error if the request fails *)

val search_id : t -> string -> Jsont.json
(** [search_id t doi] searches for a DOI.
    @raise Api_error if the request fails *)

val export : t -> format -> Jsont.json -> string
(** [export t format json] exports bibliographic data to the specified format.
    @raise Api_error if the request fails *)

val json_of_doi : t -> slug:string -> string -> Jsont.json
(** [json_of_doi t ~slug doi] fetches DOI metadata, parses BibTeX, and returns
    enriched JSON with a [bib] field containing the BibTeX entry.
    @param slug Used to generate the citation key (dashes converted to underscores)
    @raise Api_error if the request fails *)
