(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** {1 Zotero Translation Server Client}

    OCaml client for the {{:https://github.com/zotero/translation-server}Zotero
    Translation Server}, which provides DOI/URL resolution and bibliographic
    format export. *)

(** {1 Session} *)

type t
(** A Zotero Translation Server client. *)

val create :
  ?session:Requests.t ->
  sw:Eio.Switch.t ->
  < clock : _ Eio.Time.clock
  ; net : _ Eio.Net.t
  ; fs : Eio.Fs.dir_ty Eio.Path.t
  ; .. > ->
  base_url:string ->
  t
(** [create ?session ~sw env ~base_url] creates a client for the Zotero
    Translation Server at [base_url].

    @param session Optional existing HTTP session to reuse
    @param sw Eio switch for resource management
    @param env Eio environment with clock, net, and fs capabilities
    @param base_url Base URL of the translation server (e.g., "http://localhost:1969") *)

val base_url : t -> string
(** [base_url t] returns the base URL of the translation server. *)

val http_session : t -> Requests.t
(** [http_session t] returns the underlying HTTP session. *)

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
(** API error with HTTP status code and message. *)

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
