(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Tangled data types with jsont codecs.

    This module defines the record types used in Tangled's AT Protocol
    collections ([sh.tangled.*]). *)

(** {1 Repository Record} *)

type repo = {
  name : string;  (** Repository name (e.g., ["my-project"]). *)
  knot : string;
      (** Knot hostname hosting the repo (e.g., ["knot.tangled.sh"]). *)
  description : string option;  (** Optional repository description. *)
  created_at : string;  (** ISO 8601 creation timestamp. *)
  spindle : string option;  (** Optional Spindle CI/CD URL. *)
  private_ : bool;  (** Whether the repository is private. *)
}
(** Repository record stored in [sh.tangled.repo] collection. *)

val repo_jsont : repo Jsont.t
(** jsont codec for repository records. *)

val pp_repo : repo Fmt.t
(** Pretty-print a repository. *)

(** {1 Repository Info} *)

type language = {
  name : string;  (** Language name (e.g., ["OCaml"]). *)
  size : int;  (** Size in bytes. *)
  percentage : int;  (** Percentage of codebase. *)
  file_count : int option;  (** Number of files. *)
  color : string option;  (** GitHub-style color code. *)
  extensions : string list option;  (** File extensions for this language. *)
}
(** Language statistics from [sh.tangled.repo.languages]. *)

val language_jsont : language Jsont.t
(** jsont codec for language statistics. *)

val pp_language : language Fmt.t
(** Pretty-print a language. *)

type repo_info = {
  default_branch : string;  (** Default branch name (e.g., ["main"]). *)
  languages : language list;  (** Language statistics from knot API. *)
}
(** Extended repository information from knot API. *)

val pp_repo_info : repo_info Fmt.t
(** Pretty-print repo info. *)

(** {1 AT URI Handling} *)

type at_uri = {
  did : string;  (** DID of the record owner. *)
  collection : string;  (** Collection NSID (e.g., ["sh.tangled.repo"]). *)
  rkey : string;  (** Record key. *)
}
(** Parsed AT URI components. *)

val parse_at_uri : string -> at_uri option
(** [parse_at_uri uri] parses an AT URI like
    ["at://did:plc:abc/sh.tangled.repo/my-repo"]. Returns [None] if the URI is
    malformed. *)

val make_at_uri : did:string -> collection:string -> rkey:string -> string
(** [make_at_uri ~did ~collection ~rkey] constructs an AT URI. *)

val pp_at_uri : at_uri Fmt.t
(** Pretty-print an AT URI. *)

(** {1 List Records Response} *)

type 'a list_response = {
  records : (string * 'a) list;  (** List of (rkey, value) pairs. *)
  cursor : string option;  (** Pagination cursor for next page. *)
}
(** Response from [com.atproto.repo.listRecords]. *)

val list_response_jsont : 'a Jsont.t -> 'a list_response Jsont.t
(** jsont codec for list responses. *)

(** {1 Create Record Response} *)

type create_response = {
  uri : string;  (** AT URI of the created record. *)
  cid : string;  (** CID of the created record. *)
}
(** Response from [com.atproto.repo.createRecord]. *)

val create_response_jsont : create_response Jsont.t
(** jsont codec for create response. *)

(** {1 Service Auth} *)

type service_auth_response = { token : string  (** ServiceAuth JWT token. *) }
(** Response from [com.atproto.server.getServiceAuth]. *)

val service_auth_response_jsont : service_auth_response Jsont.t
(** jsont codec for service auth response. *)

(** {1 Resolve Handle} *)

type resolve_handle_response = { did : string  (** The DID for the handle. *) }
(** Response from [com.atproto.identity.resolveHandle]. *)

val resolve_handle_response_jsont : resolve_handle_response Jsont.t
(** jsont codec for resolve handle response. *)
