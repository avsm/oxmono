(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** High-level Standard Site API operations.

    This module provides operations for managing Standard Site publications and
    documents on AT Protocol. *)

(** {1 API Client} *)

type t = Xrpc_auth.Client.t
(** Standard Site API client (uses shared xrpc_auth client). *)

val create :
  sw:Eio.Switch.t ->
  env:
    < clock : _ Eio.Time.clock
    ; net : _ Eio.Net.t
    ; fs : Eio.Fs.dir_ty Eio.Path.t
    ; .. > ->
  app_name:string ->
  ?profile:string ->
  pds:string ->
  unit ->
  t
(** [create ~sw ~env ~app_name ?profile ~pds ()] creates a Standard Site API
    client. *)

(** {1 Authentication} *)

val login : t -> identifier:string -> password:string -> unit
(** [login api ~identifier ~password] authenticates with the PDS. *)

val resume : t -> session:Xrpc_auth.Session.t -> unit
(** [resume api ~session] resumes from a saved session. *)

val logout : t -> unit
(** [logout api] logs out and clears the session from disk. *)

val get_session : t -> Xrpc_auth.Session.t option
(** [get_session api] returns the current session, if authenticated. *)

val is_logged_in : t -> bool
(** [is_logged_in api] returns [true] if there's an active session. *)

(** {1 Identity} *)

val resolve_handle : t -> string -> string
(** [resolve_handle api handle] resolves a handle to a DID. *)

val resolve_bsky_post :
  t -> string -> Atp_lexicon_standard_site.Com.Atproto.Repo.StrongRef.main
(** [resolve_bsky_post api url] resolves a Bluesky post URL to a StrongRef.
    Accepts both web URLs (https://bsky.app/profile/handle/post/rkey) and
    AT URIs (at://did/app.bsky.feed.post/rkey). *)

val get_did : t -> string
(** [get_did api] returns the DID of the authenticated user. *)

(** {1 Publication Operations} *)

val list_publications :
  t ->
  ?did:string ->
  unit ->
  (string * Atp_lexicon_standard_site.Site.Standard.Publication.main) list
(** [list_publications api ?did ()] lists publications for a user. *)

val get_publication :
  t ->
  did:string ->
  rkey:string ->
  Atp_lexicon_standard_site.Site.Standard.Publication.main option
(** [get_publication api ~did ~rkey] fetches a single publication record. *)

val create_publication :
  t ->
  name:string ->
  url:string ->
  ?description:string ->
  ?rkey:string ->
  unit ->
  string
(** [create_publication api ~name ~url ?description ?rkey ()] creates a new
    publication. Returns the rkey. *)

val update_publication :
  t ->
  rkey:string ->
  name:string ->
  url:string ->
  ?description:string ->
  unit ->
  unit
(** [update_publication api ~rkey ~name ~url ?description ()] updates an
    existing publication. *)

val delete_publication : t -> rkey:string -> unit
(** [delete_publication api ~rkey] deletes a publication. *)

(** {1 Document Operations} *)

val list_documents :
  t ->
  ?did:string ->
  unit ->
  (string * Atp_lexicon_standard_site.Site.Standard.Document.main) list
(** [list_documents api ?did ()] lists documents for a user. *)

val get_document :
  t ->
  did:string ->
  rkey:string ->
  Atp_lexicon_standard_site.Site.Standard.Document.main option
(** [get_document api ~did ~rkey] fetches a single document record. *)

val create_document :
  t ->
  site:string ->
  title:string ->
  published_at:string ->
  ?path:string ->
  ?description:string ->
  ?text_content:string ->
  ?tags:string list ->
  ?bsky_post_ref:Atp_lexicon_standard_site.Com.Atproto.Repo.StrongRef.main ->
  ?rkey:string ->
  unit ->
  string
(** [create_document api ~site ~title ~published_at ...] creates a new document.
    Returns the rkey. *)

val update_document :
  t ->
  rkey:string ->
  site:string ->
  title:string ->
  published_at:string ->
  ?path:string ->
  ?description:string ->
  ?text_content:string ->
  ?tags:string list ->
  ?bsky_post_ref:Atp_lexicon_standard_site.Com.Atproto.Repo.StrongRef.main ->
  ?updated_at:string ->
  unit ->
  unit
(** [update_document api ~rkey ...] updates an existing document. *)

val delete_document : t -> rkey:string -> unit
(** [delete_document api ~rkey] deletes a document. *)
