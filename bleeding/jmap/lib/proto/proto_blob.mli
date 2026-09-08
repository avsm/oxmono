@@ portable

(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Binary data upload, download and copy.

    {{:https://datatracker.ietf.org/doc/html/rfc8620#section-6} RFC 8620 Section
     6} moves binary data outside the method call machinery. A blob is uploaded
    and downloaded over plain HTTP at URLs the session gives, and copied between
    accounts with the [Blob/copy] method.

    @canonical Jmap.Proto.Blob *)

(** {1 Upload} *)

type upload_response = {
  account_id : Proto_id.t;  (** The account the blob was uploaded to. *)
  blob_id : Proto_id.t;  (** The server assigned blob id. *)
  type_ : string;  (** The media type the server will serve the blob as. *)
  size : int64;  (** The size of the blob in octets. *)
}
(** The type for the body of an upload response, defined by RFC 8620 Section
    6.1. *)

val upload_response_jsont : upload_response Jsont.t
(** [upload_response_jsont] is the codec for an upload response. *)

(** {1 Download} *)

type download_vars = {
  account_id : Proto_id.t;  (** The account holding the blob. *)
  blob_id : Proto_id.t;  (** The blob to download. *)
  type_ : string;  (** The media type to ask the server to serve. *)
  name : string;  (** The filename to ask the server to offer. *)
}
(** The type for the variables of a download URI template. *)

val expand_download_template :
  template:Httpz_uri.Template.t ->
  download_vars ->
  (string, Httpz_uri.Template.error) result
(** [expand_download_template ~template vars] expands the parsed [downloadUrl]
    [template] with [{accountId}], [{blobId}], [{type}] and [{name}] bound to
    the corresponding value of [vars]. Values are percent encoded and any other
    variable is undefined and omitted. *)

val expand_download_url :
  template:string -> download_vars -> (string, Httpz_uri.Template.error) result
(** [expand_download_url ~template vars] parses [template] and expands it with
    [vars] as {!expand_download_template} does. A malformed template, an invalid
    UTF-8 value, and an expansion that is not an RFC 3986 URI reference are
    errors. *)

(** {1 Blob/copy} *)

type copy_args = {
  from_account_id : Proto_id.t;  (** The account to copy blobs from. *)
  account_id : Proto_id.t;  (** The account to copy blobs to. *)
  blob_ids : Proto_id.t list;  (** The blobs to copy. *)
}
(** The type for the arguments of a [Blob/copy] call, defined by RFC 8620
    Section 6.3. *)

val copy_args_jsont : copy_args Jsont.t
(** [copy_args_jsont] is the codec for the arguments of a [Blob/copy] call. *)

type copy_response = {
  from_account_id : Proto_id.t;  (** The account the blobs were copied from. *)
  account_id : Proto_id.t;  (** The account the blobs were copied to. *)
  copied : (Proto_id.t * Proto_id.t) list option;
      (** The new blob id of each blob copied, keyed by its old blob id. RFC
          8620 Section 6.3 types this [Id[Id]|null], so [None] covers both an
          absent member and an explicit [null], which means that none were
          copied. *)
  not_copied : (Proto_id.t * Proto_error.Set_error.t) list option;
      (** Why each blob that was not copied failed, typed [Id[SetError]|null].
      *)
}
(** The type for the response of a [Blob/copy] call. *)

val copy_response_jsont : copy_response Jsont.t
(** [copy_response_jsont] is the codec for the response of a [Blob/copy] call.
*)
