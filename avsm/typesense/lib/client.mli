(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Authenticated Typesense client with session persistence and profile support.

    This module provides a high-level client that wraps the generated
    {!Typesense} module with authentication support. Sessions are stored in
    profile-specific directories.

    {2 Usage}

    {[
      let t = Client.login ~sw ~env
        ~server_url:"http://localhost:8108"
        ~api_key:"xyz123"
        () in
      let collections = Typesense.Collections.list_collections (Client.client t) ()
    ]}

    {2 Resuming Sessions}

    {[
      match Session.load fs () with
      | Some session ->
          let t = Client.resume ~sw ~env ~session () in
          ...
      | None ->
          Fmt.epr "Not logged in@."
    ]} *)

type t
(** Authenticated client state. *)

(** {1 Authentication} *)

val login :
  sw:Eio.Switch.t ->
  env:< clock : _ Eio.Time.clock
      ; net : _ Eio.Net.t
      ; fs : Eio.Fs.dir_ty Eio.Path.t
      ; .. > ->
  ?requests_config:Requests.Cmd.config ->
  ?profile:string ->
  server_url:string ->
  api_key:string ->
  unit ->
  t
(** [login ~sw ~env ?requests_config ?profile ~server_url ~api_key ()]
    authenticates using an API key. The API key is sent in the
    [X-TYPESENSE-API-KEY] header for all requests.

    @param requests_config Optional Requests.Cmd.config for HTTP settings
    @param profile Profile name (default: "default") *)

val resume :
  sw:Eio.Switch.t ->
  env:< clock : _ Eio.Time.clock
      ; net : _ Eio.Net.t
      ; fs : Eio.Fs.dir_ty Eio.Path.t
      ; .. > ->
  ?requests_config:Requests.Cmd.config ->
  ?profile:string ->
  session:Session.t ->
  unit ->
  t
(** [resume ~sw ~env ?requests_config ?profile ~session ()] resumes from a saved session.

    @param requests_config Optional Requests.Cmd.config for HTTP settings *)

val logout : t -> unit
(** [logout t] clears the session from disk. *)

(** {1 Client Access} *)

val client : t -> Typesense.t
(** [client t] returns the underlying Typesense client for API calls. *)

val session : t -> Session.t
(** [session t] returns the current session. *)

val profile : t -> string option
(** [profile t] returns the current profile name, if set. *)

val fs : t -> Eio.Fs.dir_ty Eio.Path.t
(** [fs t] returns the filesystem capability. *)

(** {1 JSONL Import/Export}

    These functions provide bulk document operations using JSONL format,
    which is more efficient than individual API calls for large batches. *)

type import_action = Create | Upsert | Update | Emplace
(** The action to perform for each imported document. *)

type import_result = {
  success : bool;
  error : string option;
  document : string option;
}
(** Result of importing a single document. *)

val import :
  t ->
  collection:string ->
  ?action:import_action ->
  ?batch_size:int ->
  ?return_doc:bool ->
  ?return_id:bool ->
  Jsont.json list ->
  import_result list
(** [import t ~collection documents] imports documents using JSONL format.

    @param action Import action (default: Upsert)
    @param batch_size Documents per batch (default: 40)
    @param return_doc Include document in result (default: false)
    @param return_id Include ID in result (default: false) *)

type export_params = {
  filter_by : string option;
  include_fields : string list option;
  exclude_fields : string list option;
}
(** Parameters for document export. *)

val export_params :
  ?filter_by:string ->
  ?include_fields:string list ->
  ?exclude_fields:string list ->
  unit ->
  export_params
(** Create export parameters. *)

val export :
  t ->
  collection:string ->
  ?params:export_params ->
  unit ->
  Jsont.json list
(** [export t ~collection ?params ()] exports documents as JSONL. *)
