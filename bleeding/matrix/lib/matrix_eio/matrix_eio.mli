(** matrix_eio — Eio bindings for the Matrix client-server API.

    Each module here wraps its {!Matrix_client} counterpart with the result type
    unwrapped, so a failure raises [Eio.Io (Error.E e, _)] carrying an
    {!Error.type-err} rather than returning [Error]. {!Error.unwrap} is the one
    place that conversion happens, and {!Error.pp_err} prints what it carries.

    The driver loops live here rather than in [matrix-chat.client], which
    performs one logical request per call and owns no long-lived loop. {!Sync}
    owns the [/sync] loop and defines the {!Sync.type-action} and
    {!Sync.type-callbacks} every loop in this library speaks, {!Sliding_sync}
    the MSC4186 one, {!Sync_service} the loop that also maintains room state,
    and {!Send_queue} the fibers that drain the outgoing queue. Each forks onto
    the switch it is given and returns at once, so releasing that switch cancels
    the fibers, and an exception out of a callback fails it. *)

(** {1 The client} *)

module Error = Error
(** The failure every call raises, and the exception it travels in. *)

module Client = Client
(** The client value every module here takes. *)

val connect :
  sw:Eio.Switch.t ->
  env:Eio_unix.Stdenv.base ->
  homeserver:Uriz.t ->
  ?user_agent:string ->
  ?well_known_policy:Matrix_client.Client.well_known_policy ->
  ?fetch:Fetch.plain ->
  ?request_timeout:float ->
  ?media_fetcher:Matrix_client.Media_fetcher.t ->
  unit ->
  Client.t
(** [connect ~sw ~env ~homeserver ()] is {!Client.create}, a client bound to
    [sw] and not yet logged in. [well_known_policy] controls whether homeserver
    discovery may request [/.well-known/matrix/client]. [media_fetcher] is the
    initial replaceable high-level attachment fetcher. *)

(** {1 Authentication} *)

module Auth = Auth
(** Logging in, logging out, registering and login tokens. *)

module Oauth = Oauth
(** The OAuth 2.0 flow that replaces [/login] from Matrix 1.15, covering
    [/auth_metadata] discovery, RFC 7591 dynamic client registration, the PKCE
    authorisation code grant over a loopback listener, token refresh and RFC
    7009 revocation. *)

module Qr_login = Qr_login
(** MSC4388 QR payloads and rendezvous capability discovery. *)

val login_password :
  sw:Eio.Switch.t ->
  env:Eio_unix.Stdenv.base ->
  homeserver:Uriz.t ->
  user:string ->
  password:string ->
  ?fetch:Fetch.plain ->
  ?request_refresh_token:bool ->
  unit ->
  Client.t
(** [login_password ~sw ~env ~homeserver ~user ~password ()] connects to
    [homeserver], logs in with ["m.login.password"], and is the authenticated
    client. [fetch] is as in {!connect}.

    Raises [Eio.Io] with [Error.E e] on failure. Wrong credentials arrive as
    [Error.Matrix] with [M_FORBIDDEN]. [request_refresh_token=true] asks the
    homeserver to issue a refresh token and is omitted by default. *)

val login_password_with_expiry :
  sw:Eio.Switch.t ->
  env:Eio_unix.Stdenv.base ->
  homeserver:Uriz.t ->
  user:string ->
  password:string ->
  ?fetch:Fetch.plain ->
  ?request_refresh_token:bool ->
  unit ->
  Client.t * Ptime.t option
(** [login_password_with_expiry] is {!login_password} and also returns the
    optional absolute access-token expiry advertised by the server. *)

(** {1 Discovery} *)

module Server = Server
(** What a homeserver is and what it will do, from [/versions], [/capabilities]
    and [/.well-known/matrix/client]. *)

module Directory = Directory
(** Room aliases, visibility, room summaries and the published room list. *)

module Room_preview = Room_preview
(** Raising room-preview wrappers. *)

module Knock_requests = Knock_requests
(** Raising knock-request moderation wrappers. *)

module Thirdparty = Thirdparty
(** Lookup of the protocols a homeserver bridges to. *)

(** {1 Rooms} *)

module Rooms = Rooms
(** Creating, joining, leaving and inviting. *)

module Room_details = Room_details
(** Cached room members with raising lazy-refresh semantics. *)

module Peeking = Peeking
(** Raising wrappers for the historical [/initialSync] and [/events] endpoints.
*)

module Messages = Messages
(** Sending messages and paginating a room's history. *)

module Thread_paginator = Thread_paginator
(** Raising stateful pagination over a room's thread roots. *)

module Media = Media
(** Uploading and downloading attachments. *)

module State = State
(** Reading and writing room state events. *)

module Retention = Retention
(** Eio wrappers for MSC1763 retention operations. *)

module Relations = Relations
(** Reactions, edits, replies and threads. *)

module Typing = Typing
(** Typing notifications. *)

module Receipts = Receipts
(** Sending read receipts and moving the fully-read marker. *)

module Tags = Tags
(** The tags a user puts on a room, such as favourite and low priority. *)

module Search = Search
(** Searching a room's events, and the user directory. *)

module Report = Report
(** Reporting events, rooms and users to the server administrator. *)

module Delayed_events = Delayed_events
(** Events the server sends later on the client's behalf (MSC4140). *)

module Send_queue = Send_queue
(** A per-room queue with local echo and retry, and the fibers that drain it. *)

(** {1 The account} *)

module Account = Account
(** Third-party identifiers, password changes, deactivation and the ignore list.
*)

module Account_data = Account_data
(** The per-user key-value store the homeserver keeps. *)

module Recovery = Recovery
(** Recovery account-data state and ordered marker plans. *)

module Profile = Profile
(** Display name and avatar. *)

module Presence = Presence
(** Online status. *)

module Devices = Devices
(** Listing, renaming and deleting the account's devices. *)

module Admin = Admin
(** Server-administration queries, including authenticated [/admin/whois]. *)

module Openid = Openid
(** OpenID tokens, which prove an account's identity to a third party. *)

module Notifications = Notifications
(** The notification list the server keeps. *)

module Notification_settings = Notification_settings
(** High-level push-rule settings with raising network mutations. *)

module Thread_subscriptions = Thread_subscriptions
(** Raising MSC4306/MSC4308 thread-subscription transport wrappers. *)

(** {1 Syncing} *)

module Sync = Sync
(** The [/sync] endpoint and the loop that keeps calling it. *)

module Sliding_sync = Sliding_sync
(** Simplified sliding sync (MSC4186), and its loop. *)

module Sync_service = Sync_service
(** The [/sync] loop that folds each response into room state, with reconnect
    backoff and persistence through {!Matrix_client.Store}. *)

module Adaptive_sync = Adaptive_sync
(** Native sliding-sync discovery with an opt-in classic [/sync] fallback. *)

val run_sync :
  sw:Eio.Switch.t ->
  env:Eio_unix.Stdenv.base ->
  Client.t ->
  on_sync:(Matrix_proto.Sync.Response.t -> Sync.action) ->
  ?on_error:(Error.err -> Sync.action) ->
  ?encryption:Encryption.t ->
  ?verification:Verification_service.t ->
  unit ->
  unit
(** [run_sync ~sw ~env client ~on_sync ()] runs the [/sync] loop in a fiber on
    [sw], calling [on_sync] with each response and maintaining room state behind
    it. It returns as soon as the fiber is forked.

    [encryption] is a machine run over every response before [on_sync] sees it.
    To-device events are processed and answered, encrypted timeline events are
    decrypted, and the machine is saved. [verification] receives the
    [m.key.verification.*] traffic, and needs [encryption] to decrypt it.
    [on_error] decides what the loop does after a failed request, and defaults
    as in {!Sync_service.run}.

    A client with no session has no room state to maintain, so the plain
    {!Sync.sync_forever} loop runs instead and its first request fails into
    [on_error]. A caller that wants the state the loop builds should drive
    {!Sync_service.run} itself, which hands it over. *)

(** {1 End-to-end encryption} *)

module Keys = Keys
(** Uploading, querying and claiming device keys. *)

module To_device = To_device
(** [/sendToDevice], the transport under Olm. *)

module Verification = Verification
(** The interactive verification state machines, re-exported, plus sending one
    message to a device. *)

module Encryption = Encryption
(** The crypto machine, raising rather than returning. {!Encryption.sync_hook}
    folds a sync response in and performs the requests it produces, and
    {!Encryption.send_encrypted} encrypts a room event and sends it. *)

module Verification_service = Verification_service
(** A {!Matrix_client.Verification.Flow} wired to the sync loop, with one
    callback asking the user whether the emoji match. *)

(** {1 Key backup and secret storage} *)

module Backup = Backup
(** The backup key, its recovery key, and the encryption of a room key to it. *)

module Room_keys = Room_keys
(** The [/room_keys] endpoints the backup is stored through. *)

module Secret_storage = Secret_storage
(** The cryptography of secure secret storage, per the SSSS specification. *)

module Secrets = Secrets
(** The account-data endpoints secret storage is kept in. *)

module Dehydrated_device = Dehydrated_device
(** Dehydrated devices, which receive room keys while the user is offline
    (MSC3814). *)
