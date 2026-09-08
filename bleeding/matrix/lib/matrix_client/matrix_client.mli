(** matrix_client — the Matrix client-server API.

    Every endpoint module takes a {!Client.t}, which holds the homeserver
    origin, the access token and a source of randomness. HTTP and randomness are
    capabilities the caller supplies, so nothing here opens a socket or reads a
    global generator, and a backend must be passed to {!Client.create}.
    [Fetch_httpz.std], used by the [matrix-chat.eio] library with {!Http_retry}, is
    one that is ready to use.

    Nothing here forks a fiber or waits on a clock either. The loops that keep
    calling [/sync] live in [matrix-chat.eio]. What this library does touch is the
    filesystem, and only through {!Store}, {!Profile_store} and {!Crypto_store}.
*)

(** {1 The client} *)

module Error = Error
(** The error every call returns. *)

module Random = Random
(** Cryptographic randomness, as a capability the caller supplies. *)

module Client = Client
(** The client value, and the requests the endpoint modules make through it. *)

module Http_retry = Http_retry
(** The Matrix-aware retry policy used by the default Eio transport. *)

module Route = Route
(** Checked URI-template expansion for Matrix endpoint paths. This is useful to
    extension endpoint modules as well as the built-in API surface. *)

module Notification_settings = Notification_settings

module Thread_subscriptions = Thread_subscriptions
(** MSC4306 thread subscription status and MSC4308 change pagination. *)

(** {1 Authentication} *)

module Auth = Auth
(** Logging in, logging out, registering and login tokens. *)

module Uiaa = Uiaa
(** The challenge-response flow that guards the sensitive endpoints. *)

module Oauth = Oauth
(** The OAuth 2.0 flow that replaces [/login] from Matrix 1.15, covering
    [/auth_metadata] discovery, RFC 7591 dynamic client registration, the PKCE
    authorisation code grant, token refresh and RFC 7009 revocation. *)

(** {1 Discovery} *)

module Server = Server
(** What a homeserver is and what it will do, from [/versions], [/capabilities]
    and [/.well-known/matrix/client]. *)

module Directory = Directory
(** Room aliases, visibility, room summaries and the published room list. *)

module Room_preview = Room_preview
(** Typed previews of known and remote rooms. *)

module Knock_requests = Knock_requests
(** Persisted incoming knock requests and moderation actions. *)

module Spaces = Spaces
(** Space hierarchies (MSC1772). *)

module Space_graph = Space_graph
(** A deterministic DAG projected from cached mutual space relationships. *)

module Thirdparty = Thirdparty
(** Lookup of the protocols a homeserver bridges to. *)

(** {1 Rooms} *)

module Rooms = Rooms
(** Creating, joining, leaving and inviting. *)

module Room = Room
(** Cache-backed conveniences for a single room. *)

module Room_details = Room_details
(** Cached member details with explicit lazy-member refresh. *)

module Messages = Messages
(** Sending messages and paginating a room's history. *)

module Peeking = Peeking
(** The historical [/initialSync] and [/events] peeking endpoints. *)

module Paginator = Paginator
(** Stateful backward and forward pagination around a target event. *)

module Thread_paginator = Thread_paginator
(** Stateful pagination over a room's thread roots. *)

module Media = Media
(** Uploading and downloading attachments. *)

module Media_fetcher = Media_fetcher
(** Replaceable media retrieval with cache integration. *)

module Media_store = Media_store
(** The deterministic media cache used by attachment and send-queue layers. *)

module Encrypted_attachment = Encrypted_attachment
(** AES-CTR encryption, ciphertext hashing and metadata for encrypted media. *)

module Attachment = Encrypted_attachment
(** Short alias for {!Encrypted_attachment}. *)

module State = State
(** Reading and writing room state events. *)

module Retention = Retention
(** MSC1763 room and server message-retention policies. *)

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

module Calls = Calls
(** VoIP signalling events. *)

module Delayed_events = Delayed_events
(** Events the server sends later on the client's behalf (MSC4140). *)

(** {1 The account} *)

module Account = Account
(** Third-party identifiers, password changes, deactivation and the ignore list.
*)

module Account_data = Account_data
(** The per-user key-value store the homeserver keeps. *)

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

(** {1 Syncing} *)

module Sync = Sync
(** The [/sync] endpoint and the filters it takes. *)

module Sliding_sync = Sliding_sync
(** Simplified sliding sync (MSC4186). *)

module Base_client = Base_client
(** Folding a [/sync] response into room summaries and reporting what changed.
*)

(** {1 Notifications} *)

module Push = Push
(** The push rules and pushers a homeserver holds. *)

module Push_evaluator = Push_evaluator
(** Running the push rules locally, including for encrypted rooms the server
    cannot read. *)

module Notifications = Notifications
(** The notification list the server keeps. *)

module Read_state = Read_state
(** The read position of each room, and the unread counts derived from it. *)

(** {1 End-to-end encryption} *)

module Crypto_key = Crypto_key
(** Ed25519 and Curve25519 keys, signatures and the identifiers keys are
    published under. *)

module Keys = Keys
(** Uploading, querying and claiming device keys. *)

module Olm = Olm
(** The Olm and Megolm ratchets. *)

module To_device = To_device
(** [/sendToDevice], the transport under Olm. *)

module Verification = Verification
(** Interactive device verification, by short authentication string or QR code.
*)

module Cross_signing = Cross_signing
(** The signing keys that say which devices a user owns, and the trust decisions
    drawn from them. *)

module Encryption = Encryption
(** Device lists, Olm and Megolm sessions, key sharing, gossiping and key
    backup, as a machine that performs no I/O. *)

module Encryption_driver = Encryption_driver
(** An {!Encryption} machine joined to a running client and a store. *)

module Crypto_store = Crypto_store
(** The encryption machine's state, persisted in a profile directory. *)

(** {1 Key backup and secret storage} *)

module Backup = Backup
(** The backup key, its recovery key, and the encryption of a room key to it. *)

module Room_key_export = Room_key_export
(** Passphrase-protected portable Megolm key files. *)

module Room_keys = Room_keys
(** The [/room_keys] endpoints the backup is stored through. *)

module Secret_storage = Secret_storage
(** The cryptography of secure secret storage, per the SSSS specification. *)

module Secrets = Secrets
(** The account-data endpoints secret storage is kept in. *)

module Recovery = Recovery
(** High-level recovery state and account-data marker write plans. *)

module Dehydrated_device = Dehydrated_device
(** Dehydrated devices, which receive room keys while the user is offline
    (MSC3814). *)

module Olm_dehydrated_pickle = Olm_dehydrated_pickle
(** The interoperable legacy libolm account-pickle codec used by MSC3814 V1
    dehydrated devices. *)

module Qr_login = Qr_login
(** MSC4388 QR payloads and rendezvous capability discovery. *)

(** {1 Local state} *)

module Store = Store
(** Room summaries, account data and the sync token, in memory or on disk. *)

module Composer_draft = Composer_draft
(** Composer text and attachments, persisted separately for each room and
    optional thread. *)

module Send_queue = Send_queue
(** A per-room queue with local echo and retry. *)

module Timeline = Timeline
(** A room's events with edits and redactions applied. *)

module Session = Session
(** The records a profile's files hold, and their codecs. *)

module Session_pickle = Session_pickle
(** Olm and Megolm ratchet state as a string. *)

module Profile_store = Profile_store
(** The directory a profile's files live in. *)

(** {1 Compilation units}

    {!Olm} and {!Verification} re-export the modules below under shorter names.
    These are the same modules under the names their files give them, which is
    what a reference written either way resolves through. *)

module Olm_error = Olm_error
(** @canonical Matrix_client.Olm *)

module Olm_account = Olm_account
(** @canonical Matrix_client.Olm.Account *)

module Olm_session = Olm_session
(** @canonical Matrix_client.Olm.Session *)

module Megolm = Megolm
(** @canonical Matrix_client.Olm.Megolm *)

module Olm_machine = Olm_machine
(** @canonical Matrix_client.Olm.Machine *)

module Verification_base = Verification_base
(** @canonical Matrix_client.Verification *)

module Verification_sas = Verification_sas
(** @canonical Matrix_client.Verification.Sas *)

module Verification_qr = Verification_qr
(** @canonical Matrix_client.Verification.Qr *)

module Verification_flow = Verification_flow
(** @canonical Matrix_client.Verification.Flow *)
