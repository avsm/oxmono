module Error = Error
module Client = Client

let connect = Client.create

module Auth = Auth
module Oauth = Oauth
module Qr_login = Qr_login

let login_password ~sw ~env ~homeserver ~user ~password ?fetch
    ?request_refresh_token () =
  let client = connect ~sw ~env ~homeserver ?fetch () in
  Auth.login_password client ~user ~password ?request_refresh_token ()

let login_password_with_expiry ~sw ~env ~homeserver ~user ~password ?fetch
    ?request_refresh_token () =
  let client = connect ~sw ~env ~homeserver ?fetch () in
  Auth.login_password_with_expiry client ~user ~password ?request_refresh_token
    ()

module Server = Server
module Directory = Directory
module Room_preview = Room_preview
module Knock_requests = Knock_requests
module Thirdparty = Thirdparty
module Rooms = Rooms
module Room_details = Room_details
module Messages = Messages
module Peeking = Peeking
module Thread_paginator = Thread_paginator
module Media = Media
module State = State
module Retention = Retention
module Relations = Relations
module Typing = Typing
module Receipts = Receipts
module Tags = Tags
module Search = Search
module Report = Report
module Delayed_events = Delayed_events
module Send_queue = Send_queue
module Account = Account
module Account_data = Account_data
module Recovery = Recovery
module Profile = Profile
module Presence = Presence
module Devices = Devices
module Admin = Admin
module Openid = Openid
module Notifications = Notifications
module Notification_settings = Notification_settings
module Thread_subscriptions = Thread_subscriptions
module Sync = Sync
module Sliding_sync = Sliding_sync
module Sync_service = Sync_service
module Adaptive_sync = Adaptive_sync

let run_sync ~sw ~env client ~on_sync ?on_error ?encryption ?verification () =
  let clock = Eio.Stdenv.clock env in
  match Client.session client with
  | None ->
      (* No user id means no base-client state and no crypto machine, so
         fall back to the plain loop and let its first request fail into
         [on_error]. *)
      Sync.sync_forever ~sw ~clock client
        ~callbacks:(Sync.callbacks ?on_error ~on_response:on_sync ())
        ()
  | Some { Matrix_client.Client.user_id; _ } ->
      let service = Sync_service.of_user ~user_id () in
      Sync_service.run ~sw ~clock client service ?on_error ?encryption
        ?verification ~on_response:on_sync
        ~on_change:(fun _state _changes -> ())
        ()

module Keys = Keys
module To_device = To_device
module Verification = Verification
module Encryption = Encryption
module Verification_service = Verification_service
module Backup = Backup
module Room_keys = Room_keys
module Secret_storage = Secret_storage
module Secrets = Secrets
module Dehydrated_device = Dehydrated_device
