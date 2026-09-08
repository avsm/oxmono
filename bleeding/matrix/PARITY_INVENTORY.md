# Mechanical parity inventory

> This is a bounded, textual inventory for parity-audit bookkeeping. It is not a semantic API comparison or a claim of behavioural parity.

- OCaml repository commit: `948229e367f16abcc7b609310585904cdff0983d`
- Rust repository commit: `523b5af53a8fd9fae9e2bc981bfb01ac86fd2890`
- Rust crate: `crates/matrix-sdk`
- Regeneration command: `tools/parity-inventory.sh --ocaml-repo '.' --rust-repo '../matrix-rust-sdk' --rust-commit '523b5af53a8fd9fae9e2bc981bfb01ac86fd2890' --ocaml-commit '948229e367f16abcc7b609310585904cdff0983d' > PARITY_INVENTORY.md`

## Rust Cargo features

The `default` declaration is recorded separately from opt-in features.

- Declared default: `default = ["e2e-encryption", "automatic-room-key-forwarding", "sqlite", "rustls-aws-lc-rs"]`
- Enabled by default:
  - `e2e-encryption`
  - `automatic-room-key-forwarding`
  - `sqlite`
  - `rustls-aws-lc-rs`
- Opt-in-only features:
  - `testing`
  - `js`
  - `bundled-sqlite`
  - `indexeddb`
  - `qrcode`
  - `experimental-send-custom-to-device`
  - `experimental-encrypted-state-events`
  - `experimental-push-secrets`
  - `markdown`
  - `socks`
  - `local-server`
  - `sso-login`
  - `federation-api`
  - `uniffi`
  - `experimental-widgets`
  - `docsrs`
  - `unstable-msc4274`
  - `unstable-msc4426`
  - `experimental-search`
  - `experimental-element-recent-emojis`
  - `experimental-x509-identity-verification`

## Rust source modules and direct public items

Each source path is listed once; item names are extracted from lines beginning with `pub `.
This does not expand macros or conditional compilation and does not resolve re-exports.

- Source files: `157`

- `crates/matrix-sdk/src/account.rs`: struct:Account, fn:get_display_name, fn:set_display_name, fn:request_openid_token,
  fn:get_avatar_url, fn:get_cached_avatar_url, fn:set_avatar_url, fn:get_avatar, fn:upload_avatar, fn:fetch_user_profile,
  fn:fetch_user_profile_of, fn:fetch_profile_field_of, fn:fetch_profile_field_of_static, fn:set_status, fn:clear_status,
  fn:set_call, fn:clear_call, fn:set_profile_field, fn:delete_profile_field, fn:change_password, fn:deactivate,
  fn:get_3pids, fn:request_3pid_email_token, fn:request_3pid_msisdn_token, fn:add_3pid, fn:delete_3pid, fn:account_data,
  fn:account_data_raw, fn:fetch_account_data, fn:fetch_account_data_static, fn:set_account_data, fn:set_account_data_raw,
  fn:mark_as_dm, fn:ignore_user, fn:unignore_user, fn:push_rules, fn:get_recently_visited_rooms,
  fn:track_recently_visited_room, fn:observe_media_preview_config, fn:fetch_media_preview_config_event_content,
  fn:get_media_preview_config_event_content, fn:set_media_previews_display_policy, fn:set_invite_avatars_display_policy,
  fn:add_recent_emoji, fn:get_recent_emojis
- `crates/matrix-sdk/src/attachment.rs`: struct:BaseImageInfo, struct:BaseVideoInfo, struct:BaseAudioInfo,
  struct:BaseFileInfo, enum:AttachmentInfo, struct:Thumbnail, fn:into_parts, struct:AttachmentConfig, fn:new,
  fn:thumbnail, fn:txn_id, fn:info, fn:caption, fn:mentions, fn:reply, fn:extra_content, struct:GalleryConfig, fn:new,
  fn:txn_id, fn:add_item, fn:caption, fn:mentions, fn:reply, fn:len, fn:is_empty, struct:GalleryItemInfo
- `crates/matrix-sdk/src/authentication/matrix/login_builder.rs`: struct:LoginBuilder, fn:device_id,
  fn:initial_device_display_name, fn:request_refresh_token, fn:send, struct:SsoLoginBuilder, fn:device_id,
  fn:initial_device_display_name, fn:server_builder, fn:identity_provider_id, fn:request_refresh_token, fn:send
- `crates/matrix-sdk/src/authentication/matrix/mod.rs`: use:self::login_builder::LoginBuilder,
  use:self::login_builder::SsoLoginBuilder, struct:MatrixAuth, enum:SsoError, fn:get_login_types, fn:get_sso_login_url,
  fn:login_username, fn:login_identifier, fn:login_custom, fn:login_token, fn:login_with_sso_callback, fn:login_sso,
  fn:logged_in, fn:refresh_access_token, fn:register, fn:logout, fn:session, fn:restore_session, struct:MatrixSession
- `crates/matrix-sdk/src/authentication/mod.rs`: mod:matrix, mod:oauth, struct:SessionTokens, enum:AuthApi,
  enum:AuthSession, fn:meta, fn:into_meta, fn:access_token, fn:get_refresh_token
- `crates/matrix-sdk/src/authentication/oauth/auth_code_builder.rs`: struct:OAuthAuthCodeUrlBuilder, fn:prompt,
  fn:login_hint, fn:user_id_hint, fn:build, struct:OAuthAuthorizationData, fn:login_url
- `crates/matrix-sdk/src/authentication/oauth/cross_process.rs`: fn:new, fn:spin_lock, fn:restore_session, fn:on_logout,
  fn:save_in_memory_and_db, fn:handle_mismatch, enum:CrossProcessRefreshLockError
- `crates/matrix-sdk/src/authentication/oauth/error.rs`: use:oauth2::{,
  use:super::cross_process::CrossProcessRefreshLockError, type:OAuthRequestError, enum:RedirectUriQueryParseError,
  enum:OAuthError, enum:OAuthDiscoveryError, fn:is_not_supported, enum:OAuthAuthorizationCodeError,
  enum:AuthorizationCodeErrorResponseType, enum:OAuthTokenRevocationError, enum:OAuthClientRegistrationError,
  enum:ClientRegistrationErrorResponseType
- `crates/matrix-sdk/src/authentication/oauth/http_client.rs`: (no direct public declarations)
- `crates/matrix-sdk/src/authentication/oauth/mod.rs`: use:oauth2::{ClientId,, mod:error, mod:qrcode, mod:registration,
  use:self::{, struct:OAuth, fn:enable_cross_process_refresh_lock, fn:msc_4388_rendezvous_server_supported,
  fn:login_with_qr_code, fn:grant_login_with_qr_code, fn:cached_server_metadata, fn:server_metadata, fn:client_id,
  fn:user_session, fn:full_session, fn:register_client, fn:restore_registered_client, fn:restore_session, fn:login,
  fn:finish_login, fn:abort_login, fn:refresh_access_token, fn:logout, struct:LoginWithQrCodeBuilder, fn:scan,
  fn:generate, struct:GrantLoginWithQrCodeBuilder, fn:device_creation_timeout, fn:scan, fn:generate, struct:OAuthSession,
  struct:UserSession, struct:ClientRegistrationData, fn:new
- `crates/matrix-sdk/src/authentication/oauth/qrcode/grant.rs`: enum:GrantLoginProgress,
  struct:GrantLoginWithScannedQrCode, fn:subscribe_to_progress, struct:GrantLoginWithGeneratedQrCode,
  fn:subscribe_to_progress
- `crates/matrix-sdk/src/authentication/oauth/qrcode/login.rs`: enum:LoginProgress, struct:LoginWithQrCode,
  fn:subscribe_to_progress, struct:LoginWithGeneratedQrCode, fn:subscribe_to_progress
- `crates/matrix-sdk/src/authentication/oauth/qrcode/messages.rs`: enum:QrAuthMessage,
  fn:authorization_grant_login_protocol, struct:AuthorizationGrant, enum:LoginFailureReason, enum:LoginProtocolType
- `crates/matrix-sdk/src/authentication/oauth/qrcode/mod.rs`: use:matrix_sdk_base::crypto::types::qr_login::{,
  use:oauth2::{, use:vodozemac::ecies::{Error, use:self::{, enum:QRCodeLoginError, enum:QRCodeGrantLoginError,
  enum:DeviceAuthorizationOAuthError, fn:as_request_token_error, enum:SecureChannelError, struct:QrProgress,
  enum:GeneratedQrProgress, type:CheckCodeSender, fn:send, struct:ContinuationMessageSender, fn:confirm, fn:cancel,
  struct:CloneableSender, enum:SenderError
- `crates/matrix-sdk/src/authentication/oauth/qrcode/rendezvous_channel/mod.rs`: (no direct public declarations)
- `crates/matrix-sdk/src/authentication/oauth/qrcode/rendezvous_channel/msc_4108.rs`: struct:Channel
- `crates/matrix-sdk/src/authentication/oauth/qrcode/secure_channel/crypto_channel.rs`: (no direct public declarations)
- `crates/matrix-sdk/src/authentication/oauth/qrcode/secure_channel/mod.rs`: struct:MockedRendezvousServer, fn:new
- `crates/matrix-sdk/src/authentication/oauth/registration.rs`: use:language_tags, struct:ClientRegistrationResponse,
  struct:ClientMetadata, fn:new, enum:OAuthGrantType, enum:ApplicationType, struct:Localized, fn:new, fn:non_localized,
  fn:get
- `crates/matrix-sdk/src/authentication/oauth/tests.rs`: (no direct public declarations)
- `crates/matrix-sdk/src/automatic_call_status.rs`: fn:enable_automatic_call_status
- `crates/matrix-sdk/src/client/builder/homeserver_config.rs`: fn:discover
- `crates/matrix-sdk/src/client/builder/mod.rs`: struct:ClientBuilder, fn:media_fetcher, fn:dm_room_definition,
  fn:homeserver_url, fn:server_name, fn:insecure_server_name_no_tls, fn:server_name_or_homeserver_url,
  fn:sliding_sync_version_builder, fn:sqlite_store, fn:sqlite_store_with_cache_path,
  fn:sqlite_store_with_config_and_cache_path, fn:indexeddb_store, fn:store_config, fn:respect_login_well_known,
  fn:disable_well_known_lookup, fn:request_config, fn:proxy, fn:disable_ssl_verification, fn:user_agent,
  fn:add_root_certificates, fn:disable_built_in_root_certificates, fn:http_client, fn:server_versions,
  fn:handle_refresh_tokens, fn:base_client, fn:with_encryption_settings, fn:with_room_key_recipient_strategy,
  fn:with_decryption_settings, fn:with_enable_share_history_on_invite, fn:with_enable_automatic_back_pagination,
  fn:cross_process_store_config, fn:with_threading_support, fn:search_index_store, fn:with_x509_signer,
  fn:with_x509_verifier, fn:build, fn:sanitize_server_name, enum:ClientBuildError
- `crates/matrix-sdk/src/client/caches.rs`: (no direct public declarations)
- `crates/matrix-sdk/src/client/futures.rs`: struct:SendRequest, fn:with_send_progress_observable,
  fn:with_request_config, fn:subscribe_to_send_progress, struct:SendMediaUploadRequest, fn:new,
  fn:with_send_progress_observable, fn:subscribe_to_send_progress
- `crates/matrix-sdk/src/client/homeserver_capabilities.rs`: struct:HomeserverCapabilities, fn:new, fn:refresh,
  fn:can_change_password, fn:can_change_displayname, fn:can_change_avatar, fn:can_change_thirdparty_ids,
  fn:can_get_login_token, fn:extended_profile_fields, fn:room_versions, fn:account_moderation,
  fn:forgets_room_when_leaving
- `crates/matrix-sdk/src/client/mod.rs`: use:self::builder::{ClientBuildError,, enum:LoopCtrl, enum:SessionChange,
  struct:ServerVendorInfo, struct:TileServerInfo, struct:Client, fn:new, fn:subscribe_to_ignore_user_list_changes,
  fn:builder, fn:http_client, fn:cross_process_lock_config, fn:homeserver_capabilities, fn:server_vendor_info,
  fn:request_config, fn:is_active, fn:server, fn:homeserver, fn:sliding_sync_version, fn:set_sliding_sync_version,
  fn:session_meta, fn:room_info_notable_update_receiver, fn:subscribe_to_global_profile_updates,
  fn:subscribe_to_own_profile, fn:search_users, fn:user_id, fn:device_id, fn:access_token, fn:set_presence,
  fn:session_tokens, fn:auth_api, fn:session, fn:state_store, fn:event_cache_store, fn:media_store, fn:matrix_auth,
  fn:account, fn:encryption, fn:media, fn:pusher, fn:oauth, fn:add_event_handler, fn:add_room_event_handler,
  fn:observe_events, fn:observe_room_events, fn:observe_own_beacon_info_updates, fn:remove_event_handler,
  fn:event_handler_drop_guard, fn:add_event_handler_context, fn:register_notification_handler,
  fn:subscribe_to_room_updates, fn:subscribe_to_all_room_updates, fn:rooms, fn:rooms_filtered, fn:rooms_stream,
  fn:joined_rooms, fn:invited_rooms, fn:left_rooms, fn:joined_space_rooms, fn:get_room, fn:get_room_preview,
  fn:resolve_room_alias, fn:is_room_alias_available, fn:create_room_alias, fn:remove_room_alias, fn:restore_session,
  fn:restore_session_with, fn:refresh_access_token, fn:logout, fn:get_or_upload_filter, fn:join_room_by_id,
  fn:join_room_by_id_or_alias, fn:public_rooms, fn:create_room, fn:create_dm, fn:get_dm_room, fn:get_dm_rooms,
  fn:public_rooms_filtered, fn:send, fn:fetch_server_versions, fn:fetch_client_well_known, fn:supported_versions,
  fn:supported_versions_cached, fn:server_versions, fn:unstable_features, fn:reset_supported_versions,
  fn:disable_well_known_lookup, fn:rtc_foci, fn:well_known_rtc_transports, fn:fetch_rtc_transports,
  fn:reset_rtc_transports, fn:discover_rtc_transports, fn:tile_server, fn:reset_well_known,
  fn:can_homeserver_push_encrypted_event_to_device, fn:devices, fn:get_retention_configuration, fn:delete_devices,
  fn:rename_device, fn:device_exists, fn:sync_once, fn:sync, fn:sync_with_callback, fn:sync_with_result_callback,
  fn:sync_stream, fn:whoami, fn:subscribe_to_session_changes, fn:set_session_callbacks, fn:notification_settings,
  fn:notification_client, fn:event_cache, fn:latest_events, fn:await_room_remote_echo, fn:knock, fn:is_user_ignored,
  fn:load_or_fetch_max_upload_size, fn:decryption_settings, fn:search_index, fn:enabled_thread_subscriptions,
  fn:fetch_thread_subscriptions, fn:pause, fn:resume, fn:optimize_stores, fn:get_store_sizes, fn:task_monitor,
  fn:subscribe_to_duplicate_key_upload_errors, fn:get_pending_key_bundle_details_for_room, fn:dm_room_definition,
  fn:set_media_fetcher, fn:get_media_fetcher, struct:StoreSizes, fn:update_tracked_users_for_testing, fn:from_client,
  fn:get, fn:strong_count
- `crates/matrix-sdk/src/client/thread_subscriptions.rs`: struct:ThreadSubscriptionCatchup, fn:new
- `crates/matrix-sdk/src/config/mod.rs`: use:matrix_sdk_base::store::StoreConfig, use:request::RequestConfig,
  use:sync::{SyncSettings,
- `crates/matrix-sdk/src/config/request.rs`: struct:RequestConfig, fn:new, fn:short_retry, fn:disable_retry,
  fn:retry_limit, fn:max_concurrent_requests, fn:timeout, fn:read_timeout, fn:max_retry_time, fn:force_auth,
  fn:skip_auth
- `crates/matrix-sdk/src/config/sync.rs`: enum:SyncToken, fn:from_optional_token, struct:SyncSettings, fn:new, fn:token,
  fn:timeout, fn:ignore_timeout_on_first_sync, fn:filter, fn:full_state, fn:set_presence
- `crates/matrix-sdk/src/deduplicating_handler.rs`: fn:run
- `crates/matrix-sdk/src/encryption/backups/futures.rs`: enum:SteadyStateError, struct:WaitForSteadyState,
  fn:subscribe_to_progress, fn:with_delay
- `crates/matrix-sdk/src/encryption/backups/mod.rs`: mod:futures, use:types::{BackupState,, struct:Backups, fn:create,
  fn:disable, fn:disable_and_delete, fn:wait_for_steady_state, fn:state_stream, fn:state, fn:are_enabled,
  fn:fetch_exists_on_server, fn:exists_on_server, fn:room_keys_for_room_stream, fn:download_room_keys_for_room,
  fn:download_room_key, enum:EnableBackupError
- `crates/matrix-sdk/src/encryption/backups/types.rs`: enum:UploadState, enum:BackupState
- `crates/matrix-sdk/src/encryption/dehydrated_devices.rs`: enum:DehydratedDeviceError, enum:DehydratedDeviceEvent,
  struct:DehydratedDevices, fn:state_stream, fn:is_supported, fn:create, fn:rehydrate, fn:is_key_stored, fn:reset_key,
  fn:start, fn:stop, fn:delete, struct:StartDehydration, fn:create_new_key, fn:skip_rehydration, fn:only_if_key_cached
- `crates/matrix-sdk/src/encryption/futures.rs`: struct:UploadEncryptedFile, fn:with_send_progress_observable,
  fn:with_request_config, fn:subscribe_to_send_progress
- `crates/matrix-sdk/src/encryption/identities/devices.rs`: struct:DeviceUpdates, struct:Device, fn:request_verification,
  fn:request_verification_with_methods, fn:start_verification, fn:verify, fn:is_verified,
  fn:is_verified_with_cross_signing, fn:set_local_trust, fn:is_cross_signed_by_owner, struct:UserDevices, fn:get,
  fn:keys, fn:devices
- `crates/matrix-sdk/src/encryption/identities/mod.rs`: use:devices::{Device,,
  use:matrix_sdk_base::crypto::types::MasterPubkey, use:users::{IdentityUpdates,, enum:ManualVerifyError,
  enum:RequestVerificationError
- `crates/matrix-sdk/src/encryption/identities/users.rs`: struct:IdentityUpdates, struct:UserIdentity, fn:user_id,
  fn:request_verification, fn:request_verification_with_methods, fn:verify, fn:is_verified, fn:was_previously_verified,
  fn:withdraw_verification, fn:has_verification_violation, fn:pin, fn:master_key
- `crates/matrix-sdk/src/encryption/mod.rs`: mod:backups, mod:dehydrated_devices, mod:futures, mod:identities,
  mod:recovery, mod:secret_storage, mod:verification, use:matrix_sdk_base::crypto::{,
  use:crate::error::RoomKeyImportError, enum:BundleExportError, enum:BundleImportError,
  fn:export_secrets_bundle_from_store, fn:new, fn:initialize_tasks, fn:initialize_recovery_state_update_task,
  struct:EncryptionSettings, enum:BackupDownloadStrategy, enum:VerificationState, struct:CrossSigningResetHandle, fn:new,
  fn:auth_type, fn:auth, fn:cancel, enum:CrossSigningResetAuthType, struct:OAuthCrossSigningResetInfo,
  struct:DuplicateOneTimeKeyErrorMessage, fn:upload_encrypted_file, fn:olm_machine_for_testing,
  fn:abort_bundle_receiver_task, struct:Encryption, fn:ed25519_key, fn:curve25519_key, fn:device_creation_timestamp,
  fn:import_secrets_bundle, fn:cross_signing_status, fn:has_devices_to_verify_against, fn:tracked_users,
  fn:verification_state, fn:get_verification, fn:get_verification_request, fn:get_device, fn:get_own_device,
  fn:get_user_devices, fn:get_user_identity, fn:request_user_identity, fn:devices_stream, fn:user_identities_stream,
  fn:bootstrap_cross_signing, fn:reset_cross_signing, fn:bootstrap_cross_signing_if_needed, fn:export_room_keys,
  fn:import_room_keys, fn:room_keys_received_stream, fn:historic_room_key_stream, fn:secret_storage, fn:backups,
  fn:recovery, fn:dehydrated_devices, fn:enable_cross_process_store_lock, fn:spin_lock_store, fn:try_lock_store_once,
  fn:lock_store, fn:uploaded_key_count, fn:wait_for_e2ee_initialization_tasks, fn:encrypt_and_send_raw_to_device
- `crates/matrix-sdk/src/encryption/recovery/futures.rs`: struct:Enable, fn:subscribe_to_progress,
  fn:wait_for_backups_to_upload, fn:with_passphrase, struct:Reset, fn:with_passphrase, struct:RecoverAndReset,
  fn:with_passphrase
- `crates/matrix-sdk/src/encryption/recovery/mod.rs`: mod:futures, use:self::types::{EnableProgress,, struct:Recovery,
  const:KNOWN_SECRETS, fn:state, fn:state_stream, fn:enable, fn:enable_backup, fn:disable, fn:reset_key,
  fn:recover_and_reset, fn:reset_identity, fn:recover, fn:recover_and_fix_backup, fn:is_last_device,
  struct:IdentityResetHandle, fn:auth_type, fn:reset, fn:cancel
- `crates/matrix-sdk/src/encryption/recovery/types.rs`: type:Result, enum:RecoveryError, enum:EnableProgress,
  enum:RecoveryState
- `crates/matrix-sdk/src/encryption/secret_storage/futures.rs`: struct:CreateStore, fn:with_passphrase
- `crates/matrix-sdk/src/encryption/secret_storage/mod.rs`: use:futures::CreateStore, use:secret_store::SecretStore,
  type:Result, enum:ImportError, enum:SecretStorageError, enum:DecryptionError, struct:SecretStorage,
  fn:open_secret_store, fn:create_secret_store, fn:is_enabled, fn:fetch_default_key_id
- `crates/matrix-sdk/src/encryption/secret_storage/secret_store.rs`: struct:SecretStore, fn:secret_storage_key,
  fn:get_secret, fn:put_secret, fn:import_secrets, fn:export_secrets
- `crates/matrix-sdk/src/encryption/tasks.rs`: fn:to_room_key_info, type:RoomKeyInfo, fn:new, fn:should_download, fn:new
- `crates/matrix-sdk/src/encryption/verification/mod.rs`: use:matrix_sdk_base::crypto::{, use:matrix_sdk_base::crypto::{,
  use:qrcode::QrVerification, use:requests::{VerificationRequest,, use:sas::SasVerification, enum:Verification, fn:sas,
  fn:qr, fn:is_done, fn:is_cancelled, fn:cancel_info, fn:own_user_id, fn:other_user_id, fn:is_self_verification,
  fn:we_started, fn:room_id
- `crates/matrix-sdk/src/encryption/verification/qrcode.rs`: struct:QrVerification, fn:own_user_id,
  fn:is_self_verification, fn:is_done, fn:has_been_scanned, fn:we_started, fn:cancel_info, fn:other_user_id,
  fn:other_device, fn:is_cancelled, fn:to_qr_code, fn:to_bytes, fn:confirm, fn:cancel, fn:changes, fn:state, fn:room_id
- `crates/matrix-sdk/src/encryption/verification/requests.rs`: struct:VerificationRequest, enum:VerificationRequestState,
  fn:is_done, fn:is_cancelled, fn:flow_id, fn:cancel_info, fn:own_user_id, fn:is_passive, fn:is_ready, fn:we_started,
  fn:other_user_id, fn:is_self_verification, fn:their_supported_methods, fn:accept, fn:accept_with_methods,
  fn:generate_qr_code, fn:scan_qr_code, fn:start_sas, fn:cancel, fn:changes, fn:state, fn:room_id
- `crates/matrix-sdk/src/encryption/verification/sas.rs`: struct:SasVerification, fn:accept, fn:accept_with_settings,
  fn:confirm, fn:mismatch, fn:cancel, fn:emoji, fn:decimals, fn:supports_emoji, fn:is_done, fn:can_be_presented,
  fn:we_started, fn:cancel_info, fn:is_cancelled, fn:other_device, fn:started_from_request, fn:is_self_verification,
  fn:own_user_id, fn:other_user_id, fn:changes, fn:state, fn:room_id
- `crates/matrix-sdk/src/error.rs`: type:Result, type:HttpResult, type:RumaApiError, enum:HttpError,
  fn:as_ruma_api_error, fn:as_client_api_error, fn:client_api_error_kind, fn:as_uiaa_response,
  fn:is_endpoint_not_implemented, enum:Error, fn:as_ruma_api_error, fn:as_client_api_error, fn:client_api_error_kind,
  fn:as_uiaa_response, enum:RoomKeyImportError, enum:BeaconError, enum:RefreshTokenError, enum:NotificationSettingsError,
  fn:is_rule_not_found, struct:WrongRoomState
- `crates/matrix-sdk/src/event_cache/back_pagination_queue.rs`: struct:BackPaginationQueue
- `crates/matrix-sdk/src/event_cache/caches/aggregator.rs`: fn:aggregate_timeline_for_room,
  fn:aggregate_timeline_for_threads, fn:aggregate_timeline_for_pinned_events
- `crates/matrix-sdk/src/event_cache/caches/event_focused/mod.rs`: enum:EventFocusThreadMode,
  struct:EventFocusedCacheState, fn:reload, struct:EventFocusedCache, fn:events, fn:subscribe, fn:hit_timeline_start,
  fn:hit_timeline_end, fn:paginate_backwards, fn:paginate_forwards, fn:thread_root, struct:EventFocusedCacheKey,
  type:EventFocusedCacheUpdateSender
- `crates/matrix-sdk/src/event_cache/caches/event_linked_chunk.rs`: use:matrix_sdk_base::event_cache::{Event,, fn:new,
  fn:with_initial_linked_chunk, fn:reset, fn:remove_events_by_position, fn:replace_event_at, fn:chunk_identifier,
  fn:first_chunk, fn:chunks, fn:rchunks, fn:revents, fn:events, fn:event_order, fn:updates_as_vector_diffs,
  fn:debug_string, fn:rgap, fn:push_gap, fn:push_live_events, fn:push_backwards_pagination_events,
  fn:push_forwards_pagination_events, fn:find_event, fn:replace_utds, fn:first_chunk_as_gap, fn:last_chunk_as_gap
- `crates/matrix-sdk/src/event_cache/caches/mod.rs`: mod:event_focused, mod:event_linked_chunk, mod:pagination,
  mod:pinned_events, mod:room, mod:subscriber, mod:thread, fn:new, fn:room, fn:thread, fn:pinned_events,
  fn:event_focused, fn:all_in_memory_events, fn:all_events_of_type, struct:TimelineVectorDiffs
- `crates/matrix-sdk/src/event_cache/caches/pagination.rs`: fn:new, fn:run_backwards_until, fn:run_backwards_once,
  enum:PaginationStatus, struct:BackPaginationOutcome
- `crates/matrix-sdk/src/event_cache/caches/pinned_events/mod.rs`: struct:PinnedEventsCacheState, fn:reload,
  fn:remove_events, fn:replace_event_at, fn:save_events, fn:propagate_changes, struct:PinnedEventsCache, fn:subscribe
- `crates/matrix-sdk/src/event_cache/caches/pinned_events/updates.rs`: struct:PinnedEventsCacheUpdateSender, fn:new,
  fn:send
- `crates/matrix-sdk/src/event_cache/caches/read_receipts.rs`: trait:EventFilter, struct:RoomReadReceiptEventFilter,
  fn:new, struct:ThreadReadReceiptEventFilter, fn:new
- `crates/matrix-sdk/src/event_cache/caches/room/mod.rs`: mod:pagination, use:self::{, struct:RoomEventCache, fn:room_id,
  fn:events, fn:subscribe, fn:pagination, fn:rfind_map_event_in_memory_by, fn:find_event, fn:find_event_with_relations,
  fn:find_event_relations, fn:debug_string
- `crates/matrix-sdk/src/event_cache/caches/room/pagination.rs`: use:super::super::pagination::PaginationStatus,
  struct:PaginationStatusSubscriber, fn:get, fn:next, fn:next_now, struct:RoomPagination, fn:run_backwards_until,
  fn:run_backwards_once, fn:status
- `crates/matrix-sdk/src/event_cache/caches/room/state.rs`: struct:RoomEventCacheState, fn:new, fn:room_linked_chunk,
  fn:subscribers_handle, fn:find_event, fn:find_event_with_relations, fn:find_event_relations,
  fn:rfind_map_event_in_memory_by, fn:is_dirty, fn:room_linked_chunk_mut, fn:waited_for_initial_prev_token,
  fn:waited_for_initial_prev_token_mut, fn:find_event, fn:reload, fn:auto_shrink_if_no_subscribers, fn:remove_events,
  fn:handle_sync, fn:update_read_receipts, fn:update_thread_summary, fn:replace_event_at, fn:save_events, fn:is_dirty
- `crates/matrix-sdk/src/event_cache/caches/room/updates.rs`: enum:RoomEventCacheUpdate,
  struct:RoomEventCacheGenericUpdate, struct:RoomEventCacheLinkedChunkUpdate, fn:events,
  struct:RoomEventCacheUpdateSender, fn:new, fn:send
- `crates/matrix-sdk/src/event_cache/caches/subscriber.rs`: struct:SubscribersHandle, fn:count, fn:new_subscriber_handle,
  struct:SubscriberHandle, fn:count, struct:Subscriber, enum:AutoShrinkMessage
- `crates/matrix-sdk/src/event_cache/caches/thread/mod.rs`: mod:pagination, struct:ThreadEventCache, fn:room_id,
  fn:thread_id, fn:num_unread_messages, fn:num_unread_notifications, fn:num_unread_mentions, fn:read_receipts,
  fn:subscribe, fn:pagination, fn:find_event_with_relations
- `crates/matrix-sdk/src/event_cache/caches/thread/pagination.rs`: struct:ThreadPagination, fn:run_backwards_until,
  fn:run_backwards_once
- `crates/matrix-sdk/src/event_cache/caches/thread/state.rs`: struct:ThreadEventCacheState, fn:new, fn:propagate_changes,
  fn:thread_linked_chunk, fn:subscribers_handle, fn:compute_thread_summary, fn:find_event_with_relations,
  fn:thread_linked_chunk, fn:thread_linked_chunk_mut, fn:waited_for_initial_prev_token,
  fn:waited_for_initial_prev_token_mut, fn:reload, fn:handle_sync, fn:update_read_receipts, fn:replace_event_at,
  fn:save_events, fn:remove_events, fn:auto_shrink_if_no_subscribers
- `crates/matrix-sdk/src/event_cache/caches/thread/updates.rs`: struct:ThreadEventCacheUpdateSender, fn:new, fn:send
- `crates/matrix-sdk/src/event_cache/deduplicator.rs`: fn:filter_duplicate_events
- `crates/matrix-sdk/src/event_cache/mod.rs`: use:redecryptor::{DecryptionRetryRequest,, use:self::{,
  enum:EventCacheError, type:Result, struct:EventCacheDropHandles, struct:EventCache, fn:config, fn:config_mut,
  fn:subscribe_thread_subscriber_updates, fn:subscribe, fn:handle_room_updates, fn:has_subscribed, fn:room, fn:thread,
  fn:pinned_events, fn:event_focused, fn:forget_room, fn:clear_all_rooms, fn:subscribe_to_room_generic_updates,
  fn:back_pagination_queue, struct:EventCacheConfig, const:DEFAULT_MAX_EVENTS_TO_LOAD,
  const:DEFAULT_MAX_CONCURRENT_REQUESTS, const:DEFAULT_MAX_CONCURRENT_BACK_PAGINATIONS, enum:EventsOrigin
- `crates/matrix-sdk/src/event_cache/persistence.rs`: fn:find_event, fn:find_event_with_relations,
  fn:find_event_relations
- `crates/matrix-sdk/src/event_cache/redecryptor.rs`: fn:try_resolve_event, fn:as_resolved,
  struct:DecryptionRetryRequest, enum:RedecryptorReport, fn:request_decryption, fn:subscribe_to_decryption_reports
- `crates/matrix-sdk/src/event_cache/states/mod.rs`: struct:State, struct:StateLock, fn:new, struct:StateLockReadGuard,
  enum:StateLockReadGuardKind, struct:StateLockWriteGuard, enum:StateLockWriteGuardKind, struct:CacheStateLock, fn:read,
  fn:write, fn:reload_no_preprocessing, enum:ReloadPreprocessing
- `crates/matrix-sdk/src/event_cache/states/selectors.rs`: trait:CacheState, fn:new, struct:RoomStateSelector, fn:new,
  struct:ThreadStateSelector, fn:new, struct:PinnedEventsStateSelector, fn:new, struct:EventFocusedStateSelector, fn:new
- `crates/matrix-sdk/src/event_cache/tasks.rs`: (no direct public declarations)
- `crates/matrix-sdk/src/event_handler/context.rs`: trait:EventHandlerContext, struct:RawEvent, struct:Ctx
- `crates/matrix-sdk/src/event_handler/maps.rs`: fn:add, fn:get_handlers, fn:remove, fn:len
- `crates/matrix-sdk/src/event_handler/mod.rs`: use:self::context::{Ctx,, fn:add_handler, fn:add_context, fn:remove,
  enum:HandlerKind, trait:SyncEvent, struct:EventHandlerHandle, trait:EventHandler, trait:EventHandlerFuture,
  struct:EventHandlerData, trait:EventHandlerResult, struct:EventHandlerDropGuard, struct:ObservableEventHandler,
  fn:subscribe, struct:EventHandlerSubscriber
- `crates/matrix-sdk/src/event_handler/static_events.rs`: (no direct public declarations)
- `crates/matrix-sdk/src/http_client/mod.rs`: fn:send, struct:TransmissionProgress, trait:SupportedAuthScheme,
  trait:SupportedPathBuilder
- `crates/matrix-sdk/src/http_client/native.rs`: (no direct public declarations)
- `crates/matrix-sdk/src/http_client/wasm.rs`: (no direct public declarations)
- `crates/matrix-sdk/src/latest_events/error.rs`: enum:LatestEventsError
- `crates/matrix-sdk/src/latest_events/latest_event/builder.rs`: use:matrix_sdk_base::latest_event::{LatestEventValue,,
  fn:new_remote, fn:new_remote_for_invite, fn:new_local, fn:new, fn:is_empty, struct:FilterContinue,
  fn:filter_timeline_event
- `crates/matrix-sdk/src/latest_events/latest_event/mod.rs`: use:builder::filter_timeline_event,
  use:matrix_sdk_base::latest_event::{, fn:new, fn:subscribe, fn:get, fn:update_with_event_cache,
  fn:update_with_send_queue, fn:update_with_room_info, fn:map, fn:inner, fn:unzip
- `crates/matrix-sdk/src/latest_events/mod.rs`: use:error::LatestEventsError, use:latest_event::{LatestEventValue,,
  struct:LatestEvents, fn:listen_to_room, fn:is_listening_to_room, fn:listen_and_subscribe_to_room, fn:listen_to_thread,
  fn:listen_and_subscribe_to_thread, fn:forget_room, fn:forget_thread, fn:for_room, fn:for_thread, fn:forget_room,
  fn:forget_thread
- `crates/matrix-sdk/src/latest_events/room_latest_events.rs`: fn:new, fn:read, fn:write, fn:for_room, fn:for_thread,
  fn:per_thread, fn:has_thread, fn:create_and_insert_latest_event_for_thread, fn:forget_thread,
  fn:update_with_event_cache, fn:update_with_send_queue, fn:update_with_room_info
- `crates/matrix-sdk/src/lib.rs`: use:async_trait::async_trait, use:bytes, use:matrix_sdk_base::{,
  use:matrix_sdk_common::*, use:reqwest, mod:attachment, mod:authentication, mod:config, mod:encryption, mod:event_cache,
  mod:event_handler, mod:latest_events, mod:media, mod:notification_settings, mod:paginators, mod:pusher, mod:room,
  mod:room_directory_search, mod:room_preview, mod:send_queue, mod:utils, mod:futures,
  use:super::client::futures::SendRequest, mod:sliding_sync, mod:sync, mod:widget, mod:message_search,
  use:account::Account, use:authentication::{AuthApi,, use:client::homeserver_capabilities::HomeserverCapabilities,
  mod:search_index, use:client::{, use:error::{, use:http_client::{SupportedAuthScheme,,
  use:matrix_sdk_sqlite::SqliteCryptoStore, use:matrix_sdk_sqlite::{, use:media::Media, use:pusher::Pusher,
  use:room::Room, use:ruma::{IdParseError,, use:sliding_sync::{, mod:live_locations_observer, mod:test_utils
- `crates/matrix-sdk/src/live_locations_observer.rs`: struct:LastLocation, struct:LiveLocationShare,
  struct:BeaconInfoUpdate, struct:LiveLocationsObserver, fn:subscribe
- `crates/matrix-sdk/src/media.rs`: use:matrix_sdk_base::media::{store::MediaRetentionPolicy,, struct:Media,
  struct:MediaFileHandle, fn:path, fn:persist, struct:PersistError, struct:PreallocatedMxcUri, enum:MediaError,
  trait:MediaFetcher, fn:upload, fn:create_content_uri, fn:upload_preallocated, fn:get_media_file, fn:get_media_content,
  fn:remove_media_content, fn:remove_media_content_for_uri, fn:get_file, fn:remove_file, fn:get_thumbnail,
  fn:remove_thumbnail, fn:get_media_preview, fn:set_media_retention_policy, fn:media_retention_policy, fn:clean,
  struct:DefaultMediaFetcher
- `crates/matrix-sdk/src/message_search.rs`: fn:search, enum:SearchError, fn:search_messages, fn:search_messages_events,
  struct:GlobalSearchBuilder, fn:only_dm_rooms, fn:no_dms, fn:build, fn:build_events, fn:search_messages
- `crates/matrix-sdk/src/notification_settings/command.rs`: (no direct public declarations)
- `crates/matrix-sdk/src/notification_settings/mod.rs`: use:matrix_sdk_base::notification_settings::RoomNotificationMode,
  enum:IsEncrypted, enum:IsOneToOne, struct:NotificationSettings, fn:subscribe_to_changes,
  fn:get_user_defined_room_notification_mode, fn:get_default_room_notification_mode,
  fn:get_rooms_with_user_defined_rules, fn:contains_keyword_rules, fn:is_push_rule_enabled, fn:set_push_rule_enabled,
  fn:set_default_room_notification_mode, fn:set_underride_push_rule_actions, fn:create_custom_conditional_push_rule,
  fn:set_room_notification_mode, fn:delete_user_defined_room_rules, fn:unmute_room, fn:enabled_keywords, fn:add_keyword,
  fn:remove_keyword, fn:ruleset
- `crates/matrix-sdk/src/notification_settings/rule_commands.rs`: (no direct public declarations)
- `crates/matrix-sdk/src/notification_settings/rules.rs`: (no direct public declarations)
- `crates/matrix-sdk/src/paginators/mod.rs`: mod:thread, use:room::*, enum:PaginationToken, fn:into_token,
  struct:PaginationResult, enum:PaginatorError
- `crates/matrix-sdk/src/paginators/room.rs`: enum:PaginatorState, struct:PaginationTokens, struct:Paginator,
  struct:StartFromResult, fn:new, fn:state, fn:start_from, fn:paginate_backward, fn:hit_timeline_start,
  fn:hit_timeline_end, fn:paginate_forward, fn:tokens, trait:PaginableRoom
- `crates/matrix-sdk/src/paginators/thread.rs`: trait:PaginableThread, struct:ThreadedEventsLoader, fn:new,
  fn:paginate_backwards, fn:paginate_forwards, fn:thread_root_event_id
- `crates/matrix-sdk/src/pusher.rs`: struct:Pusher, fn:set, fn:delete
- `crates/matrix-sdk/src/room/calls.rs`: enum:CallError, fn:make_decline_call_event, fn:subscribe_to_call_decline_events
- `crates/matrix-sdk/src/room/edit.rs`: enum:EditedContent, enum:EditError, fn:make_edit_event
- `crates/matrix-sdk/src/room/futures.rs`: struct:SendMessageLikeEventResult, struct:SendMessageLikeEvent,
  fn:with_transaction_id, fn:with_request_config, struct:SendRawMessageLikeEvent, fn:with_transaction_id,
  fn:with_request_config, struct:SendAttachment, fn:with_send_progress_observable, fn:store_in_cache,
  struct:SendRawStateEvent, fn:with_request_config, struct:SendStateEvent, fn:with_request_config
- `crates/matrix-sdk/src/room/identity_status_changes.rs`: struct:IdentityStatusChanges, fn:create_stream
- `crates/matrix-sdk/src/room/knock_requests.rs`: struct:KnockRequest, fn:room_id, fn:mark_as_seen, fn:accept,
  fn:decline, fn:decline_and_ban, struct:KnockRequestMemberInfo
- `crates/matrix-sdk/src/room/member.rs`: struct:RoomMember, fn:avatar, fn:ignore, fn:unignore, fn:is_account_user,
  fn:suggested_role_for_power_level, enum:RoomMemberRole, fn:suggested_role_for_power_level, fn:suggested_power_level
- `crates/matrix-sdk/src/room/messages.rs`: struct:MessagesOptions, fn:new, fn:backward, fn:forward, fn:from,
  struct:Messages, struct:EventWithContextResponse, struct:ListThreadsOptions, struct:ThreadRoots, enum:IncludeRelations,
  struct:RelationsOptions, struct:Relations
- `crates/matrix-sdk/src/room/mod.rs`: use:identity_status_changes::IdentityStatusChanges,
  use:matrix_sdk_base::store::StoredThreadSubscription, use:self::{, mod:edit, mod:futures, mod:identity_status_changes,
  mod:knock_requests, mod:power_levels, mod:reply, mod:calls, mod:privacy_settings, struct:Room,
  struct:ThreadSubscription, struct:PushContext, fn:new, fn:for_event, fn:traced_for_event, fn:leave, fn:join, fn:client,
  fn:is_synced, fn:avatar, fn:messages, fn:add_event_handler, fn:subscribe_to_updates,
  fn:subscribe_to_typing_notifications, fn:subscribe_to_identity_status_changes, fn:live_locations_observer, fn:event,
  fn:load_or_fetch_event, fn:load_or_fetch_event_with_relations, fn:event_with_context, fn:request_encryption_state,
  fn:encryption_state, fn:latest_encryption_state, fn:crypto_context_info, fn:sync_members, fn:get_member,
  fn:get_member_no_sync, fn:members, fn:members_no_sync, fn:human_member_ids, fn:human_member_ids_no_sync,
  fn:set_own_member_display_name, fn:get_state_events, fn:get_state_events_static, fn:get_state_events_for_keys,
  fn:get_state_events_for_keys_static, fn:get_state_event, fn:get_state_event_static, fn:get_state_event_static_for_key,
  fn:parent_spaces, fn:account_data, fn:account_data_static, fn:contains_only_verified_devices, fn:set_account_data,
  fn:set_account_data_raw, fn:set_tag, fn:remove_tag, fn:set_is_favourite, fn:set_is_low_priority, fn:set_is_direct,
  fn:decrypt_event, fn:decrypt_event, fn:get_encryption_info, fn:discard_room_key, fn:ban_user, fn:unban_user,
  fn:kick_user, fn:invite_user_by_id, fn:invite_user_by_3pid, fn:typing_notice, fn:send_single_receipt,
  fn:send_multiple_receipts, fn:enable_encryption, fn:enable_encryption_with_state_event_encryption, fn:sync_up, fn:send,
  fn:send_raw, fn:send_attachment, fn:update_power_levels, fn:apply_power_level_changes, fn:reset_power_levels,
  fn:get_suggested_user_role, fn:get_user_power_level, fn:users_with_power_levels, fn:set_name, fn:set_room_topic,
  fn:set_avatar_url, fn:remove_avatar, fn:upload_avatar, fn:send_state_event, fn:send_state_event,
  fn:send_state_event_for_key, fn:send_state_event_for_key, fn:send_state_event_raw, fn:send_state_event_raw, fn:redact,
  fn:route, fn:matrix_to_permalink, fn:matrix_permalink, fn:matrix_to_event_permalink, fn:matrix_event_permalink,
  fn:load_user_receipt, fn:load_event_receipts, fn:push_condition_room_ctx, fn:push_context, fn:event_push_actions,
  fn:invite_details, fn:member_with_sender_info, fn:forget, fn:notification_mode, fn:user_defined_notification_mode,
  fn:report_content, fn:report_room, fn:set_unread_flag, fn:event_cache, fn:start_live_location_share,
  fn:stop_live_location_share, fn:send_location_beacon, fn:save_composer_draft, fn:load_composer_draft,
  fn:clear_composer_draft, fn:load_pinned_events, fn:subscribe_to_knock_requests, fn:effective_retention,
  fn:effective_retention_with_server_config, fn:privacy_settings, fn:list_threads, fn:relations, fn:subscribe_thread,
  fn:subscribe_thread_if_needed, fn:unsubscribe_thread, fn:fetch_thread_subscription,
  fn:load_or_fetch_thread_subscription, fn:pin_event, fn:unpin_event, fn:compute_is_dm, fn:is_dm, fn:new, fn:get,
  fn:room_id, struct:Invite, struct:Receipts, fn:new, fn:fully_read_marker, fn:public_read_receipt,
  fn:private_read_receipt, fn:is_empty, enum:ParentSpace, struct:RoomMemberWithSenderInfo
- `crates/matrix-sdk/src/room/power_levels.rs`: struct:RoomPowerLevelChanges, fn:new, fn:power_level_user_changes
- `crates/matrix-sdk/src/room/privacy_settings.rs`: struct:RoomPrivacySettings, fn:publish_room_alias_in_room_directory,
  fn:remove_room_alias_from_room_directory, fn:update_canonical_alias, fn:update_room_history_visibility,
  fn:update_join_rule, fn:update_room_retention, fn:get_room_visibility, fn:update_room_visibility
- `crates/matrix-sdk/src/room/reply.rs`: struct:Reply, enum:ReplyError, enum:EnforceThread, fn:make_reply_event
- `crates/matrix-sdk/src/room/shared_room_history.rs`: (no direct public declarations)
- `crates/matrix-sdk/src/room_directory_search.rs`: struct:RoomDescription, struct:RoomDirectorySearch, fn:new,
  fn:search, fn:next_page, fn:results, fn:loaded_pages, fn:is_at_last_page
- `crates/matrix-sdk/src/room_preview.rs`: struct:RoomPreview, fn:from_room_summary, fn:from_state_events
- `crates/matrix-sdk/src/search_index/mod.rs`: enum:SearchIndexStoreKind, struct:SearchIndex, fn:new, fn:lock,
  struct:SearchIndexGuard, fn:handle_timeline_event, fn:bulk_handle_timeline_event
- `crates/matrix-sdk/src/send_queue/mod.rs`: use:progress::AbstractProgress, struct:SendQueue,
  fn:respawn_tasks_for_rooms_with_unsent_requests, fn:set_enabled, fn:is_enabled, fn:enable_upload_progress,
  fn:subscribe, fn:local_echoes, fn:subscribe_errors, struct:SendQueueRoomError, fn:send_queue, fn:new, fn:send_queue,
  struct:RoomSendQueue, fn:send_raw, fn:send, fn:redact, fn:subscribe, fn:is_enabled, fn:set_enabled,
  enum:LocalEchoContent, struct:LocalEcho, enum:RoomSendQueueUpdate, struct:SendQueueUpdate, enum:RoomSendQueueError,
  enum:RoomSendQueueStorageError, struct:SendEvent, fn:with_extra_content, struct:SendHandle, fn:abort,
  fn:abort_with_reason, fn:edit_raw, fn:edit, fn:edit_media_caption, fn:unwedge, fn:react, struct:SendReactionHandle,
  fn:abort, fn:transaction_id, struct:SendRedactionHandle, fn:abort
- `crates/matrix-sdk/src/send_queue/progress.rs`: struct:AbstractProgress
- `crates/matrix-sdk/src/send_queue/upload.rs`: fn:send_attachment, fn:send_gallery
- `crates/matrix-sdk/src/sliding_sync/builder.rs`: struct:SlidingSyncBuilder, fn:version, fn:add_list,
  fn:add_cached_list, fn:with_all_extensions, fn:with_e2ee_extension, fn:without_e2ee_extension,
  fn:with_to_device_extension, fn:without_to_device_extension, fn:with_account_data_extension,
  fn:without_account_data_extension, fn:with_typing_extension, fn:without_typing_extension, fn:with_receipt_extension,
  fn:without_receipt_extension, fn:with_thread_subscriptions_extension, fn:without_thread_subscriptions_extension,
  fn:with_profiles_extension, fn:without_profiles_extension, fn:poll_timeout, fn:network_timeout, fn:share_pos, fn:build
- `crates/matrix-sdk/src/sliding_sync/cache.rs`: (no direct public declarations)
- `crates/matrix-sdk/src/sliding_sync/client.rs`: enum:Version, enum:VersionBuilderError, enum:VersionBuilder, fn:build,
  fn:available_sliding_sync_versions, fn:sliding_sync, fn:process_sliding_sync_test_helper, fn:new, fn:handle_encryption,
  fn:handle_room_response, fn:handle_thread_subscriptions, fn:process_and_take_response
- `crates/matrix-sdk/src/sliding_sync/error.rs`: enum:Error
- `crates/matrix-sdk/src/sliding_sync/list/builder.rs`: struct:SlidingSyncListBuilder, fn:once_built, fn:once_built,
  fn:sync_mode, fn:requires_timeout, fn:requires_timeout, fn:required_state, fn:filters, fn:timeline_limit,
  fn:no_timeline_limit
- `crates/matrix-sdk/src/sliding_sync/list/frozen.rs`: struct:FrozenSlidingSyncList
- `crates/matrix-sdk/src/sliding_sync/list/mod.rs`: use:self::builder::*, type:Bound, type:Range, type:Ranges,
  struct:SlidingSyncList, fn:builder, fn:name, fn:set_sync_mode, fn:state, fn:state_stream, fn:timeline_limit,
  fn:set_timeline_limit, fn:maximum_number_of_rooms, fn:maximum_number_of_rooms_stream, fn:sync_mode, fn:set_sync_mode,
  enum:SlidingSyncListLoadingState, struct:SlidingSyncSelectiveModeBuilder, fn:add_range, fn:add_ranges,
  struct:SlidingSyncWindowedModeBuilder, fn:maximum_number_of_rooms_to_fetch, enum:SlidingSyncMode, fn:new_selective,
  fn:new_paging, fn:new_growing
- `crates/matrix-sdk/src/sliding_sync/list/request_generator.rs`: enum:SlidingSyncListRequestGeneratorKind,
  struct:SlidingSyncListRequestGenerator, fn:requested_ranges, fn:kind, fn:is_fully_loaded, fn:is_selective
- `crates/matrix-sdk/src/sliding_sync/mod.rs`: use:client::{Version,, use:self::{builder::*,, struct:SlidingSync,
  fn:builder, fn:add_room_subscriptions, fn:remove_room_subscriptions, fn:set_room_subscriptions,
  fn:reset_and_add_room_subscriptions, fn:on_list, fn:add_list, fn:add_cached_list, fn:sync_once, fn:sync, fn:stop_sync,
  fn:expire_session, fn:set_pos, struct:UpdateSummary, enum:PollTimeout
- `crates/matrix-sdk/src/sync.rs`: use:matrix_sdk_base::sync::*, struct:SyncResponse, enum:RoomUpdate
- `crates/matrix-sdk/src/test_utils/client.rs`: struct:MockClientBuilder, fn:new, fn:no_server_versions,
  fn:server_versions, fn:unlogged, fn:registered_with_oauth, fn:logged_in_with_oauth, fn:logged_in_with_token,
  fn:on_builder, fn:build, fn:mock_session_meta, fn:mock_session_tokens, fn:mock_session_tokens_with_refresh,
  fn:mock_prev_session_tokens_with_refresh, fn:mock_matrix_session, mod:oauth, fn:mock_client_id, fn:mock_redirect_uri,
  fn:mock_client_metadata, fn:mock_session
- `crates/matrix-sdk/src/test_utils/mocks/encryption.rs`: type:PendingToDeviceMessages,
  fn:client_builder_for_crypto_end_to_end, fn:exhaust_one_time_keys, fn:exchange_e2ee_identities,
  fn:set_up_alice_and_bob_for_encryption, fn:set_up_carl_for_encryption, fn:set_up_new_device_for_encryption,
  fn:mock_crypto_endpoints_preset, fn:mock_capture_put_to_device, fn:mock_capture_put_to_device_then_sync_back,
  fn:capture_put_to_device_traffic, fn:sync_back_pending_to_device_messages
- `crates/matrix-sdk/src/test_utils/mocks/mod.rs`: mod:encryption, mod:oauth, struct:MatrixMockServer, fn:new,
  fn:from_server, fn:client_builder, fn:server, fn:uri, fn:oauth, fn:sync_room, fn:sync_joined_room, fn:verify_and_reset,
  fn:mock_sync, fn:mock_sliding_sync, fn:mock_room_join, fn:mock_room_send, fn:mock_room_send_state,
  fn:mock_room_state_encryption, fn:mock_set_room_state_encryption, fn:mock_room_redact, fn:mock_room_event,
  fn:mock_room_event_context, fn:mock_room_messages, fn:mock_upload, fn:mock_room_directory_resolve_alias,
  fn:mock_room_directory_create_room_alias, fn:mock_room_directory_remove_room_alias, fn:mock_public_rooms,
  fn:mock_room_directory_set_room_visibility, fn:mock_room_directory_get_room_visibility, fn:mock_room_keys_version,
  fn:mock_add_room_keys_version, fn:mock_delete_room_keys_version, fn:mock_send_to_device, fn:mock_get_members,
  fn:mock_invite_user_by_id, fn:mock_kick_user, fn:mock_ban_user, fn:mock_versions, fn:mock_room_summary,
  fn:mock_set_room_pinned_events, fn:mock_who_am_i, fn:mock_upload_keys, fn:mock_query_keys, fn:mock_well_known,
  fn:mock_upload_cross_signing_keys, fn:mock_upload_cross_signing_signatures, fn:mock_get_dehydrated_device,
  fn:mock_put_dehydrated_device, fn:mock_delete_dehydrated_device, fn:mock_dehydrated_device_events, fn:mock_room_leave,
  fn:mock_room_forget, fn:mock_logout, fn:mock_room_threads, fn:mock_room_relations, fn:mock_get_recent_emojis,
  fn:mock_add_recent_emojis, fn:mock_get_default_secret_storage_key, fn:mock_get_secret_storage_key,
  fn:mock_get_master_signing_key, fn:mock_send_receipt, fn:mock_send_read_markers, fn:mock_set_room_account_data,
  fn:mock_authenticated_media_config, fn:mock_media_config, fn:mock_login, fn:mock_devices, fn:mock_get_device,
  fn:mock_user_directory, fn:mock_create_room, fn:mock_upgrade_room, fn:mock_media_allocate,
  fn:mock_media_allocated_upload, fn:mock_media_download, fn:mock_media_thumbnail, fn:mock_authed_media_download,
  fn:mock_media_preview, fn:mock_authed_media_preview, fn:mock_authed_media_thumbnail,
  fn:mock_room_get_thread_subscription, fn:mock_room_put_thread_subscription, fn:mock_room_delete_thread_subscription,
  fn:mock_enable_push_rule, fn:mock_set_push_rules_actions, fn:mock_set_push_rules, fn:mock_delete_push_rules,
  fn:mock_federation_version, fn:mock_get_thread_subscriptions, fn:mock_get_hierarchy, fn:mock_set_space_child,
  fn:mock_set_space_parent, fn:mock_get_profile_field, fn:mock_set_profile_field, fn:mock_delete_profile_field,
  fn:mock_get_profile, fn:mock_get_homeserver_capabilities, enum:PushRuleIdSpec, fn:to_path, enum:AnyRoomBuilder,
  struct:MatrixMock, fn:expect, fn:named, fn:mock_once, fn:never, fn:up_to_n_times, fn:with_priority, fn:mount,
  fn:mount_as_scoped, struct:MockEndpoint, fn:expect_default_access_token, fn:expect_access_token,
  fn:expect_any_access_token, fn:expect_missing_access_token, fn:ignore_access_token, fn:expect_uiaa_auth_data,
  fn:respond_with, fn:error500, fn:error_unrecognized, fn:error_unknown_token, fn:error_too_large,
  struct:RoomSendEndpoint, fn:body_matches_partial_json, fn:for_type, fn:match_delayed_event, fn:ok, fn:ok_with_delay,
  fn:ok_with_capture, struct:RoomSendStateEndpoint, fn:body_matches_partial_json, fn:for_type, fn:match_delayed_event,
  fn:for_key, fn:ok, struct:SyncEndpoint, fn:timeout, fn:set_presence, fn:set_presence_missing, fn:ok, fn:ok_and_run,
  struct:EncryptionStateEndpoint, fn:encrypted, fn:state_encrypted, fn:plain, struct:SetEncryptionStateEndpoint, fn:ok,
  struct:RoomRedactEndpoint, fn:ok, struct:RoomEventEndpoint, fn:room, fn:match_event_id, fn:ok, fn:ok_with_template,
  struct:RoomContextResponseTemplate, fn:new, fn:events_before, fn:events_after, fn:start, fn:end, fn:state_events,
  struct:RoomEventContextEndpoint, fn:room, fn:match_event_id, fn:ok, struct:RoomMessagesEndpoint, fn:match_limit,
  fn:match_from, fn:ok, struct:RoomMessagesResponseTemplate, fn:events, fn:end_token, fn:with_delay,
  struct:UploadEndpoint, fn:expect_mime_type, fn:ok_with_capture, fn:ok, struct:ResolveRoomAliasEndpoint, fn:for_alias,
  fn:ok, fn:not_found, struct:CreateRoomAliasEndpoint, fn:ok, struct:RemoveRoomAliasEndpoint, fn:ok,
  struct:PublicRoomsEndpoint, fn:ok, fn:ok_with_via_params, struct:GetRoomVisibilityEndpoint, fn:ok,
  struct:SetRoomVisibilityEndpoint, fn:ok, struct:RoomKeysVersionEndpoint, fn:exists, fn:exists_with_key, fn:none,
  fn:error429, fn:error404, struct:AddRoomKeysVersionEndpoint, fn:ok, struct:DeleteRoomKeysVersionEndpoint, fn:ok,
  struct:SendToDeviceEndpoint, fn:ok, struct:GetRoomMembersEndpoint, fn:ok, struct:InviteUserByIdEndpoint, fn:ok,
  struct:KickUserEndpoint, fn:ok, struct:BanUserEndpoint, fn:ok, struct:VersionsEndpoint, fn:ok, fn:with_feature,
  fn:with_push_encrypted_events, fn:with_thread_subscriptions, fn:with_simplified_sliding_sync, fn:with_versions,
  struct:RoomSummaryEndpoint, fn:ok, struct:SetRoomPinnedEventsEndpoint, fn:ok, fn:unauthorized, struct:WhoAmIEndpoint,
  fn:ok, fn:ok_with_device_id, struct:UploadKeysEndpoint, fn:ok, fn:ok_with_signed_curve_key_count,
  struct:QueryKeysEndpoint, fn:ok, struct:WellKnownEndpoint, fn:ok, fn:ok_with_homeserver_url, fn:error404,
  struct:UploadCrossSigningKeysEndpoint, fn:ok, fn:uiaa_invalid_password, fn:uiaa, fn:uiaa_unstable_oauth,
  fn:uiaa_stable_oauth, struct:UploadCrossSigningSignaturesEndpoint, fn:ok, struct:GetDehydratedDeviceEndpoint, fn:ok,
  fn:not_found, struct:PutDehydratedDeviceEndpoint, fn:ok, fn:ok_echo, struct:DeleteDehydratedDeviceEndpoint, fn:ok,
  fn:not_found, struct:DehydratedDeviceEventsEndpoint, fn:ok, fn:match_next_batch, fn:match_missing_next_batch,
  struct:RoomLeaveEndpoint, fn:ok, fn:forbidden, struct:RoomForgetEndpoint, fn:ok, struct:LogoutEndpoint, fn:ok,
  struct:RoomThreadsEndpoint, fn:match_from, fn:ok, struct:RoomRelationsEndpoint, fn:match_from, fn:match_limit,
  fn:match_subrequest, fn:match_target_event, fn:ok, struct:GetRecentEmojisEndpoint, fn:ok,
  struct:UpdateRecentEmojisEndpoint, fn:match_emojis_in_request_body, fn:ok, struct:GetDefaultSecretStorageKeyEndpoint,
  fn:ok, struct:GetSecretStorageKeyEndpoint, fn:ok, struct:GetMasterSigningKeyEndpoint, fn:ok,
  struct:RoomRelationsResponseTemplate, fn:events, fn:next_batch, fn:prev_batch, fn:recursion_depth,
  struct:ReceiptEndpoint, fn:ok, fn:body_matches_partial_json, fn:body_json, fn:match_thread, fn:match_event_id,
  struct:ReadMarkersEndpoint, fn:ok, struct:RoomAccountDataEndpoint, fn:ok, struct:AuthenticatedMediaConfigEndpoint,
  fn:ok, fn:ok_default, struct:MediaConfigEndpoint, fn:ok, struct:LoginEndpoint, fn:ok, fn:ok_with,
  fn:body_matches_partial_json, struct:LoginResponseTemplate200, fn:new, fn:expires_in, fn:refresh_token, fn:well_known,
  struct:DevicesEndpoint, fn:ok, struct:GetDeviceEndpoint, fn:ok, struct:UserDirectoryEndpoint, fn:ok,
  struct:CreateRoomEndpoint, fn:ok, struct:UpgradeRoomEndpoint, fn:ok_with, struct:MediaAllocateEndpoint, fn:ok,
  struct:MediaAllocatedUploadEndpoint, fn:ok, struct:MediaDownloadEndpoint, fn:ok_plain_text, fn:ok_image,
  struct:MediaThumbnailEndpoint, fn:ok, struct:MediaPreviewEndpoint, fn:ok, fn:ok_empty,
  struct:AuthedMediaPreviewEndpoint, fn:ok, fn:ok_empty, struct:AuthedMediaDownloadEndpoint, fn:ok_plain_text,
  fn:ok_bytes, fn:ok_image, struct:AuthedMediaThumbnailEndpoint, fn:ok, struct:JoinRoomEndpoint, fn:ok,
  struct:RoomGetThreadSubscriptionEndpoint, fn:ok, fn:match_room_id, fn:match_thread_id,
  struct:RoomPutThreadSubscriptionEndpoint, fn:ok, fn:conflicting_unsubscription, fn:match_room_id, fn:match_thread_id,
  fn:match_automatic_event_id, struct:RoomDeleteThreadSubscriptionEndpoint, fn:ok, fn:match_room_id, fn:match_thread_id,
  struct:EnablePushRuleEndpoint, fn:ok, struct:SetPushRulesActionsEndpoint, fn:ok, struct:SetPushRulesEndpoint, fn:ok,
  struct:DeletePushRulesEndpoint, fn:ok, struct:FederationVersionEndpoint, fn:ok, fn:ok_empty,
  struct:GetThreadSubscriptionsEndpoint, fn:add_subscription, fn:add_unsubscription, fn:with_delay, fn:match_from,
  fn:match_to, fn:ok, struct:GetHierarchyEndpoint, fn:ok_with_room_ids, fn:ok_with_room_ids_and_children_state, fn:ok,
  struct:SetSpaceChildEndpoint, fn:ok, fn:unauthorized, struct:SetSpaceParentEndpoint, fn:ok, fn:unauthorized,
  struct:SlidingSyncEndpoint, fn:ok, fn:ok_and_run, struct:GetProfileFieldEndpoint, fn:ok_with_value,
  struct:SetProfileFieldEndpoint, fn:ok, fn:expect_field_value, struct:DeleteProfileFieldEndpoint, fn:ok,
  struct:GetProfileEndpoint, fn:ok_with_fields, struct:GetHomeserverCapabilitiesEndpoint, fn:ok_with_capabilities
- `crates/matrix-sdk/src/test_utils/mocks/oauth.rs`: struct:OAuthMockServer, fn:server_metadata, fn:mock_server_metadata,
  fn:mock_registration, fn:mock_device_authorization, fn:mock_token, fn:mock_revocation, struct:ServerMetadataEndpoint,
  fn:with_delay, fn:ok, fn:ok_https, fn:ok_without_device_authorization, fn:ok_without_registration,
  struct:MockServerMetadataBuilder, fn:new, fn:build, struct:RegistrationEndpoint, fn:ok,
  struct:DeviceAuthorizationEndpoint, fn:ok, struct:TokenEndpoint, fn:ok, fn:ok_with_tokens, fn:access_denied,
  fn:expired_token, fn:invalid_grant, struct:RevocationEndpoint, fn:ok
- `crates/matrix-sdk/src/test_utils/mod.rs`: mod:client, mod:mocks, fn:assert_event_matches_msg, fn:test_client_builder,
  fn:no_retry_test_client, fn:set_client_session, fn:logged_in_client, fn:test_client_builder_with_server,
  fn:no_retry_test_client_with_server, fn:logged_in_client_with_server
- `crates/matrix-sdk/src/utils/local_server.rs`: struct:LocalServerBuilder, fn:new, fn:ip_address, fn:port_range,
  fn:bind_tries, fn:response, fn:spawn, struct:LocalServerRedirectHandle, fn:shutdown_handle,
  struct:LocalServerShutdownHandle, fn:shutdown, enum:LocalServerIpAddress, enum:LocalServerResponse, struct:QueryString
- `crates/matrix-sdk/src/utils/mod.rs`: mod:local_server, trait:IntoRawMessageLikeEventContent,
  trait:IntoRawStateEventContent, fn:is_room_alias_format_valid, fn:formatted_body_from, enum:UrlOrQuery, fn:query
- `crates/matrix-sdk/src/widget/capabilities.rs`: trait:CapabilitiesProvider, struct:Capabilities
- `crates/matrix-sdk/src/widget/filter.rs`: enum:Filter, enum:MessageLikeEventFilter, enum:StateEventFilter,
  struct:ToDeviceEventFilter, fn:new, enum:FilterInput, fn:message_like, fn:state, struct:FilterInputState,
  struct:FilterInputMessageLike, struct:FilterInputToDevice
- `crates/matrix-sdk/src/widget/machine/driver_req.rs`: (no direct public declarations)
- `crates/matrix-sdk/src/widget/machine/from_widget.rs`: (no direct public declarations)
- `crates/matrix-sdk/src/widget/machine/incoming.rs`: (no direct public declarations)
- `crates/matrix-sdk/src/widget/machine/mod.rs`: (no direct public declarations)
- `crates/matrix-sdk/src/widget/machine/openid.rs`: (no direct public declarations)
- `crates/matrix-sdk/src/widget/machine/pending.rs`: (no direct public declarations)
- `crates/matrix-sdk/src/widget/machine/tests/api_versions.rs`: (no direct public declarations)
- `crates/matrix-sdk/src/widget/machine/tests/capabilities.rs`: (no direct public declarations)
- `crates/matrix-sdk/src/widget/machine/tests/error.rs`: (no direct public declarations)
- `crates/matrix-sdk/src/widget/machine/tests/mod.rs`: (no direct public declarations)
- `crates/matrix-sdk/src/widget/machine/tests/openid.rs`: (no direct public declarations)
- `crates/matrix-sdk/src/widget/machine/tests/send_event.rs`: (no direct public declarations)
- `crates/matrix-sdk/src/widget/machine/to_widget.rs`: (no direct public declarations)
- `crates/matrix-sdk/src/widget/matrix.rs`: (no direct public declarations)
- `crates/matrix-sdk/src/widget/mod.rs`: use:self::{, struct:WidgetDriver, struct:WidgetDriverHandle, fn:recv, fn:send,
  fn:new, fn:run
- `crates/matrix-sdk/src/widget/settings/element_call.rs`: enum:EncryptionSystem, enum:Intent, enum:HeaderStyle,
  enum:NotificationType, struct:VirtualElementCallWidgetConfig, struct:VirtualElementCallWidgetProperties,
  fn:new_virtual_element_call_widget
- `crates/matrix-sdk/src/widget/settings/mod.rs`: use:self::element_call::{, struct:WidgetSettings, fn:new, fn:widget_id,
  fn:init_on_content_load, fn:raw_url, fn:base_url, fn:generate_webview_url, struct:ClientProperties, fn:new
- `crates/matrix-sdk/src/widget/settings/url_params.rs`: const:USER_ID, const:ROOM_ID, const:WIDGET_ID, const:AVATAR_URL,
  const:DISPLAY_NAME, const:LANGUAGE, const:CLIENT_THEME, const:CLIENT_ID, const:DEVICE_ID, const:HOMESERVER_URL,
  struct:QueryProperties, fn:replace_properties

## OCaml public interfaces

Only tracked `lib/**/*.mli` files are included; declaration names are extracted mechanically.
This does not expand functors/includes or interpret generated interfaces.

- Interface files: `194`

- `lib/matrix_bot/args.mli`: val:parse, val:argv, val:find_word, val:find_int, val:find_user
- `lib/matrix_bot/bot.mli`: type:t, type:spec, type:plugin, type:handler, val:v, val:on, val:on_message, val:on_edit,
  val:on_reaction, val:on_membership, val:on_room_state, val:on_custom, val:on_invite, val:on_join, val:on_leave,
  val:on_sync, val:command, val:help, val:on_unknown_command, val:only, val:in_rooms, val:from_users, val:on_error,
  type:command_info, val:commands, val:run, exception:from, val:stop, val:context, val:user_id, val:plugin_store,
  val:spec, val:runtime, val:find_room, val:rooms, val:is_admin
- `lib/matrix_bot/context.mli`: type:clock, type:t, val:v, type:error, val:pp_error, val:error_to_string, val:connect,
  val:save, val:env, val:switch, val:clock, val:client, val:user_id, val:encryption, val:event_store, val:plugin_store,
  val:profile_dir
- `lib/matrix_bot/event.mli`: type:envelope, type:message, type:command, type:edit, type:sticker, type:poll,
  type:reaction, type:redaction, type:membership, type:profile, type:room_state, type:custom, type:invitation, type:t,
  val:envelope, val:room, val:room_id, val:sender, val:pp, val:reply, val:react
- `lib/matrix_bot/logging.mli`: val:src
- `lib/matrix_bot/main.mli`: type:mode, val:run_mode, val:run, val:run_once, val:run_once_with_context, val:plugin_flag,
  val:compose
- `lib/matrix_bot/matrix_bot.mli`: module:Logging, module:Plugin_store, module:Context, module:Sent, module:Room,
  module:Event, module:Args, module:Bot, module:Main
- `lib/matrix_bot/plugin_store.mli`: type:error, val:pp_error, val:error_to_string, type:t, val:memory, val:open_file,
  val:find, val:set, val:update, val:remove, val:keys
- `lib/matrix_bot/room.mli`: type:t, val:id, val:name, val:topic, val:encrypted, val:is_dm, val:members,
  val:sync_members, val:ready_to_send, val:await_ready_to_send, val:info, val:timeline, val:send_text, val:send_notice,
  val:send_emote, val:react, val:redact, val:set_topic, val:set_name, val:invite, val:kick, val:ban, val:leave,
  val:power_level, val:backfill, module:Internal, val:v
- `lib/matrix_bot/sent.mli`: type:outcome, type:status, type:t, val:status, val:await, val:request, val:cancel,
  module:Internal, type:tracker, val:tracker, val:v
- `lib/matrix_cli/matrix_cli.mli`: type:http_policy, val:http_policy_default, val:http_policy_term, type:http_options,
  val:http_options, val:homeserver_term, val:homeserver_opt_term, val:profile_term, val:username_term,
  val:username_opt_term, val:password_env_var, val:password_opt_term, val:password_term, type:login_credentials,
  val:login_credentials_term, val:room_term, val:room_opt_term, val:recipient_term, val:recipient_opt_term,
  val:message_term, val:message_opt_term, val:encrypted_term, val:verbosity_term, val:user_id_conv, val:room_id_conv,
  val:uri_conv, val:exit_ok, val:exit_usage, val:exit_auth, val:exit_network, val:exit_internal
- `lib/matrix_client/account.mli`: type:medium, val:medium_to_string, val:medium_of_string, type:threepid,
  val:get_threepids, val:request_email_token, val:request_msisdn_token, val:add_threepid, val:delete_threepid,
  val:change_password, val:deactivate, val:get_ignored_users, val:ignore_user, val:unignore_user
- `lib/matrix_client/account_data.mli`: val:get, type:the, val:set, val:get_room, val:set_room, val:set_marked_unread,
  val:find_dm_rooms, val:mark_as_dm, val:unmark_as_dm, val:mark_room_as_dm, val:unmark_room_as_dm, val:get_or_create_dm
- `lib/matrix_client/admin.mli`: type:connection, type:connection_info, type:session, type:session_info, type:device,
  type:device_info, type:response, val:connection_jsont, val:session_jsont, val:device_jsont, val:response_jsont,
  val:whois, val:get_user_info
- `lib/matrix_client/auth.mli`: type:login_flow, val:get_login_flows, type:login_params, val:default_login_params,
  type:login, val:login_password_with_expiry, val:login_password, val:login_token_with_expiry, val:login_token,
  type:refreshed, type:refreshed_with_expiry, val:refresh_token, val:refresh_token_with_expiry, val:logout,
  val:logout_all, type:registration_kind, val:register, val:register_uiaa, val:register_available,
  val:check_registration_token, val:whoami, type:token_login, val:get_login_token
- `lib/matrix_client/backup.mli`: type:error, val:backup_algorithm, type:encryption_key, module:Decryption_key, type:t,
  val:generate, val:of_bytes, val:of_base64, val:to_base64, val:public, module:Recovery_key, val:encode, val:decode,
  type:encrypted_session_data, val:encrypted_session_data_jsont, type:key_backup_data, val:key_backup_data_jsont,
  type:sessions, type:rooms, type:backed_up_session_data, val:backed_up_session_data_jsont, val:encrypt_session_data,
  val:encrypt_room_key, val:decrypt_room_key, type:recovered_room_key, val:parse_recovered_key, type:megolm_v1_auth_data,
  val:megolm_v1_auth_data_jsont, val:auth_data_to_json, val:sign_auth_data, type:signature_state,
  val:verify_auth_data_signature, type:version_state, val:version_state, type:current_version_state,
  val:current_version_state
- `lib/matrix_client/base58.mli`: type:error, val:alphabet, val:encode, val:decode, val:encode_key, val:decode_key
- `lib/matrix_client/base_client.mli`: type:membership, type:room_info, val:display_name, type:state,
  type:profile_change, val:create, val:of_store, val:persist, val:migrate_legacy_sliding_state, val:with_ruleset,
  val:with_push_rules, val:with_display_name, val:user_id, val:next_batch, val:sliding_pos, val:sliding_to_device_since,
  val:sliding_lists, val:reset_sliding_session, val:ruleset, val:rooms, val:find_room, val:forget_room,
  val:remove_direct_room, val:inviter, val:rooms_with, val:find_account_data, val:all_account_data, val:profiles,
  val:find_profile, val:find_profile_field, val:apply_profile_updates, val:receipts, val:with_local_unread_counts,
  val:presence, val:members, val:replace_members, val:human_members, val:service_members, val:human_member_count,
  val:state_events, val:find_state_event, val:retention, val:push_context, type:state_coverage,
  val:unknown_state_coverage, val:complete_state_coverage, type:decrypted, type:room_change, type:changes, val:apply,
  val:apply_sliding, module:Hooks, type:t, val:create, val:on_response, val:on_sliding_response, val:on_room_event,
  val:run, val:run_sliding, val:compute_display_name
- `lib/matrix_client/calls.mli`: type:call_id, type:party_id, val:call_id_of_string, val:party_id_of_string,
  val:generate_call_id, val:generate_party_id, val:send_invite, val:send_candidates, val:send_answer, val:send_hangup,
  val:send_reject, type:turn_server, val:turn_server_jsont, val:get_turn_server
- `lib/matrix_client/client.mli`: type:well_known_policy, type:config, val:config, type:session, type:refreshed_tokens,
  type:refreshed_tokens_with_expiry, module:Server_metadata_cache, type:t, val:get, val:set, val:clear,
  val:get_oauth_metadata, val:set_oauth_metadata, val:get_oauth_metadata_with_expiry, val:set_oauth_metadata_with_expiry,
  val:invalidate_oauth_metadata, type:t, type:sync_presence, val:create, val:with_session, val:without_session,
  val:with_auto_refresh, val:with_auto_refresh_expiry, val:with_access_token, val:session, val:sync_presence,
  val:set_sync_presence, val:register_presence_wakeup, val:homeserver, val:well_known_policy, val:random,
  val:server_metadata_cache, module:Http, val:get, val:get_absolute, val:post, val:post_absolute, val:put,
  val:put_absolute, val:delete, val:delete_absolute, val:post_unauthenticated, val:get_bytes,
  val:get_bytes_with_cache_control, val:get_bytes_unauthenticated, type:raw_response, val:request_unauthenticated,
  val:get_stream, val:get_stream_unauthenticated, val:post_empty, val:post_bytes, val:put_bytes, val:post_stream,
  val:decode_response, val:encode_body
- `lib/matrix_client/composer_draft.mli`: type:draft_type, type:thumbnail, type:attachment_content, type:attachment,
  type:t, val:save, val:load, val:clear
- `lib/matrix_client/cross_signing.mli`: type:local_trust, type:own_identity_state, type:role, type:key, val:key,
  val:role, val:published, val:key_user_id, val:key_ed25519, val:verify_key, type:private_identity,
  val:create_private_identity, val:generate_private_keys, val:identity_user_id, val:master_secret,
  val:self_signing_secret, val:user_signing_secret, val:set_user_signing_secret, val:master_public,
  type:private_identity_import_error, val:pp_private_identity_import_error, val:private_identity_of_secrets,
  val:private_identity_of_secrets_unchecked, val:sign_cross_signing_key, type:upload, val:build_upload, type:device,
  val:create_device, val:device_keys, val:device_id, val:device_user_id, val:device_algorithms, val:device_public_keys,
  val:device_signatures, val:device_ed25519, val:device_local_trust, val:set_device_local_trust,
  val:device_cross_signing_trusted, val:is_device_verified, val:verify_device_signature, val:update_device_trust,
  val:sign_device, val:sign_device_keys, type:own_identity, val:own_identity, val:own_user_id, val:own_master_key,
  val:own_self_signing_key, val:own_user_signing_key, val:own_identity_state, val:is_own_identity_verified,
  type:other_identity, val:other_identity, val:other_user_id, val:other_master_key, val:other_self_signing_key,
  val:is_other_identity_verified, val:pin_master_key, val:pinned_master_key, val:has_identity_changed,
  val:verify_master_trust, val:verify_device_trust_chain
- `lib/matrix_client/crypto_key.mli`: module:only, type:error, module:Signature, type:t, val:of_bytes, val:to_bytes,
  val:of_base64, val:to_base64, val:equal, val:compare, val:pp, val:jsont, module:Ed25519, module:Public, type:t,
  val:of_bytes, val:to_bytes, val:of_base64, val:to_base64, val:verify, val:equal, val:compare, val:pp, val:jsont,
  module:Private, type:t, val:of_bytes, val:of_expanded_bytes, val:of_stored_bytes, val:to_bytes, val:to_expanded_bytes,
  val:public, val:sign, val:generate, module:Curve25519, module:Public, type:t, val:of_bytes, val:to_bytes,
  val:of_base64, val:to_base64, val:equal, val:compare, val:pp, val:jsont, module:Secret, type:t, val:of_bytes,
  val:to_bytes, val:public, val:generate, val:key_exchange, module:Key_id, type:t, val:v, val:of_device, val:of_string,
  val:to_string, val:algorithm, val:id, val:equal, val:compare, val:pp, val:jsont
- `lib/matrix_client/crypto_store.mli`: type:snapshot, type:t, val:create, val:exists, val:load, val:save, val:clear
- `lib/matrix_client/dehydrated_device.mli`: type:t, val:pickle_key_secret_name, module:Pickle_key, type:t, type:error,
  val:generate, val:of_base64, val:to_base64, val:is_key_stored, val:load_key, val:cached_key, val:load_key_with_driver,
  val:reset_key, val:reset_key_with_driver, val:is_supported, val:get, val:get_if_present, val:put, val:put_and_remember,
  val:create_and_upload, type:create_event, val:create_and_upload_with_callbacks, val:delete, val:delete_if_present,
  type:rehydrate_outcome, val:rehydrate, type:rehydrate_event, val:rehydrate_with_callbacks, type:events, val:get_events
- `lib/matrix_client/delayed_events.mli`: val:unstable_prefix, type:delay_id, val:delay_id_of_string,
  val:delay_id_to_string, val:send, val:send_state, type:action, val:update, val:send_now, val:cancel, val:restart,
  type:delayed_event, val:list
- `lib/matrix_client/devices.mli`: type:device, val:get_devices, val:get_device, val:update_device, val:delete_device,
  val:delete_devices
- `lib/matrix_client/directory.mli`: type:room_id_or_alias, val:room_id_or_alias_to_string, type:alias_info,
  val:resolve_alias, val:create_alias, val:delete_alias, val:get_visibility, val:set_visibility, type:space_child,
  type:room_summary, val:room_summary_jsont, val:get_summary, type:search_filter, type:published_rooms,
  val:get_public_rooms, val:search_public_rooms
- `lib/matrix_client/encrypted_attachment.mli`: type:error, val:pp_error, type:metadata, module:Metadata, type:t,
  val:make, val:key, val:iv, val:hash, val:version, val:algorithm, val:key_ops, val:ext, val:jsont, val:of_json,
  val:to_json, val:of_json_string, val:to_json_string, val:of_event_file, val:to_event_file, module:Encryptor, type:t,
  val:create, val:feed, val:finish, module:Decryptor, type:t, val:create, val:feed, val:finish, type:encrypted,
  val:encrypt, val:decrypt, val:decrypt_chunks, val:decrypt_verified
- `lib/matrix_client/encryption.mli`: type:trust, type:utd_cause, type:withheld, type:pending_key_bundle,
  type:room_key_bundle_content, type:received_key_bundle, val:room_key_bundle_content_jsont, type:outbound_withheld,
  type:secret_cancel, type:secret_request, type:secret_send, type:utd_context, type:trust_requirement,
  type:identity_status, type:identity_change, type:device, val:device_key, val:device_ed25519, val:device_curve25519,
  type:room_settings, val:default_room_settings, val:enable_room_encryption, val:room_encryption_content, type:t,
  val:create, val:create_with_account, val:user_id, val:device_id, val:dehydrated_pickle_key,
  val:set_dehydrated_pickle_key, val:last_uploaded_device_id, val:set_last_uploaded_device_id, val:identity_keys,
  val:sign, val:device_keys_for_upload, val:track_users, val:untrack_users, val:tracked_users, val:outdated_users,
  val:devices_of, val:find_device, val:find_device_by_curve25519, val:set_device_trust, val:set_trust_requirement,
  val:trust_requirement, val:store_secret, val:secret, val:request_secret, val:cancel_secret_request,
  val:trust_user_identity, val:reset_cross_signing, val:acknowledge_user_identity, val:identity_has_pin_violation,
  val:pin_user_identity, val:identity_status, val:identity_master_key, val:identity_self_signing_key,
  val:identity_user_signing_key, val:identity_changes, val:receive_keys_query, val:receive_keys_claim,
  val:receive_keys_upload, val:withheld_for, val:record_invite_acceptance, val:pending_key_bundle,
  val:pending_key_bundles, val:received_key_bundles, val:clear_received_key_bundle,
  val:room_key_bundle_sender_is_trusted, val:clear_pending_key_bundle, val:should_accept_room_key_bundle,
  val:accept_room_key_bundle, val:clear_expired_pending_key_bundles, val:set_room_encryption_settings,
  val:find_room_settings, val:is_room_encrypted, type:request, val:pp_request, val:outgoing_requests, val:mark_sent,
  val:share_room_key_bundle, type:verification_state, type:to_device_event, val:pp_to_device_event, type:outcome,
  val:process_sync, val:process_sliding_sync, type:decrypted_event, type:decrypt_error, val:default_utd_context,
  val:classify_utd, val:utd_context, val:pp_decrypt_error, val:decrypt_room_event, val:request_room_key,
  val:ensure_sessions, val:encrypt_room_event, val:enable_backup, val:disable_backup, val:backup_version,
  val:backup_decryption_enabled, val:room_key_backup_is_fully_downloaded, val:mark_room_key_backup_fully_downloaded,
  val:clear_room_key_backup_fully_downloaded, val:backup_pending_count, val:pending_backup, val:import_backup,
  val:export_room_keys, type:room_key_import_result, val:import_room_keys, val:build_room_key_bundle,
  val:import_room_key_bundle, val:inbound_sessions, val:has_inbound_session, val:outbound_session_id,
  val:outbound_message_count, module:Session_meta, type:t, type:backup_state, type:identity, type:state, val:empty_state,
  type:snapshot, val:snapshot, val:of_snapshot
- `lib/matrix_client/encryption_driver.mli`: type:t, val:v, val:create, val:create_with_account, val:machine, val:store,
  val:save, val:execute_requests, type:room_key_bundle_outcome, type:share_room_history_outcome,
  type:share_room_history_error, type:invite_outcome, type:invite_error, val:accept_received_room_key_bundle,
  val:sync_hook, val:sync_hook_sliding, val:encrypt_room_event, val:send_encrypted, val:backup_pending,
  val:restore_from_backup, val:restore_room_from_backup, val:restore_session_from_backup, val:share_room_history,
  val:invite_user_by_id
- `lib/matrix_client/error.mli`: type:errcode, val:errcode_to_string, val:errcode_of_string, type:matrix_error,
  val:matrix_error_jsont, type:t, val:errcode, val:equal, val:pp, val:to_string
- `lib/matrix_client/json_codec.mli`: val:obj, val:merge_extra_content, val:string_map, val:uri, val:keyed_map,
  val:ptime
- `lib/matrix_client/keys.mli`: type:signatures, type:device_keys, val:device_keys_jsont, type:one_time_key,
  val:one_time_key_jsont, val:one_time_key_signing_json, type:key_usage, val:key_usage_to_string,
  val:key_usage_of_string, type:cross_signing_key, val:signatures_jsont, val:key_id_map, val:cross_signing_key_jsont,
  type:failures, type:upload_keys_response, val:upload_keys, type:query_keys_response, val:query_keys,
  type:claim_keys_response, val:claim_keys, type:key_changes_response, val:get_key_changes, val:upload_signing_keys,
  val:upload_signing_keys_uiaa, type:upload_signatures_response, val:upload_signatures
- `lib/matrix_client/knock_requests.mli`: type:t, val:list, val:all, val:mark_seen, val:accept, val:decline,
  val:decline_and_ban
- `lib/matrix_client/matrix_client.mli`: module:Error, module:Random, module:Client, module:Notification_settings,
  module:Thread_subscriptions, module:Auth, module:Uiaa, module:Oauth, module:Server, module:Directory,
  module:Room_preview, module:Knock_requests, module:Spaces, module:Space_graph, module:Thirdparty, module:Rooms,
  module:Room, module:Room_details, module:Messages, module:Paginator, module:Thread_paginator, module:Media,
  module:Media_fetcher, module:Media_store, module:Encrypted_attachment, module:Attachment, module:State,
  module:Retention, module:Relations, module:Typing, module:Receipts, module:Tags, module:Search, module:Report,
  module:Calls, module:Delayed_events, module:Account, module:Account_data, module:Profile, module:Presence,
  module:Devices, module:Admin, module:Openid, module:Sync, module:Sliding_sync, module:Base_client, module:Push,
  module:Push_evaluator, module:Notifications, module:Read_state, module:Crypto_key, module:Keys, module:Olm,
  module:To_device, module:Verification, module:Cross_signing, module:Encryption, module:Encryption_driver,
  module:Crypto_store, module:Backup, module:Room_key_export, module:Room_keys, module:Secret_storage, module:Secrets,
  module:Recovery, module:Dehydrated_device, module:Olm_dehydrated_pickle, module:Qr_login, module:Store,
  module:Composer_draft, module:Send_queue, module:Timeline, module:Session, module:Session_pickle, module:Profile_store,
  module:Olm_error, module:Olm_account, module:Olm_session, module:Megolm, module:Olm_machine, module:Verification_base,
  module:Verification_sas, module:Verification_qr, module:Verification_flow
- `lib/matrix_client/media.mli`: module:Mxc, type:t, val:of_string, val:to_string, val:server_name, val:media_id,
  val:equal, val:pp, val:jsont, val:mxc_option_jsont, val:upload, val:upload_stream, type:preallocated,
  val:create_content_uri, type:preallocated_upload_error, val:pp_preallocated_upload_error, val:upload_preallocated,
  type:encrypted_file, type:source, type:format, type:request, type:encrypted_error, val:pp_encrypted_error,
  val:upload_encrypted, val:upload_encrypted_stream, type:content, val:download, val:download_encrypted,
  val:download_encrypted_stream, val:thumbnail, val:mxc_to_http, val:mxc_to_http_unauthenticated,
  val:mxc_to_http_resolved, type:preview, val:get_url_preview, type:config, val:get_config
- `lib/matrix_client/media_fetcher.mli`: type:t, val:create, val:default, val:get_content
- `lib/matrix_client/media_store.mli`: type:format, type:key, type:retention_policy, module-type:S, type:t,
  val:retention, val:set_retention, val:add, val:get, val:protect, val:unprotect, val:is_protected,
  val:set_ignore_retention, val:replace_key, val:remove, val:remove_uri, val:prune_local, val:clean, val:last_cleanup,
  val:set_last_cleanup, val:close, type:t, val:v, val:memory, val:create, val:retention, val:last_cleanup,
  val:set_last_cleanup, val:set_retention, val:local_uri, val:is_local_uri, val:derived_key, val:add, val:get,
  val:protect, val:unprotect, val:is_protected, val:set_ignore_retention, val:replace_key, val:remove, val:remove_uri,
  val:prune_local, val:clean, val:close
- `lib/matrix_client/megolm.mli`: type:decrypted, type:encrypted, module:Inbound, type:t, val:of_session_key,
  val:of_exported_session_key, val:from_room_key, val:session_id, val:sender_key, val:room_id, val:signing_key,
  val:sender_claimed_ed25519_key, val:signing_key_verified, val:first_known_index, val:creation_time, val:decrypt,
  val:export_at, val:export_at_first_known_index, type:pickle, val:to_pickle, val:of_pickle, module:Outbound, type:t,
  val:create, val:session_id, val:room_id, val:signing_key, val:message_index, val:message_count, val:creation_time,
  val:rotation_period, val:rotation_messages, val:needs_rotation, val:session_key, val:exported_session_key, val:encrypt,
  val:mark_shared_with, val:is_shared_with, val:shared_with, type:pickle, val:to_pickle, val:of_pickle
- `lib/matrix_client/messages.mli`: type:send_response, val:send_response_jsont, val:send_event, val:send_text,
  val:send_emote, val:send_notice, val:send_image, val:send_file, val:redact, type:messages_response, val:get_messages,
  val:get_event, type:context, val:get_context
- `lib/matrix_client/notification_settings.mli`: type:room_notification_mode, type:t, val:create, val:client,
  val:ruleset, val:refresh, val:user_defined_room_mode, val:default_room_mode, val:room_mode,
  val:rooms_with_user_defined_rules, val:set_room_mode, val:delete_room_mode, val:unmute_room, val:set_default_room_mode,
  val:contains_keyword_rules, val:enabled_keywords, val:add_keyword, val:remove_keyword, val:is_enabled, val:set_enabled,
  val:set_actions, type:subscription, val:subscribe, val:unsubscribe
- `lib/matrix_client/notifications.mli`: type:notification, type:notifications, val:get
- `lib/matrix_client/oauth.mli`: type:oauth_error, val:pp_oauth_error, val:oauth_error_of_json, val:oauth_error_of_error,
  val:scope_api, val:scope_device_prefix, val:scope_api_unstable, val:scope_device_prefix_unstable, val:scope_device,
  val:scope_device_unstable, val:device_id_of_scope, val:generate_device_id, type:account_action,
  val:account_action_to_string, val:account_action_of_string, module:Metadata, type:t, val:jsont, val:v1_path,
  val:unstable_path, val:openid_configuration_path, val:fetch, val:fetch_cached, val:invalidate_cache, val:validate,
  val:validate_device, val:supports_response_type, val:supports_grant_type, val:supports_response_mode,
  val:supports_code_challenge_method, val:supports_prompt, val:supports_account_action, val:account_management_url,
  module:Registration, type:localized, val:plain, type:client_metadata, val:v, val:loopback_redirect_uri, val:to_json,
  type:response, val:register, module:Pkce, type:t, val:challenge_method, val:create, val:of_verifier,
  module:Authorization, val:build_url, type:request, val:request, type:redirect, type:redirect_error,
  val:pp_redirect_error, val:parse_redirect, module:Token, type:t, val:jsont, val:is_expired, val:exchange, val:refresh,
  type:token_type_hint, val:revoke, val:logout, val:finish_login, module:Device_authorization, type:t, val:jsont,
  type:poll_error, val:request, val:poll
- `lib/matrix_client/olm.mli`: type:error, val:pp_error, module:Account, module:Session, module:Megolm, module:Machine
- `lib/matrix_client/olm_account.mli`: type:t, val:create, val:one_time_key_algorithm, val:ed25519_key,
  val:curve25519_key, val:identity_keys, val:sign, val:generate_one_time_keys, val:one_time_keys, val:one_time_key_ids,
  val:signed_one_time_keys, val:one_time_keys_count, val:max_one_time_keys, val:generate_fallback_key, val:fallback_key,
  val:forget_previous_fallback_key, val:identity_exchange, val:one_time_key_exchange, val:consume_one_time_key,
  type:stored_key, type:pickle, val:to_pickle, val:of_pickle
- `lib/matrix_client/olm_dehydrated_pickle.mli`: type:error, type:decoded, val:pickle, val:unpickle
- `lib/matrix_client/olm_error.mli`: type:t, val:pp
- `lib/matrix_client/olm_machine.mli`: type:t, val:create, val:of_account, val:account, val:find_olm_session,
  val:olm_sessions, val:store_olm_session, val:create_olm_session, val:create_inbound_session, val:encrypt_to_device,
  val:decrypt_to_device
- `lib/matrix_client/olm_primitives.mli`: val:ct_equal, val:version, val:default_salt, val:sha256, val:hmac_sha256,
  val:hkdf, val:now, val:base64_decode, val:base64_encode, module:Varint, val:encode, val:decode, module:Pb, type:value,
  val:parse, val:bytes, val:varint, val:tag_bytes, val:tag_varint, module:Cipher, type:t, val:olm, val:megolm,
  val:of_expanded, val:encrypt, val:decrypt, val:mac8, val:verify_mac8
- `lib/matrix_client/olm_session.mli`: type:t, type:message_type, type:message, val:create_outbound, val:create_inbound,
  val:session_id, val:their_identity_key, val:creation_time, val:last_used_at, val:last_received_at,
  val:has_received_message, val:encrypt, val:decrypt, type:chain_key, type:active_chain, type:sending,
  type:receiver_chain, type:pickle, val:to_pickle, val:of_pickle
- `lib/matrix_client/openid.mli`: type:token, val:request_token, val:request_own_token
- `lib/matrix_client/paginator.mli`: type:state, type:pagination_token, type:tokens, type:thread_mode, type:error,
  val:pp_error, type:start_result, type:page, type:t, val:create, val:state, val:tokens, val:thread_root, val:subscribe,
  val:start_from, val:paginate_backward, val:paginate_forward, val:reset
- `lib/matrix_client/presence.mli`: type:presence_state, val:presence_state_to_string, val:presence_state_of_string,
  val:presence_state_jsont, type:presence, val:get_presence, val:set_presence
- `lib/matrix_client/profile.mli`: type:profile, val:get_profile, val:get_displayname, val:set_displayname,
  val:clear_displayname, val:get_avatar_url, val:set_avatar_url, val:clear_avatar_url, val:find_field, val:set_field,
  val:delete_field
- `lib/matrix_client/profile_store.mli`: type:t, val:create, val:create_at, val:dir, val:exists, val:with_lock,
  val:with_dir_lock, val:load_session, val:save_session, val:update_session, val:load_device_keys, val:save_device_keys,
  val:load_one_time_keys, val:save_one_time_keys, val:load_olm_sessions, val:save_olm_sessions, val:load_megolm_inbound,
  val:save_megolm_inbound, val:load_megolm_outbound, val:save_megolm_outbound, val:clear
- `lib/matrix_client/push.mli`: val:get_push_rules, val:get_push_rule, val:delete_push_rule, val:set_push_rule,
  val:set_enabled, val:set_actions, type:pusher_kind, type:pusher_data, type:pusher, val:get_pushers, val:set_pusher,
  val:delete_pusher
- `lib/matrix_client/push_evaluator.mli`: module:Power_levels, type:t, val:default, val:of_json, module:Context, type:t,
  val:v, val:user_id, val:room_id, val:display_name, val:member_count, val:power_levels, val:find_matching_rule,
  val:evaluate, type:notification, val:no_notification, val:notification_of_actions, val:notification_for_event
- `lib/matrix_client/qr_login.mli`: type:intent, type:t, type:codec_error, val:pp_codec_error, val:make, val:of_bytes,
  val:to_bytes, val:of_base64, val:to_base64, val:rendezvous_path, val:rendezvous_server_supported,
  type:secure_channel_error, val:establish_secure_channel, module:Msc4108, type:intent, type:code, type:t,
  type:codec_error, val:pp_codec_error, val:make, val:of_bytes, val:to_bytes, val:of_base64, val:to_base64,
  val:rendezvous_path, module:Messages, type:login_protocol, type:protocol, type:login_failure_reason,
  type:failure_reason, type:authorization_grant, type:grant, type:login_protocols, type:login_protocol_message,
  type:cross_signing_secrets, type:backup_secrets, type:backup_secret, type:secrets_bundle, type:secret_bundle, type:t,
  val:login_protocol_of_string, val:login_protocol_to_string, val:login_failure_reason_of_string,
  val:login_failure_reason_to_string, val:authorization_grant_login_protocol, val:jsont, val:message_jsont,
  val:auth_message_jsont, val:of_json, val:to_json, val:of_string, val:to_string, module:Auth_message,
  module:Qr_auth_message, module:Secrets, type:backup, type:imported, type:error, val:pp_error, val:export, val:import,
  module:Rendezvous, module:Ecies, module:Secure_channel, type:error, val:pp_error, type:displayed,
  type:almost_established, type:established, val:create, val:login, val:reciprocate, val:qr_code, val:qr_code_base64,
  val:connect, val:check_code, val:confirm, val:cancel_displayed, val:cancel_almost, val:from_qr_code,
  val:check_code_established, val:send, val:receive, val:send_json, val:receive_json, val:send_message,
  val:receive_message, val:close, module:Application, type:'e, val:secure_channel, type:login_start, type:token_failure,
  type:'token, type:login_progress, type:'token, type:grant_start, type:grant_decision, type:grant_progress,
  type:grant_hooks, type:'e, val:pp_error, val:run_login, val:run_grant
- `lib/matrix_client/qr_login_ecies.mli`: type:error, val:pp_error, type:pending, type:t, val:create, val:public_key,
  val:establish_outbound, val:establish_inbound, val:encrypt, val:decrypt, val:check_code_bytes, val:check_code
- `lib/matrix_client/qr_login_rendezvous.mli`: type:method_, type:response, type:transport, type:error, type:t,
  val:transport_of_client, val:create, val:accept, val:rendezvous_url, val:status, val:send, val:receive, val:close
- `lib/matrix_client/random.mli`: type:t, val:of_source, val:of_env, val:generate, val:txn_id
- `lib/matrix_client/read_state.mli`: type:receipt, type:t, val:empty, val:v, val:public_read, val:private_read,
  val:fully_read, val:thread_ids, val:thread_public_read, val:thread_private_read, val:thread_latest_read,
  val:latest_read, val:jsont, val:ingest_receipt_event, val:ingest_ephemeral, val:ingest_fully_read, type:counts,
  val:zero_counts, val:marks_as_unread, val:is_main_timeline_event, val:latest_read_in_timeline, val:count_unread,
  val:latest_read_in_thread, val:count_unread_in_thread
- `lib/matrix_client/receipts.mli`: type:receipt_type, val:receipt_type_to_string, val:send_receipt, val:set_read_marker
- `lib/matrix_client/recovery.mli`: type:state, type:marker, type:markers, val:key_backup_event_type,
  val:backup_disabled_event_type, val:key_backup_jsont, val:backup_disabled_jsont, val:key_backup_marker,
  val:backup_disabled_marker, type:inputs, val:state, val:check_state, type:account_data_write,
  val:known_secret_event_types, val:mark_enabled_writes, val:disable_writes, val:fetch_markers, val:apply_writes,
  val:mark_backup_enabled, val:disable_account_data, val:disable, val:disable_and_delete_backups, type:recovered,
  val:recover, val:recover_and_fix_backup, type:backup_upload, type:enabled, val:enable, val:reset_key,
  type:recovered_and_reset, val:recover_and_reset, module:Manager, type:t, type:subscription, val:create, val:client,
  val:encryption, val:base_state, val:private_identity, val:set_private_identity, val:state, val:refresh_from_base,
  val:refresh, val:subscribe, val:unsubscribe, val:watch, val:disable, val:disable_and_delete_backups, val:recover,
  val:recover_and_fix_backup, val:enable, val:reset_key, val:recover_and_reset, val:reset_identity,
  val:cancel_pending_identity_reset
- `lib/matrix_client/relations.mli`: val:send_reaction, val:edit_message, val:send_reply, val:send_in_thread,
  type:related_event, val:get_relations, val:get_raw_relations, val:get_edit_revisions, val:get_reactions,
  type:thread_filter, val:list_threads
- `lib/matrix_client/report.mli`: val:event, val:room, val:user
- `lib/matrix_client/retention.mli`: type:policy, type:lifetime_limits, type:limits, type:configuration, val:policy,
  val:policy_min_lifetime, val:policy_max_lifetime, val:policy_jsont, val:lifetime_limits_jsont, val:limits_jsont,
  val:configuration_jsont, val:get_configuration, val:get_room_policy, val:set_room_policy, val:effective_policy,
  val:effective
- `lib/matrix_client/room.mli`: type:t, val:create, val:room_id, val:routing_candidates, val:permalink,
  val:event_permalink, val:direct_targets, val:dm_target, val:set_is_direct, val:mark_as_dm, val:unmark_as_dm, type:role,
  val:suggested_role, type:invite_details, val:invite_details
- `lib/matrix_client/room_details.mli`: type:t, type:member, val:create, val:state, val:room_id, val:members_complete,
  val:members, val:member_count, val:service_member_count, val:human_member_count, val:ensure_members
- `lib/matrix_client/room_key_export.mli`: type:room_key, type:history_not_shared, type:historic_room_key,
  type:room_key_bundle, val:room_key_jsont, val:room_keys_jsont, val:history_not_shared_jsont,
  val:historic_room_key_jsont, val:room_key_bundle_jsont, type:error, val:pp_error, val:encrypt, val:decrypt
- `lib/matrix_client/room_keys.mli`: type:version_info, val:equal_version_info, type:key_backup_data, type:sessions,
  type:rooms, type:update_response, val:create_version, val:get_current_version, val:get_version, val:update_version,
  val:delete_version, val:put_keys, val:get_keys, val:delete_keys, val:put_room_keys, val:get_room_keys,
  val:delete_room_keys, val:put_session_key, val:get_session_key, val:delete_session_key
- `lib/matrix_client/room_preview.mli`: type:t, val:of_room, val:get
- `lib/matrix_client/rooms.mli`: type:preset, val:create, val:join, val:knock, val:leave, val:forget, val:invite,
  val:kick, val:ban, val:unban, val:get_joined_rooms, type:member, val:get_members, type:joined_member,
  val:get_joined_members, val:get_power_levels, val:set_power_levels, val:set_user_power_level,
  val:start_live_location_share, val:stop_live_location_share, val:send_location_beacon, val:upgrade, val:get_aliases,
  type:timestamp_event, val:timestamp_to_event
- `lib/matrix_client/search.mli`: type:key, type:order_by, type:group_by, type:event_context_request, type:criteria,
  val:v, type:user_profile, type:event_context, type:hit, type:group, type:room_events_result, val:room_events,
  type:user, type:user_directory_result, val:user_directory
- `lib/matrix_client/secret_storage.mli`: type:error, val:algorithm, val:pbkdf2_algorithm,
  val:secret_cross_signing_master, val:secret_cross_signing_self_signing, val:secret_cross_signing_user_signing,
  val:secret_megolm_backup_v1, type:key, val:generate_key, val:key_of_bytes, val:to_bytes, module:Recovery_key,
  val:encode, val:decode, module:Passphrase_info, type:t, val:default_bits, val:v, val:jsont, module:Key_description,
  type:t, val:v, val:jsont, val:key_of_passphrase, type:key_check, val:check_key, module:Encrypted, type:t, val:jsont,
  val:encrypt, val:decrypt
- `lib/matrix_client/secrets.mli`: val:default_key_event_type, val:key_event_type, val:get_default_key_id,
  val:set_default_key_id, val:get_key_description, val:put_key_description, val:get_secret, val:get_secret_opt,
  val:store_secret, type:store, type:created_store, val:create_secret_store, val:open_secret_store,
  val:create_recovery_store, val:store_key_id, val:store_user_id, val:store_homeserver, val:get_store_secret,
  type:backup_import_result, type:backup_import_error, val:import_backup_typed, val:put_store_secret,
  val:export_recovery_secrets, val:import_backup, val:import_cross_signing
- `lib/matrix_client/send_queue.mli`: type:kind, type:upload_result, type:dependency_result, type:progress,
  type:attachment_upload, type:attachment_edit_result, type:status, type:request, val:id, val:room_id, val:kind,
  val:txn_id, val:status, val:attempts, val:created_at, val:last_error, type:t, val:create, val:user_id, val:store,
  val:enqueue, val:send_message, val:upload, val:upload_encrypted, val:attachment_upload,
  val:attachment_upload_encrypted, val:send_attachment, val:edit_attachment_caption, val:send_text, val:send_edit,
  val:send_reaction, val:send_redaction, val:requests, val:room_requests, val:dependencies, val:resolved_dependencies,
  val:dependency_results, val:rooms, val:next, val:pending_count, val:is_empty, val:cancel, val:cancel_with_reason,
  val:forget_room, val:unwedge, val:enabled, val:set_enabled, val:room_enabled, val:set_room_enabled, val:on_change,
  val:on_progress, type:outcome, val:send_one, val:classify, val:retry_delay, type:payload, val:payload,
  val:content_for_send, val:send_as, val:local_echo, val:save
- `lib/matrix_client/server.mli`: type:versions, val:get_versions, val:invalidate_cache, val:supports_version,
  val:supports_version_at_least, val:has_unstable_feature, type:room_version_stability, type:room_versions_capability,
  type:capabilities, val:get_capabilities, val:refresh_capabilities, val:find_capability,
  type:account_moderation_capability, type:profile_fields_capability, val:can_change_password,
  val:can_change_thirdparty_ids, val:can_get_login_token, val:room_versions, val:account_moderation,
  val:forgets_room_when_leaving, val:can_change_displayname, val:can_change_avatar, val:extended_profile_fields,
  type:server_info, type:authentication_info, type:well_known, val:get_well_known, type:get_versions_at, type:discovery,
  val:discover
- `lib/matrix_client/session.mli`: module:Server, type:t, val:jsont, module:Auth, type:method_, type:t, val:jsont,
  module:Sync_state, type:t, val:jsont, module:Metadata, type:t, val:jsont, module:Session_file, type:t, val:jsont,
  module:Device_keys, type:t, val:jsont, module:One_time_key, type:t, val:jsont, module:One_time_keys_file, type:t,
  val:jsont, module:Olm_session, type:t, val:jsont, module:Olm_sessions_file, type:t, val:jsont, module:Megolm_inbound,
  type:t, val:jsont, module:Megolm_inbound_file, type:t, val:jsont, module:Shared_with, type:t, val:jsont,
  module:Megolm_outbound, type:t, val:jsont, module:Megolm_outbound_file, type:t, val:jsont
- `lib/matrix_client/session_pickle.mli`: type:error, val:pickle_account, val:unpickle_account, val:pickle_session,
  val:unpickle_session, val:pickle_megolm_inbound, val:unpickle_megolm_inbound, val:pickle_megolm_outbound,
  val:unpickle_megolm_outbound
- `lib/matrix_client/sliding_sync.mli`: val:path, val:default_timeout_ms, val:native_feature, val:is_available_in,
  val:is_available, val:sync_once, val:is_unsupported, val:is_expired_pos
- `lib/matrix_client/sliding_sync_state.mli`: type:t, type:room, type:profile, val:empty, val:load, val:load_opt,
  val:discard, val:save, val:apply, val:pos, val:to_device_since, val:lists, val:profiles, val:find_profile,
  val:find_profile_field, val:rooms_by_recency, val:find_room, val:timeline_capacity, val:room_id, val:name,
  val:avatar_url, val:is_dm, val:is_invite, val:highlight_count, val:notification_count, val:timeline,
  val:required_state, val:find_state, val:prev_batch, val:joined_count, val:invited_count, val:bump_stamp, val:heroes
- `lib/matrix_client/space_graph.mli`: type:t, val:of_state, val:parents, val:children, val:roots, val:flattened_subtree
- `lib/matrix_client/spaces.mli`: type:space_child, val:get_hierarchy, val:add_child, val:remove_child, val:set_parent,
  val:remove_parent, val:is_space, val:create_space
- `lib/matrix_client/state.mli`: val:get_state, val:get_state_event, val:set_state, val:get_name, val:set_name,
  val:get_topic, val:set_topic, val:get_avatar, val:set_avatar
- `lib/matrix_client/store.mli`: type:membership, val:membership_to_string, val:membership_of_string, type:hero,
  type:display_name, val:display_name_to_string, type:state_completeness, type:marked_unread_source, type:state_event,
  type:room_info, module:writes, val:empty_room_info, val:room_info_jsont, val:find_state_event,
  val:state_events_of_type, type:plaintext_policy, type:profile, type:t, type:snapshot, val:memory, val:on_disk,
  val:on_disk_with_policy, val:plaintext_policy, val:dir, val:snapshot, val:restore, val:next_batch, val:set_next_batch,
  val:sliding_pos, val:set_sliding_pos, val:sliding_to_device_since, val:set_sliding_to_device_since, val:sliding_lists,
  val:replace_sliding_session, val:rooms, val:find_room, val:set_room, val:remove_room, val:find_account_data,
  val:set_account_data, val:remove_account_data, val:all_account_data, val:profiles, val:replace_profiles, val:receipts,
  val:set_receipts, val:remove_receipts, val:all_receipts, module:Slot, type:'a, val:v, val:find, val:set, val:remove,
  val:dirty, val:flush, val:clear
- `lib/matrix_client/sync.mli`: type:params, val:default_params, val:sync_once, module:Filter, type:event,
  type:room_event, type:room, type:t, val:default_event, val:default_room_event, val:default_room, val:default,
  val:jsont, val:room_event_jsont, val:create, val:get
- `lib/matrix_client/tags.mli`: val:favourite, val:low_priority, val:server_notice, val:get, val:set, val:remove,
  val:set_favourite, val:set_low_priority
- `lib/matrix_client/thirdparty.mli`: type:field_type, type:protocol_instance, type:protocol, val:protocols,
  val:get_protocol, type:location, val:locations_of_alias, val:locations, type:user, val:users_of_user_id, val:users
- `lib/matrix_client/thread_paginator.mli`: type:state, type:t, val:create, val:set_filter, val:reset, val:close,
  val:state, val:roots, val:loaded_pages, val:is_at_last_page, val:subscribe, val:next_page
- `lib/matrix_client/thread_subscriptions.mli`: type:status, type:subscription, type:unsubscription, type:page,
  type:stored_status, type:stored_subscription, type:update, type:catchup_token, val:is_supported, val:get,
  val:subscribe, val:unsubscribe, val:changes, val:subscriptions, val:find_stored, val:merge, val:upsert,
  val:upsert_many, val:remove, val:remove_room, val:catchup_tokens, val:queue_catchup_token, val:catch_up_once,
  val:catch_up, val:subscribe_and_store, val:unsubscribe_and_store, val:get_and_store, val:load_or_fetch,
  val:subscribe_if_needed, val:apply_sliding_extension
- `lib/matrix_client/timeline.mli`: type:item, val:event, val:local_echo, val:redacted, val:replacement, val:content,
  type:t, val:create, val:room_id, val:items, val:length, val:find, val:last, val:clear, val:add, val:add_many,
  val:prepend, val:prev_batch, val:set_prev_batch, val:paginate_back
- `lib/matrix_client/to_device.mli`: type:recipient, type:messages, val:send, val:send_with_new_txn
- `lib/matrix_client/typing.mli`: val:users_of_content, val:set_typing
- `lib/matrix_client/uiaa.mli`: type:auth_type, val:auth_type_of_string, val:auth_type_to_string, type:auth_flow,
  val:auth_flow_jsont, type:uiaa_response, val:uiaa_response_jsont, val:parse_uiaa_response, val:has_dummy_flow,
  type:user_identifier, type:threepid_creds, type:auth_data, val:password_auth, val:dummy_auth, val:recaptcha_auth,
  val:email_identity_auth, val:msisdn_auth, val:token_auth, val:oauth_auth, val:terms_auth, val:user_identifier_to_json,
  val:auth_data_to_json, val:add_auth_to_body, type:'a, val:with_uiaa, type:request_token_response,
  val:request_token_response_jsont, type:token_use, val:request_email_token, val:request_msisdn_token,
  val:validate_email_token
- `lib/matrix_client/verification.mli`: include:module, include:Verification_base, module:Sas, module:Qr, module:Flow
- `lib/matrix_client/verification_base.mli`: type:error, module:Cancel_code, type:t, val:to_string, val:of_string,
  val:reason, val:equal, val:pp, module:Method, type:t, val:to_string, val:of_string, val:equal, val:pp, val:all,
  val:common, module:Transaction, type:t, val:to_device, val:in_room, val:id, val:room_id, val:transaction_id,
  val:relates_to, val:equal, val:pp, module:Message, type:payload, type:t, val:v, val:transaction, val:payload,
  val:event_type, val:to_json, val:to_string, val:of_json, val:of_string, val:pp, val:cancel, val:done_, val:ready,
  type:request, val:request_to_device, val:request_in_room, val:ready_response
- `lib/matrix_client/verification_flow.mli`: type:stage, type:session, type:t, type:routed_send, type:directed_send,
  type:step, val:create, val:find, val:sessions, val:remove, val:session_transaction, val:session_stage,
  val:session_their_user_id, val:session_their_device_id, val:session_their_methods, val:session_we_requested,
  val:session_requested_devices, val:session_sas, type:request, val:request, val:take_pending_sends, val:request_in_room,
  val:accept, val:start_sas, val:confirm, val:mismatch, val:cancel, val:show_qr, val:scanned_qr, val:handle, val:tick
- `lib/matrix_client/verification_qr.mli`: type:error, type:mode, val:mode_to_int, val:mode_of_int, type:t, val:mode,
  val:flow_id, val:first_key, val:second_key, val:shared_secret, val:shared_secret_raw, val:equal, val:pp, val:make,
  val:create, val:for_other_user, val:for_self_trusted, val:for_self_untrusted, val:encode, val:decode, val:check,
  val:reciprocate_start, val:check_reciprocate
- `lib/matrix_client/verification_sas.mli`: module:Mac_method, type:t, val:to_string, val:of_string, val:equal, val:pp,
  val:all, type:emoji, val:emoji_table, val:emoji_indices, val:emoji_of_bytes, val:decimals_of_bytes, val:commitment,
  type:identity, val:identity, val:identity_keys, type:stage, type:t, type:step, val:stage, val:transaction,
  val:we_started, val:our_identity, val:their_identity, val:mac_method, val:is_done, val:is_cancelled, val:cancel_code,
  val:verified_keys, val:emoji, val:decimals, val:pp, val:start, val:from_start, val:handle, val:confirm, val:cancel,
  val:mismatch, val:tick
- `lib/matrix_eio/account.mli`: type:medium, type:threepid, val:get_threepids, val:request_email_token,
  val:request_msisdn_token, val:add_threepid, val:delete_threepid, val:change_password, val:deactivate,
  val:get_ignored_users, val:ignore_user, val:unignore_user
- `lib/matrix_eio/account_data.mli`: val:get, val:set, val:get_room, val:set_room, val:set_marked_unread,
  val:find_dm_rooms, val:get_or_create_dm
- `lib/matrix_eio/adaptive_sync.mli`: type:mode, type:response, val:run
- `lib/matrix_eio/admin.mli`: type:connection, type:connection_info, type:session, type:session_info, type:device,
  type:device_info, type:response, val:whois, val:get_user_info
- `lib/matrix_eio/auth.mli`: type:login_flow, val:get_login_flows, type:login_params, val:default_login_params,
  val:login_password_with_expiry, val:login_password, val:login_token_with_expiry, val:login_token, val:refresh_token,
  val:refresh_token_with_expiry, val:logout, val:logout_session, val:logout_all, type:registration_kind, val:register,
  val:register_uiaa, val:register_available, val:check_registration_token, val:whoami, type:token_login,
  val:get_login_token, val:request_registration_email_token, val:request_registration_msisdn_token,
  val:request_password_email_token, val:request_password_msisdn_token
- `lib/matrix_eio/backup.mli`: val:backup_algorithm, type:encryption_key, module:Decryption_key, type:t, val:generate,
  val:of_bytes, val:of_base64, val:to_base64, val:public, module:Recovery_key, val:encode, val:decode,
  type:encrypted_session_data, type:key_backup_data, type:sessions, type:rooms, type:backed_up_session_data,
  type:recovered_room_key, val:encrypt_session_data, val:encrypt_room_key, val:decrypt_room_key, val:parse_recovered_key,
  val:encrypted_session_data_jsont, val:key_backup_data_jsont, val:backed_up_session_data_jsont,
  type:megolm_v1_auth_data, val:megolm_v1_auth_data_jsont, val:auth_data_to_json, val:sign_auth_data,
  type:signature_state, val:verify_auth_data_signature
- `lib/matrix_eio/client.mli`: type:t, val:create, val:base, val:switch, val:http, val:media_fetcher,
  val:get_media_fetcher, val:set_media_fetcher, val:homeserver, val:well_known_policy, val:session, val:sync_presence,
  val:set_sync_presence, val:register_presence_wakeup, val:is_logged_in, val:user_id, val:device_id, val:access_token,
  val:with_session, val:with_access_token, val:with_auto_refresh, val:with_auto_refresh_expiry
- `lib/matrix_eio/dehydrated_device.mli`: type:t, val:pickle_key_secret_name, module:Pickle_key, type:t, type:error,
  val:generate, val:of_base64, val:to_base64, val:is_key_stored, val:load_key, val:cached_key, val:load_key_with_driver,
  val:reset_key, val:reset_key_with_driver, val:is_supported, type:events, type:rehydrate_outcome, val:get,
  val:get_if_present, val:put, val:put_and_remember, val:create_and_upload, val:rehydrate, val:delete,
  val:delete_if_present, val:get_events, module:Manager, type:event, type:subscription, type:t, val:create,
  val:subscribe, val:unsubscribe, val:start, val:stop, val:delete
- `lib/matrix_eio/delayed_events.mli`: val:unstable_prefix, type:delay_id, val:delay_id_of_string,
  val:delay_id_to_string, val:send, val:send_state, type:action, val:update, val:send_now, val:cancel, val:restart,
  type:delayed_event, val:list
- `lib/matrix_eio/devices.mli`: type:device, val:get_devices, val:get_device, val:update_device, val:delete_device,
  val:delete_devices
- `lib/matrix_eio/directory.mli`: type:room_id_or_alias, type:alias_info, val:resolve_alias, val:create_alias,
  val:delete_alias, val:get_visibility, val:set_visibility, type:space_child, type:room_summary, val:get_summary,
  type:search_filter, type:published_rooms, val:get_public_rooms, val:search_public_rooms
- `lib/matrix_eio/encryption.mli`: module:does, type:t, val:create, val:create_with_account, val:of_env, val:machine,
  val:save, val:user_id, val:device_id, val:identity_keys, val:sign, val:device_keys_for_upload, val:snapshot,
  type:trust, type:identity_status, type:device, val:device_key, val:device_ed25519, val:device_curve25519,
  val:track_users, val:untrack_users, val:tracked_users, val:outdated_users, val:devices_of, val:find_device,
  val:find_device_by_curve25519, val:identity_master_key, val:identity_self_signing_key, val:identity_user_signing_key,
  val:identity_status, val:identity_has_pin_violation, val:pin_user_identity, val:trust_user_identity,
  val:set_device_trust, val:receive_keys_query, val:receive_keys_claim, type:room_settings, val:enable_room_encryption,
  val:room_encryption_content, val:set_room_encryption_settings, val:find_room_settings, val:is_room_encrypted,
  type:request, val:pp_request, val:outgoing_requests, val:execute_requests, type:room_key_bundle_outcome,
  val:accept_received_room_key_bundle, type:to_device_event, val:pp_to_device_event, type:outcome, val:process_sync,
  val:process_sliding_sync, val:sync_hook, val:sync_hook_sliding, type:decrypted_event, type:decrypt_error,
  val:pp_decrypt_error, val:decrypt_room_event, val:request_room_key, val:encrypt_room_event, val:send_encrypted,
  val:send_encrypted_text, val:enable_backup, val:disable_backup, val:backup_version, val:backup_pending_count,
  val:backup_pending, val:restore_from_backup, val:restore_room_from_backup, val:restore_session_from_backup,
  type:share_room_history_outcome, type:share_room_history_error, type:invite_outcome, type:invite_error,
  val:share_room_history, val:invite_user_by_id, val:inbound_sessions, val:has_inbound_session, val:outbound_session_id,
  val:outbound_message_count
- `lib/matrix_eio/error.mli`: type:err, type:Eio.Exn.err, val:err, val:pp_err, val:of_client_error,
  val:raise_client_error, val:unwrap, val:is_retryable
- `lib/matrix_eio/http.mli`: val:https, val:client
- `lib/matrix_eio/keys.mli`: type:signatures, type:device_keys, type:one_time_key, val:one_time_key_signing_json,
  type:key_usage, type:cross_signing_key, type:upload_keys_response, val:upload_keys, type:query_keys_response,
  val:query_keys, type:claim_keys_response, val:claim_keys, type:key_changes_response, val:get_key_changes,
  val:upload_signing_keys, val:upload_signing_keys_uiaa, type:upload_signatures_response, val:upload_signatures
- `lib/matrix_eio/knock_requests.mli`: type:t, val:list, val:all, val:mark_seen, val:accept, val:decline,
  val:decline_and_ban
- `lib/matrix_eio/matrix_eio.mli`: module:Error, module:Http, module:Client, val:connect, module:Auth, module:Oauth,
  module:Qr_login, val:login_password, val:login_password_with_expiry, module:Server, module:Directory,
  module:Room_preview, module:Knock_requests, module:Thirdparty, module:Rooms, module:Room_details, module:Messages,
  module:Thread_paginator, module:Media, module:State, module:Retention, module:Relations, module:Typing,
  module:Receipts, module:Tags, module:Search, module:Report, module:Delayed_events, module:Send_queue, module:Account,
  module:Account_data, module:Recovery, module:Profile, module:Presence, module:Devices, module:Admin, module:Openid,
  module:Notifications, module:Notification_settings, module:Thread_subscriptions, module:Sync, module:Sliding_sync,
  module:Sync_service, module:Adaptive_sync, val:run_sync, module:Keys, module:To_device, module:Verification,
  module:Encryption, module:Verification_service, module:Backup, module:Room_keys, module:Secret_storage, module:Secrets,
  module:Dehydrated_device
- `lib/matrix_eio/media.mli`: module:Mxc, type:encrypted_file, type:source, type:format, type:request,
  type:encrypted_error, val:get_content, val:mxc_to_http, val:mxc_to_http_unauthenticated, val:mxc_to_http_resolved,
  val:upload, type:preallocated, val:create_content_uri, type:preallocated_upload_error,
  val:pp_preallocated_upload_error, val:upload_preallocated, type:content, val:download, val:thumbnail, type:preview,
  val:get_url_preview, type:config, val:get_config
- `lib/matrix_eio/messages.mli`: val:send_event, val:send_text, val:send_emote, val:send_notice, val:send_image,
  val:send_file, val:redact, type:messages_response, val:get_messages, val:get_event, type:context, val:get_context
- `lib/matrix_eio/notification_settings.mli`: type:room_notification_mode, type:t, type:subscription, val:create,
  val:client, val:ruleset, val:refresh, val:user_defined_room_mode, val:default_room_mode, val:room_mode,
  val:rooms_with_user_defined_rules, val:set_room_mode, val:delete_room_mode, val:unmute_room, val:set_default_room_mode,
  val:contains_keyword_rules, val:enabled_keywords, val:add_keyword, val:remove_keyword, val:is_enabled, val:set_enabled,
  val:set_actions, val:subscribe, val:unsubscribe
- `lib/matrix_eio/notifications.mli`: type:notification, type:notifications, val:get
- `lib/matrix_eio/oauth.mli`: type:oauth_error, val:pp_oauth_error, val:oauth_error_of_json, val:oauth_error_of_error,
  type:err, type:session_invalid_reason, type:Eio.Exn.err, val:pp_err, val:scope_api, val:scope_device_prefix,
  val:scope_api_unstable, val:scope_device_prefix_unstable, val:scope_device, val:scope_device_unstable,
  val:device_id_of_scope, val:generate_device_id, type:account_action, val:account_action_to_string,
  val:account_action_of_string, module:Metadata, type:t, val:jsont, val:v1_path, val:unstable_path,
  val:openid_configuration_path, val:fetch, val:fetch_cached, val:invalidate_cache, val:validate, val:validate_device,
  val:supports_response_type, val:supports_grant_type, val:supports_response_mode, val:supports_code_challenge_method,
  val:supports_prompt, val:supports_account_action, val:account_management_url, module:Registration, type:localized,
  val:plain, type:client_metadata, val:v, val:loopback_redirect_uri, val:to_json, type:response, val:register,
  module:Pkce, module:Authorization, type:request, val:build_url, val:request, type:redirect, type:redirect_error,
  val:pp_redirect_error, val:parse_redirect, module:Token, type:t, type:token_type_hint, val:jsont, val:is_expired,
  val:exchange, val:refresh, val:revoke, val:logout, val:finish_login, val:with_auto_refresh,
  val:with_auto_refresh_expiry, module:Device_authorization, type:t, val:jsont, type:poll_error, val:request, val:poll,
  module:Loopback, type:t, val:create, val:port, val:redirect_uri, val:wait, val:default_client_metadata,
  val:default_device_client_metadata, val:default_timeout, val:browser_opener, type:browser_login,
  type:browser_login_with_expiry, val:login_with_browser_full_expiry, val:login_with_browser_full,
  val:login_with_browser, type:device_login, type:device_login_with_expiry, val:login_with_device_expiry,
  val:login_with_device
- `lib/matrix_eio/openid.mli`: type:token, val:request_token, val:request_own_token
- `lib/matrix_eio/presence.mli`: type:presence_state, type:presence, val:get_presence, val:set_presence
- `lib/matrix_eio/profile.mli`: type:profile, val:get_profile, val:get_displayname, val:set_displayname,
  val:clear_displayname, val:get_avatar_url, val:set_avatar_url, val:clear_avatar_url, val:find_field, val:set_field,
  val:delete_field
- `lib/matrix_eio/qr_login.mli`: type:intent, type:t, type:codec_error, val:pp_codec_error, val:make, val:of_bytes,
  val:to_bytes, val:of_base64, val:to_base64, val:rendezvous_path, val:rendezvous_server_supported,
  type:secure_channel_error, val:establish_secure_channel, module:Msc4108, include:module, module:Application_eio,
  type:login_progress, type:login_start, type:grant_progress, type:grant_decision, type:login, type:Eio.Exn.err,
  val:pp_application_error, val:login, val:grant, module:Session, type:progress, type:persistence, type:error,
  type:Eio.Exn.err, val:pp_error, val:login, val:grant
- `lib/matrix_eio/receipts.mli`: type:receipt_type, val:send_receipt, val:set_read_marker
- `lib/matrix_eio/recovery.mli`: type:state, type:marker, type:markers, type:account_data_write, type:inputs,
  val:key_backup_event_type, val:backup_disabled_event_type, val:key_backup_jsont, val:backup_disabled_jsont,
  val:key_backup_marker, val:backup_disabled_marker, val:state, val:check_state, val:known_secret_event_types,
  val:mark_enabled_writes, val:disable_writes, val:fetch_markers, val:apply_writes, val:mark_backup_enabled,
  val:disable_account_data, val:disable, val:disable_and_delete_backups, type:recovered, val:recover,
  val:recover_and_fix_backup, type:backup_upload, type:enabled, val:enable, val:reset_key, type:recovered_and_reset,
  val:recover_and_reset, module:Manager, type:t, type:subscription, val:create, val:base_state, val:private_identity,
  val:set_private_identity, val:state, val:refresh_from_base, val:subscribe, val:unsubscribe, val:watch, val:refresh,
  val:disable, val:disable_and_delete_backups, val:recover, val:recover_and_fix_backup, val:enable, val:reset_key,
  val:recover_and_reset, val:reset_identity, val:cancel_pending_identity_reset
- `lib/matrix_eio/relations.mli`: val:send_reaction, val:edit_message, val:send_reply, val:send_in_thread,
  type:related_event, val:get_relations, val:get_raw_relations, val:get_edit_revisions, val:get_reactions,
  type:thread_filter, val:list_threads
- `lib/matrix_eio/report.mli`: val:event, val:room, val:user
- `lib/matrix_eio/retention.mli`: type:policy, type:lifetime_limits, type:limits, type:configuration,
  val:get_configuration, val:get_room_policy, val:set_room_policy, val:effective
- `lib/matrix_eio/room_details.mli`: type:t, type:member, val:create, val:state, val:room_id, val:members_complete,
  val:members, val:member_count, val:service_member_count, val:human_member_count, val:ensure_members
- `lib/matrix_eio/room_keys.mli`: type:version_info, val:equal_version_info, type:key_backup_data, type:sessions,
  type:rooms, type:update_response, val:create_version, val:get_current_version, val:get_version, val:update_version,
  val:delete_version, val:put_keys, val:get_keys, val:delete_keys, val:put_room_keys, val:get_room_keys,
  val:delete_room_keys, val:put_session_key, val:get_session_key, val:delete_session_key
- `lib/matrix_eio/room_preview.mli`: type:t, val:get
- `lib/matrix_eio/rooms.mli`: type:preset, val:create, val:join, val:knock, val:leave, val:forget, val:invite, val:kick,
  val:ban, val:unban, val:get_joined_rooms, type:member, val:get_members, type:joined_member, val:get_joined_members,
  val:get_power_levels, val:set_power_levels, val:set_user_power_level, val:start_live_location_share,
  val:stop_live_location_share, val:send_location_beacon, val:upgrade, val:get_aliases, type:timestamp_event,
  val:timestamp_to_event
- `lib/matrix_eio/search.mli`: type:key, type:order_by, type:group_by, type:event_context_request, type:criteria, val:v,
  type:user_profile, type:event_context, type:hit, type:group, type:room_events_result, val:room_events, type:user,
  type:user_directory_result, val:user_directory
- `lib/matrix_eio/secret_storage.mli`: val:algorithm, val:pbkdf2_algorithm, val:secret_cross_signing_master,
  val:secret_cross_signing_self_signing, val:secret_cross_signing_user_signing, val:secret_megolm_backup_v1, type:key,
  val:generate_key, val:key_of_bytes, val:to_bytes, module:Recovery_key, val:encode, val:decode, module:Passphrase_info,
  type:t, val:default_bits, val:v, val:jsont, module:Key_description, type:t, val:v, val:jsont, val:key_of_passphrase,
  type:key_check, val:check_key, module:Encrypted, type:t, val:jsont, val:encrypt, val:decrypt
- `lib/matrix_eio/secrets.mli`: val:default_key_event_type, val:key_event_type, val:get_default_key_id,
  val:set_default_key_id, val:get_key_description, val:put_key_description, val:get_secret, val:get_secret_opt,
  val:store_secret, type:store, type:created_store, val:create_secret_store, val:open_secret_store,
  val:create_recovery_store, val:store_key_id, val:store_user_id, val:store_homeserver, val:get_store_secret,
  val:put_store_secret, val:export_recovery_secrets, val:import_backup, val:import_cross_signing
- `lib/matrix_eio/send_queue.mli`: type:kind, type:attachment_upload, type:attachment_edit_result, type:upload_result,
  type:dependency_result, type:progress, type:status, type:request, type:t, val:create, val:enqueue, val:send_message,
  val:upload, val:upload_encrypted, val:attachment_upload, val:attachment_upload_encrypted, val:send_attachment,
  val:edit_attachment_caption, val:send_text, val:send_edit, val:send_reaction, val:send_redaction, val:id, val:room_id,
  val:kind, val:txn_id, val:status, val:attempts, val:last_error, val:requests, val:room_requests, val:dependencies,
  val:resolved_dependencies, val:dependency_results, val:rooms, val:next, val:pending_count, val:is_empty,
  val:local_echo, val:cancel, val:cancel_with_reason, val:forget_room, val:unwedge, val:enabled, val:set_enabled,
  val:room_enabled, val:set_room_enabled, val:on_change, val:on_progress, val:retry_delay, val:save, type:outcome,
  type:payload, val:payload, val:send_one, val:sender_for, val:start
- `lib/matrix_eio/server.mli`: type:versions, val:get_versions, val:supports_version, val:supports_version_at_least,
  val:has_unstable_feature, val:invalidate_cache, type:room_version_stability, type:room_versions_capability,
  type:capabilities, type:account_moderation_capability, type:profile_fields_capability, val:get_capabilities,
  val:refresh_capabilities, val:find_capability, val:can_change_password, val:can_change_thirdparty_ids,
  val:can_get_login_token, val:room_versions, val:account_moderation, val:forgets_room_when_leaving,
  val:can_change_displayname, val:can_change_avatar, val:extended_profile_fields, type:server_info,
  type:authentication_info, type:well_known, val:get_well_known, type:discovery, val:discover
- `lib/matrix_eio/sliding_sync.mli`: module:Request, module:Response, module:Controller, type:t, val:create, val:request,
  val:add_room_subscriptions, val:remove_room_subscriptions, val:set_room_subscriptions,
  val:reset_and_add_room_subscriptions, module:Own_profile, type:t, type:subscription, val:create, val:user_id,
  val:current, val:refresh, val:refresh_base, val:subscribe, val:unsubscribe, val:watch, val:path,
  val:default_timeout_ms, val:native_feature, val:is_available_in, val:is_available, val:sync_once, val:is_unsupported,
  val:is_expired_pos, val:sync_forever, val:sync_forever_controlled, val:sync_to_stream, val:create_sync_stream
- `lib/matrix_eio/state.mli`: val:get_state, val:get_state_event, val:set_state, val:get_name, val:set_name,
  val:get_topic, val:set_topic, val:get_avatar, val:set_avatar
- `lib/matrix_eio/sync.mli`: type:params, val:default_params, type:response, val:sync, type:action, type:'a,
  val:default_on_error, val:callbacks, val:sync_forever, val:sync_to_stream, val:create_sync_stream, val:iter,
  module:Filter, type:filter, val:default_filter, val:default_room_filter, val:default_event_filter,
  val:default_room_event_filter, val:create_filter, val:get_filter
- `lib/matrix_eio/sync_service.mli`: type:state, type:changes, type:room_change, type:decrypted, type:action, type:t,
  val:create, val:of_store, val:of_user, val:state, val:store, val:forget_room, val:remove_direct_room, val:members,
  val:replace_members, val:on_response, val:on_sliding_response, val:on_room_event, val:on_profile_change,
  type:profile_subscription, val:subscribe_profile_changes, val:unsubscribe_profile_changes, val:apply_profile_updates,
  val:clear_profiles, val:persist, val:set_local_unread_counts_if_current, val:bootstrap_push_rules, val:begin_forget,
  val:apply, val:generation, val:migrate_legacy_sliding_state, val:reset_sliding_session, val:apply_sliding,
  val:apply_sliding_if_current, val:sync_once, val:run
- `lib/matrix_eio/tags.mli`: val:favourite, val:low_priority, val:server_notice, val:get, val:set, val:remove,
  val:set_favourite, val:set_low_priority
- `lib/matrix_eio/thirdparty.mli`: type:field_type, type:protocol_instance, type:protocol, val:protocols,
  val:get_protocol, type:location, val:locations_of_alias, val:locations, type:user, val:users_of_user_id, val:users
- `lib/matrix_eio/thread_paginator.mli`: type:state, type:t, val:create, val:set_filter, val:reset, val:state, val:roots,
  val:loaded_pages, val:is_at_last_page, val:subscribe, val:next_page
- `lib/matrix_eio/thread_subscriptions.mli`: type:status, type:subscription, type:unsubscription, type:page,
  type:stored_status, type:stored_subscription, type:update, type:catchup_token, val:is_supported, val:subscriptions,
  val:find_stored, val:merge, val:upsert, val:upsert_many, val:remove, val:remove_room, val:catchup_tokens,
  val:queue_catchup_token, val:catch_up_once, val:catch_up, val:get, val:subscribe, val:unsubscribe, val:changes,
  val:subscribe_and_store, val:unsubscribe_and_store, val:get_and_store, val:load_or_fetch, val:subscribe_if_needed,
  val:apply_sliding_extension
- `lib/matrix_eio/to_device.mli`: type:recipient, type:messages, val:send, val:send_with_new_txn
- `lib/matrix_eio/typing.mli`: val:set_typing
- `lib/matrix_eio/verification.mli`: module:Cancel_code, module:Method, module:Transaction, module:Message, module:Sas,
  module:Qr, module:Flow, module:Cross_signing, val:send_to_devices
- `lib/matrix_eio/verification_service.mli`: type:emoji, type:prompt, type:result, type:t, val:create, val:flow,
  val:sessions, val:handle, val:handle_room, val:request, val:request_in_room, val:accept, val:respond, val:cancel,
  val:tick
- `lib/matrix_proto/matrix_base64.mli`: val:encode, val:decode, val:decode_opt
- `lib/matrix_proto/matrix_common.mli`: module:Direction, type:t, val:to_string, val:of_string, val:equal, val:pp,
  val:jsont, module:Visibility, type:t, val:to_string, val:of_string, val:equal, val:pp, val:jsont, module:Page, type:'a,
  val:v, val:jsont
- `lib/matrix_proto/matrix_event.mli`: module:Timestamp, module:Rel_type, module:Relates_to, module:Image_info,
  module:Unsigned, type:t, val:make, val:empty, val:age, val:prev_content, val:prev_sender, val:redacted_because,
  val:transaction_id, val:membership, val:relations, val:without_relations, val:pp, val:jsont, module:Membership,
  module:Join_rule, module:History_visibility, module:Guest_access, module:Room_create_content, module:Room_name_content,
  module:Room_topic_content, module:Room_avatar_content, module:Room_member_content, module:Room_join_rules_content,
  module:Room_history_visibility_content, module:Room_canonical_alias_content, module:Room_power_levels_content,
  module:Room_retention_content, module:Room_member_hints_content, module:Io_element_functional_members_content,
  module:Room_encryption_content, module:Room_pinned_events_content, module:Room_server_acl_content,
  module:Room_tombstone_content, module:Room_guest_access_content, module:Space_child_content,
  module:Space_parent_content, module:Sdp, module:Hangup_reason, module:Call_invite_content, module:Call_answer_content,
  module:Call_hangup_content, module:Call_candidates_content, module:Call_member_content,
  module:Key_verification_request_content, module:Key_verification_request_message_content,
  module:Key_verification_ready_content, module:Key_verification_start_content, module:Key_verification_accept_content,
  module:Key_verification_key_content, module:Key_verification_mac_content, module:Key_verification_cancel_content,
  module:Key_verification_done_content, module:Msgtype, module:Media_info, module:Text_message_content,
  module:Media_message_content, module:Sticker_content, module:Location_message_content, module:Recommendation,
  module:Policy_rule_content, module:Marked_unread_content, module:Encryption_algorithm, module:Encrypted_content,
  module:Olm_message_type, module:Olm_ciphertext, module:Olm_plaintext, module:Encrypted, module:Room_key_content,
  module:Forwarded_room_key_content, module:Reaction_content, module:Beacon_info_content, module:Beacon_content,
  module:Poll_start_content, module:Poll_response_content, module:Poll_end_content, module:Event_type, type:t,
  val:to_string, val:of_string, val:equal, val:pp, val:jsont, module:Typed_event, type:'content, val:jsont,
  module:Raw_event, type:t, val:jsont, module:Stripped_event, type:t, val:jsont
- `lib/matrix_proto/matrix_event_call.mli`: module:Sdp, type:t, val:v, val:pp, val:jsont, module:Hangup_reason, type:t,
  val:to_string, val:of_string, val:equal, val:pp, val:jsont, module:Call_invite_content, type:t, val:make, val:call_id,
  val:party_id, val:version, val:lifetime, val:offer, val:invitee, val:pp, val:jsont, module:Call_answer_content, type:t,
  val:make, val:call_id, val:party_id, val:version, val:answer, val:pp, val:jsont, module:Call_hangup_content, type:t,
  val:make, val:call_id, val:party_id, val:version, val:reason, val:pp, val:jsont, module:Call_candidates_content,
  type:candidate, val:make_candidate, type:t, val:make, val:call_id, val:party_id, val:version, val:candidates, val:pp,
  val:jsont, module:Call_member_content, type:focus, val:make_focus, type:membership, val:make_membership, type:t,
  val:make, val:memberships, val:pp, val:jsont
- `lib/matrix_proto/matrix_event_core.mli`: module:Timestamp, type:t, val:of_ms, val:to_ms, val:of_ptime,
  val:to_ptime_opt, val:equal, val:compare, val:pp, val:jsont, module:Rel_type, type:t, val:to_string, val:of_string,
  val:equal, val:pp, val:jsont, module:Relates_to, type:t, val:v, val:reference, val:equal, val:pp, val:jsont,
  module:Image_info, type:t, val:v, val:pp, val:jsont
- `lib/matrix_proto/matrix_event_encrypted.mli`: module:Olm_message_type, type:t, val:to_int, val:of_int, val:equal,
  val:pp, val:jsont, module:Olm_ciphertext, type:entry, type:t, val:find, val:jsont, module:Olm_plaintext, type:t,
  val:jsont, module:Encrypted, module:Olm, type:t, val:jsont, module:Megolm, type:t, val:jsont, module:Room_key_content,
  type:t, val:jsont, module:Forwarded_room_key_content, type:t, val:jsont
- `lib/matrix_proto/matrix_event_extensible.mli`: module:Recommendation, type:t, val:to_string, val:of_string, val:equal,
  val:pp, val:jsont, module:Policy_rule_content, type:t, val:jsont, module:Marked_unread_content, type:t, val:jsont,
  module:Encryption_algorithm, type:t, val:to_string, val:of_string, val:equal, val:pp, val:jsont,
  module:Encrypted_content, type:t, val:jsont, module:Reaction_content, type:t, val:jsont, module:Beacon_info_content,
  type:t, val:jsont, module:Beacon_content, type:location, type:t, val:jsont, module:Poll_start_content,
  type:poll_answer, type:poll_kind, type:poll_start, type:t, val:jsont, module:Poll_response_content, type:t, val:jsont,
  module:Poll_end_content, type:t, val:jsont
- `lib/matrix_proto/matrix_event_message.mli`: module:Msgtype, type:t, val:to_string, val:of_string, val:equal, val:pp,
  val:jsont, module:Media_info, type:t, val:v, val:jsont, module:Text_message_content, type:t, val:make, val:body,
  val:msgtype, val:format, val:formatted_body, val:pp, val:jsont, module:Media_message_content, type:t, val:jsont,
  module:Sticker_content, type:t, val:jsont, module:Location_message_content, type:location_info, type:t, val:jsont
- `lib/matrix_proto/matrix_event_space.mli`: module:Space_child_content, type:t, val:make, val:via, val:order,
  val:suggested, val:pp, val:jsont, module:Space_parent_content, type:t, val:make, val:via, val:canonical, val:pp,
  val:jsont
- `lib/matrix_proto/matrix_event_state.mli`: module:Membership, type:t, val:to_string, val:of_string, val:equal, val:pp,
  val:jsont, type:does, module:Join_rule, type:t, val:to_string, val:of_string, val:equal, val:pp, val:jsont,
  module:History_visibility, type:t, val:to_string, val:of_string, val:equal, val:pp, val:jsont, type:does,
  module:Guest_access, type:t, val:to_string, val:of_string, val:equal, val:pp, val:jsont, module:Room_create_content,
  module:Predecessor, type:t, val:make, val:room_id, val:event_id, val:pp, val:jsont, type:t, val:make, val:creator,
  val:room_version, val:predecessor, val:room_type, val:pp, val:jsont, module:Room_name_content, type:t, val:make,
  val:name, val:pp, val:jsont, module:Room_topic_content, type:t, val:make, val:topic, val:pp, val:jsont,
  module:Room_avatar_content, type:t, val:make, val:url, val:info, val:pp, val:jsont, module:Room_member_content, type:t,
  val:make, val:membership, val:displayname, val:avatar_url, val:is_direct, val:reason, val:pp, val:jsont,
  module:Room_join_rules_content, module:Allow_condition, type:t, val:make, val:condition_type, val:room_id, val:pp,
  val:jsont, type:t, val:make, val:join_rule, val:allow, val:pp, val:jsont, module:Room_history_visibility_content,
  type:t, val:make, val:history_visibility, val:pp, val:jsont, module:Room_canonical_alias_content, type:t, val:make,
  val:alias, val:alt_aliases, val:pp, val:jsont, module:Room_power_levels_content, type:t, val:make, val:ban, val:events,
  val:events_default, val:invite, val:kick, val:redact, val:state_default, val:users, val:users_default,
  val:notifications, val:user_level, val:with_user_level, val:pp, val:jsont, module:Room_retention_content, type:t,
  val:make, val:min_lifetime, val:max_lifetime, val:pp, val:jsont, module:Room_member_hints_content, type:t, val:make,
  val:service_members, val:pp, val:jsont, module:Io_element_functional_members_content, type:t, val:make,
  val:service_members, val:functional_members, val:pp, val:jsont, module:Room_encryption_content, type:t, val:make,
  val:algorithm, val:rotation_period_ms, val:rotation_period_msgs, val:pp, val:jsont, module:Room_pinned_events_content,
  type:t, val:make, val:pinned, val:pp, val:jsont, module:Room_server_acl_content, type:t, val:make, val:allow,
  val:allow_ip_literals, val:deny, val:pp, val:jsont, module:Room_tombstone_content, type:t, val:make, val:body,
  val:replacement_room, val:pp, val:jsont, module:Room_guest_access_content, type:t, val:make, val:guest_access, val:pp,
  val:jsont
- `lib/matrix_proto/matrix_event_verification.mli`: module:Key_verification_request_content, type:t, val:make,
  val:from_device, val:methods, val:transaction_id, val:timestamp, val:pp, val:jsont,
  module:Key_verification_request_message_content, type:t, val:msgtype, val:make, val:body, val:from_device, val:methods,
  val:to_, val:format, val:formatted_body, val:pp, val:jsont, module:Key_verification_ready_content, type:t, val:make,
  val:from_device, val:methods, val:transaction_id, val:relates_to, val:pp, val:jsont,
  module:Key_verification_start_content, type:t, val:make, val:from_device, val:method_, val:transaction_id,
  val:next_method, val:key_agreement_protocols, val:hashes, val:message_authentication_codes,
  val:short_authentication_string, val:secret, val:relates_to, val:pp, val:jsont, module:Key_verification_accept_content,
  type:t, val:make, val:transaction_id, val:method_, val:key_agreement_protocol, val:hash,
  val:message_authentication_code, val:short_authentication_string, val:commitment, val:relates_to, val:pp, val:jsont,
  module:Key_verification_key_content, type:t, val:make, val:transaction_id, val:key, val:relates_to, val:pp, val:jsont,
  module:Key_verification_mac_content, type:t, val:make, val:transaction_id, val:mac, val:keys, val:relates_to, val:pp,
  val:jsont, module:Key_verification_cancel_content, type:t, val:make, val:transaction_id, val:code, val:reason,
  val:relates_to, val:pp, val:jsont, module:Key_verification_done_content, type:t, val:make, val:transaction_id,
  val:relates_to, val:pp, val:jsont
- `lib/matrix_proto/matrix_id.mli`: module-type:S, type:t, val:of_string, val:of_string_exn, val:to_string, val:equal,
  val:compare, val:pp, val:jsont, module:Server_name, include:S, module:User_id, include:S, val:localpart,
  val:server_name, val:is_spec_conformant, module:Room_id, include:S, val:opaque_id, val:server_name, module:Event_id,
  include:S, module:Room_alias, include:S, val:alias, val:server_name, module:Device_id, include:S, module:Session_id,
  include:S, module:Transaction_id, include:S, val:v, val:of_bytes
- `lib/matrix_proto/matrix_json.mli`: val:as_string, val:as_bool, val:as_int, val:as_int64, val:as_float, val:as_array,
  val:as_object, val:find_mem, val:find_string, val:find_bool, val:find_int
- `lib/matrix_proto/matrix_proto.mli`: module:Id, module:Event, module:Sync, module:Sliding_sync, module:Push,
  module:Json, module:Common, module:Base64, module:Signed_json, module:Matrix_id, module:Matrix_event,
  module:Matrix_event_core, module:Matrix_event_state, module:Matrix_event_space, module:Matrix_event_call,
  module:Matrix_event_verification, module:Matrix_event_message, module:Matrix_event_extensible,
  module:Matrix_event_encrypted, module:Matrix_sync, module:Matrix_sliding_sync, module:Matrix_push, module:Matrix_json,
  module:Matrix_common, module:Matrix_base64, module:Matrix_signed_json
- `lib/matrix_proto/matrix_push.mli`: module:Kind, type:t, val:all, val:to_string, val:of_string, val:equal, val:pp,
  val:jsont, module:Rule_id, type:t, val:override, val:content, val:room, val:sender, val:underride, val:v, val:kind,
  val:id, val:equal, val:pp, module:Tweak, type:t, val:name, val:equal, val:pp, module:Action, type:t, val:equal, val:pp,
  val:jsont, module:Condition, module:Comparison, type:t, val:to_string, val:of_string, val:equal, val:pp, type:t,
  val:equal, val:pp, val:jsont, module:Rule, type:t, val:v, val:kind, val:equal, val:pp, val:jsont, module:Ruleset,
  type:t, val:empty, val:rules, val:equal, val:pp, val:jsont, val:global_jsont, val:default_ruleset
- `lib/matrix_proto/matrix_signed_json.mli`: val:canonical_json, val:json_for_signing
- `lib/matrix_proto/matrix_sliding_sync.mli`: module:Required_state, type:t, val:v, val:any_event_type,
  val:any_state_key, val:lazy_members, val:own_membership, val:equal, val:jsont, module:Request, type:filters,
  val:no_filters, type:list_request, type:room_subscription, type:extension_room, type:e2ee, type:to_device, type:scoped,
  type:profiles, type:thread_subscriptions, type:extensions, val:no_extensions, type:t, val:jsont, val:v, val:add_list,
  val:remove_list, val:subscribe_room, val:unsubscribe_room, val:clear_room_subscriptions, val:enable_e2ee,
  val:enable_to_device, val:with_to_device_since, val:to_device_enabled, val:enable_account_data, val:enable_receipts,
  val:enable_typing, val:enable_profiles, val:enable_thread_subscriptions, val:with_txn_id, module:Response,
  type:list_response, type:hero, type:profile_update, type:profiles, type:avatar, type:room, type:e2ee, type:to_device,
  type:account_data, type:ephemeral, type:thread_subscription, type:thread_unsubscription, type:thread_subscriptions,
  type:extensions, type:t, val:jsont, val:to_device_next_batch
- `lib/matrix_proto/matrix_string_map.mli`: val:jsont
- `lib/matrix_proto/matrix_sync.mli`: module:Raw_events, type:t, val:pp, val:jsont, module:Stripped_events, type:t,
  val:pp, val:jsont, module:Timeline, type:t, val:pp, val:jsont, module:Room_state, type:t, val:jsont, module:Ephemeral,
  module:Account_data, module:To_device, module:Presence, module:Unread_notification_counts, type:t, val:jsont,
  module:Room_summary, type:t, val:jsont, module:Joined_room, type:t, val:pp, val:jsont, module:Invited_room, type:t,
  val:jsont, module:Left_room, type:t, val:jsont, module:Knocked_room, type:t, val:jsont, module:Rooms, type:t,
  val:jsont, module:Device_lists, type:t, val:jsont, module:Response, type:t, val:pp, val:jsont
- `lib/matrix_ui/back_pagination.mli`: type:priority, type:stop_reason, type:run_result, type:request, type:t,
  type:handle, val:create, val:enqueue, val:await, val:cancel, val:close
- `lib/matrix_ui/event_cache.mli`: type:delivery, type:event, val:effective, module:Gap_id, type:t, val:equal,
  val:to_string, type:gap, type:t, val:create, val:last_error, val:forget_room, val:is_forgotten,
  val:subscribe_forget_room, val:subscribe_physical_decryption, val:events, val:snapshot, val:snapshot_with_gaps,
  val:with_snapshot_if_current, val:position, val:find_event, val:register_external_event, val:related_events,
  val:prev_batch, val:has_gap, val:gaps, val:undecrypted, val:apply_room_change, val:prepend, type:prepend_applied,
  type:prepend_result, val:prepend_if_token, val:resolve_gap, val:set_decrypted, val:track_send_queue, val:find_echo,
  val:flush_room
- `lib/matrix_ui/event_focused.mli`: type:state, type:thread_mode, type:page, type:start_result, type:error,
  val:pp_error, type:t, val:create, val:events, val:snapshot, val:start, val:paginate_backward, val:paginate_forward,
  val:reset, val:state, val:subscribe, val:close
- `lib/matrix_ui/event_store.mli`: module:Error, type:t, val:to_string, val:pp, module:Internal, type:delivery,
  type:event, type:events_chunk, type:gap_chunk, type:chunk, val:max_external_events, type:room, val:room_events,
  val:room_prev_batch, val:room_has_gap, type:change, module-type:S, type:t, val:load_room, val:save_room, val:apply,
  val:remove_room, val:close, type:plaintext_policy, type:t, val:v, val:memory, val:plaintext_policy, val:load_room,
  val:save_room, val:apply, val:remove_room, val:close
- `lib/matrix_ui/live_locations.mli`: type:last_location, type:share, type:t, val:create, val:shares, val:refresh,
  val:refresh_state, val:refresh_time, val:close, val:user_id, val:beacon_id, val:last_location
- `lib/matrix_ui/matching.mli`: val:search_key, val:contains, val:fuzzy_score, val:truncate_graphemes
- `lib/matrix_ui/matrix_ui.mli`: module:Observable, module:Matching, module:Presentation, module:Event_store,
  module:Event_cache, module:Pinned_events, module:Room_timeline, module:Room_list, module:Runtime, module:Utd_hook,
  module:Room_identity, module:Live_locations, module:Live_location, module:Notification_client,
  module:Room_directory_search, module:Search_service, module:Thread_info, module:Thread_cache, module:Thread_list,
  module:Event_focused, module:Back_pagination
- `lib/matrix_ui/notification_client.mli`: type:decrypt, val:decrypt_with, type:notification_event, type:status, type:t,
  val:create, val:fetch
- `lib/matrix_ui/observable.mli`: module:Value, type:'a, type:'a, val:create, val:get, val:set, val:subscribe, val:next,
  val:unsubscribe, module:List, type:'a, type:'a, type:'a, val:create, val:snapshot, val:length, val:get, val:subscribe,
  val:next, val:unsubscribe, val:insert, val:append, val:remove, val:set, val:reconcile_by, val:apply, val:apply_all
- `lib/matrix_ui/pinned_events.mli`: type:t, val:create, val:events, val:snapshot, val:refresh, val:close
- `lib/matrix_ui/presentation.mli`: module:Html, val:sanitize, val:to_plain, type:relation_kind, type:relation,
  type:message_kind, type:formatted_body, type:message, type:membership_change, type:'a, type:profile_change,
  type:other_state, type:content, type:t, val:equal, val:of_event, val:new_content, val:replacement,
  val:is_valid_replacement, val:is_valid_replacement_with_encryption, val:preview, val:is_preview_worthy
- `lib/matrix_ui/room_directory_search.mli`: type:state, type:t, val:create, val:search, val:next_page, val:results,
  val:state, val:loaded_pages, val:is_at_last_page
- `lib/matrix_ui/room_identity.mli`: type:violation, type:member, type:t, val:create, val:members, val:refresh,
  val:user_id, val:violation
- `lib/matrix_ui/room_list.mli`: type:section, type:order, type:room, val:unread, module:Filter, type:unread, type:t,
  val:matches, val:score, val:everything, val:equal, val:pp, val:compare_rooms, type:t, val:create, val:all_rooms,
  val:rooms, val:find, val:filter, val:set_filter, val:sort, val:set_sort, val:refresh
- `lib/matrix_ui/room_timeline.mli`: type:reaction, type:event_item, type:gap_id, type:date, type:virtual_item,
  type:item, type:event_filter, val:item_id, type:t, val:default_event_filter, val:create, val:close, val:discard,
  val:room_id, val:items, val:snapshot, val:loading, val:pagination_error, val:refresh, val:edit_revisions,
  type:pagination, val:paginate_back, val:paginate_gap, val:send_message, val:send_text, val:send_reply,
  type:location_asset, val:send_location, val:send_edit, val:send_reaction, val:redact, val:item_of_request,
  val:delivery, type:receipt_type, type:receipt, val:latest_user_read_receipt, val:send_single_receipt,
  val:send_multiple_receipts, val:mark_as_read
- `lib/matrix_ui/runtime.mli`: type:sync_state, type:t, val:create, val:event_cache, val:pinned_events,
  val:event_focused, val:thread_list, val:thread_info, val:thread_cache, val:room_list, val:send_queue, val:sync_service,
  val:sync_state, val:recovery_state, val:room_identity, val:typing_users, val:timeline, val:close_timeline, val:join,
  val:join_room, val:leave, val:forget, val:start, val:stop
- `lib/matrix_ui/search_service.mli`: type:state, type:t, val:create, val:search, val:next_page, val:results, val:state,
  val:last_error, val:loaded_pages
- `lib/matrix_ui/thread_cache.mli`: type:pagination_token, type:pagination, type:snapshot, type:t, val:create,
  val:snapshot, val:ingest, val:ingest_thread, val:set_pagination, val:set_receipts, val:set_room_receipts,
  val:set_unread, val:subscribe_events, val:subscribe_receipts, val:subscribe_unread, val:forget_room, val:close
- `lib/matrix_ui/thread_info.mli`: type:summary, type:summary_status, type:info, type:t, val:create, val:infos,
  val:snapshot, val:subscribe, val:summary_of_root, val:ingest_root, val:refresh_room, val:remove_room
- `lib/matrix_ui/thread_list.mli`: type:state, type:t, val:create, val:set_filter, val:reset, val:close, val:state,
  val:continuation, val:roots, val:loaded_pages, val:is_at_last_page, val:subscribe, val:next_page, val:infos,
  val:snapshot
- `lib/matrix_ui/utd_hook.mli`: type:report, type:t, val:create, val:on_utd, val:on_late_decrypt
- `lib/matrix_ui_sqlite/matrix_ui_sqlite.mli`: val:create, val:create_media_store
