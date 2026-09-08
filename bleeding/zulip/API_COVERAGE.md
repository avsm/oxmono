# Zulip API coverage

This library targets Zulip Server 12.2 and the public `zulip.Client` surface at
Python SDK revision `b1723475`. The compatibility target is functional: each
public Python helper has an OCaml operation or an intentional OCaml
replacement, and each documented 12.2 wire parameter for those operations is
accounted for. It is not a source-compatible port of Python call signatures.

The frozen parity inventory contains 77 Python helpers: 71 endpoint helpers and
six transport or callback helpers. Three pairs of endpoint helpers share an
operation (`get_users`/`get_members`, `get_subscriptions`/`list_subscriptions`,
and `update_message`/`move_topic`), giving 68 canonical REST operations. Across
those canonical operations, the inventory accounts for 294 distinct wire
parameters.

This is an SDK parity claim, not complete coverage of every REST operation in
the Zulip 12.2 OpenAPI document. The OCaml API also includes several current
12.2 operations useful to hosted bots that are absent from the pinned Python
client; those are described below.

## What “typed” and “extensions” mean

Core request choices use OCaml variants, records, optional arguments, and
distinct ID types shared from `Zulip.Id`. Structured form and query values use
Jsont and are encoded exactly once. Extensible enums retain future wire values
instead of silently dropping them. Core response fields have Jsont codecs.
Models for messages,
users, channels, subscriptions, groups, presence, attachments, drafts,
scheduled messages, reminders, settings, and registration state expose their
commonly used documented fields directly.

Zulip can add response members as its API evolves. Typed models therefore
retain unmodeled fields through explicitly named `raw` or `extensions`
boundaries; detailed responses often retain the complete JSON object. The raw value supplements
the typed fields; it is not presented as a second, fully typed schema. Simple
success endpoints may return `unit`, while `Client.request`,
`Client.request_typed`, and `Client.request_json` remain available for custom or
specialist operations.

## Verification status

The ordinary offline suite contains 107 Alcotest cases:

| Suite | Cases |
| --- | ---: |
| Protocol codecs | 7 |
| Client and transport | 14 |
| Profile storage | 5 |
| Existing endpoint behavior | 10 |
| Bot runtime | 26 |
| Hosted foundation | 11 |
| Message parity | 6 |
| Channel/group parity | 12 |
| Account/server parity | 11 |
| Saved snippets | 5 |

The offline parity rule runs alongside those tests. It reports 77 helpers, 68
canonical operations, and 294 canonical wire parameters. Its guarantees are
deliberately structural: frozen inventory equality, public declaration
presence, operation identity, parameter accounting, source evidence tokens,
route components, HTTP method tokens, and absence of selected obsolete public
declarations. It does not prove serialization semantics, response fidelity,
authorization behavior, retry behavior, or server compatibility. Those claims
come from the OCaml request/response tests and the live Docker scenarios.

**Offline result:** 107 cases and the frozen parity audit pass.

**Live Zulip 12.2 result (2026-09-06):** all 19 API/runtime scenarios passed
against a fresh Docker stack, including expanded message, channel/group,
account/settings, server-restart and queue-recovery, event-projection, and
bot-runtime scenarios, plus saved-snippet operations using a bot account.
The API/runtime suite took about 36 seconds after startup.
It also verifies date and selected-ID history queries and topic-deletion
completion.
The separate tutorial CLI check also passed: private profile import, read-only
preflight, explicit HTTP opt-in, live echo delivery, SIGTERM shutdown, and a
failing exit for an invalid API key. Run both with
`test/integration/zulip.sh run`.

**Public hosted check (2026-09-06):** anonymous HTTPS GETs to EEG's public-web
message endpoint fetched three messages each from `Blogs` and `Tessera` in
two responses. Fetch/httpz and `Zulip.Message.jsont` handled all six message
objects.
The site reported feature level 507. Authenticated hosted checks remain pending;
see `HOSTED_BOTS.md` for the scope and setup.

## Python SDK helper mapping

This table is rendered from `test/parity/bindings.json`; that file is the
machine-readable source of truth for the binding, operation, parameter evidence,
result shape, and family notes. Aliases map to one canonical OCaml operation
rather than adding compatibility-only names.

| Python helper | OCaml binding | Canonical operation |
| --- | --- | --- |
| `ensure_session` | `Transport.v` | transport construction |
| `get_user_agent` | `Client.user_agent` | client configuration |
| `do_api_query` | `Client.request_json` | generic transport utility |
| `call_endpoint` | `Client.request_json` | generic endpoint utility |
| `call_on_each_event` | `Event_queue.iter` | callback collector |
| `call_on_each_message` | `Event_queue.iter_messages` | message callback collector |
| `get_messages` | `Zulip_eio.Messages.get_messages` | `GET /messages` |
| `check_messages_match_narrow` | `Zulip_eio.Messages.check_messages_match_narrow_detailed` | `GET /messages/matches_narrow` |
| `get_raw_message` | `Zulip_eio.Messages.get_raw` | `GET /messages/{message_id}` |
| `send_message` | `Zulip_eio.Messages.send_detailed` | `POST /messages` |
| `upload_file` | `Zulip_eio.Attachments.upload_file_detailed` | `POST /user_uploads` |
| `get_attachments` | `Attachments.list` | `GET /attachments` |
| `update_message` | `Zulip_eio.Messages.edit_detailed` | `PATCH /messages/{message_id}` |
| `delete_message` | `Zulip_eio.Messages.delete` | `DELETE /messages/{message_id}` |
| `update_message_flags` | `Zulip_eio.Messages.update_flags_detailed` | `POST /messages/flags` |
| `mark_all_as_read` | `Zulip_eio.Messages.mark_all_as_read` | `POST /mark_all_as_read` |
| `mark_stream_as_read` | `Zulip_eio.Messages.mark_channel_as_read` | `POST /mark_stream_as_read` |
| `mark_topic_as_read` | `Zulip_eio.Messages.mark_topic_as_read` | `POST /mark_topic_as_read` |
| `get_message_history` | `Zulip_eio.Messages.get_history` | `GET /messages/{message_id}/history` |
| `add_reaction` | `Zulip_eio.Messages.add_reaction` | `POST /messages/{message_id}/reactions` |
| `remove_reaction` | `Zulip_eio.Messages.remove_reaction` | `DELETE /messages/{message_id}/reactions` |
| `get_realm_emoji` | `Zulip_eio.Server.get_emoji` | `GET /realm/emoji` |
| `upload_custom_emoji` | `Zulip_eio.Server.upload_emoji / upload_emoji_stream` | `POST /realm/emoji/{emoji_name}` |
| `delete_custom_emoji` | `Zulip_eio.Server.deactivate_emoji` | `DELETE /realm/emoji/{emoji_name}` |
| `get_realm_linkifiers` | `Zulip_eio.Server.get_linkifiers` | `GET /realm/linkifiers` |
| `add_realm_filter` | `Zulip_eio.Server.add_linkifier` | `POST /realm/filters` |
| `remove_realm_filter` | `Zulip_eio.Server.delete_linkifier` | `DELETE /realm/filters/{filter_id}` |
| `get_realm_profile_fields` | `Zulip_eio.Server.get_profile_fields` | `GET /realm/profile_fields` |
| `create_realm_profile_field` | `Zulip_eio.Server.create_profile_field` | `POST /realm/profile_fields` |
| `remove_realm_profile_field` | `Zulip_eio.Server.delete_profile_field` | `DELETE /realm/profile_fields/{field_id}` |
| `reorder_realm_profile_fields` | `Zulip_eio.Server.reorder_profile_fields` | `PATCH /realm/profile_fields` |
| `update_realm_profile_field` | `Zulip_eio.Server.update_profile_field` | `PATCH /realm/profile_fields/{field_id}` |
| `get_server_settings` | `Zulip_eio.Server.get_settings / get_settings_json` | `GET /server_settings` |
| `get_events` | `Event_queue.get_events` | `GET /events` |
| `register` | `Event_queue.register` | `POST /register` |
| `deregister` | `Event_queue.delete` | `DELETE /events` |
| `get_profile` | `Zulip_eio.Users.me` | `GET /users/me` |
| `get_user_presence` | `Zulip_eio.Presence.get_user_by_email / get_user_by_email_detailed` | `GET /users/{user_id_or_email}/presence` |
| `get_realm_presence` | `Zulip_eio.Presence.get_all / get_all_detailed` | `GET /realm/presence` |
| `update_presence` | `Zulip_eio.Presence.update` | `POST /users/me/presence` |
| `get_streams` | `Channels.list_all` | `GET /streams` |
| `update_stream` | `Channels.update` | `PATCH /streams/{stream_id}` |
| `delete_stream` | `Channels.delete (alias Channels.archive)` | `DELETE /streams/{stream_id}` |
| `add_default_stream` | `Channels.add_default` | `POST /default_streams` |
| `get_user_by_id` | `Zulip_eio.Users.get_by_id` | `GET /users/{user_id}` |
| `deactivate_user_by_id` | `Zulip_eio.Users.deactivate` | `DELETE /users/{user_id}` |
| `reactivate_user_by_id` | `Zulip_eio.Users.reactivate` | `POST /users/{user_id}/reactivate` |
| `update_user_by_id` | `Zulip_eio.Users.update` | `PATCH /users/{user_id}` |
| `get_users` | `Zulip_eio.Users.list_all` | `GET /users` |
| `get_members` | `Zulip_eio.Users.list_all` | `GET /users` |
| `get_alert_words` | `Zulip_eio.Users.get_alert_words` | `GET /users/me/alert_words` |
| `add_alert_words` | `Zulip_eio.Users.add_alert_words` | `POST /users/me/alert_words` |
| `remove_alert_words` | `Zulip_eio.Users.remove_alert_words` | `DELETE /users/me/alert_words` |
| `get_subscriptions` | `Channels.get_subscriptions_with` | `GET /users/me/subscriptions` |
| `list_subscriptions` | `Channels.get_subscriptions_with` | `GET /users/me/subscriptions` |
| `add_subscriptions` | `Channels.subscribe` | `POST /users/me/subscriptions` |
| `remove_subscriptions` | `Channels.unsubscribe` | `DELETE /users/me/subscriptions` |
| `get_subscription_status` | `Channels.get_subscription_status` | `GET /users/{user_id}/subscriptions/{stream_id}` |
| `mute_topic` | `Channels.set_topic_mute / Channels.set_topic_visibility` | `POST /user_topics` |
| `update_subscription_settings` | `Channels.update_subscription_properties` | `POST /users/me/subscriptions/properties` |
| `update_notification_settings` | `Settings.update` | `PATCH /settings` |
| `get_stream_id` | `Channels.get_id` | `GET /get_stream_id` |
| `get_stream_topics` | `Channels.get_topics` | `GET /users/me/{stream_id}/topics` |
| `get_stream_email_address` | `Channels.get_email_address` | `GET /streams/{stream_id}/email_address` |
| `get_user_groups` | `User_group.list_all` | `GET /user_groups` |
| `create_user_group` | `User_group.create` | `POST /user_groups/create` |
| `update_user_group` | `User_group.update` | `PATCH /user_groups/{user_group_id}` |
| `remove_user_group` | `User_group.delete` | `POST /user_groups/{user_group_id}/deactivate` |
| `update_user_group_members` | `User_group.update_members` | `POST /user_groups/{user_group_id}/members` |
| `get_subscribers` | `Channels.get_subscribers_by_name` | `GET /streams/{stream_id}/members` |
| `render_message` | `Zulip_eio.Messages.render` | `POST /messages/render` |
| `create_user` | `Zulip_eio.Users.create / create_detailed` | `POST /users` |
| `update_storage` | `Zulip_eio.Bot_storage.set` | `PUT /bot_storage` |
| `get_storage` | `Zulip_eio.Bot_storage.get` | `GET /bot_storage` |
| `remove_storage` | `Zulip_eio.Bot_storage.remove` | `DELETE /bot_storage` |
| `set_typing_status` | `Zulip_eio.Typing.set / set_dm / set_channel` | `POST /typing` |
| `move_topic` | `Zulip_eio.Messages.move_topic` | `PATCH /messages/{message_id}` |

## Current 12.2 operations beyond the Python client

The hosted-bot work adds current server capabilities that are not helpers in the
pinned Python client:

- Messages and files: flag updates by narrow, read receipts, message reporting,
  attachment deletion, temporary upload URLs, thumbnail status, and complete
  create/list/update/delete support for scheduled messages and drafts, plus
  reminder creation, listing and deletion. Saved snippets have list, create,
  edit and delete operations through `Saved_snippets`, with typed identifiers,
  title, Markdown content and creation time, plus response extensions.
  Message retrieval ranges support ISO 8601 date anchors, while selected-ID
  queries form a separate request mode without range parameters.
- Channels and topics: dedicated channel creation, unarchive and explicit
  archive state, removal from default channels, subscription updates, user
  channel queries, per-channel subscription properties, topic deletion, and all
  current topic visibility policies.
- Groups and organization structure: direct-member/direct-subgroup queries,
  subgroup updates, and channel-folder create/list/reorder/update/archive
  operations.
- Accounts and metadata: lookup by email, profile-data updates/removal, avatar
  upload/removal, self-deactivation, user status administration, mute/unmute,
  linkifier update/reorder, attachment deletion, and message-edit typing.
- Settings and registration: the complete documented `PATCH /settings` key set,
  typed initial-state access, full registration controls, and callback event and
  message collectors with recovery and explicit acknowledgement.

These additions have focused request/response fixtures. The expanded live
scenarios exercise representative operations from each family, as described in
`test/integration/README.md`; they do not exhaust every parameter combination.

Zulip 12.2 reports attachment creation times as Unix seconds.
`Channels.delete_topic` exposes the server's `Complete` or `Incomplete`
batch result and does not retry an incomplete deletion automatically.
`Messages.get_messages` accepts either a bounded history range, including a
`Date` anchor, or a selected-ID query without range parameters.

## Intentional differences from Python

The OCaml construction API separates concerns that Python combines in
`Client.__init__`: `Auth` holds the origin and Basic credentials, `Profile`
handles XDG JSON profiles and zuliprc import, `Transport` owns Fetch/TLS and GET
retry policy, and `Client.create` sets deadlines, response bounds, cleartext
policy, and user-agent identity. Custom trust stores and client certificates are
provided by injecting a Fetch HTTPS connector. Mutating requests are not
automatically replayed by `Transport.v`.

Python's `verbose` constructor flag configures its logging. It has
no direct boolean counterpart; OCaml applications choose their own
logging/reporting policy, while structured `Error.t` values retain transport,
HTTP, API, JSON, timeout, and indeterminate-send distinctions.

The API uses current channel terminology in OCaml names while retaining the
server's `/streams` wire paths where Zulip 12.2 specifies them. Python aliases
such as `get_members` and `list_subscriptions` map to one canonical binding.
Operations live in their resource modules rather than being duplicated through
`Messages`. IDs for users, channels, messages, groups, folders, attachments,
drafts, scheduled messages, reminders, snippets, and other server objects are
nominally distinct modules under `Zulip.Id`.
Version-compatibility branches for older servers are omitted. The
`ZulipStream` writable-object convenience maps to ordinary `Messages.send_*`
functions.

Older endpoint-shaped helpers are not kept as public compatibility shims. The
current surface obtains the user's settings and muted-user snapshot from queue
registration, updates settings through `PATCH /settings`, uses the modern topic
visibility operation, and implements subgroup membership through supported
collection/query operations. Message-pointer helpers are likewise absent from
the current API.

## REST coverage outside this target

The broader Zulip 12.2 REST API still contains specialist and administrative
families beyond this SDK-focused target. Significant remaining families include:

- API-key bootstrap, JWT/development authentication, and API-key regeneration;
- invitations, reusable invite links, organization domains, export/consent
  administration, and organization user-setting defaults;
- navigation views;
- mobile, APNs, FCM, remote-push, and client-device registration/testing;
- bot API-key administration, code playgrounds, and custom welcome-message
  testing;
- video-call provider endpoints; and
- specialist real-time, outgoing-webhook, and REST-error demonstration
  endpoints.

`Client.request_json` makes such operations reachable when needed, but generic
reachability is not counted as typed endpoint coverage or as Python SDK parity.
Adding these families requires native request/response models and tests before
they should be listed as covered.

## Audit sources

The frozen inputs and their maintenance procedure are documented in
`test/parity/README.md`. Family-level parameter and result notes live in:

- `test/messages_parity/coverage.json`
- `test/channels_parity/coverage.json`
- `test/accounts_parity/coverage.json`
