# apubt implementation review

Reviewed 2026-09-07: protocol codecs, Fetch integration, signing, discovery,
delivery helpers, pagination, OAuth, session storage, CLI, and package metadata.

The Fetch integration is substantially simpler after the fixes below. The
federation helpers still have correctness gaps that prevent treating this as a
complete ActivityPub client. The open findings below describe the resulting
worktree, not bugs that were already fixed during the review.

## Open findings, in priority order

1. **P1 — Public and followers-only notes cannot reach follower collections.**
   `lib/client/apubt.ml:530` decodes every non-public recipient as an actor.
   `public_note` and `followers_only_note` supply a followers collection URI,
   which therefore fails actor decoding. Before this review the exception was
   swallowed and the call reported success without delivery; it now raises.
   Implement bounded collection expansion and actor/reference decoding, or
   submit to the actor's authenticated outbox and let its server deliver.
   Collection expansion is part of [ActivityPub delivery](https://www.w3.org/TR/activitypub/#delivery).

2. **P1 — Undo operations reference activities that never existed.**
   `Actor.unfollow` (`lib/client/apubt.ml:399`) substitutes the actor's URI for
   the original Follow; `Outbox.unlike` (645) and `unannounce` (707) generate new
   Like/Announce IDs and immediately undo them. None can identify the original
   activity. Change these APIs to accept the original activity or its ID;
   applications must retain it. `accept_follow` and `reject_follow` also
   substitute an actor URI when the supplied Follow lacks an ID. Reject that
   input or embed the actual Follow. [Undo refers to a previous activity](https://www.w3.org/TR/activitypub/#undo-activity-inbox).

3. **P1 — OAuth likes and boosts can target the wrong local status.**
   `bin/apub.ml:541` and 606 extract the last numeric path segment of a remote
   URL, then send it to the logged-in instance. That number is not the local
   instance's identifier. If a different local status has that ID, the command
   acts on it. `--reply-to` similarly passes a URI as `in_reply_to_id` (393).
   Resolve status URLs on the authenticated instance with
   [`GET /api/v2/search?resolve=true&type=statuses`](https://docs.joinmastodon.org/methods/search/)
   and use the returned ID. Adjust OAuth scopes for search and test different
   remote/local IDs. Following a URI also needs resolution: the OAuth branch
   currently passes it to an account-handle lookup.

4. **P1 — Common ActivityStreams values cannot be represented by the codecs.**
   `Object_ref` (`lib/proto/apubt_proto.ml:987`) accepts only URI or ordinary
   Object, so an embedded Follow in an Accept or Undo fails on its `type`.
   `Object.attachment` decodes only Link/URI values: a normal
   `{"type":"Document","url":"https://example.com/photo.jpg"}` fails
   because `Link` requires `href`. Followers/following collections require
   full actors (`lib/client/apubt.ml:371`) and reject URI items. Collection page
   pointers accept only URI strings (`lib/proto/apubt_proto.ml:1352`, 1434),
   rejecting inline pages. These need a coherent recursive reference model;
   adding independent special cases will duplicate the protocol further.
   The [ActivityStreams data model](https://www.w3.org/TR/activitystreams-core/)
   supports object references and collection representations beyond this subset.

5. **P2 — Generated activities have no persistence or reliable identity policy.**
   `Actor.follow` (386), Accept, Reject, and Unfollow omit IDs. The CLI no longer
   crashes when a successful Follow lacks one, but subsequent references still
   cannot identify it. `Outbox.generate_uri` (516) combines wall-clock seconds
   with only 24 bits from the default `Random` state. Separate processes can
   repeat the same initial sequence. Introduce caller-supplied IDs or an
   injected secure generator, and persist the object/activity before delivery.
   The helpers currently neither store these URIs nor post to a local outbox;
   a successful inbox response alone cannot make the generated object available
   for a later fetch.

6. **P2 — Signature coverage is limited to POST and RFC 9421 peers.**
   The corrected POST profile uses RSA-SHA256 and covers `@target-uri` and
   `content-digest`. GET remains unsigned, so authenticated-fetch instances
   cannot supply actors or objects. Older peers may require draft signatures.
   Add explicit protocol/signing policies if that compatibility is required;
   do not send POST's body-dependent component set on an empty GET.
   [Mastodon's signing requirements](https://docs.joinmastodon.org/spec/security/)
   describe both its RFC 9421 profile and these compatibility constraints.

7. **P2 — Federation posts discard content-warning options.**
   `bin/apub.ml:408` binds `_summary` and passes neither it nor `sensitive` to
   the note helpers. OAuth also discards a supplied summary unless `--sensitive`
   is set (391). Pass both fields through the public/follower helper APIs and
   verify the resulting object/form. The `--public` flag is currently ignored.

8. **P2 — Partial credential overrides can silently select a different key.**
   `resolve_credentials` (`bin/apub.ml:295`) honours explicit credentials only
   when all three flags are supplied. Its `_, _, Some actor` branch ignores a
   supplied key file or key ID and loads the saved pair instead, potentially
   signing for the wrong actor. Resolve each override explicitly, then validate
   the complete tuple and actor identity before network access. Auth setup also
   accepts non-RSA PEM keys although `Signing.from_pem` rejects them later.

9. **P2 — Profile paths are not validated; persistence is not atomic.**
   `lib/auth/apub_auth_session.ml:76` appends the profile directly as a path.
   Reject empty names, `.`/`..`, separators, and absolute paths, including names
   derived from remote account data. `config_dir` defaults to `default`, contrary
   to its documented current-profile behavior. Saves (100, 155) truncate in
   place: interruption can destroy credentials, and the create permission does
   not tighten an existing file's mode. Use a private temporary file and rename,
   and validate stored configuration before using its profile name. Missing and
   malformed session files should be distinguishable.

10. **P2 — Update reconstructs and loses existing object fields.**
    `Outbox.update_note` (`lib/client/apubt.ml:772`) rebuilds a Note while omitting
    attachments, tags, audience and other fields. The codec also discards unknown
    extension members, so decode/re-encode cannot preserve them. Define update
    semantics and preserve the original representation/unknown fields instead
    of advertising metadata preservation. Delete builds a Tombstone with
    `published`, without using its available `deleted` field.

11. **P2 — Discovery can return incomplete or misleading results.**
    `jrd_of_webfinger` (193) looks for a subscription `template` in link
    properties, while the underlying WebFinger codec drops the top-level
    template extension. Thus `subscribe_template` cannot recover the ordinary
    wire value. Share or extend the WebFinger link representation rather than
    maintaining two lossy JRD models. NodeInfo selection accepts any relation
    ending in `/schema/2.0` or `/schema/2.1`; match the complete schema relation.
    Shared-inbox discovery still guesses `/inbox` after any ordinary discovery
    failure, including authorization/transport errors.

12. **P2 — Remaining resource/policy boundaries need caller control.**
    Pagination now rejects repeated page URIs, but an endless stream of distinct
    page URLs remains unbounded. Add an optional page/item budget. Discovery and
    recipient resolution follow supplied URLs; server-side consumers should pass
    an appropriately restricted Fetch capability and enforce resolved-address
    policy at the backend. `Rate_limited` currently retains numeric Retry-After
    only, discarding HTTP dates. Keep retry policy in Fetch rather than creating
    another retry loop in apubt.

## Changes made during this review

- Added `Apubt.of_fetch`, keeping `create` as a curl convenience constructor.
  Consumers can supply restricted, configured, or mock Fetch clients.
- Replaced manual response buffering and JSON parsing with `Fetch.Json.v`,
  `Fetch.decode`, and `Fetch.encode`. Core response sizes are configurable
  (16 MiB default); JSON nesting uses Fetch's limit; error diagnostics are capped
  at 64 KiB. WebFinger retains the upstream account parser, URL builder, and JRD
  codec while using the same bounded response path.
- Retained `Fetch.with_response` ownership, explicit HTTP status handling, typed
  form encoding, and origin-scoped bearer credentials. Mapped transport errors
  into the existing exception/result APIs and allowed cancellation to propagate.
- Replaced manual signing with Fetch's signing middleware. Added the backwards
  compatible `Fetch_signature.config ?algorithm` option so apubt can choose
  RSA-SHA256 without changing Fetch's default RSA-PSS algorithm. Signatures now
  cover the canonical complete URI, including its query.
- Disabled redirect following for ActivityPub POSTs and OAuth/API writes.
  This prevents method-changing success responses and forwarding secret/private
  form or activity bodies on 307/308 redirects.
- Initialized CLI randomness for RSA blinding and removed its post-Follow
  `Option.get` crash.
- Removed silent recipient/delivery error suppression and the empty follower URI
  fallback. Simplified collection iteration to one fold with cycle detection.
- Fixed `Image_ref.Link` encoding/decoding, accepted singleton addressing fields,
  and searched all WebFinger self links for an ActivityPub representation.
- Added missing direct package dependencies and corrected misleading API/README
  examples and delivery descriptions.

Intentional behavior changes: malformed/missing response Content-Type is now an
error, response bodies and JSON depth are bounded, writes reject redirects,
delivery errors propagate, and RSA POST signatures use the interoperable profile.
Existing Fetch signature callers retain their default algorithm.

## Validation

Passed from the repository root using the requested switch:

```sh
opam exec --switch=5.2.0+ox -- dune build --profile release \
  bleeding/apubt/bin/apub.exe @bleeding/apubt/runtest \
  @bleeding/fetch/signature/runtest bleeding/apubt/apubt.opam
opam exec --switch=5.2.0+ox -- python3 bleeding/apubt/test/curl_cli_smoke.py
```

The three apubt test executables cover JSON limits/media types, resource release,
transport errors, cancellation, canonical target signing and query tampering,
OAuth redirect confidentiality, credential scoping, discovery, pagination cycles,
delivery failures and protocol round trips. All 26 existing Fetch signature
unit/vector cases pass. The loopback smoke test exercises the real curl-backed
CLI Follow path and verifies the wire body digest and RSA signature independently
with OpenSSL. It uses temporary credentials and no external service.

The default dev-profile build is blocked by an existing unrelated fatal
unused-value warning for `is_alpha` at `bleeding/httpz/uri/uri_template.ml:66`.
Release builds also emit existing `unsafe_multidomain` alerts in httpz's range
module. This review did not alter either file. No live federation, standalone
opam installation, or full-monorepo test run was performed.
