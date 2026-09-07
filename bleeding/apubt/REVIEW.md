# apubt implementation review

Reviewed 2026-09-07: protocol codecs, Fetch integration, signing, discovery,
delivery helpers, pagination, OAuth, session storage, CLI, and package metadata.
The initial audit and Fetch integration changes are committed as `6a9c67927`.
The follow-up addresses all twelve findings from that audit.

## Findings and fixes

| Priority | Finding | Result |
| --- | --- | --- |
| P1 | Follower collection recipients were decoded as actors; notes could not reach followers. | Expand URI/embedded collections with depth/page/item budgets, deduplicate inboxes, exclude the sender, and propagate delivery failures. |
| P1 | Undo helpers invented or substituted IDs rather than identifying the original activity. | Require the original Follow/Like/Announce, validate its actor/type, and embed it in Undo. Accept/Reject also embed the received Follow. |
| P1 | OAuth actions reused a remote status's numeric ID on the local instance. | Resolve status/account URLs on the authenticated instance through search; replies use the resolved local ID. Add `read:search` scope and stop before mutation if resolution fails. |
| P1 | Embedded activities, Document attachments, URI followers, and inline pages failed decoding. | Introduce one lossless URI/embedded JSON reference type with typed projection; use it for activity objects, attachments, tags, and page pointers. Followers/following accept actor references. |
| P2 | Generated activities lacked durable identity and persistence. | Allocate IDs using an injected generator or 128 cryptographic random bits. Require a persistence callback before delivery. The CLI atomically stores activities/objects; `Outbox.deliver` supports retries with the same ID. |
| P2 | Signing covered POST only and only RFC 9421. | Sign GET and POST via Fetch middleware; offer explicit RFC 9421 or legacy Cavage format, with canonical method/target/digest coverage and RSA-SHA256. |
| P2 | Federation discarded content warnings and ignored conflicting visibility flags. | Pass summary/sensitive through note APIs and both CLI backends; reject conflicting public/followers flags. |
| P2 | Partial credential overrides silently selected saved keys. | Resolve overrides individually, require explicit credentials when changing actors, and validate selected key ID/owner/public key against the actor. Reject non-RSA setup keys. |
| P2 | Profile paths were unchecked and credentials were truncated in place. | Validate path components, honour current profile and XDG configuration, reject symlink directories, sync/rename private temporary files, and distinguish malformed from missing sessions. |
| P2 | Updates lost metadata and extensions; Tombstones used the wrong timestamp. | Preserve the Object representation and unknown members when changing content; check authorship for Update/Delete and set Tombstone `deleted`. |
| P2 | Discovery dropped subscription templates and guessed endpoints. | Preserve WebFinger's top-level template extension, match complete NodeInfo relations, and require an advertised shared inbox. Propagate authorization/transport errors. |
| P2 | Distinct pagination URLs could run forever; HTTP-date Retry-After was lost. | Add configurable page/item budgets, retain cycle detection, and parse HTTP dates against the injected clock. Keep transport restrictions/retries in Fetch. |

The protocol choices follow [ActivityPub delivery](https://www.w3.org/TR/activitypub/#delivery),
[Undo semantics](https://www.w3.org/TR/activitypub/#undo-activity-inbox), and the
[ActivityStreams data model](https://www.w3.org/TR/activitystreams-core/).
Interoperability choices use [Mastodon's signature profile](https://docs.joinmastodon.org/spec/security/)
and [search API](https://docs.joinmastodon.org/methods/search/).

## Fetch and simplicity

`Apubt.of_fetch` accepts a caller-owned capability; `create` is a curl convenience
constructor. JSON uses `Fetch.Json.v`, `Fetch.decode`, and `Fetch.encode`, with
16 MiB response limits by default, bounded nesting, and 64 KiB diagnostic limits.
Response ownership stays in `Fetch.with_response`. OAuth keeps origin-scoped
bearer credentials and typed form encoding. Transport errors use the public
exception/result APIs; cancellation propagates. Writes reject redirects so private
bodies and credentials are not replayed at another target.

Signing belongs to Fetch's middleware rather than a parallel apubt signing path.
Fetch's default RSA-PSS behavior remains unchanged for existing consumers; apubt
selects RSA-SHA256 explicitly. Collection traversal shares one bounded fold.
The reference codec avoids a mutually recursive hierarchy while preserving
embedded extensions and allowing callers to choose a typed interpretation.

## API migration and limits

- `Object_ref.Object` becomes `Reference.Embedded`; project with
  `Reference.decode Object.jsont` or `Activity.jsont`. Construct typed embedded
  values with `Reference.of_value` (`Object_ref.obj` remains a convenience).
- Collection pointers take `Reference.t`; followers/following return actor
  references. URI and inline representations are accepted.
- `Actor.unfollow ~follow`, `Outbox.unlike ~like`, and
  `Outbox.unannounce ~announce` take the original activity.
- Generated writes require `~persist`; initialize Mirage Crypto randomness when
  using the default ID generator or RSA signing. Persist before delivery and
  serve generated IDs from the application's server. The CLI saves documents but
  does not provide an HTTP server, inbox processing, or a durable delivery queue.
- Delivery is sequential and can partially succeed. Retain the persisted
  activity and retry with `Outbox.deliver`; application policy controls retries.
- Server-side use should supply a restricted Fetch capability with backend
  address policy for discovery and recipient URLs. Successful local tests do not
  establish interoperability with every peer or support for arbitrary JSON-LD.

## Validation

```sh
opam exec --switch=5.2.0+ox -- dune build --profile release \
  bleeding/apubt/bin/apub.exe @bleeding/apubt/runtest \
  @bleeding/fetch/signature/runtest bleeding/apubt/apubt.opam
opam exec --switch=5.2.0+ox -- python3 bleeding/apubt/test/curl_cli_smoke.py
```

Five apubt test executables cover response limits/media types, resource release,
transport errors, cancellation, signing and query tampering, OAuth redirect and
credential scope, remote/local reply IDs, discovery, bounded pagination, follower
expansion, original-activity Undo, metadata preservation, persistence, and profile
safety. The 26 existing Fetch signature unit/vector cases pass. The loopback smoke
test uses the real curl-backed CLI and independently verifies RFC 9421 and Cavage
GET/POST wire signatures with OpenSSL, plus persisted activity identity.

The dev-profile build remains blocked by an unrelated existing fatal unused-value
warning at `bleeding/httpz/uri/uri_template.ml:66`. Release builds emit existing
httpz `unsafe_multidomain` alerts; isolated session tests also warn about setting
their test configuration environment. No live federation, standalone opam
installation, or full-monorepo test run was performed.
