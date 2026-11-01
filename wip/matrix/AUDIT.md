# ocaml-matrix — security & correctness audit vs matrix-rust-sdk

- Date (UTC): 2026-09-05
- OCaml tree: `/home/avsm2/src/git/knot/ocaml-matrix` (`lib/`, `bin/`, `example/`, `test/`)
- Rust reference: `../matrix-rust-sdk` (`crates/matrix-sdk`, `matrix-sdk-crypto`, `matrix-sdk-base`, `matrix-sdk-sqlite`, `matrix-sdk-store-encryption`, `matrix-sdk-ui`, `bindings/matrix-sdk-ffi`)
- Method: read `lib/matrix_client` (~93 modules, ~37k lines), `lib/matrix_proto`, `lib/matrix_eio`, `lib/matrix_ui`, plus `test/` oracle/fixtures; `grep` for `assert false|failwith|Obj.magic|Marshal|TODO|ignore`; compared crypto paths against `matrix-sdk-crypto` and vodozemac semantics; checked store/auth/media/UI trust boundaries. No code changed.
- Existing bookkeeping (not re-verified blindly): `PARITY_INVENTORY.md`, `PARITY_PLAN.md`, `PORT_PLAN.md`, `STATUS.md`, `TODO.md`.

> **Disposition note (2026-09-05).** This file preserves the findings as they
> were observed and is not an active checklist. Commits
> `304e677ec8c6d8caf607f373a23fc3ba9f095f5f`,
> `3d12d35c531884296565f671def6670c4e1b6282`, and its adversarial correction
> `d2c230c122d1c176b5fe5b430f41735326397927`, followed by the granular
> queue/import and structured-Eio-context rereview in
> `a76431516fe6982259eb3eb29b3a1db696f05a54`, remediate or recheck every
> concrete finding except H1 (at-rest encryption) and S7 (packaging), which were
> explicitly excluded, and H2, whose remaining reliable-zeroisation work needs
> a foreign/locked-memory design rather than cosmetic `Bytes.fill` calls. The
> authoritative per-item disposition and pickup specification is the
> "Security/correctness audit follow-up" section of `TODO.md`.

## TL;DR

The translation is unusually careful where it matters most: Olm/Megolm wire formats, SAS commitment + MAC negotiation, canonical JSON, duplicate-member rejection, ECIES/rendezvous, PKCE/OAuth, atomic 0600 stores, and encrypted-attachment hash-before-release are all done right, with vodozemac-oracle interop backing it (`test/test_olm.ml`, `test/vodozemac-oracle`, `test/fixtures/olm`).

The residual risk is concentrated in four systemic gaps, not scattered typos:

1. **No at-rest encryption** — stores are plaintext JSON (`H1`).
2. **No secret zeroing** — OCaml strings retain key material (`H2`).
3. **Crash-on-input paths** — `assert false`/`failwith`/`invalid_arg` reachable from decodable data (`H3`).
4. **Incomplete log redaction** — E2EE secrets outside the redaction allowlist (`H4`).

Everything else is medium/low or simplification. Details below are ordered by severity, each with location, Rust reference, impact, and fix.

---

## What is good (keep it)

- `lib/matrix_client/olm_primitives.ml`: constant-time `Eqaf.equal` for MACs, explicit varint caps (`shift > 55`), PKCS#7 full-byte check, `Cipher.verify_mac8`.
- `lib/matrix_client/megolm.ml`: version separation (v1 export / v2 session), 32-bit index masking, signature-before-ratchet, `signing_key_verified` tracked separately from `claimed_ed25519`.
- `lib/matrix_client/olm_session.ml`: `max_message_gap 2000`, `max_message_keys 40`, `max_receiving_chains 5` match vodozemac/Rust constants; skipped-key ring (`push_skipped`/`keep_last`) present; scratch-copy advance with commit-on-success.
- `lib/matrix_client/verification_sas.ml`: commitment `sha256(pubkey || canonical(start))` checked for the starter side; accept constrained to offered MAC methods; `ct_equal` MAC compare; libolm-bug `mac_base64_libolm` isolated to v1 negotiation only.
- `lib/matrix_proto/matrix_signed_json.ml` + `lib/matrix_client/client.ml`: canonical JSON validates first, sorts members, rejects non-finite/fractional/out-of-`2^53` numbers, normalises `-0`; `json_for_signing` validates *before* stripping `signatures`/`unsigned`; client `decode_json` runs bounded decode → duplicate-member rejection → typed decode (defeats Jsont last-member-wins ambiguity).
- `lib/matrix_proto/matrix_id.ml`: historical-vs-spec split (`is_spec_conformant`), opaque room-ID handling with server-name fallback, IPv6/port server-name grammar over `domain-name`+`ipaddr`.
- `lib/matrix_client/encrypted_attachment.ml` + `lib/matrix_client/media.ml`: streaming decrypt hashes ciphertext and only releases plaintext after `finish` verifies (`decrypt_chunks` calls `on_chunk` post-verify; media spool does download→verify→emit, three passes).
- `lib/matrix_client/qr_login_ecies.ml` + `qr_login_rendezvous.ml`: role-separated HKDF info strings, counter-exhaustion errors, strict RFC 9110 ETag wire check, `min_expiry` + 404/410 terminal handling.
- `lib/matrix_client/oauth.ml`: S256-only PKCE, verifier length/charset checks, `allow_insecure` explicit opt-in, loopback `http://127.0.0.1/callback` documented per RFC 8252 §7.3, no token/verifier logging (line 8 contract).
- `lib/matrix_client/profile_store.ml`: 0700 dirs, 0600 exclusive-create + `fchmod` + `fsync` + same-directory rename; `store.ml`/`crypto_store.ml` share it.
- `lib/matrix_ui/presentation.ml`: real allowlist sanitizer — element allowlist, `safe_href` scheme check, `mxc://`-only `img src`, suppressed `script/style/iframe/object/embed/mx-reply` subtrees.

---

## High severity

### H1 — No StoreCipher: crypto stores are plaintext JSON on disk

- Where: `lib/matrix_client/store.ml`, `crypto_store.ml`, `profile_store.ml`, `session.ml`, `media_store.ml`.
- Rust ref: `crates/matrix-sdk-store-encryption` (`StoreCipher::new/export/encrypt_value/decrypt_value`, ChaCha + KDF rounds); `matrix-sdk-sqlite` encrypted stores.
- Finding: OCaml persists Olm account pickles, Olm/Megolm sessions, backup keys, `dehydrated_pickle_key`, `access_token`/`refresh_token`, and room keys as indented JSON via `Profile_store.save`/`atomic_write`. Permissions (0600) and crash safety are correct, but there is no passphrase/key-based at-rest encryption. Any backup, image, or stolen laptop yields long-term secrets. `PARITY_PLAN.md` already admits "no store encryption… SQLite defaults to ciphertext-only persistence" for UI cache — the base SDK path is still plaintext.
- Impact: confidentiality of E2EE identity depends entirely on filesystem DAC. Higher blast radius than TLS or memory issues.
- Fix: port `StoreCipher` (or document explicit non-goal): KDF (Argon2id or PBKDF2 with high rounds) → ChaCha20-Poly1305/XChaCha envelope for `session_file`, `olm_sessions_file`, `megolm_*`, crypto store; add `export/import` for migration; wire through `Profile_store.save/load`. At minimum, add a `STATUS.md` warning that profiles are plaintext.

### H2 — Secrets are never wiped from the OCaml heap

- Where: `lib/matrix_client/crypto_key.ml` (`Ed25519.Private {seed; expanded}`, `Curve25519.Secret`), `megolm.ml` (`Ratchet.data: Bytes.t`), `secret_storage.ml`, `backup.ml`, `recovery.ml`, `room_key_export.ml`, `session_pickle.ml`, `qr_login_ecies.ml` (`mutable secret_key`).
- Rust ref: `zeroize`/`Zeroizing` throughout `matrix-sdk-crypto` and `store-encryption`.
- Finding: `Random.generate` correctly wipes its transient `Cstruct` buffer, but `Cstruct.to_string` copies into an immutable OCaml `string` that is never wiped; `Ratchet`, seeds, expanded Ed25519 keys, Megolm/MM keys, recovery/backup keys, and ECIES ephemerals all live in GC-managed strings/bytes with no `memset`. Core dumps, `/proc` snapshots, and heap inspection retain them indefinitely.
- Impact: raises value of any memory-disclosure or disk-image (combined with H1) bug; breaks "delete session = forget key" expectations.
- Fix: decide policy explicitly. Cheap wins: use `Bytes` + `Bytes.fill 0` for transient copies, wipe `Ratchet.data` on drop/rotation, wipe ECIES `secret_key` after `derive`; document that long-lived OCaml strings cannot be reliably zeroed and recommend process isolation + H1. Do not claim "wiped" where only the temp buffer was.

### H3 — `assert false` / `failwith` / `invalid_arg` on attacker-influenced paths (remote DoS)

Confirmed instances (`grep assert\ false`):

- `lib/matrix_client/encrypted_attachment.ml:258,261,317,338` — `raw_key`/`raw_iv`/`expected` decode after `validate`. Currently unreachable *if* every caller validates first, but the invariant is by convention, not by type (`Metadata.t` is still a record of strings).
- `lib/matrix_client/dehydrated_device.ml:299,351,377` — base64 decode of `Pickle_key.to_base64` results.
- `lib/matrix_client/send_queue.ml:590,834,1664`, `lib/matrix_client/oauth.ml:300`, `lib/matrix_client/base_client.ml:1550`, `lib/matrix_client/qr_login.ml:805`, `lib/matrix_client/recovery.ml:923`, `lib/matrix_ui/runtime.ml:319`, `lib/matrix_proto/matrix_json.ml:145`, `lib/matrix_proto/matrix_sliding_sync.ml:503` (`Dropped -> assert false`).
- `lib/matrix_client/profile_store.ml:128` — `failwith "too many atomic-write temporary files"` after 100 collisions.

- Rust ref: `matrix-sdk` returns `Result`/`SessionStorageError` on malformed store/sync payloads; panics are not on the sync-decode path.
- Impact: any missed validation or future caller that skips `validate` turns a malformed room event, backup payload, or dehydrated-device response into a process/fiber crash. Local-collision `failwith` is a local DoS.
- Fix: replace each with `Result`/`option` propagation: make `raw_key`/`raw_iv` return `Result`, thread through `Decryptor.create`; convert `Dropped` to `Error`; convert atomic-write exhaustion to `Error.Json_error`/`Error.Network_error`. Add hostile-input tests (truncate/overlong varint, bad base64, `Dropped` frame).

### H4 — Debug-log redaction allowlist misses E2EE secrets

- Where: `lib/matrix_client/client.ml:513-566` (`sensitive_members`, `redact_json`, `redacted_body`), used for all request/response debug logs.
- Current list: `password`, `new_password`, `access_token`, `refresh_token`, `token`, `client_secret`, `response` (recaptcha), `id_access_token`.
- Missing: `session_key`, `session_data`, `recovery_key`, `secret`, `secrets`, `key`, `keys`, `pickle`, `device_pickle`, `private_key`, `seed`, `iv`+`mac` pairs that decrypt, `backup` auth data with public key is fine but `decryption_key` is not.
- Rust ref: `matrix-sdk` uses targeted redaction + `RUST_LOG` sensitive-module gating; crypto store values are never `Debug`.
- Impact: turning on `Logs` debug (common in `bin/omatrix`, `bin/matrix-bot`) can persist Megolm session keys, recovery keys, and pickles from `/sync`, `/keys`, `/backup`, `/dehydrated_device` bodies to disk/journal.
- Fix: extend list to cover E2EE members or switch to deny-by-default for `/sync`, `/keys/*`, `/backup/*`, `*dehydrated*`, `*secret*` bodies (log shape/length only). Add a `test_json_safety`-style test asserting each E2EE fixture redacts.

---

## Medium severity

### M1 — Megolm advances the ratchet before MAC verification

- Where: `lib/matrix_client/megolm.ml` `Inbound.decrypt`: signature check → `find_ratchet` (mutates `latest_ratchet`) → `Cipher.verify_mac8` → `Cipher.decrypt`.
- Rust/vodozemac ref: verify authenticity before committing ratchet state.
- Impact: limited — signature is checked first, so only the holder of the session signing key (legit sender or compromised sender) can make the receiver advance. Still, a MAC-failing future-index message leaves `latest_ratchet` advanced, potentially skipping honest messages (mitigated by skipped-key ring, but bounded at 40).
- Fix: compute on a copy and commit only after MAC+decrypt succeed, or re-derive without mutating on failure. Add a test: MAC-corrupt future message must not move `first_known_index`/`latest` index.

### M2 — Backup `session_mac` covers the empty string (spec-inherited, document it)

- Where: `lib/matrix_client/backup.ml` (`session_mac ~mac_key` = `HMAC("", mac_key)[0..8]`, with an honest comment).
- Rust ref: same (`matrix-sdk-crypto/backups`), per spec `m.megolm_backup.v1.curve25519-aes-sha2`.
- Impact: backup ciphertext has no per-message integrity beyond AES-CBC padding; integrity comes from transport + outer auth-data signature + trust in the backup version. Callers that treat "decrypts = authentic" overstate the guarantee; `forwarded_count`/`is_verified` are unauthenticated metadata.
- Fix: no code change (interop requires it); enforce at call sites: always check `verify_auth_data_signature`, pin `sender_key`/`forwarding_curve25519_key_chain`, and surface `forwarded`/`is_verified=false` in UI. Add a comment at `decrypt_room_key` callers, not just the primitive.

### M3 — Send-queue retries `M_UNKNOWN` with pure exponential backoff (no jitter)

- Where: `lib/matrix_client/send_queue.ml:1553-1576` (`retry_delay`, `classify`).
- Rust ref: `matrix-sdk/send_queue` backoff with jitter + `M_LIMIT_EXCEEDED.retry_after_ms`.
- Finding: `M_UNKNOWN -> retry` is generous (a persistent server bug becomes a hot loop until `q_max_retries=5`); delay is `base * 2^n` capped, with no jitter. Correctness is fine; fleet behaviour under a 5xx storm is not.
- Fix: add jitter (`delay * (0.5 + rand)`), keep `retry_after_ms` authoritative, consider not retrying `M_UNKNOWN` more than once without a `Retry_in` hint. Unit-test `classify` matrix.

### M4 — Encrypted-media spool: correct protocol, unverified filesystem properties

- Where: `lib/matrix_client/media.ml:379-480` (spool → `pwrite_all` → `verify_spool` → emit), `media_store.ml`, `Encrypted_attachment.Decryptor`.
- Finding: the three-pass design is right (ciphertext spooled, hash verified, then plaintext streamed; `decrypt_chunks` also gates `on_chunk` on `finish`). What is *not* shown: spool file location, permissions, quota, and cleanup on cancel/error (`protect`/`reset` rewind but do they unlink/truncate on every path?). `thumbnail` ignores `content_type` (`~content_type:_`), leaving MIME-sniffing to the UI layer. Upload `filename` goes into a query param (`media.ml:85-99`) — verify percent-encoding (header/query injection if a raw filename with `&`/`#`/`%0d` is interpolated).
- Fix: assert spool is 0600 + `O_EXCL` in cache dir with size cap; `finally` unlink on error/cancel; sniff or pin `Content-Type` at presentation; add filename-encoding test.

### M5 — `Profile_store.atomic_write` exhaustion is an uncaught exception

- Where: `lib/matrix_client/profile_store.ml:115-152`.
- Impact: 100 temp collisions (adversarial sibling writer or forked PID+counter reuse, both noted in comments) raises `failwith`, escaping the `Result` API of `save`.
- Fix: return `(unit, Error.t) result`. Same for the `invalid_arg` on missing basename.

### M6 — OAuth `allow_insecure` + loopback listener need end-to-end checks

- Where: `lib/matrix_client/oauth.ml:424,494,788-791`, `lib/matrix_eio/oauth.ml:472` (`| \`Unix _ -> assert false`), `lib/matrix_eio/qr_login.ml`.
- Finding: `validate ?allow_insecure` correctly defaults to HTTPS-only; the flag is easy to leave enabled from tests/examples. Loopback `http://127.0.0.1/callback` is per RFC 8252, but the Eio listener branch asserts on `` `Unix `` sockets — a platform/config surprise, not a vuln. Device-code `poll` (`oauth.mli:739`) and `finish_login` need a `state` mismatch test (verifier present and checked).
- Fix: `grep allow_insecure` in `bin/`+`example/` must be empty outside tests; replace the `` `Unix `` assert with `Error`; add OAuth negative tests (http metadata without flag fails; `state` mismatch fails; PKCE `plain` rejected).

### M7 — Base64 is lenient (unpadded-then-padded)

- Where: `lib/matrix_proto/matrix_base64.ml:decode`.
- Rust/ruma ref: strict unpadded base64url for Matrix wire values.
- Impact: accepts non-canonical encodings; masks sender bugs; negligible exploitability but weakens "invalid input rejected" tests.
- Fix: prefer strict unpadded-decode on the wire path; keep lenient helper only for legacy store migration, clearly named.

### M8 — QR login / rendezvous: strong, with two hygiene items

- Where: `lib/matrix_client/qr_login*.ml`, `qr_login_ecies.ml`, `qr_login_rendezvous.ml`.
- Good: role-separated info strings, `Counter_exhausted`/`Pending_consumed`/`Authentication_failed` distinct, ETag/expiry/`terminal_status` handling.
- Items: (1) `pending.secret_key : Curve25519.Secret.t option` lingers after `derive` — wipe/take it (`None` it + zero). (2) `close_rendezvous_best_effort` failures are `ignore`d in several `qr_login.ml` paths (1036-1205) — correct for cleanup, but ensure a cancelled rendezvous cannot be reused for a second login (replay). Add a "double-close then reuse fails `Closed`" test.

### M9 — SAS negotiation is correct; lifecycle needs pruning

- Where: `lib/matrix_client/verification_sas.ml`, `verification_flow.ml:109` (`ignore (track …)`), `lib/matrix_eio/verification_service.ml`.
- Good: offered-MAC enforcement, commitment check, `tick` timeout (600s default).
- Item: `tick` only runs on `handle`; idle verifications linger until the next message. `verification_flow.ml:109` ignores tracking result. Ensure `Verification_service` sweeps expired/timed-out flows and caps concurrent flows per device (state-exhaustion DoS otherwise).
- Fix: bounded flow table + periodic `tick` sweep; propagate `track` errors.

### M10 — Room-key export decrypt takes attacker-controlled PBKDF2 rounds (CPU DoS)

- Where: `lib/matrix_client/room_key_export.ml:191-218` (`default_rounds 500_000`, `maximum_rounds 0xffffffff`, `derive_keys`).
- Finding: encrypt defaults are sane; decrypt honours file-supplied `rounds` up to ~4G with `Pbkdf.pbkdf2 ~prf:`SHA512`. A malicious import file pins a core for minutes.
- Rust ref: similarly bounded; typically capped + progress UI.
- Fix: cap decrypt rounds (e.g. refuse > 2M without explicit `~allow_high_rounds`), run KDF in a cancellable worker, and rate-limit import attempts. Test with `rounds = 0xffffffff` fixture expecting `Invalid_rounds`/prompt, not hang.

### M11 — `matrix_eio` converts `Result` to exceptions (`Error.unwrap`)

- Where: `lib/matrix_eio/*.ml` (e.g. `receipts.ml`, `typing.ml`, `report.ml`, `account_data.ml` — nearly every function).
- Rust ref: `matrix-sdk-ffi` returns `Result` across FFI; panics are bugs.
- Impact: correctness/robustness, not memory safety — Eio callers that forget `try` crash the fiber; the two-layer API (`matrix_client` Result vs `matrix_eio` raise) is a footgun.
- Fix: provide `*_result` variants or document that `matrix_eio` raises `Error.client_error`; add a lint (`check_matrix_route_usage.sh`-style) forbidding bare `unwrap` in new code paths that handle network input.

### M12 — Sync presence default is implicit (`Online → None`)

- Where: `lib/matrix_client/sync.ml:21-28` (`wire_presence`).
- Finding: matches the spec (absent presence = online) and honours `Client.sync_presence`, but a caller reading `params` back sees `Some Online` while the wire sends nothing. Benign if documented; confusing under test replay.
- Fix: comment + round-trip test (`query_of_params`).

---

## Low / hygiene

- **L1 — Megolm rotation wiring**: defaults (`100` msgs / 7 days in `megolm.ml` `Outbound`) match the spec; confirm `needs_rotation` is consulted on every send path in `encryption.ml` (grep suggests `shared_with`/`message_count` tracked — add an integration assertion, not just unit).
- **L2 — HTML sanitizer residuals**: `presentation.ml` `keep_attribute` is allowlisted (good). Residual: `img width/height` and `font/span data-mx-*` *values* are unvalidated; `a title` is free text (escaped by `Markup.write_html`, so只有 CSS-in-webview rendering needs a second look). Validate `width/height` numeric, clamp `data-mx-color` to `#rgb/#rrggbb`/named set if rendered in a webview.
- **L3 — Transaction IDs**: `Random.txn_id` (`"m" ^ base64url(16 bytes)`) is fine; `Transaction_id.of_bytes` hex helper is deterministic — ensure callers never use it for uniqueness (only for display of received bytes).
- **L4 — `report.ml:35` `ignore score`**: spam-report `score` is accepted but dropped — confirm the homeserver endpoint still receives it or remove the parameter (spec: `score` is deprecated but still sent by some clients).
- **L5 — `sliding_sync_state.ml:413` / `thread_subscriptions.ml:190`**: `ignore (Store.Slot.set …)` return — confirm persistence failures surface elsewhere or log them.
- **L6 — `media_fetcher` default in `matrix_eio/client.ml`**: mutable `ref` shared across fibers — confirm `set_media_fetcher` is startup-only or guard with mutex (data race on policy swap).

---

## Completeness: what is missing vs matrix-rust-sdk (and what to do)

This is *not* a claim that everything must be ported. Disposition follows the tree's own docs (`STATUS.md` live, `TODO.md` queue, `PARITY_PLAN.md` historical).

| Rust capability | OCaml status | Disposition |
|---|---|---|
| `StoreCipher` at-rest encryption | **Missing** (H1) | Port or explicitly decline with warning |
| Secret zeroing | **Missing** (H2) | Port policy + docs |
| Widgets, content scanner, search index, MatrixRTC (beyond event types), QR image render/scan (`qrcode` feature), `socks`, `federation-api`, `indexeddb`, `bundled-sqlite`, `js` | Absent | Keep absent; ensure `README`/`STATUS` say so (mostly already do) |
| VoIP (`calls.ml` 102 lines: signalling + TURN fetch, no WebRTC) | Narrow by design | Keep; document "signalling only" at `calls.mli` (already does) |
| Peeking / retention / MSC4108 rendezvous | `rendezvous` now present (`qr_login_rendezvous.ml`); peeking/retention thin | Update `PARITY_PLAN.md` snapshot note (§B) which still says "absent" |
| MSC4268 history-sharing bundle | Landed (bundle codec + shared-history metadata per plan) | Verify trust-gated transport + join orchestration have hostile tests |
| Sliding sync → base-client feed, adaptive fallback, dehydrated-device lifecycle, SAS publish, SSSS/recovery import | Landed per plan commits | Keep; add negative tests per M1/M9 |
| `bindings/matrix-sdk-ffi` surface vs `bin/omatrix`+`matrix_cli` | `omatrix` covers encrypt/verify/backup; FFI macros (`uniffi`) N/A | Fine; ensure CLI never enables `allow_insecure` by default |

No new "poorly translated" crypto logic was found beyond M1/M2/M10: Olm double-ratchet, Megolm ratchet (`Ratchet.advance/advance_to`), OLM/MEGOLM HKDF info strings (`OLM_KEYS`/`MEGOLM_KEYS`, one-zero-byte salt), dehydrated-pickle v4 layout, and SAS emoji/decimal derivation all match vodozemac/Rust semantics on inspection, and the oracle harness is the right way to keep it that way.

---

## Simplification / code-reuse opportunities (concrete, ordered by payoff)

### S1 — Generate the `matrix_eio` thin wrappers (biggest win)

- Evidence: `lib/matrix_eio/*.ml` (~50 files); samples `receipts.ml`/`typing.ml`/`report.ml` are 4–15-line `Error.unwrap (Matrix_client.X.f (Client.base client) …)` shims. `wc` shows most under 45 lines; total ≈ 1k+ lines of boilerplate plus drift risk (new `matrix_client` function → forgotten wrapper).
- Fix: one `Eio_lift` functor or a small `tools/gen_eio_wrappers` dune rule emitting `let f client … = Error.unwrap (Matrix_client.M.f (Client.base client) …)`. Keep hand-written files only where Eio adds behaviour (`client.ml`, `sync_service.ml`, `verification_service.ml`, `adaptive_sync.ml`, `qr_login.ml`, `oauth.ml`, `sliding_sync.ml`). Estimated saving: ~800 lines + eliminates a whole class of "wrapper forgot new param" bugs.

### S2 — Unify the three error/unwrap helpers

- Evidence: `lib/matrix_client/error.ml` (full errcode table), `lib/matrix_eio/error.ml` (raise/unwrap), proto-level `Olm_error`, `Encrypted_attachment.error`, `Rendezvous.error`, `Room_key_export.error`.
- Fix: keep domain errors, but route all `client_error` raise/unwrap through one `Matrix_client.Error` (+ `Matrix_eio.Error` re-export). Forbid new `assert false` via the existing `tools/` lint pattern (`check_matrix_json_codecs.sh`, `check_matrix_route_usage.sh` — add `check_no_assert_false.sh`).

### S3 — Finish the JSON facade consolidation (already started)

- Evidence: `TODO.md` R0.1 mandates `Matrix_proto.Json.Codec` as the single checked facade (int/range/float/string/duplicate policy); `matrix_json.ml` (282 lines) + `client.ml` `decode_json` already implement most of it.
- Fix: migrate remaining per-module `Jsont.int`/string maps to the facade; delete duplicated range checks; add one hostile test per rule (numeric string, `1.5` for int, `2^53`, duplicate member, invalid UTF-8). This removes the most dangerous *future* translation drift.

### S4 — Share the encrypted-media verify path

- Evidence: `media.ml` first-pass/verify-spool/emit (~110 lines) reimplements `Encrypted_attachment.Decryptor` feed/finish three times; `decrypt_chunks` already abstracts it for the non-streaming case.
- Fix: extract `verify_spool : metadata -> spool -> result` + `stream_verified : metadata -> spool -> output -> result` in `encrypted_attachment.ml`; `media.ml` calls them. Also share `xor_with_keystream`/CTR setup between `Encryptor`/`Decryptor` (already adjacent — just dedupe comments).

### S5 — Collapse repetitive `profile_store`/`store`/`crypto_store` plumbing

- Good reuse already (`atomic_write` shared). Remaining: identical `load/save` wrappers per file name in `store.ml:868-870` and `crypto_store.ml:694-705`; per-pickle `jsont` in `session_pickle.ml` (~476 lines, largely mechanical). Consider a `Stored_json (Name) (Codec)` functor. Low risk, moderate readability win.

### S6 — Examples/bins duplicate the login→sync→send loop

- Evidence: `example/` (25 dirs) + `bin/matrix-bot/*` + `bin/omatrix/omatrix.ml` each re-drive login, filter setup, and sync loops; `matrix_bot/context.ml`+`room.ml` already abstract some of it.
- Fix: promote one `Matrix_bot.Context`-style helper for examples (or explicitly mark examples as copy-paste teaching material and exclude them from the wrapper generator). Prevents examples drifting from library semantics (e.g. `allow_insecure`, presence, filters).

### S7 — Pin `ocaml-httpz`

- Evidence: `ocaml-httpz -> /home/avsm2/src/git/avsm/ocaml-httpz` symlink at repo root.
- Fix: replace with an opam pin + commit hash (as `TODO.md` R-audit already notes for the adjacent HTTPz checkout). A floating symlink makes every TLS/date/ETag behaviour in this audit non-reproducible.

---

## Suggested fix order

1. H4 (redaction — one-line list + test, stops active secret leakage to logs).
2. H3 (assert/false → Result — mechanical, kills crash bugs; add hostile fixtures).
3. H1 (StoreCipher design + warning — largest work, start now).
4. H2 (zeroing policy + ECIES/ratchet wipes — alongside H1).
5. M10 (round cap), M3 (jitter), M1 (commit-after-MAC), M5/M11 (error plumbing).
6. S1+S2+S3 (generators/facade — prevents recurrence), then S4–S7.

## Appendix — smoke signals sampled

- `grep -rn "assert false" lib/`: `oauth.ml:300`, `send_queue.ml:590,834,1664`, `encrypted_attachment.ml:258,261,317,338`, `dehydrated_device.ml:299,351,377`, `profile_store` n/a (uses `failwith`), `base_client.ml:1550`, `qr_login.ml:805`, `recovery.ml:923`, `matrix_json.ml:145`, `matrix_sliding_sync.ml:503`, `matrix_ui/runtime.ml:319`, `matrix_eio/{oauth:472,verification_service:156}`.
- `grep -rn "failwith" lib/`: `profile_store.ml:128` only (good).
- No `Obj.magic` / `Marshal` in `lib/` (good).
- `ignore` hits are overwhelmingly benign (`Eio.seek`, best-effort rendezvous close, `Slot.set`); the ones worth a second look are listed in L5/M8/M9.
- Test coverage noted: `test/test_olm.ml` (recorded + live oracle), `test/test_verification.ml`, `test/test_qr_ecies.ml`, `test/test_dehydrated_pickle.ml`, `test/test_encryption*.ml`, `test/test_json_safety.ml`, `test/test_oauth.ml`, `test/integration` Synapse suite (per docs). Additions proposed above fit existing files (no new harness needed except a `check_no_assert_false.sh` lint).

*End of audit — no code changed; findings are file- and line-anchored for follow-up `edit` tasks.*
