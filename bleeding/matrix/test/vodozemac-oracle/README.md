# vodozemac-oracle

A tiny Rust binary that wraps [vodozemac](https://crates.io/crates/vodozemac),
the Olm/Megolm implementation used by matrix-rust-sdk, behind a line-oriented
JSON protocol. `test/test_olm.ml` drives it to prove that this SDK's Olm and
Megolm wire formats are byte-for-byte interoperable with a real implementation,
in both directions.

## Building

```sh
cargo build --release --manifest-path test/vodozemac-oracle/Cargo.toml
```

`Cargo.lock` is committed, so after the first fetch the build works offline.
The build tree (`target/`) is not committed and dune is told not to descend
into this directory (see the `dirs` stanza in `test/dune`).

`test/test_olm.ml` looks for `target/release/vodozemac-oracle` by walking up
from its working directory, or uses `$VODOZEMAC_ORACLE` if that names an
existing file. When neither is found the cross-implementation cases print
`SKIP` and pass; the recorded vectors in `test/fixtures/olm` still run.

## Protocol

One JSON object per line on stdin, one JSON object per line on stdout. Every
reply carries `"ok": true` or `"ok": false` with an `"error"` string. Accounts
and sessions are held in memory and addressed by small integer handles; pickle
commands operate only on strings and nothing touches the disk. All keys,
messages and session keys
cross the boundary as unpadded base64, the same encoding Matrix uses.

| Command | Arguments | Reply |
|---|---|---|
| `ping` | – | `pong` |
| `create_account` | `one_time_keys` (optional count) | `account`, `curve25519`, `ed25519`, `one_time_keys` (key id → key) |
| `generate_one_time_keys` | `account`, `count` | `one_time_keys` |
| `identity_keys` | `account` | `curve25519`, `ed25519` |
| `generate_fallback_key` | `account` | `fallback_keys` |
| `mark_keys_as_published` | `account` | `marked` |
| `account_pickle` | `account`, `pickle_key`, optional `device_id` | encrypted legacy libolm account pickle |
| `account_from_pickle` | `pickle`, `pickle_key`, optional `device_id` | identity and key sets |
| `account_sign` | `account`, `message` | Ed25519 `signature` |
| `account_sign_from_pickle` | `pickle`, `pickle_key`, optional `device_id`, `message` | Ed25519 `signature` |
| `create_outbound_session` | `account`, `identity_key`, `one_time_key` | `session`, `session_id` |
| `create_inbound_session` | `account`, `identity_key`, `ciphertext` (base64 pre-key message) | `session`, `session_id`, `plaintext` |
| `session_id` | `session` | `session_id` |
| `session_encrypt` | `session`, `plaintext` | `message_type`, `ciphertext` |
| `session_decrypt` | `session`, `message_type`, `ciphertext` | `plaintext` |
| `sas_create` | – | `sas`, `public_key` |
| `sas_establish` | `sas`, `public_key` | `established` |
| `sas_mac` | `established`, `input`, `info`, optional `legacy` | `mac` |
| `megolm_create` | – | `session`, `session_id`, `session_key` |
| `megolm_session_key` | `session` | `session_key`, `message_index` |
| `megolm_encrypt` | `session`, `plaintext` | `message_index`, `ciphertext` |
| `megolm_inbound_import` | `session_key`, `exported` (bool, default `false`) | `session`, `session_id`, `first_known_index` |
| `megolm_decrypt` | `session`, `ciphertext` | `plaintext`, `message_index` |
| `megolm_export_at` | `session`, `index` | `session_key` (version-1 export) |
| `megolm_first_known_index` | `session` | `first_known_index` |
| `ed25519_verify` | `key`, `message`, `signature` | `verified` |

For account-pickle commands, `pickle_key` is raw 32-byte key material encoded
as base64. Supplying `device_id` first derives the effective key with
HKDF-SHA-256, using the device ID bytes as salt and
`dehydrated-device-pickle-key` as info. Omitting it exercises the ordinary
vodozemac/libolm pickle API directly.

`megolm_inbound_import` with `"exported": false` expects the signed version-2
session-sharing format; with `"exported": true` it expects the unsigned
version-1 export format.

The SAS commands create an ephemeral vodozemac Curve25519 key and establish a
shared secret with the supplied OCaml public key. `sas_mac` returns the normal
HKDF/HMAC-SHA-256 base64 encoding by default. With `legacy: true`, it uses
vodozemac's `calculate_mac_invalid_base64` compatibility implementation for
the historical libolm `hkdf-hmac-sha256` encoding bug.

Example:

```sh
$ printf '%s\n' '{"cmd":"create_account","one_time_keys":1}' \
  | ./target/release/vodozemac-oracle
{"account":0,"curve25519":"…","ed25519":"…","ok":true,"one_time_keys":{"AAAAAAAAAAA":"…"}}
```

## Regenerating the recorded vectors

`test/fixtures/olm/*.json` are recorded oracle outputs so that the interop
check still bites without cargo. Re-record them with:

```sh
OLM_FIXTURE_OUT=$PWD/test/fixtures/olm dune exec test/test_olm.exe
```
