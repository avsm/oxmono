# Native vCard storage and CardDAV

Sortal's authoritative store is `~/.local/share/sortal`. Contacts are vCard
3.0 files in `cards/`, named by UID. `store.json` records the store UUID and
storage format version 1. This UUID binds local sync journals to the store.
Cards contain no store identity and can move between stores unchanged. Assets
and feed caches retain their original paths.
The contact library, Bushel, Arod and Termanil read this store directly.

Embedded `PHOTO` bytes are authoritative. File-based callers receive a cached
materialization under the XDG cache directory. A local photo file may be
removed once its bytes are represented in the card. Unreferenced legacy assets
remain separate files until explicitly cleaned up.

CardDAV connection defaults are stored in
`$XDG_CONFIG_HOME/sortal/config.toml` under `[carddav]`. The password is kept
in the configured `password_file`, which `sortal init` creates beside the
configuration with mode `0600`. CardDAV command options override configured
values. Bundle and report paths default to `$XDG_STATE_HOME/sortal/carddav`.

The one-off migration preserves the store UUID and every existing contact UID.
It retains the complete original store, including comments, source formatting,
photos, caches, uncommitted changes and Git history, in a separate backup.
The active store contains no YAML contacts. Sortal has no YAML contact reader,
writer or migration command.

## Field mapping

| Contact field | Compatible vCard representation |
| --- | --- |
| Stable contact identity | `UID` |
| Readable handle | `X-SORTAL-ID` |
| Primary name | `FN`, with an unsplit `N` fallback |
| Additional names | `X-SORTAL-ALT-NAME` |
| Person or organization | `X-ADDRESSBOOKSERVER-KIND`, `X-ABShowAs` |
| Email addresses | `EMAIL`, with preference/type parameters |
| Web links | `URL`, grouped `X-ABLabel` |
| Social accounts | `X-SOCIALPROFILE` and visible `URL` fallbacks |
| AT Protocol DID and apps | Grouped AT Protocol extension properties |
| Affiliations | Grouped `ORG`, `TITLE`, `ADR`, `URL` and validity dates |
| RSS, Atom and other feeds | `URL` with feed type, hint and paused metadata |
| Local photo | Inline `PHOTO` plus its relative asset path |
| Future fields | Individual `X-SORTAL-FIELD` properties |

Field path annotations retain list order, unknown nested values and the
presence of explicitly empty collections or false booleans. Future fields use
JSON values and encoded JSON Pointer paths. There is no complete contact blob
such as `X-SORTAL-META`.

The public contact schema is a projection of the vCard. Loaded contacts retain
their source revision in memory. A typed save patches only changed fields,
preserves metadata outside the projection and checks the revision before an
atomic write. A no-op save preserves every byte except retired
`X-SORTAL-STORE` properties. Legacy tags are removed on save or export and are
ignored when matching server copies by UID. Store writes take an advisory
lock. Editors that bypass that lock should save and refresh before another
writer edits the same contact. Ambiguous grouped or repeated metadata causes an
error rather than an uncertain rewrite.

Sortal requires its identity and field annotations for typed contact editing.
Termanil can also display ordinary unannotated vCards and remote address books.
Native CardDAV import normalization remains part of the server sync step.

## Local snapshots

```sh
dune exec -- sortal carddav export \
  --source ~/.local/share/sortal --output /tmp/sortal-baseline
dune exec -- sortal carddav verify \
  --bundle /tmp/sortal-baseline --source ~/.local/share/sortal
```

The output must be new and outside the source. Export copies the live cards
without re-encoding them, omitting retired store tags. The original files are
archived byte for byte. Verification checks every archived file, card UID,
handle, embedded photo and typed no-op round trip. Store identity is checked
against archived local metadata. Previous snapshots and pull journals must
belong to the current local store, account and collection.

Native snapshots use manifest version 3. The earlier YAML recovery bundles
and pull journals are archival material. They cannot be used as native sync
baselines. Existing identities survive in the active cards independently of
these bundles.

## Preview a server

```sh
dune exec -- sortal carddav sync --dry-run \
  --source ~/.local/share/sortal --bundle /tmp/sortal-baseline \
  --username YOUR_FASTMAIL_LOGIN --password-file /path/to/app-password \
  --report /tmp/sortal-preview
```

The default endpoint is Fastmail. `--server` selects another HTTPS CardDAV
server. `--collection` selects an address book when discovery is ambiguous.
The report directory must be new. Dry runs write only that report, including
a fresh snapshot of current source edits. They do not mutate local contacts,
server contacts or existing journals.

Possible matches by UID, email, account or normalized name are held for review.
Matching names are never automatically merged. The report explains each hold.
A fresh baseline can preview a previously seeded account, but pulling edits
requires a verified binding to the account and its last common server state.
Do not substitute a fresh snapshot for that common state.

## Explicit operations and limits

`sortal carddav seed --apply` conditionally creates unambiguous new contacts,
then reads each one back and verifies it. It never overwrites an existing card.
`sortal carddav pull prepare` produces a journal of supported remote changes.
`sortal carddav pull apply --apply` checks local revisions, remote ETags and
saved hashes before atomically replacing local vCards. Replay is supported.
Without `--apply`, it validates and previews only.

Native pull journals use version 2 and retain `before.vcf`, `after.vcf` and a
separate `common.vcf`. The common state includes only changes received from the
server. Unrelated local edits remain in the working card, so they do not become
assumed server state.

The importer supports primary names, email addresses, kind and a conservative
set of ordinary properties. Unsupported changes require reconciliation. General
push updates, deletion propagation and re-binding the pre-migration Fastmail
state belong to the second migration step. The local migration performs no
network writes.

## Verification

```sh
dune runtest avsm/sortal --force
```

Tests cover native storage, unknown nested fields and unsupported feed types,
parameter retention, stale revisions, stable filenames, Git operations,
lossless mappings, source snapshots, duplicate detection, conditional creation,
remote capability restrictions and pull journal replay. Network tests use a
mock transport.
