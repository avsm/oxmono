# Sortal fields in vCard and future two-way CardDAV sync

The implemented offline exporter translates Sortal V2 records into individual
vCard properties. It emits **no complete YAML or JSON contact payload**.
`tools/sortal_vcard.py` provides the forward mapping and a reverse mapping for
this export profile. `tools/carddav_export.py` builds and verifies the bundle.
The original files remain in a local recovery archive. `tools/carddav_trial.py`
can inspect and seed a Fastmail test collection, verify each new card, and hold
possible duplicates for review. It does not update existing contacts.

The selected sync mode is **two-way**: Sortal, Fastmail and connected phone or
desktop apps can all contribute edits. Remote reconciliation, durable sync
state and conflict handling are partially implemented by the conservative
pull tool described below. General two-way merging remains unfinished. The
inverse mapping alone is an offline verification primitive, not a general
CardDAV importer.

## Dry run against a real account

`tools/carddav_sync.py --dry-run` builds a fresh export of the current Sortal
root, retains the identities from the supplied bundle, and inspects the
destination. It writes a private Markdown report, JSON plan and snapshots to
a new report directory. It does not change source contacts, server contacts,
or existing synchronization journals. This combined command only exposes
previewing; omitting `--dry-run` also previews.

From the monorepo root, replace the login and password-file path:

```sh
uv run --no-project --script avsm/sortal/tools/carddav_sync.py \
  --dry-run --source ~/bushel/sortal \
  --bundle avsm/sortal/_carddav-export-compatible-2026-09-11 \
  --username YOUR_REAL_LOGIN --password-file ~/.fm-real \
  --report /tmp/sortal-real-preview
```

`uv` installs the script's declared dependencies into its cache automatically.
Alternatively, install `tools/requirements.txt` and invoke the script with
Python. The default server is Fastmail. For another server, add
`--server https://contacts.example.org/`, or its discovery endpoint such as
`https://contacts.example.org/remote.php/dav/`. Use `--collection FULL_URL`
to select an address book explicitly. The server URL must use HTTPS and
credentials cannot follow a redirect to another origin or port.

Read `/tmp/sortal-real-preview/report.md` for proposed creations, unchanged
contacts, review candidates, and the pull preview. The JSON plan and complete
remote cards are available alongside it. Each run requires a new report
directory outside the source tree. Reports contain private contact data and
use a mode-0700 parent directory; credentials are not included.

For an account already synchronized with this bundle, add
`--previous-pull LAST_APPLIED_JOURNAL` when a pull has been applied. The preview
uses that account's common baseline to generate proposed YAML diffs under
`pull/`. A test-account baseline is never used to infer merges for a different
real account. Without a common baseline, existing-name/email/account matches
are held for review and no pull merge is invented. Unsupported changes and
lost annotations can also appear as review items even when no local update
is needed. This is a conservative preview, not a claim of arbitrary two-way
merge support or an actual server write/readback test.

The lower-level commands also accept an explicit dry mode:

```sh
python3 avsm/sortal/tools/carddav_trial.py BUNDLE --dry-run \
  --server https://contacts.example.org/ \
  --username ACCOUNT --password-file PASSWORD_FILE --report NEW_REPORT_DIR
python3 avsm/sortal/tools/carddav_pull.py apply PREPARED_PULL_JOURNAL \
  --dry-run --username ACCOUNT --password-file PASSWORD_FILE
```

The trial's `--dry-run` and `--apply` flags are mutually exclusive. Its
transport permits only GET, HEAD, OPTIONS, PROPFIND and REPORT during a dry
run, rejecting write methods before opening a connection. Pull dry mode
validates the prepared hashes and current remote version without writing
YAML, returned-card files, status changes or ETags into the existing journal.

Validation on 2026-09-11: the complete suite passes 69 tests, including an
actual proposed pull diff that leaves the source and prior baseline untouched,
HTTP mutation blocking, cross-origin redirect rejection, fresh-source export,
and existing-name duplicate prevention. A live dry run against the test
account proposed 0 creations and 0 local updates: 459 uploads were unchanged,
and 5 records were held for review (the two possible duplicate pairs plus the
previously edited card's stripped annotations). All current source files
matched the fresh export afterward; prior applied baseline hashes remained
valid.

## Field mapping

Fastmail documents vCard 3.0 as its sync format, and stores custom `X-` fields.
That is the default export profile. `--vcard-version 4.0` uses newer standard
properties where available. Its documentation also warns that some editors
can transform or lose fields; the intended clients still need round-trip
checks. [Fastmail field documentation](https://www.fastmail.help/hc/en-us/articles/360058753094-Troubleshooting-CardDAV-fields).

| Sortal field | vCard 3.0 profile | vCard 4.0 profile |
| --- | --- | --- |
| Primary name | `FN`, with unsplit `N` for compatibility | `FN;PREF=1`, with unsplit `N` |
| Additional full-name variants | Repeated `X-SORTAL-ALT-NAME` | Additional `FN` properties |
| Email addresses | Repeated `EMAIL`; first has `TYPE=INTERNET,PREF` | Repeated `EMAIL`; first has `PREF=1` |
| Links | `URL`, with grouped `X-ABLabel` when labeled | Same |
| Account platform and handle | `X-SOCIALPROFILE;TYPE=github;X-USER=example` plus an ordinary profile `URL` | `SOCIALPROFILE;SERVICE-TYPE=github;USERNAME=example` plus an ordinary profile `URL` |
| AT Protocol handle and DID | `X-ATPROTO` with `X-ATPROTO-HANDLE` and `X-ATPROTO-DID` parameters | Same |
| AT Protocol apps | Repeated `X-ATPROTO-APP`, each grouped with an app `URL` | Same |
| Person or organization | `X-ADDRESSBOOKSERVER-KIND` | `KIND` |
| Organization and department | Components of grouped `ORG` | Same |
| Affiliation title, address and URL | `TITLE`, `ADR`, `URL` in the same group as `ORG` | Same |
| Affiliation dates | `X-VALID-FROM`, `X-VALID-UNTIL` parameters on the grouped properties | Same |
| Feed URL and format | Ordinary `URL;X-FEED-TYPE=atom` | Ordinary `URL;MEDIATYPE=application/atom+xml;X-FEED-TYPE=atom` |
| Feed name | `X-ABLabel` in the feed's group | Same |
| Feed discovery hint and paused flag | Grouped `X-FEED-HINT` TEXT and `X-FEED-PAUSED:TRUE` or `FALSE` | Same |
| Selected local photo | Inline `PHOTO;ENCODING=b;TYPE=JPEG` or `PNG` | `PHOTO` with an image data URI |
| Remote photo | `PHOTO;VALUE=uri` | Same |

`SOCIALPROFILE`, `SERVICE-TYPE` and `USERNAME` are standard vCard 4.0
extensions. The `X-SOCIALPROFILE` spelling is a vCard 3.0 compatibility
convention, not the same standard property. [RFC 9554 sections 3.5, 4.9 and
4.10](https://www.rfc-editor.org/rfc/rfc9554.html#section-3.5).

Feeds use ordinary URLs so clients that ignore extensions still show usable
links. Grouped `X-ABLabel` supplies the feed name, or a generated label such as
"ATOM feed" when no name was supplied. There is no dedicated registered vCard
feed property. vCard 4.0 allows a `MEDIATYPE` parameter on `URL`: Atom uses
`application/atom+xml`, RSS uses `application/rss+xml`, and JSON Feed uses
`application/feed+json`. The 3.0 compatibility profile retains the format in
`X-FEED-TYPE`; it does not emit `X-FEED`. The inverse still accepts the earlier
`X-FEED` export. [vCard URL definition](https://www.rfc-editor.org/rfc/rfc6350.html#section-6.7.8),
[vCard registry](https://www.iana.org/assignments/vcard-elements/vcard-elements.xhtml),
[JSON Feed format](https://www.jsonfeed.org/version/1.1/).

The feed settings, AT Protocol and date extensions build on the repository's
`draft-madhavapeddy-sortal-vcard-00.txt`; they are proposals/custom extensions,
not IETF-standard properties. `X-ATPROTO-APP` and `X-FEED-PAUSED` cover V2
fields the earlier draft did not represent. The hint is a grouped TEXT
property here, rather than the draft's parameter, to handle quotes, newlines
and arbitrary discovery instructions without vCard-version-specific parameter
escaping. A username needing similar escaping uses grouped `X-SORTAL-USERNAME`.

Groups bind a feed to its settings and an affiliation to its title, department
and address. All affiliations, including past and future ones, are exported
with their dates. Clients unaware of temporal extensions may display every
workplace as current; the source history is nevertheless represented. Dates
retain year/month/day precision; `until` remains exclusive.

Full-name variants are not converted into nicknames. An unsplit full name in
`N` is a compatibility fallback; it does not infer given and family names.
Unstructured Sortal addresses stay together in `ADR`'s street component.
Future import must preserve genuine structured names and addresses separately
when Sortal cannot represent their components.

## Identity and reverse mapping

| Property | Meaning |
| --- | --- |
| `UID` | Stable CardDAV contact identity; adopt the existing server UID when linking to a destination card. |
| `X-SORTAL-ID` | Sortal handle, including Unicode and case. |
| `X-SORTAL-STORE` | Store UUID, distinguishing equal handles in different Sortal roots. |
| `X-SORTAL-SCHEMA:2` | Sortal schema version. |
| `X-SORTAL-MAPPING:3` | Current mapping, adding vCard passthrough; versions 1 and 2 remain readable. |
| `X-SORTAL-PATH` parameter | The field location represented by a property, such as `/emails/0` or `/affiliations/1/title`. |
| Grouped `X-SORTAL-PHOTO-PATH` | Relative local filename for an embedded photo. |

The small field-path annotations preserve membership and ordering when a
server reorders properties. They distinguish a personal link from a workplace
URL, and an alternate full name from the primary name. They contain locations,
not duplicate contact values. An explicitly empty collection uses
`X-SORTAL-EMPTY;X-SORTAL-PATH=/emails:array`; this preserves the distinction
between an absent field and an explicit empty list without a serialized blob.

For example, a short vCard 3.0 excerpt is:

```text
UID:2d5d2547-c51d-46d1-8346-39d3054a6b42
X-SORTAL-ID;X-SORTAL-PATH=/handle:casey
FN;X-SORTAL-PATH=/names/0:Casey Example
EMAIL;TYPE=INTERNET,PREF;X-SORTAL-PATH=/emails/0:casey@example.org
item1.X-SOCIALPROFILE;TYPE=github;X-USER=casey
 ;X-SORTAL-PATH=/accounts/github:https://github.com/casey
item1.URL;X-SORTAL-DERIVED=profile:https://github.com/casey
item1.X-ABLabel:github
item2.URL;X-FEED-TYPE=atom;X-SORTAL-PATH=/feeds/0:
 https://example.org/feed.atom
item2.X-ABLabel:ATOM feed
item2.X-FEED-PAUSED;X-SORTAL-PATH=/feeds/0/paused:TRUE
```

Changed visible values are read back from these properties. There is no old
embedded record to overwrite them. The inverse detects conflicting account
usernames and profile URLs, including ordinary social and AT Protocol fallback
URLs, instead of silently choosing one. AT Protocol accounts with no explicit
app list have a derived ordinary Bluesky URL. The future live
merger must also handle added or removed unannotated remote fields, validate
received annotations, and consult its baseline if a client strips them.

The first export assigns a store UUID and derives UUIDv5 contact IDs within
that namespace. Pass `--previous BUNDLE` on re-export to retain identities;
`--rename OLD=NEW` retains a UID across a handle rename. These offline UIDs
are provisional until reconciliation: confirmed destination matches keep
their existing UID and href. The resource href, UID and Sortal handle are
separate identities; names are not identifiers.

CardDAV supports custom properties and parameters in stored vCards. Keep the
extensions inside the vCard, so they travel with the contact, rather than in
arbitrary WebDAV resource metadata. [RFC 6352 sections 6.3.2.2 and
8.5](https://www.rfc-editor.org/rfc/rfc6352.html#section-6.3.2.2).

## Preservation and local verification

The vCard fields preserve contact values, list order, affiliation relationships,
DIDs, feed settings and selected photo bytes. YAML comments, formatting and
line endings belong in the local archive; they are not contact properties.
The archive also retains extra images, feed caches, annotations and Git files,
which are not address-book data and are not uploaded as contact fields.

Eight source URLs have trailing newlines. Their usable `URL` values are
normalized, with the original value retained only on those fields in a
grouped `X-SORTAL-ORIGINAL-URL` TEXT property. The inverse uses that original
only while it still normalizes to the visible URL; a changed visible URL
wins over the stale original. This exception preserves the source values
without serializing whole records.

Unknown fields, unknown account platforms and unsupported `vcard`
passthrough shapes cause an explicit export failure. Nothing is silently dropped or
hidden in a catch-all payload. Add a mapping before exporting a schema with
such fields. Missing photos, duplicate handles/YAML keys, special files,
symlinks and paths outside the root also fail the export. Remote image URLs
are retained; their contents are not fetched.

From the monorepo root, with Python 3.10+ and PyYAML:

```sh
python3 avsm/sortal/tools/carddav_export.py export ~/bushel/sortal \
  avsm/sortal/_carddav-export-compatible-2026-09-11 \
  --previous avsm/sortal/_carddav-export-fields-2026-09-10
python3 avsm/sortal/tools/carddav_export.py verify \
  avsm/sortal/_carddav-export-compatible-2026-09-11 --source ~/bushel/sortal
dune exec avsm/sortal/tools/validate_carddav.exe -- \
  avsm/sortal/_carddav-export-compatible-2026-09-11
```

Choose a new destination for subsequent runs. The prior `--previous` bundle
above is the prior field-mapped export, used to keep exactly the same IDs;
new exports no longer emit `X-SORTAL-META`. Omit `--previous` only when creating
an independent store identity. Use `--vcard-version 4.0` for the newer profile.
`--as-of` records a reference date; it does not discard historical affiliations.

```text
contacts.vcf        all contacts, for inspection or an empty destination
cards/<uuid>.vcf    individual contact resources
manifest.json      identities, checksums and projection warnings
originals/         untouched YAML, photos, feed state and .git files
```

The exporter never modifies the source. The output must be new and outside
it. Bundle directories use mode 0700 and the default output paths are
Git-ignored. Verification checks every source file checksum and reconstructs
every contact from its vCard fields, comparing complete parsed values with
the source. Local photo bytes are decoded and compared independently.

All 464 contacts and 169 photos in the inspected root passed, with the same
UIDs as the previous export. The archive retains all 899 files (871 working
files and 28 Git files). All 464 contacts also survived parsing and
reserialization through the independent native vCard library, followed by
reverse conversion; synthetic cases passed for both vCard 3.0 and 4.0.

The initial combined compatibility vCard file was 6,727,418 bytes. The largest
original PNG made its card 4,099,200 bytes. The test Fastmail collection advertises a
15,728,640-byte resource limit, so all cards fit. Other destinations and
editing clients need their own size checks. No display photo was resized;
all selected photo bytes are retained.

To recover a whole original root, copy `originals/` to a new empty location
and verify the checksums. The reverse field mapper reconstructs contact
values and selected photos; it cannot restore YAML comments, additional
images, feed caches or Git history without the local archive.

## First sync: prevent duplicate contacts

Do not bulk-import `contacts.vcf` into an existing address book as a substitute
for reconciliation. Importers may create duplicates or rewrite identifiers.
The initial synchronization must download complete destination cards and
produce a local plan before uploading anything.

1. Match a previously recorded destination association, its UID, or the
   pair `X-SORTAL-STORE`/`X-SORTAL-ID`. Conflicting or duplicate identity
   evidence blocks automatic action.
2. For unlinked records, use exact email and account identifiers to propose
   candidates. Normalize email domains, but do not invent provider-specific
   equivalences such as removing dots or plus suffixes. Shared emails and
   contradictory evidence require review; they are not automatic merges.
3. Compare primary and alternate names against destination `FN`, `N` and
   `NICKNAME`, with Unicode normalization, case folding and whitespace
   normalization. **Any unresolved matching name blocks creation.** Show the
   candidates for review, including records without an email address. This
   avoids creating another card with an existing name while acknowledging
   that two different people can share that name. Fuzzy names can be surfaced
   too, but cannot prove identity or guarantee detection of all duplicates.
4. Classify each source contact as `link/update`, `create`, `needs review` or
   `unchanged`. Flag pre-existing destination duplicates separately. A reviewer
   can explicitly authorize a distinct person with the same name. Never merge
   or delete existing duplicates automatically.
5. For a confirmed link, keep the server UID and href, preserve all remote
   fields, add the Sortal extensions, and persist the identity association.
   New UUIDs are used only for confirmed new contacts. Linking multiple Sortal
   records to one server card requires explicit resolution as well.

The existing native client at
`bleeding/idk/carddav/eio/carddav_eio_client.mli` supplies discovery, listing,
full fetches, conditional writes and sync reports. Its Fastmail quirks module
already recognizes vCard 3.0 conventions. The reconciliation, durable state
and merge policy described here are new work; the offline exporter does not
claim to perform them.

## Fastmail initial-sync trial

The trial discovers the personal collection through `/.well-known/carddav`,
checks supported formats and maximum resource size, and fetches the full
destination address book before planning. It rejects incomplete or failed
DAV listings. An uncertain name/email/account match is held; it also holds
overlapping names within the source root. Existing contacts are never updated
or deleted by this tool. A repeated run recognizes unchanged previously
uploaded UIDs and requires reconciliation for changed cards.

Each new card uses `If-None-Match: *`, followed by GET and complete Sortal
field/photo reconstruction. The returned UID, store identity and strong ETag
are checked. A conservative property comparison also checks that every emitted
property survives, including unannotated display fallbacks; casing of standard
`TYPE`, `VALUE` and `ENCODING` parameter tokens is normalized. Extra server
properties are retained in the readback. One contact must pass before the remaining uploads begin, with
at most two requests in flight. A failed write or readback stops new uploads;
already completed writes remain, with their outcomes in the local journal.
Interrupted or uncertain writes must be reconciled by inspecting the account
again, not replayed with unconditional PUT.

```sh
python3 avsm/sortal/tools/carddav_trial.py BUNDLE \
  --username ACCOUNT --password-file PASSWORD_FILE --report NEW_REPORT_DIR
python3 avsm/sortal/tools/carddav_trial.py BUNDLE \
  --username ACCOUNT --password-file PASSWORD_FILE --report ANOTHER_REPORT_DIR \
  --apply
```

The first command is read-only; the second authorizes conditional creations.
The password is read at runtime and is never included in logs or reports.
Credentials are restricted to the configured HTTPS server origin, including
redirects; Fastmail is the default.
Reports contain private contact data and use a mode-0700 parent directory.
Use a Git-ignored report directory inside the private bundle. Each report
includes the discovery result, full plan, previous cards, and saved readbacks
with per-contact outcomes and ETags. These are initial-sync evidence and a
starting baseline; they are not a complete two-way synchronization database.

On 2026-09-11 the test account's Personal collection was empty. The plan
selected 460 creations and held two possible duplicate pairs. Their identities
remain in the private reports. The trial deliberately leaves both members of
each unresolved pair out of the destination.

All 460 planned creations succeeded and passed GET readback, including every
emitted property, complete reconstructed contact values, and the 166 selected
photos belonging to those contacts. Fastmail normalized `PHOTO;ENCODING=b` to
`ENCODING=B` on some cards without changing the image bytes. The independent
native parser also accepted all 460 returned cards and decoded all 166 photos.
A fresh full-address-book fetch found exactly 460 contacts; the repeated plan
was **0 create, 460 unchanged, 4 review**. No existing cards were overwritten
and no source files were changed. The remaining three selected photos belong
to held contacts and remain in the complete offline bundle.

The source export and native parser round-trip checks cover all 464 contacts
and 169 photos; the live trial covers
460 contacts and 166 photos. The ignored compatibility bundle contains
`fastmail-seed/report.json`, the fresh `fastmail-recheck/report.json`, and all
before/after representations. This proves initial server storage and repeat
planning for this dataset. It does not prove lossless edits through Fastmail's
web editor, phones, or other clients, and no automatic two-way merger is
running.

## Pulling an edited Fastmail contact

`tools/carddav_pull.py` compares the full fetched card with its saved server
baseline, then compares the resulting field edits with the corresponding
Sortal baseline and current YAML. The current importer supports primary names,
kind, email lists and an explicit set of additional properties, including
nicknames, phones, notes and birthdays. Unsupported changes to mapped fields
are conflicts. Source fields absent because a client removed their annotations
are recovered from the baseline, not treated as deletions.

The first live pull on 2026-09-11 found one edited contact. Fastmail
added a work email and nickname while removing `FN` and kind annotations,
regrouping the URL and photo, adding `PROP-ID`, and uppercasing kind. The
importer recognized those representation changes and retained the original
name, kind, link and local photo. It added the email to `emails`, the nickname
to `vcard`, and an email overlay retaining its work/preference parameters and
server property ID. Only that contact's YAML changed among the original 899
files. The updated YAML passed the native Sortal schema and vCard round-trip
checks. Contact names and addresses remain in the private pull journal.

Install `tools/requirements.txt` for the Python tools; pull additionally uses
`ruamel.yaml` for YAML editing. First fetch a fresh snapshot with the trial
tool **without `--apply`**, then prepare the pull:

```sh
python3 avsm/sortal/tools/carddav_pull.py prepare BUNDLE \
  --snapshot FRESH_INSPECTION_REPORT --source ~/bushel/sortal \
  --output NEW_PULL_JOURNAL
python3 avsm/sortal/tools/carddav_pull.py apply NEW_PULL_JOURNAL \
  --username ACCOUNT --password-file PASSWORD_FILE
```

Preparation writes reviewable before/after YAML, a separate `common.yaml`,
the baseline and current remote cards, and the projected vCard. It refuses
output inside the source, fetched snapshot or previous contact/journal trees.
Apply checks all prepared hashes,
refetches the remote card and its strong ETag, checks the local file for
concurrent edits, and atomically replaces that one YAML file. It never writes
to the CardDAV server. Interrupted application can finish the same journal
without duplicating the local changes. Full remote cards and the source
before-image remain available for recovery.

On subsequent pulls, pass `--previous-pull LAST_APPLIED_JOURNAL` to `prepare`.
Applied journals retain the new common YAML/card baseline and follow a chain
of previous journals for unchanged contacts. `common.yaml` advances only the
fields received from the server; unrelated local edits remain in `after.yaml`
without being mistaken for already synchronized data. Later conflicting edits
still require resolution. Older journals without a separate common file remain
readable using their saved after-image. The repeat plan after the live
pull was **0 remote changes, 0 local updates**. The current tool uses complete
snapshots, requires the same set of linked remote UIDs, and rejects new/deleted
contacts for separate reconciliation. It does not implement general pushes,
photo changes, arbitrary affiliation changes, or background synchronization.

### vCard passthrough contract

The existing native Sortal schema represents `vcard` as a YAML mapping from
strings to strings. Keys are unfolded vCard property headers, including groups
and parameters; values are wire-format property values, with vCard escaping.
Repeated properties need distinct headers, for example distinct group names.
This is field-specific data rather than a serialized complete card:

```yaml
emails:
  - casey@example.invalid
vcard:
  NICKNAME;PROP-ID=nick: Caz
  EMAIL;TYPE=WORK,PREF;PREF=1;PROP-ID=email-id: casey@example.invalid
```

An `EMAIL` passthrough entry overlays the unique matching native email instead
of emitting a duplicate. Other supported entries become additional vCard
properties. Reserved identity and mapped-field overrides are refused. A stale
email overlay whose address no longer matches `emails` causes an error; until
general merge support is implemented, edit both representations together.

Export mapping 3 binds each passthrough property to a grouped
`X-SORTAL-VCARD-KEY`, retaining its original header. Its field path uses a
percent-encoded header segment; the inverse uses the marker to reconstruct
the exact original YAML key. Properties sharing an original group keep a
shared emitted group, including phone/email labels; repeated property names
within that group are distinguished by their parameters. Standard properties retain their actual values
and parameters, including edits, without an opaque contact payload. Full
server representations remain in the pull journal to preserve admin metadata
and support later merging.

## Subsequent synchronization and loss prevention

Persist the server/account/collection identity, store UUID, per-contact
handle-to-UID/href binding, ETag, last fetched full vCard, last synchronized
Sortal record, its exported projection, and collection sync token. Keep this
state in a local durable directory outside the contact root, independent of
fields an editing client might strip. Export bundles are immutable recovery
snapshots, not the mutable synchronization database.

Use complete vCard objects for merging, preserving unknown properties,
groups, parameters and repetitions. Sortal's `vcard` mapping uses the
header/value contract above; merely reconstructing a card from recognized
Sortal fields without its passthrough and full server baseline would lose
telephone numbers, birthdays, notes and other existing server data.

Compare the current Sortal record and the current remote card against the
last successfully synchronized versions. Compare semantic property values
against the stored projection so line folding, property reordering and other
equivalent serialization changes do not count as edits. Keep the full original
vCard representation alongside those comparisons to retain unknown data.

| Change since the last common version | Action |
| --- | --- |
| Only Sortal changed a mapped field | Apply it to the remote card while retaining other remote properties. |
| Only the remote card changed a mapped field | Apply it to the Sortal record while retaining Sortal-only metadata. |
| Each side changed different fields | Merge both changes and synchronize the merged result. |
| Both sides made the same semantic change | Accept it once. |
| Both sides changed the same field differently, or an edit conflicts with a removal | Record a conflict and leave that contact's local and remote versions intact until resolved. |
| An extension disappeared while the identity remains linked | Retain the local metadata, flag the loss, and reconcile visible edits before restoring the extension. |

An edit to primary `FN` updates the primary Sortal name without erasing
aliases. Use the field annotations and common baseline to distinguish name
variants, and retain genuine remote structured names and nicknames separately.
The generated unsplit `N` does not authorize replacing all source names when
a client rewrites it.
Remote email changes retain types and preference parameters, which Sortal's
string list cannot represent by itself. Repeated fields need per-entry
matching against the baseline: adding an email must not revive a different
email that was explicitly removed. Ambiguous entry matches are conflicts.

Account URLs are derived data. A changed GitHub URL can update the account
only when its platform and handle can be parsed unambiguously. Preserve other
new URLs as links. Changing an AT Protocol profile URL does not authorize
rewriting its cached DID; resolve or flag any identity inconsistency. Editing
a workplace must not erase past affiliations. A new remote photo is saved
locally with the previous original retained in history. Preserve fields such
as telephone numbers, birthdays and notes through the full vCard passthrough,
even when the Sortal UI does not yet expose them.

Local writes must retain unknown YAML members and untouched source content;
re-encoding only the typed V2 contact is insufficient. Preserve original
versions before edits, and use a YAML representation that can update selected
fields without dropping unknown members or comments. Regenerate the mapped
vCard properties from the merged values after successful reconciliation. Keep
the original remote property AST and local baseline so stripped extensions
can be recovered without overwriting newer visible edits. There is no embedded
YAML snapshot to refresh or use as the import authority.

For new remote contacts in the selected address book, apply the same identity
and duplicate-name checks before creating a Sortal record. Assign a unique
local handle once, bind it to the existing remote UID, and preserve all remote
fields. Changing a display name must not regenerate the handle or create a
second contact. Deletions start as reviewed tombstones. A missing field in a
partial response is not a deletion; only complete successfully fetched cards
and confirmed resource removals can establish one.

Create new cards with `If-None-Match: *`; update linked cards with `If-Match`
and their last ETag. A 412 means refetch and reconsider the change. Refetch
after successful writes to record the actual stored representation and ETag.
Advance the sync token only after all changes and conflicts are durably
recorded. [RFC 6352 sections 6.3.2.3 and 9.2](https://www.rfc-editor.org/rfc/rfc6352.html#section-9.2).

Journal each planned merge and its source versions before applying local or
remote writes. Commit the new common baseline only once both writes and the
remote readback have succeeded. Recovery from interruption must replay or
reconcile the journal, preserving the existing identity binding, rather than
creating another card or treating a half-applied merge as the common version.

Fastmail's server is `https://carddav.fastmail.com/`; use discovery to locate
the selected collection. Its developer documentation specifies an app
password for personal CardDAV integrations. Keep credentials outside contact
metadata and manifests. [Fastmail server settings](https://www.fastmail.help/hc/en-us/articles/1500000278342-Server-names-and-ports),
[Fastmail authentication documentation](https://www.fastmail.com/dev/).

Before enabling two-way synchronization, use a synthetic contact to test
edits through Fastmail and each intended phone/desktop client. Check UID, extensions,
Unicode, aliases, multiple emails, grouped affiliations, feed settings,
account identities and photo contents.
Discover the collection's supported formats and maximum resource size. A
server accepting an initial PUT is not evidence that every later client edit
will preserve all of these fields.
