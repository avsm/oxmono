# Sortal schema V2

Replace the V1 contact schema with a model built on a single notion of an
account, drop the machinery that 460 contacts never used, and add per-platform
probes that verify a handle is real.

## Problem

V1 offers four ways to record where a person is reachable online. A GitHub
account may be a `services` entry with `kind: github`, or a bare `urls` entry
pointing at `github.com`. An ORCID may be the top-level `orcid` field or a
`urls` entry pointing at `orcid.org`. A Bluesky account may be the `atproto`
block, a `services` entry with `kind: bluesky`, or a `urls` entry pointing at
`bsky.app`. All four spellings occur in the live database.

A `services` entry also stores its `url`, `kind` and `handle` together, so the
same fact is written three times and nothing checks that the three agree. Two
entries hold a bare handle in the `url` field.

## Evidence

Measured across the 460 contacts in `$XDG_DATA_HOME/sortal`.

| observation | count |
|---|---|
| contacts with any temporal `range`, `from` or `until` | 0 |
| contacts using `organizations` | 2 |
| files serialising `organizations:` as null | 173 |
| `services` with `primary`, every one `false` | 45 |
| `service.label` entries, 5 of which restate `kind` | 6 |
| contacts with exactly one `urls` entry | 313 |
| contacts with two `urls` entries | 3 |
| contacts with no `urls` entry | 144 |
| GitHub as a `service` versus as a bare `urls` entry | 63 versus 1 |
| LinkedIn as a `service` versus as a bare `urls` entry | 4 versus 10 |
| ORCID as a field versus as a bare `urls` entry | 14 versus 3 |
| contacts with more than one email | 1 |
| distinct `kind` spellings for Mastodon | 2 |

Two results shape the design. Nothing uses temporal ranges, so the entire
`Sortal_schema_temporal` query surface is dead. And `urls` is not a general
list of URLs, it is a homepage field with a long tail of account URLs that
belong in `services`.

Projecting the whole database through the model below promotes 10 LinkedIn, 4
Google Scholar, 3 ORCID and 1 GitHub URL out of `urls`, maps every existing
`services` entry without residue, and produces no contact with two handles on
one platform. Total YAML shrinks from 4412 to 3655 lines.

Note that `*.github.io` is a personal site, not a GitHub account. Twenty of the
twenty-one `urls` entries containing the string `github` are `github.io` pages
and must stay in `links`.

## Model

A contact is an identity, the accounts it is reachable through, and the context
around it.

### Accounts

An account is a handle on a platform. The URL is derived from the two and is
never stored. Platforms fall into three shapes, which is what keeps a closed
variant tractable.

```ocaml
(** Platforms whose URL derives from a bare handle. *)
type simple =
  | Github | Gitlab | Codeberg
  | Orcid | Scholar
  | Twitter | LinkedIn | Threads | Instagram | Flickr

(** Platforms federated across instances. An account is a user at a host. *)
type federated =
  | Mastodon | Pixelfed | PeerTube
  | Matrix | Zulip | Discourse

(** Front ends onto a single AT Protocol identity. *)
type app = Bluesky | Tangled | Standard_site

type atproto = {
  handle : string;      (** the AT Protocol handle, such as ["anil.recoil.org"] *)
  did : string option;  (** cached resolution, [None] until a probe fills it *)
  apps : app list;      (** front ends this identity is reachable through *)
}

type account =
  | Simple of simple * string             (** platform, handle *)
  | Federated of federated * string * string  (** platform, user, host *)
  | Atproto of atproto
```

Splitting by shape makes illegal states unrepresentable. `Github` cannot carry
a host, `Mastodon` cannot lack one, and only `Atproto` carries apps. Adding a
platform touches the constructor list and one specification function rather
than every match site.

This is the answer to assigning an AT Protocol handle and then saying which
services it is usable through. The handle and the DID identify the person once,
and `apps` lists the front ends.

### Contact

```ocaml
type kind = Person | Organization

type t = {
  version : int;                  (** 2 *)
  kind : kind;
  handle : string;                (** unique, the file stem *)
  names : string list;            (** primary first, never empty *)

  emails : string list;           (** preferred first *)
  accounts : account list;
  links : link list;              (** pages that are not accounts *)

  affiliations : affiliation list;
  photo : string option;          (** local filename or URL *)
  feeds : feed list;

  vcard : (string * string) list; (** reserved, see "vCard mapping" *)
}

and link = {
  url : string;
  label : string option;
}

and affiliation = {
  org : string;
  department : string option;
  title : string option;
  url : string option;
  address : string option;
  from : Date.t option;           (** inclusive *)
  until : Date.t option;          (** exclusive *)
}
```

Affiliations are the only place a date survives. Employment history is the one
case where a range means something a reader would act on, and it is the field
`draft-madhavapeddy-vcard-temporal-00` already specifies `X-VALID-FROM` and
`X-VALID-UNTIL` for.

### What V2 removes

| removed | reason |
|---|---|
| `services`, `urls`, `orcid`, `atproto` as separate fields | one fact, four spellings |
| `service.url`, `service.label` | derived, and 5 of 6 labels restate the platform |
| `service.primary` | 45 occurrences, all `false` |
| `email.type` | one contact has two emails, and vCard `TYPE=` is optional |
| `icon` | 2 uses against 169 for `thumbnail`, and vCard `PHOTO` takes either |
| `Temporal.range`, `valid_at`, `overlaps` | no contact has a range |
| `email_at`, `emails_at`, `organization_at`, `url_at`, `services_at` | unreachable without ranges |
| `current_email`, `current_url`, `current_organization`, `current_organizations`, `current_services` | become plain accessors |
| eleven per-platform accessors such as `twitter`, `mastodon`, `zulip` | one `account` lookup by platform |

`Sortal_schema_temporal` becomes `Sortal_schema_date`, holding only
`parse_date_string` and `format_date` over `Ptime.date`.

## Serialisation

`accounts` is a YAML mapping keyed by the platform key. A scalar value is the
handle. A sequence value is several handles on that platform. A mapping value
carries per-platform fields, which today only `atproto` has.

No contact in the live database has two handles on one platform, so the
sequence form exists only so that a second account does not force a schema
change. It costs one decoder combinator, because `account list` already permits
repeated constructors.

```yaml
version: 2
kind: person
handle: avsm
names:
  - Anil Madhavapeddy
  - A. Madhavapeddy
emails:
  - anil@recoil.org
  - avsm2@cam.ac.uk
accounts:
  github: avsm
  orcid: 0000-0001-8954-2428
  twitter: avsm
  linkedin: anilmadhavapeddy
  instagram: avsm
  flickr: avsm
  threads: avsm
  mastodon: avsm@amok.recoil.org
  peertube: anil@crank.recoil.org
  discourse: avsm@discuss.ocaml.org
  matrix: avsm@recoil.org
  zulip: Anil Madhavapeddy@eeg.zulipchat.com
  atproto:
    handle: anil.recoil.org
    did: did:plc:nhyitepp3u4u6fcfboegzcjw
    apps: [bluesky, tangled]
links:
  - https://anil.recoil.org
  - https://www.cst.cam.ac.uk/people/avsm2
affiliations:
  - org: Pembroke College
    title: Fellow
    url: https://www.pem.cam.ac.uk
    address: "T1, Pembroke College, Cambridge CB2 1RF"
photo: avsm.jpg
feeds:
  - type: atom
    url: https://anil.recoil.org/news.xml
```

The typical contact is five lines.

```yaml
version: 2
kind: person
handle: ablake
names:
  - Andrew Blake
links:
  - https://royalsociety.org/people/andrew-blake-11097/
photo: ablake.jpg
```

Encoding rules:

- Omit empty lists, empty mappings and `None`. V1 writes `emails:`,
  `organizations:` and `services:` as null in most files.
- A `links` entry with no label encodes as a bare string rather than a mapping
  with a single `url` key. 318 of 319 entries have no label.
- Federated accounts encode as `user@host` whatever the platform's native
  spelling. Matrix renders as `@user:host` only when a URL is derived. The user
  part is split on the last `@`, because a Zulip user part may itself contain
  spaces and punctuation. The live database has
  `Anil Madhavapeddy@eeg.zulipchat.com`, which is a display name rather than a
  handle, and which no URL can be derived from. Zulip therefore derives a host
  URL only, and its probe is `Skipped`.
- An unknown key under `accounts` is a decode error. This is the point of the
  closed variant, and it is what would have caught the `url: mdales` entry.

## Declaring a platform

Adding a platform is two edits in `sortal_platform.ml` and nothing else.

```ocaml
type 'url spec = {
  key : string;              (** the YAML mapping key *)
  url : 'url;                (** derive the canonical URL *)
  syntax : string -> (unit, string) result;  (** local validation *)
  probe : probe;             (** how to check the account exists *)
}

val simple_spec : simple -> (string -> string) spec
val federated_spec : federated -> (user:string -> host:string -> string) spec
```

Both are total matches, so adding a constructor fails the build until its row
exists. One record holds the key, the URL template, the syntax check and the
probe, so there is no second table to forget. Parameterising `spec` over the
URL template keeps a handle-only platform from being given an instance
template. A genuine one-off that does not deserve a platform goes in `links`
with no code change.

## Probes

A probe answers whether a handle names a real account. Its result is not a
boolean.

```ocaml
(** How a platform is checked. Declared once, in the platform's [spec]. *)
type probe =
  | Syntax                    (** the local check is the whole check *)
  | Status                    (** GET the derived URL, 2xx means present *)
  | Json of string * string   (** GET a URL template, require a member *)
  | Webfinger                 (** RFC 7033, for the federated platforms *)
  | Atproto_did               (** resolve the handle to a DID *)
  | Unverifiable of string    (** the platform refuses automated checks *)

type verdict =
  | Present of {
      url : string;
      display_name : string option;
      did : string option;      (** AT Protocol only *)
    }
  | Absent of string        (** definitive, such as 404 or an empty WebFinger *)
  | Inconclusive of string  (** 403, rate limit, timeout, TLS failure *)
  | Skipped of string       (** the platform refuses automated checks *)

val probe :
  sw:Eio.Switch.t ->
  fetch:Fetch.plain ->
  account ->
  verdict
```

`Inconclusive` and `Skipped` are load bearing. LinkedIn, Twitter, Instagram,
Threads, Flickr, Scholar and Zulip all refuse unauthenticated automated access,
which is 9 real accounts in the live database today and grows with every
LinkedIn URL promoted out of `urls`. Collapsing those to `false` would report
them as fake, and would be actively destructive the moment anything grows a
flag that acts on the result.

`probe` takes a `Fetch.read_only` client, so a probe cannot mutate anything by
construction. Callers wrap it in `Fetch.with_limits ~min_interval
~max_concurrent`, which bounds per origin, and `Fetch.with_retry`.

| platform | method | library |
|---|---|---|
| Mastodon, Pixelfed, PeerTube | WebFinger, `acct:{user}@{host}` | `webfinger` |
| Atproto | handle to DID, then per app | `atp`, `fetch` |
| Bluesky | `app.bsky.actor.getProfile` on the DID | `atp` lexicon `bsky` |
| Tangled | profile lookup on the DID | `atp` lexicon `tangled` |
| Standard\_site | profile lookup on the DID | `atp` lexicon `standard-site` |
| Orcid | MOD 11-2 checksum, then `pub.orcid.org/v3.0/{id}` | `fetch` |
| Github | `api.github.com/users/{handle}`, or `HEAD github.com/{handle}` | `fetch` |
| Gitlab, Codeberg | status on the derived URL | `fetch` |
| Matrix | `.well-known/matrix/server`, then the client profile endpoint | `fetch` |
| Discourse | `https://{host}/u/{user}.json` | `fetch` |
| LinkedIn, Twitter, Threads, Instagram, Flickr, Scholar, Zulip | `Skipped` | none |

The syntax check runs before any request, so a mistyped ORCID or a
single-segment AT Protocol handle fails offline.

GitHub is the one platform where the volume matters. The database holds 64
GitHub accounts and `api.github.com` allows 60 unauthenticated requests an
hour, so a cold `check --all` cannot use the API alone. Default to
`HEAD https://github.com/{handle}`, which distinguishes 200 from 404 without
the API budget, and use the API only when `GITHUB_TOKEN` is set, where it also
yields a display name.

### AT Protocol resolution

Per the AT Protocol handle specification, a handle resolves to a DID two ways.
A DNS TXT record at `_atproto.{handle}` holds `did=did:plc:...`. An HTTPS GET of
`https://{handle}/.well-known/atproto-did` returns the bare DID as plain text.
When both exist and disagree, the DNS result is authoritative.

V2 implements the HTTPS route and falls back to
`com.atproto.identity.resolveHandle` through `atp`. DNS TXT is deferred because
it needs a `dns-client` dependency that sortal does not otherwise have. This is
a deliberate deviation from the specification: for a handle published only in
DNS, the well-known route 404s and resolution falls to the XRPC endpoint, whose
answer sortal cannot itself verify. Record it and revisit.

Handle syntax is checked locally first. A handle is ASCII, at most 253
characters, and has two or more dot-separated segments of 1 to 63 characters
drawn from letters, digits and hyphens, where no segment starts or ends with a
hyphen and the final segment does not start with a digit. Handles are compared
case-insensitively after lowercasing.

The AT Protocol probe is also the resolver. A successful check yields the DID.

### Where results go

The DID is part of the identity and is written back into the contact file.
Every other probe result is derived and goes to a cache under `XDG_CACHE_HOME`,
keyed by platform and handle with a timestamp.

Probe results must not enter the data directory. It is git versioned, and
writing a check timestamp into 460 files on every run would leave `git log`
useless for seeing what actually changed.

### Command surface

    sortal check [HANDLE]     verify one contact's accounts
    sortal check --all        verify every contact, honouring the cache
    sortal check --refresh    ignore cached verdicts

Output is one row per account with its verdict. `--all` over the live database
today is 104 network probes plus 2 per-app requests, with 9 accounts skipped.
That is small, but it concentrates on a few origins: GitHub alone accounts for
64 of them, which is above the unauthenticated rate limit of 60 per hour. The
cache and the per-origin limits both matter for that reason rather than for
total volume.

## Migration

A `sortal migrate` command rewrites all 460 files in place and commits the
result. The V1 reader is retained only until it has run. The store is git
versioned, so the rewrite is recoverable.

Rules:

- `services` becomes `accounts`, dropping `url`, `primary` and `label`. Where
  `handle` is absent, derive it from the URL tail. Where the URL is not a URL,
  treat it as the handle. This covers `mdales` and `emils`.
- `kind: mastodon` and `kind: activitypub:mastodon` both become `mastodon`.
  `activitypub:peertube` becomes `peertube`.
- `kind: photo` splits by host into `instagram` and `flickr`.
- `kind: bluesky` becomes `atproto` with `apps: [bluesky]`.
- The `orcid` field becomes `accounts.orcid`.
- The `atproto` block becomes `accounts.atproto`, with its `services` becoming
  `apps`.
- A `urls` entry whose host is `github.com`, `gitlab.com`, `codeberg.org`,
  `orcid.org`, `linkedin.com`, `uk.linkedin.com`, `twitter.com`, `x.com`,
  `instagram.com`, `flickr.com`, `threads.com` or `scholar.google.com` becomes
  an account. Every other entry becomes a `links` entry. Hosts under
  `github.io` are personal sites and stay in `links`.
- `emails` keeps its addresses in order and drops `type`.
- `organizations` becomes `affiliations`, with `name` renamed to `org`.
- `icon` and `thumbnail` collapse into `photo`.
- Empty collections are dropped rather than written as null.

The migration must be a pure function from a V1 record to a V2 record, tested
against a golden copy of the live database so the diff can be reviewed before
it is committed. It must fail loudly rather than silently drop a field it does
not recognise.

## vCard mapping

Not built now. Every field is chosen to be representable so that adding it
later needs no version bump.

| sortal | vCard | source |
|---|---|---|
| `handle` | `UID` | RFC 6350 |
| `kind` | `KIND` | RFC 6350 |
| `names` | `FN`, cardinality `1*` | RFC 6350 |
| `emails` | `EMAIL`, first gets `PREF=1` | RFC 6350 |
| `links` | `URL`, label via `GROUP` and `X-ABLabel` | RFC 6350 |
| `photo` | `PHOTO` | RFC 6350 |
| `accounts` | `SOCIALPROFILE;SERVICE-TYPE={key};USERNAME={handle}:{url}` | RFC 9554 |
| `affiliations` | `ORG`, `TITLE`, bound by `GROUP` | RFC 6350, draft-madhavapeddy-vcard-temporal-00 |
| `affiliation.from`, `.until` | `X-VALID-FROM`, `X-VALID-UNTIL` | draft-madhavapeddy-vcard-temporal-00 |
| `feeds` | `X-FEED` | draft-madhavapeddy-vcard-temporal-00 |
| `atproto` | `X-ATPROTO` | draft-madhavapeddy-vcard-temporal-00 |

`SOCIALPROFILE` takes a URI value with `SERVICE-TYPE` and `USERNAME`
parameters, which is exactly platform, handle and derived URL. Deriving the URL
rather than storing it costs nothing on export, because it can be regenerated.

The `vcard` field holds vCard properties that have no sortal field, unfolded
and verbatim, so a future import and re-export round trips without loss. It is
written only by an importer and is never hand edited. Properties expected to
land there include `TEL`, `ADR`, `BDAY`, `ANNIVERSARY`, `GEO`, `TZ`, `LANG`,
`CATEGORIES`, `NOTE`, `RELATED`, `MEMBER`, `N` and `PRONOUNS`.

An inbound `SOCIALPROFILE` whose `SERVICE-TYPE` is not a known platform goes
into `vcard` verbatim rather than being lost, and re-exports unchanged. This is
what makes the closed platform variant safe for a future two-way sync.

## Specification impact

`avsm/sortal/spec/draft-madhavapeddy-sortal-00.txt` normatively describes the
V1 model. Sections 2 (Temporal Range), 4 (Email Object), 5 (Organisation
Object), 6 (URL Entry Object), 7 (Service Object) and 8 (AT Protocol Object)
all change, along with Appendix A (JSON Schema) and Appendix B (ABNF for
Service Kind Strings). The draft needs a `-01` revision alongside the code.

`draft-madhavapeddy-vcard-temporal-00` needs no change. Its `X-VALID-FROM`,
`X-VALID-UNTIL`, `X-FEED` and `X-ATPROTO` definitions are what the mapping
above uses.

`draft-madhavapeddy-sortal-impl-00.txt` needs review for the same reasons.

## Consumer impact

`bushel`, `arod` and `tessabot` hold roughly 60 call sites against the V1
accessors, dominated by `Contact.name`, `Contact.handle`, `Contact.url` and
`Contact.best_url`. The per-platform accessors they use (`github_handle`,
`bluesky_handle`, `twitter_handle`, `linkedin`, `mastodon`, `zulip`,
`discourse`, `services_of_kind`) all become one lookup by platform.

Keep `Contact.name`, `Contact.handle` and `Contact.best_url` with their current
signatures. They are the bulk of the call sites and their meaning does not
change.

## Out of scope

- Reading or writing vCard, and speaking CardDAV. V2 only reserves room.
- DNS TXT resolution of AT Protocol handles.
- Phone numbers and structured addresses. Neither appears in the database.
- `PRONOUNS` from RFC 9554. Worth revisiting, but nothing needs it today.
- A V1 compatibility reader beyond what `sortal migrate` requires.
