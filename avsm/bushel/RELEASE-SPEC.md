# Release tracking

A design for recording code releases in bushel, covering GitHub, tangled and
the package registries that carry the same code.

## Purpose

Bushel records what has been written. It does not record what has been
shipped. This adds a `releases.yml` holding, for each tracked repository, the
dated versions released from it, so the site can answer "what did I release
and when" without that being reconstructed by hand.

## Decisions

These were settled before the design was written down. The rationale matters
more than the choice, because it is what a later change has to argue with.

**Releases are side data, not entries.** Bushel entries are `Paper`,
`Project`, `Idea`, `Video` and `Note`. Each has a slug, a body, a page and
backlinks. A release has none of those, there will be hundreds of them, and a
page per release would put thin pages into the sitemap, the search index and
the feed for no gain. Releases follow the pattern of `links.yml` and
`doi.yml`: externally synced facts with provenance, referring to entries
rather than being referred to. A release is therefore not addressable as
`[:mirage-4.5.0]` in prose. If that is wanted later it is a real change of
shape, not a small extension.

**A record is a repository, not a release.** The repository is the stable
thing that maps to a project, so the organisation, the forge and the project
slug are stated once rather than on every version. A new release is a
one-line append inside an existing record, which keeps diffs small over the
years this file is expected to live.

**The set of tracked repositories is an explicit allowlist.** Taking every
repository under `avsm` would sweep in a long tail of forks and experiments,
and `ucam-eo` and `mirage` contain other people's work. Nothing appears
without being asked for.

**opam is not tracked.** The GitHub release or the tangled tarball is the
moment of release. opam-repository is downstream packaging that lands later
by pull request, and tracking it would put a second row against nearly every
OCaml release. This is the decision most likely to be revisited, since opam
is where an OCaml package actually becomes installable. Section
[Not doing](#not-doing) records what it would cost.

**Downstream repackagings are kept.** ecosyste.ms reports nixpkgs, guix,
alpine, debian and others alongside the canonical registry. These are other
people packaging the code rather than the author releasing it, but they
answer "where has this ended up", which is worth having. They are recorded as
ordinary rows distinguished by their `source`, not held apart in a separate
block.

## Data model

`Bushel.Release`, in `lib/bushel_release.mli`.

```
type forge = Github | Tangled

type source =
  | Forge                 (* released on the repository's own forge *)
  | Registry of string    (* carried by a registry, named as ecosyste.ms does *)

type release = {
  source  : source;
  version : string;          (* as its source names it, no leading v *)
  tag     : string option;   (* the forge's tag, where it differs *)
  date    : Ptime.date;
  name    : string option;   (* release title, where there is one *)
  url     : string option;
}

type t = {
  repo      : string;          (* org/name, or handle/name on tangled *)
  forge     : forge;
  project   : string option;   (* bushel project slug *)
  synced_at : Ptime.date option;
  releases  : release list;    (* newest first *)
}
```

A release cut on the repository's own forge and a version observed on a
registry are the same kind of fact, dated separately. They are separate rows
because a registry version rarely lands on the day the tag is cut, and
forcing them into one row would either lose a date or hide a publish that has
no matching tag.

`source` is absent in the file for a release cut on the forge, and is the
registry name otherwise. The set of registries is whatever ecosyste.ms
reports and is deliberately not enumerated in the type, so a new registry
needs no code change.

## File format

`releases.yml`, at the root of the data directory beside `links.yml`.

```yaml
- repo: mirage/mirage
  forge: github
  project: unikernels
  synced_at: 2026-08-31
  releases:
    - version: 4.10.0
      tag: v4.10.0
      date: 2026-03-04
      name: Mirage 4.10.0
      url: 'https://github.com/mirage/mirage/releases/tag/v4.10.0'
    - source: nixpkgs-24.11
      version: 4.4.1
      date: 2026-02-01
- repo: anil.recoil.org/dune-rpc-eio
  forge: tangled
  synced_at: 2026-08-31
  releases:
    - version: 0.1.0
      date: 2026-08-09
      name: dune-rpc-eio-0.1.0.tbz
```

### Field reference

| Field | Where | Required | Notes |
|---|---|---|---|
| `repo` | record | yes | `org/name`. The key the file is merged on. |
| `forge` | record | no | `github` or `tangled`. Defaults to `github`. |
| `project` | record | no | Slug of the project this repository serves. |
| `synced_at` | record | no | When the sync last read this repository. |
| `releases` | record | no | Newest first. Sorted on write. |
| `source` | release | no | Absent on the forge's own release. Else the registry. |
| `version` | release | yes | String, never a number. See below. |
| `tag` | release | no | Only where the tag differs from the version. |
| `date` | release | yes | `YYYY-MM-DD`. |
| `name` | release | no | Release title. |
| `url` | release | no | Canonical URL for the release. |

### Versions are strings

A version is always a string, never a number. Bare `4.10` in YAML reads back
as the float `4.1`, which would silently turn mirage 4.10 into 4.1. `yamlrw`
already quotes the strings that would be misread and leaves unambiguous ones
such as `4.5.0` and `v1.0` bare, so the writer does not need to force quotes.
The reader still has to accept a number, because a file edited by hand will
not quote, so `version_field` coerces a float or an int back to a string.

## Sources

### GitHub

`GET /repos/{org}/{repo}/releases`, paginated at 100 per page. Each release
gives `tag_name`, `name`, `published_at`, `html_url`, `draft` and
`prerelease`.

- Drafts are skipped. A draft is not a release.
- Prereleases are recorded. Whether to show them is a rendering decision, not
  a storage one.
- `version` is `tag_name` with a leading `v` stripped. `tag` is kept only
  when it differs from `version`.
- `date` is the date part of `published_at`, not `created_at`. The tag is
  often cut days before the release is published.
- Authentication is required in practice. Unauthenticated requests are capped
  at 60 an hour, which a few dozen repositories exhaust. A token in the
  environment lifts this to 5000.
- Repositories that use bare tags and no releases yield nothing. This is
  accepted: the decision was to track releases, not tags.

### Tangled

Tangled has no release record. A release is an artifact attached to a tag,
held as `sh.tangled.repo.artifact` in the author's atproto repository.

Resolution runs handle to DID to PDS to records:

1. `_atproto.<handle>` DNS TXT, or `https://<handle>/.well-known/atproto-did`,
   gives the DID.
2. `https://plc.directory/<did>` gives the PDS service endpoint.
3. `<pds>/xrpc/com.atproto.repo.listRecords?repo=<did>&collection=sh.tangled.repo.artifact`
   gives the artifacts.

An artifact record looks like:

```json
{
  "$type": "sh.tangled.repo.artifact",
  "name": "dune-rpc-eio-0.1.0.tbz",
  "repo": "at://did:plc:.../sh.tangled.repo/dune-rpc-eio",
  "tag": { "$bytes": "hfqO9uUkXe4qt0XtpiY8kX9iIJw" },
  "artifact": { "$type": "blob", "size": 7910, "mimeType": "application/x-bzip2" },
  "createdAt": "2026-08-09T13:21:57+03:00"
}
```

Three quirks the implementation has to handle:

- **The version is only in the filename.** `tag` is a raw object hash in
  `$bytes`, not `v0.1.0`. The version is parsed out of `name` by stripping
  the repository name prefix and the archive suffix, so
  `dune-rpc-eio-0.1.0.tbz` gives `0.1.0`. An artifact whose name does not
  match that shape is recorded with the filename as `name` and no `version`,
  rather than guessed at.
- **The `repo` pointer needs a second lookup.** Its rkey is sometimes the
  repository name, as in `.../sh.tangled.repo/dune-rpc-eio`, and sometimes an
  opaque tid, as in `.../sh.tangled.repo/3m6re5rp2ri22`. Resolving it means
  fetching the `sh.tangled.repo` record, which carries `name` in some records
  and relies on the rkey being the name in others.
- **Every artifact is a `.tbz`**, being dune-release output. Several
  artifacts may share one tag. They collapse to one release row per version.

`repo` for a tangled record is written `<handle>/<name>`, so
`anil.recoil.org/dune-rpc-eio`.

### Registries, via ecosyste.ms

One lookup per repository returns every package built from it, across the 100
registries ecosyste.ms indexes.

- `GET https://packages.ecosyste.ms/api/v1/packages/lookup?repository_url=<url>`
  gives package name, registry, `latest_release_number`,
  `latest_release_published_at` and `versions_count`.
- `GET https://packages.ecosyste.ms/api/v1/registries/<registry>/packages/<name>/versions`
  gives `number` and `published_at` per version.

`source` is the registry name as ecosyste.ms gives it, so `pypi.org`,
`npmjs.org`, `nixpkgs-24.11`, `guix`. No allowlist is applied, per the
decision to keep downstream repackagings.

**opam is not indexed by ecosyste.ms.** This was checked against the full
registry list. It is why opam needs its own source or none, and the decision
was none.

Observed shape: `mirage/mirage` returns 17 packages, of which 16 are nixpkgs,
guix and a `proxy.golang.org` entry that appears to be spurious. Expect
volume, and expect noise.

## Configuration

A `[releases]` section, alongside the existing `peertube_servers` and
`zotero_translation_server`.

```toml
[releases]
github = [
  "mirage/mirage",
  "avsm/ocaml-cohttp",
  "ucam-eo/tessera",
]
tangled = [
  "anil.recoil.org/dune-rpc-eio",
]
# Optional. Maps a repository to the project it serves, where the sync
# cannot infer it.
[releases.projects]
"ucam-eo/tessera" = "tessera"
"mirage/mirage" = "unikernels"
```

The GitHub token is read from the environment rather than the configuration
file, so a credential never lands in a file that is committed.

## Sync

A `Releases` step in `bushel pull`, joining
`Git | Images | Thumbs | Faces | Videos | Srcsetter | Links | Dois`, and
runnable alone with `bushel pull --only releases`.

Per run:

1. Load `releases.yml`. A missing file is an empty list. A malformed file is
   an error, not an empty list, because the step merges onto what it loads
   and writes the result back. Swallowing a parse failure would replace a
   good file with nothing.
2. For each repository in the allowlist, fetch from its forge, then from
   ecosyste.ms.
3. Build a record per repository with `synced_at` set to today.
4. `Release.merge existing incoming`. A repository in `incoming` replaces its
   counterpart. One only in `existing` is kept, so a partial or failed run
   does not drop repositories it did not cover.
5. Write the file back, sorted newest first.

A repository that fails to fetch is left as it was, with a warning. It is not
written as an empty record, which would delete its history.

`--dry-run` reports what would change without writing, as the other steps do.

## Rendering

Two surfaces in arod.

**Project pages.** A release appears as an activity row on the project named
by `project`, reusing the existing `project-activity-row` markup, so there is
nothing new to design. Rows are interleaved with the notes and papers already
in that stream.

**A `/releases` page.** Every release across all repositories, newest first,
grouped by year, filterable by project in the manner of `/ideas`.

```
2026
  tessera        v1.1.0    ucam-eo    Aug
  mirage         v4.10.0   mirage     Mar
2025
  cohttp         v6.1.0    mirage     Dec
```

Open, and worth settling before this is built: whether downstream
repackagings appear on `/releases` alongside the author's own releases, or
only on a repository's own detail. Keeping them was the decision for storage.
Showing sixteen nixpkgs rows against one mirage release would drown the page,
so the likely answer is that `/releases` shows `source: forge` and the
registries the author publishes to, with repackagings folded behind a count.

## Not doing

**opam.** Would need a clone of `ocaml/opam-repository`, reading versions
from `packages/<name>/` directory names and dates from `git log`, since
`ocaml.org/api/packages/<name>` is a 404 and there is no JSON API. That is an
extra sync step and a working clone for one registry.

**Bare tags.** A repository that tags without releasing yields nothing.

**Release notes.** The body of a GitHub release is not stored. It is often
long, sometimes generated, and the URL is recorded so it is one click away.

**Release entries.** No slug, no page, no `[:slug]` reference.

## Status

Implemented and tested:

- `Bushel.Release` in `lib/bushel_release.{ml,mli}`, with the types, the YAML
  codec, ordering, `latest` and `merge`.
- `test/test_release.ml`, 23 checks covering the round trip, the default
  `source`, ordering, the version cases that YAML would otherwise mangle, and
  that `merge` neither drops an uncovered repository nor duplicates one.

Not started: the configuration section, the three sync backends, the
`Releases` step, and both rendering surfaces.
