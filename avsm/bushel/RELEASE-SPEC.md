# Release tracking

Bushel records what has been written. This adds a record of what has been
released, so that the notes view in arod can show a discreet line when a
release is made.

A release is a version published from a repository on GitHub or tangled. It may
also reach package registries such as PyPI and opam. Bushel registers the
release once, with a one-line summary, and links to the release page and to the
ecosyste.ms metadata.

## Decisions

**Releases are side data, not entries.** An entry has a slug, a body, a page
and backlinks. A release has none of these, and a page per release would put
thin pages into the sitemap, the search index and the feeds. Releases follow
`links.yml` and `doi.yml`. A release is not addressable as `[:slug]` in prose.

**A registered release is a forge release.** The forge is where the release is
made, so its publication date is the date of the release. Registries are
attachments to that release, because a registry version lands later and
ecosyste.ms lags it further. A release registers with only its forge link and
gains registries on a later `refresh`. There is one notes line per release, not
one per registry.

**Ownership is read from the forge.** A GitHub release records who published
it, and `author.login` is the release author whatever organisation owns the
repository. A tangled artifact lives in the author's own atproto repository.
ecosyste.ms cannot answer who released, because opam maintainers are not
indexed and a repository's owner is often an organisation.

**Registration is explicit.** `bushel release discover` proposes and
`bushel release add` registers. Nothing registers during `bushel pull`, so
nothing reaches the site that was not confirmed.

**Only registries that the author publishes to are attached.** ecosyste.ms
also reports repackagings such as nixpkgs and guix. They are other people's
packaging and would drown a line meant to be discreet. A configured list names
the registries to attach.

**The summary is one line, written or chosen at registration.** It is not
derived at render time, so a page never changes because a registry
description did.

## Data model

`Bushel.Release`, in `lib/bushel_release.mli`.

```
type forge = Github | Tangled

type registry = {
  name : string;     (* as ecosyste.ms names it: pypi.org, opam.ocaml.org *)
  package : string;  (* the package's name on that registry *)
  url : string;      (* the registry's page for this version *)
}

type release = {
  version : string;       (* no leading v *)
  tag : string option;    (* the forge's tag, where it differs from version *)
  date : Ptime.date;      (* when the forge published it *)
  summary : string;       (* one line *)
  url : string;           (* the release page, or the repository on tangled *)
  registries : registry list;
}

type t = {
  repo : string;                (* org/name, or handle/name on tangled *)
  forge : forge;
  project : string option;      (* bushel project slug *)
  releases : release list;      (* newest first *)
}
```

`metadata_url r release` is the ecosyste.ms page for a registry entry,
`https://packages.ecosyste.ms/registries/<name>/packages/<package>/versions/<version>`.
It is derived, not stored. A release with no registries has no metadata link.

`version` is a string and never a number. Bare `4.10` in YAML reads back as the
float `4.1`. The reader coerces a number back to a string because a hand-edited
file will not quote.

## File format

`releases.yml`, in the data directory beside `links.yml`. A missing file is
empty. A malformed file is an error, because a command that merges and writes
back would otherwise replace a good file with nothing.

```yaml
- repo: realworldocaml/mdx
  forge: github
  releases:
    - version: 2.6.0
      date: 2026-07-22
      summary: Executable code blocks inside markdown files.
      url: https://github.com/realworldocaml/mdx/releases/tag/2.6.0
      registries:
        - name: opam.ocaml.org
          package: mdx
          url: https://opam.ocaml.org/packages/mdx/mdx.2.6.0/
- repo: ucam-eo/geotessera
  forge: github
  project: tessera
  releases:
    - version: 0.10.2
      tag: v0.10.2
      date: 2026-09-04
      summary: Python interface to the Tessera geospatial embeddings.
      url: https://github.com/ucam-eo/geotessera/releases/tag/v0.10.2
      registries:
        - name: pypi.org
          package: geotessera
          url: https://pypi.org/project/geotessera/0.10.2
```

## Commands

All take `--config` and `--data-dir` as other bushel commands do. `add` and
`refresh` also take `--dry-run`.

`bushel release discover [--repo REPO] [--since DATE]` lists releases that the
author published and that are not registered. It prints one line per candidate,
giving the repository, version, date and forge. `--repo` backfills the full
history of one repository.

`bushel release add REPO TAG [--summary TEXT] [--project SLUG] [--force]`
registers a release. For GitHub, `TAG` is the git tag. For tangled it is the
version. The command:

1. Fetches the forge release for the date and URL.
2. Refuses a release the author did not publish unless `--force` is given.
3. Asks ecosyste.ms which configured registries carry that version, and
   attaches them.
4. Takes `--summary`, else the package description from ecosyste.ms cut to one
   sentence of at most 120 characters, else the release title unless that is
   only the version, else the repository's name and the version. The
   description is the attached package's, and otherwise the package of an
   allowed registry, so a release no registry carries yet still has one.
5. Writes the summary it chose, so the author sees what was registered.

Registering a release that is already registered updates it in place. The date
and URL are refreshed. The summary is kept unless `--summary` is given, and the
registries are kept and added to, so a lookup that fails removes nothing.

`bushel release refresh [--days N]` re-queries ecosyste.ms for releases made in
the last `N` days (default 90) and attaches registries that have appeared since.
It never removes a registry and never changes a summary or a date.

`bushel release list` prints the registered releases, newest first.

## Sources

### GitHub

- `GET /repos/{org}/{repo}/releases` lists releases. Each gives `tag_name`,
  `name`, `published_at`, `html_url`, `draft`, `prerelease` and `author.login`.
- `GET /repos/{org}/{repo}/releases/tags/{tag}` gives one release.
- Drafts are skipped. `discover` does not list prereleases and `add` registers
  one when asked for by tag.
- `date` is the date part of `published_at`.
- `version` is `tag_name` with a leading `v` removed. `tag` is kept only when it
  differs.
- `GET /users/{login}/events/public` yields `ReleaseEvent` records for
  discovery. It reaches back about a month, so older releases are found with
  `--repo`.
- A token is read from `GITHUB_TOKEN`, never from the configuration file.

### Tangled

Tangled has no release record. A release is an artifact attached to a tag, held
as `sh.tangled.repo.artifact` in the author's atproto repository. The atp
libraries in this repository read it.

1. `Xrpc.Identity.did_of_handle` resolves the handle with
   `https://<handle>/.well-known/atproto-did`.
2. `Xrpc.Identity.pds_of_did` reads the DID document for the data server.
3. `Tangled.Api.list_artifacts` lists the artifacts of the repository.

An artifact points at a repository record. A repository is named by the `name`
of that record, or by its record key if the record has none, so
`ocaml-json-pointer` is a different name from the package `json-pointer` that
it releases. `Tangled.Api.artifact_version` reads the version from the file
name, which has the shape `package-version.tbz`. It starts at the first dash
that is followed by a digit. An artifact whose name has no version is not a
release. Several artifacts sharing one version are one release. `date` is the
date part of `createdAt`. The `tag` field of the record is a hash, not a
version, and is not used.

### ecosyste.ms

The `ecosystems` library of this repository is used.

- `Ecosystems.PackageWithRegistry.lookup_package ~repository_url` returns every
  package built from a repository, across registries. A GitHub repository is
  looked up as `https://github.com/org/name` and a tangled repository as
  `git+https://tangled.org/handle/name`, which is how ecosyste.ms stores it.
- `Ecosystems.VersionWithDependencies.get_registry_package_version` confirms
  that a registry carries the version, and a 404 means it does not.
- A registry entry's `url` is the version's `registry_url`.
- A registry with several packages for one repository uses the one named after
  the repository, with or without a leading `ocaml-`, and otherwise the first.

opam is indexed as `opam.ocaml.org`. Its dates are not used, because they differ
from the forge's. mdx 2.6.0 was published on GitHub on 2026-07-22 and is dated
2026-08-06 by ecosyste.ms.

## Configuration

A `[releases]` section beside `peertube_servers`.

```toml
[releases]
github_user = "avsm"
github = ["mirage/ocaml-cohttp", "ucam-eo/geotessera", "realworldocaml/mdx"]
tangled = ["anil.recoil.org/dune-rpc-eio"]
registries = ["pypi.org", "opam.ocaml.org", "npmjs.org", "crates.io"]

[releases.projects]
"ucam-eo/geotessera" = "tessera"
```

`github` and `tangled` name repositories that `discover` scans in addition to
the author's events. `registries` defaults to the list above.

## Rendering

arod's notes view at `/notes`.

- `Arod.Ctx.releases` is the registered releases, loaded by the bushel loader
  from `releases.yml`.
- A release is one line inside the month section of its date, ordered with the
  note cards. A month with releases and no notes still appears.
- The line has the date, the repository name and version, the summary, and one
  tag per registry. The name and version link to the release page. Each registry
  tag links to the ecosyste.ms metadata for that registry. The summary truncates
  with an ellipsis rather than wrapping.
- The weeknote rail is unchanged.
- Releases do not appear in the Atom feed, the JSON feed, the search index, the
  sitemap or the markdown export.

## Compatibility

`Bushel.Release` exists with a different shape, a `source` per row, a `name`
and a `synced_at`. Nothing else uses it. This change replaces its types and
codec and updates `test/test_release.ml`.

With no `releases.yml`, every page renders byte-identically to before.
`avsm/arod/test/render_capture.sh` before and after proves it.

## Testing

- Codec round trips, including versions YAML would mangle, a release with no
  registries, and `merge`.
- Parsers for GitHub releases, GitHub events and tangled artifacts, over
  recorded responses.
- Registry attachment over ecosyste.ms fixtures, covering a registry that does
  not yet carry the version and a repackaging that is filtered out.
- The decisions of `add` and `refresh` as functions with stubbed lookups:
  registering again, refusing a release that is not the author's, and the
  refresh window. The commands themselves are checked by hand against live data
  and not by a test.
- A render test that a release appears in its month, that a release-only month
  appears, and that the page is unchanged without releases.

## Not doing

- Bare tags. A repository that tags without releasing yields nothing.
- Release notes. The URL is recorded.
- A local clone of opam-repository, and discovery by PyPI maintainer. Every
  release of the author reaches opam and PyPI through a GitHub or tangled
  release.
- Release entries, with a slug, a page or a `[:slug]` reference.
- Showing repackagings.

## Status

Implemented, with tests over recorded responses and stubbed lookups:

- `Bushel.Release`, its `releases.yml` codec and merge.
- The `[releases]` configuration section.
- Parsers for GitHub releases and events, and Tangled artifacts as candidates.
- Registry attachment through ecosyste.ms.
- `bushel release list|discover|add|refresh`.
- `Arod.Ctx.releases`, and the release line in the notes view.

Added to the atp libraries because they are generally useful:
`Xrpc.Identity`, `Tangled.Api.list_artifacts` and `artifact_version`, and a fix
so that `$bytes` is read with or without base64 padding.

Not verified: the served site. arod did not start on the author's data because
the contact store needs a one-off sortal migration, so the page was checked by
rendering the notes list in a test and not by fetching it.
