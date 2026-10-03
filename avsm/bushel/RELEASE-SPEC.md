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
  url : string;           (* the release page on the forge *)
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

All take `--config`, `--data-dir` and `--dry-run` as other bushel commands do.

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
   sentence of at most 120 characters, else the release title.
5. Writes the summary it chose, so the author sees what was registered.

Registering a release that is already registered updates it in place.

`bushel release refresh [--days N]` re-queries ecosyste.ms for releases made in
the last `N` days (default 90) and attaches registries that have appeared since.
It never removes a registry and never changes a summary or a date.

`bushel release list` prints the registered releases, newest first.

## Sources

### GitHub

- `GET /repos/{org}/{repo}/releases` lists releases. Each gives `tag_name`,
  `name`, `published_at`, `html_url`, `draft`, `prerelease` and `author.login`.
- `GET /repos/{org}/{repo}/releases/tags/{tag}` gives one release.
- Drafts are skipped. Prereleases are discoverable and are registered only when
  asked for by tag.
- `date` is the date part of `published_at`.
- `version` is `tag_name` with a leading `v` removed. `tag` is kept only when it
  differs.
- `GET /users/{login}/events/public` yields `ReleaseEvent` records for
  discovery. It reaches back about a month, so older releases are found with
  `--repo`.
- A token is read from `GITHUB_TOKEN`, never from the configuration file.

### Tangled

Tangled has no release record. A release is an artifact attached to a tag, held
as `sh.tangled.repo.artifact` in the author's atproto repository.

1. `https://<handle>/.well-known/atproto-did` gives the DID.
2. `https://plc.directory/<did>` gives the PDS endpoint.
3. `<pds>/xrpc/com.atproto.repo.listRecords?repo=<did>&collection=sh.tangled.repo.artifact`
   lists the artifacts.

An artifact has `name`, such as `dune-rpc-eio-0.1.0.tbz`, and `createdAt`. The
`tag` field is a raw object hash and carries no version. The version is the
name with the repository name prefix and the archive suffix removed. An
artifact whose name does not have that shape is not registerable without
`--force`. Several artifacts sharing one version are one release. `date` is the
date part of `createdAt`.

### ecosyste.ms

The `ecosystems` library of this repository is used.

- `Ecosystems.PackageWithRegistry.lookup_package ~repository_url` returns every
  package built from a repository, across registries.
- `Ecosystems.Version.get_registry_package_versions` confirms that a registry
  carries the version.
- A registry entry's `url` is the version's `registry_url`.

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
- The commands over a loopback server serving recorded responses, in the manner
  of the `oecosystems` tests.
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

`Bushel.Release` and its tests exist in the old shape. Everything else in this
document is not started.
