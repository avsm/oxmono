# Arod

## Restore a website

`arod pull` restores repositories, local configuration, service logins and
generated assets. Its built-in public defaults describe Anil's website:
Bushel data and images on fork.recoil.org, Sortal contacts, StandardSite,
ActivityPub on amok.recoil.org, Karakeep on hoard.recoil.org, three PeerTube
servers, and a Zotero translation server at localhost:1969.

On the working machine:

```sh
arod pull --scan
arod pull --scan --export website.toml
```

Copy the manifest to the new machine, then run:

```sh
arod pull --manifest website.toml --dry-run
arod pull --manifest website.toml
```

Without `--manifest`, the command reads `$XDG_CONFIG_HOME/arod/pull.toml`.
If it is absent, the command scans local configuration and fills absent
configuration with the built-in defaults. The first application saves this
manifest, including when some steps remain incomplete. Subsequent pulls
inspect actual state and retry missing work. Existing manifests and exported
files are never overwritten. To change the saved setup, edit `pull.toml`.
An explicit manifest applies to that invocation only if a saved manifest
already exists.

Install arod, Git and ImageMagick before processing assets. ActivityPub OAuth
login also needs the `apub` executable. PDF transfer needs `rsync`. Install or
point `bushel.zotero.translation_server` at a running Zotero translation server
when DOI resolution is needed. The command does not install system packages
or launch daemons. Git uses the target machine's SSH configuration and keys.

The manifest contains typed public settings, repository origins and branches,
home-relative paths, and selected service account/profile identifiers. It does
not contain session tokens, passwords, signing keys, API keys, access logs or
arbitrary files from XDG directories. Unknown settings are excluded. Keep
secret settings such as `server.stats_password` in the local arod config.
Scan output counts untracked and ignored repository files that need separate
review. A Git clone cannot restore them.

The scanner uses each application's current profile, not every account on the
machine. StandardSite and ActivityPub use their XDG config stores. Karakeep's
existing library uses `~/.config/karakeep`, even with a custom XDG config root.
The manifest never relocates credentials. Authentication verifies existing
sessions online, prompts before replacing an unverifiable session, and selects
the verified profile for subsequent CLI commands. Password and API-key prompts
disable terminal echo. ActivityPub signature identities need explicit key
provisioning with `apub auth setup`; they cannot be recovered through OAuth.

Paper PDFs currently have no default remote source. Put `pdf_source` at the
top level of the manifest, before its first table:

```toml
pdf_source = "old-machine:bushel/papers"
```

A local directory or an SSH rsync directory source is supported. Only PDF
files and their directories are copied. Existing files are preserved. Without
a source, existing PDFs are checked and a missing PDF directory is reported
as incomplete. Processed images are rebuilt from the original image repository.

Pulls preserve existing configuration and reject differing arod or Bushel
processing settings. Repository operations use the manifest's origin and
branch, while existing applications' sync settings are preserved. Existing
checkouts must have that origin and branch and a clean working tree. Updates
are fast-forward only. Divergence, a failed fetch, and a non-repository
destination are errors. Failed clones use a `.arod-clone` sibling directory
which must be inspected and removed before retrying. Sortal's XDG data path
is linked to its checkout only if absent or already mapped there.

Repositories and Sortal data are restored before loading entries. Processing
runs paper thumbnails, contact portraits, video thumbnails, Srcsetter, and
DOI resolution, then checks the website context and rebuilds the search index.
These operations can modify local content repositories. Commit or stash those
changes before the next pull. No posts or publications are created. Karakeep
bookmark creation is excluded from this pipeline.

`--dry-run` performs no writes, logins or network requests. `--non-interactive`
disables login prompts and Git/rsync SSH password prompts, and reports missing
authentication as incomplete. `--skip-auth` leaves logins unchanged and reports
them as unchecked. `--skip-process` retains existing assets but still checks
serving readiness. A nonzero exit status means a required step is incomplete.
Serving and authentication have separate status lines.
