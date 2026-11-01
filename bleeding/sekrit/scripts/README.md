# Migrate service credentials

Run as the service account on each host, with the same `HOME`, XDG and app path
overrides used by its services. Migration makes no requests to configured mail,
WebDAV or model endpoints.
It uses `uv`, Python 3.9 or newer, and a pinned TOML parser to preserve unrelated
configuration and comments. `uv` may download the Python environment and parser
on first use, including during a preview.

Build the updated binaries in this checkout:

```sh
dune build bleeding/sekrit/bin/main.exe avsm/crowthebot/bin/main.exe
bleeding/sekrit/scripts/migrate-services.sh --dry-run
bleeding/sekrit/scripts/migrate-services.sh --apply
```

The script prefers these local binaries, then installed commands on `PATH`.
`--sekrit-bin PATH` and `--crowthebot-bin PATH` select explicit executables.
To use it outside a checkout, copy both `migrate-services.sh` and
`migrate_services.py` into the same directory and install the updated binaries.
Crow is required only on hosts with Crow secret configurations.

The script discovers:

| Service | Credentials migrated |
| --- | --- |
| Crow | Every profile under its config directory's `secrets/` directory |
| JMAP | Literal bearer/basic credentials and private-file references in profiles |
| Dooit | WebDAV inline passwords, password files and existing secret references |
| Sortal | Configured CardDAV password file, including its default location |
| Termanil | CardDAV password file and any linked Dooit configuration |
| Bushel | HTTP embedding API key file |

Standard config files are discovered using XDG and the apps' `*_CONFIG_DIR`
overrides. Those app overrides name a **parent directory**: for example,
`SORTAL_CONFIG_DIR=/srv/config` selects `/srv/config/sortal/config.toml`.
`--dooit-config`, `--sortal-config`, `--termanil-config`, `--bushel-config` and
`--jmap-profiles` add explicit locations and may be repeated. Sortal resolves
relative password files against its working directory. Pass `--sortal-cwd DIR`
when that differs from the directory where you run this script.

Named secrets use deterministic names under `migration/`. Each import is read
back and compared before its reference is published. Existing names must have
the same value. They are never overwritten. JMAP basic-auth references contain
the complete `user:password`, so the old separate `user` field is removed.
Existing named references are verified too. No values appear in output or argv.

`SEKRIT_KEY_FILE` and `SEKRIT_STORE_DIR` retain their normal meanings. Use the
same overrides in service environments. Each host initializes its own key unless
you supply an existing one. Nothing is copied between hosts. Existing encrypted
Crow files or named references with a missing key block initialization.

The script leaves credential source files intact. Before rewriting configs, it
stores their original bytes in a private directory under
`${XDG_STATE_HOME:-$HOME/.local/state}/sekrit/migrations/`. Its `manifest.json`
maps each backup to the original path and records a checksum. **These backups
contain plaintext credentials.** Keep them private and remove obsolete backups
and credential source files after verifying your services. Back up the master
key separately.

Migration is not a transaction across all services. A failure retains completed
steps and prints the backup directory if one was created. Fix the reported
problem and rerun the same command. Imports and Crow migration are repeatable.
For manual recovery, the manifest identifies the original config files to
restore. Do not restore over subsequent edits. Avoid editing configurations or
rotating credentials while migrating. The script detects changed sources and
serializes other instances of itself, but does not lock out every app's editor.

Existing processes may retain credentials until restarted. Restarts are opt-in
and occur only after all migration steps succeed:

```sh
./migrate-services.sh --apply --restart-systemd-user crowthebot.service
./migrate-services.sh --apply --restart-launchd "gui/$(id -u)/org.example.crowthebot"
```

Both options may be repeated. The systemd option restarts only active user units.
The launchd option requires a target in the current user's `gui/UID` or
`user/UID` domain. A restart failure returns a failure status while retaining the
completed migration.

Matrix device/session storage and OwnTracks' own credentials are outside this
migration. Credentials supplied solely through environment variables or service
command-line arguments are not discovered. Those service definitions require
explicit changes. Crow's imported named configuration values remain snapshots.
Later rotation requires re-importing and restarting Crow.

Integration tests use temporary homes, fake restart commands and real Sekrit and
Crow binaries. They do not read live configuration or call remote services:

```sh
uv run --no-project --script bleeding/sekrit/scripts/test_migrate_services.py
```
