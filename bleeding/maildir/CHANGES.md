## Unreleased

`0ca78db3d` Keywords owns the `dovecot-keywords` letter table in both
directions, tolerates blank lines and a missing final newline, and
names the offending line when a mapping cannot be read.

`e7b5fbfdb` Dotlock now runs on Eio and raises `Lost` when its lock
file is removed or replaced, instead of leaving the lock silently
invalid.

`fa2e83445` Scanning and staging skip dotfiles and non-regular
directory entries, and `find` no longer fails on an unrelated
malformed name. Appending a message with no keywords takes one lock
instead of reading the keyword map, and several redundant fsyncs on
temporary files are removed.

`9b0b82631` Publishing a message and changing its flags both use
`rename`, and the metadata lock still refuses a duplicate identity
before the rename.

`0b58aaf34` The Dovecot metadata lock now raises its own
`Metadata_lock_busy` instead of sharing `Writer_lock_busy` with the
application writer lease.

`e5a4c796c` `check_append` reports, without writing, whether a
pending append's flags or date would be rejected, so a caller such
as Bridge can check before it takes the writer lease.

`97b0820c9` Maildir is a standalone package. `Imap_maildir` is now
`Maildir`, with its own `dune-project`, opam file and README.

`0791e4d65` `occurrence` no longer carries `internal_date`. `append`
and `check_append` take `?mtime` in POSIX seconds instead, and the
paged inventory (`with_inventory_pages`, `inventory_find`,
`inventory_page`, `upload_internal_date`) is replaced by a new
`fold`.

`68af6a250` `with_writer` replaces `with_writer_lock`, granting a
`writer` capability that `append`, `check_append`, `set_flags`,
`remove` and `recover` now require. A writer used after its callback
returns raises `Writer_expired`. `open_dir`, `scan`, `fold`, `find`,
`append`, `check_append` and `set_flags` return `(_, error) result`
over one `error` type with `pp_error`, in place of `Failure`.
`Dotlock` and `Keywords` are public submodules.

`39d8a46a7` The package gains an odoc index page and a compiled
`writer.ml` example.

`62d068812` The opam file lists `eio_main` as a test-only
dependency.

`796c75184` The README points at the odoc pages and the writer
example.

`f5c84e74d` Every interface (`Maildir`, `Dotlock`, `Keywords`,
`Maildir_error`) is fully documented under the doc-style rules. No
signature changed.

`d22b216e3` The README is rewritten to the current layout and names
the metadata and writer lock exceptions.

`5c4f7363c` `check_append` reads the keyword map under the metadata
lock when the flags include a keyword, so it can now raise
`Metadata_lock_busy` or `Metadata_lock_lost`. Another keyword writer
can still fill the free slots after it returns.

`1f57fbd49` `Keywords`, its errors and `pp_error` are declared
`@ portable`.

The in-process writer registry is an atomic `Base.Set` of directory
inodes instead of a mutex-guarded `Hashtbl`.

`of_writer` is portable. `Dotlock` and the other `Maildir` functions
are not, since they call `Eio.Path` or `Eio_unix.run_in_systhread`.

The library stanza lists its dependencies one per line, as `dune fmt`
writes them.
