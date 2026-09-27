# Resource-scale regression

Run the real Maildir inventory and bridge journal paging paths with 100,001
external Maildir occurrences, including duplicate bytes and standard flags:

```sh
opam exec --switch=5.2.0+ox -- dune runtest --force --profile release-check bleeding/imap/test/scale
```

`IMAP_SCALE_COUNT` defaults to `100001`; `IMAP_SCALE_JOURNAL_COUNT` defaults
to `1201`. To exercise a larger journal, for example:

```sh
IMAP_SCALE_COUNT=100001 IMAP_SCALE_JOURNAL_COUNT=10001 \
  opam exec --switch=5.2.0+ox -- dune runtest --force --profile release-check bleeding/imap/test/scale
```

For the opt-in million-occurrence gate, including 100,001 pair and operation
journal rows, run the executable directly so the fixture counts are passed
unambiguously to the test process:

```sh
IMAP_SCALE_COUNT=1000001 IMAP_SCALE_JOURNAL_COUNT=100001 \
  opam exec --switch=5.2.0+ox -- dune exec --profile release-check \
    bleeding/imap/test/scale/test_scale.exe
```

The test removes its temporary Maildir and SQLite database on exit. Allow
several minutes and enough free inodes for over one million files. It prints
elapsed time and process high-water RSS after fixture creation, inventory
pagination, journal creation, and read-only reopening.

On the development host, the million-occurrence / 100,001-journal-row gate
passed in 267 seconds including fixture cleanup. The Maildir inventory raised
process VmHWM by 4,376 KiB; the full test raised it by 13,660 KiB, reaching
22,204 KiB before cleanup. These figures describe this host and should be
compared with future runs on comparable hardware.

The test checks complete, strictly ordered pagination, indexed identity lookup,
100 indexed Maildir imports into the populated fixture, bounded-memory startup recovery,
terminal-operation exclusion, and reopening the journal read-only. On Linux it
reports the increase in process high-water RSS during Maildir staging and
pagination and rejects an increase above 256 MiB. This is a coarse regression
bound, not an allocation profile. The separate store test stages and publishes
100,001 remote UIDs through the disk-staged FETCH/SEARCH path.
