# Catalogue measurements

Measured on 2026-10-23 with the `release-check` build and local storage.
These are warm local measurements, not a full-corpus capacity estimate.

The reproducible `catalogue.exe` benchmark populated 250,000 synthetic papers
across 256 shards. Every paper contains the queried FTS word, exercising a
broad match. Each read operation was measured 101 times.

| Operation | Result |
| --- | --- |
| Import | 37.483 seconds |
| Statistics | p50 0.121 ms, p95 0.137 ms |
| Search | p50 124.131 ms, p95 127.918 ms |
| Browse | p50 0.762 ms, p95 0.962 ms |

The production DOI shard `doi/10.1016/06.jsonl` imported 17,074 rows in
2.36 seconds. The PMC shard `pmc/446/3e.jsonl` imported two rows in 0.36
seconds. Both were read from the existing archive into a fresh local
catalogue, with zero import errors. Total corpus coverage remains unknown. A bounded enrichment of 100 papers
completed in 17.07 seconds with eight concurrent readers: 99 complete results,
one partial result and no unavailable manifests. A subsequent source check
found no changes. Validation of 100 papers and their artifact sizes passed.

Still measure full-corpus import time, database size, cold reads and
metadata enrichment throughput before planning a complete deployment.

## Enrichment concurrency

Run the stage profiler against an existing catalogue and a new scratch database:

```sh
dune exec --profile release-check taposaur/bench/enrichment.exe -- \
  /data/papers/taposaur /local/catalog.db /local/new-benchmark.db
```

The source catalogue and archive are read-only. The benchmark publishes only
to the scratch database. It divides sampled records into disjoint groups for
each worker count to avoid repeatedly reading the same cached files. It reports
wall time, process CPU time and summed worker time for each stage. Worker times
overlap and must not be added to estimate elapsed time at concurrency above one.

On 2026-10-23, while the full import was running, 768 DOI records from the
available Springer and Elsevier holdings gave these results. Each setting read
192 distinct papers. Filesystem caches were not explicitly cleared, and the
samples are not representative of every publisher or artifact combination.

| Workers | Elapsed seconds | Papers/second | CPU seconds |
| --- | --- | --- | --- |
| 1 | 173.474 | 1.11 | 1.361 |
| 8 | 49.607 | 3.87 | 1.338 |
| 32 | 19.557 | 9.82 | 1.435 |
| 64 | 21.283 | 9.02 | 1.500 |

The single-worker run spent 40.212 seconds loading manifests and 129.824
seconds obtaining initial artifact fingerprints. Reading and parsing documents
took 2.446 seconds, and catalogue publication took 0.391 seconds. Archive
metadata operations dominated. These measurements favour 32 workers over 64.
They do not establish whole-corpus completion time or sustained throughput
when publishing into the larger live catalogue.

### Artifact freshness I/O

Add `--compare-fingerprint` to compare the previous open/fstat checks with
directory-entry stat checks. Four disjoint groups run with 32 workers in
old/new/new/old order. This avoids rereading the same artifact, but cannot
eliminate shared directory caches or changes in archive load.

| Group | Method | Papers | Elapsed seconds |
| --- | --- | --- | --- |
| 1 | Open/fstat | 250 | 46.200 |
| 2 | Directory-entry stat | 250 | 39.163 |
| 3 | Directory-entry stat | 250 | 28.859 |
| 4 | Open/fstat | 250 | 18.403 |

These results do not establish a throughput improvement. The strong change
over time also affected manifest loading, which was identical in both methods.
The new implementation removes per-artifact opens and reuses one directory
handle per fingerprint. Cold archive metadata access remains the bottleneck.
Compatibility tests verify unchanged stamps for regular files, ctime-only
invalidation, missing files and rejection of symlink targets.

## Detail cache size estimate

`cache_size.exe ARCHIVE CATALOGUE` measures a proposed JSON representation
containing the manifest and each document's bibliography or parsing error.
Together with the already stored original index row, the pure resolver can
reconstruct the detail view without archive reads. The estimate avoids
duplicating artifact records inside every field's provenance. It includes
neither document bodies nor event history.

On 2026-10-24, 115 papers selected from eight DOI/PMC ranges averaged 1,820
bytes, with a median of 1,460 bytes, a 95th percentile of 3,767 bytes and a
maximum of 5,174 bytes. At 25,820,739 papers the mean extrapolates to 47 GB
of JSON before SQLite overhead. These are small, nonrandom samples using
the current readers. Fuller publisher support and other article populations
can increase the size. Allow approximately 75–100 GB of additional storage
for planning until a broader sample or an actual cache build is measured.
