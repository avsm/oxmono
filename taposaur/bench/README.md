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

## Ceph metadata investigation

Measured on 2026-10-24 around 16:15–16:22 UTC while the 32-worker detail
backfill continued. The client was blocked on metadata-server requests.
Local disk latency was below 1 ms and indexer CPU use was about 3% of one
core. The kernel client reported a cumulative mean metadata latency of
528 ms versus 3.1 ms for file reads. These counters cover the mount, not
only the indexer.

Two samples of the rank-0 daemon exporter showed 47,000–55,000 inode
insertions and 47,000–56,000 evictions per second. Over 23.8 seconds,
lookup requests averaged 1.054 seconds and total requests averaged 76/s.
Over another 89 seconds there were 1,557 full directory-fragment fetches
and zero keyed fetches. The capability-acquisition throttle counter did
not increase. This is evidence of metadata-cache churn, not a measured
network-bandwidth or local-SQLite bottleneck.

Rank 0 (`cephfs.alkaia.jcixhl`) held approximately 8.9 million inodes with
41.5 GiB process RSS. Its configured cache target was 32 GiB. RSS includes
memory beyond the metadata cache. Rank 1 held only 16,417 inodes with
183 MiB RSS. The rank-0 host had about 28 GiB available RAM, negligible
swap-in traffic and no swap-out traffic during the sample. All 86 OSDs
were up, their CRUSH class was SSD, and all PGs were active and clean,
including PGs being scrubbed. A large metadata-pool omap warning and
scrubbing were present, but their contribution was not established.

The effective `mds_dir_prefetch` setting was `true`. In Ceph 19.2.6,
[`CDir::fetch`](https://github.com/ceph/ceph/blob/v19.2.6/src/mds/CDir.cc)
uses a keyed fetch for a named, nonsnapshot lookup when this setting is
false. Otherwise it fetches the directory fragment. Whole-fragment loads
combined with eviction are the leading explanation for the amplification.
This diagnosis has not been confirmed by changing the setting.

The importer reads in UID order. Adjacent UIDs jump among 256 hash
directories. The sampled `10.1002/fe` directory contained 3,709 papers.
Explicit and ephemeral directory pins were unset at each visible ancestor
from `/data/papers` through this hash directory. Ancestors above the mount
root were not inspected.

The read-only profiler compares interleaved, disjoint groups with and
without a directory listing before resolution:

```sh
dune exec --profile release-check taposaur/bench/ceph.exe -- \
  /data/papers/taposaur /local/catalog.db \
  doi:10.1002/ajmg.b.33000 128 8
```

Append `doi/10.1002/fe.jsonl` to select records from one archive shard.
The optional shard is read completely before selecting the sample.
The profiler writes no catalogue records. It reports per-stage latency
and reopens each manifest to measure immediate warm-cache access.

Across 128 UID-ordered papers, the two baseline groups processed 32 papers
in 38.7 and 35.1 seconds. Directory-prefetch groups took 34.5 and 34.8
seconds. Prefetch mostly moved the delay into `readdir`. Cold manifest
medians were 1.56–1.80 seconds versus 0.57–0.80 ms immediately warm.
Initial fingerprints accounted for about 58–61% of summed stage time.

A separate 128-paper sample within one hash directory improved as the
directory warmed: successive groups took 20.9, 12.6, 8.3 and 7.4 seconds.
The middle groups used directory prefetch. This supports testing directory
locality, but different papers and changing server load prevent treating
the ratio as a production speedup estimate.

Next experiments are a bounded server-side trial of keyed fetches, MDS
workload distribution, and shard-local application scheduling. Increasing
workers alone does not address the observed cache churn. No cluster
configuration or running importer settings were changed during diagnosis.

## Directory-ordered enrichment

`ordering.exe ARCHIVE SOURCE_DB NEW_BENCH_DIR SHARD_PREFIX` compares actual
enrichment in UID and directory order, including cache encoding and database
publication. It selects 128 records from each of 16 shards under the prefix,
assigns disjoint groups of four shards to four passes, and writes only to
new scratch catalogues. Pass order is UID, directory, directory, UID, with
32 workers. The archive and source catalogue are read-only.

On 2026-10-25, with `doi/10.1007/` and the existing backfill running:

| Pass | Order | Papers | Seconds | Papers/second |
| --- | --- | --- | --- | --- |
| 1 | UID | 512 | 52.209 | 9.81 |
| 2 | Directory | 512 | 53.333 | 9.60 |
| 3 | Directory | 512 | 57.159 | 8.96 |
| 4 | UID | 512 | 52.666 | 9.72 |

All 2,048 papers published complete detail caches with no unavailable
manifests. This small comparison establishes no throughput improvement.
Each pass touches only four hash directories, compared with 256 for the
production UID scan. A warm repeat of the first 512 records completed in
0.97 seconds and exercised concurrent CLI progress updates.

Directory scans use `papers_shard (shard=? AND rowid>?)`, as confirmed by
SQLite's query plan. No schema migration or additional index is required.
They feed up to 512 candidates into the bounded worker pool and complete
the current shard before advancing. Saved details and failed attempts
remain the durable resume state. Source fingerprints and guarded catalogue
publication are unchanged. The CLI retains `--order uid` for comparison
and rollback.

The live backfill switched to directory order at 07:04:42 UTC on 2026-10-25,
retaining 513,924 cached papers and 32 workers. After 129 seconds it had
published 3,511 more details and reached `doi/10.1002/02.jsonl`: 27.2
papers/second versus about 6.9/second over the preceding overnight run.
This short observation spans three hash directories. It is encouraging but
does not establish sustained whole-corpus throughput or a completion date.
