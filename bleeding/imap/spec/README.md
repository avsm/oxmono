# IMAP implementation references

See the root [IMAP-SPEC.md](../../../IMAP-SPEC.md) for the implementation plan,
feature priorities, repository references and test requirements.

`sources.json` records the URL, retrieval date, byte length and SHA-256 checksum
of 60 RFC texts, the pinned OBJECTID+ Internet-Draft revision -06, and three IANA
registry snapshots, initially retrieved on 2026-09-26. RFC 1951 and RFC 5267 were added on
2026-09-27 (their entries have per-document retrieval dates). These are unmodified reference
documents; their original copyright and license notices apply. The draft is
work in progress. RFC 8620/8621 already exist in `../../jmap/spec/`.

Verify the corpus offline from the monorepo root:

```sh
python3 - <<'PY'
import hashlib, json, pathlib
root = pathlib.Path('bleeding/imap/spec')
manifest = json.loads((root / 'sources.json').read_text())
for entry in manifest['sources']:
    body = (root / entry['file']).read_bytes()
    assert len(body) == entry['bytes'], entry['file']
    assert hashlib.sha256(body).hexdigest() == entry['sha256'], entry['file']
print(f"Verified {len(manifest['sources'])} reference documents")
PY
```

`cyrus-probe.json` is a research observation, not an OCaml test result or a
portable golden transcript. It records an independent Python IMAP/HTTP/LMTP
probe against an isolated copy of the existing JMAP Cyrus image. Generated
mailbox IDs, UIDVALIDITY values and MODSEQs are intentionally not assertions for
future runs. The probe container and synthetic data were removed after testing;
the existing `jmap-oracle` was left unchanged. IMAP-SPEC.md describes the exact
fixture coverage and the reproducible integration harness to implement.

Before implementing an RFC feature, consult its current RFC Editor errata.
Verified RFC 9698 erratum 8635 corrects the example command to GETJMAPACCESS.
The RFC 7162 errata lookup was rate-limited during this research; this corpus is
not an exhaustive errata audit.

The RFC 5267 errata lookup was attempted on 2026-09-27, but the RFC Editor
errata endpoint was inaccessible through the research tool. Its text and the
RFC 9394 update were consulted; this is not a verified absence of errata.
