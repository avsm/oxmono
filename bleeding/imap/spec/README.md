# IMAP implementation references

See [acceptance requirements](../ACCEPTANCE.md) for verification scope and
[production gates](../PRODUCTION-GATES.md) for outstanding evidence.

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

`cyrus-probe.json` is a historical Python IMAP/HTTP/LMTP observation, not
an OCaml test result or a portable golden. See the
[current Cyrus oracle](../test/oracle/README.md) for IMAP/JMAP round trips
and fixture configuration. Corpus integrity does not prove interoperability.

Before implementing an RFC feature, consult its current RFC Editor errata.
Verified RFC 9698 erratum 8635 corrects the example command to GETJMAPACCESS.
The RFC 7162 errata lookup was rate-limited during this research; this corpus is
not an exhaustive errata audit.

The RFC 5267 errata lookup was attempted on 2026-09-27, but the RFC Editor
errata endpoint was inaccessible through the research tool. Its text and the
RFC 9394 update were consulted; this is not a verified absence of errata.
