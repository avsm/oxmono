# Live interoperability observations

The opt-in [`test_live.ml`](test_live.ml) was run against
`https://myfiles.fastmail.com/` on 2026-09-08, after validating the same executable
against the pinned Apache fixture. These observations describe one account and
server deployment at that time. No credentials or existing file contents are
stored in this report.

Authentication, OPTIONS, depth-one PROPFIND and a bounded download succeeded.
There was one existing file. Every write run used a new random collection and
removed it afterwards. The final listing, the existing file's ETag and length,
and its downloaded bytes matched the values captured before each run.

| Operation | Observed result |
| --- | --- |
| MKCOL, PUT, GET and DELETE | Succeeded inside the scratch collection, including a filename with spaces, a percent sign and non-ASCII text |
| Conditional GET with the current ETag | 304 |
| PUT with `If-None-Match: *` to an existing scratch file | 204; replaced the contents, instead of the expected 412 |
| PUT with a deliberately stale strong `If-Match` | 204, instead of the expected 412 |
| PUT with the current strong `If-Match` | Succeeded and stored the supplied bytes |
| COPY, MOVE and COPY with `Overwrite: F` to an existing destination | Succeeded, with the collision correctly rejected by 412 |
| Repeated Set/Set PROPPATCH on a plain filename | Decoded successfully; subsequent PROPFIND returned the second value |
| The same PROPPATCH on the encoded filename | Response rejected by the decoder with `invalid DAV href` |
| LOCK, tagged locked PUT, refresh and UNLOCK | Succeeded |
| COPY over a locked destination using its tagged token | 409; the lock was then released successfully |

The live test exits unsuccessfully for these deviations even when cleanup and
preservation checks pass. It continues independent scratch checks after known
conditional-request and optional-feature failures so one result does not hide
the rest. An ordinary completed-phase message does not override a reported
deviation.

The same operations succeed against the pinned Apache fixture, which also
enforces both PUT preconditions. The client sends the HTTP conditions as
requested; it cannot make an ignored server-side precondition atomic by checking
the resource first. The strict href decoder remains unchanged. Further diagnosis
of Fastmail's encoded property href and locked-destination response is separate
from passing the local regression suite.

Run instructions and credential/cleanup behavior are in the
[client README](../README.md). Normal `dune runtest` never contacts a live server.
