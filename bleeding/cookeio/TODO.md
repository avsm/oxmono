# Cookie Library TODO

Features that may be added in future releases:

## Cryptographic Cookie Support
- **Signed cookies**: HMAC-based cookie integrity verification
- **Private cookies**: Encrypted cookie values with authentication
- Key management and rotation strategies

## Server-side Conveniences
- A session-oriented helper for servers (the old delta-tracking jar was
  removed with its last consumer; `Cookeio.parse_cookie_header` and
  `Cookeio.set_cookie_header` cover the protocol itself)
- `make_permanent` / `make_removal` constructors

## Client Jar
- Partitioned storage (the `Partitioned` attribute is parsed and
  round-tripped, but the jar does not key its store by top-level site)
