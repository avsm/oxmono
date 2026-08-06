# RFC 6265 Compliance

This document tracks how ocaml-cookeio relates to
[RFC 6265](https://datatracker.ietf.org/doc/html/rfc6265) (HTTP State
Management Mechanism) and its successors.

## Implemented

Client-side parsing and storage (`Cookeio.parse_set_cookie`, `Cookeio_jar`):

- [x] Case-insensitive attribute name matching (§5.2)
- [x] Leading dot removal and lowercasing of the Domain attribute (§5.2.3);
      domains are canonical (lowercase, no leading dot) throughout
- [x] The Domain attribute must domain-match the setting host (§5.3 step 6)
- [x] Public suffix rejection unless the host is exactly the suffix
      (§5.3 step 5, via the Mozilla Public Suffix List)
- [x] Max-Age precedence over Expires regardless of order (§5.3 step 3),
      resolved once at parse time into a single `expiry`
- [x] Non-positive Max-Age is expired immediately, so the Max-Age=0
      deletion idiom works even at the same clock tick (§5.2.2)
- [x] Default path computation, also for empty or relative Path values
      (§5.1.4 / §5.2.4)
- [x] Cookie date parsing in the four sane-cookie-date shapes, with
      RFC-conforming two-digit years — 0-69 are 20xx, 70-99 are 19xx
      (§5.1.1)
- [x] Host-only flag and exact-match domain behavior (§5.3 step 6)
- [x] Domain matching with no suffix matching against IP literals (§5.1.3)
- [x] Path matching (§5.1.4)
- [x] Cookie header ordering: longer paths first, then earlier creation,
      with a name tiebreak for same-tick cookies (§5.4 step 2)
- [x] Creation-time preservation when a cookie is replaced (§5.3 step 12)
- [x] Last-access updates on retrieval and eviction of expired cookies
      (§5.4)
- [x] Storage limits with LRU eviction: 4096 bytes name+value, 50 per
      domain, 3000 total (§6.1)

Server-side (`Cookeio.parse_cookie_header`, `Cookeio.set_cookie_header`):

- [x] Lenient Cookie header parsing into name-value pairs, repeated
      names preserved in order (§4.2.2)
- [x] Set-Cookie emission with IMF-fixdate Expires (§4.1.1) and the
      Domain attribute omitted for host-only cookies (§4.1.2.3)

Extensions:

- [x] SameSite parsing and the SameSite=None-requires-Secure rule
      (RFC 6265bis §5.4.7); a non-browser client has no site notion,
      so matching does not consult it
- [x] `__Secure-`/`__Host-` name prefixes: attribute rules at parse,
      plaintext-channel refusal in the jar (RFC 6265bis §4.1.3)
- [x] Refusal of plaintext cookies that would shadow a stored Secure
      cookie (RFC 6265bis §5.5 step 13)
- [x] The Partitioned attribute parsed, validated (requires Secure) and
      round-tripped (CHIPS)
- [x] Netscape cookies.txt persistence, curl-compatible including the
      `#HttpOnly_` marking (de facto standard)

## Open

- The jar does not partition its store by top-level site, so
  Partitioned cookies are stored like ordinary ones (CHIPS).
- The jar has no session-end notion; session cookies persist until
  cleared, and are written to cookies.txt with expiry 0 as curl does.

## References

- [RFC 6265](https://datatracker.ietf.org/doc/html/rfc6265) - HTTP State Management Mechanism
- [RFC 6265bis](https://datatracker.ietf.org/doc/html/draft-ietf-httpbis-rfc6265bis) - Updated cookie spec (draft)
- [Public Suffix List](https://publicsuffix.org/) - Mozilla's public suffix database
- [CHIPS](https://developer.chrome.com/docs/privacy-sandbox/chips/) - Cookies Having Independent Partitioned State
