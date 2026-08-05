# Cookie Library TODO

This library is feature-complete for basic HTTP cookie management with RFC 6265 compliance. The following features are not yet implemented but may be added in future releases:

## Missing Features

### Cryptographic Cookie Support (Priority 1)
- **Signed cookies**: HMAC-based cookie integrity verification
- **Private cookies**: Encrypted cookie values with authentication
- Key management and rotation strategies

### Modern Web Features (Priority 2)
- **Partitioned attribute**: CHIPS (Cookies Having Independent Partitioned State) support for privacy
- **Cookie prefixes**: `__Host-` and `__Secure-` prefix validation
- **Expiration type**: Explicit `Session | DateTime` distinction instead of optional expiry

### Parser Enhancements (Priority 3)
- **Multiple cookie parsing**: Parse semicolon-separated Cookie header into list
- **Value trimming**: Strip quotes from cookie values automatically
- **Builder pattern**: Fluent API for constructing cookies

### Helper Functions
- `make_permanent`: Create cookie with 20-year expiration
- `make_removal`: Already implemented in `remove` function, could be exposed separately

## Notes

The library currently provides full RFC 6265 compliance including:
- Max-Age and Expires handling with multiple date format support
- Domain normalization and matching
- Delta tracking for Set-Cookie generation
- SameSite, Secure, HttpOnly attributes
- Mozilla format persistence

All 35 tests pass. No breaking changes are planned for future releases.
