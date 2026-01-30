# RFC Specifications

This directory contains IETF RFC specifications relevant to the OCaml Requests HTTP client library implementation.

## Core HTTP Specifications

### RFC 9110 - HTTP Semantics
{{:https://datatracker.ietf.org/doc/html/rfc9110}RFC 9110}

Defines the core HTTP semantics, including methods, status codes, header fields, content negotiation, authentication framework, and caching. This is the foundational specification for all HTTP versions.

**Status**: Internet Standard (June 2022)
**Obsoletes**: RFC 7230, 7231, 7232, 7233, 7235, 7538, 7615, 7694

### RFC 9112 - HTTP/1.1
{{:https://datatracker.ietf.org/doc/html/rfc9112}RFC 9112}

Specifies HTTP/1.1 message syntax, connection management, and framing. Essential for implementing HTTP/1.1 clients.

**Status**: Internet Standard (June 2022)
**Obsoletes**: RFC 7230

### RFC 9111 - HTTP Caching
{{:https://datatracker.ietf.org/doc/html/rfc9111}RFC 9111}

Defines the HTTP caching model, including Cache-Control directives, freshness calculations, validation, and cache invalidation.

**Status**: Internet Standard (June 2022)
**Obsoletes**: RFC 7234

### RFC 9113 - HTTP/2
{{:https://datatracker.ietf.org/doc/html/rfc9113}RFC 9113}

Defines the HTTP/2 protocol, including binary framing, multiplexing, stream prioritization, and server push.

**Status**: Internet Standard (June 2022)
**Obsoletes**: RFC 7540, 8740

## URI and Resource Identification

### RFC 3986 - URI Generic Syntax
{{:https://datatracker.ietf.org/doc/html/rfc3986}RFC 3986}

Defines the generic syntax for Uniform Resource Identifiers (URIs), including parsing, resolution, and normalization rules.

**Status**: Internet Standard (January 2005)
**Obsoletes**: RFC 2732, 2396, 1808

## Authentication

### RFC 7235 - HTTP Authentication Framework
{{:https://datatracker.ietf.org/doc/html/rfc7235}RFC 7235}

Defines the HTTP authentication framework, including challenge-response mechanism and the WWW-Authenticate and Authorization header fields.

**Status**: Proposed Standard (June 2014)

### RFC 7617 - HTTP Basic Authentication
{{:https://datatracker.ietf.org/doc/html/rfc7617}RFC 7617}

Specifies the "Basic" HTTP authentication scheme, using base64-encoded username:password credentials.

**Status**: Proposed Standard (September 2015)
**Obsoletes**: RFC 2617

### RFC 6750 - OAuth 2.0 Bearer Token Usage
{{:https://datatracker.ietf.org/doc/html/rfc6750}RFC 6750}

Defines how to use bearer tokens in HTTP requests to access OAuth 2.0 protected resources.

**Status**: Proposed Standard (October 2012)

### RFC 7616 - HTTP Digest Access Authentication
{{:https://datatracker.ietf.org/doc/html/rfc7616}RFC 7616}

Specifies the "Digest" HTTP authentication scheme, providing challenge-response authentication with MD5 or SHA-256 hashing.

**Status**: Proposed Standard (September 2015)
**Obsoletes**: RFC 2617

## State Management

### RFC 6265 - HTTP State Management Mechanism (Cookies)
{{:https://datatracker.ietf.org/doc/html/rfc6265}RFC 6265}

Specifies the HTTP cookie mechanism for maintaining state between client and server, including Set-Cookie and Cookie header fields.

**Status**: Proposed Standard (April 2011)
**Obsoletes**: RFC 2965

## Security

### RFC 2818 - HTTP Over TLS
{{:https://datatracker.ietf.org/doc/html/rfc2818}RFC 2818}

Defines how HTTP is secured using TLS (Transport Layer Security), including the https URI scheme and certificate verification.

**Status**: Informational (May 2000)

### RFC 8446 - The Transport Layer Security (TLS) Protocol Version 1.3
{{:https://datatracker.ietf.org/doc/html/rfc8446}RFC 8446}

Specifies TLS 1.3, the latest version of the Transport Layer Security protocol used to secure HTTP connections.

**Status**: Proposed Standard (August 2018)
**Obsoletes**: RFC 5077, 5246

## Content Encoding

### RFC 1950 - ZLIB Compressed Data Format
{{:https://datatracker.ietf.org/doc/html/rfc1950}RFC 1950}

Defines the ZLIB compressed data format used for HTTP content encoding with the "deflate" Content-Encoding.

**Status**: Informational (May 1996)

### RFC 1951 - DEFLATE Compressed Data Format
{{:https://datatracker.ietf.org/doc/html/rfc1951}RFC 1951}

Specifies the DEFLATE compression algorithm used in gzip and zlib compression.

**Status**: Informational (May 1996)

## Using These Specifications

When implementing features or fixing bugs in the OCaml Requests library, refer to the relevant RFC sections and include citations in the code documentation using the OCamldoc link format:

```ocaml
(** [get uri] performs an HTTP GET request.

    Implements {{:https://datatracker.ietf.org/doc/html/rfc9110#section-9.3.1}RFC 9110 Section 9.3.1}
    for the GET method semantics. *)
```

## Additional Resources

- IETF HTTP Working Group: https://httpwg.org/
- RFC Editor: https://www.rfc-editor.org/
- HTTP/2 and HTTP/3 specifications: https://http2.github.io/
