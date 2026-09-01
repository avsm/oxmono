# URL validation and normalization

Request functions take URL strings and validate them internally. The
parsed form (`Fetch.Middleware.Url.t`) is what a request's `url` field
carries. These tests use the backend-section helpers to show what
validation does.

```ocaml
# #require "fetch";;
```

```ocaml
let () = Printexc.record_backtrace false
open Fetch

let show s =
  match Middleware.Url.of_string s with
  | Ok u -> Middleware.Url.to_string u
  | Error e -> "error: " ^ e
```

Transport URLs are canonicalized: scheme and host lowercase, default port
elided, dot-segments resolved, fragment omitted; an empty path becomes "/":

```ocaml
# show "HTTP://Example.COM:80/a/../b/./c?x=1#frag";;
- : string = "http://example.com/b/c?x=1"
# show "https://example.com";;
- : string = "https://example.com/"
# show "https://example.com:8443/x";;
- : string = "https://example.com:8443/x"
# show "https://example.com:443/x";;
- : string = "https://example.com/x"
# show "http://example.com:/x";;
- : string = "http://example.com/x"
```

Diagnostic redaction preserves the query's encoded structure, including empty
parameters, while replacing only named values:

```ocaml
# let redact s =
    let u = Result.get_ok (Middleware.Url.of_string s) in
    Fmt.str "%a" (Middleware.Url.pp_redacted ~names:[ "secret" ]) u;;
val redact : string -> string = <fun>
# redact "https://example.com/?&a=1&&secret=x&";;
- : string = "https://example.com/?&a=1&&secret=<redacted>&"
```

Internationalized host names are canonicalized to their lowercase A-label
form before origin policy or transport sees them:

```ocaml
# show "https://bücher.example/catalog";;
- : string = "https://xn--bcher-kva.example/catalog"
# show "https://xn--bcher-kva.example/catalog";;
- : string = "https://xn--bcher-kva.example/catalog"
```

The parsed value retains its client-side fragment for response URLs and
redirect resolution, while its transport serialization keeps the fragment off
the wire:

```ocaml
# let u = Result.get_ok (Middleware.Url.of_string "https://example.com/a#frag");;
val u : Middleware.url = <abstr>
# Middleware.Url.effective_string u;;
- : string = "https://example.com/a#frag"
# Httpz.Uriz.encoded_fragment (Middleware.Url.to_uri u);;
- : string or_null = Null
```

Only absolute http(s) URLs with a host are accepted, and userinfo is
rejected:

```ocaml
# show "ftp://example.com/";;
- : string = "error: unsupported scheme \"ftp\" (must be http or https)"
# show "/relative/path";;
- : string = "error: not an absolute URL (missing scheme)"
# show "http://user:pw@example.com/";;
- : string = "error: userinfo (user:password@) is not allowed in http URLs"
# show "https://";;
- : string = "error: URL has no host"
```

The URI parser is strict about the RFC 3986 grammar. Percent-encoded bytes in a
registered name are decoded before host policy is applied, so characters that
cannot name a host cannot survive into the origin used by policy:

```ocaml
# show "http://example.com%2f.evil.com/";;
- : string = "error: invalid character '/' in host"
# show "http://example.com%00.evil.com/";;
- : string = "error: invalid character '\\000' in host"
# show "https://[::1]:8080/x";;
- : string = "https://[::1]:8080/x"
```

An invalid URL given to a request function raises before any network
activity:

```ocaml
# #require "fetch.mock";;
# #require "eio.mock";;
# Eio_mock.Backend.run @@ fun () ->
  Fetch.read (Fetch_mock.client (Fetch_mock.respond "ok")) "not a url";;
Exception:
Eio.Io Http Invalid_url "not a valid URI reference (\"not a url\")"
```

## Policy path segments

Policy checks match on decoded segments with "." and ".." resolved; an
encoded slash (%2F) stays inside its segment:

```ocaml
# let seg s = Middleware.Url.path_segments (Result.get_ok (Middleware.Url.of_string s));;
val seg : string -> string list = <fun>
# seg "https://example.com/api/v2/users";;
- : string list = ["api"; "v2"; "users"]
# seg "https://example.com/a%2Fb/c";;
- : string list = ["a/b"; "c"]
# seg "https://example.com/api/../secrets";;
- : string list = ["secrets"]
```

An origin-wide scope still covers such a path: there is no path boundary in
the prefix for the encoded separator to confuse. A narrower path scope rejects
it.

```ocaml
# let parsed_url s = Result.get_ok (Middleware.Url.of_string s);;
val parsed_url : string -> Middleware.url = <fun>
# Middleware.Url.under
    ~prefix:(parsed_url "https://example.com/")
    (parsed_url "https://example.com/a%2Fb");;
- : bool = true
# Middleware.Url.under
    ~prefix:(parsed_url "https://example.com/api")
    (parsed_url "https://example.com/api/a%2Fb");;
- : bool = false
```

## Host canonicalization

`inet_aton(3)` accepts an IPv4 address in far more spellings than the
dotted quad, and every policy layer here — `restrict`, credential
scopes, `with_limits` buckets, the cookie jar — compares host strings.
An address is folded to one spelling before any of them sees it, and the
root dot of an absolute DNS name is dropped:

```ocaml
# List.map show
    [ "http://2130706433/"; "http://0x7f000001/"; "http://127.1/";
      "http://127.0.1/"; "http://0177.0.0.1/"; "http://0x7f.1/" ];;
- : string list =
["http://127.0.0.1/"; "http://127.0.0.1/"; "http://127.0.0.1/";
 "http://127.0.0.1/"; "http://127.0.0.1/"; "http://127.0.0.1/"]
```

```ocaml
# show "http://localhost./";;
- : string = "http://localhost/"
# show "https://example.com./";;
- : string = "https://example.com/"
```

A trailing dot on an address is a spelling no resolver accepts rather
than a name to canonicalize, and an empty label is not a host:

```ocaml
# show "http://127.1./";;
- : string = "error: host \"127.1\" is an IP address with a trailing dot"
# show "http://example..com/";;
- : string = "error: host \"example..com\" has an empty label"
```

An IPv6 literal is checked against the RFC 3986 grammar, not merely its
character set, so a malformed one fails here instead of at DNS time. A
dotted-quad embedded in IPv6 is also refused: a mapped address must not look
like a separate origin to string policy. DNS rebinding still cannot be seen
here and belongs in `~connect`, which can check the resolved address.

```ocaml
# show "http://[::ffff:127.0.0.1]/";;
- : string =
"error: host \"::ffff:127.0.0.1\" embeds an IPv4 address in IPv6"
# show "http://[:::]/";;
- : string = "error: not a valid URI reference"
# show "http://[99999::1]/";;
- : string = "error: not a valid URI reference"
```

IPvFuture is URI syntax, but neither shipped HTTP transport has a connection
address model for it, so it is rejected instead of entering hostname policy:

```ocaml
# show "http://[v9.x]/";;
- : string =
"error: IPvFuture literals are not supported as HTTP connection hosts"
```

Relative redirect references receive the same internationalized-authority
normalization as initial URLs:

```ocaml
# let base = Result.get_ok (Middleware.Url.of_string "https://example.com/start") in
  Middleware.Url.resolve ~base "//bücher.example/finish"
  |> Result.map Middleware.Url.to_string;;
- : (string, string) result = Ok "https://xn--bcher-kva.example/finish"
```

A host rule written against one spelling therefore covers the rest:

```ocaml
# let no_loopback (req : Middleware.request) =
    if Middleware.Url.host req.url = "127.0.0.1" then
      `Reject "loopback is out of bounds"
    else `Allow;;
val no_loopback : Middleware.request -> [> `Allow | `Reject of string ] =
  <fun>
# let guarded =
    Fetch.restrict ~filter:no_loopback (Fetch_mock.client (Fetch_mock.respond "ok"));;
val guarded : plain = Eio.Resource.T (<poly>, <abstr>)
# Eio_mock.Backend.run @@ fun () -> Fetch.read guarded "http://127.1/";;
Exception:
Eio.Io Http Denied "loopback is out of bounds",
  GET http://127.0.0.1/
# Eio_mock.Backend.run @@ fun () -> Fetch.read guarded "http://0x7f000001/";;
Exception:
Eio.Io Http Denied "loopback is out of bounds",
  GET http://127.0.0.1/
# Eio_mock.Backend.run @@ fun () -> Fetch.read guarded "http://example.com/";;
- : string = "ok"
```

## Components

A backend deciding where to connect, or a `restrict ~filter` deciding
whether it may, reads the parts off `to_uri` with the `Httpz.Uriz` API. A
default port is elided by canonicalization, so a caller wanting the
port on the wire supplies 80 or 443 itself, and an IPv6 host comes back
without its brackets:

```ocaml
# let parts s =
    let u = Middleware.Url.to_uri (Result.get_ok (Middleware.Url.of_string s)) in
    (Httpz.Uriz.scheme u, Httpz.Uriz.decoded_host u, Httpz.Uriz.port u);;
val parts : string -> string or_null * string or_null * int or_null = <fun>
# parts "https://api.example.com/x";;
- : string or_null * string or_null * int or_null =
(This "https", This "api.example.com", Null)
# parts "https://api.example.com:8443/x";;
- : string or_null * string or_null * int or_null =
(This "https", This "api.example.com", This 8443)
# parts "https://[::1]:8080/x";;
- : string or_null * string or_null * int or_null =
(This "https", This "::1", This 8080)
```

Saying where requests may go is a separate job, done with the URL
prefixes of `restrict ~under` and every `scope`.
