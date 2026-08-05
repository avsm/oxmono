# URL validation and normalization

Request functions take URL strings and validate them internally. The
parsed form (`Fetch.Middleware.Url.t`) is what a request's `url` field
carries. These tests use the backend-section helpers to show what
validation does.

```ocaml
# #require "fetch";;
```

```ocaml
open Fetch

let show s =
  match Middleware.Url.of_string s with
  | Ok u -> Middleware.Url.to_string u
  | Error e -> "error: " ^ e
```

URLs are canonicalized: scheme and host lowercase, default port elided,
dot-segments resolved, fragment dropped; an empty path becomes "/":

```ocaml
# show "HTTP://Example.COM:80/a/../b/./c?x=1#frag";;
- : string = "http://example.com/b/c?x=1"
# show "https://example.com";;
- : string = "https://example.com/"
# show "https://example.com:8443/x";;
- : string = "https://example.com:8443/x"
# show "https://example.com:443/x";;
- : string = "https://example.com/x"
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

`uri` recovers from a malformed authority rather than failing, so
characters that cannot appear in a host can survive into one, where
they would go on to stand for a host a policy never named. They are
rejected:

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
Eio.Io Http Invalid_url "not an absolute URL (missing scheme) (\"not a url\")"
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

## Components

A backend deciding where to connect, or a `restrict ~filter` deciding
whether it may, reads the parts off `to_uri` with the `uri` API. A
default port is elided by canonicalization, so a caller wanting the
port on the wire supplies 80 or 443 itself, and an IPv6 host comes back
without its brackets:

```ocaml
# let parts s =
    let u = Middleware.Url.to_uri (Result.get_ok (Middleware.Url.of_string s)) in
    (Uri.scheme u, Uri.host u, Uri.port u);;
val parts : string -> string option * string option * int option = <fun>
# parts "https://api.example.com/x";;
- : string option * string option * int option =
(Some "https", Some "api.example.com", None)
# parts "https://api.example.com:8443/x";;
- : string option * string option * int option =
(Some "https", Some "api.example.com", Some 8443)
# parts "https://[::1]:8080/x";;
- : string option * string option * int option =
(Some "https", Some "::1", Some 8080)
```

Saying where requests may go is a separate job, done with the URL
prefixes of `restrict ~under` and every `scope`.
