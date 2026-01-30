# Cookeio - HTTP Cookie Management for OCaml

Cookeio is an OCaml library for managing HTTP cookies.

## Overview

HTTP cookies are a mechanism for maintaining client-side state in web
applications. Originally specified to allow "server side connections to store
and retrieve information on the client side," cookies enable persistent storage
of user preferences, session data, shopping cart contents, and authentication
tokens.

This library provides a complete cookie jar implementation following
established standards while integrating with OCaml's for efficient asynchronous
operations.

## Cookie Attributes

The library supports all standard HTTP cookie attributes:

- **Domain**: Controls which domains can access the cookie using tail matching
- **Path**: Defines URL subsets where the cookie is valid
- **Secure**: Restricts transmission to HTTPS connections only
- **HttpOnly**: Prevents JavaScript access to the cookie
- **Expires**: Sets cookie lifetime (session cookies when omitted)
- **SameSite**: Controls cross-site request behavior (`Strict`, `Lax`, or `None`)

## Usage

```ocaml
(* Create a new cookie jar *)
let jar = Cookeio.create () in

(* Parse a Set-Cookie header *)
let cookie = Cookeio.parse_set_cookie
  ~domain:"example.com"
  ~path:"/"
  "session=abc123; Secure; HttpOnly; SameSite=Strict" in

(* Add cookie to jar *)
Option.iter (Cookeio.add_cookie jar) cookie;

(* Get cookies for a request *)
let cookies = Cookeio.get_cookies jar
  ~domain:"example.com"
  ~path:"/api"
  ~is_secure:true in

(* Generate Cookie header *)
let header = Cookeio.make_cookie_header cookies
```

## Storage and Persistence

Cookies can be persisted to disk in Mozilla format for compatibility with other
tools:

```ocaml
(* Save cookies to file *)
Cookeio.save (Eio.Path.of_string "cookies.txt") jar;

(* Load cookies from file *)
let jar = Cookeio.load (Eio.Path.of_string "cookies.txt")
```
