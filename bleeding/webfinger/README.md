# webfinger - RFC 7033 WebFinger and RFC 7565 acct URI

An OCaml implementation of the WebFinger protocol (RFC 7033) and the acct URI scheme (RFC 7565) for discovering information about resources identified by URIs.

## Key Features

- **RFC 7033 WebFinger**: Complete implementation of the WebFinger protocol
- **RFC 7565 acct URIs**: Type-safe parsing and construction of acct URIs with proper percent-encoding
- **Type-safe JRD**: JSON Resource Descriptor encoding/decoding using jsont
- **Eio-based HTTP client**: Async HTTP queries using the requests library
- **Command-line tool**: CLI for performing WebFinger lookups

## Usage

### Library Usage

```ocaml
Eio_main.run @@ fun env ->
Eio.Switch.run @@ fun sw ->
let session = Requests.create ~sw env in

(* Parse and query an acct URI *)
let acct = Webfinger.Acct.of_string_exn "acct:user@example.com" in
match Webfinger.query_acct session acct () with
| Ok jrd ->
    Format.printf "%a@." Webfinger.Jrd.pp jrd;
    begin match Webfinger.Jrd.find_link ~rel:"self" jrd with
    | Some link ->
        Format.printf "ActivityPub: %s@."
          (Option.get (Webfinger.Link.href link))
    | None -> ()
    end
| Error e ->
    Format.eprintf "Error: %a@." Webfinger.pp_error e
```

### Working with acct URIs (RFC 7565)

```ocaml
(* Create an acct URI *)
let acct = Webfinger.Acct.make ~userpart:"user" ~host:"example.com" in
Format.printf "%a@." Webfinger.Acct.pp acct;
(* Output: acct:user@example.com *)

(* Handle email addresses as userparts (@ is percent-encoded) *)
let acct = Webfinger.Acct.make
  ~userpart:"juliet@capulet.example"
  ~host:"shoppingsite.example" in
Format.printf "%s@." (Webfinger.Acct.to_string acct);
(* Output: acct:juliet%40capulet.example@shoppingsite.example *)

(* Parse and extract components *)
let acct = Webfinger.Acct.of_string_exn
  "acct:juliet%40capulet.example@shoppingsite.example" in
Format.printf "User: %s, Host: %s@."
  (Webfinger.Acct.userpart acct)  (* juliet@capulet.example *)
  (Webfinger.Acct.host acct)      (* shoppingsite.example *)
```

### Command-line Tool

```bash
# Query a Mastodon user's WebFinger record
webfinger acct:gargron@mastodon.social

# Get only ActivityPub-related links
webfinger --rel self acct:user@example.com

# Output raw JSON for processing
webfinger --json acct:user@example.com | jq .
```

## Installation

```
opam install webfinger
```

## Documentation

API documentation is available via:

```
opam install webfinger
odig doc webfinger
```

## References

- [RFC 7033](https://datatracker.ietf.org/doc/html/rfc7033) - WebFinger
- [RFC 7565](https://datatracker.ietf.org/doc/html/rfc7565) - The 'acct' URI Scheme
- [RFC 6415](https://datatracker.ietf.org/doc/html/rfc6415) - Web Host Metadata

## License

ISC
