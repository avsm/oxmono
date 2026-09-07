# apubt - ActivityPub Client Library for OCaml

An ActivityPub/ActivityStreams protocol implementation for OCaml using Eio for concurrent I/O. Provides typed representations of actors, activities, and objects with bidirectional JSON codecs.

## Key Features

- **Typed ActivityPub subset**: OCaml types for actors, activities, objects, and collections
- **JSON codecs**: Bidirectional encoding/decoding using jsont
- **Eio-based HTTP**: Direct-style concurrent I/O with connection pooling
- **HTTP Signatures**: RFC 9421 and legacy RSA signatures for authenticated GET/POST
- **Webfinger**: Actor discovery via RFC 7033/7565 using the `webfinger` library
- **NodeInfo**: Server metadata discovery
- **CLI Tool**: Command-line interface for interacting with ActivityPub servers

## Usage

```ocaml
open Eio.Std

let () = Eio_main.run @@ fun env ->
  Switch.run @@ fun sw ->

  (* Create an ActivityPub client *)
  let client = Apubt.create ~sw env in

  (* Discover an actor via Webfinger *)
  let actor = Apubt.Actor.lookup client "user@example.com" in
  Printf.printf "Found: %s\n"
    (Option.value ~default:"<none>" (Apubt.Proto.Actor.name actor));

  (* Fetch their outbox *)
  let outbox = Apubt.Actor.outbox client actor in
  List.iter (fun activity ->
    Printf.printf "Activity: %s\n"
      (Apubt.Proto.Activity_type.to_string (Apubt.Proto.Activity.type_ activity))
  ) (Option.value ~default:[] (Apubt.Proto.Collection.items outbox))
```

### With HTTP Signatures

```ocaml
(* Initialize randomness for RSA blinding, then configure signing *)
Mirage_crypto_rng_unix.use_default ();
let signing = Apubt.Signing.from_pem_exn
  ~key_id:"https://example.com/users/alice#main-key"
  ~pem:private_key_pem
  () in
(* Application callback: durably save the activity and embedded objects, and
   serve them at their IDs. Called before any delivery. *)
let client = Apubt.create ~sw ~signing ~persist:store_activity_and_objects env in

(* Post a public note *)
let _activity = Apubt.Outbox.public_note client
  ~actor:my_actor
  ~content:"<p>Hello from OCaml!</p>"
  ()
```

### Using an existing Fetch client

```ocaml
let client = Apubt.of_fetch ~clock:env#clock
  ~max_response_bytes:(4 * 1024 * 1024) fetch
```

The caller owns the Fetch client's lifetime and configures its backend,
timeouts, restrictions, and retries. `Apubt.create` remains a curl convenience
constructor. JSON responses are bounded to 16 MiB by default and require a JSON
Content-Type. POST requests do not follow redirects.

Federation helpers require a persistence callback before creating and delivering
activities. The default ID generator uses cryptographic randomness; applications
can inject their own URI allocator. Follower collections are expanded with bounded
pagination and deduplicated inbox delivery. Delivery failures propagate; retry a
persisted activity with `Outbox.deliver` to retain its ID. Undo helpers take the
original Follow, Like, or Announce.

Signed clients authenticate GETs and POSTs. Pass `~format:`Cavage` to
`Signing.from_pem` (or CLI `--signature-format cavage`) for older peers.

The CLI stores activities and embedded objects atomically under
`$XDG_CONFIG_HOME/apub/profiles/<profile>/objects/` (default `~/.config`). An
application or server must still serve these documents at their IDs; the CLI
is not a federation server. Signature credentials are checked against the actor's
advertised key. OAuth commands resolve remote URLs on the authenticated instance;
existing logins need the `read:search` scope for these operations.
See [the implementation review](REVIEW.md) for coverage and limitations.

## Command-Line Interface

The `apub` command provides a CLI for interacting with ActivityPub servers:

```bash
# Discover an actor via Webfinger
apub webfinger gargron@mastodon.social

# Fetch an actor's profile
apub actor gargron@mastodon.social

# Fetch an actor's outbox
apub outbox gargron@mastodon.social

# Post a note (requires signing)
apub post --actor https://example.com/users/alice \
          --key-file ~/.keys/alice.pem \
          --key-id "https://example.com/users/alice#main-key" \
          "Hello, Fediverse!"

# Follow an actor
apub follow --actor https://example.com/users/alice \
            gargron@mastodon.social

# Like a post
apub like --actor https://example.com/users/alice \
          https://mastodon.social/notes/123

# Boost a post
apub boost --actor https://example.com/users/alice \
           https://mastodon.social/notes/123
```

## Installation

```
opam install apubt
```

## Documentation

API documentation is available via:

```
opam install apubt
odig doc apubt
```

## License

ISC
