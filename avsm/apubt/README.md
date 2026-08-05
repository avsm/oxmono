# apubt - ActivityPub Client Library for OCaml

An ActivityPub/ActivityStreams protocol implementation for OCaml using Eio for concurrent I/O. Provides typed representations of actors, activities, and objects with bidirectional JSON codecs.

## Key Features

- **Type-safe ActivityPub**: Full OCaml types for actors, activities, objects, and collections
- **JSON codecs**: Bidirectional encoding/decoding using jsont
- **Eio-based HTTP**: Direct-style concurrent I/O with connection pooling
- **HTTP Signatures**: RFC 9421 message signatures for authenticated federation
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
(* Configure signing for authenticated requests *)
let signing = Apubt.Signing.from_pem_exn
  ~key_id:"https://example.com/users/alice#main-key"
  ~pem:private_key_pem
  () in
let client = Apubt.create ~sw ~signing env in

(* Post a public note *)
let _activity = Apubt.Outbox.public_note client
  ~actor:my_actor
  ~content:"<p>Hello from OCaml!</p>"
  ()
```

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
