# OCaml-JSONFeed

An OCaml library for parsing and generating [JSON Feed](https://www.jsonfeed.org/) documents.

JSON Feed is a format similar to RSS and Atom but uses JSON instead of XML.
It is designed to be easier for developers to work with while providing all the
functionality needed for feed syndication.

## Installation

Add to your `dune-project`:

```lisp
(package
  (name your-package)
  (depends
    jsonfeed
    ...))
```

## Quick Start

### Creating a Feed

```ocaml
open Jsonfeed

(* Create an author *)
let author = Author.create
  ~name:"Jane Doe"
  ~url:"https://example.com/jane"
  ()

(* Create an item with HTML content *)
let item = Item.create
  ~id:"https://example.com/posts/1"
  ~url:"https://example.com/posts/1"
  ~title:"Hello, JSON Feed!"
  ~content:(`Html "<p>My first post using JSON Feed.</p>")
  ~authors:[author]
  ~tags:["introduction"; "jsonfeed"]
  ()

(* Create the feed *)
let feed = Jsonfeed.create
  ~title:"My Blog"
  ~home_page_url:"https://example.com"
  ~feed_url:"https://example.com/feed.json"
  ~items:[item]
  ()

(* Serialize to JSON *)
let json = Jsonfeed.to_string feed
```

### Parsing a Feed

```ocaml
open Jsonfeed

(* Parse from string *)
match Jsonfeed.of_string json_string with
| Ok feed ->
    Printf.printf "Feed: %s\n" (Jsonfeed.title feed);
    List.iter (fun item ->
      match Item.title item with
      | Some title -> Printf.printf "- %s\n" title
      | None -> ()
    ) (Jsonfeed.items feed)
| Error err ->
    Printf.eprintf "Parse error: %s\n" err

(* Parse from file *)
let content = In_channel.with_open_text "feed.json" In_channel.input_all in
match Jsonfeed.of_string content with
| Ok feed -> (* ... *)
| Error _ -> (* ... *)
```

### Content Types

Items can have HTML content, plain text content, or both:

```ocaml
(* HTML only *)
let item1 = Item.create
  ~id:"1"
  ~content:(`Html "<p>Rich <strong>HTML</strong> content</p>")
  ()

(* Plain text only *)
let item2 = Item.create
  ~id:"2"
  ~content:(`Text "Plain text content")
  ()

(* Both HTML and text *)
let item3 = Item.create
  ~id:"3"
  ~content:(`Both ("<p>HTML version</p>", "Text version"))
  ()

(* Access content *)
match Item.content_html item1 with
| Some html -> Printf.printf "HTML: %s\n" html
| None -> ()
```

### Podcast Feed with Attachments

```ocaml
(* Create an audio attachment *)
let episode_audio = Attachment.create
  ~url:"https://podcast.example.com/ep1.mp3"
  ~mime_type:"audio/mpeg"
  ~size_in_bytes:15_728_640L
  ~duration_in_seconds:1800
  ()

(* Create a podcast episode *)
let episode = Item.create
  ~id:"https://podcast.example.com/episodes/1"
  ~title:"Episode 1: Introduction"
  ~content:(`Html "<p>Welcome to the show!</p>")
  ~attachments:[episode_audio]
  ()
```

## Examples

The `example/` directory contains several complete examples:

- **feed_example.ml** - Creating and serializing feeds (blog and podcast)
- **feed_parser.ml** - Parsing and analyzing feeds from files
- **feed_validator.ml** - Validating feeds and demonstrating various feed types
- **feed_echo.ml** - Round-trip parsing: reads a feed from stdin and outputs to stdout

Run examples:

```bash
# Create and display sample feeds
opam exec -- dune exec -- ./example/feed_example.exe

# Parse and analyze a feed file
opam exec -- dune exec -- ./example/feed_parser.exe path/to/feed.json

# Validate feeds
opam exec -- dune exec -- ./example/feed_validator.exe

# Test round-trip parsing
cat feed.json | opam exec -- dune exec -- ./example/feed_echo.exe
```

## API Documentation

Build the API documentation:

```bash
opam exec -- dune build @doc
```

Then open `_build/default/_doc/_html/index.html` in your browser.

## Specification

This library implements [JSON Feed Version 1.1](https://www.jsonfeed.org/version/1.1/).

## License

See LICENSE.md for details.
