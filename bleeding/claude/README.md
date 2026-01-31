# ClaudeIO - OCaml Eio Library for Claude Code CLI

An OCaml library that provides high-quality Eio-style bindings for the Claude Code CLI, enabling programmatic interaction with Claude through JSON streaming.

## Overview

ClaudeIO wraps Claude Code CLI invocations in an idiomatic OCaml Eio interface, leveraging:
- JSON input/output streaming modes of the CLI
- Ezjsonm for JSON message handling
- Eio abstractions including `Buf_read` and `Seq` for efficient streaming

## Features

- **Streaming JSON Interface**: Communicate with Claude using structured JSON messages
- **Eio Integration**: Built on modern OCaml concurrency primitives
- **Type-safe API**: Strongly typed OCaml interface for Claude interactions
- **Efficient Buffering**: Uses Eio's buffer management for optimal performance

## Installation

```bash
opam install claudeio
```

## Usage

```ocaml
open Eio
open Claudeio

let main ~env =
  let claude = Claude.create ~env in
  Claude.query claude ~prompt:"Your question here"
  |> Seq.iter (fun response -> 
    Format.printf "Claude: %s\n" (Claude.Response.to_string response))
```

## Known Issues

⚠️ **Permissions Support**: The permissions functionality is temporarily broken and awaiting a fix from Anthropic. This feature will be restored in a future update.

## Requirements

- OCaml >= 5.0
- Eio >= 1.0
- Ezjsonm >= 1.3
- Claude Code CLI installed and configured

## License

See LICENSE file for details.
