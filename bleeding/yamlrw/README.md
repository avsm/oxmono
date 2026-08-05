# Yamlrw

A pure OCaml implementation of YAML 1.2 parsing and emission.

## Features

- **Pure OCaml**: No C bindings, works on all OCaml platforms
- **YAML 1.2 Compliant**: Full support for the YAML 1.2 specification
- **High-level API**: JSON-compatible value representation for simple use cases
- **Low-level Streaming**: Event-based parsing for fine-grained control
- **Multiple I/O Backends**:
  - `yamlrw`: Core library with bytesrw-based I/O
  - `yamlrw-unix`: Unix file and channel operations
  - `yamlrw-eio`: Eio-based streaming I/O (OCaml 5.0+)

## Installation

```bash
opam install yamlrw
# For Eio support:
opam install yamlrw-eio
# For Unix support:
opam install yamlrw-unix
```

## Quick Start

### Parsing YAML

```ocaml
let value = Yamlrw.of_string "name: Alice\nage: 30" in
match value with
| `O [("name", `String "Alice"); ("age", `Float 30.)] ->
    print_endline "Parsed successfully"
| _ ->
    print_endline "Unexpected structure"
```

### Emitting YAML

```ocaml
let yaml = `O [
  ("name", `String "Bob");
  ("active", `Bool true);
  ("tags", `A [`String "developer"; `String "ocaml"])
] in
let s = Yamlrw.to_string yaml in
print_endline s
(* Output:
   name: Bob
   active: true
   tags:
     - developer
     - ocaml
*)
```

### Using the Utility Functions

```ocaml
open Yamlrw.Util

let config = Yamlrw.of_string_exn "
server:
  host: localhost
  port: 8080
" in

let host = get_string (get "host" (get "server" config)) in
let port = get_int (get "port" (get "server" config)) in
Printf.printf "Server: %s:%d\n" host port
```

## Command-line Tool

The `yamlcat` binary validates and pretty-prints YAML files:

```bash
yamlcat input.yaml
```

## Testing

Yamlrw is tested against the official [YAML Test Suite](https://github.com/yaml/yaml-test-suite), a comprehensive collection of YAML test cases.

### Running the Full Test Suite

To run the complete YAML test suite with HTML report generation:

1. **Clone the test suite** (one-time setup):
   ```bash
   cd tests
   git clone --depth 1 --branch data https://github.com/yaml/yaml-test-suite
   cd ..
   ```

2. **Run the tests**:
   ```bash
   # Standard tests with HTML report
   opam exec -- dune build @yaml-test-suite

   # Eio-based tests with HTML report
   opam exec -- dune build @yaml-test-suite-eio
   ```

**View the results**:
- The HTML reports are generated in `_build/default/tests/yaml-test-results.html` and `_build/default/tests/yaml-test-results-eio.html`
- Open them in a browser to see detailed test results with filtering and search capabilities

### Running Unit Tests

Run the standard unit tests:

```bash
opam exec -- dune runtest
```

## API Documentation

Build the documentation with:

```bash
opam exec -- dune build @doc
```

## License

ISC License - see [LICENSE.md](LICENSE.md) for details.

Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>
