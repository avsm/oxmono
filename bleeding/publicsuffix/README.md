# ocaml-publicsuffix - Public Suffix List for OCaml

An OCaml library for parsing and querying the Mozilla Public Suffix List (PSL) to determine public suffixes and registrable domains. This library implements the algorithm specified at [publicsuffix.org](https://publicsuffix.org/list/) and provides efficient lookups using a pre-compiled trie data structure.

## Key Features

- **Complete PSL Support**: Handles ICANN and private domain sections
- **Full Rule Coverage**: Supports normal rules, wildcard rules (e.g., `*.uk`), and exception rules (e.g., `!parliament.uk`)
- **Efficient Lookups**: Pre-compiled trie structure for fast domain matching
- **Punycode Support**: Automatic handling of internationalized domain names via the `punycode` library
- **Type Safety**: Uses the `domain-name` library for validated domain representations

## Usage

```ocaml
(* Determine the registrable domain (public suffix + one label) *)
let domain = Domain_name.of_string_exn "www.example.com" in
match Publicsuffix.registrable_domain domain with
| Some reg_domain -> Format.printf "Registrable: %a\n" Domain_name.pp reg_domain
| None -> Format.printf "No registrable domain\n"
(* Output: Registrable: example.com *)

(* Find the public suffix *)
match Publicsuffix.public_suffix domain with
| Some suffix -> Format.printf "Suffix: %a\n" Domain_name.pp suffix
| None -> Format.printf "No public suffix\n"
(* Output: Suffix: com *)

(* Check if a domain is itself a public suffix *)
let is_suffix = Publicsuffix.is_public_suffix domain in
Format.printf "Is public suffix: %b\n" is_suffix
(* Output: Is public suffix: false *)
```

For domains with wildcards and exceptions:

```ocaml
(* Example with wildcard rule: *.uk *)
let domain = Domain_name.of_string_exn "example.uk" in
match Publicsuffix.public_suffix domain with
| Some suffix -> Format.printf "Suffix: %a\n" Domain_name.pp suffix
| None -> ()
(* Output: Suffix: uk *)

(* Example with exception rule: !parliament.uk *)
let domain = Domain_name.of_string_exn "parliament.uk" in
match Publicsuffix.registrable_domain domain with
| Some reg_domain -> Format.printf "Registrable: %a\n" Domain_name.pp reg_domain
| None -> ()
(* Output: Registrable: parliament.uk *)
```

## Installation

```
opam install publicsuffix
```

## Updating the Public Suffix List Data

The `data/public_suffix_list.dat` file contains the PSL data, which is compiled into the library at build time. To update to the latest version:

```bash
curl -o data/public_suffix_list.dat https://publicsuffix.org/list/public_suffix_list.dat
opam exec -- dune build
```

## Documentation

API documentation is available via:

```
opam install publicsuffix
odig doc publicsuffix
```

Or build locally:

```bash
opam exec -- dune build @doc
```

## Technical Standards

This library is built on the following Internet standards:

- **[RFC 1034](https://datatracker.ietf.org/doc/html/rfc1034)** - Domain Names: Concepts and Facilities
- **[RFC 1035](https://datatracker.ietf.org/doc/html/rfc1035)** - Domain Names: Implementation and Specification
- **[RFC 3492](https://datatracker.ietf.org/doc/html/rfc3492)** - Punycode: A Bootstring encoding of Unicode for IDNA
- **[RFC 5890](https://datatracker.ietf.org/doc/html/rfc5890)** - IDNA: Definitions and Document Framework
- **[RFC 5891](https://datatracker.ietf.org/doc/html/rfc5891)** - IDNA: Protocol

RFC specifications are available in the `spec/` directory for reference.

## License

ISC
