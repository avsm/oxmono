# Public Suffix List lookup

The `httpz.pubsuffix` library identifies public suffixes and registrable
domains using an embedded copy of the
[Public Suffix List](https://publicsuffix.org/list/). It supports ICANN and
Private rules, including wildcards and exceptions, and accepts Unicode or
ASCII Punycode domain names through `httpz.punycode.idna`. That helper applies
NFC normalization and Punycode conversion but does not implement complete
IDNA2008 validation; validate untrusted internationalized names separately
before DNS use.

Install the library and the `httpz-pubsuffix` command with:

```console
opam install httpz
```

## Library use

```ocaml
let () =
  match Pubsuffix.registrable_domain "www.example.co.uk" with
  | Ok domain -> Format.printf "%s@." domain
  | Error error -> Format.eprintf "%a@." Pubsuffix.pp_error error
```

Here the result is `example.co.uk`; the corresponding public suffix is
`co.uk`. Results are lower-case ASCII, and a trailing dot in the input is
preserved. `Pubsuffix.public_suffix_with_section` and
`Pubsuffix.registrable_domain_with_section` also report whether the prevailing
rule came from the ICANN or Private section.

The command provides the same queries:

```console
httpz-pubsuffix suffix www.example.co.uk
httpz-pubsuffix registrable www.example.co.uk
httpz-pubsuffix is_suffix co.uk
```

A failed query writes its diagnostic to standard error and exits nonzero.

## Updating the embedded list

Refresh the vendored data with one command, run from the repository root:

```console
curl -fsSL -o httpz/pubsuffix/data/public_suffix_list.dat \
  https://publicsuffix.org/list/public_suffix_list.dat && \
  dune build && dune runtest httpz/pubsuffix --auto-promote
```

The build generates a reverse-label trie and embeds it in the library at
build time; runtime lookups perform no file I/O. `--auto-promote` updates the
cram test's rule-count expectations to match the new list; review the diff
before committing. The list carries its own Mozilla Public License 2.0
header (see the top of `data/public_suffix_list.dat`); keep it intact across
updates.

`check_psl_freshness.sh` (also wired to the `@psl-check` dune alias) fails if
the embedded `VERSION` line is more than 90 days old:

```console
httpz/pubsuffix/check_psl_freshness.sh
# or
dune build @httpz/pubsuffix/psl-check
```

`.github/workflows/psl.yml` runs a similar check weekly and fails the
workflow when upstream has moved on, so a refresh is rarely more than a
week overdue.

Domain-name processing follows
[RFC 1035](https://www.rfc-editor.org/rfc/rfc1035.html),
[RFC 3492](https://www.rfc-editor.org/rfc/rfc3492.html), and
[RFC 5891](https://www.rfc-editor.org/rfc/rfc5891.html).
