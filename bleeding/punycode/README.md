# puny - RFC 3492 Punycode and IDNA for OCaml

High-quality implementation of RFC 3492 (Punycode) with IDNA (Internationalized Domain Names in Applications) support for OCaml. Enables encoding and decoding of internationalized domain names with proper Unicode normalization.

## Key Features

- **RFC 3492 Punycode**: Complete implementation of the Bootstring algorithm for encoding Unicode in ASCII-compatible form
- **IDNA Support**: ToASCII and ToUnicode operations per RFC 5891 (IDNA 2008) for internationalized domain names
- **Unicode Normalization**: Automatic NFC normalization using `uunf` for proper IDNA compliance
- **Mixed-Case Annotation**: Optional case preservation through Punycode encoding round-trips
- **Domain Integration**: Native support for the `domain-name` library
- **Comprehensive Error Handling**: Detailed position tracking and RFC-compliant error reporting

## Usage

### Basic Punycode Encoding/Decoding

```ocaml
(* Encode a UTF-8 string to Punycode *)
let encoded = Punycode.encode_utf8 "münchen"
(* = Ok "mnchen-3ya" *)

(* Decode Punycode back to UTF-8 *)
let decoded = Punycode.decode_utf8 "mnchen-3ya"
(* = Ok "münchen" *)
```

### Domain Label Operations

```ocaml
(* Encode a domain label with ACE prefix *)
let label = Punycode.encode_label "münchen"
(* = Ok "xn--mnchen-3ya" *)

(* Decode an ACE-prefixed label *)
let original = Punycode.decode_label "xn--mnchen-3ya"
(* = Ok "münchen" *)
```

### IDNA Domain Name Conversion

```ocaml
(* Convert internationalized domain to ASCII for DNS lookup *)
let ascii_domain = Punycode_idna.to_ascii "münchen.example.com"
(* = Ok "xn--mnchen-3ya.example.com" *)

(* Convert ASCII domain back to Unicode for display *)
let unicode_domain = Punycode_idna.to_unicode "xn--mnchen-3ya.example.com"
(* = Ok "münchen.example.com" *)
```

### Working with Unicode Code Points

```ocaml
(* Encode an array of Unicode code points *)
let codepoints = [| Uchar.of_int 0x4ED6; Uchar.of_int 0x4EEC |]
let encoded = Punycode.encode codepoints
(* Result is Punycode string *)

(* Decode to code points *)
let decoded = Punycode.decode "ihqwcrb4cv8a8dqg056pqjye"
(* Result is Uchar.t array *)
```

### Integration with domain-name Library

```ocaml
(* Convert a Domain_name.t to ASCII *)
let domain = Domain_name.of_string_exn "münchen.example.com" in
let ascii = Punycode_idna.domain_to_ascii domain
(* = Ok (Domain_name for "xn--mnchen-3ya.example.com") *)

(* Convert back to Unicode *)
let unicode = Punycode_idna.domain_to_unicode ascii
(* = Ok (original domain) *)
```

## Installation

```
opam install puny
```

## Documentation

API documentation is available at https://tangled.org/@anil.recoil.org/ocaml-punycode or via:

```
opam install puny
odig doc puny
```

## Limitations

The following IDNA 2008 features are not yet implemented:

- **Bidi rules** (RFC 5893): Bidirectional text validation for right-to-left scripts
- **Contextual joiners** (RFC 5892 Appendix A.1): Zero-width joiner/non-joiner validation

These checks are disabled by default in the API. Most common use cases (European languages, CJK) work correctly without them.

## References

- [RFC 3492](https://datatracker.ietf.org/doc/html/rfc3492) - Punycode: A Bootstring encoding of Unicode for IDNA
- [RFC 5891](https://datatracker.ietf.org/doc/html/rfc5891) - Internationalized Domain Names in Applications (IDNA): Protocol
- [RFC 5892](https://datatracker.ietf.org/doc/html/rfc5892) - Unicode Code Points and IDNA
- [RFC 5893](https://datatracker.ietf.org/doc/html/rfc5893) - Right-to-Left Scripts for IDNA
- [RFC 1035](https://datatracker.ietf.org/doc/html/rfc1035) - Domain Names Implementation and Specification

## License

ISC
