# `httpz.punycode`

`httpz.punycode` implements the RFC 3492 Punycode algorithm for OCaml. The
companion `httpz.punycode.idna` library provides UTF-8 domain-label conversion
and NFC normalization.

```ocaml
let payload = Punycode.encode_utf8 "münchen"
(* payload = "mnchen-3ya" *)

let label = Punycode.encode_label "münchen"
(* label = "xn--mnchen-3ya" *)

let ascii = Punycode_idna.to_ascii "münchen.example"
(* ascii = "xn--mnchen-3ya.example" *)

let display = Punycode_idna.to_unicode ascii
(* display = "münchen.example" *)
```

IDNA conversion rejects malformed UTF-8 before normalization, checks the
63-byte DNS limit on the resulting ASCII label, and preserves a trailing DNS
root dot.

Conversion functions return strings and raise `Punycode.Error` or
`Punycode_idna.Error` on failure.

The IDNA helper is intentionally limited. It applies NFC normalization, DNS
length limits, optional ASCII STD3 checks, and a hyphen-position check. It does
not implement RFC 5892 code-point tables, RFC 5893 bidirectional checks,
contextual joiner checks, or UTS #46 mapping. Apply a complete IDNA validator
before resolving untrusted internationalized names.

Install the libraries as part of the `httpz` package:

```sh
opam install httpz
```

References: [RFC 3492](https://www.rfc-editor.org/rfc/rfc3492.html),
[RFC 5891](https://www.rfc-editor.org/rfc/rfc5891.html), and
[Unicode Standard Annex #15](https://www.unicode.org/reports/tr15/).
