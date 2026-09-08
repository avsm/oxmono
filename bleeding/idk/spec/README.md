# Vendored specifications

The RFCs this library is written against, kept in the repository so that every
citation in the source can be checked against the text it cites, offline and at
the version the code was written to.

| File | RFC | Title | Why it is here |
|---|---|---|---|
| `rfc9553.txt` | [9553](https://www.rfc-editor.org/rfc/rfc9553.html) | JSContact: A JSON Representation of Contact Data | The specification this library implements. Every module cites the section it comes from. |
| `rfc9555.txt` | [9555](https://www.rfc-editor.org/rfc/rfc9555.html) | JSContact: Converting from and to vCard | The companion conversion spec. Not implemented, but it registers three more JSContact properties, the `JSCOMPS` parameter `vcard` reads, and the vendored test corpus is largely drawn from it. |
| `rfc6350.txt` | [6350](https://www.rfc-editor.org/rfc/rfc6350.html) | vCard Format Specification | The specification the `vcard` library implements. |
| `rfc6868.txt` | [6868](https://www.rfc-editor.org/rfc/rfc6868.html) | Parameter Value Encoding in iCalendar and vCard | The circumflex escapes `Vcard.Param` applies to parameter values. |
| `rfc7095.txt` | [7095](https://www.rfc-editor.org/rfc/rfc7095.html) | jCard: The JSON Format for vCard | The JCardProp encoding `Jscontact_vcard.Jcard` writes for a vCard property with no JSContact counterpart. |
| `rfc9554.txt` | [9554](https://www.rfc-editor.org/rfc/rfc9554.html) | vCard Format Extensions for JSContact | The extra `N` and `ADR` components, properties and parameters `vcard` knows. |
| `rfc4918.txt` | [4918](https://www.rfc-editor.org/rfc/rfc4918.html) | HTTP Extensions for WebDAV | Implemented by `httpz.dav` and `fetch.dav` in the fetch repository. Vendored for the citations in `carddav` and `caldav`. |
| `rfc3253.txt` | [3253](https://www.rfc-editor.org/rfc/rfc3253.html) | Versioning Extensions to WebDAV | The REPORT method and `DAV:supported-report-set`, Sections 3.6 and 3.1.5, in `fetch.dav` and `httpz.dav`. |
| `rfc3744.txt` | [3744](https://www.rfc-editor.org/rfc/rfc3744.html) | WebDAV Access Control Protocol | The principal properties `httpz.dav` reads. |
| `rfc5397.txt` | [5397](https://www.rfc-editor.org/rfc/rfc5397.html) | WebDAV Current Principal Extension | `DAV:current-user-principal`, read by `fetch.dav` discovery. |
| `rfc5689.txt` | [5689](https://www.rfc-editor.org/rfc/rfc5689.html) | Extended MKCOL for WebDAV | The MKCOL body `fetch.dav` sends to create an address book. |
| `rfc5995.txt` | [5995](https://www.rfc-editor.org/rfc/rfc5995.html) | Using POST to Add Members to WebDAV Collections | The `DAV:add-member` property, named in `httpz.dav`. |
| `rfc6578.txt` | [6578](https://www.rfc-editor.org/rfc/rfc6578.html) | Collection Synchronization for WebDAV | The sync-collection report `fetch.dav` runs. |
| `rfc6764.txt` | [6764](https://www.rfc-editor.org/rfc/rfc6764.html) | Locating Services for CalDAV and CardDAV | The well-known path and discovery steps `fetch.dav` follows. |
| `rfc6352.txt` | [6352](https://www.rfc-editor.org/rfc/rfc6352.html) | CardDAV: vCard Extensions to WebDAV | The specification the `carddav` package implements. |
| `rfc4791.txt` | [4791](https://www.rfc-editor.org/rfc/rfc4791.html) | CalDAV | The specification the `caldav` package implements. |
| `rfc5545.txt` | [5545](https://www.rfc-editor.org/rfc/rfc5545.html) | iCalendar | The specification the `ical` package implements. |
| `rfc7986.txt` | [7986](https://www.rfc-editor.org/rfc/rfc7986.html) | New Properties for iCalendar | The calendar properties `ical` registers. |
| `rfc6638.txt` | [6638](https://www.rfc-editor.org/rfc/rfc6638.html) | Scheduling Extensions to CalDAV | Not implemented. The `calendar-user-address-set` property is named. |
| `rfc5546.txt` | [5546](https://www.rfc-editor.org/rfc/rfc5546.html) | iTIP | Not implemented. Vendored for the METHOD values a calendar object may carry. |
| `rfc7809.txt` | [7809](https://www.rfc-editor.org/rfc/rfc7809.html) | CalDAV Time Zones by Reference | Not implemented. |
| `rfc9610.txt` | [9610](https://www.rfc-editor.org/rfc/rfc9610.html) | JMAP for Contacts | The reason this library exists: a `ContactCard` is a JSContact Card. Not implemented here; it belongs to a JMAP library. |

## What is implemented

**RFC 9553 in full.** Every object type of Section 2, the common data types of
Section 1.4, the validation rules of Section 1.7 and the localization algorithm
of Section 2.7.1.

**RFC 6350, 6868 and 9554 in the `vcard` package.** The content line syntax,
value data types, parameters and properties of vCard 4.0, with typed views of
the extended `N` and `ADR` components. The `JSCOMPS` parameter of RFC 9555 is
read and written there too.

**RFC 4918, 3253, 3744, 5397, 5689, 5995, 6578 and 6764 by `httpz.dav` and
`fetch.dav`.** Those libraries live in the fetch repository. The packages here
build on them and vendor the RFCs for their citations.

**RFC 6352 in the `carddav` package.** The address book properties and
resource type, the `addressbook-query` report with its filters and partial
retrieval, the `addressbook-multiget` report, and the preconditions. The
client in `carddav.eio` performs discovery, creates and lists address books,
reads, writes and deletes address objects with conditional requests, runs the
reports and synchronises. `Carddav.Filter.matches` evaluates a filter on a
`Vcard.t` by the rules of Section 10.5, so a server's answer can be checked.

**RFC 5545 and 7986 in the `ical` package.** The content line syntax,
value data types, components and properties of iCalendar, read and written
losslessly, with validation of the cardinalities of Section 3.6. Recurrence
rules are parsed and validated but not expanded.

**RFC 4791 in the `caldav` package.** The calendar properties and resource
type, the `calendar-query` report with its filters, partial
retrieval and expansion, `calendar-multiget`, `free-busy-query` and the
preconditions. Scheduling (RFC 6638) is not implemented.

**RFC 9555 in the `jscontact.vcard` sublibrary.** Both directions of the
conversion, with `vCardProps`, `vCardParams`, `vCardName`, `JSPROP`, `JSPTR`
and `JSCOMPS` for what has no counterpart. Two consequences remain visible in
the core library: Two consequences
are visible in this library:

- Its Section 5.3 adds `vCardName`, `vCardParams` and `vCardProps` to the IANA
  "JSContact Properties" registry. They are in `Jscontact.Vendor.registered`,
  so a name differing from one of them only in case is rejected as Section
  1.7.1 requires, but they are not typed: a value carries them through its
  `unknown` members and writes them back unchanged.
- Its `JCardProp` type is likewise untyped here.

**RFC 9610 not at all.** It adds `id` and `addressBookIds` to a Card, and a
`blobId` to a Media. Those too survive in `unknown` members today. Note that
Section 2 of RFC 9610 defines a `ContactCard` as "a JSContact Card object as
defined in Section 2 of RFC 9553 with the following additional properties",
which relaxes none of the Card rules: `@type`, `version` and `uid` stay
mandatory, and Cyrus refuses a `ContactCard/set` create that omits the first
two.

## Refreshing

    curl -sSLO https://www.rfc-editor.org/rfc/rfc9553.txt

RFCs are immutable once published, so these files change only if one of them is
obsoleted and the library moves to its successor.

## Citation style

Citations in the source link to `rfc-editor.org` and name the section, so that
a reader lands on the paragraph rather than the document:

```ocaml
(** [validate_pref p] is [Ok p] if [p] is in the range 1 to 100, which
    {{:https://www.rfc-editor.org/rfc/rfc9553.html#section-1.5.3} Section 1.5.3}
    requires of a preference. *)
```

The first citation in a module gives the RFC number, and later ones in the same
module give the section alone.
