# idk

IDKit: contact data for OCaml. The repository holds one package per
specification and the `idk` umbrella that installs them all:

| Package | What it is |
|---|---|
| `jscontact` | RFC 9553 JSContact, with the `jscontact.vcard` conversion of RFC 9555 |
| `vcard` | RFC 6350 vCard 4.0 with RFC 6868 and RFC 9554 |
| `ical` | RFC 5545 iCalendar with RFC 7986 |
| `carddav` | RFC 6352 CardDAV, with an Eio client and a JSContact view of address objects |
| `caldav` | RFC 4791 CalDAV, with an Eio client |

The JSContact `Card` is the representation the others meet at. A vCard converts
to and from it by RFC 9555, a CardDAV address object is read and written as
one through `carddav.jscontact`, and a JMAP `ContactCard` is one already.

WebDAV itself comes from `httpz.dav` and `fetch.dav` in the [fetch][fetch]
repository, which carry the XML codecs, the Multi-Status reader, the live
property readers, the sync-collection report of RFC 6578, the extended MKCOL
of RFC 5689, MKCALENDAR, REPORT and the principal discovery of RFC 6764. The
packages here add only the namespaces, reports and data formats of each
protocol.

## jscontact

JSContact for OCaml: a typed representation of contact data as defined by
[RFC 9553][rfc9553], with [jsont][jsont] codecs for every object type the
specification defines.

JSContact is the JSON contact data model that succeeds vCard. It is the payload
of the `ContactCard` object in [JMAP for Contacts (RFC 9610)][rfc9610], and this
library exists to be that payload's implementation.

[rfc9553]: https://www.rfc-editor.org/rfc/rfc9553.html
[rfc9610]: https://www.rfc-editor.org/rfc/rfc9610.html
[jsont]: https://erratique.ch/software/jsont

## vcard

This repository also holds `vcard`, a separate opam package implementing the
vCard 4.0 format of [RFC 6350][rfc6350] with the parameter escapes of
[RFC 6868][rfc6868] and the extensions of [RFC 9554][rfc9554] that JSContact
conversion needs. See `vcard/doc/index.mld`. The `jscontact.vcard` sublibrary
converts between the two by the rules of [RFC 9555][rfc9555]. A vCard property
with no JSContact counterpart survives as a `vCardProps` entry, a parameter no
rule converts as `vCardParams`, and a JSContact property with no vCard
counterpart as a `JSPROP`, so a Card converted to vCard and back is unchanged.

```ocaml
let card = Result.get_ok (Jscontact_vcard.of_vcard vcard) in
let vcard' = Result.get_ok (Jscontact_vcard.to_vcard card) in
...
```

[rfc6350]: https://www.rfc-editor.org/rfc/rfc6350.html
[rfc6868]: https://www.rfc-editor.org/rfc/rfc6868.html
[rfc9554]: https://www.rfc-editor.org/rfc/rfc9554.html
[rfc9555]: https://www.rfc-editor.org/rfc/rfc9555.html

## CardDAV

`carddav/` adds the address book properties, the `addressbook-query` and
`addressbook-multiget` reports and the filters of [RFC 6352][rfc6352] on
`httpz.dav`. `carddav.eio` is the client, on `fetch.dav`, [Eio][eio] and
[fetch][fetch] like `jmap.eio`, and `carddav.jscontact` lets it read and write
address objects as JSContact cards:

```ocaml
let client =
  Carddav_eio.Client.connect ~sw ~credentials (Fetch_httpz.std env)
    "https://contacts.example.com/.well-known/carddav"
  |> Result.get_ok
in
let cards, _truncated =
  Carddav_eio.Client.query Carddav_jscontact.card client book
    Carddav.Filter.(v [ prop "EMAIL" [ text_match "example.com" ] ])
  |> Result.get_ok
```

[rfc6352]: https://www.rfc-editor.org/rfc/rfc6352.html
[eio]: https://github.com/ocaml-multicore/eio
[fetch]: https://github.com/avsm/httpz

## CalDAV

`ical/` implements [RFC 5545][rfc5545] losslessly, with the properties
of [RFC 7986][rfc7986]; the content line syntax is that of vCard, so its
reader is shared with `vcard`. `caldav/` adds the calendar properties,
the `calendar-query` report with component, property, parameter
and time-range filters, partial retrieval and recurrence expansion, the
`calendar-multiget` and `free-busy-query` reports, and the conditions of
[RFC 4791][rfc4791]. `caldav.eio` is the client, in the shape of
`carddav.eio`:

```ocaml
let events, _ =
  Caldav_eio.Client.events Caldav.Data.ical client
    ~start ~finish ~expand:true calendar
  |> Result.get_ok
```

Recurrence rules are read and written but not expanded locally. The server
expands them on request, RFC 4791 Section 9.6.5.

[rfc5545]: https://www.rfc-editor.org/rfc/rfc5545.html
[rfc7986]: https://www.rfc-editor.org/rfc/rfc7986.html
[rfc4791]: https://www.rfc-editor.org/rfc/rfc4791.html

### Mirroring a collection

`idk-dav-mirror` keeps a directory in step with an address book or a calendar
by the RFC 6578 sync-collection report, using `Fetch_dav.Mirror` from
`fetch.dav`, and prints every action as it takes it:

```sh
idk-dav-mirror contacts https://carddav.fastmail.com/.well-known/carddav ./contacts \
  -u user@example.com -p ~/.password
```

The directory holds one file per member and a `.davsync` index with the sync
token and the entity tag each file was fetched at. A second run fetches only
what changed, removes what the server removed, pages through truncated
results, rebuilds after a refused token and falls back to entity-tag polling
on a server without the report. `test/carddav/oracle/test_mirror.ml` and
`test/caldav/oracle/test_mirror.ml` exercise it against the oracle.

### Testing against a real server

`test/carddav/oracle` and `test/caldav/oracle` run the clients against a
[Radicale][radicale] server in docker, which serves both protocols on one
port. Nothing there runs unless `CARDDAV_ORACLE_URL` is set, so a plain
`dune runtest` stays offline.

```sh
scripts/carddav-up.sh                 # docker run, waits for PROPFIND to answer
eval "$(scripts/carddav-env.sh)"      # exports CARDDAV_ORACLE_URL
dune build @test/carddav/oracle/runtest @test/caldav/oracle/runtest --force
scripts/carddav-down.sh               # stop and remove the container
```

The server listens on `localhost:15232`, authenticates any user with any
password, and creates a principal for the user on first use. The same suites
run against any CardDAV or CalDAV service by pointing the variables at it,
for instance Fastmail with an app password:

```sh
CARDDAV_ORACLE_URL=https://carddav.fastmail.com/.well-known/carddav \
CARDDAV_ORACLE_USER=user@example.com CARDDAV_ORACLE_PASSWORD=... \
  dune build @test/carddav/oracle/runtest --force
```

Every collection a test creates is removed when it ends.

### Fastmail

Fastmail is a supported target, and its departures from the RFCs are named
in `Carddav_eio.Quirks.fastmail` and `Caldav_eio.Quirks.fastmail`, which
`connect` applies to any `fastmail.com` host unless told otherwise. Each
quirk is consulted only in the client function it affects, so the protocol
libraries stay what the RFCs say, and the oracle suites are the evidence:

| Observed | What the client does |
|---|---|
| Address objects are stored and served as vCard 3.0, with `KIND` as `X-ADDRESSBOOKSERVER-KIND`, `PREF=1` folded into `TYPE=PREF`, an `N` added, and 4.0-only properties dropped | `Carddav_eio.Quirks.upgrade_vcard3` maps the first two back before decoding, so a JSContact card round trips |
| A new calendar stores VEVENT only, whatever MKCALENDAR asked for, and answers other components with 403 | `add` checks the calendar's component set first and reports a `Data` error naming the component |
| A `param-filter` with a `text-match`, such as ATTENDEE by PARTSTAT, matches nothing | `query` sends the filter without its parameter conditions and applies `Caldav.Filter.matches` to what comes back |
| The first expanded recurrence instance carries no `RECURRENCE-ID` | `events ~expand` adds one from its DTSTART |
| A `CATEGORIES` list comes back as one property per value | Allowed by RFC 5545; read them all with `Component.find_all` |
| The files service ignores `If-None-Match: *` and a stale `If-Match` on PUT, answers COPY onto a locked destination with 409, and returns hrefs with unencoded spaces | `Fetch_dav.v ~lenient_hrefs:true` repairs the hrefs; the conditional PUT cannot be made safe by a client and is left to the caller |

Everything else was as the RFCs say: conditional PUT and DELETE on address
books and calendars, `param-filter` and `match-type` on address books, UID
conflicts, invalid data, stored calendar time zones, recurrence expansion,
free-busy and sync-collection on both. Radicale ignores the component
selection of a calendar-data request, which the tests note.

| variable | default | meaning |
|---|---|---|
| `CARDDAV_ORACLE_URL` | *unset*, tests skip | the server root |
| `CARDDAV_ORACLE_USER` | `alice` | login name |
| `CARDDAV_ORACLE_PASSWORD` | `x` | password |
| `CARDDAV_ORACLE_HTTP_PORT` | `15232` | read by `carddav-up.sh` only |
| `CARDDAV_ORACLE_IMAGE` | `tomsquest/docker-radicale:latest` | image to run |

[radicale]: https://radicale.org

## What is in jscontact

Every object type of RFC 9553 Section 2, each with a record, a `jsont` codec, a
`validate` function and structural equality:

| Section | Types |
|---|---|
| 1.4 | `Id`, `PatchObject`, `Resource`, `UTCDateTime` |
| 2.1 | `Card` metadata, `Relation` |
| 2.2 | `Name`, `NameComponent`, `Nickname`, `Organization`, `OrgUnit`, `SpeakToAs`, `Pronouns`, `Title` |
| 2.3 | `EmailAddress`, `OnlineService`, `Phone`, `LanguagePref` |
| 2.4 | `Calendar`, `SchedulingAddress` |
| 2.5 | `Address`, `AddressComponent` |
| 2.6 | `CryptoKey`, `Directory`, `Link`, `Media` |
| 2.7 | `localizations`, and the algorithm that applies one |
| 2.8 | `Anniversary`, `PartialDate`, `Timestamp`, `Note`, `Author`, `PersonalInfo` |

## Two things the specification asks for that are easy to get wrong

**Unknown properties survive.** Section 1.7.4 requires an implementation to
preserve a property it does not understand rather than drop it, and Section
1.8.1 requires the same of a vendor-specific property such as
`example.com:foo`. Every object here keeps them in an `unknown` field and
writes them back out, so a Card can pass through this library without losing
data an extension put there.

**Enumerated values are open.** Section 1.7.5 lets any enumerated property
carry a vendor-specific value, so each enum is a polymorphic variant with a
`` `Vendor of string `` case. A value from a later JSContact version decodes
rather than failing.

## Shape versus meaning

A codec here enforces the *shape* of an object: the type of each property and
the presence of the mandatory ones. It does not enforce the rules the RFC
states in prose — "at least one of the name and units properties MUST be set",
"the value MUST be in the range of 1 to 100", "MUST NOT be set if the
isOrdered property value is false". Those live in the `validate` function of
each type.

The split is deliberate. A Card a server sends still decodes into a usable
value even if it breaks a rule, which is what an address book client wants;
an application that creates or edits a Card calls `validate` and holds itself
to Section 1.7.

## Building

    dune build
    dune runtest

### Testing against another implementation

`test/corpus` runs the JSContact fixtures of
[calcard](https://github.com/stalwartlabs/calcard), Stalwart Labs' Rust
implementation of vCard, JSContact and their conversions, through a decode,
encode, decode cycle. It checks that each fragment decodes, that the cycle
reaches a fixed point, that the value survives it unchanged, and that no
property is lost — a member may only disappear if it is an implied `@type` or
a value equal to a default the RFC documents. It runs as part of `dune
runtest`; 186 fragments, no external checkout needed.

The fixtures are vendored verbatim under `test/corpus/data`, whose README
records the upstream commit and how to refresh them. To check the vendored copy
against a working tree of calcard, point `JSCONTACT_CORPUS` at it:

    JSCONTACT_CORPUS=../calcard/resources/jscontact dune runtest test/corpus

## Specifications

RFC 9553 is vendored at `spec/rfc9553.txt`, alongside RFC 9555 (vCard
conversion) and RFC 9610 (JMAP for Contacts) for reference. `spec/README.md`
says what is implemented from each and what is deliberately not. Every module
cites the section it implements, and the citation links to the paragraph.

## Licence

The library is ISC, see `LICENSE.md`.

`test/corpus/data` holds test fixtures vendored from calcard, which is licensed
Apache-2.0 OR MIT at the user's option. They are redistributed here under MIT;
see `test/corpus/data/LICENSE` for the terms and `test/corpus/data/README.md`
for their provenance.
