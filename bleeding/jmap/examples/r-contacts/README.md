# `r-contacts`

<br>

This step is the address book pane of a client. It lists the address books,
creates two cards in one request, finds them with a query whose ids feed the
get beside it, fetches one card truncated, patches a property, and destroys
what it made.

It is the first step outside mail. Contacts are
[RFC 9610](https://www.rfc-editor.org/rfc/rfc9610), a data model of two types
over the same core: an **AddressBook** is a named collection, and a
**ContactCard** is a [JSContact](https://www.rfc-editor.org/rfc/rfc9553) Card
with an `id` and a set of `addressBookIds` added.

```sh
dune exec -- examples/r-contacts/contacts.exe --profile personal
```

With the [oracle profile](../0-profiles#readme):

```sh
dune exec -- examples/r-contacts/contacts.exe --profile oracle --allow-insecure
```

<br>

## The account is the contacts one

Every step so far took the primary **mail** account. A contacts program wants
the primary **contacts** account, which a server may put elsewhere, so the
frame names the capability it wants:

```ocaml
Jmap_eio.Cli.main "contacts" ~doc ~capability:Proto.Capability.contacts
@@ fun ctx -> ...
```

`Cli.main` then resolves `ctx.account_id` from
`primaryAccounts["urn:ietf:params:jmap:contacts"]`
([RFC 8620 §2](https://www.rfc-editor.org/rfc/rfc8620#section-2)) unless
`--account` names one. Nothing else about the frame changes: the client sends
the contacts capability in `using` on its own, because
`Client.default_capabilities` includes every capability the library builds
calls for that the session advertises.

<br>

## The card body is JSContact, not JMAP

A `Contacts_card.t` is three fields:

```ocaml
type t = {
  id : Proto.Id.t option;
  address_book_ids : (Proto.Id.t * bool) list option;
  card : Jscontact.Card.t;
}
```

The first two are what [RFC 9610 §3](https://www.rfc-editor.org/rfc/rfc9610#section-3)
adds; everything a person would recognise as contact data — names, emails,
phones, addresses, anniversaries — is in `card`, typed by the separate
[`jscontact`](https://github.com/avsm/ocaml-jscontact) library. So building one
is building a JSContact Card and saying which books it belongs to:

```ocaml
Proto.Contact_card.make
  ~address_book_ids:[ (book, true) ]
  (Card.make ~name ~emails uid)
```

A card must belong to at least one address book at all times, so the `create`
of a `/set` always names one.

<br>

## Names are components, not a string

A JSContact name is a list of typed components, a `full` string, or both
([RFC 9553 §2.2.1](https://www.rfc-editor.org/rfc/rfc9553#section-2.2.1)). A
component of kind `separator` holds the punctuation between the others rather
than a word, which is why joining them skips it:

```ocaml
let display_name (card : Card.t) =
  match card.name with
  | Some { full = Some full; _ } -> full
  | Some { components = Some parts; _ } ->
      let word (c : Jscontact.Name.Component.t) =
        match c.kind with `Separator -> None | _ -> Some c.value
      in
      String.concat " " (List.filter_map word parts)
  | Some _ | None -> "(unnamed)"
```

This is the shape that lets a client sort by surname in a culture whose
surname comes first, which a single display string cannot.

<br>

## A truncated card is not a whole Card

`ContactCard/get` takes a `properties` argument like any other `/get`, and a
server that is given one returns those properties and nothing else
([RFC 8620 §5.1](https://www.rfc-editor.org/rfc/rfc8620#section-5.1)). The
`@type`, `version` and `uid` that RFC 9553 makes mandatory therefore all go
missing:

```
Truncated fetch: name "Ada Lovelace", uid "", emails 0
```

The library reads such a response with `Jscontact.Card.partial_jsont`, which
accepts all three as absent and leaves `uid` empty. A card fetched this way is
a view of a card, not a Card: `Jscontact.Card.validate` rejects it, and
`uid <> ""` is the test for having a whole one. Ask for no `properties` and you
get the whole card back.

<br>

## Patching reaches only into what exists

The nickname patch replaces the whole `nicknames` property rather than writing
`nicknames/n1` into it, because
[RFC 8620 §5.3](https://www.rfc-editor.org/rfc/rfc8620#section-5.3) requires
every reference token before the last to exist already and this card has no
nicknames yet. A server answers the other form with an `invalidPatch`
SetError. [**`7-keywords`**](../7-keywords#readme) covers PatchObjects in full.

<br>

## What the run prints

```
Address books, state 212
  Default Personal [default]

Created 2 cards, state 212 -> 216
The book holds 2 card(s)
urn:uuid:example-r-contacts-grace  Grace Hopper         grace@example.com
urn:uuid:example-r-contacts-ada  Ada Lovelace         ada@example.com
Truncated fetch: name "Ada Lovelace", uid "", emails 0
Nicknames after the patch: Countess
Destroyed 2 card(s)
```

The two cards are destroyed under `Fun.protect`, so a failure part way through
still leaves the address book as it was found.

<br>

## See also

- [RFC 9610](https://www.rfc-editor.org/rfc/rfc9610) — JMAP for Contacts.
- [RFC 9553](https://www.rfc-editor.org/rfc/rfc9553) — JSContact, the card itself.
- {!Jmap.Proto.Address_book} and {!Jmap.Proto.Contact_card}.
- [**`8-mailboxes-set`**](../8-mailboxes-set#readme) — creation ids, which name
  the two cards here before the server has given them ids.
