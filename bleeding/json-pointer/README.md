# json-pointer — JSON Pointer for OCaml

`json-pointer` lets you read and update a value inside a JSON document using
a short string that describes its location. It implements the JSON Pointer
standard, RFC 6901, and works with `Jsont.json` values.

## What is a JSON Pointer?

A JSON Pointer is an address for a value inside JSON, much like a file path
is an address for a file. For example, given this document:

```json
{
  "users": [
    { "name": "Ada" },
    { "name": "Grace" }
  ],
  "active": true
}
```

The pointer `/users/0/name` means: look up `users`, take its first array
element, then look up `name`. The result is `"Ada"`.

| Pointer | Value it selects |
| --- | --- |
| `""` (the empty string) | The whole document |
| `/users` | The array of users |
| `/users/0` | `{ "name": "Ada" }` |
| `/users/0/name` | `"Ada"` |
| `/users/1/name` | `"Grace"` |
| `/active` | `true` |

Each `/` introduces the next step, called a token in JSON Pointer parlance:
- On an object, a token is a member name
- On an array, a token is an index starting at 0

The JSON value determines which meaning applies at parse time. For example,
`/0` selects the member named `"0"` if the document is an object. Array indices
cannot have leading zeroes, so use `0` or `1` and not `00` or `01`.

There are a couple of escapes for member names in this syntax, via `~1` for `/`
and `~0` for `~`. So `/a~1b` selects a member named `"a/b"`, and `/m~0n`
selects one named `"m~n"`. The pointer `/` selects a member whose name is the
empty string and only the empty pointer selects the whole document.

## Installation

With OCaml 5.2 or later and opam:

```sh
opam install json-pointer
```

Add `json-pointer` and `jsont` to the libraries in your Dune executable or
library stanza:

```dune
(libraries json-pointer jsont)
```

## Read a value in OCaml

First, build the document above using Jsont, the JSON library used by
`json-pointer`:

```ocaml
let member name value = Jsont.Json.mem (Jsont.Json.name name) value

let json =
  Jsont.Json.object'
    [ member "users"
        (Jsont.Json.list
           [ Jsont.Json.object' [member "name" (Jsont.Json.string "Ada")];
             Jsont.Json.object' [member "name" (Jsont.Json.string "Grace")] ]);
      member "active" (Jsont.Json.bool true) ]

let pointer = Json_pointer.of_string "/users/0/name"
let name = Json_pointer.get pointer json
(* name is the JSON string "Ada", represented as a Jsont.json value. *)
```

Note that parsing a pointer doesn't check whether its target exists. If a member is
missing, an array index is out of bounds, or traversal otherwise fails,
`get` raises `Jsont.Error`. Use `find` when you want an optional result:

```ocaml
let missing = Json_pointer.find (Json_pointer.of_string "/users/9/name") json
(* None *)
```

For detailed errors without exceptions, use `of_string_result` to parse a
pointer and `get_result` to look it up.

You can also build a pointer from member names and indices. The `/` operator
takes unescaped tokens, and `to_string` handles escaping:

```ocaml
let built = Json_pointer.(root / "users" / "0" / "name")
let address = Json_pointer.to_string built
(* "/users/0/name" *)

let escaped = Json_pointer.(root / "a/b" |> to_string)
(* "/a~1b" *)
```

## Update a value

JSON Patch (RFC 6902) uses pointers to describe where to change a document.
This library provides its six operations: `add`, `remove`, `replace`, `move`,
`copy`, and `test`. Operations that change a document return a new JSON value;
the original is unchanged.

For example, append another user to the array:

```ocaml
let with_katherine =
  Json_pointer.add (Json_pointer.of_string "/users/-") json
    ~value:(Jsont.Json.object'
      [member "name" (Jsont.Json.string "Katherine")])
(* with_katherine has three users; json still has two. *)
```

The final `-` means to "append" when adding to an array. It cannot be used to
read an array element and on an object, it is an ordinary member name.

## Pointers in URIs

A pointer can also be represented as a URI fragment, such as
`#/users/0/name`. The fragment functions handle percent encoding through the
`uri` library and take or return the content without the leading `#`:

```ocaml
let fragment =
  Json_pointer.(of_string "/a b/c%d" |> to_uri_fragment)
(* "/a%20b/c%25d"; prepend '#' when constructing a complete URI *)
```

## Tutorial

The [`example/`](example/README.md) directory is a runnable guide that
starts with pointer syntax and evaluation, then adds URI fragments, JSON Patch,
Jsont codecs, and JMAP wildcards.

For typed OCaml values, the [Jsont codecs example](example/5-jsont-codecs/README.md)
shows how to decode and update values at a pointer. The
[JMAP example](example/6-jmap/README.md) covers the RFC 8620 extension, which
allows `*` to select across array elements using `Json_pointer.Jmap`.

See the [API interface](src/json_pointer.mli) for all operations, or the
[single-page tutorial](doc/tutorial.mld), also included in the generated API
documentation.

## License

[ISC](LICENSE.md)
