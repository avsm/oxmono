# Karakeep OCaml Client

This library provides OCaml bindings to the Karakeep API.

## Getting Started

```ocaml
(* Setup the Karakeep client *)
let api_key = "your_api_key"
let base_url = "https://hoard.recoil.org"

(* Fetch bookmarks *)
let bookmarks = Karakeep.fetch_all_bookmarks ~api_key base_url
```

## API Documentation

- [Types](types.md)
- [Bookmarks](bookmarks.md)
- [Assets](assets.md)
