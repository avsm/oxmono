# Asset Operations

The Karakeep library provides functions for working with assets (images, documents, etc.) attached to bookmarks.

## Types

```ocaml
type asset_id = string

type asset_type =
  | Screenshot        (* Screenshot of a webpage *)
  | AssetScreenshot   (* Screenshot of an asset *)
  | BannerImage       (* Banner image *)
  | FullPageArchive   (* Archive of a full webpage *)
  | Video             (* Video asset *)
  | BookmarkAsset     (* Generic bookmark asset *)
  | PrecrawledArchive (* Pre-crawled archive *)
  | Unknown           (* Unknown asset type *)

type asset = {
  id : asset_id;
  asset_type : asset_type;
}
```

## Getting Asset URL

### `get_asset_url`

Get the asset URL for a given asset ID.

```ocaml
val get_asset_url : string -> asset_id -> string
```

#### Parameters

- `base_url`: Base URL of the Karakeep instance
- `asset_id`: ID of the asset

#### Example

```ocaml
let asset_url = 
  let base_url = "https://hoard.recoil.org" in
  let asset_id = "asset123" in
  Karakeep.get_asset_url base_url asset_id
```

## Fetching Assets

### `fetch_asset`

Fetch an asset from the Karakeep server as a binary string.

```ocaml
val fetch_asset : api_key:string -> string -> asset_id -> string Lwt.t
```

#### Parameters

- `api_key`: API key for authentication
- `base_url`: Base URL of the Karakeep instance
- `asset_id`: ID of the asset to fetch

#### Example

```ocaml
let fetch_asset_content = 
  let api_key = "your_api_key" in
  let base_url = "https://hoard.recoil.org" in
  let asset_id = "asset123" in
  let* binary_data = Karakeep.fetch_asset ~api_key base_url asset_id in
  (* Do something with the binary data *)
  Lwt.return_unit
```

## Attaching Assets

### `attach_asset`

Attach an asset to a bookmark.

```ocaml
val attach_asset :
  api_key:string ->
  asset_id:asset_id ->
  asset_type:asset_type ->
  string ->
  bookmark_id ->
  asset Lwt.t
```

#### Parameters

- `api_key`: API key for authentication
- `asset_id`: ID of the asset to attach
- `asset_type`: Type of the asset
- `base_url`: Base URL of the Karakeep instance
- `bookmark_id`: ID of the bookmark to attach the asset to

#### Example

```ocaml
let attach_screenshot =
  let api_key = "your_api_key" in
  let base_url = "https://hoard.recoil.org" in
  let bookmark_id = "bookmark123" in
  let asset_id = "asset123" in
  let* attached_asset = 
    Karakeep.attach_asset 
      ~api_key 
      ~asset_id 
      ~asset_type:Karakeep.Screenshot
      base_url 
      bookmark_id in
  Printf.printf "Attached asset %s of type %s\n" 
    attached_asset.id 
    (match attached_asset.asset_type with 
     | Screenshot -> "screenshot" 
     | _ -> "other");
  Lwt.return_unit
```

## Replacing Assets

### `replace_asset`

Replace an asset on a bookmark with a new one.

```ocaml
val replace_asset :
  api_key:string ->
  new_asset_id:asset_id ->
  string ->
  bookmark_id ->
  asset_id ->
  unit Lwt.t
```

#### Parameters

- `api_key`: API key for authentication
- `new_asset_id`: ID of the new asset
- `base_url`: Base URL of the Karakeep instance
- `bookmark_id`: ID of the bookmark
- `asset_id`: ID of the asset to replace

#### Example

```ocaml
let replace_screenshot =
  let api_key = "your_api_key" in
  let base_url = "https://hoard.recoil.org" in
  let bookmark_id = "bookmark123" in
  let old_asset_id = "old_asset123" in
  let new_asset_id = "new_asset456" in
  let* () = 
    Karakeep.replace_asset 
      ~api_key 
      ~new_asset_id
      base_url 
      bookmark_id
      old_asset_id in
  Printf.printf "Replaced asset successfully\n";
  Lwt.return_unit
```

## Detaching Assets

### `detach_asset`

Detach an asset from a bookmark.

```ocaml
val detach_asset :
  api_key:string -> string -> bookmark_id -> asset_id -> unit Lwt.t
```

#### Parameters

- `api_key`: API key for authentication
- `base_url`: Base URL of the Karakeep instance
- `bookmark_id`: ID of the bookmark
- `asset_id`: ID of the asset to detach

#### Example

```ocaml
let detach_screenshot =
  let api_key = "your_api_key" in
  let base_url = "https://hoard.recoil.org" in
  let bookmark_id = "bookmark123" in
  let asset_id = "asset123" in
  let* () = 
    Karakeep.detach_asset 
      ~api_key 
      base_url 
      bookmark_id
      asset_id in
  Printf.printf "Detached asset successfully\n";
  Lwt.return_unit
```