# Bookmark Operations

The Karakeep library provides comprehensive functions for working with bookmarks.

## Types

The primary types related to bookmarks are:

```ocaml
type bookmark_id = string

type bookmark_content_type =
  | Link     (* A URL to a webpage *)
  | Text     (* Plain text content *)
  | Asset    (* An attached asset (image, PDF, etc.) *)
  | Unknown  (* Unknown content type *)

type tagging_status =
  | Success  (* Tagging was successful *)
  | Failure  (* Tagging failed *)
  | Pending  (* Tagging is pending *)

type bookmark = {
  id : bookmark_id;
  created_at : Ptime.t;
  modified_at : Ptime.t option;
  title : string option;
  archived : bool;
  favourited : bool;
  tagging_status : tagging_status option;
  note : string option;
  summary : string option;
  tags : bookmark_tag list;
  content : content;
  assets : asset list;
}

type paginated_bookmarks = {
  bookmarks : bookmark list;
  next_cursor : string option;
}
```

## Fetching Bookmarks

### `fetch_bookmarks`

Fetch a page of bookmarks from a Karakeep instance.

```ocaml
val fetch_bookmarks :
  api_key:string ->
  ?limit:int ->
  ?cursor:string ->
  ?include_content:bool ->
  ?archived:bool ->
  ?favourited:bool ->
  string ->
  paginated_bookmarks Lwt.t
```

#### Parameters

- `api_key`: API key for authentication
- `limit`: Number of bookmarks to fetch per page (default: 50)
- `cursor`: Optional pagination cursor for cursor-based pagination
- `include_content`: Whether to include full content (default: true)
- `archived`: Whether to filter for archived bookmarks
- `favourited`: Whether to filter for favourited bookmarks
- `base_url`: Base URL of the Karakeep instance

#### Example

```ocaml
let fetch_recent = 
  let api_key = "your_api_key" in
  let base_url = "https://hoard.recoil.org" in
  let* response = Karakeep.fetch_bookmarks ~api_key ~limit:10 base_url in
  Printf.printf "Fetched %d bookmarks\n" (List.length response.bookmarks);
  Lwt.return_unit
```

### `fetch_all_bookmarks`

Fetch all bookmarks from a Karakeep instance, automatically handling pagination.

```ocaml
val fetch_all_bookmarks :
  api_key:string ->
  ?page_size:int ->
  ?max_pages:int ->
  ?archived:bool ->
  ?favourited:bool ->
  string ->
  bookmark list Lwt.t
```

#### Parameters

- `api_key`: API key for authentication
- `page_size`: Number of bookmarks to fetch per page (default: 50)
- `max_pages`: Maximum number of pages to fetch (None for all pages)
- `archived`: Whether to filter for archived bookmarks
- `favourited`: Whether to filter for favourited bookmarks
- `base_url`: Base URL of the Karakeep instance

#### Example

```ocaml
let fetch_all_favourites = 
  let api_key = "your_api_key" in
  let base_url = "https://hoard.recoil.org" in
  let* bookmarks = 
    Karakeep.fetch_all_bookmarks 
      ~api_key 
      ~favourited:true 
      base_url in
  Printf.printf "Fetched %d favourited bookmarks\n" (List.length bookmarks);
  Lwt.return_unit
```

### `search_bookmarks`

Search for bookmarks matching a query.

```ocaml
val search_bookmarks :
  api_key:string ->
  query:string ->
  ?limit:int ->
  ?cursor:string ->
  ?include_content:bool ->
  string ->
  paginated_bookmarks Lwt.t
```

#### Parameters

- `api_key`: API key for authentication
- `query`: Search query
- `limit`: Number of bookmarks to fetch per page (default: 50)
- `cursor`: Optional pagination cursor
- `include_content`: Whether to include full content (default: true)
- `base_url`: Base URL of the Karakeep instance

#### Example

```ocaml
let search_for_ocaml = 
  let api_key = "your_api_key" in
  let base_url = "https://hoard.recoil.org" in
  let* results = 
    Karakeep.search_bookmarks 
      ~api_key 
      ~query:"ocaml programming" 
      base_url in
  Printf.printf "Found %d results\n" (List.length results.bookmarks);
  Lwt.return_unit
```

### `fetch_bookmark_details`

Fetch detailed information for a single bookmark by ID.

```ocaml
val fetch_bookmark_details :
  api_key:string -> string -> bookmark_id -> bookmark Lwt.t
```

#### Parameters

- `api_key`: API key for authentication
- `base_url`: Base URL of the Karakeep instance
- `bookmark_id`: ID of the bookmark to fetch

#### Example

```ocaml
let fetch_specific_bookmark = 
  let api_key = "your_api_key" in
  let base_url = "https://hoard.recoil.org" in
  let bookmark_id = "123456" in
  let* bookmark = Karakeep.fetch_bookmark_details ~api_key base_url bookmark_id in
  Printf.printf "Fetched bookmark: %s\n" 
    (match bookmark.title with Some t -> t | None -> "Untitled");
  Lwt.return_unit
```

## Creating and Updating Bookmarks

### `create_bookmark`

Create a new bookmark in Karakeep.

```ocaml
val create_bookmark :
  api_key:string ->
  url:string ->
  ?title:string ->
  ?note:string ->
  ?summary:string ->
  ?favourited:bool ->
  ?archived:bool ->
  ?created_at:Ptime.t ->
  ?tags:string list ->
  string ->
  bookmark Lwt.t
```

#### Parameters

- `api_key`: API key for authentication
- `url`: The URL to bookmark
- `title`: Optional title for the bookmark
- `note`: Optional note to add to the bookmark
- `summary`: Optional summary for the bookmark
- `favourited`: Whether the bookmark should be marked as favourite (default: false)
- `archived`: Whether the bookmark should be archived (default: false)
- `created_at`: Optional timestamp for when the bookmark was created
- `tags`: Optional list of tag names to add to the bookmark
- `base_url`: Base URL of the Karakeep instance

#### Example

```ocaml
let create_new_bookmark = 
  let api_key = "your_api_key" in
  let base_url = "https://hoard.recoil.org" in
  let* bookmark = 
    Karakeep.create_bookmark 
      ~api_key 
      ~url:"https://ocaml.org" 
      ~title:"OCaml Programming Language" 
      ~tags:["programming"; "ocaml"; "functional"]
      base_url in
  Printf.printf "Created bookmark with ID: %s\n" bookmark.id;
  Lwt.return_unit
```

### `update_bookmark`

Update an existing bookmark.

```ocaml
val update_bookmark :
  api_key:string ->
  ?title:string ->
  ?note:string ->
  ?summary:string ->
  ?favourited:bool ->
  ?archived:bool ->
  ?url:string ->
  ?description:string ->
  ?author:string ->
  ?publisher:string ->
  ?date_published:Ptime.t ->
  ?date_modified:Ptime.t ->
  ?text:string ->
  ?asset_content:string ->
  string ->
  bookmark_id ->
  bookmark Lwt.t
```

#### Parameters

- `api_key`: API key for authentication
- `title`: Optional new title for the bookmark
- `note`: Optional new note for the bookmark
- `summary`: Optional new summary for the bookmark
- `favourited`: Whether the bookmark should be marked as favourite
- `archived`: Whether the bookmark should be archived
- `url`: Optional new URL for the bookmark
- `description`: Optional new description for the bookmark
- `author`: Optional new author for the bookmark
- `publisher`: Optional new publisher for the bookmark
- `date_published`: Optional new publication date for the bookmark
- `date_modified`: Optional new modification date for the bookmark
- `text`: Optional new text content for the bookmark
- `asset_content`: Optional new asset content for the bookmark
- `base_url`: Base URL of the Karakeep instance
- `bookmark_id`: ID of the bookmark to update

#### Example

```ocaml
let update_bookmark_title = 
  let api_key = "your_api_key" in
  let base_url = "https://hoard.recoil.org" in
  let bookmark_id = "123456" in
  let* updated = 
    Karakeep.update_bookmark 
      ~api_key 
      ~title:"Updated OCaml Programming Language" 
      ~favourited:true
      base_url 
      bookmark_id in
  Printf.printf "Updated bookmark title to: %s\n" 
    (match updated.title with Some t -> t | None -> "Untitled");
  Lwt.return_unit
```

### `delete_bookmark`

Delete a bookmark by its ID.

```ocaml
val delete_bookmark : api_key:string -> string -> bookmark_id -> unit Lwt.t
```

#### Parameters

- `api_key`: API key for authentication
- `base_url`: Base URL of the Karakeep instance
- `bookmark_id`: ID of the bookmark to delete

#### Example

```ocaml
let delete_bookmark = 
  let api_key = "your_api_key" in
  let base_url = "https://hoard.recoil.org" in
  let bookmark_id = "123456" in
  let* () = Karakeep.delete_bookmark ~api_key base_url bookmark_id in
  Printf.printf "Deleted bookmark successfully\n";
  Lwt.return_unit
```

### `summarize_bookmark`

Generate a summary for a bookmark using AI.

```ocaml
val summarize_bookmark :
  api_key:string -> string -> bookmark_id -> bookmark Lwt.t
```

#### Parameters

- `api_key`: API key for authentication
- `base_url`: Base URL of the Karakeep instance
- `bookmark_id`: ID of the bookmark to summarize

#### Example

```ocaml
let summarize_bookmark = 
  let api_key = "your_api_key" in
  let base_url = "https://hoard.recoil.org" in
  let bookmark_id = "123456" in
  let* bookmark = Karakeep.summarize_bookmark ~api_key base_url bookmark_id in
  Printf.printf "Generated summary: %s\n" 
    (match bookmark.summary with Some s -> s | None -> "No summary generated");
  Lwt.return_unit
```