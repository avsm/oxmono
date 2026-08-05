# Karakeep Types

This document describes the main types used in the Karakeep OCL client.

## Core Types

### Identifiers

```ocaml
type asset_id = string
type bookmark_id = string
type list_id = string
type tag_id = string
type highlight_id = string
```

These types are used to represent the unique identifiers for various resources in the Karakeep API.

### Enumerations

```ocaml
type bookmark_content_type =
  | Link     (* A URL to a webpage *)
  | Text     (* Plain text content *)
  | Asset    (* An attached asset (image, PDF, etc.) *)
  | Unknown  (* Unknown content type *)

type asset_type =
  | Screenshot        (* Screenshot of a webpage *)
  | AssetScreenshot   (* Screenshot of an asset *)
  | BannerImage       (* Banner image *)
  | FullPageArchive   (* Archive of a full webpage *)
  | Video             (* Video asset *)
  | BookmarkAsset     (* Generic bookmark asset *)
  | PrecrawledArchive (* Pre-crawled archive *)
  | Unknown           (* Unknown asset type *)

type tagging_status =
  | Success  (* Tagging was successful *)
  | Failure  (* Tagging failed *)
  | Pending  (* Tagging is pending *)

type list_type =
  | Manual   (* List is manually managed *)
  | Smart    (* List is dynamically generated based on a query *)

type highlight_color =
  | Yellow   (* Yellow highlight *)
  | Red      (* Red highlight *)
  | Green    (* Green highlight *)
  | Blue     (* Blue highlight *)

type tag_attachment_type =
  | AI       (* Tag was attached by AI *)
  | Human    (* Tag was attached by a human *)
```

### Content Types

```ocaml
type link_content = {
  url : string;
  title : string option;
  description : string option;
  image_url : string option;
  image_asset_id : asset_id option;
  screenshot_asset_id : asset_id option;
  full_page_archive_asset_id : asset_id option;
  precrawled_archive_asset_id : asset_id option;
  video_asset_id : asset_id option;
  favicon : string option;
  html_content : string option;
  crawled_at : Ptime.t option;
  author : string option;
  publisher : string option;
  date_published : Ptime.t option;
  date_modified : Ptime.t option;
}

type text_content = {
  text : string;
  source_url : string option;
}

type asset_content = {
  asset_type : [ `Image | `PDF ];
  asset_id : asset_id;
  file_name : string option;
  source_url : string option;
  size : int option;
  content : string option;
}

type content =
  | Link of link_content
  | Text of text_content  
  | Asset of asset_content
  | Unknown
```

### Resource Types

```ocaml
type asset = {
  id : asset_id;
  asset_type : asset_type;
}

type bookmark_tag = {
  id : tag_id;
  name : string;
  attached_by : tag_attachment_type;
}

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

type list = {
  id : list_id;
  name : string;
  description : string option;
  icon : string;
  parent_id : list_id option;
  list_type : list_type;
  query : string option;
}

type tag = {
  id : tag_id;
  name : string;
  num_bookmarks : int;
  num_bookmarks_by_attached_type : (tag_attachment_type * int) list;
}

type highlight = {
  bookmark_id : bookmark_id;
  start_offset : int;
  end_offset : int;
  color : highlight_color;
  text : string option;
  note : string option;
  id : highlight_id;
  user_id : string;
  created_at : Ptime.t;
}

type paginated_highlights = {
  highlights : highlight list;
  next_cursor : string option;
}
```

### User Information

```ocaml
type user_info = {
  id : string;
  name : string option;
  email : string option;
}

type user_stats = {
  num_bookmarks : int;
  num_favorites : int;
  num_archived : int;
  num_tags : int;
  num_lists : int;
  num_highlights : int;
}

type error_response = {
  code : string;
  message : string;
}
```

For more information about how to use these types with the API functions, see the [API documentation](index.md).