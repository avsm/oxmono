(** {1 Karakeep}

    The API for the Karakeep app

    @version 1.0.0 *)

type t

val of_fetch : ?max_response_bytes:int -> base_url:string -> _ Fetch.t -> t
(** Use an existing Fetch stack, including scoped credentials, retries and limits.
    [max_response_bytes] defaults to 16 MiB. JSON requires a declared Content-Type.
    Response bodies are closed before returning. Writes do not follow redirects. *)

val create :
  ?session:_ Fetch.t ->
  ?max_response_bytes:int ->
  sw:Eio.Switch.t ->
  < clock : _ Eio.Time.clock
  ; mono_clock : _ Eio.Time.Mono.t
  ; secure_random : _ Eio.Flow.source
  ; .. > ->
  base_url:string ->
  t
(** [create ?session ~sw env ~base_url] is a client rooted at [base_url].
    [session] is the HTTP client to issue requests through, already carrying
    whatever credentials and policy the caller wants; when it is omitted a
    default {!Fetch_curl.std} stack is created under [sw]. *)

val base_url : t -> string
val session : t -> Fetch.plain

module TagId : sig
  module T : sig
    type t = string

    val jsont : t Jsont.t

    val v : t -> t
  end
end

module Tag : sig
  module T : sig
    type t

    (** Construct a value *)
    val v : id:string -> name:string -> num_bookmarks:float -> num_bookmarks_by_attached_type:Jsont.json -> unit -> t

    val id : t -> string

    val name : t -> string

    val num_bookmarks : t -> float

    val num_bookmarks_by_attached_type : t -> Jsont.json

    val jsont : t Jsont.t
  end

  (** Get a single tag

      Get tag by its id *)
  val get_tags_by_tag_id : tag_id:string -> t -> unit -> T.t
end

module ListId : sig
  module T : sig
    type t = string

    val jsont : t Jsont.t

    val v : t -> t
  end
end

module List : sig
  module T : sig
    type t

    (** Construct a value *)
    val v : has_collaborators:bool -> icon:string -> id:string -> name:string -> public:bool -> user_role:string -> ?type_:string -> ?description:string option -> ?parent_id:string -> ?query:string option -> unit -> t

    val description : t -> string option option

    val has_collaborators : t -> bool

    val icon : t -> string

    val id : t -> string

    val name : t -> string

    val parent_id : t -> string option

    val public : t -> bool

    val query : t -> string option option

    val type_ : t -> string

    val user_role : t -> string

    val jsont : t Jsont.t
  end

  (** Create a new list

      Create a new list *)
  val post_lists : ?body:Jsont.json -> t -> unit -> T.t

  (** Get a single list

      Get list by its id *)
  val get_lists_by_list_id : list_id:string -> t -> unit -> T.t

  (** Update a list

      Update list by its id *)
  val patch_lists_by_list_id : list_id:string -> ?body:Jsont.json -> t -> unit -> T.t
end

module HighlightId : sig
  module T : sig
    type t = string

    val jsont : t Jsont.t

    val v : t -> t
  end
end

module Highlight : sig
  module T : sig
    type t

    (** Construct a value *)
    val v : bookmark_id:string -> created_at:string -> end_offset:float -> id:string -> start_offset:float -> user_id:string -> ?color:string -> ?note:string -> ?text:string -> unit -> t

    val bookmark_id : t -> string

    val color : t -> string

    val created_at : t -> string

    val end_offset : t -> float

    val id : t -> string

    val note : t -> string option

    val start_offset : t -> float

    val text : t -> string option

    val user_id : t -> string

    val jsont : t Jsont.t
  end

  (** Create a new highlight

      Create a new highlight *)
  val post_highlights : ?body:Jsont.json -> t -> unit -> T.t

  (** Get a single highlight

      Get highlight by its id *)
  val get_highlights_by_highlight_id : highlight_id:string -> t -> unit -> T.t

  (** Delete a highlight

      Delete highlight by its id *)
  val delete_highlights_by_highlight_id : highlight_id:string -> t -> unit -> T.t

  (** Update a highlight

      Update highlight by its id *)
  val patch_highlights_by_highlight_id : highlight_id:string -> ?body:Jsont.json -> t -> unit -> T.t
end

module PaginatedHighlights : sig
  module T : sig
    type t

    (** Construct a value *)
    val v : highlights:Highlight.T.t list -> ?next_cursor:string -> unit -> t

    val highlights : t -> Highlight.T.t list

    val next_cursor : t -> string option

    val jsont : t Jsont.t
  end

  (** Get all highlights

      Get all highlights *)
  val get_highlights : ?limit:string -> ?cursor:string -> t -> unit -> T.t
end

module FileToBeUploaded : sig
  module T : sig
    type t = Jsont.json

    val jsont : t Jsont.t

    val v : unit -> t
  end
end

module Cursor : sig
  module T : sig
    type t = string

    val jsont : t Jsont.t

    val v : t -> t
  end
end

module Client : sig
  (** Update user

      Update a user's role, bookmark quota, or storage quota. Admin access required. *)
  val put_admin_users_by_user_id : user_id:string -> ?body:Jsont.json -> t -> unit -> Jsont.json

  (** Get a single asset

      Get asset by its id *)
  val get_assets_by_asset_id : asset_id:string -> t -> unit -> unit

  (** Get all backups

      Get all backups *)
  val get_backups : t -> unit -> Jsont.json

  (** Trigger a new backup

      Trigger a new backup *)
  val post_backups : t -> unit -> Jsont.json

  (** Get a single backup

      Get backup by its id *)
  val get_backups_by_backup_id : backup_id:string -> t -> unit -> Jsont.json

  (** Delete a backup

      Delete backup by its id *)
  val delete_backups_by_backup_id : backup_id:string -> t -> unit -> unit

  (** Download a backup

      Download backup file *)
  val get_backups_by_backup_id_download : backup_id:string -> t -> unit -> string

  (** Delete a bookmark

      Delete bookmark by its id *)
  val delete_bookmarks_by_bookmark_id : bookmark_id:string -> t -> unit -> unit

  (** Update a bookmark

      Update bookmark by its id *)
  val patch_bookmarks_by_bookmark_id : bookmark_id:string -> ?body:Jsont.json -> t -> unit -> Jsont.json

  (** Attach asset

      Attach a new asset to a bookmark *)
  val post_bookmarks_by_bookmark_id_assets : bookmark_id:string -> ?body:Jsont.json -> t -> unit -> Jsont.json

  (** Replace asset

      Replace an existing asset with a new one *)
  val put_bookmarks_by_bookmark_id_assets_by_asset_id : bookmark_id:string -> asset_id:string -> ?body:Jsont.json -> t -> unit -> unit

  (** Detach asset

      Detach an asset from a bookmark *)
  val delete_bookmarks_by_bookmark_id_assets_by_asset_id : bookmark_id:string -> asset_id:string -> t -> unit -> unit

  (** Get highlights of a bookmark

      Get highlights of a bookmark *)
  val get_bookmarks_by_bookmark_id_highlights : bookmark_id:string -> t -> unit -> Jsont.json

  (** Get lists of a bookmark

      Get lists of a bookmark *)
  val get_bookmarks_by_bookmark_id_lists : bookmark_id:string -> t -> unit -> Jsont.json

  (** Summarize a bookmark

      Attaches a summary to the bookmark and returns the updated record. *)
  val post_bookmarks_by_bookmark_id_summarize : bookmark_id:string -> t -> unit -> Jsont.json

  (** Attach tags to a bookmark

      Attach tags to a bookmark *)
  val post_bookmarks_by_bookmark_id_tags : bookmark_id:string -> ?body:Jsont.json -> t -> unit -> Jsont.json

  (** Detach tags from a bookmark

      Detach tags from a bookmark *)
  val delete_bookmarks_by_bookmark_id_tags : bookmark_id:string -> ?body:Jsont.json -> t -> unit -> Jsont.json

  (** Get all lists

      Get all lists *)
  val get_lists : t -> unit -> Jsont.json

  (** Delete a list

      Delete list by its id *)
  val delete_lists_by_list_id : list_id:string -> t -> unit -> unit

  (** Add a bookmark to a list

      Add the bookmarks to a list *)
  val put_lists_by_list_id_bookmarks_by_bookmark_id : list_id:string -> bookmark_id:string -> t -> unit -> unit

  (** Remove a bookmark from a list

      Remove the bookmarks from a list *)
  val delete_lists_by_list_id_bookmarks_by_bookmark_id : list_id:string -> bookmark_id:string -> t -> unit -> unit

  (** Get all tags

      Get all tags *)
  val get_tags : ?name_contains:string -> ?sort:string -> ?attached_by:string -> ?cursor:string -> ?limit:string -> t -> unit -> Jsont.json

  (** Create a new tag

      Create a new tag *)
  val post_tags : ?body:Jsont.json -> t -> unit -> Jsont.json

  (** Delete a tag

      Delete tag by its id *)
  val delete_tags_by_tag_id : tag_id:string -> t -> unit -> unit

  (** Update a tag

      Update tag by its id *)
  val patch_tags_by_tag_id : tag_id:string -> ?body:Jsont.json -> t -> unit -> Jsont.json

  (** Get current user info

      Returns info about the current user *)
  val get_users_me : t -> unit -> Jsont.json

  (** Get current user stats

      Returns stats about the current user *)
  val get_users_me_stats : t -> unit -> Jsont.json
end

module BookmarkId : sig
  module T : sig
    type t = string

    val jsont : t Jsont.t

    val v : t -> t
  end
end

module Bookmark : sig
  module T : sig
    type t

    (** Construct a value *)
    val v : archived:bool -> assets:Jsont.json list -> content:Jsont.json -> created_at:string -> favourited:bool -> id:string -> tags:Jsont.json list -> user_id:string -> ?modified_at:string -> ?note:string option -> ?source:string option -> ?summarization_status:string -> ?summary:string option -> ?tagging_status:string -> ?title:string option -> unit -> t

    val archived : t -> bool

    val assets : t -> Jsont.json list

    val content : t -> Jsont.json

    val created_at : t -> string

    val favourited : t -> bool

    val id : t -> string

    val modified_at : t -> string option

    val note : t -> string option option

    val source : t -> string option option

    val summarization_status : t -> string option

    val summary : t -> string option option

    val tagging_status : t -> string option

    val tags : t -> Jsont.json list

    val title : t -> string option option

    val user_id : t -> string

    val jsont : t Jsont.t
  end

  (** Create a new bookmark

      Create a new bookmark *)
  val post_bookmarks : ?body:Jsont.json -> t -> unit -> T.t

  (** Get a single bookmark

      Get bookmark by its id
      @param include_content If set to true, bookmark's content will be included in the response. Note, this content can be large for some bookmarks.
  *)
  val get_bookmarks_by_bookmark_id : bookmark_id:string -> ?include_content:string -> t -> unit -> T.t
end

module PaginatedBookmarks : sig
  module T : sig
    type t

    (** Construct a value *)
    val v : bookmarks:Bookmark.T.t list -> ?next_cursor:string -> unit -> t

    val bookmarks : t -> Bookmark.T.t list

    val next_cursor : t -> string option

    val jsont : t Jsont.t
  end

  (** Get all bookmarks

      Get all bookmarks
      @param include_content If set to true, bookmark's content will be included in the response. Note, this content can be large for some bookmarks.
  *)
  val get_bookmarks : ?archived:string -> ?favourited:string -> ?sort_order:string -> ?limit:string -> ?cursor:string -> ?include_content:string -> t -> unit -> T.t

  (** Search bookmarks

      Search bookmarks
      @param include_content If set to true, bookmark's content will be included in the response. Note, this content can be large for some bookmarks.
  *)
  val get_bookmarks_search : q:string -> ?sort_order:string -> ?limit:string -> ?cursor:string -> ?include_content:string -> t -> unit -> T.t

  (** Get bookmarks in the list

      Get bookmarks in the list
      @param include_content If set to true, bookmark's content will be included in the response. Note, this content can be large for some bookmarks.
  *)
  val get_lists_by_list_id_bookmarks : list_id:string -> ?sort_order:string -> ?limit:string -> ?cursor:string -> ?include_content:string -> t -> unit -> T.t

  (** Get bookmarks with the tag

      Get bookmarks with the tag
      @param include_content If set to true, bookmark's content will be included in the response. Note, this content can be large for some bookmarks.
  *)
  val get_tags_by_tag_id_bookmarks : tag_id:string -> ?sort_order:string -> ?limit:string -> ?cursor:string -> ?include_content:string -> t -> unit -> T.t
end

module BackupId : sig
  module T : sig
    type t = string

    val jsont : t Jsont.t

    val v : t -> t
  end
end

module AssetId : sig
  module T : sig
    type t = string

    val jsont : t Jsont.t

    val v : t -> t
  end
end

module Asset : sig
  module T : sig
    type t

    (** Construct a value *)
    val v : asset_id:string -> content_type:string -> file_name:string -> size:float -> unit -> t

    val asset_id : t -> string

    val content_type : t -> string

    val file_name : t -> string

    val size : t -> float

    val jsont : t Jsont.t
  end

  (** Upload a new asset

      Upload a new asset *)
  val post_assets : ?body:Fetch.Form.part list -> t -> unit -> T.t
end
