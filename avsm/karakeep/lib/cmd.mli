(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** {1 Logging} *)

val setup_logging_simple : Fmt.style_renderer option -> Logs.level option -> unit
val setup_logging : (Fmt.style_renderer option * Logs.level option) Cmdliner.Term.t

(** {1 Common Terms} *)

val profile_arg : string option Cmdliner.Term.t
val limit_term : string option Cmdliner.Term.t
val cursor_term : string option Cmdliner.Term.t
val archived_term : string option Cmdliner.Term.t
val favourited_term : string option Cmdliner.Term.t
val include_content_term : string option Cmdliner.Term.t
val bookmark_id_term : string Cmdliner.Term.t
val tag_id_term : string Cmdliner.Term.t
val list_id_term : string Cmdliner.Term.t
val highlight_id_term : string Cmdliner.Term.t
val url_term : string Cmdliner.Term.t
val title_term : string option Cmdliner.Term.t
val note_term : string option Cmdliner.Term.t
val summary_term : string option Cmdliner.Term.t
val tags_term : string list Cmdliner.Term.t
val name_term : string Cmdliner.Term.t
val name_opt_term : string option Cmdliner.Term.t
val icon_term : string Cmdliner.Term.t
val icon_opt_term : string option Cmdliner.Term.t
val description_term : string option Cmdliner.Term.t
val parent_id_term : string option Cmdliner.Term.t
val query_term : string option Cmdliner.Term.t
val search_query_term : string Cmdliner.Term.t
val color_term : string option Cmdliner.Term.t

(** {1 Output} *)

type output_format = Text | Json | Quiet
val output_format_term : output_format Cmdliner.Term.t

val bookmark_title : Karakeep.Bookmark.T.t -> string
val print_bookmark : output_format -> Karakeep.Bookmark.T.t -> unit
val print_bookmarks : output_format -> Karakeep.Bookmark.T.t list -> unit
val print_tag : output_format -> Karakeep.Tag.T.t -> unit
val print_tags : output_format -> Karakeep.Tag.T.t list -> unit
val print_list : output_format -> Karakeep.List.T.t -> unit
val print_lists : output_format -> Karakeep.List.T.t list -> unit
val print_highlight : output_format -> Karakeep.Highlight.T.t -> unit
val print_highlights : output_format -> Karakeep.Highlight.T.t list -> unit
val print_user : output_format -> Jsont.json -> unit
val print_stats : output_format -> Jsont.json -> unit

(** {1 Session} *)

val with_client :
  ?profile:string ->
  (Client.t -> unit) ->
  < clock : _ Eio.Time.clock
  ; fs : Eio.Fs.dir_ty Eio.Path.t
  ; net : _ Eio.Net.t
  ; .. > ->
  unit

(** {1 Auth Commands} *)

val auth_cmd :
  < clock : _ Eio.Time.clock
  ; fs : Eio.Fs.dir_ty Eio.Path.t
  ; net : _ Eio.Net.t
  ; .. > ->
  unit Cmdliner.Cmd.t
