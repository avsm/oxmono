(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Schema.org JSON-LD generation. *)

@@ portable

val website_jsonld :
  base_url:string -> site_name:string -> description:string -> string
(** [website_jsonld ~base_url ~site_name ~description] is a [WebSite] object. *)

val person_jsonld : ctx:Arod_ctx.t -> string
(** [person_jsonld ~ctx] is the author's [Person] object. *)

val article_jsonld :
  base_url:string -> url:string -> title:string -> description:string ->
  author_name:string -> date:(int * int * int) ->
  ?modified:(int * int * int) -> ?image:string -> ?tags:string list ->
  unit -> string
(** [article_jsonld ... ()] is an [Article] object. *)

val scholarly_article_jsonld :
  base_url:string -> url:string -> title:string -> description:string ->
  authors:string list -> date:(int * int * int) ->
  ?doi:string -> ?image:string -> ?journal:string -> ?tags:string list ->
  unit -> string
(** [scholarly_article_jsonld ... ()] is a [ScholarlyArticle] object. *)

val project_jsonld :
  base_url:string -> url:string -> title:string -> description:string ->
  date_start:int -> ?date_end:int -> ?tags:string list ->
  unit -> string
(** [project_jsonld ... ()] is a [SoftwareSourceCode] object. *)

val video_jsonld :
  base_url:string -> url:string -> title:string -> description:string ->
  datetime:Ptime.t -> ?image:string ->
  embed_url:string -> ?is_talk:bool -> unit -> string
(** [video_jsonld ... ()] is a [VideoObject] object. *)

val profile_page_jsonld : ctx:Arod_ctx.t -> string
(** [profile_page_jsonld ~ctx] is the author's [ProfilePage] object. *)

val collection_page_jsonld :
  base_url:string -> url:string -> title:string -> description:string ->
  count:int -> unit -> string
(** [collection_page_jsonld ... ()] is a [CollectionPage] object. *)

val breadcrumb_jsonld :
  base_url:string -> (string * string) list -> string
(** [breadcrumb_jsonld ~base_url crumbs] is a [BreadcrumbList] object. *)
