(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Schema.org JSON-LD structured data generation. *)

val website_jsonld :
  base_url:string -> site_name:string -> description:string -> string

val person_jsonld : ctx:Arod_ctx.t -> string

val article_jsonld :
  base_url:string -> url:string -> title:string -> description:string ->
  author_name:string -> date:(int * int * int) ->
  ?modified:(int * int * int) -> ?image:string -> ?tags:string list ->
  unit -> string

val scholarly_article_jsonld :
  base_url:string -> url:string -> title:string -> description:string ->
  authors:string list -> date:(int * int * int) ->
  ?doi:string -> ?image:string -> ?journal:string -> ?tags:string list ->
  unit -> string

val project_jsonld :
  base_url:string -> url:string -> title:string -> description:string ->
  date_start:int -> ?date_end:int -> ?tags:string list ->
  unit -> string

val video_jsonld :
  base_url:string -> url:string -> title:string -> description:string ->
  datetime:Ptime.t -> ?image:string ->
  embed_url:string -> ?is_talk:bool -> unit -> string

val profile_page_jsonld : ctx:Arod_ctx.t -> string

val collection_page_jsonld :
  base_url:string -> url:string -> title:string -> description:string ->
  count:int -> unit -> string

val breadcrumb_jsonld :
  base_url:string -> (string * string) list -> string
