(** Typed results shared by the command line and agent adapters. *)

type coverage = { indexed : int; total : int }

type hit = {
  id : string;
  revision : string;
  kind : string;
  title : string;
  url : string;
  date : string;
  passage : int;
  snippet : string;
  pixel_path : string option;
  source_path : string option;
}

type source = {
  id : string;
  revision : string;
  title : string;
  kind : string;
  url : string;
  text : string;
  next_offset : int option;
  offset : int option;
  width : int option;
  height : int option;
  media_type : string option;
  pixel_path : string option;
  source_path : string option;
}

type candidate = {
  id : string;
  handle : string;
  name : string;
  matched_on : string;
  emails : string list;
}

type 'a page = {
  generation : string;
  items : 'a list;
  next_offset : int option;
}

type search = {
  page : hit page;
  mode : string option;
  coverage : coverage option;
  source : string option;
  order : string option;
  as_of : string option;
  source_ids : string list option;
}

type t =
  | Search of search
  | Source of source
  | Contacts of { page : candidate page; status : string }
  | Contact of { id : string; contact : Sortal_schema.Contact.t }
  | Context of { page : Bushel_knowledge.edge page; source_ids : string list }

type provider =
  | Http of { url : string; model : string; dimensions : int }
  | Mlx of {
      backend : string;
      model_dir : string;
      dimensions : int;
      max_tokens : int;
      query_instruction : string;
      model_identity : string;
    }

type status = {
  database : string;
  generation : string;
  documents : int;
  passages : int;
  provider : provider option;
  coverage : coverage option;
  skipped_images : int option;
  newly_indexed : int option;
}

val json : t -> Jsont.json
val encode : t -> string
val codec : t Jsont.t
val status_json : status -> Jsont.json
val status_codec : status Jsont.t
