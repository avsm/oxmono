(** Bounded OPML subscription-list decoding. No network requests are made. *)

type feed = {
  title : string;
  xml_url : string;
  html_url : string option;
  groups : string list;
}

type t = { title : string option; feeds : feed list }

val decode :
  ?max_bytes:int ->
  ?max_depth:int ->
  ?max_feeds:int ->
  string ->
  (t, string) result
(** [decode source] reads OPML 1.0, 1.1 or 2.0, flattening nested outlines in
    document order and retaining their group labels. Feed URLs must be absolute
    HTTP(S) URLs without credentials. Duplicate feed URLs keep the first entry.
    DTDs and custom entities are rejected. Defaults limit input to 2 MiB,
    nesting to 32 elements and feed outlines to 10000, including duplicates.
    Non-feed outlines are groups and are never dereferenced. *)
