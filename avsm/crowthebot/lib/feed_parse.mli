(** Bounded document decoding. No network, files or database capabilities. *)
type document =
  | Opml of string * string list
  | Feed of string * string * Feed_store.entry list

val decode : url:string -> string -> (document, string) result
(** [decode ~url body] decodes RSS 1.0/2.0, Atom or OPML. [url] resolves
    relative article links. It rejects DTDs, entities, excessive depth and
    oversized lists. Article bodies, enclosures and OPML includes are not
    fetched. *)
