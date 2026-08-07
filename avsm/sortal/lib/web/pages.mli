(** The rendered pages, one function per route that returns a document.

    Each takes the data it shows rather than the environment, so nothing here
    reaches a store. Every function returns a whole document, already wrapped
    by {!Html.page}. *)

val index :
  query:string ->
  has_thumb:(string -> bool) ->
  Sortal_schema.Contact.t list ->
  string
  @@ portable
(** [index ~query ~has_thumb contacts] lists [contacts] in the order given.
    [query] is the search text that produced them, empty for the whole store.
    [has_thumb handle] says whether a thumbnail image is worth linking. *)

val new_form :
  ?error:string ->
  handle:string ->
  name:string ->
  kind:string ->
  email:string ->
  unit ->
  string
  @@ portable
(** [new_form ~handle ~name ~kind ~email ()] is the create form, prefilled with
    what the user last submitted so a rejected post loses nothing. [kind] is
    the raw form value, ["person"] or ["org"]. *)

val detail : has_thumb:bool -> Sortal_schema.Contact.t -> string @@ portable
(** [detail ~has_thumb c] shows every field of [c], with temporal ranges and
    notes where they are set. *)

val edit : Sortal_schema.Contact.t -> string @@ portable
(** [edit c] is the scalar form for [c] followed by one editor per collection,
    each with a remove button per row and an inline add form. *)

val not_found : unit -> string @@ portable
(** [not_found ()] is the body of every 404 the site answers. *)

val error : string -> string @@ portable
(** [error msg] reports a failed store operation. [msg] is what the store
    said. *)
