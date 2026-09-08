(** Reusable personal message content.

    A saved snippet belongs to the user authenticated by the client. Its content
    is stored as Zulip-flavored Markdown. *)

module Id = Zulip.Id.Saved_snippet
(** Saved-snippet identifiers. *)

type t
(** The type for saved snippets. *)

val id : t -> Id.t
(** [id snippet] is the identifier of [snippet]. *)

val title : t -> string
(** [title snippet] is the title of [snippet]. *)

val content : t -> string
(** [content snippet] is the Zulip-flavored Markdown content of [snippet]. *)

val date_created : t -> int
(** [date_created snippet] is the creation time of [snippet] as UTC Unix
    seconds. *)

val raw : t -> Jsont.json
(** [raw snippet] is the complete JSON object for [snippet], including
    unrecognized fields. *)

val jsont : t Jsont.t
(** [jsont] is the JSON codec for saved snippets. *)

type page = {
  saved_snippets : t list;
  raw : Jsont.json;
      (** The complete response object, including unrecognized fields. *)
}
(** The type for saved-snippet list responses. *)

val page_jsont : page Jsont.t
(** [page_jsont] is the JSON codec for saved-snippet list responses. *)

val list : Client.t -> (page, Error.t) result
(** [list client] is every saved snippet belonging to the user authenticated by
    [client]. *)

type create_result = {
  id : Id.t;
  raw : Jsont.json;
      (** The complete response object, including unrecognized fields. *)
}
(** The type for detailed saved-snippet creation responses. *)

val create_detailed :
  Client.t -> title:string -> content:string -> (create_result, Error.t) result
(** [create_detailed client ~title ~content] is the detailed response after
    creating a saved snippet named [title]. [content] is Zulip-flavored
    Markdown. Zulip rejects an empty title or content. *)

val create :
  Client.t -> title:string -> content:string -> (Id.t, Error.t) result
(** [create client ~title ~content] is the identifier of a new saved snippet
    named [title]. [content] is Zulip-flavored Markdown. Zulip rejects an empty
    title or content. *)

val edit :
  Client.t ->
  saved_snippet_id:Id.t ->
  ?title:string ->
  ?content:string ->
  unit ->
  (unit, Error.t) result
(** [edit client ~saved_snippet_id ~title ~content ()] applies the supplied
    fields to the saved snippet identified by [saved_snippet_id]. Omitting
    [title] retains the existing title. Omitting [content] retains the existing
    Zulip-flavored Markdown content. Zulip rejects an identifier that does not
    belong to the authenticated user and rejects empty supplied values. *)

val delete : Client.t -> saved_snippet_id:Id.t -> (unit, Error.t) result
(** [delete client ~saved_snippet_id] deletes the saved snippet identified by
    [saved_snippet_id]. Zulip rejects an identifier that does not belong to the
    authenticated user. *)
