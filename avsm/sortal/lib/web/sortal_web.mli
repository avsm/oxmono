(** The sortal web UI as a proffer site.

    The site is data. It holds no store, no clock and no filesystem: a route
    handler is portable, so everything it needs arrives in the {!env} record
    the caller builds. {!compiled} is a compatibility alias for the site that
    a backend serves and that
    [proffer.mock] drives in a test.

    Pages are plain HTML and one embedded stylesheet. There is no JavaScript
    and no external asset, so the UI works over a loopback socket with nothing
    else installed. *)

type env = {
  list_contacts : unit -> Sortal_schema.Contact.t list;
      (** Every contact in the store, in any order. The index sorts. *)
  lookup : string -> Sortal_schema.Contact.t option;
      (** The contact with this handle. [None] answers 404. *)
  search : string -> Sortal_schema.Contact.t list;
      (** Contacts matching a free-text query, for the index search box. *)
  save : Sortal_schema.Contact.t -> (unit, string) result;
      (** Write a contact, creating it when the handle is new. The error is
          shown to the user, so it should read as a sentence. *)
  delete : string -> (unit, string) result;
  thumbnail : string -> string option;
      (** The PNG bytes for a handle, or [None] when there is no image. It is
          called once per row of the index, so it should be cheap. *)
}
(** The capabilities a handler needs. Handlers receive it as an argument, which
    the mode system does not constrain, so the closures may capture whatever
    the caller's domain owns. *)

val site : env Proffer.Site.t
(** [site] is the route table, with a styled 404 as its fallback. *)

val compiled : env Proffer.Site.t
(** [compiled] is a compatibility alias for {!site}. *)
