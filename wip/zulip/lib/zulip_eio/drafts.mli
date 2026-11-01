(** Server-synchronized message drafts.

    Drafts belong to the user authenticated by the client. Changes are
    synchronized to the user's other clients through Zulip draft events. *)

module Id = Zulip.Id.Draft
(** Draft identifiers. *)

type destination =
  | Unaddressed
  | Channel of Zulip.Id.Channel.t
  | Direct of Zulip.Id.User.t list
      (** The type for a draft's tentative destination. *)

type new_ = {
  destination : destination;
  topic : string;
      (** The tentative channel topic. Zulip ignores it for unaddressed and
          direct-message drafts. *)
  content : string;  (** The draft content in Zulip-flavored Markdown. *)
  timestamp : int option;
      (** The last edit time as Unix seconds, or [None] for a server-assigned
          creation time. *)
}
(** The type for draft creation and replacement values. *)

type t = {
  id : Id.t;
  destination : destination;
  topic : string;
      (** The tentative channel topic. It has no meaning for unaddressed and
          direct-message drafts. *)
  content : string;  (** The draft content in Zulip-flavored Markdown. *)
  timestamp : int option;
      (** The last edit time as Unix seconds, if present. *)
  raw : Jsont.json;
      (** The complete draft object, including unrecognized fields. *)
}
(** The type for stored drafts. *)

type page = {
  count : int;  (** The number of values in [drafts]. *)
  drafts : t list;  (** Drafts in descending order of last edit time. *)
  raw : Jsont.json;
      (** The complete response object, including unrecognized fields. *)
}
(** The type for draft-list responses. *)

val jsont : t Jsont.t
(** [jsont] is the JSON codec for stored drafts. *)

val page_jsont : page Jsont.t
(** [page_jsont] is the JSON codec for draft-list responses. *)

val list : Client.t -> (page, Error.t) result
(** [list client] is every draft belonging to the user authenticated by
    [client]. *)

type create_result = {
  ids : Id.t list;
      (** The new draft identifiers in the same order as the submitted drafts.
      *)
  raw : Jsont.json;
      (** The complete response object, including unrecognized fields. *)
}
(** The type for detailed draft-creation responses. *)

val create_detailed : Client.t -> new_ list -> (create_result, Error.t) result
(** [create_detailed client drafts] is the detailed response after storing
    [drafts]. A channel destination always encodes exactly one channel. *)

val create : Client.t -> new_ list -> (Id.t list, Error.t) result
(** [create client drafts] is the list of identifiers assigned to [drafts], in
    submission order. *)

val edit : Client.t -> draft_id:Id.t -> new_ -> (unit, Error.t) result
(** [edit client ~draft_id draft] replaces the stored draft identified by
    [draft_id] with [draft]. Zulip rejects an identifier that does not belong to
    the authenticated user. *)

val delete : Client.t -> draft_id:Id.t -> (unit, Error.t) result
(** [delete client ~draft_id] deletes the stored draft identified by [draft_id].
    Zulip rejects an identifier that does not belong to the authenticated user.
*)
