(** Immutable pagination and lookup snapshots. *)

@@ portable

type feed_item = {
  contact : Sortal_schema.Contact.t;
  entry : Sortal_feed.Entry.t;
  mentions : Bushel.Entry.entry list;
}

type link_group = {
  ent : Bushel.Entry.entry;
  links : Bushel.Link_graph.external_link list;
}

type timeline_item = Feed_item of feed_item * (int * int * int)

type month_section = {
  year : int;
  month : int;
  collaborators : Sortal_schema.Contact.t list;
  items : timeline_item list;
}

type contact_index =
  (string * Sortal_schema.Contact.t) list Bushel.Smap.t

type t : immutable_data

val create :
  entries:Bushel.Entry.t -> feed_items:feed_item list -> t @@ nonportable
(** [create ~entries ~feed_items] is a snapshot of their pagination groups
    and contact and student indexes. *)

val links : t -> link_group iarray
(** [links t] is the external-link groups, newest first. *)

val months : t -> month_section iarray
(** [months t] is the network months, newest first. *)

val contact_domains : t -> contact_index
(** [contact_domains t] is the contact URL index of [t]. *)

val student_ideas : t -> (string * string) list Bushel.Smap.t
(** [student_ideas t] maps student handles to idea slugs and titles. *)

val build_contact_by_domain :
  Sortal_schema.Contact.t list -> contact_index @@ nonportable
(** [build_contact_by_domain contacts] indexes their personal URL prefixes. *)

val find_contact_for_url :
  contact_index -> string -> string -> Sortal_schema.Contact.t option
(** [find_contact_for_url index host path] is the contact whose URL has the
    longest matching prefix in [index]. *)

val length : 'a iarray -> int
(** [length items] is the number of [items]. *)

val to_list : 'a iarray -> 'a list
(** [to_list items] is [items] in order. *)

val page : 'a iarray -> offset:int -> limit:int -> 'a list
(** [page items ~offset ~limit] is at most [limit] items from [offset].
    Negative offsets are clamped to zero. Nonpositive limits return no
    items. Only the selected items are traversed. *)
