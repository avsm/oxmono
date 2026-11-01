(** Organization channel folders.

    Channel folders group channels for display. Administrative operations are
    subject to the permissions and feature level of the connected Zulip server.
*)

type t = {
  id : Zulip.Id.Channel_folder.t;
  name : string;
  order : int option;
  date_created : float option;
  creator_id : Zulip.Id.User.t option;
  description : string;
  rendered_description : string;
  is_archived : bool;
  extensions : Jsont.json;
}
(** The type for channel folders. [order] is the zero-based display position
    when supplied by the server. [date_created] is a Unix timestamp in UTC
    seconds. [creator_id] and [date_created] are absent when Zulip has no
    recorded creation metadata. [rendered_description] contains HTML.
    [extensions] preserves unrecognized response members for future server
    fields. *)

val jsont : t Jsont.t
(** [jsont] is the JSON codec for channel folders. Missing descriptions decode
    as empty strings. Missing archive state decodes as [false]. Missing nullable
    creation metadata decodes as [None]. Unrecognized members are retained in
    [extensions]. *)

val raw : t -> Jsont.json
(** [raw folder] is [folder] encoded as JSON, including [folder.extensions].

    @raise Jsont.exception-Error if the value cannot be encoded. *)

val create :
  Client.t ->
  name:string ->
  ?description:string ->
  unit ->
  (Zulip.Id.Channel_folder.t, Error.t) result
(** [create client ~name ~description ()] creates a channel folder named [name]
    and is its typed identifier. [description] defaults to the empty string.
    Server, transport, and response-decoding failures are returned as [Error].
*)

val list :
  Client.t -> ?include_archived:bool -> unit -> (t list, Error.t) result
(** [list client ~include_archived ()] is the server-ordered list of channel
    folders visible to [client]. [include_archived] is omitted by default, for
    which Zulip excludes archived folders. Server, transport, and
    response-decoding failures are returned as [Error]. *)

val reorder :
  Client.t -> order:Zulip.Id.Channel_folder.t list -> (unit, Error.t) result
(** [reorder client ~order] sets the organization display order to [order].
    [order] must contain every channel-folder identifier, including archived
    folders. Encoding, server, and transport failures are returned as [Error].
*)

val update :
  Client.t ->
  folder_id:Zulip.Id.Channel_folder.t ->
  ?name:string ->
  ?description:string ->
  ?is_archived:bool ->
  unit ->
  (unit, Error.t) result
(** [update client ~folder_id ~name ~description ~is_archived ()] applies the
    supplied changes to [folder_id]. Each optional change is omitted by default.
    It returns [Error.Invalid_request] without making a request when no change
    is supplied. Server and transport failures are returned as [Error]. *)

val set_archived :
  Client.t ->
  folder_id:Zulip.Id.Channel_folder.t ->
  archived:bool ->
  (unit, Error.t) result
(** [set_archived client ~folder_id ~archived] sets the archive state of
    [folder_id] to [archived]. Server and transport failures are returned as
    [Error]. *)
