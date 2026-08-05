(** ActivityPub/ActivityStreams protocol types with jsont codecs.

    This module provides OCaml types and bidirectional JSON codecs for the
    ActivityPub and ActivityStreams 2.0 specifications. It is the wire format
    layer of the {!Apubt} library.

    {1 Example}

    {[
      (* Decode an actor from JSON *)
      let json_str = {|{"@id": "...", "@type": "Person", ...}|} in
      match Jsont_bytesrw.decode_string Actor.jsont json_str with
      | Ok actor -> Printf.printf "Actor: %s\n" (Option.value ~default:"" (Actor.name actor))
      | Error e -> Printf.eprintf "Error: %a\n" Jsont.Error.pp e

      (* Create and encode a Note *)
      let note = Object.make ~type_:Object_type.Note
          ~content:"Hello ActivityPub!" () in
      match Jsont_bytesrw.encode_string Object.jsont note with
      | Ok json_str -> print_endline json_str
      | Error e -> Printf.eprintf "Error: %a\n" Jsont.Error.pp e
    ]}

    @see <https://www.w3.org/TR/activitypub/> ActivityPub specification
    @see <https://www.w3.org/TR/activitystreams-core/> ActivityStreams Core
    @see <https://www.w3.org/TR/activitystreams-vocabulary/> ActivityStreams Vocabulary *)

(** {1 Common Types} *)

(** Timestamps in ISO 8601 format. *)
module Datetime : sig
  type t

  val v : string -> t
  (** [v s] creates a datetime from the string [s]. *)

  val to_string : t -> string
  (** [to_string t] returns the datetime as an ISO 8601 string. *)

  val jsont : t Jsont.t
  (** JSON type for datetimes. *)
end

val uri_jsont : Uri.t Jsont.t
(** JSON codec for [Uri.t] values. *)

(** JSON-LD context. *)
module Context : sig
  type t

  val default : t
  (** The default ActivityStreams context. *)

  val jsont : t Jsont.t
  (** JSON type for contexts. *)
end

(** {1 Link} *)

(** Link objects represent references to other resources.
    @see <https://www.w3.org/TR/activitystreams-vocabulary/#dfn-link> *)
module Link : sig
  type t

  val make :
    ?media_type:string ->
    ?name:string ->
    ?hreflang:string ->
    ?height:int ->
    ?width:int ->
    ?preview:Uri.t ->
    href:Uri.t ->
    unit -> t
  (** Create a new Link. *)

  val href : t -> Uri.t
  val media_type : t -> string option
  val name : t -> string option
  val hreflang : t -> string option
  val height : t -> int option
  val width : t -> int option
  val preview : t -> Uri.t option

  val jsont : t Jsont.t
  (** JSON type for Links. *)
end

(** Reference that can be either a URI string or a Link object. *)
module Link_or_uri : sig
  type t =
    | Uri of Uri.t
    | Link of Link.t

  val uri : Uri.t -> t
  val link : Link.t -> t
  val jsont : t Jsont.t
end

(** {1 Image} *)

(** Image objects.
    @see <https://www.w3.org/TR/activitystreams-vocabulary/#dfn-image> *)
module Image : sig
  type t

  val make :
    ?id:Uri.t ->
    ?name:string ->
    ?media_type:string ->
    ?width:int ->
    ?height:int ->
    url:Link_or_uri.t ->
    unit -> t

  val id : t -> Uri.t option
  val url : t -> Link_or_uri.t
  val name : t -> string option
  val media_type : t -> string option
  val width : t -> int option
  val height : t -> int option

  val jsont : t Jsont.t
end

(** Image reference - can be URI, Link, or full Image object. *)
module Image_ref : sig
  type t =
    | Uri of Uri.t
    | Link of Link.t
    | Image of Image.t

  val uri : Uri.t -> t
  val link : Link.t -> t
  val image : Image.t -> t
  val jsont : t Jsont.t
end

(** {1 Public Collection} *)

(** Special public addressing collection. *)
module Public : sig
  val id : Uri.t
  (** The public collection URI for addressing public posts. *)
end

(** {1 Recipient} *)

(** Recipient reference - can be URI or inline object with id and type. *)
module Recipient : sig
  type t

  val make : ?type_:string -> Uri.t -> t
  val id : t -> Uri.t
  val type_ : t -> string option
  val jsont : t Jsont.t
end

(** {1 Endpoints} *)

(** Actor endpoints for special server URLs. *)
module Endpoints : sig
  type t

  val make :
    ?proxy_url:Uri.t ->
    ?oauth_authorization_endpoint:Uri.t ->
    ?oauth_token_endpoint:Uri.t ->
    ?provide_client_key:Uri.t ->
    ?sign_client_key:Uri.t ->
    ?shared_inbox:Uri.t ->
    unit -> t

  val proxy_url : t -> Uri.t option
  val oauth_authorization_endpoint : t -> Uri.t option
  val oauth_token_endpoint : t -> Uri.t option
  val provide_client_key : t -> Uri.t option
  val sign_client_key : t -> Uri.t option
  val shared_inbox : t -> Uri.t option

  val jsont : t Jsont.t
end

(** {1 Public Key} *)

(** Public key for HTTP Signatures. *)
module Public_key : sig
  type t

  val make :
    id:Uri.t ->
    owner:Uri.t ->
    public_key_pem:string ->
    unit -> t

  val id : t -> Uri.t
  val owner : t -> Uri.t
  val public_key_pem : t -> string

  val jsont : t Jsont.t
end

(** {1 Actor Types} *)

(** Actor types enumeration. *)
module Actor_type : sig
  type t =
    | Person
    | Service
    | Organization
    | Group
    | Application

  val to_string : t -> string
  val of_string : string -> t option
  val jsont : t Jsont.t
end

(** {1 Actor} *)

(** Actor objects represent entities that can perform activities.
    @see <https://www.w3.org/TR/activitypub/#actor-objects> *)
module Actor : sig
  type t

  val make :
    ?context:Context.t ->
    id:Uri.t ->
    type_:Actor_type.t ->
    ?name:string ->
    ?preferred_username:string ->
    ?summary:string ->
    ?url:Uri.t ->
    inbox:Uri.t ->
    outbox:Uri.t ->
    ?followers:Uri.t ->
    ?following:Uri.t ->
    ?liked:Uri.t ->
    ?streams:Uri.t list ->
    ?endpoints:Endpoints.t ->
    ?public_key:Public_key.t ->
    ?icon:Image_ref.t list ->
    ?image:Image_ref.t list ->
    ?manually_approves_followers:bool ->
    ?also_known_as:Uri.t list ->
    ?discoverable:bool ->
    ?suspended:bool ->
    ?moved_to:Uri.t ->
    ?featured:Uri.t ->
    ?featured_tags:Uri.t ->
    unit -> t
  (** Create a new Actor. *)

  val context : t -> Context.t option
  val id : t -> Uri.t
  val type_ : t -> Actor_type.t
  val name : t -> string option
  val preferred_username : t -> string option
  val summary : t -> string option
  val url : t -> Uri.t option
  val inbox : t -> Uri.t
  val outbox : t -> Uri.t
  val followers : t -> Uri.t option
  val following : t -> Uri.t option
  val liked : t -> Uri.t option
  val streams : t -> Uri.t list option
  val endpoints : t -> Endpoints.t option
  val public_key : t -> Public_key.t option
  val icon : t -> Image_ref.t list option
  val image : t -> Image_ref.t list option
  val manually_approves_followers : t -> bool option
  val also_known_as : t -> Uri.t list option
  val discoverable : t -> bool option
  val suspended : t -> bool option
  val moved_to : t -> Uri.t option
  val featured : t -> Uri.t option
  val featured_tags : t -> Uri.t option

  val jsont : t Jsont.t
  (** JSON type for Actors. *)
end

(** Actor reference - can be URI or full Actor object. *)
module Actor_ref : sig
  type t =
    | Uri of Uri.t
    | Actor of Actor.t

  val uri : Uri.t -> t
  val actor : Actor.t -> t
  val jsont : t Jsont.t
end

(** {1 Object Types} *)

(** Object types enumeration. *)
module Object_type : sig
  type t =
    | Note
    | Article
    | Page
    | Event
    | Image
    | Video
    | Audio
    | Document
    | Place
    | Profile
    | Tombstone
    | Collection
    | OrderedCollection

  val to_string : t -> string
  val of_string : string -> t option
  val jsont : t Jsont.t
end

(** {1 Object} *)

(** ActivityStreams objects.
    @see <https://www.w3.org/TR/activitystreams-vocabulary/#object-types> *)
module Object : sig
  type t

  val make :
    ?context:Context.t ->
    ?id:Uri.t ->
    type_:Object_type.t ->
    ?name:string ->
    ?summary:string ->
    ?content:string ->
    ?media_type:string ->
    ?url:Link_or_uri.t list ->
    ?attributed_to:Actor_ref.t ->
    ?in_reply_to:Uri.t ->
    ?published:Datetime.t ->
    ?updated:Datetime.t ->
    ?deleted:Datetime.t ->
    ?to_:Recipient.t list ->
    ?cc:Recipient.t list ->
    ?bto:Recipient.t list ->
    ?bcc:Recipient.t list ->
    ?replies:Uri.t ->
    ?attachment:Link_or_uri.t list ->
    ?tag:Link_or_uri.t list ->
    ?generator:Uri.t ->
    ?icon:Image_ref.t list ->
    ?image:Image_ref.t list ->
    ?start_time:Datetime.t ->
    ?end_time:Datetime.t ->
    ?duration:string ->
    ?sensitive:bool ->
    ?conversation:Uri.t ->
    ?audience:Recipient.t list ->
    ?location:Link_or_uri.t ->
    ?preview:Link_or_uri.t ->
    unit -> t
  (** Create a new Object. *)

  val context : t -> Context.t option
  val id : t -> Uri.t option
  val type_ : t -> Object_type.t
  val name : t -> string option
  val summary : t -> string option
  val content : t -> string option
  val media_type : t -> string option
  val url : t -> Link_or_uri.t list option
  val attributed_to : t -> Actor_ref.t option
  val in_reply_to : t -> Uri.t option
  val published : t -> Datetime.t option
  val updated : t -> Datetime.t option
  val deleted : t -> Datetime.t option
  val to_ : t -> Recipient.t list option
  val cc : t -> Recipient.t list option
  val bto : t -> Recipient.t list option
  val bcc : t -> Recipient.t list option
  val replies : t -> Uri.t option
  val attachment : t -> Link_or_uri.t list option
  val tag : t -> Link_or_uri.t list option
  val generator : t -> Uri.t option
  val icon : t -> Image_ref.t list option
  val image : t -> Image_ref.t list option
  val start_time : t -> Datetime.t option
  val end_time : t -> Datetime.t option
  val duration : t -> string option
  val sensitive : t -> bool option
  val conversation : t -> Uri.t option
  val audience : t -> Recipient.t list option

  val location : t -> Link_or_uri.t option
  (** [location t] returns the physical or logical location associated with the object. *)

  val preview : t -> Link_or_uri.t option
  (** [preview t] returns a preview of the object, typically a smaller version. *)

  val jsont : t Jsont.t
  (** JSON type for Objects. *)
end

(** Object reference - can be URI or full Object. *)
module Object_ref : sig
  type t =
    | Uri of Uri.t
    | Object of Object.t

  val uri : Uri.t -> t
  val obj : Object.t -> t
  val jsont : t Jsont.t
end

(** {1 Activity Types} *)

(** Activity types enumeration. *)
module Activity_type : sig
  type t =
    | Create
    | Update
    | Delete
    | Follow
    | Accept
    | Reject
    | Add
    | Remove
    | Like
    | Announce
    | Undo
    | Block
    | Flag
    | Dislike
    | Ignore
    | Invite
    | Join
    | Leave
    | Listen
    | Move
    | Offer
    | Question
    | Read
    | TentativeAccept
    | TentativeReject
    | Travel
    | View

  val to_string : t -> string
  val of_string : string -> t option
  val jsont : t Jsont.t
end

(** {1 Activity} *)

(** ActivityPub activities.
    @see <https://www.w3.org/TR/activitystreams-vocabulary/#activity-types> *)
module Activity : sig
  type t

  val make :
    ?context:Context.t ->
    ?id:Uri.t ->
    type_:Activity_type.t ->
    actor:Actor_ref.t ->
    ?object_:Object_ref.t ->
    ?target:Object_ref.t ->
    ?result:Object_ref.t ->
    ?origin:Object_ref.t ->
    ?instrument:Object_ref.t ->
    ?to_:Recipient.t list ->
    ?cc:Recipient.t list ->
    ?bto:Recipient.t list ->
    ?bcc:Recipient.t list ->
    ?published:Datetime.t ->
    ?updated:Datetime.t ->
    ?summary:string ->
    ?one_of:Object_ref.t list ->
    ?any_of:Object_ref.t list ->
    ?closed:Datetime.t ->
    unit -> t
  (** Create a new Activity.

      The [one_of], [any_of], and [closed] fields are only used for Question
      activities (polls). Use [one_of] for single-choice polls and [any_of]
      for multiple-choice polls. *)

  val context : t -> Context.t option
  val id : t -> Uri.t option
  val type_ : t -> Activity_type.t
  val actor : t -> Actor_ref.t
  val object_ : t -> Object_ref.t option
  val target : t -> Object_ref.t option
  val result : t -> Object_ref.t option
  val origin : t -> Object_ref.t option
  val instrument : t -> Object_ref.t option
  val to_ : t -> Recipient.t list option
  val cc : t -> Recipient.t list option
  val bto : t -> Recipient.t list option
  val bcc : t -> Recipient.t list option
  val published : t -> Datetime.t option
  val updated : t -> Datetime.t option
  val summary : t -> string option

  val one_of : t -> Object_ref.t list option
  (** [one_of t] returns single-choice poll options for Question activities. *)

  val any_of : t -> Object_ref.t list option
  (** [any_of t] returns multiple-choice poll options for Question activities. *)

  val closed : t -> Datetime.t option
  (** [closed t] returns when the poll was closed, for Question activities. *)

  val jsont : t Jsont.t
  (** JSON type for Activities. *)
end

(** Activity reference - can be URI or full Activity. *)
module Activity_ref : sig
  type t =
    | Uri of Uri.t
    | Activity of Activity.t

  val uri : Uri.t -> t
  val activity : Activity.t -> t
  val jsont : t Jsont.t
end

(** {1 Collection} *)

(** Collection objects.
    @see <https://www.w3.org/TR/activitystreams-vocabulary/#dfn-collection> *)
module Collection : sig
  type 'a t

  val make :
    ?context:Context.t ->
    ?id:Uri.t ->
    ?total_items:int ->
    ?current:Uri.t ->
    ?first:Uri.t ->
    ?last:Uri.t ->
    ?items:'a list ->
    ordered:bool ->
    unit -> 'a t
  (** Create a new Collection. Use [~ordered:true] for OrderedCollection. *)

  val context : 'a t -> Context.t option
  val id : 'a t -> Uri.t option
  val total_items : 'a t -> int option
  val current : 'a t -> Uri.t option
  val first : 'a t -> Uri.t option
  val last : 'a t -> Uri.t option
  val items : 'a t -> 'a list option
  val ordered : 'a t -> bool

  val jsont : 'a Jsont.t -> 'a t Jsont.t
  (** JSON type for Collections, parameterized by item type. *)
end

(** {1 Collection Page} *)

(** Collection page objects.
    @see <https://www.w3.org/TR/activitystreams-vocabulary/#dfn-collectionpage> *)
module Collection_page : sig
  type 'a t

  val make :
    ?context:Context.t ->
    ?id:Uri.t ->
    ?total_items:int ->
    ?current:Uri.t ->
    ?first:Uri.t ->
    ?last:Uri.t ->
    ?prev:Uri.t ->
    ?next:Uri.t ->
    ?part_of:Uri.t ->
    ?items:'a list ->
    ordered:bool ->
    unit -> 'a t
  (** Create a new CollectionPage. Use [~ordered:true] for OrderedCollectionPage. *)

  val context : 'a t -> Context.t option
  val id : 'a t -> Uri.t option
  val total_items : 'a t -> int option
  val current : 'a t -> Uri.t option
  val first : 'a t -> Uri.t option
  val last : 'a t -> Uri.t option
  val prev : 'a t -> Uri.t option
  val next : 'a t -> Uri.t option
  val part_of : 'a t -> Uri.t option
  val items : 'a t -> 'a list option
  val ordered : 'a t -> bool

  val jsont : 'a Jsont.t -> 'a t Jsont.t
  (** JSON type for CollectionPages, parameterized by item type. *)
end

(** {1 Convenience type aliases} *)

(** Activity collection. *)
module Activity_collection : sig
  type t = Activity.t Collection.t
  val jsont : t Jsont.t
end

(** Object collection. *)
module Object_collection : sig
  type t = Object.t Collection.t
  val jsont : t Jsont.t
end

(** Activity collection page. *)
module Activity_collection_page : sig
  type t = Activity.t Collection_page.t
  val jsont : t Jsont.t
end

(** Object collection page. *)
module Object_collection_page : sig
  type t = Object.t Collection_page.t
  val jsont : t Jsont.t
end

(** {1 Webfinger} *)

(** Webfinger JRD (JSON Resource Descriptor) for actor discovery.
    @see <https://www.rfc-editor.org/rfc/rfc7033> Webfinger RFC *)
module Webfinger : sig
  (** A link in the Webfinger response. *)
  module Jrd_link : sig
    type t

    val make :
      rel:string ->
      ?type_:string ->
      ?href:Uri.t ->
      ?template:string ->
      unit -> t

    val rel : t -> string
    val type_ : t -> string option
    val href : t -> Uri.t option
    val template : t -> string option

    val jsont : t Jsont.t
  end

  type t

  val make :
    subject:string ->
    ?aliases:string list ->
    ?properties:(string * string) list ->
    ?links:Jrd_link.t list ->
    unit -> t

  val subject : t -> string
  val aliases : t -> string list option
  val properties : t -> (string * string) list option
  val links : t -> Jrd_link.t list option

  val jsont : t Jsont.t
end

(** {1 NodeInfo} *)

(** NodeInfo protocol for server metadata discovery.
    @see <https://nodeinfo.diaspora.software/> NodeInfo specification *)
module Nodeinfo : sig
  (** Software information. *)
  module Software : sig
    type t

    val make :
      name:string ->
      version:string ->
      ?repository:Uri.t ->
      ?homepage:Uri.t ->
      unit -> t

    val name : t -> string
    val version : t -> string
    val repository : t -> Uri.t option
    val homepage : t -> Uri.t option

    val jsont : t Jsont.t
  end

  (** Usage statistics. *)
  module Usage : sig
    type t

    val make :
      ?users_total:int ->
      ?users_active_half_year:int ->
      ?users_active_month:int ->
      ?local_posts:int ->
      ?local_comments:int ->
      unit -> t

    val users_total : t -> int option
    val users_active_half_year : t -> int option
    val users_active_month : t -> int option
    val local_posts : t -> int option
    val local_comments : t -> int option

    val jsont : t Jsont.t
  end

  type t

  val make :
    version:string ->
    software:Software.t ->
    protocols:string list ->
    usage:Usage.t ->
    open_registrations:bool ->
    ?metadata:Jsont.json ->
    unit -> t

  val version : t -> string
  val software : t -> Software.t
  val protocols : t -> string list
  val usage : t -> Usage.t
  val open_registrations : t -> bool
  val metadata : t -> Jsont.json option

  val jsont : t Jsont.t
end
