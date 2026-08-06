(** ActivityPub/ActivityStreams types with jsont codecs.

    This module provides OCaml types and bidirectional JSON codecs for the
    ActivityPub and ActivityStreams 2.0 specifications.

    @see <https://www.w3.org/TR/activitypub/> ActivityPub specification
    @see <https://www.w3.org/TR/activitystreams-core/> ActivityStreams Core
    @see <https://www.w3.org/TR/activitystreams-vocabulary/> ActivityStreams Vocabulary *)

(** {1 Common Types} *)

(** Timestamps in ISO 8601 format. *)
module Datetime : sig
  type t
  val v : string -> t
  val to_string : t -> string
  val jsont : t Jsont.t
end = struct
  type t = string
  let v s = s
  let to_string t = t
  let jsont = Jsont.string |> Jsont.with_doc ~kind:"datetime"
end

(** JSON codec for [Uri.t] values. *)
let uri_jsont : Uri.t Jsont.t =
  Jsont.string |> Jsont.map ~dec:Uri.of_string ~enc:Uri.to_string

(** JSON-LD context. *)
module Context : sig
  type t
  val default : t
  val jsont : t Jsont.t
end = struct
  type t = Jsont.json
  let default =
    Jsont.Json.string "https://www.w3.org/ns/activitystreams"
  let jsont = Jsont.json |> Jsont.with_doc ~kind:"@context"
end

(** Helper: JSON type that accepts either a single item or an array, normalizing to a list.
    On encoding, always outputs an array for consistency. *)
let one_or_many (item_jsont : 'a Jsont.t) : 'a list Jsont.t =
  let dec_array = Jsont.list item_jsont in
  let dec_single = Jsont.map item_jsont
      ~dec:(fun x -> [x])
      ~enc:(fun _ -> assert false) (* never used for encoding *)
  in
  Jsont.any ~kind:"one or many"
    ~dec_array
    ~dec_string:dec_single
    ~dec_object:dec_single
    ~enc:(fun _ -> dec_array) (* always encode as array *)
    ()

(** Helper: Nullable value - accepts null as None, value as Some value *)
let nullable (jsont : 'a Jsont.t) : 'a option Jsont.t =
  let dec_null = Jsont.null None in
  let dec_value = Jsont.map jsont
      ~dec:(fun v -> Some v)
      ~enc:(function Some v -> v | None -> assert false)
  in
  Jsont.any ~kind:"nullable"
    ~dec_null
    ~dec_string:dec_value
    ~dec_number:dec_value
    ~dec_bool:dec_value
    ~dec_array:dec_value
    ~dec_object:dec_value
    ~enc:(function
        | None -> dec_null
        | Some _ -> dec_value)
    ()

(** Helper: URI that can also be an object with an id field.
    This handles ActivityPub fields like 'replies' that can be either
    a URI string or an inline Collection object. *)
let uri_or_object_with_id : Uri.t Jsont.t =
  let id_jsont =
    Jsont.Object.map ~kind:"Object with id" (fun id -> id)
    |> Jsont.Object.mem "id" uri_jsont ~enc:Fun.id
    |> Jsont.Object.skip_unknown
    |> Jsont.Object.finish
  in
  Jsont.any ~kind:"URI or object"
    ~dec_string:uri_jsont
    ~dec_object:id_jsont
    ~enc:(fun _ -> uri_jsont)
    ()


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

  val href : t -> Uri.t
  val media_type : t -> string option
  val name : t -> string option
  val hreflang : t -> string option
  val height : t -> int option
  val width : t -> int option
  val preview : t -> Uri.t option

  val jsont : t Jsont.t
end = struct
  type t = {
    href : Uri.t;
    media_type : string option;
    name : string option;
    hreflang : string option;
    height : int option;
    width : int option;
    preview : Uri.t option;
  }

  let make ?media_type ?name ?hreflang ?height ?width ?preview ~href () =
    { href; media_type; name; hreflang; height; width; preview }

  let href t = t.href
  let media_type t = t.media_type
  let name t = t.name
  let hreflang t = t.hreflang
  let height t = t.height
  let width t = t.width
  let preview t = t.preview

  let jsont =
    Jsont.Object.map ~kind:"Link"
      (fun href media_type name hreflang height width preview ->
         { href; media_type; name; hreflang; height; width; preview })
    |> Jsont.Object.mem "href" uri_jsont ~enc:href
    |> Jsont.Object.opt_mem "mediaType" Jsont.string ~enc:media_type
    |> Jsont.Object.opt_mem "name" Jsont.string ~enc:name
    |> Jsont.Object.opt_mem "hreflang" Jsont.string ~enc:hreflang
    |> Jsont.Object.opt_mem "height" Jsont.int ~enc:height
    |> Jsont.Object.opt_mem "width" Jsont.int ~enc:width
    |> Jsont.Object.opt_mem "preview" uri_jsont ~enc:preview
    |> Jsont.Object.finish
end

(** Reference that can be either a URI string or a Link object. *)
module Link_or_uri : sig
  type t =
    | Uri of Uri.t
    | Link of Link.t

  val uri : Uri.t -> t
  val link : Link.t -> t
  val jsont : t Jsont.t
end = struct
  type t =
    | Uri of Uri.t
    | Link of Link.t

  let uri u = Uri u
  let link l = Link l

  let jsont =
    let dec_string = Jsont.map uri_jsont ~dec:(fun u -> Uri u)
        ~enc:(function Uri u -> u | Link _ -> assert false) in
    let dec_object = Jsont.map Link.jsont ~dec:(fun l -> Link l)
        ~enc:(function Link l -> l | Uri _ -> assert false) in
    Jsont.any ~kind:"Link or URI"
      ~dec_string ~dec_object
      ~enc:(function
          | Uri _ -> dec_string
          | Link _ -> dec_object)
      ()
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
end = struct
  type t = {
    id : Uri.t option;
    url : Link_or_uri.t;
    name : string option;
    media_type : string option;
    width : int option;
    height : int option;
  }

  let make ?id ?name ?media_type ?width ?height ~url () =
    { id; url; name; media_type; width; height }

  let id t = t.id
  let url t = t.url
  let name t = t.name
  let media_type t = t.media_type
  let width t = t.width
  let height t = t.height

  let jsont =
    Jsont.Object.map ~kind:"Image"
      (fun id url name media_type width height ->
         { id; url; name; media_type; width; height })
    |> Jsont.Object.opt_mem "id" uri_jsont ~enc:id
    |> Jsont.Object.mem "url" Link_or_uri.jsont ~enc:url
    |> Jsont.Object.opt_mem "name" Jsont.string ~enc:name
    |> Jsont.Object.opt_mem "mediaType" Jsont.string ~enc:media_type
    |> Jsont.Object.opt_mem "width" Jsont.int ~enc:width
    |> Jsont.Object.opt_mem "height" Jsont.int ~enc:height
    |> Jsont.Object.finish
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
end = struct
  type t =
    | Uri of Uri.t
    | Link of Link.t
    | Image of Image.t

  let uri u = Uri u
  let link l = Link l
  let image i = Image i

  let jsont =
    (* For string case: URI *)
    let dec_string = Jsont.map uri_jsont ~dec:(fun u -> Uri u)
        ~enc:(function Uri u -> u | _ -> assert false) in
    (* For object case: either Link or Image *)
    let dec_object =
      (* Default: decode as Image if we can't determine type *)
      Jsont.map Image.jsont
        ~dec:(fun i -> Image i)
        ~enc:(function Image i -> i | _ -> assert false)
    in
    Jsont.any ~kind:"Image reference"
      ~dec_string ~dec_object
      ~enc:(function
          | Uri _ -> dec_string
          | Link _ | Image _ -> dec_object)
      ()
end

(** {1 Public Collection} *)

(** Special public addressing collection. *)
module Public : sig
  val id : Uri.t
end = struct
  let id = Uri.of_string "https://www.w3.org/ns/activitystreams#Public"
end

(** {1 Recipient} *)

(** Recipient reference - can be URI or inline object with id and type. *)
module Recipient : sig
  type t = {
    id : Uri.t;
    type_ : string option;
  }

  val make : ?type_:string -> Uri.t -> t
  val id : t -> Uri.t
  val type_ : t -> string option
  val jsont : t Jsont.t
end = struct
  type t = {
    id : Uri.t;
    type_ : string option;
  }

  let make ?type_ id = { id; type_ }
  let id t = t.id
  let type_ t = t.type_

  let jsont =
    let dec_string = Jsont.map uri_jsont
        ~dec:(fun u -> { id = u; type_ = None })
        ~enc:(fun t -> t.id) in
    let dec_object =
      Jsont.Object.map ~kind:"Recipient"
        (fun id type_ -> { id; type_ })
      |> Jsont.Object.mem "id" uri_jsont ~enc:id
      |> Jsont.Object.opt_mem "type" Jsont.string ~enc:type_
      |> Jsont.Object.finish
    in
    Jsont.any ~kind:"Recipient"
      ~dec_string ~dec_object
      ~enc:(fun t ->
          match t.type_ with
          | None -> dec_string
          | Some _ -> dec_object)
      ()
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
end = struct
  type t = {
    proxy_url : Uri.t option;
    oauth_authorization_endpoint : Uri.t option;
    oauth_token_endpoint : Uri.t option;
    provide_client_key : Uri.t option;
    sign_client_key : Uri.t option;
    shared_inbox : Uri.t option;
  }

  let make ?proxy_url ?oauth_authorization_endpoint ?oauth_token_endpoint
      ?provide_client_key ?sign_client_key ?shared_inbox () =
    { proxy_url; oauth_authorization_endpoint; oauth_token_endpoint;
      provide_client_key; sign_client_key; shared_inbox }

  let proxy_url t = t.proxy_url
  let oauth_authorization_endpoint t = t.oauth_authorization_endpoint
  let oauth_token_endpoint t = t.oauth_token_endpoint
  let provide_client_key t = t.provide_client_key
  let sign_client_key t = t.sign_client_key
  let shared_inbox t = t.shared_inbox

  let jsont =
    Jsont.Object.map ~kind:"Endpoints"
      (fun proxy_url oauth_authorization_endpoint oauth_token_endpoint
        provide_client_key sign_client_key shared_inbox ->
        { proxy_url; oauth_authorization_endpoint; oauth_token_endpoint;
          provide_client_key; sign_client_key; shared_inbox })
    |> Jsont.Object.opt_mem "proxyUrl" uri_jsont ~enc:proxy_url
    |> Jsont.Object.opt_mem "oauthAuthorizationEndpoint" uri_jsont
        ~enc:oauth_authorization_endpoint
    |> Jsont.Object.opt_mem "oauthTokenEndpoint" uri_jsont
        ~enc:oauth_token_endpoint
    |> Jsont.Object.opt_mem "provideClientKey" uri_jsont ~enc:provide_client_key
    |> Jsont.Object.opt_mem "signClientKey" uri_jsont ~enc:sign_client_key
    |> Jsont.Object.opt_mem "sharedInbox" uri_jsont ~enc:shared_inbox
    |> Jsont.Object.finish
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
end = struct
  type t = {
    id : Uri.t;
    owner : Uri.t;
    public_key_pem : string;
  }

  let make ~id ~owner ~public_key_pem () =
    { id; owner; public_key_pem }

  let id t = t.id
  let owner t = t.owner
  let public_key_pem t = t.public_key_pem

  let jsont =
    Jsont.Object.map ~kind:"PublicKey"
      (fun id owner public_key_pem -> { id; owner; public_key_pem })
    |> Jsont.Object.mem "id" uri_jsont ~enc:id
    |> Jsont.Object.mem "owner" uri_jsont ~enc:owner
    |> Jsont.Object.mem "publicKeyPem" Jsont.string ~enc:public_key_pem
    |> Jsont.Object.finish
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
end = struct
  type t =
    | Person
    | Service
    | Organization
    | Group
    | Application

  let to_string = function
    | Person -> "Person"
    | Service -> "Service"
    | Organization -> "Organization"
    | Group -> "Group"
    | Application -> "Application"

  let of_string = function
    | "Person" -> Some Person
    | "Service" -> Some Service
    | "Organization" -> Some Organization
    | "Group" -> Some Group
    | "Application" -> Some Application
    | _ -> None

  let jsont =
    Jsont.enum ~kind:"ActorType" [
      "Person", Person;
      "Service", Service;
      "Organization", Organization;
      "Group", Group;
      "Application", Application;
    ]
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
end = struct
  type t = {
    context : Context.t option;
    id : Uri.t;
    type_ : Actor_type.t;
    name : string option;
    preferred_username : string option;
    summary : string option;
    url : Uri.t option;
    inbox : Uri.t;
    outbox : Uri.t;
    followers : Uri.t option;
    following : Uri.t option;
    liked : Uri.t option;
    streams : Uri.t list option;
    endpoints : Endpoints.t option;
    public_key : Public_key.t option;
    icon : Image_ref.t list option;
    image : Image_ref.t list option;
    manually_approves_followers : bool option;
    also_known_as : Uri.t list option;
    discoverable : bool option;
    suspended : bool option;
    moved_to : Uri.t option;
    featured : Uri.t option;
    featured_tags : Uri.t option;
  }

  let make ?context ~id ~type_ ?name ?preferred_username ?summary ?url
      ~inbox ~outbox ?followers ?following ?liked ?streams ?endpoints
      ?public_key ?icon ?image ?manually_approves_followers
      ?also_known_as ?discoverable ?suspended ?moved_to ?featured
      ?featured_tags () =
    { context; id; type_; name; preferred_username; summary; url;
      inbox; outbox; followers; following; liked; streams; endpoints;
      public_key; icon; image; manually_approves_followers;
      also_known_as; discoverable; suspended; moved_to; featured;
      featured_tags }

  let context t = t.context
  let id t = t.id
  let type_ t = t.type_
  let name t = t.name
  let preferred_username t = t.preferred_username
  let summary t = t.summary
  let url t = t.url
  let inbox t = t.inbox
  let outbox t = t.outbox
  let followers t = t.followers
  let following t = t.following
  let liked t = t.liked
  let streams t = t.streams
  let endpoints t = t.endpoints
  let public_key t = t.public_key
  let icon t = t.icon
  let image t = t.image
  let manually_approves_followers t = t.manually_approves_followers
  let also_known_as t = t.also_known_as
  let discoverable t = t.discoverable
  let suspended t = t.suspended
  let moved_to t = t.moved_to
  let featured t = t.featured
  let featured_tags t = t.featured_tags

  let jsont =
    Jsont.Object.map ~kind:"Actor"
      (fun context id type_ name preferred_username summary url inbox outbox
        followers following liked streams endpoints public_key icon image
        manually_approves_followers also_known_as discoverable suspended
        moved_to featured featured_tags ->
        { context; id; type_; name; preferred_username; summary; url;
          inbox; outbox; followers; following; liked; streams; endpoints;
          public_key; icon; image; manually_approves_followers;
          also_known_as; discoverable; suspended; moved_to; featured;
          featured_tags })
    |> Jsont.Object.opt_mem "@context" Context.jsont ~enc:context
    |> Jsont.Object.mem "id" uri_jsont ~enc:id
    |> Jsont.Object.mem "type" Actor_type.jsont ~enc:type_
    |> Jsont.Object.opt_mem "name" Jsont.string ~enc:name
    |> Jsont.Object.opt_mem "preferredUsername" Jsont.string
        ~enc:preferred_username
    |> Jsont.Object.opt_mem "summary" Jsont.string ~enc:summary
    |> Jsont.Object.opt_mem "url" uri_jsont ~enc:url
    |> Jsont.Object.mem "inbox" uri_jsont ~enc:inbox
    |> Jsont.Object.mem "outbox" uri_jsont ~enc:outbox
    |> Jsont.Object.opt_mem "followers" uri_jsont ~enc:followers
    |> Jsont.Object.opt_mem "following" uri_jsont ~enc:following
    |> Jsont.Object.opt_mem "liked" uri_jsont ~enc:liked
    |> Jsont.Object.opt_mem "streams" (Jsont.list uri_jsont) ~enc:streams
    |> Jsont.Object.opt_mem "endpoints" Endpoints.jsont ~enc:endpoints
    |> Jsont.Object.opt_mem "publicKey" Public_key.jsont ~enc:public_key
    |> Jsont.Object.opt_mem "icon" (one_or_many Image_ref.jsont) ~enc:icon
    |> Jsont.Object.opt_mem "image" (one_or_many Image_ref.jsont) ~enc:image
    |> Jsont.Object.opt_mem "manuallyApprovesFollowers" Jsont.bool
        ~enc:manually_approves_followers
    |> Jsont.Object.opt_mem "alsoKnownAs" (one_or_many uri_jsont)
        ~enc:also_known_as
    |> Jsont.Object.opt_mem "discoverable" Jsont.bool ~enc:discoverable
    |> Jsont.Object.opt_mem "suspended" Jsont.bool ~enc:suspended
    |> Jsont.Object.opt_mem "movedTo" uri_jsont ~enc:moved_to
    |> Jsont.Object.opt_mem "featured" uri_jsont ~enc:featured
    |> Jsont.Object.opt_mem "featuredTags" uri_jsont ~enc:featured_tags
    |> Jsont.Object.finish
end

(** Actor reference - can be URI or full Actor object. *)
module Actor_ref : sig
  type t =
    | Uri of Uri.t
    | Actor of Actor.t

  val uri : Uri.t -> t
  val actor : Actor.t -> t
  val jsont : t Jsont.t
end = struct
  type t =
    | Uri of Uri.t
    | Actor of Actor.t

  let uri u = Uri u
  let actor a = Actor a

  let jsont =
    let dec_string = Jsont.map uri_jsont
        ~dec:(fun u -> Uri u)
        ~enc:(function Uri u -> u | Actor _ -> assert false) in
    let dec_object = Jsont.map Actor.jsont
        ~dec:(fun a -> Actor a)
        ~enc:(function Actor a -> a | Uri _ -> assert false) in
    Jsont.any ~kind:"Actor reference"
      ~dec_string ~dec_object
      ~enc:(function
          | Uri _ -> dec_string
          | Actor _ -> dec_object)
      ()
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
end = struct
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

  let to_string = function
    | Note -> "Note"
    | Article -> "Article"
    | Page -> "Page"
    | Event -> "Event"
    | Image -> "Image"
    | Video -> "Video"
    | Audio -> "Audio"
    | Document -> "Document"
    | Place -> "Place"
    | Profile -> "Profile"
    | Tombstone -> "Tombstone"
    | Collection -> "Collection"
    | OrderedCollection -> "OrderedCollection"

  let of_string = function
    | "Note" -> Some Note
    | "Article" -> Some Article
    | "Page" -> Some Page
    | "Event" -> Some Event
    | "Image" -> Some Image
    | "Video" -> Some Video
    | "Audio" -> Some Audio
    | "Document" -> Some Document
    | "Place" -> Some Place
    | "Profile" -> Some Profile
    | "Tombstone" -> Some Tombstone
    | "Collection" -> Some Collection
    | "OrderedCollection" -> Some OrderedCollection
    | _ -> None

  let jsont =
    Jsont.enum ~kind:"ObjectType" [
      "Note", Note;
      "Article", Article;
      "Page", Page;
      "Event", Event;
      "Image", Image;
      "Video", Video;
      "Audio", Audio;
      "Document", Document;
      "Place", Place;
      "Profile", Profile;
      "Tombstone", Tombstone;
      "Collection", Collection;
      "OrderedCollection", OrderedCollection;
    ]
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
end = struct
  type t = {
    context : Context.t option;
    id : Uri.t option;
    type_ : Object_type.t;
    name : string option;
    summary : string option;
    content : string option;
    media_type : string option;
    url : Link_or_uri.t list option;
    attributed_to : Actor_ref.t option;
    in_reply_to : Uri.t option;
    published : Datetime.t option;
    updated : Datetime.t option;
    deleted : Datetime.t option;
    to_ : Recipient.t list option;
    cc : Recipient.t list option;
    bto : Recipient.t list option;
    bcc : Recipient.t list option;
    replies : Uri.t option;
    attachment : Link_or_uri.t list option;
    tag : Link_or_uri.t list option;
    generator : Uri.t option;
    icon : Image_ref.t list option;
    image : Image_ref.t list option;
    start_time : Datetime.t option;
    end_time : Datetime.t option;
    duration : string option;
    sensitive : bool option;
    conversation : Uri.t option;
    audience : Recipient.t list option;
    location : Link_or_uri.t option;
    preview : Link_or_uri.t option;
  }

  let make ?context ?id ~type_ ?name ?summary ?content ?media_type ?url
      ?attributed_to ?in_reply_to ?published ?updated ?deleted ?to_ ?cc
      ?bto ?bcc ?replies ?attachment ?tag ?generator ?icon ?image
      ?start_time ?end_time ?duration ?sensitive ?conversation ?audience
      ?location ?preview () =
    { context; id; type_; name; summary; content; media_type; url;
      attributed_to; in_reply_to; published; updated; deleted;
      to_; cc; bto; bcc; replies; attachment; tag; generator;
      icon; image; start_time; end_time; duration; sensitive;
      conversation; audience; location; preview }

  let context t = t.context
  let id t = t.id
  let type_ t = t.type_
  let name t = t.name
  let summary t = t.summary
  let content t = t.content
  let media_type t = t.media_type
  let url t = t.url
  let attributed_to t = t.attributed_to
  let in_reply_to t = t.in_reply_to
  let published t = t.published
  let updated t = t.updated
  let deleted t = t.deleted
  let to_ t = t.to_
  let cc t = t.cc
  let bto t = t.bto
  let bcc t = t.bcc
  let replies t = t.replies
  let attachment t = t.attachment
  let tag t = t.tag
  let generator t = t.generator
  let icon t = t.icon
  let image t = t.image
  let start_time t = t.start_time
  let end_time t = t.end_time
  let duration t = t.duration
  let sensitive t = t.sensitive
  let conversation t = t.conversation
  let audience t = t.audience
  let location t = t.location
  let preview t = t.preview

  let jsont =
    Jsont.Object.map ~kind:"Object"
      (fun context id type_ name summary content media_type url attributed_to
        in_reply_to published updated deleted to_ cc bto bcc replies
        attachment tag generator icon image start_time end_time duration
        sensitive conversation audience location preview ->
        { context; id; type_; name; summary; content; media_type; url;
          attributed_to; in_reply_to; published; updated; deleted;
          to_; cc; bto; bcc; replies; attachment; tag; generator;
          icon; image; start_time; end_time; duration; sensitive;
          conversation; audience; location; preview })
    |> Jsont.Object.opt_mem "@context" Context.jsont ~enc:context
    |> Jsont.Object.opt_mem "id" uri_jsont ~enc:id
    |> Jsont.Object.mem "type" Object_type.jsont ~enc:type_
    |> Jsont.Object.opt_mem "name" Jsont.string ~enc:name
    |> Jsont.Object.mem "summary" (nullable Jsont.string)
        ~dec_absent:None ~enc_omit:Option.is_none ~enc:summary
    |> Jsont.Object.mem "content" (nullable Jsont.string)
        ~dec_absent:None ~enc_omit:Option.is_none ~enc:content
    |> Jsont.Object.opt_mem "mediaType" Jsont.string ~enc:media_type
    |> Jsont.Object.opt_mem "url" (one_or_many Link_or_uri.jsont) ~enc:url
    |> Jsont.Object.opt_mem "attributedTo" Actor_ref.jsont ~enc:attributed_to
    |> Jsont.Object.mem "inReplyTo" (nullable uri_jsont)
        ~dec_absent:None ~enc_omit:Option.is_none ~enc:in_reply_to
    |> Jsont.Object.opt_mem "published" Datetime.jsont ~enc:published
    |> Jsont.Object.opt_mem "updated" Datetime.jsont ~enc:updated
    |> Jsont.Object.opt_mem "deleted" Datetime.jsont ~enc:deleted
    |> Jsont.Object.opt_mem "to" (Jsont.list Recipient.jsont) ~enc:to_
    |> Jsont.Object.opt_mem "cc" (Jsont.list Recipient.jsont) ~enc:cc
    |> Jsont.Object.opt_mem "bto" (Jsont.list Recipient.jsont) ~enc:bto
    |> Jsont.Object.opt_mem "bcc" (Jsont.list Recipient.jsont) ~enc:bcc
    |> Jsont.Object.opt_mem "replies" uri_or_object_with_id ~enc:replies
    |> Jsont.Object.opt_mem "attachment" (Jsont.list Link_or_uri.jsont)
        ~enc:attachment
    |> Jsont.Object.opt_mem "tag" (Jsont.list Link_or_uri.jsont) ~enc:tag
    |> Jsont.Object.opt_mem "generator" uri_jsont ~enc:generator
    |> Jsont.Object.opt_mem "icon" (one_or_many Image_ref.jsont) ~enc:icon
    |> Jsont.Object.opt_mem "image" (one_or_many Image_ref.jsont) ~enc:image
    |> Jsont.Object.opt_mem "startTime" Datetime.jsont ~enc:start_time
    |> Jsont.Object.opt_mem "endTime" Datetime.jsont ~enc:end_time
    |> Jsont.Object.opt_mem "duration" Jsont.string ~enc:duration
    |> Jsont.Object.opt_mem "sensitive" Jsont.bool ~enc:sensitive
    |> Jsont.Object.opt_mem "conversation" uri_jsont ~enc:conversation
    |> Jsont.Object.opt_mem "audience" (one_or_many Recipient.jsont) ~enc:audience
    |> Jsont.Object.opt_mem "location" Link_or_uri.jsont ~enc:location
    |> Jsont.Object.opt_mem "preview" Link_or_uri.jsont ~enc:preview
    |> Jsont.Object.finish
end

(** Object reference - can be URI or full Object. *)
module Object_ref : sig
  type t =
    | Uri of Uri.t
    | Object of Object.t

  val uri : Uri.t -> t
  val obj : Object.t -> t
  val jsont : t Jsont.t
end = struct
  type t =
    | Uri of Uri.t
    | Object of Object.t

  let uri u = Uri u
  let obj o = Object o

  let jsont =
    let dec_string = Jsont.map uri_jsont
        ~dec:(fun u -> Uri u)
        ~enc:(function Uri u -> u | Object _ -> assert false) in
    let dec_object = Jsont.map Object.jsont
        ~dec:(fun o -> Object o)
        ~enc:(function Object o -> o | Uri _ -> assert false) in
    Jsont.any ~kind:"Object reference"
      ~dec_string ~dec_object
      ~enc:(function
          | Uri _ -> dec_string
          | Object _ -> dec_object)
      ()
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
end = struct
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

  let to_string = function
    | Create -> "Create"
    | Update -> "Update"
    | Delete -> "Delete"
    | Follow -> "Follow"
    | Accept -> "Accept"
    | Reject -> "Reject"
    | Add -> "Add"
    | Remove -> "Remove"
    | Like -> "Like"
    | Announce -> "Announce"
    | Undo -> "Undo"
    | Block -> "Block"
    | Flag -> "Flag"
    | Dislike -> "Dislike"
    | Ignore -> "Ignore"
    | Invite -> "Invite"
    | Join -> "Join"
    | Leave -> "Leave"
    | Listen -> "Listen"
    | Move -> "Move"
    | Offer -> "Offer"
    | Question -> "Question"
    | Read -> "Read"
    | TentativeAccept -> "TentativeAccept"
    | TentativeReject -> "TentativeReject"
    | Travel -> "Travel"
    | View -> "View"

  let of_string = function
    | "Create" -> Some Create
    | "Update" -> Some Update
    | "Delete" -> Some Delete
    | "Follow" -> Some Follow
    | "Accept" -> Some Accept
    | "Reject" -> Some Reject
    | "Add" -> Some Add
    | "Remove" -> Some Remove
    | "Like" -> Some Like
    | "Announce" -> Some Announce
    | "Undo" -> Some Undo
    | "Block" -> Some Block
    | "Flag" -> Some Flag
    | "Dislike" -> Some Dislike
    | "Ignore" -> Some Ignore
    | "Invite" -> Some Invite
    | "Join" -> Some Join
    | "Leave" -> Some Leave
    | "Listen" -> Some Listen
    | "Move" -> Some Move
    | "Offer" -> Some Offer
    | "Question" -> Some Question
    | "Read" -> Some Read
    | "TentativeAccept" -> Some TentativeAccept
    | "TentativeReject" -> Some TentativeReject
    | "Travel" -> Some Travel
    | "View" -> Some View
    | _ -> None

  let jsont =
    Jsont.enum ~kind:"ActivityType" [
      "Create", Create;
      "Update", Update;
      "Delete", Delete;
      "Follow", Follow;
      "Accept", Accept;
      "Reject", Reject;
      "Add", Add;
      "Remove", Remove;
      "Like", Like;
      "Announce", Announce;
      "Undo", Undo;
      "Block", Block;
      "Flag", Flag;
      "Dislike", Dislike;
      "Ignore", Ignore;
      "Invite", Invite;
      "Join", Join;
      "Leave", Leave;
      "Listen", Listen;
      "Move", Move;
      "Offer", Offer;
      "Question", Question;
      "Read", Read;
      "TentativeAccept", TentativeAccept;
      "TentativeReject", TentativeReject;
      "Travel", Travel;
      "View", View;
    ]
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
end = struct
  type t = {
    context : Context.t option;
    id : Uri.t option;
    type_ : Activity_type.t;
    actor : Actor_ref.t;
    object_ : Object_ref.t option;
    target : Object_ref.t option;
    result : Object_ref.t option;
    origin : Object_ref.t option;
    instrument : Object_ref.t option;
    to_ : Recipient.t list option;
    cc : Recipient.t list option;
    bto : Recipient.t list option;
    bcc : Recipient.t list option;
    published : Datetime.t option;
    updated : Datetime.t option;
    summary : string option;
    one_of : Object_ref.t list option;
    any_of : Object_ref.t list option;
    closed : Datetime.t option;
  }

  let make ?context ?id ~type_ ~actor ?object_ ?target ?result ?origin
      ?instrument ?to_ ?cc ?bto ?bcc ?published ?updated ?summary
      ?one_of ?any_of ?closed () =
    { context; id; type_; actor; object_; target; result; origin;
      instrument; to_; cc; bto; bcc; published; updated; summary;
      one_of; any_of; closed }

  let context t = t.context
  let id t = t.id
  let type_ t = t.type_
  let actor t = t.actor
  let object_ t = t.object_
  let target t = t.target
  let result t = t.result
  let origin t = t.origin
  let instrument t = t.instrument
  let to_ t = t.to_
  let cc t = t.cc
  let bto t = t.bto
  let bcc t = t.bcc
  let published t = t.published
  let updated t = t.updated
  let summary t = t.summary
  let one_of t = t.one_of
  let any_of t = t.any_of
  let closed t = t.closed

  let jsont =
    Jsont.Object.map ~kind:"Activity"
      (fun context id type_ actor object_ target result origin instrument
        to_ cc bto bcc published updated summary one_of any_of closed ->
        { context; id; type_; actor; object_; target; result; origin;
          instrument; to_; cc; bto; bcc; published; updated; summary;
          one_of; any_of; closed })
    |> Jsont.Object.opt_mem "@context" Context.jsont ~enc:context
    |> Jsont.Object.opt_mem "id" uri_jsont ~enc:id
    |> Jsont.Object.mem "type" Activity_type.jsont ~enc:type_
    |> Jsont.Object.mem "actor" Actor_ref.jsont ~enc:actor
    |> Jsont.Object.opt_mem "object" Object_ref.jsont ~enc:object_
    |> Jsont.Object.opt_mem "target" Object_ref.jsont ~enc:target
    |> Jsont.Object.opt_mem "result" Object_ref.jsont ~enc:result
    |> Jsont.Object.opt_mem "origin" Object_ref.jsont ~enc:origin
    |> Jsont.Object.opt_mem "instrument" Object_ref.jsont ~enc:instrument
    |> Jsont.Object.opt_mem "to" (Jsont.list Recipient.jsont) ~enc:to_
    |> Jsont.Object.opt_mem "cc" (Jsont.list Recipient.jsont) ~enc:cc
    |> Jsont.Object.opt_mem "bto" (Jsont.list Recipient.jsont) ~enc:bto
    |> Jsont.Object.opt_mem "bcc" (Jsont.list Recipient.jsont) ~enc:bcc
    |> Jsont.Object.opt_mem "published" Datetime.jsont ~enc:published
    |> Jsont.Object.opt_mem "updated" Datetime.jsont ~enc:updated
    |> Jsont.Object.opt_mem "summary" Jsont.string ~enc:summary
    |> Jsont.Object.opt_mem "oneOf" (Jsont.list Object_ref.jsont) ~enc:one_of
    |> Jsont.Object.opt_mem "anyOf" (Jsont.list Object_ref.jsont) ~enc:any_of
    |> Jsont.Object.opt_mem "closed" Datetime.jsont ~enc:closed
    |> Jsont.Object.finish
end

(** Activity reference - can be URI or full Activity. *)
module Activity_ref : sig
  type t =
    | Uri of Uri.t
    | Activity of Activity.t

  val uri : Uri.t -> t
  val activity : Activity.t -> t
  val jsont : t Jsont.t
end = struct
  type t =
    | Uri of Uri.t
    | Activity of Activity.t

  let uri u = Uri u
  let activity a = Activity a

  let jsont =
    let dec_string = Jsont.map uri_jsont
        ~dec:(fun u -> Uri u)
        ~enc:(function Uri u -> u | Activity _ -> assert false) in
    let dec_object = Jsont.map Activity.jsont
        ~dec:(fun a -> Activity a)
        ~enc:(function Activity a -> a | Uri _ -> assert false) in
    Jsont.any ~kind:"Activity reference"
      ~dec_string ~dec_object
      ~enc:(function
          | Uri _ -> dec_string
          | Activity _ -> dec_object)
      ()
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

  val context : 'a t -> Context.t option
  val id : 'a t -> Uri.t option
  val total_items : 'a t -> int option
  val current : 'a t -> Uri.t option
  val first : 'a t -> Uri.t option
  val last : 'a t -> Uri.t option
  val items : 'a t -> 'a list option
  val ordered : 'a t -> bool

  val jsont : 'a Jsont.t -> 'a t Jsont.t
end = struct
  type 'a t = {
    context : Context.t option;
    id : Uri.t option;
    total_items : int option;
    current : Uri.t option;
    first : Uri.t option;
    last : Uri.t option;
    items : 'a list option;
    ordered : bool;
  }

  let make ?context ?id ?total_items ?current ?first ?last ?items ~ordered () =
    { context; id; total_items; current; first; last; items; ordered }

  let context t = t.context
  let id t = t.id
  let total_items t = t.total_items
  let current t = t.current
  let first t = t.first
  let last t = t.last
  let items t = t.items
  let ordered t = t.ordered

  let jsont item_jsont =
    let type_jsont =
      Jsont.enum ~kind:"CollectionType" [
        "Collection", false;
        "OrderedCollection", true;
      ]
    in
    let list_jsont = Jsont.list item_jsont in
    Jsont.Object.map ~kind:"Collection"
      (fun context id ordered total_items current first last items ordered_items ->
         let items = match items, ordered_items with
           | Some i, _ -> Some i
           | None, Some i -> Some i
           | None, None -> None
         in
         { context; id; total_items; current; first; last; items; ordered })
    |> Jsont.Object.opt_mem "@context" Context.jsont ~enc:context
    |> Jsont.Object.opt_mem "id" uri_jsont ~enc:id
    |> Jsont.Object.mem "type" type_jsont ~enc:ordered
    |> Jsont.Object.opt_mem "totalItems" Jsont.int ~enc:total_items
    |> Jsont.Object.opt_mem "current" uri_jsont ~enc:current
    |> Jsont.Object.opt_mem "first" uri_jsont ~enc:first
    |> Jsont.Object.opt_mem "last" uri_jsont ~enc:last
    |> Jsont.Object.opt_mem "items" list_jsont
        ~enc:(fun t -> if t.ordered then None else t.items)
    |> Jsont.Object.opt_mem "orderedItems" list_jsont
        ~enc:(fun t -> if t.ordered then t.items else None)
    |> Jsont.Object.finish
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
end = struct
  type 'a t = {
    context : Context.t option;
    id : Uri.t option;
    total_items : int option;
    current : Uri.t option;
    first : Uri.t option;
    last : Uri.t option;
    prev : Uri.t option;
    next : Uri.t option;
    part_of : Uri.t option;
    items : 'a list option;
    ordered : bool;
  }

  let make ?context ?id ?total_items ?current ?first ?last ?prev ?next
      ?part_of ?items ~ordered () =
    { context; id; total_items; current; first; last; prev; next;
      part_of; items; ordered }

  let context t = t.context
  let id t = t.id
  let total_items t = t.total_items
  let current t = t.current
  let first t = t.first
  let last t = t.last
  let prev t = t.prev
  let next t = t.next
  let part_of t = t.part_of
  let items t = t.items
  let ordered t = t.ordered

  let jsont item_jsont =
    let type_jsont =
      Jsont.enum ~kind:"CollectionPageType" [
        "CollectionPage", false;
        "OrderedCollectionPage", true;
      ]
    in
    let list_jsont = Jsont.list item_jsont in
    Jsont.Object.map ~kind:"CollectionPage"
      (fun context id ordered total_items current first last prev next
        part_of items ordered_items ->
        let items = match items, ordered_items with
          | Some i, _ -> Some i
          | None, Some i -> Some i
          | None, None -> None
        in
        { context; id; total_items; current; first; last; prev; next;
          part_of; items; ordered })
    |> Jsont.Object.opt_mem "@context" Context.jsont ~enc:context
    |> Jsont.Object.opt_mem "id" uri_jsont ~enc:id
    |> Jsont.Object.mem "type" type_jsont ~enc:ordered
    |> Jsont.Object.opt_mem "totalItems" Jsont.int ~enc:total_items
    |> Jsont.Object.opt_mem "current" uri_jsont ~enc:current
    |> Jsont.Object.opt_mem "first" uri_jsont ~enc:first
    |> Jsont.Object.opt_mem "last" uri_jsont ~enc:last
    |> Jsont.Object.opt_mem "prev" uri_jsont ~enc:prev
    |> Jsont.Object.opt_mem "next" uri_jsont ~enc:next
    |> Jsont.Object.opt_mem "partOf" uri_jsont ~enc:part_of
    |> Jsont.Object.opt_mem "items" list_jsont
        ~enc:(fun t -> if t.ordered then None else t.items)
    |> Jsont.Object.opt_mem "orderedItems" list_jsont
        ~enc:(fun t -> if t.ordered then t.items else None)
    |> Jsont.Object.finish
end

(** {1 Convenience type aliases} *)

(** Activity collection. *)
module Activity_collection : sig
  type t = Activity.t Collection.t
  val jsont : t Jsont.t
end = struct
  type t = Activity.t Collection.t
  let jsont = Collection.jsont Activity.jsont
end

(** Object collection. *)
module Object_collection : sig
  type t = Object.t Collection.t
  val jsont : t Jsont.t
end = struct
  type t = Object.t Collection.t
  let jsont = Collection.jsont Object.jsont
end

(** Activity collection page. *)
module Activity_collection_page : sig
  type t = Activity.t Collection_page.t
  val jsont : t Jsont.t
end = struct
  type t = Activity.t Collection_page.t
  let jsont = Collection_page.jsont Activity.jsont
end

(** Object collection page. *)
module Object_collection_page : sig
  type t = Object.t Collection_page.t
  val jsont : t Jsont.t
end = struct
  type t = Object.t Collection_page.t
  let jsont = Collection_page.jsont Object.jsont
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

  (** The Webfinger JRD response. *)
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
end = struct
  module Jrd_link = struct
    type t = {
      rel : string;
      type_ : string option;
      href : Uri.t option;
      template : string option;
    }

    let make ~rel ?type_ ?href ?template () =
      { rel; type_; href; template }

    let rel t = t.rel
    let type_ t = t.type_
    let href t = t.href
    let template t = t.template

    let jsont =
      Jsont.Object.map ~kind:"JrdLink"
        (fun rel type_ href template -> { rel; type_; href; template })
      |> Jsont.Object.mem "rel" Jsont.string ~enc:rel
      |> Jsont.Object.opt_mem "type" Jsont.string ~enc:type_
      |> Jsont.Object.opt_mem "href" uri_jsont ~enc:href
      |> Jsont.Object.opt_mem "template" Jsont.string ~enc:template
      |> Jsont.Object.finish
  end

  type t = {
    subject : string;
    aliases : string list option;
    properties : (string * string) list option;
    links : Jrd_link.t list option;
  }

  let make ~subject ?aliases ?properties ?links () =
    { subject; aliases; properties; links }

  let subject t = t.subject
  let aliases t = t.aliases
  let properties t = t.properties
  let links t = t.links

  module String_map = Map.Make(String)

  let properties_jsont =
    Jsont.Object.as_string_map Jsont.string
    |> Jsont.map
        ~dec:(fun m -> String_map.bindings m)
        ~enc:(fun l -> List.fold_left (fun m (k, v) ->
            String_map.add k v m) String_map.empty l)

  let jsont =
    Jsont.Object.map ~kind:"Webfinger"
      (fun subject aliases properties links ->
         { subject; aliases; properties; links })
    |> Jsont.Object.mem "subject" Jsont.string ~enc:subject
    |> Jsont.Object.opt_mem "aliases" (Jsont.list Jsont.string) ~enc:aliases
    |> Jsont.Object.opt_mem "properties" properties_jsont ~enc:properties
    |> Jsont.Object.opt_mem "links" (Jsont.list Jrd_link.jsont) ~enc:links
    |> Jsont.Object.finish
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
end = struct
  module Software = struct
    type t = {
      name : string;
      version : string;
      repository : Uri.t option;
      homepage : Uri.t option;
    }

    let make ~name ~version ?repository ?homepage () =
      { name; version; repository; homepage }

    let name t = t.name
    let version t = t.version
    let repository t = t.repository
    let homepage t = t.homepage

    let jsont =
      Jsont.Object.map ~kind:"Software"
        (fun name version repository homepage ->
           { name; version; repository; homepage })
      |> Jsont.Object.mem "name" Jsont.string ~enc:name
      |> Jsont.Object.mem "version" Jsont.string ~enc:version
      |> Jsont.Object.opt_mem "repository" uri_jsont ~enc:repository
      |> Jsont.Object.opt_mem "homepage" uri_jsont ~enc:homepage
      |> Jsont.Object.finish
  end

  module Usage = struct
    type t = {
      users_total : int option;
      users_active_half_year : int option;
      users_active_month : int option;
      local_posts : int option;
      local_comments : int option;
    }

    let make ?users_total ?users_active_half_year ?users_active_month
        ?local_posts ?local_comments () =
      { users_total; users_active_half_year; users_active_month;
        local_posts; local_comments }

    let users_total t = t.users_total
    let users_active_half_year t = t.users_active_half_year
    let users_active_month t = t.users_active_month
    let local_posts t = t.local_posts
    let local_comments t = t.local_comments

    let users_jsont =
      Jsont.Object.map ~kind:"Users"
        (fun total active_half_year active_month ->
           (total, active_half_year, active_month))
      |> Jsont.Object.opt_mem "total" Jsont.int
          ~enc:(fun (t, _, _) -> t)
      |> Jsont.Object.opt_mem "activeHalfyear" Jsont.int
          ~enc:(fun (_, h, _) -> h)
      |> Jsont.Object.opt_mem "activeMonth" Jsont.int
          ~enc:(fun (_, _, m) -> m)
      |> Jsont.Object.finish

    let jsont =
      Jsont.Object.map ~kind:"Usage"
        (fun (users_total, users_active_half_year, users_active_month)
          local_posts local_comments ->
          { users_total; users_active_half_year; users_active_month;
            local_posts; local_comments })
      |> Jsont.Object.mem "users" users_jsont
          ~dec_absent:(None, None, None)
          ~enc:(fun t -> (t.users_total, t.users_active_half_year,
                          t.users_active_month))
      |> Jsont.Object.opt_mem "localPosts" Jsont.int ~enc:local_posts
      |> Jsont.Object.opt_mem "localComments" Jsont.int ~enc:local_comments
      |> Jsont.Object.finish
  end

  type t = {
    version : string;
    software : Software.t;
    protocols : string list;
    usage : Usage.t;
    open_registrations : bool;
    metadata : Jsont.json option;
  }

  let make ~version ~software ~protocols ~usage ~open_registrations
      ?metadata () =
    { version; software; protocols; usage; open_registrations; metadata }

  let version t = t.version
  let software t = t.software
  let protocols t = t.protocols
  let usage t = t.usage
  let open_registrations t = t.open_registrations
  let metadata t = t.metadata

  let jsont =
    Jsont.Object.map ~kind:"Nodeinfo"
      (fun version software protocols usage open_registrations metadata ->
         { version; software; protocols; usage; open_registrations; metadata })
    |> Jsont.Object.mem "version" Jsont.string ~enc:version
    |> Jsont.Object.mem "software" Software.jsont ~enc:software
    |> Jsont.Object.mem "protocols" (Jsont.list Jsont.string) ~enc:protocols
    |> Jsont.Object.mem "usage" Usage.jsont ~enc:usage
    |> Jsont.Object.mem "openRegistrations" Jsont.bool ~enc:open_registrations
    |> Jsont.Object.opt_mem "metadata" Jsont.json ~enc:metadata
    |> Jsont.Object.finish
end
