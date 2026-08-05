(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** ActivityPub client library for OCaml with Eio

    This library provides a direct-style ActivityPub client using Eio for
    concurrent I/O. It handles actor discovery, inbox/outbox operations,
    HTTP signatures, and federation with other ActivityPub servers.

    {2 Overview}

    The library is organized into several components:

    - {!type:t}: Main client type for making ActivityPub requests
    - {!module:Actor}: Operations on actors (fetch, follow, unfollow)
    - {!module:Inbox}: Receiving and processing activities
    - {!module:Outbox}: Posting activities to your outbox
    - {!module:Webfinger}: Actor discovery via Webfinger protocol
    - {!module:Nodeinfo}: Server metadata discovery

    {2 Example}

    {[
      open Eio.Std

      let () = Eio_main.run @@ fun env ->
        Switch.run @@ fun sw ->

        (* Create an ActivityPub client *)
        let client = Apubt.create ~sw env in

        (* Discover an actor via Webfinger *)
        let actor = Apubt.Actor.lookup client "user@example.com" in
        Printf.printf "Found: %s\n" (Option.value ~default:"<none>" (Proto.Actor.name actor));

        (* Fetch their outbox *)
        let outbox = Apubt.Actor.outbox client actor in
        List.iter (fun activity ->
          Printf.printf "Activity: %s\n"
            (Proto.Activity_type.to_string (Proto.Activity.type_ activity))
        ) (Option.value ~default:[] (Proto.Collection.items outbox))
    ]}

    {2 HTTP Signatures}

    ActivityPub servers require HTTP signatures for authentication. Configure
    signing with {!Signing}:

    {[
      let signing = Apubt.Signing.create
        ~key_id:"https://example.com/users/alice#main-key"
        ~private_key:private_key_pem
        () in
      let client = Apubt.create ~sw ~signing env
    ]}

    @see <https://www.w3.org/TR/activitypub/> ActivityPub specification
    @see <https://www.w3.org/TR/activitystreams-core/> ActivityStreams Core *)

(** {1 Protocol Types}

    Re-exported from {!Apubt_proto} for convenience. *)

module Proto = Apubt_proto

(** {1 Client Configuration} *)

type t
(** An ActivityPub client with connection pooling and optional signing. *)

(** HTTP signature configuration for authenticated requests.

    Uses RFC 9421 HTTP Message Signatures via the Requests library.
    ActivityPub typically uses RSA-SHA256 signatures.

    The following message components are signed:
    - [@method] - HTTP request method
    - [@authority] - Target host
    - [@path] - Request target path
    - [date] - Date header
    - [content-digest] - SHA-256 digest of request body
    - [content-type] - Content-Type header *)
module Signing : sig
  type t
  (** Signing configuration. *)

  val create :
    key_id:string ->
    key:Requests.Signature.Key.t ->
    unit ->
    t
  (** [create ~key_id ~key ()] creates a signing configuration.

      @param key_id The key ID URI (typically actor URI + "#main-key")
      @param key The cryptographic key from {!Requests.Signature.Key} *)

  val from_pem :
    key_id:string ->
    pem:string ->
    unit ->
    (t, string) result
  (** [from_pem ~key_id ~pem ()] creates a signing configuration from a
      PEM-encoded RSA private key.

      @param key_id The key ID URI (typically actor URI + "#main-key")
      @param pem PEM-encoded RSA private key
      @return [Ok t] on success, [Error msg] if PEM parsing fails *)

  val from_pem_exn :
    key_id:string ->
    pem:string ->
    unit ->
    t
  (** [from_pem_exn ~key_id ~pem ()] is like {!from_pem} but raises
      {!E} with {!Error.Signature_error} on failure. *)

  val key_id : t -> string
  (** [key_id t] returns the key ID URI. *)

  val key : t -> Requests.Signature.Key.t
  (** [key t] returns the signing key. *)
end

val create :
  sw:Eio.Switch.t ->
  ?signing:Signing.t ->
  ?user_agent:string ->
  ?timeout:float ->
  < clock : _ Eio.Time.clock
  ; net : _ Eio.Net.t
  ; fs : Eio.Fs.dir_ty Eio.Path.t
  ; .. > ->
  t
(** [create ~sw ?signing ?user_agent ?timeout env] creates an ActivityPub client.

    @param sw Switch for resource management
    @param signing HTTP signature configuration for authenticated requests
    @param user_agent User-Agent header (default: "Apubt/0.1")
    @param timeout Request timeout in seconds (default: 30.0) *)

val user_agent : t -> string
(** [user_agent t] returns the User-Agent string used by the client. *)

(** {1 Error Handling} *)

(** Error types for ActivityPub operations. *)
module Error : sig
  type t =
    | Http_error of int * string  (** HTTP error with status code and body *)
    | Json_error of string  (** JSON parsing error *)
    | Webfinger_error of string  (** Webfinger lookup failed *)
    | Signature_error of string  (** HTTP signature error *)
    | Not_found  (** Resource not found (404) *)
    | Unauthorized  (** Authentication required or failed (401/403) *)
    | Rate_limited of float option  (** Rate limited, with optional retry-after *)
    | Network_error of string  (** Network/connection error *)
    | Invalid_actor of string  (** Actor validation failed *)

  val pp : Format.formatter -> t -> unit
  (** Pretty-print an error. *)

  val to_string : t -> string
  (** Convert error to string. *)
end

exception E of Error.t
(** Exception raised by client operations. *)

(** {1 Webfinger Discovery} *)

(** Webfinger actor discovery per RFC 7033.

    This module uses the [webfinger] library for robust RFC 7033/7565 compliance
    with proper acct URI handling and percent-encoding. See the
    {{:https://swicg.github.io/activitypub-webfinger/}ActivityPub WebFinger spec}
    for details on how WebFinger is used with ActivityPub.

    @see <https://www.rfc-editor.org/rfc/rfc7033> RFC 7033 WebFinger
    @see <https://www.rfc-editor.org/rfc/rfc7565> RFC 7565 acct URI *)
module Webfinger : sig
  val lookup : t -> string -> Proto.Webfinger.t
  (** [lookup client acct] performs a Webfinger lookup for the given account.

      The [acct] can be in the form "user@domain" or "acct:user@domain".
      Uses the [webfinger] library for proper RFC 7565 acct URI handling.

      @raise E on lookup failure *)

  val lookup_raw : t -> string -> Webfinger.Jrd.t
  (** [lookup_raw client acct] performs a Webfinger lookup returning the raw
      [Webfinger.Jrd.t] from the webfinger library.

      This is more efficient when you only need to extract specific fields
      and don't need the full {!Proto.Webfinger.t} type.

      @raise E on lookup failure *)

  val actor_uri : Proto.Webfinger.t -> Uri.t option
  (** [actor_uri jrd] extracts the ActivityPub actor URI from a Webfinger response.

      Looks for a link with [rel="self"] and [type="application/activity+json"]
      or [type="application/ld+json; profile=..."].

      Per the ActivityPub WebFinger spec, publishers SHOULD include exactly one
      such link. *)

  val actor_uri_raw : Webfinger.Jrd.t -> Uri.t option
  (** [actor_uri_raw jrd] extracts the ActivityPub actor URI from a raw JRD.

      More efficient variant that works directly with {!Webfinger.Jrd.t}. *)

  val profile_page : Proto.Webfinger.t -> Uri.t option
  (** [profile_page jrd] extracts the HTML profile page URI from a Webfinger response.

      Looks for [rel="http://webfinger.net/rel/profile-page"]. *)

  val subscribe_template : Proto.Webfinger.t -> string option
  (** [subscribe_template jrd] extracts the subscribe/follow template URI.

      Looks for [rel="http://ostatus.org/schema/1.0/subscribe"].
      This is used for remote follow buttons. The template contains [{uri}]
      which should be replaced with the actor to follow. *)
end

(** {1 NodeInfo Discovery} *)

(** NodeInfo server metadata discovery.
    @see <https://nodeinfo.diaspora.software/> *)
module Nodeinfo : sig
  val fetch : t -> host:string -> Proto.Nodeinfo.t
  (** [fetch client ~host] fetches NodeInfo for the given host.

      First fetches the well-known NodeInfo links, then fetches the actual
      NodeInfo document.

      @raise E on fetch failure *)

  val software_name : Proto.Nodeinfo.t -> string
  (** [software_name info] returns the server software name (e.g., "mastodon", "pleroma"). *)

  val software_version : Proto.Nodeinfo.t -> string
  (** [software_version info] returns the server software version. *)

  val supports_activitypub : Proto.Nodeinfo.t -> bool
  (** [supports_activitypub info] returns [true] if the server supports ActivityPub. *)
end

(** {1 Actor Operations} *)

(** Operations on ActivityPub actors. *)
module Actor : sig
  val fetch : t -> Uri.t -> Proto.Actor.t
  (** [fetch client uri] fetches an actor by URI.

      @raise E on fetch failure *)

  val lookup : t -> string -> Proto.Actor.t
  (** [lookup client acct] looks up an actor by Webfinger account.

      Combines {!Webfinger.lookup} and {!fetch} for convenience.

      @raise E on lookup failure *)

  (** {2 Collections} *)

  val inbox : t -> Proto.Actor.t -> Uri.t
  (** [inbox client actor] returns the inbox URI for the actor. *)

  val outbox : t -> Proto.Actor.t -> Proto.Activity.t Proto.Collection.t
  (** [outbox client actor] fetches the actor's outbox collection.

      @raise E on fetch failure *)

  val outbox_page :
    t ->
    Proto.Actor.t ->
    ?page:Uri.t ->
    unit ->
    Proto.Activity.t Proto.Collection_page.t
  (** [outbox_page client actor ?page ()] fetches a page of the outbox.

      @param page URI of specific page to fetch (default: first page)
      @raise E on fetch failure *)

  val followers : t -> Proto.Actor.t -> Proto.Actor.t Proto.Collection.t
  (** [followers client actor] fetches the actor's followers collection.

      Note: Many servers restrict follower list visibility.

      @raise E on fetch failure *)

  val following : t -> Proto.Actor.t -> Proto.Actor.t Proto.Collection.t
  (** [following client actor] fetches the actor's following collection.

      Note: Many servers restrict following list visibility.

      @raise E on fetch failure *)

  (** {2 Follow/Unfollow} *)

  val follow : t -> actor:Proto.Actor.t -> target:Proto.Actor.t -> Proto.Activity.t
  (** [follow client ~actor ~target] creates and sends a Follow activity.

      The [actor] is the local actor performing the follow (requires signing).
      The [target] is the remote actor to follow.

      @raise E on send failure *)

  val unfollow : t -> actor:Proto.Actor.t -> target:Proto.Actor.t -> Proto.Activity.t
  (** [unfollow client ~actor ~target] creates and sends an Undo(Follow) activity.

      @raise E on send failure *)

  val accept_follow :
    t ->
    actor:Proto.Actor.t ->
    follow:Proto.Activity.t ->
    Proto.Activity.t
  (** [accept_follow client ~actor ~follow] accepts an incoming Follow request.

      @raise E on send failure *)

  val reject_follow :
    t ->
    actor:Proto.Actor.t ->
    follow:Proto.Activity.t ->
    Proto.Activity.t
  (** [reject_follow client ~actor ~follow] rejects an incoming Follow request.

      @raise E on send failure *)
end

(** {1 Object Operations} *)

(** Operations on ActivityStreams objects (notes, articles, etc). *)
module Object : sig
  val fetch : t -> Uri.t -> Proto.Object.t
  (** [fetch client uri] fetches an object by URI.

      @raise E on fetch failure *)

  val replies : t -> Proto.Object.t -> Proto.Object.t Proto.Collection.t option
  (** [replies client obj] fetches the replies collection for an object, if any.

      @raise E on fetch failure *)
end

(** {1 Inbox Operations} *)

(** Operations for receiving activities in an inbox. *)
module Inbox : sig
  val post : t -> inbox:Uri.t -> Proto.Activity.t -> unit
  (** [post client ~inbox activity] delivers an activity to a remote inbox.

      The request is signed using the client's signing configuration.

      @raise E on delivery failure *)

  val post_to_actor : t -> Proto.Actor.t -> Proto.Activity.t -> unit
  (** [post_to_actor client actor activity] delivers an activity to an actor's inbox.

      Equivalent to [post client ~inbox:(Actor.inbox client actor) activity].

      @raise E on delivery failure *)

  val post_to_shared_inbox :
    t ->
    host:string ->
    Proto.Activity.t ->
    unit
  (** [post_to_shared_inbox client ~host activity] delivers to a server's shared inbox.

      Uses the shared inbox from the server's NodeInfo if available,
      otherwise falls back to individual inboxes.

      @raise E on delivery failure *)
end

(** {1 Outbox Operations} *)

(** Operations for posting activities to an outbox. *)
module Outbox : sig
  (** {2 Creating Notes} *)

  val create_note :
    t ->
    actor:Proto.Actor.t ->
    ?in_reply_to:Uri.t ->
    ?to_:Proto.Recipient.t list ->
    ?cc:Proto.Recipient.t list ->
    ?sensitive:bool ->
    ?summary:string ->
    content:string ->
    unit ->
    Proto.Activity.t
  (** [create_note client ~actor ?in_reply_to ?to_ ?cc ?sensitive ?summary ~content ()]
      creates and sends a Create(Note) activity.

      @param actor The local actor creating the note
      @param in_reply_to URI of note being replied to
      @param to_ Primary recipients (default: public)
      @param cc Secondary recipients
      @param sensitive Content warning flag
      @param summary Content warning text (if sensitive)
      @param content Note content (HTML)
      @raise E on send failure *)

  val public_note :
    t ->
    actor:Proto.Actor.t ->
    ?in_reply_to:Uri.t ->
    content:string ->
    unit ->
    Proto.Activity.t
  (** [public_note client ~actor ?in_reply_to ~content ()] creates a public note.

      Shorthand for {!create_note} with [to_] set to the public collection
      and [cc] set to the actor's followers.

      @raise E on send failure *)

  val followers_only_note :
    t ->
    actor:Proto.Actor.t ->
    ?in_reply_to:Uri.t ->
    content:string ->
    unit ->
    Proto.Activity.t
  (** [followers_only_note client ~actor ?in_reply_to ~content ()] creates
      a followers-only note.

      @raise E on send failure *)

  val direct_note :
    t ->
    actor:Proto.Actor.t ->
    to_:Proto.Actor.t list ->
    ?in_reply_to:Uri.t ->
    content:string ->
    unit ->
    Proto.Activity.t
  (** [direct_note client ~actor ~to_ ?in_reply_to ~content ()] creates
      a direct message to specific recipients.

      @raise E on send failure *)

  (** {2 Interactions} *)

  val like : t -> actor:Proto.Actor.t -> object_:Uri.t -> Proto.Activity.t
  (** [like client ~actor ~object_] likes an object.

      @raise E on send failure *)

  val unlike : t -> actor:Proto.Actor.t -> object_:Uri.t -> Proto.Activity.t
  (** [unlike client ~actor ~object_] unlikes an object (Undo(Like)).

      @raise E on send failure *)

  val announce : t -> actor:Proto.Actor.t -> object_:Uri.t -> Proto.Activity.t
  (** [announce client ~actor ~object_] boosts/reblogs an object.

      @raise E on send failure *)

  val unannounce : t -> actor:Proto.Actor.t -> object_:Uri.t -> Proto.Activity.t
  (** [unannounce client ~actor ~object_] unboosts an object (Undo(Announce)).

      @raise E on send failure *)

  (** {2 Deletion} *)

  val delete : t -> actor:Proto.Actor.t -> object_:Uri.t -> Proto.Activity.t
  (** [delete client ~actor ~object_] deletes an object.

      Creates a Delete activity with a Tombstone object.

      @raise E on send failure *)

  (** {2 Updates} *)

  val update_note :
    t ->
    actor:Proto.Actor.t ->
    object_:Uri.t ->
    content:string ->
    unit ->
    Proto.Activity.t
  (** [update_note client ~actor ~object_ ~content ()] updates a note's content.

      @raise E on send failure *)
end

(** {1 Collection Iteration} *)

(** Utilities for iterating over paginated collections. *)
module Collection : sig
  val iter :
    t ->
    ('a -> unit) ->
    'a Proto.Collection.t ->
    'a Jsont.t ->
    unit
  (** [iter client f collection item_jsont] iterates over all items in a collection,
      automatically fetching subsequent pages.

      @raise E on fetch failure *)

  val fold :
    t ->
    ('acc -> 'a -> 'acc) ->
    'acc ->
    'a Proto.Collection.t ->
    'a Jsont.t ->
    'acc
  (** [fold client f init collection item_jsont] folds over all items in a collection,
      automatically fetching subsequent pages.

      @raise E on fetch failure *)

  val to_list :
    t ->
    'a Proto.Collection.t ->
    'a Jsont.t ->
    'a list
  (** [to_list client collection item_jsont] returns all items in a collection as a list.

      Warning: This may perform many HTTP requests for large collections.

      @raise E on fetch failure *)

  val first_page :
    t ->
    'a Proto.Collection.t ->
    'a Jsont.t ->
    'a Proto.Collection_page.t option
  (** [first_page client collection item_jsont] fetches the first page of a collection.

      @raise E on fetch failure *)

  val next_page :
    t ->
    'a Proto.Collection_page.t ->
    'a Jsont.t ->
    'a Proto.Collection_page.t option
  (** [next_page client page item_jsont] fetches the next page, if any.

      @raise E on fetch failure *)
end

(** {1 Low-Level HTTP} *)

(** Low-level HTTP operations with ActivityPub content negotiation. *)
module Http : sig
  val get : t -> Uri.t -> Jsont.json
  (** [get client uri] performs a GET request with ActivityPub Accept header.

      @raise E on request failure *)

  val get_typed : t -> 'a Jsont.t -> Uri.t -> 'a
  (** [get_typed client jsont uri] performs a GET and decodes the response.

      @raise E on request failure *)

  val post : t -> Uri.t -> Jsont.json -> unit
  (** [post client uri body] performs a signed POST request.

      @raise E on request failure *)

  val post_typed : t -> 'a Jsont.t -> Uri.t -> 'a -> unit
  (** [post_typed client jsont uri value] encodes and POSTs a typed value.

      @raise E on request failure *)
end
