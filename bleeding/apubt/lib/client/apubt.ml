(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

module Proto = Apubt_proto

module Error = struct
  type t =
    | Http_error of int * string
    | Json_error of string
    | Webfinger_error of string
    | Signature_error of string
    | Not_found
    | Unauthorized
    | Rate_limited of float option
    | Network_error of string
    | Invalid_actor of string

  let pp fmt = function
    | Http_error (code, body) ->
        Format.fprintf fmt "HTTP error %d: %s" code body
    | Json_error msg -> Format.fprintf fmt "JSON error: %s" msg
    | Webfinger_error msg -> Format.fprintf fmt "Webfinger error: %s" msg
    | Signature_error msg -> Format.fprintf fmt "Signature error: %s" msg
    | Not_found -> Format.fprintf fmt "Not found"
    | Unauthorized -> Format.fprintf fmt "Unauthorized"
    | Rate_limited None -> Format.fprintf fmt "Rate limited"
    | Rate_limited (Some secs) ->
        Format.fprintf fmt "Rate limited, retry after %.0f seconds" secs
    | Network_error msg -> Format.fprintf fmt "Network error: %s" msg
    | Invalid_actor msg -> Format.fprintf fmt "Invalid actor: %s" msg

  let to_string t =
    Format.asprintf "%a" pp t
end

exception E of Error.t

module Signing = struct
  type t = {
    key_id : string;
    key : Fetch_signature.Key.t;
    config : Fetch_signature.config;
  }

  (** Cover the complete request target and body for RFC 9421 federation. *)
  let activitypub_components =
    Fetch_signature.Component.[
      method_;
      target_uri;
      date;
      content_digest;
      content_type;
    ]

  let create ~key_id ~key () =
    let algorithm = match Fetch_signature.Key.algorithm key with
      | Some `Rsa_pss_sha512 -> Some `Rsa_v1_5_sha256
      | algorithm -> algorithm
    in
    let config = Fetch_signature.config
      ~key ?algorithm
      ~keyid:key_id
      ~components:activitypub_components
      ()
    in
    { key_id; key; config }

  let from_pem ~key_id ~pem () =
    (* Parse PEM-encoded RSA private key *)
    match X509.Private_key.decode_pem pem with
    | Ok (`RSA priv) ->
        let key = Fetch_signature.Key.rsa ~priv in
        Ok (create ~key_id ~key ())
    | Ok _ ->
        Error "Only RSA keys are supported for ActivityPub signatures"
    | Error (`Msg msg) ->
        Error ("Failed to parse PEM key: " ^ msg)

  let from_pem_exn ~key_id ~pem () =
    match from_pem ~key_id ~pem () with
    | Ok t -> t
    | Error msg -> raise (E (Signature_error msg))

  let key_id t = t.key_id
  let key t = t.key
end

type t = {
  fetch : Fetch.plain;
  post_fetch : Fetch.plain;
  user_agent : string;
  max_response_bytes : int;
}

let activitypub_accept =
  "application/activity+json, application/ld+json; profile=\"https://www.w3.org/ns/activitystreams\""

let activitypub_media jsont =
  Fetch.Json.v ~media:"application/activity+json"
    ~accept:["application/ld+json"; "application/json"] jsont

let of_fetch ~clock ?signing ?(user_agent = "Apubt/0.1")
    ?(max_response_bytes = 16 * 1024 * 1024) fetch =
  if max_response_bytes < 0 then
    invalid_arg "Apubt.of_fetch: max_response_bytes must be non-negative";
  let agent = user_agent in
  let fetch = Fetch.with_headers ~mode:`If_absent
      Fetch.Header.[raw "Accept" activitypub_accept; user_agent, agent]
      fetch in
  let post_fetch = match signing with
    | None -> fetch
    | Some (signing : Signing.t) ->
        Fetch_signature.Middleware.sign ~clock ~key:signing.key
          ~config:signing.config fetch
  in
  { fetch; post_fetch; user_agent; max_response_bytes }

let create ~sw ?signing ?user_agent ?max_response_bytes ?(timeout = 30.0) env =
  if not (Float.is_finite timeout) || timeout < 0.0 then
    invalid_arg "Apubt.create: timeout must be finite and non-negative";
  let duration = Duration.of_f timeout in
  let timeout = if timeout > 0.0 && duration = 0L then 1L else duration in
  let fetch = Fetch_curl.v ~sw ~timeout ~connect_timeout:timeout () in
  of_fetch ~clock:(Eio.Stdenv.clock env) ?signing ?user_agent ?max_response_bytes fetch

let user_agent t = t.user_agent

let is_success status = status >= 200 && status < 300

(* Preserve the public error API while letting Eio cancellation propagate. *)
let with_errors f =
  try f () with
  | Eio.Io (Fetch.E (Fetch.Decode_failure { error; _ }), _) ->
      raise (E (Json_error (Fetch.Media.error_to_string error)))
  | Eio.Io (Fetch.E (Fetch.Invalid_request msg), _)
      when String.starts_with ~prefix:"fetch-signature:" msg ->
      raise (E (Signature_error msg))
  | Eio.Io _ as ex -> raise (E (Network_error (Printexc.to_string ex)))

let check_response t resp =
  let status = Fetch.status resp in
  if is_success status then ()
  else if status = 404 then raise (E Not_found)
  else if status = 401 || status = 403 then raise (E Unauthorized)
  else if status = 429 then begin
    let retry_after =
      match Fetch.header Fetch.Header.retry_after resp with
      | Some (`Seconds s) -> Some (float_of_int s)
      | Some (`Date _) | None -> None
    in
    raise (E (Rate_limited retry_after))
  end else begin
    let body =
      try Fetch.decode ~limit:(min t.max_response_bytes (64 * 1024))
          Fetch.Media.octets resp
      with Eio.Io (Fetch.E (Fetch.Decode_failure { error = Too_large _; _ }), _) ->
        "[response body exceeds diagnostic limit]"
    in
    raise (E (Http_error (status, body)))
  end

let get_json client ?headers codec url =
  with_errors @@ fun () ->
  Fetch.with_response ?headers client.fetch `GET url @@ fun resp ->
  check_response client resp;
  Fetch.decode ~limit:client.max_response_bytes codec resp

module Http = struct
  let get_typed client jsont uri =
    get_json client (activitypub_media jsont) (Uriz.to_string uri)

  let get client uri = get_typed client Jsont.json uri

  let post_typed client jsont uri value =
    with_errors @@ fun () ->
    let headers, body =
      try Fetch.encode (activitypub_media jsont) value
      with Invalid_argument msg -> raise (E (Json_error msg))
    in
    (* Inbox delivery is one POST. Redirecting may disclose a private body,
       turn delivery into a GET, or invalidate a signature. *)
    Fetch.with_response ~redirects:0 ~headers ~body client.post_fetch `POST
      (Uriz.to_string uri) @@ fun resp ->
    check_response client resp

  let post client uri body = post_typed client Jsont.json uri body
end

module Webfinger = struct
  (** Convert a webfinger library Jrd to our internal Proto.Webfinger type *)
  let jrd_of_webfinger (jrd : Webfinger.Jrd.t) : Proto.Webfinger.t =
    let links = List.map (fun (link : Webfinger.Link.t) ->
      Proto.Webfinger.Jrd_link.make
        ~rel:(Webfinger.Link.rel link)
        ?type_:(Webfinger.Link.type_ link)
        ?href:(Option.bind (Webfinger.Link.href link) (fun value ->
          match Uriz.of_string value with This uri -> Some uri | Null -> None))
        ?template:(
          (* Try to get template from properties if it exists *)
          Webfinger.Link.property ~uri:"template" link
        )
        ()
    ) (Webfinger.Jrd.links jrd) in
    let aliases = match Webfinger.Jrd.aliases jrd with
      | [] -> None
      | a -> Some a
    in
    let properties = match Webfinger.Jrd.properties jrd with
      | [] -> None
      | p -> Some (List.filter_map (fun (k, v) ->
          match v with Some s -> Some (k, s) | None -> None
        ) p)
    in
    Proto.Webfinger.make
      ~subject:(Option.value ~default:"" (Webfinger.Jrd.subject jrd))
      ?aliases
      ?properties
      ~links
      ()

  let lookup_raw client acct =
    let acct = if String.starts_with ~prefix:"acct:" acct then acct
      else "acct:" ^ acct in
    let acct = match Webfinger.Acct.of_string acct with
      | Ok acct -> acct
      | Error err -> raise (E (Webfinger_error (Webfinger.error_to_string err)))
    in
    let headers = Fetch.Header.[accept, [pref "application/jrd+json"]] in
    get_json client ~headers (Fetch.Json.v Webfinger.Jrd.jsont)
      (Webfinger.webfinger_url_acct acct ())

  let lookup client acct = jrd_of_webfinger (lookup_raw client acct)

  let activitypub_type = function
    | None -> false
    | Some media ->
        Fetch.Media.matches ~range:"application/activity+json" media ||
        Fetch.Media.matches ~range:"application/ld+json" media

  let uri_of_string value = match Uriz.of_string value with
    | This uri -> Some uri
    | Null -> None

  let actor_uri jrd =
    Option.value ~default:[] (Proto.Webfinger.links jrd)
    |> List.find_map (fun link ->
      if Proto.Webfinger.Jrd_link.rel link = Webfinger.Rel.activitypub &&
         activitypub_type (Proto.Webfinger.Jrd_link.type_ link)
      then Proto.Webfinger.Jrd_link.href link else None)

  let actor_uri_raw jrd =
    Webfinger.Jrd.links jrd |> List.find_map (fun link ->
      if Webfinger.Link.rel link = Webfinger.Rel.activitypub &&
         activitypub_type (Webfinger.Link.type_ link)
      then Option.bind (Webfinger.Link.href link) uri_of_string else None)

  let profile_page jrd =
    match Proto.Webfinger.links jrd with
    | None -> None
    | Some links ->
        List.find_map (fun link ->
          if Proto.Webfinger.Jrd_link.rel link = Webfinger.Rel.profile then
            Proto.Webfinger.Jrd_link.href link
          else None
        ) links

  let subscribe_template jrd =
    match Proto.Webfinger.links jrd with
    | None -> None
    | Some links ->
        List.find_map (fun link ->
          if Proto.Webfinger.Jrd_link.rel link = Webfinger.Rel.subscribe then
            Proto.Webfinger.Jrd_link.template link
          else None
        ) links
end

module Nodeinfo = struct
  (* Well-known nodeinfo link structure *)
  module Well_known_link = struct
    type t = {
      rel : string;
      href : string;
    }

    let jsont =
      Jsont.Object.map ~kind:"WellKnownLink"
        (fun rel href -> { rel; href })
      |> Jsont.Object.mem "rel" Jsont.string ~enc:(fun t -> t.rel)
      |> Jsont.Object.mem "href" Jsont.string ~enc:(fun t -> t.href)
      |> Jsont.Object.finish
  end

  module Well_known = struct
    type t = {
      links : Well_known_link.t list;
    }

    let jsont =
      Jsont.Object.map ~kind:"WellKnownNodeinfo"
        (fun links -> { links })
      |> Jsont.Object.mem "links" (Jsont.list Well_known_link.jsont)
          ~enc:(fun t -> t.links)
      |> Jsont.Object.finish
  end

  let fetch client ~host =
    (* Step 1: Fetch the well-known nodeinfo discovery document *)
    let well_known_url = Printf.sprintf "https://%s/.well-known/nodeinfo" host in
    let headers = Fetch.Header.[ accept, [ pref "application/json" ] ] in
    let well_known =
      get_json client ~headers (Fetch.Json.v Well_known.jsont) well_known_url
    in
    (* Step 2: Find a link with rel containing "nodeinfo" and schema 2.0 or 2.1 *)
    let nodeinfo_href =
      List.find_map (fun (link : Well_known_link.t) ->
        (* Check if rel contains nodeinfo and is schema 2.0 or 2.1 *)
        if String.length link.rel > 0 &&
           (String.ends_with ~suffix:"/schema/2.0" link.rel ||
            String.ends_with ~suffix:"/schema/2.1" link.rel)
        then Some link.href
        else None
      ) well_known.links
    in
    match nodeinfo_href with
    | None -> raise (E (Json_error "No NodeInfo 2.0 or 2.1 link found in well-known response"))
    | Some href ->
        (* Step 3: Fetch the actual NodeInfo document *)
        get_json client ~headers (Fetch.Json.v Proto.Nodeinfo.jsont) href

  let software_name info =
    Proto.Nodeinfo.Software.name (Proto.Nodeinfo.software info)

  let software_version info =
    Proto.Nodeinfo.Software.version (Proto.Nodeinfo.software info)

  let supports_activitypub info =
    List.mem "activitypub" (Proto.Nodeinfo.protocols info)
end

module Actor = struct
  let fetch t uri =
    Http.get_typed t Proto.Actor.jsont uri

  let lookup t acct =
    (* Use the raw webfinger lookup for efficiency - avoids converting to Proto.Webfinger *)
    let jrd = Webfinger.lookup_raw t acct in
    match Webfinger.actor_uri_raw jrd with
    | Some uri -> fetch t uri
    | None -> raise (E (Webfinger_error "No ActivityPub actor link in Webfinger response"))

  let inbox _t actor = Proto.Actor.inbox actor

  let outbox t actor =
    let uri = Proto.Actor.outbox actor in
    Http.get_typed t Proto.Activity_collection.jsont uri

  let outbox_page t actor ?page () =
    let uri = match page with
      | Some p -> p
      | None ->
          let collection = outbox t actor in
          match Proto.Collection.first collection with
          | Some first -> first
          | None -> raise (E (Invalid_actor "Outbox has no first page"))
    in
    Http.get_typed t Proto.Activity_collection_page.jsont uri

  let followers t actor =
    match Proto.Actor.followers actor with
    | Some uri -> Http.get_typed t (Proto.Collection.jsont Proto.Actor.jsont) uri
    | None -> raise (E (Invalid_actor "Actor has no followers collection"))

  let following t actor =
    match Proto.Actor.following actor with
    | Some uri -> Http.get_typed t (Proto.Collection.jsont Proto.Actor.jsont) uri
    | None -> raise (E (Invalid_actor "Actor has no following collection"))

  (* Helper to post activity to an actor's inbox *)
  let post_to_inbox t actor activity =
    let inbox_uri = Proto.Actor.inbox actor in
    Http.post_typed t Proto.Activity.jsont inbox_uri activity

  let follow t ~actor ~target =
    (* Create a Follow activity: actor follows target *)
    let follow_activity = Proto.Activity.make
      ~context:Proto.Context.default
      ~type_:Proto.Activity_type.Follow
      ~actor:(Proto.Actor_ref.actor actor)
      ~object_:(Proto.Object_ref.uri (Proto.Actor.id target))
      ()
    in
    (* Deliver to target's inbox *)
    post_to_inbox t target follow_activity;
    follow_activity

  let unfollow t ~actor ~target =
    (* Create a Follow activity representing the original follow *)
    let follow_activity = Proto.Activity.make
      ~type_:Proto.Activity_type.Follow
      ~actor:(Proto.Actor_ref.actor actor)
      ~object_:(Proto.Object_ref.uri (Proto.Actor.id target))
      ()
    in
    (* Wrap in an Undo activity *)
    let undo_activity = Proto.Activity.make
      ~context:Proto.Context.default
      ~type_:Proto.Activity_type.Undo
      ~actor:(Proto.Actor_ref.actor actor)
      ~object_:(Proto.Object_ref.uri (
        match Proto.Activity.id follow_activity with
        | Some id -> id
        | None -> Proto.Actor.id actor (* fallback: use actor ID as base *)
      ))
      ()
    in
    (* Deliver to target's inbox *)
    post_to_inbox t target undo_activity;
    undo_activity

  let accept_follow t ~actor ~follow =
    (* Create an Accept activity *)
    (* The object is the Follow activity being accepted *)
    let follow_ref = match Proto.Activity.id follow with
      | Some id -> Proto.Object_ref.uri id
      | None ->
          (* If the follow has no ID, we need to reference it somehow.
             In practice, Follow activities should always have IDs. *)
          Proto.Object_ref.uri (Proto.Actor.id actor)
    in
    let accept_activity = Proto.Activity.make
      ~context:Proto.Context.default
      ~type_:Proto.Activity_type.Accept
      ~actor:(Proto.Actor_ref.actor actor)
      ~object_:follow_ref
      ()
    in
    (* Get the follower's URI from the Follow activity's actor *)
    let follower_uri = match Proto.Activity.actor follow with
      | Proto.Actor_ref.Uri uri -> uri
      | Proto.Actor_ref.Actor a -> Proto.Actor.id a
    in
    (* Deliver to the follower's inbox - we need to fetch their actor info *)
    let follower = fetch t follower_uri in
    post_to_inbox t follower accept_activity;
    accept_activity

  let reject_follow t ~actor ~follow =
    (* Create a Reject activity *)
    (* The object is the Follow activity being rejected *)
    let follow_ref = match Proto.Activity.id follow with
      | Some id -> Proto.Object_ref.uri id
      | None ->
          (* If the follow has no ID, we need to reference it somehow.
             In practice, Follow activities should always have IDs. *)
          Proto.Object_ref.uri (Proto.Actor.id actor)
    in
    let reject_activity = Proto.Activity.make
      ~context:Proto.Context.default
      ~type_:Proto.Activity_type.Reject
      ~actor:(Proto.Actor_ref.actor actor)
      ~object_:follow_ref
      ()
    in
    (* Get the follower's URI from the Follow activity's actor *)
    let follower_uri = match Proto.Activity.actor follow with
      | Proto.Actor_ref.Uri uri -> uri
      | Proto.Actor_ref.Actor a -> Proto.Actor.id a
    in
    (* Deliver to the follower's inbox - we need to fetch their actor info *)
    let follower = fetch t follower_uri in
    post_to_inbox t follower reject_activity;
    reject_activity
end

module Object = struct
  let fetch t uri =
    Http.get_typed t Proto.Object.jsont uri

  let replies t obj =
    match Proto.Object.replies obj with
    | Some uri -> Some (Http.get_typed t Proto.Object_collection.jsont uri)
    | None -> None
end

module Inbox = struct
  let post t ~inbox activity =
    Http.post_typed t Proto.Activity.jsont inbox activity

  let post_to_actor t actor activity =
    let inbox = Actor.inbox t actor in
    post t ~inbox activity

  let discover_shared_inbox client ~host =
    let url = Printf.sprintf "https://%s/actor" host in
    try
      let actor = get_json client (activitypub_media Proto.Actor.jsont) url in
      Option.bind (Proto.Actor.endpoints actor) Proto.Endpoints.shared_inbox
    with E _ -> None

  let post_to_shared_inbox t ~host activity =
    match discover_shared_inbox t ~host with
    | Some shared_inbox ->
        post t ~inbox:shared_inbox activity
    | None ->
        (* Fallback: construct a standard shared inbox URL *)
        let shared_inbox = Uriz.of_string_exn (Printf.sprintf "https://%s/inbox" host) in
        post t ~inbox:shared_inbox activity
end

module Outbox = struct
  (* Generate a unique URI for a new object/activity based on actor's base URI.
     Uses timestamp + random suffix for uniqueness. *)
  let generate_uri ~actor ~suffix =
    let actor_uri = Uriz.to_string (Proto.Actor.id actor) in
    let now = Ptime_clock.now () in
    let ts = Ptime.to_float_s now |> int_of_float in
    let rand = Random.bits () land 0xFFFFFF in
    let unique_id = Printf.sprintf "%d-%06x" ts rand in
    Uriz.of_string_exn (actor_uri ^ "/" ^ suffix ^ "/" ^ unique_id)

  (* Get the current timestamp as an ISO 8601 string *)
  let now_datetime () =
    let now = Ptime_clock.now () in
    Proto.Datetime.v (Ptime.to_rfc3339 now)

  (* Extract inbox URIs from a list of recipients, resolving actors as needed *)
  let resolve_recipient_inboxes t recipients =
    List.filter_map (fun recipient ->
      let uri = Proto.Recipient.id recipient in
      let uri_str = Uriz.to_string uri in
      (* Skip the public collection - it doesn't have an inbox *)
      if String.equal uri_str (Uriz.to_string Proto.Public.id) then
        None
      else
        let actor = Actor.fetch t uri in
        Some (Proto.Actor.inbox actor)
    ) recipients

  (* Deliver an activity to all recipients in to/cc *)
  let deliver t activity =
    let to_recipients = Option.value ~default:[] (Proto.Activity.to_ activity) in
    let cc_recipients = Option.value ~default:[] (Proto.Activity.cc activity) in
    let all_recipients = to_recipients @ cc_recipients in
    let inboxes = resolve_recipient_inboxes t all_recipients in
    (* Deduplicate inboxes *)
    let seen = Hashtbl.create 16 in
    let unique_inboxes = List.filter (fun inbox ->
      let uri_str = Uriz.to_string inbox in
      if Hashtbl.mem seen uri_str then false
      else begin
        Hashtbl.add seen uri_str ();
        true
      end
    ) inboxes in
    (* A failed resolution or delivery must be visible to the caller. Earlier
       inboxes may already have accepted the activity when a later one fails. *)
    List.iter (fun inbox -> Inbox.post t ~inbox activity) unique_inboxes

  let create_note t ~actor ?in_reply_to ?to_ ?cc ?sensitive ?summary ~content () =
    let note_id = generate_uri ~actor ~suffix:"notes" in
    let activity_id = generate_uri ~actor ~suffix:"activities" in
    let published = now_datetime () in
    (* Build the Note object *)
    let note = Proto.Object.make
      ~context:Proto.Context.default
      ~id:note_id
      ~type_:Proto.Object_type.Note
      ~content
      ~attributed_to:(Proto.Actor_ref.uri (Proto.Actor.id actor))
      ?in_reply_to
      ?to_
      ?cc
      ?sensitive
      ?summary
      ~published
      ()
    in
    (* Build the Create activity *)
    let activity = Proto.Activity.make
      ~context:Proto.Context.default
      ~id:activity_id
      ~type_:Proto.Activity_type.Create
      ~actor:(Proto.Actor_ref.uri (Proto.Actor.id actor))
      ~object_:(Proto.Object_ref.obj note)
      ?to_
      ?cc
      ~published
      ()
    in
    (* Deliver to all recipients *)
    deliver t activity;
    activity

  let public_note t ~actor ?in_reply_to ~content () =
    let cc = match Proto.Actor.followers actor with
      | Some uri -> [Proto.Recipient.make uri]
      | None -> []
    in
    create_note t ~actor ?in_reply_to
      ~to_:[Proto.Recipient.make Proto.Public.id] ~cc ~content ()

  let followers_only_note t ~actor ?in_reply_to ~content () =
    let followers_uri =
      match Proto.Actor.followers actor with
      | Some uri -> uri
      | None -> raise (E (Error.Invalid_actor "Actor has no followers collection"))
    in
    create_note t ~actor ?in_reply_to
      ~to_:[Proto.Recipient.make followers_uri]
      ~content ()

  let direct_note t ~actor ~to_ ?in_reply_to ~content () =
    let recipients = List.map (fun a -> Proto.Recipient.make (Proto.Actor.id a)) to_ in
    create_note t ~actor ?in_reply_to ~to_:recipients ~content ()

  let like t ~actor ~object_ =
    let activity_id = generate_uri ~actor ~suffix:"likes" in
    let published = now_datetime () in
    (* Fetch the object to find its author for delivery *)
    let obj = Object.fetch t object_ in
    let to_recipients =
      match Proto.Object.attributed_to obj with
      | Some (Proto.Actor_ref.Uri uri) -> [Proto.Recipient.make uri]
      | Some (Proto.Actor_ref.Actor a) -> [Proto.Recipient.make (Proto.Actor.id a)]
      | None -> []
    in
    (* Build the Like activity *)
    let activity = Proto.Activity.make
      ~context:Proto.Context.default
      ~id:activity_id
      ~type_:Proto.Activity_type.Like
      ~actor:(Proto.Actor_ref.uri (Proto.Actor.id actor))
      ~object_:(Proto.Object_ref.uri object_)
      ~to_:to_recipients
      ~published
      ()
    in
    (* Deliver to the object's author *)
    deliver t activity;
    activity

  let unlike t ~actor ~object_ =
    let activity_id = generate_uri ~actor ~suffix:"undo" in
    let like_id = generate_uri ~actor ~suffix:"likes" in
    let published = now_datetime () in
    (* Fetch the object to find its author for delivery *)
    let obj = Object.fetch t object_ in
    let to_recipients =
      match Proto.Object.attributed_to obj with
      | Some (Proto.Actor_ref.Uri uri) -> [Proto.Recipient.make uri]
      | Some (Proto.Actor_ref.Actor a) -> [Proto.Recipient.make (Proto.Actor.id a)]
      | None -> []
    in
    (* Build the Undo(Like) activity - reference the Like by URI *)
    let activity = Proto.Activity.make
      ~context:Proto.Context.default
      ~id:activity_id
      ~type_:Proto.Activity_type.Undo
      ~actor:(Proto.Actor_ref.uri (Proto.Actor.id actor))
      ~object_:(Proto.Object_ref.uri like_id)
      ~to_:to_recipients
      ~published
      ()
    in
    (* Deliver to the object's author *)
    deliver t activity;
    activity

  let announce t ~actor ~object_ =
    let activity_id = generate_uri ~actor ~suffix:"announces" in
    let published = now_datetime () in
    (* Get actor's followers for cc *)
    let followers_uri = Proto.Actor.followers actor in
    let cc_recipients = match followers_uri with
      | Some uri -> [Proto.Recipient.make uri]
      | None -> []
    in
    (* Fetch the object to find its author for delivery *)
    let obj = Object.fetch t object_ in
    let author_recipients =
      match Proto.Object.attributed_to obj with
      | Some (Proto.Actor_ref.Uri uri) -> [Proto.Recipient.make uri]
      | Some (Proto.Actor_ref.Actor a) -> [Proto.Recipient.make (Proto.Actor.id a)]
      | None -> []
    in
    (* to: public, author; cc: followers *)
    let to_recipients = Proto.Recipient.make Proto.Public.id :: author_recipients in
    (* Build the Announce activity *)
    let activity = Proto.Activity.make
      ~context:Proto.Context.default
      ~id:activity_id
      ~type_:Proto.Activity_type.Announce
      ~actor:(Proto.Actor_ref.uri (Proto.Actor.id actor))
      ~object_:(Proto.Object_ref.uri object_)
      ~to_:to_recipients
      ~cc:cc_recipients
      ~published
      ()
    in
    (* Deliver to followers and the object's author *)
    deliver t activity;
    activity

  let unannounce t ~actor ~object_ =
    let activity_id = generate_uri ~actor ~suffix:"undo" in
    let announce_id = generate_uri ~actor ~suffix:"announces" in
    let published = now_datetime () in
    (* Get actor's followers for cc *)
    let followers_uri = Proto.Actor.followers actor in
    let cc_recipients = match followers_uri with
      | Some uri -> [Proto.Recipient.make uri]
      | None -> []
    in
    (* Fetch the object to find its author for delivery *)
    let obj = Object.fetch t object_ in
    let author_recipients =
      match Proto.Object.attributed_to obj with
      | Some (Proto.Actor_ref.Uri uri) -> [Proto.Recipient.make uri]
      | Some (Proto.Actor_ref.Actor a) -> [Proto.Recipient.make (Proto.Actor.id a)]
      | None -> []
    in
    let to_recipients = Proto.Recipient.make Proto.Public.id :: author_recipients in
    (* Build the Undo(Announce) activity *)
    let activity = Proto.Activity.make
      ~context:Proto.Context.default
      ~id:activity_id
      ~type_:Proto.Activity_type.Undo
      ~actor:(Proto.Actor_ref.uri (Proto.Actor.id actor))
      ~object_:(Proto.Object_ref.uri announce_id)
      ~to_:to_recipients
      ~cc:cc_recipients
      ~published
      ()
    in
    (* Deliver to followers and the object's author *)
    deliver t activity;
    activity

  let delete t ~actor ~object_ =
    let activity_id = generate_uri ~actor ~suffix:"deletes" in
    let published = now_datetime () in
    (* Fetch the original object to get its recipients *)
    let obj = Object.fetch t object_ in
    let to_recipients = Option.value ~default:[] (Proto.Object.to_ obj) in
    let cc_recipients = Option.value ~default:[] (Proto.Object.cc obj) in
    (* Create a Tombstone object *)
    let tombstone = Proto.Object.make
      ~id:object_
      ~type_:Proto.Object_type.Tombstone
      ~published
      ()
    in
    (* Build the Delete activity *)
    let activity = Proto.Activity.make
      ~context:Proto.Context.default
      ~id:activity_id
      ~type_:Proto.Activity_type.Delete
      ~actor:(Proto.Actor_ref.uri (Proto.Actor.id actor))
      ~object_:(Proto.Object_ref.obj tombstone)
      ~to_:to_recipients
      ~cc:cc_recipients
      ~published
      ()
    in
    (* Deliver to previous recipients *)
    deliver t activity;
    activity

  let update_note t ~actor ~object_ ~content () =
    let activity_id = generate_uri ~actor ~suffix:"updates" in
    let published = now_datetime () in
    (* Fetch the original note to preserve its metadata *)
    let original = Object.fetch t object_ in
    let to_recipients = Option.value ~default:[] (Proto.Object.to_ original) in
    let cc_recipients = Option.value ~default:[] (Proto.Object.cc original) in
    (* Create the updated Note object *)
    let updated_note = Proto.Object.make
      ~context:Proto.Context.default
      ~id:object_
      ~type_:Proto.Object_type.Note
      ~content
      ~attributed_to:(Proto.Actor_ref.uri (Proto.Actor.id actor))
      ?in_reply_to:(Proto.Object.in_reply_to original)
      ~to_:to_recipients
      ~cc:cc_recipients
      ?summary:(Proto.Object.summary original)
      ?sensitive:(Proto.Object.sensitive original)
      ~updated:published
      ?published:(Proto.Object.published original)
      ()
    in
    (* Build the Update activity *)
    let activity = Proto.Activity.make
      ~context:Proto.Context.default
      ~id:activity_id
      ~type_:Proto.Activity_type.Update
      ~actor:(Proto.Actor_ref.uri (Proto.Actor.id actor))
      ~object_:(Proto.Object_ref.obj updated_note)
      ~to_:to_recipients
      ~cc:cc_recipients
      ~published
      ()
    in
    (* Deliver to recipients *)
    deliver t activity;
    activity
end

module Collection = struct
  let fold t f init collection item_jsont =
    let seen = Hashtbl.create 16 in
    let rec pages acc = function
      | None -> acc
      | Some uri ->
          let key = Uriz.to_string uri in
          if Hashtbl.mem seen key then
            raise (E (Json_error ("Cyclic collection pagination: " ^ key)));
          Hashtbl.add seen key ();
          let page = Http.get_typed t (Proto.Collection_page.jsont item_jsont) uri in
          let items = Option.value ~default:[] (Proto.Collection_page.items page) in
          pages (List.fold_left f acc items) (Proto.Collection_page.next page)
    in
    let items = Option.value ~default:[] (Proto.Collection.items collection) in
    pages (List.fold_left f init items) (Proto.Collection.first collection)

  let iter t f collection item_jsont =
    fold t (fun () item -> f item) () collection item_jsont

  let to_list t collection item_jsont =
    fold t (fun acc item -> item :: acc) [] collection item_jsont
    |> List.rev

  let first_page t collection item_jsont =
    match Proto.Collection.first collection with
    | Some first_uri ->
        Some (Http.get_typed t (Proto.Collection_page.jsont item_jsont) first_uri)
    | None -> None

  let next_page t page item_jsont =
    match Proto.Collection_page.next page with
    | Some next_uri ->
        Some (Http.get_typed t (Proto.Collection_page.jsont item_jsont) next_uri)
    | None -> None
end
