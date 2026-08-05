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
    key : Requests.Signature.Key.t;
    config : Requests.Signature.config;
  }

  (** ActivityPub signing components: @method, @authority, @path, date, digest, content-type *)
  let activitypub_components =
    Requests.Signature.Component.[
      method_;
      authority;
      path;
      date;
      content_digest;
      content_type;
    ]

  let create ~key_id ~key () =
    let config = Requests.Signature.config
      ~key
      ~keyid:key_id
      ~components:activitypub_components
      ()
    in
    { key_id; key; config }

  let from_pem ~key_id ~pem () =
    (* Parse PEM-encoded RSA private key *)
    match X509.Private_key.decode_pem pem with
    | Ok (`RSA priv) ->
        let key = Requests.Signature.Key.rsa ~priv in
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

type t = T : {
  requests : Requests.t;
  clock : _ Eio.Time.clock;
  signing : Signing.t option;
  user_agent : string;
} -> t

let activitypub_accept =
  "application/activity+json, application/ld+json; profile=\"https://www.w3.org/ns/activitystreams\""

let create ~sw ?signing ?(user_agent = "Apubt/0.1") ?(timeout = 30.0) env =
  let timeout_config = Requests.Timeout.create ~connect:timeout ~read:timeout () in
  let default_headers =
    Requests.Headers.empty
    |> Requests.Headers.add `Accept activitypub_accept
    |> Requests.Headers.add `User_agent user_agent
  in
  let requests = Requests.create ~sw ~default_headers ~timeout:timeout_config env in
  let clock = Eio.Stdenv.clock env in
  T { requests; clock; signing; user_agent }

let user_agent (T t) = t.user_agent

(* Internal: check HTTP response for errors *)
let check_response resp =
  let status = Requests.Response.status_code resp in
  if status >= 200 && status < 300 then ()
  else if status = 404 then raise (E Not_found)
  else if status = 401 || status = 403 then raise (E Unauthorized)
  else if status = 429 then begin
    let retry_after =
      Requests.Response.headers resp
      |> Requests.Headers.get `Retry_after
      |> Option.map float_of_string
    in
    raise (E (Rate_limited retry_after))
  end
  else begin
    let body = Requests.Response.text resp in
    raise (E (Http_error (status, body)))
  end

module Http = struct
  let get (T t) uri =
    let url = Uri.to_string uri in
    let resp = Requests.get t.requests url in
    check_response resp;
    Requests.Response.json resp

  let get_typed (T t) jsont uri =
    let url = Uri.to_string uri in
    let resp = Requests.get t.requests url in
    check_response resp;
    Requests.Response.jsonv jsont resp

  (* Internal: sign a POST request if signing is configured *)
  let sign_post_request (T t) ~uri ~body ~headers =
    match t.signing with
    | None -> headers
    | Some signing ->
        (* Add Date header using the session clock *)
        let now_float = Eio.Time.now t.clock in
        let now = Ptime.of_float_s now_float |> Option.get in
        let date_str = Requests.Headers.http_date_of_ptime now in
        let headers = Requests.Headers.set `Date date_str headers in
        (* Create request context for signing *)
        let ctx = Requests.Signature.Context.request
          ~method_:`POST
          ~uri
          ~headers
        in
        (* Sign with digest (adds Content-Digest header and signs) *)
        match Requests.Signature.sign_with_digest
          ~clock:t.clock
          ~config:signing.config
          ~context:ctx
          ~headers
          ~body
          ~digest_algorithm:`Sha256
        with
        | Ok signed_headers -> signed_headers
        | Error err ->
            let msg = Requests.Signature.sign_error_to_string err in
            raise (E (Signature_error msg))

  (* Helper to encode JSON to string, raising on error *)
  let encode_json_exn jsont value =
    match Jsont_bytesrw.encode_string jsont value with
    | Ok s -> s
    | Error msg -> raise (E (Json_error msg))

  let post (T t as client) uri body =
    let url = Uri.to_string uri in
    let body_str = encode_json_exn Jsont.json body in
    let headers =
      Requests.Headers.empty
      |> Requests.Headers.set `Content_type "application/activity+json"
    in
    let headers = sign_post_request client ~uri ~body:body_str ~headers in
    let resp = Requests.post t.requests ~headers ~body:(Requests.Body.of_string Requests.Mime.json body_str) url in
    check_response resp

  let post_typed (T t as client) jsont uri value =
    let url = Uri.to_string uri in
    let body_str = encode_json_exn jsont value in
    let headers =
      Requests.Headers.empty
      |> Requests.Headers.set `Content_type "application/activity+json"
    in
    let headers = sign_post_request client ~uri ~body:body_str ~headers in
    let resp = Requests.post t.requests ~headers ~body:(Requests.Body.of_string Requests.Mime.json body_str) url in
    check_response resp
end

module Webfinger = struct
  (** Convert a webfinger library Jrd to our internal Proto.Webfinger type *)
  let jrd_of_webfinger (jrd : Webfinger.Jrd.t) : Proto.Webfinger.t =
    let links = List.map (fun (link : Webfinger.Link.t) ->
      Proto.Webfinger.Jrd_link.make
        ~rel:(Webfinger.Link.rel link)
        ?type_:(Webfinger.Link.type_ link)
        ?href:(Option.map Uri.of_string (Webfinger.Link.href link))
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

  let lookup (T t) acct =
    (* Parse the account string into an Acct.t *)
    let acct_uri =
      (* Handle both "user@domain" and "acct:user@domain" formats *)
      let acct_str =
        if String.starts_with ~prefix:"acct:" acct then acct
        else "acct:" ^ acct
      in
      match Webfinger.Acct.of_string acct_str with
      | Ok a -> a
      | Error e -> raise (E (Webfinger_error (Webfinger.error_to_string e)))
    in
    (* Use the webfinger library's query function *)
    match Webfinger.query_acct t.requests acct_uri () with
    | Ok jrd -> jrd_of_webfinger jrd
    | Error e -> raise (E (Webfinger_error (Webfinger.error_to_string e)))

  (** Look up using webfinger library and return the raw Webfinger.Jrd.t *)
  let lookup_raw (T t) acct =
    let acct_uri =
      let acct_str =
        if String.starts_with ~prefix:"acct:" acct then acct
        else "acct:" ^ acct
      in
      match Webfinger.Acct.of_string acct_str with
      | Ok a -> a
      | Error e -> raise (E (Webfinger_error (Webfinger.error_to_string e)))
    in
    match Webfinger.query_acct t.requests acct_uri () with
    | Ok jrd -> jrd
    | Error e -> raise (E (Webfinger_error (Webfinger.error_to_string e)))

  let actor_uri jrd =
    match Proto.Webfinger.links jrd with
    | None -> None
    | Some links ->
        List.find_map (fun link ->
          if Proto.Webfinger.Jrd_link.rel link = Webfinger.Rel.activitypub then
            match Proto.Webfinger.Jrd_link.type_ link with
            | Some t when String.equal t "application/activity+json" ->
                Proto.Webfinger.Jrd_link.href link
            | Some t when String.starts_with ~prefix:"application/ld+json" t ->
                Proto.Webfinger.Jrd_link.href link
            | _ -> None
          else None
        ) links

  (** Extract ActivityPub actor URI from a raw Webfinger.Jrd.t *)
  let actor_uri_raw (jrd : Webfinger.Jrd.t) : Uri.t option =
    (* Look for self link with ActivityPub media type *)
    match Webfinger.Jrd.find_link ~rel:Webfinger.Rel.activitypub jrd with
    | Some link ->
        (match Webfinger.Link.type_ link with
         | Some t when String.equal t "application/activity+json" ->
             Option.map Uri.of_string (Webfinger.Link.href link)
         | Some t when String.starts_with ~prefix:"application/ld+json" t ->
             Option.map Uri.of_string (Webfinger.Link.href link)
         | _ -> None)
    | None -> None

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

  let fetch (T t) ~host =
    (* Step 1: Fetch the well-known nodeinfo discovery document *)
    let well_known_url = Printf.sprintf "https://%s/.well-known/nodeinfo" host in
    let headers =
      Requests.Headers.empty
      |> Requests.Headers.add `Accept "application/json"
    in
    let resp = Requests.get t.requests ~headers well_known_url in
    check_response resp;
    let well_known = Requests.Response.jsonv Well_known.jsont resp in
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
        let resp = Requests.get t.requests ~headers href in
        check_response resp;
        Requests.Response.jsonv Proto.Nodeinfo.jsont resp

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

  let discover_shared_inbox (T t) ~host =
    (* Try to get shared inbox from instance actor endpoint *)
    let instance_actor_url = Printf.sprintf "https://%s/actor" host in
    try
      let resp = Requests.get t.requests instance_actor_url in
      if Requests.Response.status_code resp >= 200 &&
         Requests.Response.status_code resp < 300 then begin
        let actor = Requests.Response.jsonv Proto.Actor.jsont resp in
        match Proto.Actor.endpoints actor with
        | Some endpoints ->
            Proto.Endpoints.shared_inbox endpoints
        | None -> None
      end else
        None
    with _ ->
      (* If fetching instance actor fails, there's no shared inbox *)
      None

  let post_to_shared_inbox t ~host activity =
    match discover_shared_inbox t ~host with
    | Some shared_inbox ->
        post t ~inbox:shared_inbox activity
    | None ->
        (* Fallback: construct a standard shared inbox URL *)
        let shared_inbox = Uri.of_string (Printf.sprintf "https://%s/inbox" host) in
        post t ~inbox:shared_inbox activity
end

module Outbox = struct
  (* Generate a unique URI for a new object/activity based on actor's base URI.
     Uses timestamp + random suffix for uniqueness. *)
  let generate_uri ~actor ~suffix =
    let actor_uri = Uri.to_string (Proto.Actor.id actor) in
    let now = Ptime_clock.now () in
    let ts = Ptime.to_float_s now |> int_of_float in
    let rand = Random.bits () land 0xFFFFFF in
    let unique_id = Printf.sprintf "%d-%06x" ts rand in
    Uri.of_string (actor_uri ^ "/" ^ suffix ^ "/" ^ unique_id)

  (* Get the current timestamp as an ISO 8601 string *)
  let now_datetime () =
    let now = Ptime_clock.now () in
    Proto.Datetime.v (Ptime.to_rfc3339 now)

  (* Extract inbox URIs from a list of recipients, resolving actors as needed *)
  let resolve_recipient_inboxes t recipients =
    List.filter_map (fun recipient ->
      let uri = Proto.Recipient.id recipient in
      let uri_str = Uri.to_string uri in
      (* Skip the public collection - it doesn't have an inbox *)
      if String.equal uri_str (Uri.to_string Proto.Public.id) then
        None
      else begin
        (* Try to fetch the actor to get their inbox *)
        try
          let actor = Actor.fetch t uri in
          Some (Proto.Actor.inbox actor)
        with E _ ->
          (* If we can't fetch the actor, skip this recipient *)
          None
      end
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
      let uri_str = Uri.to_string inbox in
      if Hashtbl.mem seen uri_str then false
      else begin
        Hashtbl.add seen uri_str ();
        true
      end
    ) inboxes in
    (* Post to each inbox *)
    List.iter (fun inbox ->
      try
        Inbox.post t ~inbox activity
      with E _ ->
        (* Log delivery failures but don't fail the whole operation *)
        ()
    ) unique_inboxes

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
    let followers_uri =
      match Proto.Actor.followers actor with
      | Some uri -> uri
      | None -> Uri.of_string ""
    in
    create_note t ~actor ?in_reply_to
      ~to_:[Proto.Recipient.make Proto.Public.id]
      ~cc:[Proto.Recipient.make followers_uri]
      ~content ()

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
  let rec iter t f collection item_jsont =
    (* Process items in current collection if any *)
    (match Proto.Collection.items collection with
     | Some items -> List.iter f items
     | None -> ());
    (* Fetch first page if available *)
    match Proto.Collection.first collection with
    | Some first_uri ->
        let page = Http.get_typed t (Proto.Collection_page.jsont item_jsont) first_uri in
        iter_page t f page item_jsont
    | None -> ()

  and iter_page t f page item_jsont =
    (* Process items in page *)
    (match Proto.Collection_page.items page with
     | Some items -> List.iter f items
     | None -> ());
    (* Fetch next page if available *)
    match Proto.Collection_page.next page with
    | Some next_uri ->
        let next = Http.get_typed t (Proto.Collection_page.jsont item_jsont) next_uri in
        iter_page t f next item_jsont
    | None -> ()

  let rec fold t f init collection item_jsont =
    (* Fold over items in current collection *)
    let acc = match Proto.Collection.items collection with
      | Some items -> List.fold_left f init items
      | None -> init
    in
    (* Fetch first page if available *)
    match Proto.Collection.first collection with
    | Some first_uri ->
        let page = Http.get_typed t (Proto.Collection_page.jsont item_jsont) first_uri in
        fold_page t f acc page item_jsont
    | None -> acc

  and fold_page t f acc page item_jsont =
    (* Fold over items in page *)
    let acc = match Proto.Collection_page.items page with
      | Some items -> List.fold_left f acc items
      | None -> acc
    in
    (* Fetch next page if available *)
    match Proto.Collection_page.next page with
    | Some next_uri ->
        let next = Http.get_typed t (Proto.Collection_page.jsont item_jsont) next_uri in
        fold_page t f acc next item_jsont
    | None -> acc

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
