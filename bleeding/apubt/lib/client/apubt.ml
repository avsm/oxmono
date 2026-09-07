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
    get_config : Fetch_signature.config;
    format : [ `Rfc9421 | `Cavage ];
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

  let create ?(format = `Rfc9421) ~key_id ~key () =
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
    let get_config = Fetch_signature.config ~key ?algorithm ~keyid:key_id
      ~components:Fetch_signature.Component.[method_; target_uri; date; content_digest] () in
    { key_id; key; config; get_config; format }

  let from_pem ?format ~key_id ~pem () =
    (* Parse PEM-encoded RSA private key *)
    match X509.Private_key.decode_pem pem with
    | Ok (`RSA priv) ->
        let key = Fetch_signature.Key.rsa ~priv in
        Ok (create ?format ~key_id ~key ())
    | Ok _ ->
        Error "Only RSA keys are supported for ActivityPub signatures"
    | Error (`Msg msg) ->
        Error ("Failed to parse PEM key: " ^ msg)

  let from_pem_exn ?format ~key_id ~pem () =
    match from_pem ?format ~key_id ~pem () with
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
  now : unit -> Ptime.t;
  new_id : actor:Uriz.t -> kind:string -> Uriz.t;
  persist : (Proto.Activity.t -> unit) option;
}

let activitypub_accept =
  "application/activity+json, application/ld+json; profile=\"https://www.w3.org/ns/activitystreams\""

let activitypub_media jsont =
  Fetch.Json.v ~media:"application/activity+json"
    ~accept:["application/ld+json"; "application/json"] jsont

let of_fetch ~clock ?signing ?(user_agent = "Apubt/0.1")
    ?(max_response_bytes = 16 * 1024 * 1024) ?id_generator ?persist fetch =
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
          ~format:signing.format ~config:signing.config fetch
  in
  let fetch = match signing with
    | None -> fetch
    | Some signing -> Fetch_signature.Middleware.sign ~clock ~key:signing.key
        ~format:signing.format ~digest_empty:true ~config:signing.get_config fetch in
  let now () = match Ptime.of_float_s (Eio.Time.now clock) with
    | Some now -> now
    | None -> invalid_arg "Apubt: clock outside Ptime range" in
  let new_id = match id_generator with
    | Some f -> f
    | None -> fun ~actor ~kind ->
        let bytes = Mirage_crypto_rng.generate 16 in
        let suffix = String.concat "" (List.init 16 (fun i -> Printf.sprintf "%02x" (Char.code bytes.[i]))) in
        let base = Uri.of_string (Uriz.to_string actor) in
        let path = Uri.path base ^ "/" ^ kind ^ "/" ^ suffix in
        Uri.with_path base path |> fun u -> Uri.with_query u []
        |> fun u -> Uri.with_fragment u None |> Uri.to_string |> Uriz.of_string_exn in
  { fetch; post_fetch; user_agent; max_response_bytes; now; new_id; persist }

let create ~sw ?signing ?user_agent ?max_response_bytes ?id_generator ?persist ?(timeout = 30.0) env =
  if not (Float.is_finite timeout) || timeout < 0.0 then
    invalid_arg "Apubt.create: timeout must be finite and non-negative";
  let duration = Duration.of_f timeout in
  let timeout = if timeout > 0.0 && duration = 0L then 1L else duration in
  let fetch = Fetch_curl.v ~sw ~timeout ~connect_timeout:timeout () in
  of_fetch ~clock:(Eio.Stdenv.clock env) ?signing ?user_agent ?max_response_bytes ?id_generator ?persist fetch

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
      | Some (`Date date) ->
          let buf = Bytes.of_string date in
          let i16 = Httpz.Buf_read.i16 in
          let span = Httpz.Span.make ~off:(i16 0) ~len:(i16 (Bytes.length buf)) in
          let now = Ptime.to_float_s (t.now ()) in
          let #(status, at) = Httpz.Date.parse ~now buf span in
          (match status with Httpz.Date.Valid ->
            Some (max 0. (Stdlib_upstream_compatible.Float_u.to_float at -. now))
          | Httpz.Date.Invalid -> None)
      | None -> None
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
        ?template:(Webfinger.Link.template link)
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
        if List.mem link.rel ["http://nodeinfo.diaspora.software/ns/schema/2.0";
                              "http://nodeinfo.diaspora.software/ns/schema/2.1"]
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

let decode_value codec json = match Jsont.Json.decode codec json with
  | Ok value -> value
  | Error error -> raise (E (Json_error error))

let dereference t codec = function
  | Proto.Reference.Uri uri -> Http.get_typed t codec uri
  | Proto.Reference.Embedded json -> decode_value codec json

let actor_id = function Proto.Actor_ref.Uri uri -> uri | Actor a -> Proto.Actor.id a

let activity_id t actor kind =
  if Option.is_none t.persist then
    raise (E (Invalid_actor "Generated activities require an Apubt persistence callback"));
  t.new_id ~actor:(Proto.Actor.id actor) ~kind
let now_datetime t = Proto.Datetime.v (Ptime.to_rfc3339 (t.now ()))
let persist t activity =
  if Option.is_none (Proto.Activity.id activity) then
    raise (E (Invalid_actor "Delivery requires an activity ID"));
  match t.persist with
  | Some save -> save activity
  | None -> raise (E (Invalid_actor "Generated activities require an Apubt persistence callback"))

module Collection = struct
  let fold ?(max_pages = 100) ?(max_items = 10000) t f init collection item_jsont =
    if max_pages < 0 || max_items < 0 then invalid_arg "Collection.fold: negative budget";
    let seen = Hashtbl.create 16 in
    let page_count = ref 0 and item_count = ref 0 in
    let items acc values = List.fold_left (fun acc value ->
      incr item_count;
      if !item_count > max_items then raise (E (Json_error "Collection item budget exceeded"));
      f acc value) acc (Option.value ~default:[] values) in
    let rec pages acc = function
      | None -> acc
      | Some reference ->
          incr page_count;
          if !page_count > max_pages then raise (E (Json_error "Collection page budget exceeded"));
          Option.iter (fun uri ->
            let key = Uriz.to_string uri in
            if Hashtbl.mem seen key then raise (E (Json_error ("Cyclic collection pagination: " ^ key)));
            Hashtbl.add seen key ()) (Proto.Reference.id reference);
          let page = dereference t (Proto.Collection_page.jsont item_jsont) reference in
          pages (items acc (Proto.Collection_page.items page)) (Proto.Collection_page.next page) in
    pages (items init (Proto.Collection.items collection)) (Proto.Collection.first collection)

  let iter ?max_pages ?max_items t f collection item_jsont =
    fold ?max_pages ?max_items t (fun () value -> f value) () collection item_jsont
  let to_list ?max_pages ?max_items t collection item_jsont =
    List.rev (fold ?max_pages ?max_items t (fun acc v -> v :: acc) [] collection item_jsont)
  let first_page t collection item_jsont =
    Option.map (dereference t (Proto.Collection_page.jsont item_jsont)) (Proto.Collection.first collection)
  let next_page t page item_jsont =
    Option.map (dereference t (Proto.Collection_page.jsont item_jsont)) (Proto.Collection_page.next page)
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
    match page with
    | Some uri -> Http.get_typed t Proto.Activity_collection_page.jsont uri
    | None -> (match Collection.first_page t (outbox t actor) Proto.Activity.jsont with
        | Some page -> page
        | None -> raise (E (Invalid_actor "Outbox has no first page")))

  let followers t actor =
    match Proto.Actor.followers actor with
    | Some uri -> Http.get_typed t (Proto.Collection.jsont Proto.Actor_ref.jsont) uri
    | None -> raise (E (Invalid_actor "Actor has no followers collection"))

  let following t actor =
    match Proto.Actor.following actor with
    | Some uri -> Http.get_typed t (Proto.Collection.jsont Proto.Actor_ref.jsont) uri
    | None -> raise (E (Invalid_actor "Actor has no following collection"))

  (* Helper to post activity to an actor's inbox *)
  let post_to_inbox t actor activity =
    let inbox_uri = Proto.Actor.inbox actor in
    Http.post_typed t Proto.Activity.jsont inbox_uri activity

  let follow t ~actor ~target =
    let activity = Proto.Activity.make ~context:Proto.Context.default
      ~id:(activity_id t actor "follows") ~published:(now_datetime t)
      ~type_:Follow ~actor:(Proto.Actor_ref.uri (Proto.Actor.id actor))
      ~object_:(Proto.Object_ref.uri (Proto.Actor.id target))
      ~to_:[Proto.Recipient.make (Proto.Actor.id target)] () in
    persist t activity;
    post_to_inbox t target activity;
    activity

  let validate_follow ~actor ~incoming follow =
    if Proto.Activity.type_ follow <> Follow then
      raise (E (Invalid_actor "Expected a Follow activity"));
    let sender = actor_id (Proto.Activity.actor follow) in
    let target = Option.bind (Proto.Activity.object_ follow) Proto.Reference.id in
    let expected = Proto.Actor.id actor in
    let matches = if incoming then Option.fold ~none:false ~some:(fun uri -> Uriz.equal expected uri) target
      else Uriz.equal expected sender in
    if not matches then raise (E (Invalid_actor "Follow actor/target does not match the local actor"))

  let unfollow t ~actor ~follow =
    validate_follow ~actor ~incoming:false follow;
    let target_uri = match Option.bind (Proto.Activity.object_ follow) Proto.Reference.id with
      | Some uri -> uri | None -> raise (E (Invalid_actor "Follow has no target")) in
    let target = fetch t target_uri in
    let activity = Proto.Activity.make ~context:Proto.Context.default
      ~id:(activity_id t actor "undo") ~published:(now_datetime t)
      ~type_:Undo ~actor:(Proto.Actor_ref.uri (Proto.Actor.id actor))
      ~object_:(Proto.Reference.of_value Proto.Activity.jsont follow)
      ~to_:[Proto.Recipient.make target_uri] () in
    persist t activity;
    post_to_inbox t target activity;
    activity

  let respond_follow t ~actor ~follow type_ =
    validate_follow ~actor ~incoming:true follow;
    let follower_uri = actor_id (Proto.Activity.actor follow) in
    let follower = fetch t follower_uri in
    let activity = Proto.Activity.make ~context:Proto.Context.default
      ~id:(activity_id t actor "activities") ~published:(now_datetime t)
      ~type_ ~actor:(Proto.Actor_ref.uri (Proto.Actor.id actor))
      ~object_:(Proto.Reference.of_value Proto.Activity.jsont follow)
      ~to_:[Proto.Recipient.make follower_uri] () in
    persist t activity;
    post_to_inbox t follower activity;
    activity

  let accept_follow t ~actor ~follow = respond_follow t ~actor ~follow Accept
  let reject_follow t ~actor ~follow = respond_follow t ~actor ~follow Reject

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
    with E Not_found -> None

  let post_to_shared_inbox t ~host activity =
    match discover_shared_inbox t ~host with
    | Some shared_inbox ->
        post t ~inbox:shared_inbox activity
    | None -> raise (E (Invalid_actor "Instance actor does not advertise a shared inbox"))
end

module Outbox = struct
  let generate_uri t ~actor ~suffix = activity_id t actor suffix

  let resolve_recipient_inboxes ?(max_depth = 4) ?(max_recipients = 10000) t ~actor recipients =
    let seen = Hashtbl.create 32 and inboxes = Hashtbl.create 32 in
    let count = ref 0 in
    let rec resolve depth reference =
      if depth > max_depth then raise (E (Invalid_actor "Recipient collection depth exceeded"));
      let id = Proto.Reference.id reference in
      let skip = match id with
        | Some id -> Uriz.equal id Proto.Public.id || Uriz.equal id (Proto.Actor.id actor)
          || Hashtbl.mem seen (Uriz.to_string id)
        | None -> false in
      if not skip then begin
        incr count;
        if !count > max_recipients then raise (E (Invalid_actor "Recipient budget exceeded"));
        Option.iter (fun id -> Hashtbl.add seen (Uriz.to_string id) ()) id;
        let json = dereference t Jsont.json reference in
        let type_ = decode_value (Jsont.mem "type" Jsont.string) json in
        if type_ = "Collection" || type_ = "OrderedCollection" then
          let collection = decode_value (Proto.Collection.jsont Proto.Reference.jsont) json in
          Collection.iter ~max_items:max_recipients t (resolve (depth + 1)) collection Proto.Reference.jsont
        else
          let recipient = decode_value Proto.Actor.jsont json in
          if not (Uriz.equal (Proto.Actor.id recipient) (Proto.Actor.id actor)) then
            let inbox = Proto.Actor.inbox recipient in
            Hashtbl.replace inboxes (Uriz.to_string inbox) inbox
      end in
    List.iter (fun recipient -> resolve 0 (Proto.Reference.uri (Proto.Recipient.id recipient))) recipients;
    Hashtbl.to_seq_values inboxes |> List.of_seq |> List.sort (fun a b -> Uriz.compare a b)

  let deliver t ~actor activity =
    if not (Uriz.equal (actor_id (Proto.Activity.actor activity)) (Proto.Actor.id actor)) then
      raise (E (Invalid_actor "Delivery actor must match the activity author"));
    (* Persist before any delivery; callers can retry this same activity ID. *)
    persist t activity;
    let recipients = List.concat_map (Option.value ~default:[])
      [Proto.Activity.to_ activity; Proto.Activity.cc activity;
       Proto.Activity.bto activity; Proto.Activity.bcc activity; Proto.Activity.audience activity] in
    let inboxes = resolve_recipient_inboxes t ~actor recipients in
    let json = match Jsont.Json.encode Proto.Activity.jsont activity with
      | Ok json -> json | Error msg -> raise (E (Json_error msg)) in
    let rec redact = function
      | Jsont.Object (members, meta) -> Jsont.Object
          (List.filter_map (fun ((name, _) as key, value) ->
             if name = "bto" || name = "bcc" then None
             else Some (key, redact value)) members, meta)
      | Jsont.Array (values, meta) -> Jsont.Array (List.map redact values, meta)
      | json -> json in
    let json = redact json in
    List.iter (fun inbox -> Http.post t inbox json) inboxes

  let create_note t ~actor ?in_reply_to ?to_ ?cc ?sensitive ?summary ~content () =
    let note_id = generate_uri t ~actor ~suffix:"notes" in
    let activity_id = generate_uri t ~actor ~suffix:"activities" in
    let published = now_datetime t in
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
    deliver t ~actor activity;
    activity

  let public_note t ~actor ?in_reply_to ?sensitive ?summary ~content () =
    let cc = match Proto.Actor.followers actor with
      | Some uri -> [Proto.Recipient.make uri]
      | None -> []
    in
    create_note t ~actor ?in_reply_to
      ~to_:[Proto.Recipient.make Proto.Public.id] ~cc ?sensitive ?summary ~content ()

  let followers_only_note t ~actor ?in_reply_to ?sensitive ?summary ~content () =
    let followers_uri =
      match Proto.Actor.followers actor with
      | Some uri -> uri
      | None -> raise (E (Error.Invalid_actor "Actor has no followers collection"))
    in
    create_note t ~actor ?in_reply_to
      ~to_:[Proto.Recipient.make followers_uri]
      ?sensitive ?summary ~content ()

  let direct_note t ~actor ~to_ ?in_reply_to ~content () =
    let recipients = List.map (fun a -> Proto.Recipient.make (Proto.Actor.id a)) to_ in
    create_note t ~actor ?in_reply_to ~to_:recipients ~content ()

  let like t ~actor ~object_ =
    let activity_id = generate_uri t ~actor ~suffix:"likes" in
    let published = now_datetime t in
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
    deliver t ~actor activity;
    activity

  let undo t ~actor ~activity expected =
    if Proto.Activity.type_ activity <> expected ||
       not (Uriz.equal (actor_id (Proto.Activity.actor activity)) (Proto.Actor.id actor)) then
      raise (E (Invalid_actor "Undo requires the original activity by this actor"));
    if Option.is_none (Proto.Activity.id activity) then
      raise (E (Invalid_actor "Undo requires the original activity ID"));
    let undo = Proto.Activity.make ~context:Proto.Context.default
      ~id:(activity_id t actor "undo") ~published:(now_datetime t)
      ~type_:Undo ~actor:(Proto.Actor_ref.uri (Proto.Actor.id actor))
      ~object_:(Proto.Reference.of_value Proto.Activity.jsont activity)
      ?to_:(Proto.Activity.to_ activity) ?cc:(Proto.Activity.cc activity)
      ?bto:(Proto.Activity.bto activity) ?bcc:(Proto.Activity.bcc activity)
      ?audience:(Proto.Activity.audience activity) () in
    deliver t ~actor undo;
    undo

  let unlike t ~actor ~like = undo t ~actor ~activity:like Like

  let announce t ~actor ~object_ =
    let activity_id = generate_uri t ~actor ~suffix:"announces" in
    let published = now_datetime t in
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
    deliver t ~actor activity;
    activity

  let unannounce t ~actor ~announce = undo t ~actor ~activity:announce Announce

  let require_author actor obj =
    if not (Option.fold ~none:false
      ~some:(fun author -> Uriz.equal (actor_id author) (Proto.Actor.id actor))
      (Proto.Object.attributed_to obj)) then
      raise (E (Invalid_actor "Only the author can update or delete an object"))

  let delete t ~actor ~object_ =
    let activity_id = generate_uri t ~actor ~suffix:"deletes" in
    let published = now_datetime t in
    (* Fetch the original object to get its recipients *)
    let obj = Object.fetch t object_ in
    require_author actor obj;
    let to_recipients = Option.value ~default:[] (Proto.Object.to_ obj) in
    let cc_recipients = Option.value ~default:[] (Proto.Object.cc obj) in
    (* Create a Tombstone object *)
    let tombstone = Proto.Object.make
      ~id:object_
      ~type_:Proto.Object_type.Tombstone
      ~deleted:published
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
      ?bto:(Proto.Object.bto obj) ?bcc:(Proto.Object.bcc obj)
      ?audience:(Proto.Object.audience obj)
      ~published
      ()
    in
    (* Deliver to previous recipients *)
    deliver t ~actor activity;
    activity

  let update_note t ~actor ~object_ ~content () =
    let activity_id = generate_uri t ~actor ~suffix:"updates" in
    let published = now_datetime t in
    (* Fetch the original note to preserve its metadata *)
    let original = Object.fetch t object_ in
    let to_recipients = Option.value ~default:[] (Proto.Object.to_ original) in
    let cc_recipients = Option.value ~default:[] (Proto.Object.cc original) in
    (* Create the updated Note object *)
    require_author actor original;
    if Proto.Object.type_ original <> Note then
      raise (E (Invalid_actor "update_note requires a Note"));
    let updated_note = Proto.Object.with_content ~updated:published content original in
    (* Build the Update activity *)
    let activity = Proto.Activity.make
      ~context:Proto.Context.default
      ~id:activity_id
      ~type_:Proto.Activity_type.Update
      ~actor:(Proto.Actor_ref.uri (Proto.Actor.id actor))
      ~object_:(Proto.Object_ref.obj updated_note)
      ~to_:to_recipients
      ~cc:cc_recipients
      ?bto:(Proto.Object.bto original) ?bcc:(Proto.Object.bcc original)
      ?audience:(Proto.Object.audience original)
      ~published
      ()
    in
    (* Deliver to recipients *)
    deliver t ~actor activity;
    activity
end
