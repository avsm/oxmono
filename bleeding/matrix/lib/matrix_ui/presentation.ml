module Event = Matrix_proto.Event
module Id = Matrix_proto.Id

module Html = struct
  let allowed_elements =
    [
      "a";
      "b";
      "blockquote";
      "br";
      "caption";
      "code";
      "del";
      "details";
      "div";
      "em";
      "font";
      "h1";
      "h2";
      "h3";
      "h4";
      "h5";
      "h6";
      "hr";
      "i";
      "img";
      "li";
      "ol";
      "p";
      "pre";
      "s";
      "span";
      "strike";
      "strong";
      "sub";
      "summary";
      "sup";
      "table";
      "tbody";
      "td";
      "th";
      "thead";
      "tr";
      "u";
      "ul";
    ]

  let is_allowed_element name = List.mem name allowed_elements

  let safe_href value =
    match Uriz.of_string value with
    | Null -> false
    | This uri -> (
        match Uriz.scheme uri with
        | Null -> false
        | This scheme -> List.mem scheme [ "https"; "http"; "mailto"; "matrix" ]
        )

  (* [<img src>] is the one attribute whose value the spec pins to a scheme of
     its own: only an [mxc://] URI may stand there. A toolkit that can turn one
     into an HTTP URL passes [?resolve_mxc]; returning [None] from it drops the
     image, which is also what happens when the [src] is missing or is not an
     [mxc://] URI. *)
  let mxc_src ~resolve_mxc value =
    match Uriz.of_string value with
    | Null -> None
    | This uri -> (
        match Uriz.scheme uri with
        | This "mxc" -> (
            match resolve_mxc with
            | None -> Some value
            | Some resolve -> resolve value)
        | This _ | Null -> None)

  let image_dimension value =
    let length = String.length value in
    if length = 0 then None
    else
      let rec loop index number =
        if index = length then
          if number = 0 then None else Some (string_of_int number)
        else
          let code = Char.code value.[index] in
          if code < Char.code '0' || code > Char.code '9' then None
          else if number > 1_638 || (number = 1_638 && code > Char.code '4')
          then None
          else
            let number = (number * 10) + (code - Char.code '0') in
            if number > 16_384 then None else loop (index + 1) number
      in
      loop 0 0

  let matrix_color value =
    let length = String.length value in
    let valid_length = length = 4 || length = 5 || length = 7 || length = 9 in
    let is_hex c =
      (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F')
    in
    if valid_length && value.[0] = '#' then
      let rec loop index =
        index = length || (is_hex value.[index] && loop (index + 1))
      in
      if loop 1 then Some value else None
    else None

  let keep_attribute ~resolve_mxc element ((_, name), value) =
    match (element, String.lowercase_ascii name) with
    | "a", "href" when safe_href value -> Some (("", "href"), value)
    | "a", "title" -> Some (("", "title"), value)
    | "code", "class" when String.starts_with ~prefix:"language-" value ->
        Some (("", "class"), value)
    | ("span" | "font"), ("data-mx-color" | "data-mx-bg-color") ->
        Option.map (fun value -> (("", name), value)) (matrix_color value)
    | "img", "src" ->
        Option.map (fun src -> (("", "src"), src)) (mxc_src ~resolve_mxc value)
    | "img", (("alt" | "title") as attribute) -> Some (("", attribute), value)
    | "img", (("width" | "height") as attribute) ->
        Option.map
          (fun value -> (("", attribute), value))
          (image_dimension value)
    | _ -> None

  let has_src attributes =
    List.exists (fun ((_, name), _) -> String.equal name "src") attributes

  let sanitized_signals ?resolve_mxc html =
    let input =
      Markup.string html
      |> Markup.parse_html ~context:(`Fragment "div")
      |> Markup.signals
    in
    let stack = Stack.create () in
    let suppressed_depth = ref 0 in
    let suppress_contents = function
      | "script" | "style" | "iframe" | "object" | "embed" | "mx-reply" -> true
      | _ -> false
    in
    let rec next () =
      match Markup.next input with
      | None -> None
      | Some (`Start_element ((_, raw_name), attributes)) ->
          let name = String.lowercase_ascii raw_name in
          let suppressed = !suppressed_depth > 0 || suppress_contents name in
          if suppressed then incr suppressed_depth;
          let attributes =
            List.filter_map (keep_attribute ~resolve_mxc name) attributes
          in
          let keep =
            (not suppressed) && is_allowed_element name
            && ((not (String.equal name "img")) || has_src attributes)
          in
          Stack.push (keep, suppressed) stack;
          if keep then
            Some (`Start_element ((Markup.Ns.html, name), attributes))
          else next ()
      | Some `End_element ->
          let keep, suppressed =
            if Stack.is_empty stack then (false, false) else Stack.pop stack
          in
          if suppressed then decr suppressed_depth;
          if keep then Some `End_element else next ()
      | Some (`Text _ as signal) ->
          if !suppressed_depth = 0 then Some signal else next ()
      | Some (`Comment _ | `Doctype _ | `Xml _ | `PI _) -> next ()
    in
    Markup.stream next

  let sanitize ?resolve_mxc html =
    sanitized_signals ?resolve_mxc html |> Markup.write_html |> Markup.to_string

  let is_block = function
    | "blockquote" | "br" | "div" | "h1" | "h2" | "h3" | "h4" | "h5" | "h6"
    | "hr" | "li" | "ol" | "p" | "pre" | "table" | "tr" | "ul" ->
        true
    | _ -> false

  let to_plain html =
    let input =
      Markup.string html
      |> Markup.parse_html ~context:(`Fragment "div")
      |> Markup.signals
    in
    let buffer = Buffer.create (String.length html) in
    let stack = Stack.create () in
    let skip = ref 0 in
    let newline () =
      if
        Buffer.length buffer > 0
        && Buffer.nth buffer (Buffer.length buffer - 1) <> '\n'
      then Buffer.add_char buffer '\n'
    in
    let () =
      Markup.fold
        (fun () signal ->
          match signal with
          | `Start_element ((_, raw_name), _) ->
              let name = String.lowercase_ascii raw_name in
              Stack.push name stack;
              if name = "mx-reply" then incr skip
              else if !skip = 0 && is_block name then newline ()
          | `End_element ->
              if not (Stack.is_empty stack) then
                let name = Stack.pop stack in
                if name = "mx-reply" then decr skip
                else if !skip = 0 && is_block name then newline ()
          | `Text strings when !skip = 0 ->
              List.iter (fun s -> Buffer.add_string buffer s) strings
          | `Text _ | `Comment _ | `Doctype _ | `Xml _ | `PI _ -> ())
        () input
    in
    Buffer.contents buffer |> String.trim
end

type relation_kind =
  | Reply
  | Replacement
  | Annotation of string
  | Thread
  | Reference
  | Custom_relation of string

type relation = { target : Id.Event_id.t; kind : relation_kind }

type message_kind =
  | Text
  | Notice
  | Emote
  | Image
  | File
  | Audio
  | Video
  | Location
  | Verification_request
  | Custom_message of string

type formatted_body = { html : string; plain : string }

type message = {
  kind : message_kind;
  body : string;
  formatted : formatted_body option;
  filename : string option;
  url : string option;
  info : Jsont.json option;
}

type membership_change =
  | Joined
  | Left
  | Kicked
  | Banned
  | Kicked_and_banned
  | Unbanned
  | Invited
  | Invitation_accepted
  | Invitation_rejected
  | Invitation_revoked
  | Knocked
  | Knock_accepted
  | Knock_denied
  | Knock_retracted
  | No_change
  | Invalid
  | Unknown_membership

type 'a change = { previous : 'a; current : 'a }

type profile_change = {
  displayname : string option change option;
  avatar_url : string option change option;
}

type other_state =
  | Room_create
  | Room_name of string option
  | Room_topic of string option
  | Room_avatar of string option
  | Room_canonical_alias of string option
  | Room_encryption
  | Room_pinned_events
  | Room_tombstone of string option
  | Room_power_levels
  | Room_join_rules
  | Room_history_visibility
  | Room_guest_access
  | Room_server_acl
  | Room_third_party_invite of string option
  | Policy_rule of string
  | Space_child
  | Space_parent
  | Beacon_info of Event.Beacon_info_content.t
  | Other_state_type of string

type content =
  | Message of message
  | Sticker of { body : string; url : string option; info : Jsont.json option }
  | Reaction of { key : string; target : Id.Event_id.t }
  | Redaction of { target : Id.Event_id.t option; reason : string option }
  | Poll of { text : string }
  | Membership of {
      user : Id.User_id.t;
      change : membership_change;
      reason : string option;
    }
  | Profile of { user : Id.User_id.t; change : profile_change }
  | State of { event_type : string; state_key : string; state : other_state }
  | Unable_to_decrypt
  | Custom of { event_type : string; content : Jsont.json }
  | Malformed of { event_type : string; reason : string }

type t = {
  event_id : Id.Event_id.t option;
  sender : Id.User_id.t;
  timestamp : Event.Timestamp.t;
  relation : relation option;
  content : content;
  raw : Event.Raw_event.t;
}

let as_string = function Jsont.String (value, _) -> Some value | _ -> None
let as_object = function Jsont.Object (members, _) -> Some members | _ -> None

let member name json =
  match as_object json with
  | None -> None
  | Some members -> (
      match Jsont.Json.find_mem name members with
      | None -> None
      | Some (_, value) -> Some value)

let string_member name json = Option.bind (member name json) as_string

let event_id value =
  match Id.Event_id.of_string value with Ok id -> Some id | Error _ -> None

let relation_of_content content =
  match member "m.relates_to" content with
  | None -> None
  | Some relates_to -> (
      let direct_target =
        Option.bind (string_member "event_id" relates_to) event_id
      in
      match string_member "rel_type" relates_to with
      | Some "m.replace" ->
          Option.map
            (fun target -> { target; kind = Replacement })
            direct_target
      | Some "m.annotation" ->
          Option.bind direct_target (fun target ->
              Option.map
                (fun key -> { target; kind = Annotation key })
                (string_member "key" relates_to))
      | Some "m.thread" ->
          Option.map (fun target -> { target; kind = Thread }) direct_target
      | Some "m.reference" ->
          Option.map (fun target -> { target; kind = Reference }) direct_target
      | Some kind ->
          Option.map
            (fun target -> { target; kind = Custom_relation kind })
            direct_target
      | None -> (
          match member "m.in_reply_to" relates_to with
          | None -> None
          | Some reply ->
              Option.bind (string_member "event_id" reply) event_id
              |> Option.map (fun target -> { target; kind = Reply })))

let message_kind = function
  | "m.text" -> Text
  | "m.notice" -> Notice
  | "m.emote" -> Emote
  | "m.image" -> Image
  | "m.file" -> File
  | "m.audio" -> Audio
  | "m.video" -> Video
  | "m.location" -> Location
  | "m.key.verification.request" -> Verification_request
  | kind -> Custom_message kind

let formatted_body ?resolve_mxc content =
  match
    (string_member "format" content, string_member "formatted_body" content)
  with
  | Some "org.matrix.custom.html", Some html ->
      let html = Html.sanitize ?resolve_mxc html in
      Some { html; plain = Html.to_plain html }
  | _ -> None

let parse_message ?resolve_mxc content =
  match (string_member "msgtype" content, string_member "body" content) with
  | Some kind, Some body ->
      Message
        {
          kind = message_kind kind;
          body;
          formatted = formatted_body ?resolve_mxc content;
          filename = string_member "filename" content;
          url = string_member "url" content;
          info = member "info" content;
        }
  | None, _ ->
      Malformed { event_type = "m.room.message"; reason = "missing msgtype" }
  | _, None ->
      Malformed { event_type = "m.room.message"; reason = "missing body" }

let parse_sticker content =
  match string_member "body" content with
  | Some body ->
      Sticker
        {
          body;
          url = string_member "url" content;
          info = member "info" content;
        }
  | None -> Malformed { event_type = "m.sticker"; reason = "missing body" }

let parse_reaction relation =
  match relation with
  | Some { target; kind = Annotation key } -> Reaction { key; target }
  | _ ->
      Malformed
        { event_type = "m.reaction"; reason = "missing annotation relation" }

let parse_redaction (event : Event.Raw_event.t) =
  let target =
    match Option.bind (string_member "redacts" event.content) event_id with
    | Some _ as target -> target
    | None -> event.redacts
  in
  Redaction { target; reason = string_member "reason" event.content }

let poll_text content =
  match string_member "text" content with
  | Some text -> text
  | None -> (
      match member "org.matrix.msc1767.text" content with
      | Some text -> Option.value (string_member "body" text) ~default:"Poll"
      | None -> "Poll")

let decode_beacon_info content =
  Jsont.Json.decode Event.Beacon_info_content.jsont content

(* Classifying [m.room.member] by both sides of the transition is what makes
   "left" and "was kicked" different items even though the state they write
   is the same. An absent [prev_content] counts as [leave], and the profile
   case is split out first. *)

type membership =
  | Join
  | Invite
  | Leave
  | Ban
  | Knock
  | Other_membership of string

let membership_of_string = function
  | "join" -> Join
  | "invite" -> Invite
  | "leave" -> Leave
  | "ban" -> Ban
  | "knock" -> Knock
  | other -> Other_membership other

let change_of ~previous ~current =
  if Option.equal String.equal previous current then None
  else Some { previous; current }

let classify_membership ~own ~previous ~current ~previous_name ~name
    ~previous_avatar ~avatar =
  match (previous, current) with
  | (Leave | Knock), Join -> `Membership Joined
  | Invite, Join -> `Membership Invitation_accepted
  | Invite, Leave ->
      `Membership (if own then Invitation_rejected else Invitation_revoked)
  | (Invite | Leave | Knock), Ban -> `Membership Banned
  | Join, Invite
  | Ban, Invite
  | Ban, Join
  | Join, Knock
  | Invite, Knock
  | Ban, Knock ->
      `Membership Invalid
  | Join, Join
    when own
         && not
              (Option.equal String.equal previous_name name
              && Option.equal String.equal previous_avatar avatar) ->
      `Profile
        {
          displayname = change_of ~previous:previous_name ~current:name;
          avatar_url = change_of ~previous:previous_avatar ~current:avatar;
        }
  | Join, Leave -> `Membership (if own then Left else Kicked)
  | Join, Ban -> `Membership Kicked_and_banned
  | Leave, Invite -> `Membership Invited
  | Ban, Leave -> `Membership Unbanned
  | Leave, Knock -> `Membership Knocked
  | Knock, Invite -> `Membership Knock_accepted
  | Knock, Leave -> `Membership (if own then Knock_retracted else Knock_denied)
  | previous, current when previous = current -> `Membership No_change
  | _ -> `Membership Unknown_membership

let parse_member (event : Event.Raw_event.t) state_key =
  let previous_content =
    Option.bind event.unsigned Event.Unsigned.prev_content
  in
  let member_of json = Option.bind json (string_member "membership") in
  match
    (Id.User_id.of_string state_key, string_member "membership" event.content)
  with
  | Error _, _ ->
      Malformed
        { event_type = "m.room.member"; reason = "state key is not a user id" }
  | _, None ->
      Malformed { event_type = "m.room.member"; reason = "missing membership" }
  | Ok user, Some current -> (
      let classified =
        classify_membership
          ~own:(String.equal (Id.User_id.to_string event.sender) state_key)
          ~previous:
            (Option.fold ~none:Leave ~some:membership_of_string
               (member_of previous_content))
          ~current:(membership_of_string current)
          ~previous_name:
            (Option.bind previous_content (string_member "displayname"))
          ~name:(string_member "displayname" event.content)
          ~previous_avatar:
            (Option.bind previous_content (string_member "avatar_url"))
          ~avatar:(string_member "avatar_url" event.content)
      in
      match classified with
      | `Profile change -> Profile { user; change }
      | `Membership change ->
          Membership
            { user; change; reason = string_member "reason" event.content })

let other_state event_type content =
  match event_type with
  | "m.room.create" -> Room_create
  | "m.room.name" -> Room_name (string_member "name" content)
  | "m.room.topic" -> Room_topic (string_member "topic" content)
  | "m.room.avatar" -> Room_avatar (string_member "url" content)
  | "m.room.canonical_alias" ->
      Room_canonical_alias (string_member "alias" content)
  | "m.room.encryption" -> Room_encryption
  | "m.room.pinned_events" -> Room_pinned_events
  | "m.room.tombstone" -> Room_tombstone (string_member "body" content)
  | "m.room.power_levels" -> Room_power_levels
  | "m.room.join_rules" -> Room_join_rules
  | "m.room.history_visibility" -> Room_history_visibility
  | "m.room.guest_access" -> Room_guest_access
  | "m.room.server_acl" -> Room_server_acl
  | "m.room.third_party_invite" ->
      Room_third_party_invite (string_member "display_name" content)
  | "m.policy.rule.room" | "m.policy.rule.server" | "m.policy.rule.user" ->
      Policy_rule event_type
  | "m.space.child" -> Space_child
  | "m.space.parent" -> Space_parent
  | "org.matrix.msc3672.beacon_info" -> (
      match decode_beacon_info content with
      | Ok beacon_info -> Beacon_info beacon_info
      | Error _ -> Other_state_type event_type)
  | other -> Other_state_type other

let content_of_event ?resolve_mxc (event : Event.Raw_event.t) relation =
  let event_type = Event.Event_type.to_string event.type_ in
  match event.type_ with
  | Room_message -> parse_message ?resolve_mxc event.content
  | Sticker -> parse_sticker event.content
  | Reaction -> parse_reaction relation
  | Room_redaction -> parse_redaction event
  | Room_message_encrypted -> Unable_to_decrypt
  | Poll_start | Poll_response | Poll_end ->
      Poll { text = poll_text event.content }
  | Room_member -> (
      match event.state_key with
      | Some state_key -> parse_member event state_key
      | None ->
          Malformed { event_type; reason = "member event without a state key" })
  | Beacon_info -> (
      match event.state_key with
      | None ->
          Malformed
            { event_type; reason = "beacon_info event without a state key" }
      | Some state_key -> (
          match Id.User_id.of_string state_key with
          | Error _ ->
              Malformed
                {
                  event_type;
                  reason = "beacon_info state key is not a user id";
                }
          | Ok _ -> (
              match decode_beacon_info event.content with
              | Ok beacon_info ->
                  State
                    { event_type; state_key; state = Beacon_info beacon_info }
              | Error reason ->
                  Malformed
                    {
                      event_type;
                      reason = "invalid beacon_info content: " ^ reason;
                    })))
  | _ -> (
      match event.state_key with
      | Some state_key ->
          State
            {
              event_type;
              state_key;
              state = other_state event_type event.content;
            }
      | None -> Custom { event_type; content = event.content })

let of_event ?resolve_mxc (event : Event.Raw_event.t) =
  let relation = relation_of_content event.content in
  {
    event_id = event.event_id;
    sender = event.sender;
    timestamp = event.origin_server_ts;
    relation;
    content = content_of_event ?resolve_mxc event relation;
    raw = event;
  }

let equal (left : t) (right : t) = left = right

(* The [m.replace] relation is dropped from the projection so that
   [is_preview_worthy] judges the replacement on its own merits. *)
let new_content (raw : Event.Raw_event.t) = member "m.new_content" raw.content

let replacement ?resolve_mxc event =
  match event.relation with
  | Some { kind = Replacement; _ } -> (
      match new_content event.raw with
      | None -> None
      | Some content ->
          let raw = { event.raw with content } in
          Some
            {
              event with
              relation = None;
              content = content_of_event ?resolve_mxc raw None;
              raw;
            })
  | Some _ | None -> None

(* The spec's validity rules for a replacement, without the encryption clauses
   whose provenance is outside [Presentation.t]. *)
let is_valid_replacement ~original ~replacement =
  let raw (e : t) = e.raw in
  Id.User_id.equal original.sender replacement.sender
  && (raw original).type_ = (raw replacement).type_
  && Option.is_none (raw original).state_key
  && Option.is_none (raw replacement).state_key
  && (match original.relation with
    | Some { kind = Replacement; _ } -> false
    | Some _ | None -> true)
  &&
  match (replacement.relation, original.event_id) with
  | Some { kind = Replacement; target }, Some id -> Id.Event_id.equal id target
  | _ -> false

(* The two flags are provenance from the event cache: [true] means that this
   projection was decrypted from an encrypted event, rather than that the
   projected raw event itself has an encrypted type. *)
let is_valid_replacement_with_encryption ~original ~original_encrypted
    ~replacement ~replacement_encrypted =
  is_valid_replacement ~original ~replacement
  && ((not original_encrypted) || replacement_encrypted)
  && ((not replacement_encrypted)
     || Option.is_some (new_content replacement.raw))

(* The subject of a membership or profile change is the state key; of any
   other state event the sender, because it is the sender who changed the
   room. *)

let membership_body user = function
  | Joined -> user ^ " joined"
  | Left -> user ^ " left"
  | Kicked -> user ^ " was removed"
  | Banned -> user ^ " was banned"
  | Kicked_and_banned -> user ^ " was removed and banned"
  | Unbanned -> user ^ " was unbanned"
  | Invited -> user ^ " was invited"
  | Invitation_accepted -> user ^ " accepted the invitation"
  | Invitation_rejected -> user ^ " rejected the invitation"
  | Invitation_revoked -> user ^ "'s invitation was withdrawn"
  | Knocked -> user ^ " asked to join"
  | Knock_accepted -> user ^ "'s request to join was accepted"
  | Knock_denied -> user ^ "'s request to join was denied"
  | Knock_retracted -> user ^ " withdrew their request to join"
  | No_change -> user ^ " made no change"
  | Invalid -> user ^ " made an invalid membership change"
  | Unknown_membership -> user ^ " changed their membership"

let profile_body user change =
  let name =
    match change.displayname with
    | None -> None
    | Some { current = Some name; _ } ->
        Some (Printf.sprintf "changed their display name to %s" name)
    | Some { current = None; _ } -> Some "removed their display name"
  in
  let avatar =
    match change.avatar_url with
    | None -> None
    | Some { current = Some _; _ } -> Some "changed their avatar"
    | Some { current = None; _ } -> Some "removed their avatar"
  in
  match (name, avatar) with
  | None, None -> user ^ " changed their profile"
  | Some part, None | None, Some part -> user ^ " " ^ part
  | Some name, Some avatar -> Printf.sprintf "%s %s and %s" user name avatar

let with_value sender verb removed = function
  | Some value -> Printf.sprintf "%s %s %s" sender verb value
  | None -> sender ^ " " ^ removed

let state_body sender = function
  | Room_create -> sender ^ " created the room"
  | Room_name name ->
      with_value sender "changed the room name to" "removed the room name" name
  | Room_topic topic ->
      with_value sender "changed the topic to" "removed the topic" topic
  | Room_avatar (Some _) -> sender ^ " changed the room avatar"
  | Room_avatar None -> sender ^ " removed the room avatar"
  | Room_canonical_alias alias ->
      with_value sender "set the main address to" "removed the main address"
        alias
  | Room_encryption -> sender ^ " turned on end-to-end encryption"
  | Room_pinned_events -> sender ^ " changed the pinned messages"
  | Room_tombstone reason ->
      with_value sender "upgraded this room:" "upgraded this room" reason
  | Room_power_levels -> sender ^ " changed the permissions"
  | Room_join_rules -> sender ^ " changed who can join"
  | Room_history_visibility -> sender ^ " changed who can read the history"
  | Room_guest_access -> sender ^ " changed guest access"
  | Room_server_acl -> sender ^ " changed the server access rules"
  | Room_third_party_invite name ->
      with_value sender "invited" "sent a third-party invitation" name
  | Policy_rule _ -> sender ^ " changed a moderation policy"
  | Space_child -> sender ^ " changed the rooms in this space"
  | Space_parent -> sender ^ " changed the spaces this room is in"
  | Beacon_info { description; live; _ } ->
      if live then
        with_value sender "started sharing their location:"
          "started sharing their location" description
      else
        with_value sender "stopped sharing their location:"
          "stopped sharing their location" description
  | Other_state_type event_type ->
      Printf.sprintf "%s changed %s" sender event_type

let preview event =
  let sender () = Id.User_id.to_string event.sender in
  match event.content with
  | Message message -> Some message.body
  | Sticker { body; _ } -> Some body
  | Reaction { key; _ } -> Some key
  | Redaction _ -> Some "Message removed"
  | Poll { text } -> Some text
  | Membership { user; change; _ } ->
      Some (membership_body (Id.User_id.to_string user) change)
  | Profile { user; change } ->
      Some (profile_body (Id.User_id.to_string user) change)
  | State { state; _ } -> Some (state_body (sender ()) state)
  | Unable_to_decrypt -> Some "Unable to decrypt message"
  | Custom { event_type; _ } -> Some event_type
  | Malformed { event_type; _ } -> Some event_type

let is_preview_worthy ?own_user ?(can_accept_knock = fun _ -> false) event =
  match event.relation with
  | Some { kind = Replacement; _ } -> false
  | _ -> (
      match event.content with
      | Message { kind = Verification_request; _ } -> false
      | Message _ | Sticker _ -> true
      | Poll _ -> event.raw.type_ = Event.Event_type.Poll_start
      | Custom { event_type = "m.call.invite" | "m.rtc.notification"; _ } ->
          true
      | Membership { user; change; _ } -> (
          (* The own user joining or being invited is a valid latest event,
             so that a room with nothing else in it still has something to
             show. The subject is the state key, not the sender. *)
          match (own_user, change) with
          | Some own, (Joined | Invited | Invitation_accepted | Knock_accepted)
            ->
              Id.User_id.equal own user
          | Some _, Knocked -> can_accept_knock user
          | Some _, _ | None, _ -> false)
      | State { state = Beacon_info _; _ } -> true
      | Reaction _ | Redaction _ | Profile _ | State _ | Unable_to_decrypt
      | Custom _ | Malformed _ ->
          false)
