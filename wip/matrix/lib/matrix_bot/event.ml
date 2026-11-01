module Id = Matrix_proto.Id
module P = Matrix_ui.Presentation

type envelope = {
  room : Room.t;
  sender : Id.User_id.t;
  event_id : Id.Event_id.t;
}

type message = {
  envelope : envelope;
  content : P.message;
  presentation : P.t;
  reply_to : Id.Event_id.t option;
}

type command = {
  message : message;
  name : string;
  args : string;
  argv : string list;
}

type edit = { message : message; original : Id.Event_id.t }
type sticker = { envelope : envelope; body : string; url : string option }
type poll = { envelope : envelope; text : string }

type reaction = {
  envelope : envelope;
  key : string;
  relates_to : Id.Event_id.t;
}

type redaction = {
  envelope : envelope;
  target : Id.Event_id.t option;
  reason : string option;
}

type membership = {
  envelope : envelope;
  user : Id.User_id.t;
  change : P.membership_change;
  reason : string option;
}

type profile = {
  envelope : envelope;
  user : Id.User_id.t;
  change : P.profile_change;
}

type room_state = { envelope : envelope; state : P.other_state }

type custom = {
  envelope : envelope;
  event_type : string;
  content : Jsont.json;
  presentation : P.t;
}

type invitation = { room_id : Id.Room_id.t; inviter : Id.User_id.t option }

type t =
  | Message of message
  | Command of command
  | Edit of edit
  | Sticker of sticker
  | Poll of poll
  | Reaction of reaction
  | Redaction of redaction
  | Membership of membership
  | Profile of profile
  | Room_state of room_state
  | Custom of custom
  | Invited of invitation
  | Joined of Room.t
  | Left of Id.Room_id.t
  | Sync of Matrix_ui.Runtime.sync_state

let envelope = function
  | Message m -> Some m.envelope
  | Command c -> Some c.message.envelope
  | Edit e -> Some e.message.envelope
  | Sticker s -> Some s.envelope
  | Poll p -> Some p.envelope
  | Reaction r -> Some r.envelope
  | Redaction r -> Some r.envelope
  | Membership m -> Some m.envelope
  | Profile p -> Some p.envelope
  | Room_state s -> Some s.envelope
  | Custom c -> Some c.envelope
  | Invited _ | Joined _ | Left _ | Sync _ -> None

let room event =
  match event with
  | Joined room -> Some room
  | _ -> Option.map (fun e -> e.room) (envelope event)

let sender event =
  match event with
  | Invited { inviter; _ } -> inviter
  | _ -> Option.map (fun e -> e.sender) (envelope event)

let room_id event =
  match event with
  | Invited { room_id; _ } | Left room_id -> Some room_id
  | _ -> Option.map Room.id (room event)

let kind = function
  | Message _ -> "message"
  | Command _ -> "command"
  | Edit _ -> "edit"
  | Sticker _ -> "sticker"
  | Poll _ -> "poll"
  | Reaction _ -> "reaction"
  | Redaction _ -> "redaction"
  | Membership _ -> "membership"
  | Profile _ -> "profile"
  | Room_state _ -> "state"
  | Custom { event_type; _ } -> event_type
  | Invited _ -> "invited"
  | Joined _ -> "joined"
  | Left _ -> "left"
  | Sync Matrix_ui.Runtime.Not_started -> "sync not started"
  | Sync Matrix_ui.Runtime.Syncing -> "sync starting"
  | Sync (Matrix_ui.Runtime.Live _) -> "sync live"
  | Sync (Matrix_ui.Runtime.Failed _) -> "sync failed"
  | Sync Matrix_ui.Runtime.Offline -> "sync offline"
  | Sync Matrix_ui.Runtime.Stopped -> "sync stopped"

let pp ppf event =
  let where =
    match room_id event with
    | None -> ""
    | Some id -> " in " ^ Id.Room_id.to_string id
  in
  let who =
    match sender event with
    | None -> ""
    | Some user -> " from " ^ Id.User_id.to_string user
  in
  Format.fprintf ppf "%s%s%s" (kind event) where who

let reply e ?html body = Room.send_notice e.room ?html ~reply_to:e.event_id body
let react e key = Room.react e.room e.event_id key
