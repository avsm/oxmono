open Matrix_bot
module Id = Matrix_proto.Id
module P = Matrix_ui.Presentation

let say room = function
  | None -> ()
  | Some text -> ignore (Room.send_notice room text)

(* The subject of a membership change is the state key, so a bot invited by
   somebody else must still not greet itself when it accepts. *)
let membership bot (m : Event.membership) =
  if not (Id.User_id.equal m.user (Bot.user_id bot)) then
    let who = Id.User_id.to_string m.user in
    say m.envelope.room
      (match m.change with
      | P.Joined | P.Invitation_accepted | P.Knock_accepted ->
          Some (Printf.sprintf "Welcome, %s!" who)
      | P.Left -> Some (Printf.sprintf "Goodbye, %s." who)
      | P.Kicked | P.Kicked_and_banned ->
          Some (Printf.sprintf "%s was removed from the room." who)
      | P.Banned -> Some (Printf.sprintf "%s was banned." who)
      | P.Invited -> Some (Printf.sprintf "%s has been invited." who)
      | P.Invitation_rejected ->
          Some (Printf.sprintf "%s declined the invitation." who)
      | P.Unbanned | P.Invitation_revoked | P.Knocked | P.Knock_denied
      | P.Knock_retracted | P.No_change | P.Invalid | P.Unknown_membership ->
          None)

let room_state _ (s : Event.room_state) =
  say s.envelope.room
    (match s.state with
    | P.Room_name (Some name) ->
        Some (Printf.sprintf "The room is now called %s." name)
    | P.Room_name None -> Some "The room no longer has a name."
    | P.Room_topic (Some topic) ->
        Some (Printf.sprintf "The topic is now: %s" topic)
    | P.Room_topic None -> Some "The topic has been cleared."
    | P.Room_avatar _ -> Some "The room avatar changed."
    | _ -> None)

(* A display name change is not a membership change, so it has an event of
   its own rather than a registration of its own. Either field of a
   [profile_change] may change on its own, so both are checked. *)
let profile _ = function
  | Event.Profile { envelope; user; change } ->
      let who = Id.User_id.to_string user in
      let name_message =
        match change.P.displayname with
        | None -> None
        | Some { current = Some name; _ } ->
            Some (Printf.sprintf "%s is now known as %s." who name)
        | Some { current = None; _ } ->
            Some (Printf.sprintf "%s dropped their display name." who)
      in
      let avatar_message =
        match change.P.avatar_url with
        | None -> None
        | Some { current = Some _; _ } ->
            Some (Printf.sprintf "%s changed their avatar." who)
        | Some { current = None; _ } ->
            Some (Printf.sprintf "%s removed their avatar." who)
      in
      List.iter (say envelope.room) [ name_message; avatar_message ]
  | _ -> ()

let plugin spec =
  spec
  |> Bot.on_membership membership
  |> Bot.on_room_state room_state
  |> Bot.on profile
