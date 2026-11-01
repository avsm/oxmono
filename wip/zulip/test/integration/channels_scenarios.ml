open Zulip_eio

let fail = function
  | Ok value -> value
  | Error error -> failwith (Error.error_to_string error)

let require condition message =
  if condition then Ok () else Error (Error.Invalid_request message)

let create_options ~name ~user ~folder_id ~group_id =
  {
    Channels.name;
    description = Some "OCaml channel/group parity scenario";
    subscribers = [ user ];
    announce = Some false;
    invite_only = Some true;
    is_web_public = Some false;
    is_default_stream = Some false;
    history_public_to_subscribers = Some true;
    message_retention_days = Some Channels.Realm_default;
    folder_id = Some folder_id;
    topics_policy = Some Zulip.Channel.Topics_policy.Allow_empty_topic;
    can_add_subscribers_group = Some (Zulip.Group_setting.Group group_id);
    can_create_topic_group = Some (Zulip.Group_setting.Group group_id);
    can_delete_any_message_group = Some (Zulip.Group_setting.Group group_id);
    can_delete_own_message_group = Some (Zulip.Group_setting.Group group_id);
    can_remove_subscribers_group = Some (Zulip.Group_setting.Group group_id);
    can_administer_channel_group = Some (Zulip.Group_setting.Group group_id);
    can_move_messages_out_of_channel_group =
      Some (Zulip.Group_setting.Group group_id);
    can_move_messages_within_channel_group =
      Some (Zulip.Group_setting.Group group_id);
    can_send_message_group = Some (Zulip.Group_setting.Group group_id);
    can_subscribe_group = Some (Zulip.Group_setting.Group group_id);
    can_resolve_topics_group = Some (Zulip.Group_setting.Group group_id);
  }

let run ~client ~user =
  let nonce =
    Printf.sprintf "%d-%d" (Zulip.Id.User.to_int user) (Unix.getpid ())
  in
  let child_id = ref None
  and parent_id = ref None
  and folder_id = ref None
  and channel_id = ref None in
  let cleanup () =
    Option.iter
      (fun id -> ignore (Channels.archive client ~channel_id:id))
      !channel_id;
    Option.iter
      (fun id ->
        ignore
          (Channel_folders.set_archived client ~folder_id:id ~archived:true))
      !folder_id;
    Option.iter
      (fun id -> ignore (User_group.delete client ~group_id:id))
      !parent_id;
    Option.iter
      (fun id -> ignore (User_group.delete client ~group_id:id))
      !child_id
  in
  Fun.protect ~finally:cleanup @@ fun () ->
  let direct =
    Zulip.Group_setting.Direct { members = [ user ]; subgroups = [] }
  in
  let child =
    User_group.create client ~name:("ocaml-child-" ^ nonce)
      ~description:"Temporary child group" ~members:[ user ]
      ~can_add_members_group:direct ~can_join_group:direct
      ~can_leave_group:direct ~can_manage_group:direct ~can_mention_group:direct
      ~can_remove_members_group:direct ()
    |> fail
  in
  child_id := Some child;
  let parent =
    User_group.create client ~name:("ocaml-parent-" ^ nonce)
      ~description:"Temporary parent group" ~members:[] ~subgroups:[ child ]
      ~can_add_members_group:direct ~can_join_group:direct
      ~can_leave_group:direct ~can_manage_group:direct ~can_mention_group:direct
      ~can_remove_members_group:direct ()
    |> fail
  in
  parent_id := Some parent;
  User_group.update_members client ~group_id:child ~remove:[ user ] () |> fail;
  User_group.update_members client ~group_id:child ~add:[ user ] () |> fail;
  User_group.update_subgroups client ~group_id:parent ~remove:[ child ] ()
  |> fail;
  User_group.update_subgroups client ~group_id:parent ~add:[ child ] () |> fail;
  let members =
    User_group.get_members client ~group_id:parent ~direct_member_only:false ()
    |> fail
  in
  require
    (List.exists (Zulip.Id.User.equal user) members)
    "transitive group member missing"
  |> fail;
  let subgroups =
    User_group.get_subgroups client ~group_id:parent ~direct_subgroup_only:true
      ()
    |> fail
  in
  require (List.mem child subgroups) "direct subgroup missing" |> fail;
  let folder =
    Channel_folders.create client ~name:("OCaml folder " ^ nonce)
      ~description:"Temporary parity folder" ()
    |> fail
  in
  folder_id := Some folder;
  let channel =
    Channels.create client
      (create_options ~name:("ocaml-channel-" ^ nonce) ~user ~folder_id:folder
         ~group_id:parent)
    |> fail
  in
  channel_id := Some channel;
  let initial = Channels.get_by_id client ~channel_id:channel |> fail in
  require
    (Zulip.Channel.topics_policy initial
    = Some Zulip.Channel.Topics_policy.Allow_empty_topic)
    "initial topic policy was not retained"
  |> fail;
  require
    (Zulip.Channel.folder_id initial = Some (Some folder))
    "channel folder was not retained"
  |> fail;
  let permission_update =
    {
      Zulip.Group_setting.new_ =
        Zulip.Group_setting.Direct { members = [ user ]; subgroups = [ child ] };
      old = Some (Zulip.Group_setting.Group parent);
    }
  in
  Channels.update client ~channel_id:channel
    ~topics_policy:Zulip.Channel.Topics_policy.Disable_empty_topic
    ~can_send_message_group:permission_update ()
  |> fail;
  let updated = Channels.get_by_id client ~channel_id:channel |> fail in
  require
    (Zulip.Channel.topics_policy updated
    = Some Zulip.Channel.Topics_policy.Disable_empty_topic)
    "updated topic policy was not retained"
  |> fail;
  match Zulip.Channel.can_send_message_group updated with
  | Some (Zulip.Group_setting.Direct { members; subgroups }) ->
      require
        (List.exists (Zulip.Id.User.equal user) members
        && List.mem child subgroups)
        "updated channel permission was not retained"
      |> fail
  | _ -> failwith "updated channel permission has the wrong representation"
