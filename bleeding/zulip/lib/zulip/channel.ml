module Topics_policy = struct
  type t =
    | Inherit
    | Allow_empty_topic
    | Disable_empty_topic
    | Empty_topic_only
    | Other of string

  let to_string = function
    | Inherit -> "inherit"
    | Allow_empty_topic -> "allow_empty_topic"
    | Disable_empty_topic -> "disable_empty_topic"
    | Empty_topic_only -> "empty_topic_only"
    | Other value -> value

  let of_string = function
    | "inherit" -> Inherit
    | "allow_empty_topic" -> Allow_empty_topic
    | "disable_empty_topic" -> Disable_empty_topic
    | "empty_topic_only" -> Empty_topic_only
    | value -> Other value

  let jsont =
    Jsont.map ~kind:"Zulip channel topics policy" ~dec:of_string ~enc:to_string
      Jsont.string
end

module Posting_policy = struct
  type t =
    | Everyone
    | Administrators
    | Full_members
    | Moderators
    | Other of int

  let of_int = function
    | 1 -> Everyone
    | 2 -> Administrators
    | 3 -> Full_members
    | 4 -> Moderators
    | n -> Other n

  let to_int = function
    | Everyone -> 1
    | Administrators -> 2
    | Full_members -> 3
    | Moderators -> 4
    | Other n -> n

  let equal a b = Int.equal (to_int a) (to_int b)
  let compare a b = Int.compare (to_int a) (to_int b)
  let pp ppf t = Format.pp_print_int ppf (to_int t)

  let jsont =
    Jsont.map ~kind:"Zulip legacy posting policy" ~dec:of_int ~enc:to_int
      Json_integer.jsont
end

type t = {
  name : string;
  stream_id : Id.Channel.t option;
  description : string;
  rendered_description : string option;
  invite_only : bool;
  is_web_public : bool;
  history_public_to_subscribers : bool;
  is_default : bool;
  message_retention_days : int option option;
  first_message_id : Id.Message.t option;
  date_created : float option;
  creator_id : Id.User.t option;
  stream_post_policy : Posting_policy.t;
  is_archived : bool;
  topics_policy : Topics_policy.t option;
  folder_id : Id.Channel_folder.t option option;
  is_recently_active : bool option;
  is_announcement_only : bool option;
  subscriber_count : int option;
  stream_weekly_traffic : int option option;
  can_add_subscribers_group : Group_setting.t option;
  can_remove_subscribers_group : Group_setting.t option;
  can_administer_channel_group : Group_setting.t option;
  can_delete_any_message_group : Group_setting.t option;
  can_delete_own_message_group : Group_setting.t option;
  can_move_messages_out_of_channel_group : Group_setting.t option;
  can_move_messages_within_channel_group : Group_setting.t option;
  can_send_message_group : Group_setting.t option;
  can_subscribe_group : Group_setting.t option;
  can_resolve_topics_group : Group_setting.t option;
  can_create_topic_group : Group_setting.t option;
  extensions : Jsont.json;
}

let empty_extensions () = Jsont.Json.object' []

let create ~name ?stream_id ?(description = "") ?rendered_description
    ?(invite_only = false) ?(is_web_public = false)
    ?(history_public_to_subscribers = true) ?(is_default = false)
    ?message_retention_days ?first_message_id ?date_created ?creator_id
    ?(stream_post_policy = Posting_policy.Everyone) ?(is_archived = false)
    ?topics_policy ?folder_id ?is_recently_active ?is_announcement_only
    ?subscriber_count ?stream_weekly_traffic ?can_add_subscribers_group
    ?can_remove_subscribers_group ?can_administer_channel_group
    ?can_delete_any_message_group ?can_delete_own_message_group
    ?can_move_messages_out_of_channel_group
    ?can_move_messages_within_channel_group ?can_send_message_group
    ?can_subscribe_group ?can_resolve_topics_group ?can_create_topic_group
    ?extensions () =
  (match extensions with
  | None | Some (Jsont.Object _) -> ()
  | Some _ -> invalid_arg "Zulip.Channel.create: extensions must be an object");
  {
    name;
    stream_id;
    description;
    rendered_description;
    invite_only;
    is_web_public;
    history_public_to_subscribers;
    is_default;
    message_retention_days;
    first_message_id;
    date_created;
    creator_id;
    stream_post_policy;
    is_archived;
    topics_policy;
    folder_id;
    is_recently_active;
    is_announcement_only;
    subscriber_count;
    stream_weekly_traffic;
    can_add_subscribers_group;
    can_remove_subscribers_group;
    can_administer_channel_group;
    can_delete_any_message_group;
    can_delete_own_message_group;
    can_move_messages_out_of_channel_group;
    can_move_messages_within_channel_group;
    can_send_message_group;
    can_subscribe_group;
    can_resolve_topics_group;
    can_create_topic_group;
    extensions = Option.value ~default:(empty_extensions ()) extensions;
  }

let name t = t.name
let stream_id t = t.stream_id
let description t = t.description
let rendered_description t = t.rendered_description
let invite_only t = t.invite_only
let is_web_public t = t.is_web_public
let history_public_to_subscribers t = t.history_public_to_subscribers
let is_default t = t.is_default
let message_retention_days t = t.message_retention_days
let first_message_id t = t.first_message_id
let date_created t = t.date_created
let creator_id t = t.creator_id
let stream_post_policy t = t.stream_post_policy
let is_archived t = t.is_archived
let topics_policy t = t.topics_policy
let folder_id t = t.folder_id
let is_recently_active t = t.is_recently_active
let is_announcement_only t = t.is_announcement_only
let subscriber_count t = t.subscriber_count
let stream_weekly_traffic t = t.stream_weekly_traffic
let can_add_subscribers_group t = t.can_add_subscribers_group
let can_remove_subscribers_group t = t.can_remove_subscribers_group
let can_administer_channel_group t = t.can_administer_channel_group
let can_delete_any_message_group t = t.can_delete_any_message_group
let can_delete_own_message_group t = t.can_delete_own_message_group

let can_move_messages_out_of_channel_group t =
  t.can_move_messages_out_of_channel_group

let can_move_messages_within_channel_group t =
  t.can_move_messages_within_channel_group

let can_send_message_group t = t.can_send_message_group
let can_subscribe_group t = t.can_subscribe_group
let can_resolve_topics_group t = t.can_resolve_topics_group
let can_create_topic_group t = t.can_create_topic_group
let extensions t = t.extensions

let base_jsont ~kind make enc =
  Jsont.Object.map ~kind make
  |> Jsont.Object.mem "name" Jsont.string ~enc:(fun x -> (enc x).name)
  |> Jsont.Object.opt_mem "stream_id" Id.Channel.jsont ~enc:(fun x ->
      (enc x).stream_id)
  |> Jsont.Object.mem "description" Jsont.string
       ~dec_absent:(fun () -> "")
       ~enc:(fun x -> (enc x).description)
  |> Jsont.Object.opt_mem "rendered_description" Jsont.string ~enc:(fun x ->
      (enc x).rendered_description)
  |> Jsont.Object.mem "invite_only" Jsont.bool
       ~dec_absent:(fun () -> false)
       ~enc:(fun x -> (enc x).invite_only)
  |> Jsont.Object.mem "is_web_public" Jsont.bool
       ~dec_absent:(fun () -> false)
       ~enc:(fun x -> (enc x).is_web_public)
  |> Jsont.Object.mem "history_public_to_subscribers" Jsont.bool
       ~dec_absent:(fun () -> true)
       ~enc:(fun x -> (enc x).history_public_to_subscribers)
  |> Jsont.Object.mem "is_default" Jsont.bool
       ~dec_absent:(fun () -> false)
       ~enc:(fun x -> (enc x).is_default)
  |> Jsont.Object.opt_mem "message_retention_days" (Jsont.option Jsont.int)
       ~enc:(fun x -> (enc x).message_retention_days)
  |> Jsont.Object.mem "first_message_id"
       (Jsont.option Id.Message.jsont)
       ~dec_absent:(fun () -> None)
       ~enc:(fun x -> (enc x).first_message_id)
  |> Jsont.Object.opt_mem "date_created" Jsont.number ~enc:(fun x ->
      (enc x).date_created)
  |> Jsont.Object.mem "creator_id"
       (Jsont.option Id.User.jsont)
       ~dec_absent:(fun () -> None)
       ~enc:(fun x -> (enc x).creator_id)
  |> Jsont.Object.mem "stream_post_policy" Posting_policy.jsont
       ~dec_absent:(fun () -> Posting_policy.Everyone)
       ~enc:(fun x -> (enc x).stream_post_policy)
  |> Jsont.Object.mem "is_archived" Jsont.bool
       ~dec_absent:(fun () -> false)
       ~enc:(fun x -> (enc x).is_archived)
  |> Jsont.Object.opt_mem "topics_policy" Topics_policy.jsont ~enc:(fun x ->
      (enc x).topics_policy)
  |> Jsont.Object.opt_mem "folder_id" (Jsont.option Id.Channel_folder.jsont)
       ~enc:(fun x -> (enc x).folder_id)
  |> Jsont.Object.opt_mem "is_recently_active" Jsont.bool ~enc:(fun x ->
      (enc x).is_recently_active)
  |> Jsont.Object.opt_mem "is_announcement_only" Jsont.bool ~enc:(fun x ->
      (enc x).is_announcement_only)
  |> Jsont.Object.opt_mem "subscriber_count" Jsont.int ~enc:(fun x ->
      (enc x).subscriber_count)
  |> Jsont.Object.opt_mem "stream_weekly_traffic" (Jsont.option Jsont.int)
       ~enc:(fun x -> (enc x).stream_weekly_traffic)
  |> Jsont.Object.opt_mem "can_add_subscribers_group" Group_setting.jsont
       ~enc:(fun x -> (enc x).can_add_subscribers_group)
  |> Jsont.Object.opt_mem "can_remove_subscribers_group" Group_setting.jsont
       ~enc:(fun x -> (enc x).can_remove_subscribers_group)
  |> Jsont.Object.opt_mem "can_administer_channel_group" Group_setting.jsont
       ~enc:(fun x -> (enc x).can_administer_channel_group)
  |> Jsont.Object.opt_mem "can_delete_any_message_group" Group_setting.jsont
       ~enc:(fun x -> (enc x).can_delete_any_message_group)
  |> Jsont.Object.opt_mem "can_delete_own_message_group" Group_setting.jsont
       ~enc:(fun x -> (enc x).can_delete_own_message_group)
  |> Jsont.Object.opt_mem "can_move_messages_out_of_channel_group"
       Group_setting.jsont ~enc:(fun x ->
         (enc x).can_move_messages_out_of_channel_group)
  |> Jsont.Object.opt_mem "can_move_messages_within_channel_group"
       Group_setting.jsont ~enc:(fun x ->
         (enc x).can_move_messages_within_channel_group)
  |> Jsont.Object.opt_mem "can_send_message_group" Group_setting.jsont
       ~enc:(fun x -> (enc x).can_send_message_group)
  |> Jsont.Object.opt_mem "can_subscribe_group" Group_setting.jsont
       ~enc:(fun x -> (enc x).can_subscribe_group)
  |> Jsont.Object.opt_mem "can_resolve_topics_group" Group_setting.jsont
       ~enc:(fun x -> (enc x).can_resolve_topics_group)
  |> Jsont.Object.opt_mem "can_create_topic_group" Group_setting.jsont
       ~enc:(fun x -> (enc x).can_create_topic_group)

let make name stream_id description rendered_description invite_only
    is_web_public history_public_to_subscribers is_default
    message_retention_days first_message_id date_created creator_id
    stream_post_policy is_archived topics_policy folder_id is_recently_active
    is_announcement_only subscriber_count stream_weekly_traffic
    can_add_subscribers_group can_remove_subscribers_group
    can_administer_channel_group can_delete_any_message_group
    can_delete_own_message_group can_move_messages_out_of_channel_group
    can_move_messages_within_channel_group can_send_message_group
    can_subscribe_group can_resolve_topics_group can_create_topic_group
    extensions =
  {
    name;
    stream_id;
    description;
    rendered_description;
    invite_only;
    is_web_public;
    history_public_to_subscribers;
    is_default;
    message_retention_days;
    first_message_id;
    date_created;
    creator_id;
    stream_post_policy;
    is_archived;
    topics_policy;
    folder_id;
    is_recently_active;
    is_announcement_only;
    subscriber_count;
    stream_weekly_traffic;
    can_add_subscribers_group;
    can_remove_subscribers_group;
    can_administer_channel_group;
    can_delete_any_message_group;
    can_delete_own_message_group;
    can_move_messages_out_of_channel_group;
    can_move_messages_within_channel_group;
    can_send_message_group;
    can_subscribe_group;
    can_resolve_topics_group;
    can_create_topic_group;
    extensions;
  }

let jsont =
  base_jsont ~kind:"Zulip channel" make Fun.id
  |> Jsont.Object.keep_unknown Jsont.json_mems ~enc:extensions
  |> Jsont.Object.finish

let raw t =
  match Jsont.Json.encode' jsont t with
  | Ok j -> j
  | Error e -> raise (Jsont.Error e)

module Subscription = struct
  type channel = t

  type t = {
    channel : channel;
    color : string option;
    is_muted : bool;
    in_home_view : bool option;
    pin_to_top : bool;
    desktop_notifications : bool option;
    audible_notifications : bool option;
    push_notifications : bool option;
    email_notifications : bool option;
    wildcard_mentions_notify : bool option;
    subscribers : Id.User.t list option;
    partial_subscribers : Id.User.t list option;
  }

  let channel t = t.channel
  let color t = t.color
  let is_muted t = t.is_muted
  let in_home_view t = t.in_home_view
  let pin_to_top t = t.pin_to_top
  let desktop_notifications t = t.desktop_notifications
  let audible_notifications t = t.audible_notifications
  let push_notifications t = t.push_notifications
  let email_notifications t = t.email_notifications
  let wildcard_mentions_notify t = t.wildcard_mentions_notify
  let subscribers t = t.subscribers
  let partial_subscribers t = t.partial_subscribers
  let extensions t = t.channel.extensions

  let make_sub name stream_id description rendered_description invite_only
      is_web_public history_public_to_subscribers is_default
      message_retention_days first_message_id date_created creator_id
      stream_post_policy is_archived topics_policy folder_id is_recently_active
      is_announcement_only subscriber_count stream_weekly_traffic
      can_add_subscribers_group can_remove_subscribers_group
      can_administer_channel_group can_delete_any_message_group
      can_delete_own_message_group can_move_messages_out_of_channel_group
      can_move_messages_within_channel_group can_send_message_group
      can_subscribe_group can_resolve_topics_group can_create_topic_group color
      is_muted in_home_view pin_to_top desktop_notifications
      audible_notifications push_notifications email_notifications
      wildcard_mentions_notify subscribers partial_subscribers extensions =
    let channel =
      make name stream_id description rendered_description invite_only
        is_web_public history_public_to_subscribers is_default
        message_retention_days first_message_id date_created creator_id
        stream_post_policy is_archived topics_policy folder_id
        is_recently_active is_announcement_only subscriber_count
        stream_weekly_traffic can_add_subscribers_group
        can_remove_subscribers_group can_administer_channel_group
        can_delete_any_message_group can_delete_own_message_group
        can_move_messages_out_of_channel_group
        can_move_messages_within_channel_group can_send_message_group
        can_subscribe_group can_resolve_topics_group can_create_topic_group
        extensions
    in
    {
      channel;
      color;
      is_muted;
      in_home_view;
      pin_to_top;
      desktop_notifications;
      audible_notifications;
      push_notifications;
      email_notifications;
      wildcard_mentions_notify;
      subscribers;
      partial_subscribers;
    }

  let jsont =
    base_jsont ~kind:"Zulip channel subscription" make_sub (fun t -> t.channel)
    |> Jsont.Object.opt_mem "color" Jsont.string ~enc:color
    |> Jsont.Object.mem "is_muted" Jsont.bool
         ~dec_absent:(fun () -> false)
         ~enc:is_muted
    |> Jsont.Object.opt_mem "in_home_view" Jsont.bool ~enc:in_home_view
    |> Jsont.Object.mem "pin_to_top" Jsont.bool
         ~dec_absent:(fun () -> false)
         ~enc:pin_to_top
    |> Jsont.Object.mem "desktop_notifications" (Jsont.option Jsont.bool)
         ~dec_absent:(fun () -> None)
         ~enc:desktop_notifications
    |> Jsont.Object.mem "audible_notifications" (Jsont.option Jsont.bool)
         ~dec_absent:(fun () -> None)
         ~enc:audible_notifications
    |> Jsont.Object.mem "push_notifications" (Jsont.option Jsont.bool)
         ~dec_absent:(fun () -> None)
         ~enc:push_notifications
    |> Jsont.Object.mem "email_notifications" (Jsont.option Jsont.bool)
         ~dec_absent:(fun () -> None)
         ~enc:email_notifications
    |> Jsont.Object.mem "wildcard_mentions_notify" (Jsont.option Jsont.bool)
         ~dec_absent:(fun () -> None)
         ~enc:wildcard_mentions_notify
    |> Jsont.Object.opt_mem "subscribers" (Jsont.list Id.User.jsont)
         ~enc:subscribers
    |> Jsont.Object.opt_mem "partial_subscribers" (Jsont.list Id.User.jsont)
         ~enc:partial_subscribers
    |> Jsont.Object.keep_unknown Jsont.json_mems ~enc:extensions
    |> Jsont.Object.finish

  let raw t =
    match Jsont.Json.encode' jsont t with
    | Ok j -> j
    | Error e -> raise (Jsont.Error e)
end

let pp ppf t =
  Format.fprintf ppf "Channel{name=%S%a}" t.name
    (fun ppf -> function
      | None -> () | Some id -> Format.fprintf ppf "; id=%a" Id.Channel.pp id)
    t.stream_id
