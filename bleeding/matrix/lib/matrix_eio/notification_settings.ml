module Core = Matrix_client.Notification_settings

type room_notification_mode = Core.room_notification_mode =
  | All_messages
  | Mentions_and_keywords_only
  | Mute

type t = Core.t
type subscription = Core.subscription

let create ?ruleset client = Core.create ?ruleset (Client.base client)
let client = Core.client
let ruleset = Core.ruleset

let refresh t =
  Error.unwrap ~context:"refreshing Matrix notification settings"
    (Core.refresh t)

let user_defined_room_mode = Core.user_defined_room_mode
let default_room_mode = Core.default_room_mode
let room_mode = Core.room_mode
let rooms_with_user_defined_rules = Core.rooms_with_user_defined_rules

let set_room_mode t room_id mode =
  Error.unwrap ~context:"setting a room notification mode"
    (Core.set_room_mode t room_id mode)

let delete_room_mode t room_id =
  Error.unwrap ~context:"deleting a room notification mode"
    (Core.delete_room_mode t room_id)

let unmute_room t room_id ~encrypted ~one_to_one =
  Error.unwrap ~context:"unmuting a Matrix room"
    (Core.unmute_room t room_id ~encrypted ~one_to_one)

let set_default_room_mode t ~encrypted ~one_to_one mode =
  Error.unwrap ~context:"setting a default room notification mode"
    (Core.set_default_room_mode t ~encrypted ~one_to_one mode)

let contains_keyword_rules = Core.contains_keyword_rules
let enabled_keywords = Core.enabled_keywords

let add_keyword t keyword =
  Error.unwrap ~context:"adding a notification keyword"
    (Core.add_keyword t keyword)

let remove_keyword t keyword =
  Error.unwrap ~context:"removing a notification keyword"
    (Core.remove_keyword t keyword)

let is_enabled t rule_id =
  Error.unwrap ~context:"reading a notification rule"
    (Core.is_enabled t rule_id)

let set_enabled t rule_id ~enabled =
  Error.unwrap ~context:"enabling or disabling a notification rule"
    (Core.set_enabled t rule_id ~enabled)

let set_actions t rule_id ~actions =
  Error.unwrap ~context:"setting notification-rule actions"
    (Core.set_actions t rule_id ~actions)

let subscribe = Core.subscribe
let unsubscribe = Core.unsubscribe
