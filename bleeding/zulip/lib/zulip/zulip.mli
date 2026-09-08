@@ portable

(** Pure Zulip protocol types and JSON codecs.

    The module re-exports the protocol modules used to represent identifiers,
    messages, channels, users, and event queue payloads. *)

module Id = Id
module Group_setting = Group_setting
module Message = Message
module Message_type = Message_type
module Message_response = Message_response
module Message_flag = Message_flag
module Narrow = Narrow
module Channel = Channel
module User = User
module Event = Event
module Event_type = Event_type
module Event_payload = Event_payload
module Topic_visibility = Topic_visibility
