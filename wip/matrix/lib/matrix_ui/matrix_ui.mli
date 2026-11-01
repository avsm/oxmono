(** matrix_ui — Eio-based reactive models on top of [matrix-chat.client].

    {!Runtime} wires the rest together and is where a client starts. It owns an
    {!Event_cache} over an optional {!Event_store}, publishes a {!Room_list} and
    a {!Room_timeline} per room, and drives them from one sync loop. Every model
    publishes through {!Observable}, so a toolkit renders a snapshot and then
    follows diffs. *)

module Observable = Observable
module Matching = Matching
module Presentation = Presentation
module Event_store = Event_store
module Event_cache = Event_cache
module Pinned_events = Pinned_events
module Room_timeline = Room_timeline
module Room_list = Room_list
module Runtime = Runtime
module Utd_hook = Utd_hook
module Room_identity = Room_identity
module Live_locations = Live_locations
module Live_location = Live_locations
module Notification_client = Notification_client
module Room_directory_search = Room_directory_search
module Search_service = Search_service
module Thread_info = Thread_info
module Thread_cache = Thread_cache
module Thread_list = Thread_list
module Event_focused = Event_focused
module Back_pagination = Back_pagination
