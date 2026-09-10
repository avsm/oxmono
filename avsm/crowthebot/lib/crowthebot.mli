module Config = Config
(** Matrix assistant components for embedding and adding plugins. *)

module Store = Store
module Diagnostics = Diagnostics
module Trace = Trace
module Typing = Typing
module Inspect = Inspect
module Rich_text = Rich_text
module Audit = Audit
module Memory = Memory
module Daily = Daily
module Cron = Cron
module Feed_store = Feed_store
module Location_store = Location_store
module Secret_store = Secret_store
module Tool_config = Tool_config
module Owntracks_source = Owntracks_source
module Overpass = Overpass
module Room_context = Room_context
module Compaction = Compaction
module Model_config = Model_config
module Locations = Locations
module Email_client = Email_client
module Email_source = Email_source
module Email_cache = Email_cache
module Emails = Emails
module Feed_http = Feed_http
module Feed_parse = Feed_parse
module Feeds = Feeds
module Plugin = Plugin
module Engine = Engine
module Address = Address
module Matrix_input = Matrix_input
module Matrix_rooms = Matrix_rooms
module Verification = Verification
module App = App
module Calendar_source = Calendar_source
module Calendar_store = Calendar_store
module Calendars = Calendars
module Caldav_source : module type of Caldav_source
module Caldav_http : module type of Caldav_http
module Caldav_probe : module type of Caldav_probe
module Caldav_store : module type of Caldav_store
module Caldav_tools : module type of Caldav_tools
module Caldav_agenda = Caldav_agenda
module Caldav_agenda_store = Caldav_agenda_store
module Caldav_text = Caldav_text
