include struct
  open Async_log_kernel
  module Message_event = Message_event
  module Global = Global
  module Level = Level
end

include struct
  open Async_unix
  module Reader = Reader
  module Shutdown = Shutdown
  module Sys = Sys
  module Unix = Unix
  module Writer = Writer
end
