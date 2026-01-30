open! Core
open! Import

module T = struct
  type time = Time_float.t

  module Instance = struct
    type t = Log.t
    type return_type = unit

    let would_log = Log.would_log
    let message = Log.structured_message
    let default = ()
  end
end

include T

module Global = struct
  type return_type = unit

  let default = ()
  let would_log = Global.would_log
  let message = Global.structured_message
end

module No_global = struct
  module Ppx_log_syntax = struct
    include T

    module Global = struct
      type return_type = [ `Do_not_use_because_it_will_not_log ]

      let default = `Do_not_use_because_it_will_not_log
      let would_log _ = false
      let message ?level:_ ?time:_ ?tags:_ _ _ = `Do_not_use_because_it_will_not_log
    end
  end
end
