open Hardcaml

module Source = struct
  type 'a t =
    { tvalid : 'a
    ; tdata : 'a
    ; tkeep : 'a
    ; tstrb : 'a
    ; tlast : 'a
    ; tuser : 'a
    }
  [@@deriving hardcaml, compare ~localize]

  let get_valid (t : Signal.t t) = t.tvalid
  let set_valid t ~valid:tvalid = { t with tvalid }
end

module Dest = struct
  type 'a t = { tready : 'a } [@@deriving hardcaml]
end
