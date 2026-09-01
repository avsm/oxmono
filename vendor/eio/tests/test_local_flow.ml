module Partial_sink = struct
  type t = {
    output : Buffer.t;
    chunk : int;
  }

  let single_write t (bufs @ local) =
    let data = Cstruct.copyv bufs in
    let len = min t.chunk (String.length data) in
    Buffer.add_substring t.output data 0 len;
    len

  let copy t ~src = Eio.Flow.Pi.simple_copy ~single_write t ~src
end

let sink =
  let ops = Eio.Flow.Pi.sink (module Partial_sink) in
  fun t -> Eio.Resource.T (t, ops)

let () =
  let output = Buffer.create 6 in
  let dst = sink Partial_sink.{ output; chunk = 3 } in
  let data = Cstruct.of_string "abcdef" in
  let local_ first = Cstruct.sub_local data 0 2 in
  let local_ second = Cstruct.sub_local data 2 4 in
  Eio.Flow.write dst (stack_ [ first; Cstruct.empty; second ]);
  assert (Buffer.contents output = "abcdef")
