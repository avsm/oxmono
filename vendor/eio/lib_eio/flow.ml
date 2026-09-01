open Std

type shutdown_command = [ `Receive | `Send | `All ]

type 't read_method = ..
type 't read_method +=
  Read_source_buffer of ('t -> (Cstruct.t list @ local -> int) -> unit)

type source_ty = [`R | `Flow]
type 'a source = ([> source_ty] as 'a) r

type sink_ty = [`W | `Flow]
type 'a sink = ([> sink_ty] as 'a) r

type shutdown_ty = [`Shutdown]
type 'a shutdown = ([> shutdown_ty] as 'a) r

module Pi = struct
  module type SOURCE = sig
    type t
    val read_methods : t read_method list
    val single_read : t -> Cstruct.t @ local -> int
  end

  module type SINK = sig
    type t
    val single_write : t -> Cstruct.t list @ local -> int
    val copy : t -> src:_ source -> unit
  end

  module type SHUTDOWN = sig
    type t
    val shutdown : t -> shutdown_command -> unit
  end

  type (_, _, _) Resource.pi +=
    | Source : ('t, (module SOURCE with type t = 't), [> source_ty]) Resource.pi
    | Sink : ('t, (module SINK with type t = 't), [> sink_ty]) Resource.pi
    | Shutdown : ('t, (module SHUTDOWN with type t = 't), [> shutdown_ty]) Resource.pi

  let source (type t) (module X : SOURCE with type t = t) =
    Resource.handler [H (Source, (module X))]

  let sink (type t) (module X : SINK with type t = t) =
    Resource.handler [H (Sink, (module X))]

  let shutdown (type t) (module X : SHUTDOWN with type t = t) =
    Resource.handler [ H (Shutdown, (module X))]

  module type TWO_WAY = sig
    include SHUTDOWN
    include SOURCE with type t := t
    include SINK with type t := t
  end

  let two_way (type t) (module X : TWO_WAY with type t = t) =
    Resource.handler [
      H (Shutdown, (module X));
      H (Source, (module X));
      H (Sink, (module X));
    ]

  let (simple_copy @ portable) ~single_write t
      ~src:(Resource.T (src, src_ops)) =
    let rec write_all (buf @ local) limit off =
      let remaining = limit - off in
      if remaining > 0 then (
        let view = Cstruct.sub_local buf off remaining in
        let sent = single_write t (stack_ [view]) in
        write_all buf limit (off + sent)
      )
    in
    let module Src = (val (Resource.get src_ops Source)) in
    try
      let buf = Cstruct.create 4096 in
      while true do
        let got = Src.single_read src buf in
        write_all buf got 0
      done
    with End_of_file -> ()
end

open Pi

let close = Resource.close

let (single_read @ portable) (Resource.T (t, ops)) (buf @ local) =
  let module X = (val (Resource.get ops Source)) in
  let got = X.single_read t buf in
  assert (got > 0 && got <= Cstruct.length buf);
  got

let (read_exact @ portable) t (buf @ local) =
  let len = Cstruct.length buf in
  let rec loop t (buf @ local) off =
    if off < len then (
      let view = Cstruct.sub_local buf off (len - off) in
      let got = single_read t view in
      loop t buf (off + got)
    )
  in
  loop t buf 0

module Cstruct_source = struct
  type t = Cstruct.t list ref

  let create data = ref data

  let read_source_buffer t fn =
    let rec aux () =
      match !t with
      | [] -> raise End_of_file
      | x :: xs when Cstruct.length x = 0 -> t := xs; aux ()
      | xs ->
        let n = fn xs in
        t := Cstruct.shiftv xs n
    in
    aux ()

  let read_methods =
    [ Read_source_buffer read_source_buffer ]

  let single_read t dst =
    let avail, src = Cstruct.fillv ~dst ~src:!t in
    if avail = 0 then raise End_of_file;
    t := src;
    avail

end

let cstruct_source =
  let ops = Pi.source (module Cstruct_source) in
  fun data -> Resource.T (Cstruct_source.create data, ops)

module String_source = struct
  type t = {
    s : string;
    mutable offset : int;
  }

  let single_read t dst =
    if t.offset = String.length t.s then raise End_of_file;
    let len = min (Cstruct.length dst) (String.length t.s - t.offset) in
    Cstruct.blit_from_string t.s t.offset dst 0 len;
    t.offset <- t.offset + len;
    len

  let read_methods = []

  let create s = { s; offset = 0 }
end

let string_source =
  let ops = Pi.source (module String_source) in
  fun s -> Resource.T (String_source.create s, ops)

let (single_write @ portable) (Resource.T (t, ops)) (bufs @ local) =
  let module X = (val (Resource.get ops Sink)) in
  X.single_write t bufs

let (write @ portable) (Resource.T (t, ops)) (bufs @ local) =
  let module X = (val (Resource.get ops Sink)) in
  let rec loop (remaining @ local) head_off =
    match remaining with
    | [] -> ()
    | head :: tail ->
        let available = Cstruct.length head - head_off in
        if available = 0 then loop tail 0
        else begin
          let wrote =
            if head_off = 0 then X.single_write t remaining
            else begin
              let local_ head = Cstruct.sub_local head head_off available in
              let local_ iovec = stack_ (head :: tail) in
              let wrote = X.single_write t iovec in
              wrote
            end
          in
          if wrote <= 0 then
            invalid_arg "Eio.Flow.write: single_write made no progress";
          advance remaining head_off wrote
        end
  and advance (remaining @ local) head_off left =
    match remaining with
    | [] -> invalid_arg "Eio.Flow.write: single_write wrote too much"
    | head :: tail ->
        let available = Cstruct.length head - head_off in
        if left < available then loop remaining (head_off + left)
        else if left = available then loop tail 0
        else advance tail 0 (left - available)
  in
  loop bufs 0

let copy src (Resource.T (t, ops)) =
  let module X = (val (Resource.get ops Sink)) in
  X.copy t ~src

let (copy_string @ portable) s dst = write dst [ Cstruct.of_string s ]

module Buffer_sink = struct
  type t = Buffer.t

  let single_write t (bufs @ local) =
    let old_length = Buffer.length t in
    let rec add (bufs @ local) =
      match bufs with
      | [] -> ()
      | buf :: rest ->
          Buffer.add_bytes t (Cstruct.to_bytes buf);
          add rest
    in
    add bufs;
    Buffer.length t - old_length

  let copy t ~src = Pi.simple_copy ~single_write t ~src
end

let buffer_sink =
  let ops = Pi.sink (module Buffer_sink) in
  fun b -> Resource.T (b, ops)

module Null = struct
  type t = unit

  let read_methods = []

  let single_read () _dst = raise End_of_file

  let single_write () bufs = Cstruct.lenv bufs

  let copy t ~src = Pi.simple_copy ~single_write t ~src
end

let null_handler = 
  Resource.handler [
    H (Source, (module Null));
    H (Sink, (module Null));
  ]

let null =
  let x = Resource.T ((), null_handler) in
  (x : [sink_ty | source_ty] r :> [< sink_ty | source_ty] r)

type two_way_ty = [source_ty | sink_ty | shutdown_ty]
type 'a two_way = ([> two_way_ty] as 'a) r

let shutdown (Resource.T (t, ops)) cmd =
  let module X = (val (Resource.get ops Shutdown)) in
  X.shutdown t cmd
