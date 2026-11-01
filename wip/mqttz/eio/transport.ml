module Slice = Mqttz.Slice
module B = Bytesrw.Bytes

type t = {
  read : bytes -> int -> int -> int;
  write : Slice.t list -> unit;
  close : unit -> unit;
  header : bytes;
}

let create ~read ~write ~close =
  { read; write; close; header = Bytes.create 5 }

let of_flow flow =
  let reader = Bytesrw_eio.bytes_reader_of_flow flow in
  let writer = Bytesrw_eio.bytes_writer_of_flow flow in
  let read bytes off len =
    let slice = B.Reader.read reader in
    if B.Slice.is_eod slice then 0
    else
      let n = min len (B.Slice.length slice) in
      Bytes.blit (B.Slice.bytes slice) (B.Slice.first slice) bytes off n;
      Option.iter (B.Reader.push_back reader) (B.Slice.drop_first n slice);
      n
  in
  let write slices =
    List.iter (fun (s : Slice.t) ->
      if s.len > 0 then
        B.Writer.write writer
          (B.Slice.make s.bytes ~first:s.off ~length:s.len)) slices
  in
  let close () =
    (try Eio.Flow.shutdown flow `All with _ -> ());
    Eio.Resource.close flow
  in
  create ~read ~write ~close

(* This path reads directly into OCaml bytes. Holding the managed descriptor
   across readiness waits prevents descriptor reuse while an operation runs. *)
let of_socket socket =
  match Eio_unix.Resource.fd_opt socket with
  | None -> of_flow socket
  | Some fd ->
      Eio_unix.Fd.use_exn "mqttz nonblocking socket" fd Unix.set_nonblock;
      let read bytes off len =
        Eio_unix.Fd.use_exn "mqttz read" fd (fun raw ->
          let rec retry () =
            Eio.Fiber.check ();
            match Unix.read raw bytes off len with
            | n -> n
            | exception Unix.Unix_error (Unix.EINTR, _, _) -> retry ()
            | exception Unix.Unix_error ((Unix.EAGAIN | Unix.EWOULDBLOCK), _, _) ->
                Eio_unix.await_readable raw;
                retry ()
          in
          retry ())
      in
      let write slices =
        Eio_unix.Fd.use_exn "mqttz write" fd (fun raw ->
          let rec send bytes off len =
            if len > 0 then begin
              Eio.Fiber.check ();
              match Unix.single_write raw bytes off len with
              | 0 -> raise End_of_file
              | n -> send bytes (off + n) (len - n)
              | exception Unix.Unix_error (Unix.EINTR, _, _) -> send bytes off len
              | exception Unix.Unix_error ((Unix.EAGAIN | Unix.EWOULDBLOCK), _, _) ->
                  Eio_unix.await_writable raw;
                  send bytes off len
            end
          in
          List.iter (fun (s : Slice.t) -> send s.bytes s.off s.len) slices)
      in
      let close () =
        (try Eio.Flow.shutdown socket `All with _ -> ());
        Eio.Resource.close socket
      in
      create ~read ~write ~close

let read_exact t bytes off len =
  let rec loop off len =
    if len > 0 then begin
      let n = t.read bytes off len in
      if n = 0 then raise End_of_file;
      if n < 0 || n > len then invalid_arg "invalid transport read count";
      loop (off + n) (len - n)
    end
  in
  loop off len

let read_frame t ~max_size =
  read_exact t t.header 0 2;
  let rec header_size size =
    if Bytes.get_uint8 t.header (size - 1) land 128 = 0 then size
    else if size = 5 then
      raise (Mqttz.Frame.Malformed "remaining length exceeds 4 bytes")
    else begin
      read_exact t t.header size 1;
      header_size (size + 1)
    end
  in
  let size = header_size 2 in
  let local_ header = Slice.make_local t.header ~off:0 ~len:size in
  let total = Mqttz.Frame.length ~max_size header in
  let bytes = Bytes.create total in
  Bytes.blit t.header 0 bytes 0 size;
  read_exact t bytes size (total - size);
  Slice.make bytes
