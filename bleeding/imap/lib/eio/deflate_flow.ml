let buffer_size = 65_536
let no_output_limit = 16 * 1024 * 1024

type t = {
  raw : [Eio.Flow.two_way_ty | Eio.Resource.close_ty] Eio.Resource.t;
  input : Cstruct.t;
  decoded : Cstruct.t;
  encoded : Cstruct.t;
  decoder : De.Inf.decoder;
  encoder : De.Def.encoder;
  queue : De.Queue.t;
  reads : Eio.Mutex.t;
  writes : Eio.Mutex.t;
  mutable needs_input : bool;
  mutable decoded_pos : int;
  mutable no_output : int;
  mutable closed : bool;
}

let create raw =
  let input=Cstruct.create buffer_size and decoded=Cstruct.create buffer_size
  and encoded=Cstruct.create buffer_size in
  let queue=De.Queue.create buffer_size in
  let decoder=De.Inf.decoder `Manual ~o:(Cstruct.to_bigarray decoded)
    ~w:(De.make_window ~bits:15) in
  let encoder=De.Def.encoder `Manual ~q:queue in
  De.Def.dst encoder (Cstruct.to_bigarray encoded) 0 buffer_size;
  {raw=(raw :> [Eio.Flow.two_way_ty | Eio.Resource.close_ty] Eio.Resource.t);
   input;decoded;encoded;decoder;encoder;queue;
   reads=Eio.Mutex.create ();writes=Eio.Mutex.create ();
   needs_input=false;decoded_pos=0;no_output=0;closed=false}

let close t =
  if not t.closed then (
    t.closed <- true;
    Eio.Cancel.protect (fun () -> Eio.Resource.close t.raw))

let protect t f =
  if t.closed then invalid_arg "closed IMAP DEFLATE flow";
  try f () with ex ->
    let backtrace=Printexc.get_raw_backtrace () in
    (try Eio.Cancel.protect (fun () -> close t) with _ -> ());
    Printexc.raise_with_backtrace ex backtrace

let read t dst =
  if Cstruct.length dst=0 then invalid_arg "empty DEFLATE read buffer";
  Eio.Mutex.use_ro t.reads (fun () -> protect t (fun () ->
    let rec receive () =
      let available=buffer_size-De.Inf.dst_rem t.decoder-t.decoded_pos in
      if available>0 then (
        let count=min available (Cstruct.length dst) in
        Cstruct.blit t.decoded t.decoded_pos dst 0 count;
        t.decoded_pos <- t.decoded_pos+count;
        t.no_output <- 0;
        count)
      else (
        (* De.Inf may produce output while returning Await. Drain it before
           asking the network for more bytes, including on a sync flush. *)
        if t.decoded_pos>0 then (
          De.Inf.flush t.decoder;
          t.decoded_pos <- 0);
        if t.needs_input then (
          Eio.Fiber.yield ();
          let count=Eio.Flow.single_read t.raw t.input in
          if count=0 then raise End_of_file;
          t.no_output <- t.no_output+count;
          if t.no_output>no_output_limit then
            failwith "IMAP DEFLATE exceeded input budget without decoded output";
          De.Inf.src t.decoder (Cstruct.to_bigarray t.input) 0 count;
          t.needs_input <- false);
        (match De.Inf.decode t.decoder with
         | `Await -> t.needs_input <- true
         | `Flush -> ()
         | `End -> failwith "unexpected final IMAP DEFLATE block"
         | `Malformed _ -> failwith "invalid IMAP DEFLATE stream");
        receive ())
    in receive ()))

let write t buffers =
  Eio.Mutex.use_ro t.writes (fun () -> protect t (fun () ->
    let drain () =
      let count=buffer_size-De.Def.dst_rem t.encoder in
      if count>0 then Eio.Flow.write t.raw [Cstruct.sub t.encoded 0 count];
      De.Def.dst t.encoder (Cstruct.to_bigarray t.encoded) 0 buffer_size in
    let rec encode action =
      match De.Def.encode t.encoder action with
      | `Partial -> drain (); encode `Await
      | `Ok | `Block -> () in
    let fixed () =
      encode (`Block {De.Def.kind=De.Def.Fixed;last=false});
      if not (De.Queue.is_empty t.queue) then
        failwith "IMAP DEFLATE encoder did not consume its bounded block" in
    let chunk bytes =
      let lz=De.Lz77.state ~level:4 ~q:t.queue
        ~w:(De.Lz77.make_window ~bits:15) (`String bytes) in
      let rec compress () = match De.Lz77.compress lz with
        | `Flush -> fixed (); compress ()
        | `End -> fixed ()
        | `Await -> assert false in
      compress ();
      Eio.Fiber.yield () in
    List.iter (fun buffer ->
      let rec chunks offset =
        if offset<Cstruct.length buffer then (
          let count=min buffer_size (Cstruct.length buffer-offset) in
          chunk (Cstruct.to_string (Cstruct.sub buffer offset count));
          chunks (offset+count)) in
      chunks 0) buffers;
    (* An empty stored block completes pending Huffman bits and byte-aligns
       the stream: RFC 1951/Z_SYNC_FLUSH, without BFINAL or a zlib header. *)
    encode (`Block {De.Def.kind=De.Def.Flat;last=false});
    drain ()))
