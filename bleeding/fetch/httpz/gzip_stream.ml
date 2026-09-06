open Fetch

let malformed message =
  raise (err (Protocol_error ("malformed gzip response: " ^ message)))

(* Decompress 1.6.0 has four RFC 1952 header bugs: it does not check CM or
   reserved FLG bits, reads XLEN as big-endian, and checks FHCRC against a
   reconstructed (and incomplete) header using the wrong half and byte order
   of its CRC-32.  Validate the original header here, then adjust only the
   bytes handed to that decoder.  The adjusted header has exactly the same
   length, so [src_rem] still maps onto the transport input window. *)

let max_header_bytes = 256 * 1024
let max_members = 1024

type header_stage =
  | Fixed
  | Extra_length
  | Extra_data
  | Filename
  | Comment
  | Header_checksum
  | Ready

type header = {
  mutable stage : header_stage;
  mutable pos : int;
  mutable flags : int;
  mutable extra_left : int;
  (* Header CRC work is unnecessary unless FHCRC is present, and an optional
     name or comment can be large. *)
  mutable checksum : bool;
  (* The RFC checksum covers every original header byte. *)
  mutable crc : Optint.t;
  (* This is the subset decompress 1.6.0 reconstructs for its broken FHCRC
     comparison: the fixed header and the two zero-terminated strings, but no
     XLEN or FEXTRA payload. *)
  mutable decompress_crc : Optint.t;
}

let header () =
  {
    stage = Fixed;
    pos = 0;
    flags = 0;
    extra_left = 0;
    checksum = false;
    crc = Checkseum.Crc32.default;
    decompress_crc = Checkseum.Crc32.default;
  }

let byte cs i = Cstruct.get_uint8 cs i

(* [Checkseum.Crc32] is seedable, so the accumulators still compose over the
   ranges the header arrives in. *)
let crc_range cs h ~decompress first last =
  if h.checksum && last > first then begin
    let bs = cs.Cstruct.buffer in
    let off = cs.Cstruct.off + first and len = last - first in
    h.crc <- Checkseum.Crc32.digest_bigstring bs off len h.crc;
    if decompress then
      h.decompress_crc <-
        Checkseum.Crc32.digest_bigstring bs off len h.decompress_crc
  end

(* FEXTRA, FNAME, FCOMMENT and FHCRC appear in that fixed RFC 1952 order, so
   one successor serves every optional field: enter the first one the flags
   select beyond position [past]. *)
let after h ~past =
  h.stage <-
    (if past < 1 && h.flags land 0x08 <> 0 then Filename
     else if past < 2 && h.flags land 0x10 <> 0 then Comment
     else if h.flags land 0x02 <> 0 then Header_checksum
     else Ready)

type header_result = [ `Ready | `Partial | `Malformed of string ]

let rec prepare_header h cs len : header_result =
  let get = byte cs in
  match h.stage with
  | Ready -> `Ready
  | Fixed ->
      (* Judge each fixed-header byte as soon as it arrives.  Waiting for all
         ten would report trailing non-gzip bytes as a header the peer stopped
         short of finishing. *)
      if len >= 1 && get 0 <> 0x1f then `Malformed "invalid magic bytes"
      else if len >= 2 && get 1 <> 0x8b then `Malformed "invalid magic bytes"
      else if len >= 3 && get 2 <> 8 then
        `Malformed
          (Printf.sprintf "unsupported compression method %d (expected 8)"
             (get 2))
      else if len >= 4 && get 3 land 0xe0 <> 0 then
        `Malformed
          (Printf.sprintf "reserved flag bits are set (FLG=0x%02x)" (get 3))
      else if len < 10 then `Partial
      else begin
        let flags = get 3 in
        h.flags <- flags;
        h.checksum <- flags land 0x02 <> 0;
        crc_range cs h ~decompress:true 0 10;
        h.pos <- 10;
        h.stage <- if flags land 0x04 <> 0 then Extra_length else Extra_data;
        prepare_header h cs len
      end
  | Extra_length ->
      if len - h.pos < 2 then `Partial
      else begin
        let at = h.pos in
        let lo = get at and hi = get (at + 1) in
        crc_range cs h ~decompress:false at (at + 2);
        h.pos <- at + 2;
        h.extra_left <- lo lor (hi lsl 8);
        h.stage <- Extra_data;
        (* The decoder below expects the opposite byte order.  The RFC
           checksum state above has already consumed the original bytes. *)
        Cstruct.set_uint8 cs at hi;
        Cstruct.set_uint8 cs (at + 1) lo;
        prepare_header h cs len
      end
  | Extra_data when h.extra_left = 0 ->
      after h ~past:0;
      prepare_header h cs len
  | Extra_data ->
      let available = len - h.pos in
      if available = 0 then `Partial
      else begin
        let n = min available h.extra_left in
        crc_range cs h ~decompress:false h.pos (h.pos + n);
        h.pos <- h.pos + n;
        h.extra_left <- h.extra_left - n;
        prepare_header h cs len
      end
  | Filename | Comment as stage ->
      if h.pos = len then `Partial
      else begin
        let start = h.pos in
        let rec find_zero i =
          if i = len then None
          else if get i = 0 then Some i
          else find_zero (i + 1)
        in
        match find_zero start with
        | None ->
            crc_range cs h ~decompress:true start len;
            h.pos <- len;
            `Partial
        | Some zero ->
            crc_range cs h ~decompress:true start (zero + 1);
            h.pos <- zero + 1;
            after h ~past:(if stage = Filename then 1 else 2);
            prepare_header h cs len
      end
  | Header_checksum ->
      if len - h.pos < 2 then `Partial
      else
        let observed = get h.pos lor (get (h.pos + 1) lsl 8) in
        let expected =
          Int32.to_int (Int32.logand (Checkseum.Crc32.to_int32 h.crc) 0xffffl)
        in
        if observed <> expected then
          `Malformed
            (Printf.sprintf
               "invalid header checksum (expected 0x%04x, received 0x%04x)"
               expected observed)
        else begin
          (* Satisfy decompress's broken comparison after validating the real
             FHCRC.  It compares the high half of a CRC over its reconstructed
             header as a big-endian integer. *)
          let compat = Checkseum.Crc32.to_int32 h.decompress_crc in
          let compat =
            Int32.to_int
              (Int32.logand (Int32.shift_right_logical compat 16) 0xffffl)
          in
          Cstruct.set_uint8 cs h.pos (compat lsr 8);
          Cstruct.set_uint8 cs (h.pos + 1) (compat land 0xff);
          h.stage <- Ready;
          `Ready
        end

module type INF = sig
  type decoder

  val decode :
    decoder ->
    [ `Await of decoder | `Flush of decoder | `End of decoder
    | `Malformed of string ]

  val src : decoder -> De.bigstring -> int -> int -> decoder
  val dst_rem : decoder -> int
  val src_rem : decoder -> int
  val reset : decoder -> decoder
  val flush : decoder -> decoder
end

module Inflate (Inf : INF) = struct
  type phase =
    | Need_header of header
    | Decoding
    | Member_ended
    | Ended

  type t = {
    src : Eio.Flow.source_ty Eio.Resource.t;
    mutable i : De.bigstring;
    o : De.bigstring;
    o_cs : Cstruct.t;
    mutable i_cs : Cstruct.t;
    mutable d : Inf.decoder;
    mutable ready_pos : int;
    mutable ready_len : int;
    mutable phase : phase;
    mutable input_len : int;
    mutable input_eof : bool;
    mutable members : int;
  }

  let v ~src ~i ~o d =
    {
      src;
      i;
      o;
      o_cs = Cstruct.of_bigarray o;
      i_cs = Cstruct.of_bigarray i;
      d;
      ready_pos = 0;
      ready_len = 0;
      phase = Need_header (header ());
      input_len = 0;
      input_eof = false;
      members = 0;
    }

  let read_methods = []
  let window t = De.bigstring_length t.o - Inf.dst_rem t.d

  let grow_header_buffer t =
    let old = De.bigstring_length t.i in
    if old >= max_header_bytes then
      malformed
        (Printf.sprintf "member header exceeds %d bytes" max_header_bytes);
    let size = min max_header_bytes (max (old * 2) (old + 1)) in
    let next = De.bigstring_create size in
    let next_cs = Cstruct.of_bigarray next in
    Cstruct.blit t.i_cs 0 next_cs 0 t.input_len;
    t.i <- next;
    t.i_cs <- next_cs

  (* A large FNAME or FCOMMENT grows [t.i] for one member only.  Nothing
     downstream sees that allocation, so drop it before the next member
     rather than holding it for the rest of the representation. *)
  let shrink_header_buffer t =
    if
      De.bigstring_length t.i > De.io_buffer_size
      && t.input_len <= De.io_buffer_size
    then begin
      let next = De.bigstring_create De.io_buffer_size in
      let next_cs = Cstruct.of_bigarray next in
      Cstruct.blit t.i_cs 0 next_cs 0 t.input_len;
      t.i <- next;
      t.i_cs <- next_cs
    end

  let read_header_input t =
    if t.input_eof then
      if t.input_len = 0 && t.members > 0 then t.phase <- Ended
      else malformed "unexpected end of input in member header"
    else begin
      if t.input_len = De.bigstring_length t.i then grow_header_buffer t;
      let dst =
        Cstruct.sub_local t.i_cs t.input_len
          (De.bigstring_length t.i - t.input_len)
      in
      match Eio.Flow.single_read t.src dst with
      | n -> t.input_len <- t.input_len + n
      | exception End_of_file ->
          t.input_eof <- true;
          if t.input_len = 0 && t.members > 0 then t.phase <- Ended
          else malformed "unexpected end of input in member header"
    end

  (* A decoder that needs a structure whole, the ten fixed header bytes above
     all, asks for more input without consuming what it already has, so the
     unconsumed suffix has to survive the refill. *)
  let refill_decoder t =
    let rem = Inf.src_rem t.d in
    let base =
      if rem <= 0 then 0
      else begin
        if rem >= De.bigstring_length t.i then
          malformed "decoder stalled on a full input buffer";
        Cstruct.blit t.i_cs (t.input_len - rem) t.i_cs 0 rem;
        rem
      end
    in
    match
      Eio.Flow.single_read t.src
        (Cstruct.sub_local t.i_cs base (De.bigstring_length t.i - base))
    with
    | n ->
        t.input_len <- base + n;
        t.d <- Inf.src t.d t.i 0 (base + n)
    | exception End_of_file ->
        t.input_eof <- true;
        t.input_len <- 0;
        t.d <- Inf.src t.d t.i 0 0

  let start_next_member t =
    let rem = max 0 (Inf.src_rem t.d) in
    if rem > 0 then
      Cstruct.blit t.i_cs (t.input_len - rem) t.i_cs 0 rem;
    t.input_len <- rem;
    shrink_header_buffer t;
    t.d <- Inf.reset t.d;
    t.phase <- Need_header (header ())

  let rec read t (buf @ local) =
    if t.ready_len > 0 then begin
      let n = min t.ready_len (Cstruct.length buf) in
      Cstruct.blit t.o_cs t.ready_pos buf 0 n;
      t.ready_pos <- t.ready_pos + n;
      t.ready_len <- t.ready_len - n;
      if t.ready_len = 0 && t.phase = Decoding then t.d <- Inf.flush t.d;
      n
    end
    else
        match t.phase with
        | Ended -> raise End_of_file
        | Need_header h -> begin
            match prepare_header h t.i_cs t.input_len with
            | `Ready ->
                if t.members >= max_members then
                  malformed
                    (Printf.sprintf "representation has more than %d members"
                       max_members);
                t.members <- t.members + 1;
                t.d <- Inf.src t.d t.i 0 t.input_len;
                t.phase <- Decoding;
                read t buf
            | `Partial ->
                read_header_input t;
                read t buf
            | `Malformed message -> malformed message
          end
        | Member_ended ->
            start_next_member t;
            read t buf
        | Decoding -> begin
            match Inf.decode t.d with
            | `Await d ->
                t.d <- d;
                refill_decoder t;
                read t buf
            | `Flush d ->
                t.d <- d;
                (match window t with
                | 0 -> t.d <- Inf.flush t.d
                | len ->
                    t.ready_pos <- 0;
                    t.ready_len <- len);
                read t buf
            | `End d ->
                t.d <- d;
                t.phase <- Member_ended;
                (* [Gz] permits a non-empty [o] at [`End]; today it always
                   emits a [`Flush] first, so this window is normally 0. *)
                (match window t with
                | 0 -> ()
                | len ->
                    t.ready_pos <- 0;
                    t.ready_len <- len);
                read t buf
            | `Malformed "Unexpected end of input" when not t.input_eof ->
                (* Decompress reports this when the buffered input ends exactly
                   at the end of a gzip header: [Gz] then hands [De.Inf.src] a
                   zero-length range and eoi's the inner decoder although this
                   module has signalled no EOF.  Recovery is sound because
                   [`Malformed] carries no decoder, so [t.d] still holds the
                   header state and the whole header is presented again. *)
                refill_decoder t;
                read t buf
            | `Malformed message -> malformed message
          end

  (* [Eio.Flow.Pi.SOURCE] asserts a positive count, so an empty destination
     has no answer this module could give. *)
  let single_read t (buf @ local) =
    if Cstruct.length buf = 0 then
      invalid_arg "Gzip_stream: single_read into an empty buffer";
    read t buf
end

module Gunzip = Inflate (Gz.Inf)

let handler = Eio.Flow.Pi.source (module Gunzip)

let gunzip src =
  let i = De.bigstring_create De.io_buffer_size in
  let o = De.bigstring_create De.io_buffer_size in
  let d = Gz.Inf.decoder `Manual ~o in
  Eio.Resource.T (Gunzip.v ~src ~i ~o d, handler)
